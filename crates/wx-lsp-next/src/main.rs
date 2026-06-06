use std::collections::{HashMap, HashSet};
use std::panic;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use codespan_reporting::diagnostic::{Diagnostic as CodeDiagnostic, LabelStyle, Severity};
use codespan_reporting::files::Files as _;
use tokio::sync::Mutex;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentFormattingParams, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    Location, MarkupContent, MarkupKind, NumberOrString, OneOf, Position, Range, ReferenceParams,
    RenameParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit,
    Uri, WorkspaceEdit,
};
use tower_lsp_server::{Client, LanguageServer, LspService, Server};
use wx_compiler::ast;
use wx_compiler::tir::TIR;
use wx_compiler::vfs::{self, FileId, FileSource, LoadError, NativeFileSource};

mod symbol_index;
use symbol_index::{SymbolIndex, SymbolKind, build_symbol_index};

#[cfg(debug_assertions)]
macro_rules! debug_log {
    ($($arg:tt)*) => { eprintln!($($arg)*); };
}

#[cfg(not(debug_assertions))]
macro_rules! debug_log {
    ($($arg:tt)*) => {};
}

#[derive(Clone)]
struct OpenDocument {
    text: String,
}

#[derive(Default)]
struct ServerState {
    open_documents: HashMap<PathBuf, OpenDocument>,
    file_to_root: HashMap<PathBuf, PathBuf>,
    published_by_root: HashMap<PathBuf, HashSet<PathBuf>>,
    workspace_folders: Vec<PathBuf>,
    cached: HashMap<PathBuf, CompiledRoot>,
}

struct AnalysisResult {
    diagnostics_by_file: HashMap<PathBuf, Vec<Diagnostic>>,
    owned_files: HashSet<PathBuf>,
}

#[derive(Clone, Copy)]
struct ModuleLoc {
    crate_idx: usize,
    module_idx: usize,
}

struct CompiledRoot {
    graph: vfs::CompilationGraph,
    tir: TIR,
    symbol_index: SymbolIndex,
    path_to_module_loc: HashMap<PathBuf, ModuleLoc>,
}

struct OverlayFileSource<'a> {
    open_documents: &'a HashMap<PathBuf, OpenDocument>,
    native: NativeFileSource,
}

impl<'a> OverlayFileSource<'a> {
    fn new(open_documents: &'a HashMap<PathBuf, OpenDocument>) -> Self {
        Self {
            open_documents,
            native: NativeFileSource,
        }
    }
}

impl FileSource for OverlayFileSource<'_> {
    fn read_to_string(&self, path: &str) -> std::result::Result<String, LoadError> {
        if let Some(doc) = self.open_documents.get(Path::new(path)) {
            return Ok(doc.text.clone());
        }
        self.native.read_to_string(path)
    }

    fn exists(&self, path: &str) -> bool {
        self.open_documents.contains_key(Path::new(path)) || self.native.exists(path)
    }
}

struct Backend {
    client: Client,
    state: Arc<Mutex<ServerState>>,
}

impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        debug_log!("wx-lsp-next initializing");
        let workspace_folders = params
            .workspace_folders
            .iter()
            .flatten()
            .filter_map(|folder| uri_to_path(&folder.uri))
            .collect();
        self.state.lock().await.workspace_folders = workspace_folders;
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        debug_log!("wx-lsp-next initialized");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        if let Some(path) = uri_to_path(&params.text_document.uri) {
            let publications = {
                let mut state = self.state.lock().await;
                state.open_documents.insert(
                    path.clone(),
                    OpenDocument {
                        text: params.text_document.text,
                    },
                );
                compute_refresh(&mut state, &path)
            };
            self.publish_all(publications).await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(path) = uri_to_path(&params.text_document.uri) {
            if let Some(change) = params.content_changes.into_iter().last() {
                let publications = {
                    let mut state = self.state.lock().await;
                    state
                        .open_documents
                        .insert(path.clone(), OpenDocument { text: change.text });
                    compute_refresh(&mut state, &path)
                };
                self.publish_all(publications).await;
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        if let Some(path) = uri_to_path(&params.text_document.uri) {
            let publications = {
                let mut state = self.state.lock().await;
                state.open_documents.remove(&path);
                compute_refresh(&mut state, &path)
            };
            self.publish_all(publications).await;
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let state = self.state.lock().await;
        Ok((|| {
            let path = uri_to_path(&params.text_document_position_params.text_document.uri)?;
            let root = discover_crate_root(&state.open_documents, &state.workspace_folders, &path)?;
            let compiled = state.cached.get(&root)?;
            let file_id = file_id_for_path(compiled, &path)?;
            let offset = position_to_offset(
                &compiled.graph.files,
                file_id,
                params.text_document_position_params.position,
            )?;
            let info = compiled.symbol_index.find_at_position(file_id, offset)?;
            let text = symbol_hover_text(&compiled.tir, &compiled.graph.interner, &info.kind)?;
            let range = span_to_range(&compiled.graph.files, file_id, info.span.start, info.span.end)?;
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```wx\n{text}\n```"),
                }),
                range: Some(range),
            })
        })())
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let state = self.state.lock().await;
        Ok((|| {
            let path = uri_to_path(&params.text_document_position_params.text_document.uri)?;
            let root = discover_crate_root(&state.open_documents, &state.workspace_folders, &path)?;
            let compiled = state.cached.get(&root)?;
            let file_id = file_id_for_path(compiled, &path)?;
            let offset = position_to_offset(
                &compiled.graph.files,
                file_id,
                params.text_document_position_params.position,
            )?;
            let info = compiled.symbol_index.find_at_position(file_id, offset)?;
            let (def_file_id, def_span) = compiled.symbol_index.find_definition(&info.kind)?;
            let def_path = path_for_file_id(compiled, def_file_id)?;
            let uri = path_to_uri(&def_path)?;
            let range = span_to_range(&compiled.graph.files, def_file_id, def_span.start, def_span.end)?;
            Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
        })())
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let state = self.state.lock().await;
        Ok((|| {
            let path = uri_to_path(&params.text_document_position.text_document.uri)?;
            let root = discover_crate_root(&state.open_documents, &state.workspace_folders, &path)?;
            let compiled = state.cached.get(&root)?;
            let file_id = file_id_for_path(compiled, &path)?;
            let offset = position_to_offset(
                &compiled.graph.files,
                file_id,
                params.text_document_position.position,
            )?;
            let info = compiled.symbol_index.find_at_position(file_id, offset)?;
            let mut refs = compiled.symbol_index.find_all_references(&info.kind);
            if !params.context.include_declaration {
                if let Some((def_file_id, def_span)) = compiled.symbol_index.find_definition(&info.kind) {
                    refs.retain(|(fid, span)| !(*fid == def_file_id && span.start == def_span.start));
                }
            }
            let locations = refs
                .into_iter()
                .filter_map(|(ref_file_id, ref_span)| {
                    let ref_path = path_for_file_id(compiled, ref_file_id)?;
                    let uri = path_to_uri(&ref_path)?;
                    let range = span_to_range(
                        &compiled.graph.files,
                        ref_file_id,
                        ref_span.start,
                        ref_span.end,
                    )?;
                    Some(Location { uri, range })
                })
                .collect::<Vec<_>>();
            (!locations.is_empty()).then_some(locations)
        })())
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let state = self.state.lock().await;
        Ok((|| {
            let path = uri_to_path(&params.text_document_position.text_document.uri)?;
            let root = discover_crate_root(&state.open_documents, &state.workspace_folders, &path)?;
            let compiled = state.cached.get(&root)?;
            let file_id = file_id_for_path(compiled, &path)?;
            let offset = position_to_offset(
                &compiled.graph.files,
                file_id,
                params.text_document_position.position,
            )?;
            let info = compiled.symbol_index.find_at_position(file_id, offset)?;
            let refs = compiled.symbol_index.find_all_references(&info.kind);
            if refs.is_empty() {
                return None;
            }
            let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();
            for (ref_file_id, ref_span) in refs {
                let ref_path = path_for_file_id(compiled, ref_file_id)?;
                let uri = path_to_uri(&ref_path)?;
                let range = span_to_range(&compiled.graph.files, ref_file_id, ref_span.start, ref_span.end)?;
                changes.entry(uri).or_default().push(TextEdit {
                    range,
                    new_text: params.new_name.clone(),
                });
            }
            Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            })
        })())
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let state = self.state.lock().await;
        Ok((|| {
            let path = uri_to_path(&params.text_document.uri)?;
            let root = discover_crate_root(&state.open_documents, &state.workspace_folders, &path)?;
            let compiled = state.cached.get(&root)?;
            let module = module_for_path(compiled, &path)?;
            let source = compiled.graph.files.get(module.file_id).ok()?.source.as_str();
            let has_errors = module.ast.diagnostics.iter().any(|d| {
                matches!(
                    d.severity,
                    codespan_reporting::diagnostic::Severity::Error
                        | codespan_reporting::diagnostic::Severity::Bug
                )
            });
            if has_errors {
                return None;
            }
            let config = wx_compiler::fmt::RendererConfig {
                indent_width: params.options.tab_size as u8,
                ..Default::default()
            };
            let formatted = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                wx_compiler::fmt::format(&module.ast, &compiled.graph.interner, source, config)
            }))
            .ok()?;
            let end = byte_to_position(&compiled.graph.files, module.file_id, source.len())?;
            Some(vec![TextEdit {
                range: Range {
                    start: Position::default(),
                    end,
                },
                new_text: formatted,
            }])
        })())
    }
}

impl Backend {
    async fn publish_all(&self, publications: Vec<(PathBuf, Vec<Diagnostic>)>) {
        for (path, diagnostics) in publications {
            if let Some(uri) = path_to_uri(&path) {
                self.client.publish_diagnostics(uri, diagnostics, None).await;
            }
        }
    }
}

#[tokio::main]
async fn main() {
    debug_log!("wx-lsp-next starting");
    let (service, socket) = LspService::new(|client| Backend {
        client,
        state: Arc::new(Mutex::new(ServerState::default())),
    });
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
        .serve(service)
        .await;
}

// ── State management ──────────────────────────────────────────────────────────

pub(crate) fn compute_refresh(
    state: &mut ServerState,
    file_path: &Path,
) -> Vec<(PathBuf, Vec<Diagnostic>)> {
    let previous_root = state.file_to_root.get(file_path).cloned();
    let current_root =
        discover_crate_root(&state.open_documents, &state.workspace_folders, file_path);
    let mut publications = Vec::new();

    if let Some(root) = current_root.as_ref() {
        let analysis = analyze_root(state, root);
        publications.extend(collect_publish_operations(state, root, analysis));
    }

    if previous_root.as_ref() != current_root.as_ref() {
        if let Some(old_root) = previous_root {
            if current_root.as_ref() != Some(&old_root) {
                publications.extend(collect_clear_operations(state, &old_root));
            }
        } else if current_root.is_none() {
            publications.push((file_path.to_path_buf(), Vec::new()));
        }
    }

    if current_root.is_none() {
        state.file_to_root.remove(file_path);
    }

    publications
}

pub(crate) fn analyze_root(state: &mut ServerState, root: &Path) -> AnalysisResult {
    match compile_root(state, root) {
        Ok(compiled) => {
            let result = analysis_from_compiled_root(&compiled);
            state.cached.insert(root.to_path_buf(), compiled);
            result
        }
        Err(error) => {
            state.cached.remove(root);
            analysis_from_load_error(root, error)
        }
    }
}

fn collect_publish_operations(
    state: &mut ServerState,
    root: &Path,
    analysis: AnalysisResult,
) -> Vec<(PathBuf, Vec<Diagnostic>)> {
    let AnalysisResult {
        diagnostics_by_file,
        owned_files,
    } = analysis;

    let previous = state
        .published_by_root
        .get(root)
        .cloned()
        .unwrap_or_default();
    let publish_paths = diagnostic_publish_paths(&previous, &owned_files, &diagnostics_by_file);

    for path in &owned_files {
        state.file_to_root.insert(path.clone(), root.to_path_buf());
    }
    state
        .file_to_root
        .retain(|path, mapped_root| mapped_root != root || owned_files.contains(path));

    state
        .published_by_root
        .insert(root.to_path_buf(), owned_files);

    publish_paths
        .into_iter()
        .map(|path| {
            let diagnostics = diagnostics_by_file.get(&path).cloned().unwrap_or_default();
            (path, diagnostics)
        })
        .collect()
}

fn collect_clear_operations(
    state: &mut ServerState,
    root: &Path,
) -> Vec<(PathBuf, Vec<Diagnostic>)> {
    state.cached.remove(root);
    if let Some(previous) = state.published_by_root.remove(root) {
        previous
            .into_iter()
            .map(|path| {
                state.file_to_root.remove(&path);
                (path, Vec::new())
            })
            .collect()
    } else {
        Vec::new()
    }
}

fn compile_root(
    state: &ServerState,
    root: &Path,
) -> std::result::Result<CompiledRoot, LoadError> {
    let overlay_source = OverlayFileSource::new(&state.open_documents);
    let mut builder = vfs::CompilationGraphBuilder::new();
    let stdlib_id = builder.load_crate(
        "std.wx".to_string(),
        &vfs::VirtualFileSource::new(std::collections::HashMap::from([(
            "std.wx".to_string(),
            wx_compiler::STDLIB_SOURCE.to_string(),
        )])),
    )?;
    builder.load_crate(root.to_str().unwrap_or_default().to_string(), &overlay_source)?;
    let mut graph = builder.build(stdlib_id);
    let tir = TIR::build(&mut graph);
    let symbol_index = build_symbol_index(&tir);
    let path_to_module_loc = graph
        .crates
        .iter()
        .enumerate()
        .flat_map(|(ci, crate_graph)| {
            crate_graph
                .modules
                .iter()
                .enumerate()
                .map(move |(mi, module)| {
                    (PathBuf::from(&module.file_path), ModuleLoc { crate_idx: ci, module_idx: mi })
                })
        })
        .collect();
    Ok(CompiledRoot {
        graph,
        tir,
        symbol_index,
        path_to_module_loc,
    })
}

fn analysis_from_compiled_root(compiled: &CompiledRoot) -> AnalysisResult {
    let mut diagnostics_by_file = HashMap::new();
    let mut owned_files = HashSet::new();

    for crate_graph in &compiled.graph.crates {
        for path in crate_graph.path_to_module.keys() {
            let path_buf = PathBuf::from(path);
            if path_buf.is_absolute() {
                owned_files.insert(path_buf);
            }
        }
        for diagnostic in &crate_graph.diagnostics {
            add_compiler_diagnostic(&mut diagnostics_by_file, &compiled.graph.files, diagnostic);
        }
    }

    for diagnostic in &compiled.tir.diagnostics {
        add_compiler_diagnostic(&mut diagnostics_by_file, &compiled.graph.files, diagnostic);
    }

    AnalysisResult {
        diagnostics_by_file,
        owned_files,
    }
}

fn analysis_from_load_error(root: &Path, error: LoadError) -> AnalysisResult {
    let mut diagnostics_by_file = HashMap::new();
    let mut owned_files = HashSet::new();

    let (path, diagnostic) = match error {
        LoadError::ReadFailed { path } => {
            let target = if Path::new(&path).is_absolute() {
                PathBuf::from(&path)
            } else {
                root.to_path_buf()
            };
            let diagnostic = Diagnostic {
                range: Range::default(),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("wx-lsp-next".to_string()),
                message: format!("failed to read file `{path}`"),
                related_information: None,
                tags: None,
                data: None,
            };
            (target, diagnostic)
        }
        LoadError::AmbiguousModule {
            file,
            directory_file,
        } => {
            let target = if Path::new(&file).is_absolute() {
                PathBuf::from(&file)
            } else {
                root.to_path_buf()
            };
            let diagnostic = Diagnostic {
                range: Range::default(),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("wx-lsp-next".to_string()),
                message: format!("ambiguous module: both `{file}` and `{directory_file}` exist"),
                related_information: None,
                tags: None,
                data: None,
            };
            (target, diagnostic)
        }
        LoadError::TooManyFiles => {
            let diagnostic = Diagnostic {
                range: Range::default(),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("wx-lsp-next".to_string()),
                message: "too many files in crate".to_string(),
                related_information: None,
                tags: None,
                data: None,
            };
            (root.to_path_buf(), diagnostic)
        }
    };

    if path.is_absolute() {
        owned_files.insert(path.clone());
    }
    diagnostics_by_file.insert(path, vec![diagnostic]);

    AnalysisResult {
        diagnostics_by_file,
        owned_files,
    }
}

pub(crate) fn diagnostic_publish_paths(
    previous: &HashSet<PathBuf>,
    owned_files: &HashSet<PathBuf>,
    diagnostics_by_file: &HashMap<PathBuf, Vec<Diagnostic>>,
) -> HashSet<PathBuf> {
    let mut paths = previous.clone();
    paths.extend(owned_files.iter().cloned());
    paths.extend(diagnostics_by_file.keys().cloned());
    paths
}

fn add_compiler_diagnostic(
    grouped: &mut HashMap<PathBuf, Vec<Diagnostic>>,
    files: &vfs::Files,
    diagnostic: &CodeDiagnostic<FileId>,
) {
    let Some(label) = diagnostic
        .labels
        .iter()
        .find(|label| label.style == LabelStyle::Primary)
        .or_else(|| diagnostic.labels.first())
    else {
        return;
    };

    let Ok(name) = files.name(label.file_id) else {
        return;
    };
    let path = PathBuf::from(name);
    if !path.is_absolute() {
        return;
    }

    let Some(range) = span_to_range(
        files,
        label.file_id,
        label.range.start as u32,
        label.range.end as u32,
    ) else {
        return;
    };

    let label_messages: Vec<String> = diagnostic
        .labels
        .iter()
        .filter_map(|label| {
            (!label.message.is_empty()).then(|| match label.style {
                LabelStyle::Primary => label.message.clone(),
                LabelStyle::Secondary => format!("note: {}", label.message),
            })
        })
        .collect();
    let message = if label_messages.is_empty() {
        diagnostic.message.clone()
    } else {
        format!("{}\n{}", diagnostic.message, label_messages.join("\n"))
    };

    let primary_uri = path_to_uri(&path);
    let related_information =
        diagnostic_related_information(files, diagnostic, primary_uri.as_ref(), range);

    let tags = diagnostic.code.as_ref().and_then(|code| {
        use std::str::FromStr;
        use wx_compiler::tir::DiagnosticCode;
        DiagnosticCode::from_str(code).ok().and_then(|c| match c {
            DiagnosticCode::UnreachableCode
            | DiagnosticCode::UnusedVariable
            | DiagnosticCode::UnnecessaryMutability
            | DiagnosticCode::UnusedItem => Some(vec![DiagnosticTag::UNNECESSARY]),
            _ => None,
        })
    });

    grouped.entry(path).or_default().push(Diagnostic {
        range,
        severity: Some(severity_to_lsp(diagnostic.severity)),
        code: diagnostic
            .code
            .as_ref()
            .map(|code| NumberOrString::String(code.to_string())),
        code_description: None,
        source: Some("wx".to_string()),
        message,
        related_information,
        tags,
        data: None,
    });
}

fn diagnostic_related_information(
    files: &vfs::Files,
    diagnostic: &CodeDiagnostic<FileId>,
    primary_uri: Option<&Uri>,
    primary_range: Range,
) -> Option<Vec<DiagnosticRelatedInformation>> {
    let label_infos = diagnostic.labels.iter().filter_map(|label| {
        if label.message.is_empty() {
            return None;
        }
        let path = PathBuf::from(files.name(label.file_id).ok()?);
        let uri = path_to_uri(&path)?;
        let range = span_to_range(
            files,
            label.file_id,
            label.range.start as u32,
            label.range.end as u32,
        )?;
        Some(DiagnosticRelatedInformation {
            location: Location { uri, range },
            message: label.message.clone(),
        })
    });

    let note_infos = diagnostic.notes.iter().filter_map(|note| {
        let uri = primary_uri?.clone();
        Some(DiagnosticRelatedInformation {
            location: Location {
                uri,
                range: primary_range,
            },
            message: note.clone(),
        })
    });

    let infos: Vec<_> = label_infos.chain(note_infos).collect();
    (!infos.is_empty()).then_some(infos)
}

fn symbol_hover_text(
    tir: &TIR,
    interner: &ast::StringInterner,
    kind: &SymbolKind,
) -> Option<String> {
    let fmt = tir.formatter(interner);
    match kind {
        SymbolKind::Function(def_id) => {
            let fi = *tir.function_index_lookup.get(def_id)? as usize;
            let func = &tir.functions[fi];
            let name = interner.resolve(func.name.inner).unwrap_or("?");
            let pub_prefix = if func.pub_span.is_some() { "pub " } else { "" };
            let mut s = format!("{pub_prefix}fn {name}(");
            for (i, param) in func.params.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                let pname = interner.resolve(param.name.inner).unwrap_or("_");
                s.push_str(pname);
                s.push_str(": ");
                s.push_str(&fmt.display_type(param.ty.inner));
            }
            s.push(')');
            s.push_str(" -> ");
            match &func.result {
                Some(result) => s.push_str(&fmt.display_type(result.inner)),
                None => s.push_str("unit"),
            }
            Some(s)
        }
        SymbolKind::Global(def_id) => {
            let gi = *tir.global_index_lookup.get(def_id)? as usize;
            let global = &tir.globals[gi];
            let name = interner.resolve(global.name.inner).unwrap_or("?");
            let type_str = fmt.display_type(global.ty.inner);
            let pub_prefix = if global.pub_span.is_some() {
                "pub "
            } else {
                ""
            };
            let mut_kw = if global.mut_span.is_some() {
                "mut "
            } else {
                ""
            };
            Some(format!("{pub_prefix}global {mut_kw}{name}: {type_str}"))
        }
        SymbolKind::Struct(struct_idx) => {
            let struct_ = tir.structs.get(*struct_idx as usize)?;
            let name = interner.resolve(struct_.name.inner).unwrap_or("?");
            let pub_prefix = if struct_.pub_span.is_some() {
                "pub "
            } else {
                ""
            };
            Some(format!("{pub_prefix}struct {name}"))
        }
        SymbolKind::Enum(enum_idx) => {
            let enum_ = tir.enums.get(*enum_idx as usize)?;
            let name = interner.resolve(enum_.name.inner).unwrap_or("?");
            let pub_prefix = if enum_.pub_span.is_some() { "pub " } else { "" };
            let repr = fmt.display_type(enum_.ty);
            Some(format!("{pub_prefix}enum {name}: {repr} {{ ... }}"))
        }
        SymbolKind::Local {
            func_id,
            scope_idx,
            local_idx,
        } => {
            let fi = *tir.function_index_lookup.get(func_id)? as usize;
            let body = tir.functions[fi].body.as_ref()?;
            let local = body
                .stack
                .scopes
                .get(*scope_idx as usize)?
                .locals
                .get(*local_idx as usize)?;
            let name = interner.resolve(local.name.inner).unwrap_or("_");
            let type_str = fmt.display_type(local.ty);
            let mut_kw = if local.mut_span.is_some() { "mut " } else { "" };
            Some(format!("local {mut_kw}{name}: {type_str}"))
        }
        SymbolKind::Param { func_id, param_idx } => {
            let fi = *tir.function_index_lookup.get(func_id)? as usize;
            let param = tir.functions[fi].params.get(*param_idx as usize)?;
            let name = interner.resolve(param.name.inner).unwrap_or("_");
            let type_str = fmt.display_type(param.ty.inner);
            let mut_kw = if param.mut_span.is_some() { "mut " } else { "" };
            Some(format!("{mut_kw}{name}: {type_str}"))
        }
        SymbolKind::EnumVariant {
            enum_idx,
            variant_idx,
        } => {
            let enum_ = tir.enums.get(*enum_idx as usize)?;
            let variant = enum_.variants.get(*variant_idx as usize)?;
            let enum_name = interner.resolve(enum_.name.inner).unwrap_or("?");
            let variant_name = interner.resolve(variant.name.inner).unwrap_or("?");
            Some(format!("{enum_name}::{variant_name}"))
        }
        SymbolKind::Label { .. } => None,
        SymbolKind::Const(def_id) => {
            let ci = *tir.const_index_lookup.get(def_id)? as usize;
            let constant = &tir.constants[ci];
            let name = interner.resolve(constant.name.inner).unwrap_or("?");
            let type_str = fmt.display_type(constant.ty.inner);
            let pub_prefix = if constant.pub_span.is_some() {
                "pub "
            } else {
                ""
            };
            Some(format!("{pub_prefix}const {name}: {type_str}"))
        }
    }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn file_id_for_path(compiled: &CompiledRoot, path: &Path) -> Option<FileId> {
    let &ModuleLoc { crate_idx, module_idx } = compiled.path_to_module_loc.get(path)?;
    Some(compiled.graph.crates[crate_idx].modules[module_idx].file_id)
}

fn path_for_file_id(compiled: &CompiledRoot, file_id: FileId) -> Option<PathBuf> {
    let name = compiled.graph.files.name(file_id).ok()?;
    Some(PathBuf::from(name))
}

fn module_for_path<'a>(compiled: &'a CompiledRoot, path: &Path) -> Option<&'a vfs::SourceModule> {
    let &ModuleLoc { crate_idx, module_idx } = compiled.path_to_module_loc.get(path)?;
    Some(&compiled.graph.crates[crate_idx].modules[module_idx])
}

fn position_to_offset(
    files: &vfs::Files,
    file_id: FileId,
    position: Position,
) -> Option<u32> {
    let line_range = files.line_range(file_id, position.line as usize).ok()?;
    Some((line_range.start + position.character as usize) as u32)
}

fn span_to_range(files: &vfs::Files, file_id: FileId, start: u32, end: u32) -> Option<Range> {
    let start = byte_to_position(files, file_id, start as usize)?;
    let end = byte_to_position(files, file_id, end as usize)?;
    Some(Range { start, end })
}

fn byte_to_position(
    files: &vfs::Files,
    file_id: FileId,
    byte_index: usize,
) -> Option<Position> {
    let line = files.line_index(file_id, byte_index).ok()?;
    let line_range = files.line_range(file_id, line).ok()?;
    let character = byte_index.saturating_sub(line_range.start);
    Some(Position {
        line: line as u32,
        character: character as u32,
    })
}

fn severity_to_lsp(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Bug | Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::INFORMATION,
        Severity::Help => DiagnosticSeverity::HINT,
    }
}

pub(crate) fn discover_crate_root(
    open_documents: &HashMap<PathBuf, OpenDocument>,
    workspace_folders: &[PathBuf],
    file_path: &Path,
) -> Option<PathBuf> {
    if file_path.file_name().is_some_and(|name| name == "main.wx")
        && path_exists(open_documents, file_path)
    {
        return Some(file_path.to_path_buf());
    }

    let mut current = file_path.parent();
    while let Some(dir) = current {
        if !workspace_folders.is_empty()
            && !workspace_folders.iter().any(|root| dir.starts_with(root))
        {
            current = dir.parent();
            continue;
        }

        let candidate = dir.join("main.wx");
        if path_exists(open_documents, &candidate) {
            return Some(candidate);
        }
        current = dir.parent();
    }

    None
}

fn path_exists(open_documents: &HashMap<PathBuf, OpenDocument>, path: &Path) -> bool {
    open_documents.contains_key(path) || path.exists()
}

fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    uri.to_file_path().map(|cow| cow.into_owned())
}

fn path_to_uri(path: &Path) -> Option<Uri> {
    Uri::from_file_path(path)
}

#[cfg(test)]
mod tests;
