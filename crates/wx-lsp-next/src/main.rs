use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::{Path, PathBuf};

use codespan_reporting::diagnostic::{Diagnostic as CodeDiagnostic, LabelStyle, Severity};
use codespan_reporting::files::Files as _;
use lsp_server::{
    Connection, ExtractError, Message, Notification, Request, RequestId, Response, ResponseError,
};
use lsp_types::notification::Notification as _;
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Hover, HoverContents,
    HoverProviderCapability, InitializeParams, Location, MarkedString, NumberOrString, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Uri,
};
use wx_compiler::ast::{self};
use wx_compiler::tir::{ExprKind, Expression, TIR, TypeIndex};
use wx_compiler::vfs::{self, FileId, FileSource, LoadError, NativeFileSource};

#[cfg(debug_assertions)]
macro_rules! debug_log {
    ($($arg:tt)*) => {
        eprintln!($($arg)*);
    };
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
}

struct AnalysisResult {
    diagnostics_by_file: HashMap<PathBuf, Vec<Diagnostic>>,
    owned_files: HashSet<PathBuf>,
}

struct CompiledRoot {
    graph: vfs::CompilationGraph,
    tir: TIR,
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
    fn read_to_string(&self, path: &Path) -> Result<String, LoadError> {
        if let Some(doc) = self.open_documents.get(path) {
            return Ok(doc.text.clone());
        }
        self.native.read_to_string(path)
    }

    fn exists(&self, path: &Path) -> bool {
        self.open_documents.contains_key(path) || self.native.exists(path)
    }
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    debug_log!("wx-lsp-next starting");

    let (connection, io_threads) = Connection::stdio();
    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        ..Default::default()
    })?;

    let initialization_params = connection.initialize(server_capabilities)?;
    let params: InitializeParams = serde_json::from_value(initialization_params)?;

    let mut state = ServerState {
        workspace_folders: collect_workspace_folders(&params),
        ..Default::default()
    };

    main_loop(connection, &mut state)?;
    io_threads.join()?;
    Ok(())
}

fn main_loop(
    connection: Connection,
    state: &mut ServerState,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                let req = match cast_request::<lsp_types::request::HoverRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_hover(state, &params);
                        connection.sender.send(Message::Response(Response {
                            id,
                            result: Some(serde_json::to_value(result)?),
                            error: None,
                        }))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => return Err(err.into()),
                };

                respond_method_not_found(&connection, req.id.clone())?;
            }
            Message::Notification(notif) => {
                handle_notification(&connection, state, notif.clone())?;
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

fn handle_notification(
    connection: &Connection,
    state: &mut ServerState,
    notif: Notification,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let notif = match cast_notification::<lsp_types::notification::DidOpenTextDocument>(notif) {
        Ok(params) => {
            if let Some(path) = uri_to_path(&params.text_document.uri) {
                state.open_documents.insert(
                    path.clone(),
                    OpenDocument {
                        text: params.text_document.text,
                    },
                );
                refresh_file(connection, state, &path)?;
            }
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(notif)) => notif,
        Err(err) => return Err(err.into()),
    };

    let notif = match cast_notification::<lsp_types::notification::DidChangeTextDocument>(notif) {
        Ok(params) => {
            if let Some(path) = uri_to_path(&params.text_document.uri) {
                if let Some(change) = params.content_changes.into_iter().last() {
                    state
                        .open_documents
                        .insert(path.clone(), OpenDocument { text: change.text });
                    refresh_file(connection, state, &path)?;
                }
            }
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(notif)) => notif,
        Err(err) => return Err(err.into()),
    };

    let notif = match cast_notification::<lsp_types::notification::DidCloseTextDocument>(notif) {
        Ok(params) => {
            if let Some(path) = uri_to_path(&params.text_document.uri) {
                state.open_documents.remove(&path);
                refresh_file(connection, state, &path)?;
            }
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(notif)) => notif,
        Err(err) => return Err(err.into()),
    };

    let _notif = notif;
    Ok(())
}

fn refresh_file(
    connection: &Connection,
    state: &mut ServerState,
    file_path: &Path,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let previous_root = state.file_to_root.get(file_path).cloned();
    let current_root =
        discover_crate_root(&state.open_documents, &state.workspace_folders, file_path);

    if let Some(root) = current_root.as_ref() {
        let analysis = analyze_root(state, root);
        publish_analysis(connection, state, root, analysis)?;
    }

    if previous_root.as_ref() != current_root.as_ref() {
        if let Some(old_root) = previous_root {
            if current_root.as_ref() != Some(&old_root) {
                clear_root_diagnostics(connection, state, &old_root)?;
            }
        } else if current_root.is_none() {
            publish_file_diagnostics(connection, file_path, Vec::new())?;
        }
    }

    if current_root.is_none() {
        state.file_to_root.remove(file_path);
    }

    Ok(())
}

fn analyze_root(state: &mut ServerState, root: &Path) -> AnalysisResult {
    match compile_root(state, root) {
        Ok(compiled) => analysis_from_compiled_root(&compiled),
        Err(error) => analysis_from_load_error(root, error),
    }
}

fn compile_root(state: &ServerState, root: &Path) -> Result<CompiledRoot, LoadError> {
    let overlay_source = OverlayFileSource::new(&state.open_documents);
    let mut interner = ast::StringInterner::new();
    let graph = vfs::load_compilation_with_source(root, &overlay_source, &mut interner)?;
    let tir = TIR::build(&graph, &mut interner);
    Ok(CompiledRoot { graph, tir })
}

fn analysis_from_compiled_root(compiled: &CompiledRoot) -> AnalysisResult {
    let mut diagnostics_by_file = HashMap::new();
    let mut owned_files = HashSet::new();

    for crate_graph in &compiled.graph.crates {
        for path in crate_graph.path_to_module.keys() {
            if path.is_absolute() {
                owned_files.insert(path.clone());
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
            let target = if path.is_absolute() {
                path
            } else {
                root.to_path_buf()
            };
            let diagnostic = Diagnostic {
                range: Range::default(),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("wx-lsp-next".to_string()),
                message: format!("failed to read file `{}`", target.display()),
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
            let target = if file.is_absolute() {
                file
            } else {
                root.to_path_buf()
            };
            let diagnostic = Diagnostic {
                range: Range::default(),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("wx-lsp-next".to_string()),
                message: format!(
                    "ambiguous module: both `{}` and `{}` exist",
                    target.display(),
                    directory_file.display()
                ),
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

fn publish_analysis(
    connection: &Connection,
    state: &mut ServerState,
    root: &Path,
    analysis: AnalysisResult,
) -> Result<(), Box<dyn Error + Sync + Send>> {
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

    for path in publish_paths {
        let diagnostics = diagnostics_by_file.get(&path).cloned().unwrap_or_default();
        publish_file_diagnostics(connection, &path, diagnostics)?;
    }

    state
        .published_by_root
        .insert(root.to_path_buf(), owned_files.clone());
    Ok(())
}

fn diagnostic_publish_paths(
    previous: &HashSet<PathBuf>,
    owned_files: &HashSet<PathBuf>,
    diagnostics_by_file: &HashMap<PathBuf, Vec<Diagnostic>>,
) -> HashSet<PathBuf> {
    let mut paths = previous.clone();
    paths.extend(owned_files.iter().cloned());
    paths.extend(diagnostics_by_file.keys().cloned());
    paths
}

fn clear_root_diagnostics(
    connection: &Connection,
    state: &mut ServerState,
    root: &Path,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    if let Some(previous) = state.published_by_root.remove(root) {
        for path in previous {
            publish_file_diagnostics(connection, &path, Vec::new())?;
            state.file_to_root.remove(&path);
        }
    }
    Ok(())
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

    let related_information = diagnostic_related_information(files, diagnostic);

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
        tags: None,
        data: None,
    });
}

fn diagnostic_related_information(
    files: &vfs::Files,
    diagnostic: &CodeDiagnostic<FileId>,
) -> Option<Vec<DiagnosticRelatedInformation>> {
    let infos: Vec<_> = diagnostic
        .labels
        .iter()
        .filter_map(|label| {
            let path = PathBuf::from(files.name(label.file_id).ok()?);
            let uri = path_to_uri(&path)?;
            let range = span_to_range(
                files,
                label.file_id,
                label.range.start as u32,
                label.range.end as u32,
            )?;
            let message = if label.message.is_empty() {
                match label.style {
                    LabelStyle::Primary => "primary location".to_string(),
                    LabelStyle::Secondary => "related location".to_string(),
                }
            } else {
                label.message.clone()
            };

            Some(DiagnosticRelatedInformation {
                location: Location { uri, range },
                message,
            })
        })
        .collect();

    (!infos.is_empty()).then_some(infos)
}

fn position_to_offset(files: &vfs::Files, file_id: FileId, position: Position) -> Option<u32> {
    let line_range = files.line_range(file_id, position.line as usize).ok()?;
    Some((line_range.start + position.character as usize) as u32)
}

fn handle_hover(state: &ServerState, params: &lsp_types::HoverParams) -> Option<Hover> {
    let path = uri_to_path(&params.text_document_position_params.text_document.uri)?;
    let root = discover_crate_root(&state.open_documents, &state.workspace_folders, &path)?;
    let compiled = compile_root(state, &root).ok()?;
    let file_id = compiled
        .graph
        .crates
        .iter()
        .flat_map(|crate_graph| crate_graph.modules.iter())
        .find(|module| module.file_path == path)
        .map(|module| module.file_id)?;
    let offset = position_to_offset(
        &compiled.graph.files,
        file_id,
        params.text_document_position_params.position,
    )?;

    let hover_text = hover_text_at_offset(&compiled.tir, file_id, offset)?;
    let range = span_to_range(&compiled.graph.files, file_id, offset, offset)?;

    Some(Hover {
        contents: HoverContents::Scalar(MarkedString::String(hover_text)),
        range: Some(range),
    })
}

fn hover_text_at_offset(tir: &TIR, file_id: FileId, offset: u32) -> Option<String> {
    let mut best: Option<(u32, String)> = None;

    for function in tir
        .functions
        .iter()
        .filter(|function| function.file_id == file_id)
    {
        if span_contains(function.name.span, offset) {
            offer_hover(
                &mut best,
                function.name.span,
                format!("type: {}", tir.display_type(function.signature_index)),
            );
        }

        for param in function.params.iter() {
            if span_contains(param.name.span, offset) || span_contains(param.ty.span, offset) {
                offer_hover(
                    &mut best,
                    merge_spans(param.name.span, param.ty.span),
                    format!("type: {}", tir.display_type(param.ty.inner)),
                );
            }
        }

        if let Some(result) = &function.result {
            if span_contains(result.span, offset) {
                offer_hover(
                    &mut best,
                    result.span,
                    format!("type: {}", tir.display_type(result.inner)),
                );
            }
        }

        if let Some(body) = &function.body {
            if let Some((span, ty)) = find_expression_type_at_offset(&body.block, offset) {
                offer_hover(&mut best, span, format!("type: {}", tir.display_type(ty)));
            }

            for scope in &body.stack.scopes {
                for local in &scope.locals {
                    if span_contains(local.name.span, offset)
                        || local
                            .accesses
                            .iter()
                            .any(|access| span_contains(access.span, offset))
                    {
                        offer_hover(
                            &mut best,
                            local.name.span,
                            format!("type: {}", tir.display_type(local.ty)),
                        );
                    }
                }
            }
        }
    }

    for global in tir
        .globals
        .iter()
        .filter(|global| global.file_id == file_id)
    {
        if span_contains(global.name.span, offset) || span_contains(global.ty.span, offset) {
            offer_hover(
                &mut best,
                merge_spans(global.name.span, global.ty.span),
                format!("type: {}", tir.display_type(global.ty.inner)),
            );
        }

        if global
            .accesses
            .iter()
            .any(|span| span_contains(*span, offset))
        {
            offer_hover(
                &mut best,
                global.name.span,
                format!("type: {}", tir.display_type(global.ty.inner)),
            );
        }

        if let Some(value) = &global.value {
            if let Some((span, ty)) = find_expression_type_at_offset(&value.inner, offset) {
                offer_hover(&mut best, span, format!("type: {}", tir.display_type(ty)));
            }
        }
    }

    best.map(|(_, text)| text)
}

fn offer_hover(best: &mut Option<(u32, String)>, span: ast::TextSpan, text: String) {
    let width = span.end.saturating_sub(span.start);
    if best
        .as_ref()
        .is_none_or(|(best_width, _)| width <= *best_width)
    {
        *best = Some((width, text));
    }
}

fn find_expression_type_at_offset(
    expr: &Expression,
    offset: u32,
) -> Option<(ast::TextSpan, TypeIndex)> {
    if !span_contains(expr.span, offset) {
        return None;
    }

    let mut best = Some((expr.span, expr.ty));
    for child in expression_children(expr) {
        if let Some(candidate) = find_expression_type_at_offset(child, offset) {
            if best
                .as_ref()
                .is_none_or(|(best_span, _)| span_width(candidate.0) <= span_width(*best_span))
            {
                best = Some(candidate);
            }
        }
    }
    best
}

fn expression_children(expr: &Expression) -> Vec<&Expression> {
    match &expr.kind {
        ExprKind::LocalDeclaration { value, .. } => vec![value.as_ref()],
        ExprKind::Return { value } | ExprKind::Break { value, .. } => {
            value.as_deref().into_iter().collect()
        }
        ExprKind::Unary { operand, .. }
        | ExprKind::Loop { block: operand, .. }
        | ExprKind::TupleFieldAccess {
            object: operand, ..
        } => vec![operand.as_ref()],
        ExprKind::Binary { left, right, .. } => vec![left.as_ref(), right.as_ref()],
        ExprKind::Call { callee, arguments } => std::iter::once(callee.as_ref())
            .chain(arguments.iter())
            .collect(),
        ExprKind::GenericCall { arguments, .. }
        | ExprKind::StructInit {
            fields: arguments, ..
        }
        | ExprKind::TupleInit {
            elements: arguments,
            ..
        }
        | ExprKind::Block {
            expressions: arguments,
            ..
        } => arguments.iter().collect(),
        ExprKind::GenericMethodCall {
            object, arguments, ..
        }
        | ExprKind::MethodCall {
            object, arguments, ..
        } => std::iter::once(object.as_ref())
            .chain(arguments.iter())
            .collect(),
        ExprKind::IfElse {
            condition,
            then_block,
            else_block,
        } => std::iter::once(condition.as_ref())
            .chain(std::iter::once(then_block.as_ref()))
            .chain(else_block.as_deref())
            .collect(),
        ExprKind::ObjectAccess { object, .. } => vec![object.as_ref()],
        _ => Vec::new(),
    }
}

fn span_contains(span: ast::TextSpan, offset: u32) -> bool {
    span.start <= offset && offset <= span.end
}

fn span_width(span: ast::TextSpan) -> u32 {
    span.end.saturating_sub(span.start)
}

fn merge_spans(left: ast::TextSpan, right: ast::TextSpan) -> ast::TextSpan {
    ast::TextSpan::merge(left, right)
}

fn span_to_range(files: &vfs::Files, file_id: FileId, start: u32, end: u32) -> Option<Range> {
    let start = byte_to_position(files, file_id, start as usize)?;
    let end = byte_to_position(files, file_id, end as usize)?;
    Some(Range { start, end })
}

fn byte_to_position(files: &vfs::Files, file_id: FileId, byte_index: usize) -> Option<Position> {
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

fn publish_file_diagnostics(
    connection: &Connection,
    path: &Path,
    diagnostics: Vec<Diagnostic>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let Some(uri) = path_to_uri(path) else {
        return Ok(());
    };

    let notification = lsp_server::Notification::new(
        lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
        PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        },
    );
    connection
        .sender
        .send(Message::Notification(notification))?;
    Ok(())
}

fn discover_crate_root(
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

#[allow(deprecated)]
fn collect_workspace_folders(params: &InitializeParams) -> Vec<PathBuf> {
    if let Some(folders) = &params.workspace_folders {
        return folders
            .iter()
            .filter_map(|folder| uri_to_path(&folder.uri))
            .collect();
    }

    params
        .root_uri
        .as_ref()
        .and_then(uri_to_path)
        .into_iter()
        .collect()
}

fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    url::Url::parse(uri.as_str()).ok()?.to_file_path().ok()
}

fn path_to_uri(path: &Path) -> Option<Uri> {
    url::Url::from_file_path(path).ok()?.as_str().parse().ok()
}

fn respond_method_not_found(
    connection: &Connection,
    id: RequestId,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    connection.sender.send(Message::Response(Response {
        id,
        result: None,
        error: Some(ResponseError {
            code: lsp_server::ErrorCode::MethodNotFound as i32,
            message: "method not supported by wx-lsp-next".to_string(),
            data: None,
        }),
    }))?;
    Ok(())
}

fn cast_request<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_notification<R>(notif: Notification) -> Result<R::Params, ExtractError<Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    notif.extract(R::METHOD)
}

#[cfg(test)]
mod tests;
