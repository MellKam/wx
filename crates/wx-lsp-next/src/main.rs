use std::collections::{HashMap, HashSet};
use std::panic;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;

use codespan_reporting::diagnostic::{
	Diagnostic as CodeDiagnostic, Label, LabelStyle, Severity,
};
use codespan_reporting::files::Files as _;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::Ansi;
use tokio::sync::Mutex;
use tower_lsp_server::jsonrpc::{Error as JsonRpcError, Result};
use tower_lsp_server::ls_types::{
	CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
	DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag,
	DidChangeTextDocumentParams, DidCloseTextDocumentParams,
	DidOpenTextDocumentParams, DidSaveTextDocumentParams,
	DocumentFormattingParams, GotoDefinitionParams, GotoDefinitionResponse,
	Hover, HoverContents, HoverParams, HoverProviderCapability,
	InitializeParams, InitializeResult, InitializedParams, Location,
	MarkupContent, MarkupKind, MessageType, NumberOrString, OneOf,
	ParameterInformation, ParameterLabel, Position, Range, ReferenceParams,
	RenameParams, SemanticToken, SemanticTokenType, SemanticTokensFullOptions,
	SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
	SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities,
	SignatureHelp, SignatureHelpOptions, SignatureHelpParams,
	SignatureInformation, TextDocumentSyncCapability, TextDocumentSyncKind,
	TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TextEdit, Uri,
	WorkspaceEdit,
};
use tower_lsp_server::{Client, LanguageServer, LspService, Server};
use wx_compiler::ast;
use wx_compiler::ast::TextSpan;
use wx_compiler::tir::{
	ModuleDeclarationKind, SourceSpan, TIR, TypeParamInfo, TypeParamOwner,
};
use wx_compiler::vfs::{self, FileId, FileSource, NativeFileSource};

mod completion;
mod symbol_index;
use symbol_index::{SymbolIndex, SymbolKind, build_symbol_index};

/// Ordered list of token types declared in the semantic tokens legend.
#[repr(u32)]
enum TokenType {
	Function = 0,
	Variable = 1,
	Enum = 2,
	Struct = 3,
	Namespace = 4,
	Parameter = 5,
	EnumMember = 6,
	Interface = 7,
	TypeParameter = 8,
	Type = 9,
}

/// The index of each entry is what gets emitted as `token_type` in the data.
const SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
	SemanticTokenType::FUNCTION,    // TokenType::Function
	SemanticTokenType::VARIABLE,    // TokenType::Variable
	SemanticTokenType::ENUM,        // TokenType::Enum
	SemanticTokenType::STRUCT,      // TokenType::Struct
	SemanticTokenType::NAMESPACE,   // TokenType::Namespace
	SemanticTokenType::PARAMETER,   // TokenType::Parameter
	SemanticTokenType::ENUM_MEMBER, // TokenType::EnumMember
	SemanticTokenType::INTERFACE,   // TokenType::Interface
	SemanticTokenType::TYPE_PARAMETER, // TokenType::TypeParameter
	SemanticTokenType::TYPE,        // TokenType::Type
];

#[derive(serde::Deserialize)]
struct VirtualFileContentParams {
	uri: String,
}

#[derive(serde::Deserialize)]
struct FullDiagnosticParams {
	uri: String,
	index: usize,
}

/// Flushes buffered log lines (collected via `logs.push(...)` in plain,
/// `Client`-less helpers like `analyze_root`/`compile_root`/`parse_root`) to
/// the client. Not raw `eprintln!`: `vscode-languageclient` pipes a server's
/// stderr straight into `outputChannel.error(...)`, so every line written
/// there shows up tagged `[error]` regardless of content — `window/logMessage`
/// is the channel that actually carries a real severity.
async fn flush_logs(client: &Client, logs: Vec<String>) {
	for line in logs {
		client.log_message(MessageType::LOG, line).await;
	}
}

#[derive(Clone)]
struct OpenDocument {
	text: String,
	lsp_version: i32,
}

#[derive(Default)]
struct ServerState {
	open_documents: HashMap<PathBuf, OpenDocument>,
	workspace_folders: Vec<PathBuf>,
	/// Compiled artifacts per crate root — the one source of truth. Which
	/// `CompiledRoot`/`FileId` a given URI belongs to is computed on demand
	/// by `resolve_uri` rather than tracked in a second index, since keeping
	/// a hand-maintained reverse map in sync with this one is exactly the
	/// kind of bookkeeping that silently drifts.
	cached: HashMap<PathBuf, CompiledRoot>,
	/// root -> files we last published diagnostics for. Needed to know which
	/// files to clear when a root is dropped or a file leaves its owning
	/// root; also doubles as the reverse (file -> owning root) index via
	/// `owning_root`, so there's no separate `file_to_root` map to drift out
	/// of sync with it.
	published_by_root: HashMap<PathBuf, HashSet<PathBuf>>,
}

struct AnalysisResult {
	diagnostics_by_file: HashMap<PathBuf, Vec<Diagnostic>>,
	owned_files: HashSet<PathBuf>,
}

struct CompiledRoot {
	graph: vfs::CompilationGraph,
	tir: TIR,
	symbol_index: SymbolIndex,
	/// LSP version of each file in the crate at the time TIR was last built.
	/// `None` means the file was on disk (not open via LSP) at compile time.
	compiled_versions: HashMap<PathBuf, Option<std::num::NonZeroI32>>,
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
	fn read_to_string(&self, path: &str) -> std::result::Result<String, ()> {
		if let Some(doc) = self.open_documents.get(Path::new(path)) {
			return Ok(doc.text.clone());
		}
		self.native.read_to_string(path)
	}

	fn exists(&self, path: &str) -> bool {
		self.open_documents.contains_key(Path::new(path))
			|| self.native.exists(path)
	}
}

struct Backend {
	client: Client,
	state: Arc<Mutex<ServerState>>,
}

/// Rebuild TIR this long after the last edit to a file, so completion/hover/etc.
/// stay close to the live buffer without recompiling on every keystroke.
const REBUILD_DEBOUNCE_MS: u64 = 250;

impl LanguageServer for Backend {
	async fn initialize(
		&self,
		params: InitializeParams,
	) -> Result<InitializeResult> {
		self.client
			.log_message(MessageType::LOG, "initializing...")
			.await;
		let workspace_folders = params
			.workspace_folders
			.iter()
			.flatten()
			.filter_map(|folder| uri_to_path(&folder.uri))
			.collect();
		self.state.lock().await.workspace_folders = workspace_folders;
		Ok(InitializeResult {
			capabilities: ServerCapabilities {
				text_document_sync: Some(TextDocumentSyncCapability::Options(
					TextDocumentSyncOptions {
						open_close: Some(true),
						change: Some(TextDocumentSyncKind::FULL),
						save: Some(TextDocumentSyncSaveOptions::Supported(
							true,
						)),
						..Default::default()
					},
				)),
				hover_provider: Some(HoverProviderCapability::Simple(true)),
				completion_provider: Some(CompletionOptions {
					trigger_characters: Some(vec![
						".".to_string(),
						":".to_string(),
					]),
					..Default::default()
				}),
				definition_provider: Some(OneOf::Left(true)),
				references_provider: Some(OneOf::Left(true)),
				rename_provider: Some(OneOf::Left(true)),
				document_formatting_provider: Some(OneOf::Left(true)),
				signature_help_provider: Some(SignatureHelpOptions {
					trigger_characters: Some(vec![
						"(".to_string(),
						",".to_string(),
					]),
					..Default::default()
				}),
				semantic_tokens_provider: Some(
					SemanticTokensServerCapabilities::SemanticTokensOptions(
						SemanticTokensOptions {
							legend: SemanticTokensLegend {
								token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
								token_modifiers: vec![],
							},
							full: Some(SemanticTokensFullOptions::Bool(true)),
							..Default::default()
						},
					),
				),
				..Default::default()
			},
			..Default::default()
		})
	}

	async fn initialized(&self, _: InitializedParams) {
		self.client
			.log_message(MessageType::LOG, "initialized")
			.await;
	}

	async fn shutdown(&self) -> Result<()> {
		Ok(())
	}

	async fn did_open(&self, params: DidOpenTextDocumentParams) {
		if let Some(path) = uri_to_path(&params.text_document.uri) {
			let mut logs = Vec::new();
			let publications = {
				let mut state = self.state.lock().await;
				state.open_documents.insert(
					path.clone(),
					OpenDocument {
						text: params.text_document.text,
						lsp_version: params.text_document.version,
					},
				);
				compute_refresh(&mut state, &path, &mut logs)
			};
			flush_logs(&self.client, logs).await;
			self.publish_all(publications).await;
		}
	}

	async fn did_change(&self, params: DidChangeTextDocumentParams) {
		let Some(path) = uri_to_path(&params.text_document.uri) else {
			return;
		};
		let Some(change) = params.content_changes.into_iter().last() else {
			return;
		};
		let version = params.text_document.version;
		{
			let mut state = self.state.lock().await;
			state.open_documents.insert(
				path.clone(),
				OpenDocument {
					text: change.text,
					lsp_version: version,
				},
			);
		}

		// Debounced rebuild: wait for a quiet period, then refresh only if no
		// newer edit to this file has landed in the meantime (`lsp_version`
		// already carries that generation number, so no separate counter is
		// needed — a superseded task just no-ops and lets the newer one win).
		let client = self.client.clone();
		let state = Arc::clone(&self.state);
		tokio::spawn(async move {
			tokio::time::sleep(std::time::Duration::from_millis(
				REBUILD_DEBOUNCE_MS,
			))
			.await;
			let mut logs = Vec::new();
			let publications = {
				let mut state = state.lock().await;
				if !is_current_version(&state, &path, version) {
					return;
				}
				compute_refresh(&mut state, &path, &mut logs)
			};
			flush_logs(&client, logs).await;
			publish_diagnostics(&client, publications).await;
		});
	}

	async fn did_save(&self, params: DidSaveTextDocumentParams) {
		if let Some(path) = uri_to_path(&params.text_document.uri) {
			let mut logs = Vec::new();
			let publications = {
				let mut state = self.state.lock().await;
				compute_refresh(&mut state, &path, &mut logs)
			};
			flush_logs(&self.client, logs).await;
			self.publish_all(publications).await;
		}
	}

	async fn did_close(&self, params: DidCloseTextDocumentParams) {
		if let Some(path) = uri_to_path(&params.text_document.uri) {
			let mut logs = Vec::new();
			let publications = {
				let mut state = self.state.lock().await;
				state.open_documents.remove(&path);
				compute_refresh(&mut state, &path, &mut logs)
			};
			flush_logs(&self.client, logs).await;
			self.publish_all(publications).await;
		}
	}

	async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
		let state = self.state.lock().await;
		Ok((|| {
			let (compiled, file_id) = resolve_uri(
				&state,
				&params.text_document_position_params.text_document.uri,
			)?;
			let offset = position_to_offset(
				&compiled.graph.files,
				file_id,
				params.text_document_position_params.position,
			)?;
			let info =
				compiled.symbol_index.find_at_position(file_id, offset)?;
			let text = symbol_hover_text(
				&compiled.tir,
				&compiled.graph.interner,
				&info.kind,
			)?;
			let range = span_to_range(&compiled.graph.files, info.source)?;
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
			let (compiled, file_id) = resolve_uri(
				&state,
				&params.text_document_position_params.text_document.uri,
			)?;
			let offset = position_to_offset(
				&compiled.graph.files,
				file_id,
				params.text_document_position_params.position,
			)?;
			let info =
				compiled.symbol_index.find_at_position(file_id, offset)?;
			let def = compiled
				.symbol_index
				.definitions
				.iter()
				.find(|e| e.kind == info.kind)
				.map(|e| e.source)?;
			let uri = file_id_to_uri(compiled, def.file_id)?;
			let range = span_to_range(&compiled.graph.files, def)?;
			Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
		})())
	}

	async fn references(
		&self,
		params: ReferenceParams,
	) -> Result<Option<Vec<Location>>> {
		let state = self.state.lock().await;
		Ok((|| {
			let (compiled, file_id) = resolve_uri(
				&state,
				&params.text_document_position.text_document.uri,
			)?;
			let offset = position_to_offset(
				&compiled.graph.files,
				file_id,
				params.text_document_position.position,
			)?;
			let info =
				compiled.symbol_index.find_at_position(file_id, offset)?;
			let locations = compiled
				.symbol_index
				.references
				.iter()
				.filter(|e| e.kind == info.kind)
				.chain(
					params
						.context
						.include_declaration
						.then(|| {
							compiled
								.symbol_index
								.definitions
								.iter()
								.filter(|d| d.kind == info.kind)
						})
						.into_iter()
						.flatten(),
				)
				.filter_map(|entry| {
					let uri = file_id_to_uri(compiled, entry.source.file_id)?;
					let range =
						span_to_range(&compiled.graph.files, entry.source)?;
					Some(Location { uri, range })
				})
				.collect::<Vec<_>>();
			match locations.len() {
				0 => None,
				_ => Some(locations),
			}
		})())
	}

	async fn rename(
		&self,
		params: RenameParams,
	) -> Result<Option<WorkspaceEdit>> {
		let state = self.state.lock().await;
		Ok((|| {
			let (compiled, file_id) = resolve_uri(
				&state,
				&params.text_document_position.text_document.uri,
			)?;
			let offset = position_to_offset(
				&compiled.graph.files,
				file_id,
				params.text_document_position.position,
			)?;
			let info =
				compiled.symbol_index.find_at_position(file_id, offset)?;
			let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();
			compiled
				.symbol_index
				.references
				.iter()
				.chain(compiled.symbol_index.definitions.iter())
				.filter(|e| e.kind == info.kind)
				.filter_map(|entry| {
					let uri = file_id_to_uri(compiled, entry.source.file_id)?;
					let range =
						span_to_range(&compiled.graph.files, entry.source)?;
					Some((uri, range))
				})
				.for_each(|(uri, range)| {
					changes.entry(uri).or_default().push(TextEdit {
						range,
						new_text: params.new_name.clone(),
					});
				});
			if changes.is_empty() {
				return None;
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
		let Some(path) = uri_to_path(&params.text_document.uri) else {
			return Ok(None);
		};
		let Some(root) = discover_crate_root(
			&state.open_documents,
			&state.workspace_folders,
			&path,
		) else {
			return Ok(None);
		};

		// Always reparse fresh from the live buffer rather than going through
		// `cached`: format-on-save fires before `didSave`, so `cached` would
		// still reflect the previous save. Parsing is cheap enough (~1ms on
		// typical files) that there's no need to cache it across calls.
		let mut logs = Vec::new();
		let parse_result = parse_root(&state, &root, &mut logs);
		flush_logs(&self.client, logs).await;
		let Ok(graph) = parse_result else {
			return Ok(None);
		};
		let Some(module) = graph
			.crates
			.iter()
			.flat_map(|cg| cg.modules.iter())
			.find(|m| Path::new(&m.file_path) == path.as_path())
		else {
			return Ok(None);
		};
		let has_errors = module.ast.diagnostics.iter().any(|d| {
			matches!(
				d.severity,
				codespan_reporting::diagnostic::Severity::Error
					| codespan_reporting::diagnostic::Severity::Bug
			)
		});
		if has_errors {
			return Ok(None);
		}
		let Ok(file) = graph.files.get(module.file_id) else {
			return Ok(None);
		};
		let source = file.source.as_str();
		let config = wx_fmt::RendererConfig {
			indent_width: params.options.tab_size as u8,
			..Default::default()
		};
		let fmt_start = std::time::Instant::now();
		let Ok(formatted) =
			panic::catch_unwind(panic::AssertUnwindSafe(|| {
				wx_fmt::format(&module.ast, &graph.interner, source, config)
			}))
		else {
			return Ok(None);
		};
		self.client
			.log_message(
				MessageType::LOG,
				format!("formatting took {:?}", fmt_start.elapsed()),
			)
			.await;
		let Some(end) =
			byte_to_position(&graph.files, module.file_id, source.len())
		else {
			return Ok(None);
		};
		Ok(Some(vec![TextEdit {
			range: Range {
				start: Position::default(),
				end,
			},
			new_text: formatted,
		}]))
	}

	async fn signature_help(
		&self,
		params: SignatureHelpParams,
	) -> Result<Option<SignatureHelp>> {
		let state = self.state.lock().await;
		Ok((|| {
			let uri = &params.text_document_position_params.text_document.uri;
			let (compiled, file_id) = resolve_uri(&state, uri)?;
			let position = params.text_document_position_params.position;

			let (source, offset) = resolve_source_and_offset(
				&state, compiled, uri, file_id, position,
			)?;

			let call = find_active_call(source, offset)?;
			let info = compiled
				.symbol_index
				.find_at_position(file_id, call.func_name_start as u32)?;
			let SymbolKind::Function(def_id) = &info.kind else {
				return None;
			};
			let fi = compiled.tir.function_index(*def_id)? as usize;
			let func = &compiled.tir.functions[fi];
			let fmt = compiled.tir.formatter(&compiled.graph.interner);
			let interner = &compiled.graph.interner;

			let name = interner.resolve(func.name.inner).unwrap_or("?");
			let mut label = format!("fn {name}(");
			let mut param_infos: Vec<ParameterInformation> = Vec::new();
			// If the first parameter is named `self`, treat this as a
			// method: show `self` in the signature label but do not
			// include it in the interactive `parameters` list so editors
			// won't tab into it.
			let is_method = func
				.params
				.get(0)
				.map(|p| {
					interner
						.resolve(p.name.inner)
						.map(|s| s == "self")
						.unwrap_or(false)
				})
				.unwrap_or(false);
			let start_idx = if is_method { 1 } else { 0 };
			for (i, param) in func.params.iter().enumerate() {
				if i > 0 {
					label.push_str(", ");
				}
				let param_start = label.len() as u32;
				let pname = interner.resolve(param.name.inner).unwrap_or("_");
				label.push_str(pname);
				label.push_str(": ");
				label.push_str(&fmt.display_type(param.ty.inner).unwrap());
				let param_end = label.len() as u32;
				if i >= start_idx {
					param_infos.push(ParameterInformation {
						label: ParameterLabel::LabelOffsets([
							param_start,
							param_end,
						]),
						documentation: None,
					});
				}
			}
			label.push_str(") -> ");
			match &func.result {
				Some(r) => label.push_str(&fmt.display_type(r.inner).unwrap()),
				None => label.push_str("()"),
			}

			Some(SignatureHelp {
				signatures: vec![SignatureInformation {
					label,
					documentation: None,
					parameters: Some(param_infos),
					active_parameter: Some(call.active_param as u32),
				}],
				active_signature: Some(0),
				active_parameter: Some(call.active_param as u32),
			})
		})())
	}

	async fn semantic_tokens_full(
		&self,
		params: SemanticTokensParams,
	) -> Result<Option<SemanticTokensResult>> {
		let state = self.state.lock().await;
		let Some((compiled, file_id)) =
			resolve_uri(&state, &params.text_document.uri)
		else {
			return Ok(None);
		};
		let files = &compiled.graph.files;

		let mut data: Vec<SemanticToken> = Vec::new();
		let mut prev_line = 0u32;
		let mut prev_char = 0u32;

		let mut entries: Vec<&symbol_index::SpanInfo> = compiled
			.symbol_index
			.definitions
			.iter()
			.chain(compiled.symbol_index.references.iter())
			.filter(|e| e.source.file_id == file_id)
			.collect();
		entries.sort_by_key(|e| e.source.span.start);

		for entry in entries {
			let Some(token_type) = symbol_kind_to_token_type(&entry.kind)
			else {
				continue;
			};
			let Some(pos) = byte_to_position(
				files,
				file_id,
				entry.source.span.start as usize,
			) else {
				continue;
			};
			let length = entry.source.span.end - entry.source.span.start;
			let delta_line = pos.line - prev_line;
			let delta_start = if delta_line == 0 {
				pos.character - prev_char
			} else {
				pos.character
			};
			data.push(SemanticToken {
				delta_line,
				delta_start,
				length,
				token_type: token_type as u32,
				token_modifiers_bitset: 0,
			});
			prev_line = pos.line;
			prev_char = pos.character;
		}

		Ok(Some(SemanticTokensResult::Tokens(
			tower_lsp_server::ls_types::SemanticTokens {
				result_id: None,
				data,
			},
		)))
	}

	async fn completion(
		&self,
		params: CompletionParams,
	) -> Result<Option<CompletionResponse>> {
		let state = self.state.lock().await;
		let uri = &params.text_document_position.text_document.uri;
		let Some((compiled, file_id)) = resolve_uri(&state, uri) else {
			return Ok(None);
		};
		let position = params.text_document_position.position;

		let Some((source, offset)) =
			resolve_source_and_offset(&state, compiled, uri, file_id, position)
		else {
			return Ok(None);
		};
		let completion_start = std::time::Instant::now();
		let items = completion::completion_items(
			&compiled.tir,
			&compiled.graph.interner,
			&compiled.symbol_index,
			file_id,
			source,
			offset,
		);
		self.client
			.log_message(
				MessageType::LOG,
				format!("completion took {:?}", completion_start.elapsed()),
			)
			.await;
		Ok(Some(CompletionResponse::Array(items)))
	}
}

impl Backend {
	async fn publish_all(&self, publications: Vec<(PathBuf, Vec<Diagnostic>)>) {
		publish_diagnostics(&self.client, publications).await;
	}

	async fn virtual_file_content(
		&self,
		params: VirtualFileContentParams,
	) -> Result<String> {
		self.client
			.log_message(
				MessageType::LOG,
				format!("virtual_file_content uri={}", params.uri),
			)
			.await;
		let filename =
			params.uri.strip_prefix("wx://std/").ok_or_else(|| {
				JsonRpcError::invalid_params(format!(
					"not a wxstd URI: {}",
					params.uri
				))
			})?;
		match filename {
			"lib.wx" => Ok(wx_compiler::vfs::STDLIB_SOURCE.to_string()),
			other => Err(JsonRpcError::invalid_params(format!(
				"unknown stdlib file: {other}"
			))),
		}
	}

	/// Re-renders one diagnostic's full, source-snippet-annotated text on
	/// demand — see `notes/lsp-full-diagnostic-view-plan.md`. Deliberately
	/// doesn't cache anything new: `state.cached` already keeps the raw
	/// `codespan_reporting::Diagnostic<FileId>` values alive for as long as
	/// this can usefully be called, so this just re-derives the same
	/// `(path, index)` list `add_compiler_diagnostic` would have produced and
	/// re-renders the one entry the client clicked.
	async fn full_diagnostic(
		&self,
		params: FullDiagnosticParams,
	) -> Result<String> {
		let uri = Uri::from_str(&params.uri).map_err(|_| {
			JsonRpcError::invalid_params(format!("bad uri: {}", params.uri))
		})?;
		let state = self.state.lock().await;
		Ok(render_full_diagnostic(&state, &uri, params.index))
	}
}

/// Re-renders one diagnostic's full, source-snippet-annotated text on
/// demand — see `notes/lsp-full-diagnostic-view-plan.md`. Deliberately
/// doesn't cache anything new: `state.cached` already keeps the raw
/// `codespan_reporting::Diagnostic<FileId>` values alive for as long as this
/// can usefully be called, so this just re-derives the same `(path, index)`
/// list `add_compiler_diagnostic` would have produced and re-renders the one
/// entry the client clicked. Free function (rather than a `Backend` method
/// body) so it's directly testable without a real `Client`.
fn render_full_diagnostic(
	state: &ServerState,
	uri: &Uri,
	index: usize,
) -> String {
	let Some((compiled, _file_id)) = resolve_uri(state, uri) else {
		return "Unable to find original wx diagnostic (file is no longer tracked)."
			.to_string();
	};
	let Some(target_path) = uri_to_path(uri) else {
		return "Unable to find original wx diagnostic.".to_string();
	};
	// Same order `analysis_from_compiled_root`/`add_compiler_diagnostic`
	// iterate and expand in — crate diagnostics per crate, then TIR
	// diagnostics, one slot per `diagnostic_locations` entry matching this
	// path (a diagnostic with no primary label, like unused-enum-variant
	// warnings, contributes one slot per variant) — so `index` lines up
	// exactly with what the client saw when this was published.
	let diagnostic = compiled
		.graph
		.crates
		.iter()
		.flat_map(|cg| cg.diagnostics.iter())
		.chain(compiled.tir.diagnostics.iter())
		.flat_map(|d| {
			let target_path = &target_path;
			diagnostic_locations(&compiled.graph.files, d)
				.into_iter()
				.filter(move |(path, _)| path == target_path)
				.map(move |_| d)
		})
		.nth(index);
	let Some(diagnostic) = diagnostic else {
		return "Unable to find original wx diagnostic (it may have changed since this link was created)."
			.to_string();
	};
	// ANSI-colored, not `emit_to_string`'s plain output: the client strips
	// (for the virtual doc's text) and separately re-parses (for
	// `TextEditorDecorationType`s matching the user's terminal theme) the
	// same escape codes `wx-cli` prints to a real terminal — see
	// `notes/lsp-full-diagnostic-view-plan.md`.
	let mut buffer = Ansi::new(Vec::new());
	if let Err(err) = term::emit_to_write_style(
		&mut buffer,
		&term::Config::default(),
		&compiled.graph.files,
		diagnostic,
	) {
		return format!("Unable to render wx diagnostic: {err}");
	}
	String::from_utf8(buffer.into_inner()).unwrap_or_default()
}

#[tokio::main]
async fn main() {
	let (service, socket) = LspService::build(|client| Backend {
		client,
		state: Arc::new(Mutex::new(ServerState::default())),
	})
	.custom_method("wx/virtualFileContent", Backend::virtual_file_content)
	.custom_method("wx/fullDiagnostic", Backend::full_diagnostic)
	.finish();
	Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
		.serve(service)
		.await;
}

// ── State management
// ──────────────────────────────────────────────────────────

/// Whether `path`'s open buffer is still at `version` — i.e. no newer edit
/// has landed since a debounced rebuild for that edit was scheduled.
pub(crate) fn is_current_version(
	state: &ServerState,
	path: &Path,
	version: i32,
) -> bool {
	state
		.open_documents
		.get(path)
		.is_some_and(|doc| doc.lsp_version == version)
}

async fn publish_diagnostics(
	client: &Client,
	publications: Vec<(PathBuf, Vec<Diagnostic>)>,
) {
	for (path, diagnostics) in publications {
		if let Some(uri) = Uri::from_file_path(&path) {
			client.publish_diagnostics(uri, diagnostics, None).await;
		}
	}
}

/// Resolves the `CompiledRoot`/`FileId` a URI belongs to by scanning
/// `state.cached`, rather than through a hand-maintained reverse index —
/// see the comment on `ServerState::cached`. Matches by reconstructing each
/// module's URI and comparing strings — not by comparing `uri_to_path(uri)`
/// against `m.file_path`, since `Uri::to_file_path()` doesn't check the
/// scheme and happily returns a bogus path for non-`file://` URIs (like the
/// virtual `wx://std/...` stdlib URI), which would wrongly fail to match.
fn resolve_uri<'a>(
	state: &'a ServerState,
	uri: &Uri,
) -> Option<(&'a CompiledRoot, FileId)> {
	state.cached.values().find_map(|compiled| {
		compiled
			.graph
			.crates
			.iter()
			.flat_map(|cg| cg.modules.iter())
			.find_map(|m| {
				let matches = file_id_to_uri(compiled, m.file_id)
					.is_some_and(|u| u.as_str() == uri.as_str());
				matches.then_some((compiled, m.file_id))
			})
	})
}

/// Finds which root `file`'s diagnostics were last published under, by
/// scanning `published_by_root` — the reverse of `ServerState::cached`'s
/// forward (root -> files) direction, computed on demand instead of kept in
/// a second `file -> root` map.
fn owning_root<'a>(state: &'a ServerState, file: &Path) -> Option<&'a Path> {
	state.published_by_root.iter().find_map(|(root, files)| {
		files.contains(file).then_some(root.as_path())
	})
}

/// Whether any file in `tracked` (a root's `compiled_versions`) has moved on
/// from the version it was last built/parsed against.
fn versions_stale(
	tracked: &HashMap<PathBuf, Option<std::num::NonZeroI32>>,
	open_documents: &HashMap<PathBuf, OpenDocument>,
) -> bool {
	tracked.iter().any(|(path, tracked_ver)| {
		match (open_documents.get(path), tracked_ver) {
			(Some(doc), Some(v)) => doc.lsp_version > v.get(),
			(Some(_), None) => true, // was on disk, now open
			(None, Some(_)) => true, // was open, now closed
			(None, None) => false,   // still on disk
		}
	})
}

pub(crate) fn compute_refresh(
	state: &mut ServerState,
	file_path: &Path,
	logs: &mut Vec<String>,
) -> Vec<(PathBuf, Vec<Diagnostic>)> {
	let previous_root = owning_root(state, file_path).map(Path::to_path_buf);
	let current_root = discover_crate_root(
		&state.open_documents,
		&state.workspace_folders,
		file_path,
	);
	let mut publications = Vec::new();

	if let Some(root) = current_root.as_ref() {
		let analysis = analyze_root(state, root, logs);
		publications.extend(collect_publish_operations(state, root, analysis));
	}

	if previous_root != current_root {
		if let Some(old_root) = previous_root {
			if current_root.as_ref() != Some(&old_root) {
				publications.extend(collect_clear_operations(state, &old_root));
			}
		} else if current_root.is_none() {
			publications.push((file_path.to_path_buf(), Vec::new()));
		}
	}

	publications
}

pub(crate) fn analyze_root(
	state: &mut ServerState,
	root: &Path,
	logs: &mut Vec<String>,
) -> AnalysisResult {
	// Skip TIR rebuild if no open file's version has advanced since last compile.
	if let Some(compiled) = state.cached.get(root) {
		if !versions_stale(&compiled.compiled_versions, &state.open_documents) {
			logs.push(format!("TIR cache hit for {:?}", root));
			return analysis_from_compiled_root(compiled);
		}
	}

	let graph = match parse_root(state, root, logs) {
		Ok(graph) => graph,
		Err(()) => {
			state.cached.remove(root);
			return analysis_from_missing_entry_file(root);
		}
	};

	let compiled = compile_root(state, graph, logs);
	let result = analysis_from_compiled_root(&compiled);
	state.cached.insert(root.to_path_buf(), compiled);
	result
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
	let publish_paths =
		diagnostic_publish_paths(&previous, &owned_files, &diagnostics_by_file);

	state
		.published_by_root
		.insert(root.to_path_buf(), owned_files);

	publish_paths
		.into_iter()
		.map(|path| {
			let diagnostics =
				diagnostics_by_file.get(&path).cloned().unwrap_or_default();
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
			.map(|path| (path, Vec::new()))
			.collect()
	} else {
		Vec::new()
	}
}

fn compile_root(
	state: &ServerState,
	mut graph: vfs::CompilationGraph,
	logs: &mut Vec<String>,
) -> CompiledRoot {
	let compiled_versions = graph
		.crates
		.iter()
		.flat_map(|cg| cg.modules.iter())
		.map(|m| {
			let path = PathBuf::from(&m.file_path);
			let ver = state
				.open_documents
				.get(&path)
				.and_then(|doc| std::num::NonZeroI32::new(doc.lsp_version));
			(path, ver)
		})
		.collect();
	let tir_start = std::time::Instant::now();
	let tir = TIR::build(&mut graph);
	logs.push(format!("typechecking took {:?}", tir_start.elapsed()));
	let symbol_index = build_symbol_index(&tir, &graph.interner);
	CompiledRoot {
		graph,
		tir,
		symbol_index,
		compiled_versions,
	}
}

/// Parses `root` fresh from the live overlay (open buffers over disk
/// contents). Not cached — parsing is cheap (~1ms on typical files), and a
/// persistent parse-only cache duplicating `cached`'s staleness tracking
/// wasn't buying anything since every caller either needs it once
/// (`formatting`) or immediately feeds it into a TIR rebuild anyway
/// (`analyze_root`).
/// Fails only if the entry file itself (`root`) can't be read — everything
/// past that point (missing/ambiguous `module` declarations elsewhere in the
/// crate) is a diagnostic on the resulting graph instead, since `discover_crate_root`
/// already verified `root` exists before calling this.
fn parse_root(
	state: &ServerState,
	root: &Path,
	logs: &mut Vec<String>,
) -> std::result::Result<vfs::CompilationGraph, ()> {
	let overlay = OverlayFileSource::new(&state.open_documents);
	let mut builder = vfs::CompilationGraphBuilder::new();
	let parse_start = std::time::Instant::now();
	let stdlib_id = builder.load_stdlib();
	let root_id = builder
		.load_binary(root.to_str().unwrap_or_default().to_string(), &overlay)?;
	let graph = builder.build(root_id, stdlib_id);
	logs.push(format!("parsing took {:?}", parse_start.elapsed()));
	Ok(graph)
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
			add_compiler_diagnostic(
				&mut diagnostics_by_file,
				&compiled.graph.files,
				diagnostic,
			);
		}
	}

	for diagnostic in &compiled.tir.diagnostics {
		add_compiler_diagnostic(
			&mut diagnostics_by_file,
			&compiled.graph.files,
			diagnostic,
		);
	}

	AnalysisResult {
		diagnostics_by_file,
		owned_files,
	}
}

/// The only way `parse_root` can still fail: the entry file itself couldn't
/// be read (e.g. deleted between `discover_crate_root`'s existence check and
/// this call). Everything else — missing/ambiguous child modules — is now a
/// diagnostic on the graph rather than a hard failure, so this is a rare,
/// narrow case rather than the general error path it used to be.
fn analysis_from_missing_entry_file(root: &Path) -> AnalysisResult {
	let mut diagnostics_by_file = HashMap::new();
	let mut owned_files = HashSet::new();

	owned_files.insert(root.to_path_buf());
	diagnostics_by_file.insert(
		root.to_path_buf(),
		vec![Diagnostic {
			range: Range::default(),
			severity: Some(DiagnosticSeverity::ERROR),
			code: None,
			code_description: None,
			source: Some("wx-lsp".to_string()),
			message: format!("failed to read file `{}`", root.display()),
			related_information: None,
			tags: None,
			data: None,
		}],
	);

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

/// Resolves one label to the absolute path + LSP range it points at, or
/// `None` if the label's file has no name, the name isn't an absolute path
/// (true for stdlib's virtual `wx://std/...` "files"), or the span doesn't
/// map to a valid range.
fn label_location(
	files: &vfs::Files,
	label: &Label<FileId>,
) -> Option<(PathBuf, Range)> {
	let path = PathBuf::from(files.name(label.file_id).ok()?);
	if !path.is_absolute() {
		return None;
	}
	let range = span_to_range(
		files,
		SourceSpan::new(
			label.file_id,
			TextSpan::new(label.range.start as u32, label.range.end as u32),
		),
	)?;
	Some((path, range))
}

/// Returns the absolute path + LSP range(s) a diagnostic should be filed
/// under. A diagnostic with a primary label collapses to that one location —
/// secondary labels are supplementary context for that one site (e.g.
/// `report_enum_duplicate_value`'s "value assigned here" labels) and become
/// `related_information` instead, not separate squiggles. A diagnostic with
/// *no* primary label (e.g. `report_unused_enum_variants`, where every listed
/// variant is equally "the problem") instead gets one location per label, so
/// each is independently squiggled rather than arbitrarily collapsing to
/// whichever label happens to be first in the vec — LSP's `Diagnostic` only
/// carries a single `range`, so multiple equally-important locations can only
/// be represented as multiple `Diagnostic`s.
///
/// Single source of truth for "which diagnostics belong to file X, in what
/// order, and how many slots each contributes" — used both when building the
/// published list and when re-deriving it later for `wx/fullDiagnostic`, so
/// the two can't silently drift apart.
fn diagnostic_locations(
	files: &vfs::Files,
	diagnostic: &CodeDiagnostic<FileId>,
) -> Vec<(PathBuf, Range)> {
	if let Some(primary) = diagnostic
		.labels
		.iter()
		.find(|label| label.style == LabelStyle::Primary)
	{
		return label_location(files, primary).into_iter().collect();
	}
	diagnostic
		.labels
		.iter()
		.filter_map(|label| label_location(files, label))
		.collect()
}

fn add_compiler_diagnostic(
	grouped: &mut HashMap<PathBuf, Vec<Diagnostic>>,
	files: &vfs::Files,
	diagnostic: &CodeDiagnostic<FileId>,
) {
	let label_messages: Vec<String> = diagnostic
		.labels
		.iter()
		.filter_map(|label| {
			(!label.message.is_empty()).then(|| label.message.clone())
		})
		.collect();
	let message = if label_messages.is_empty() {
		diagnostic.message.clone()
	} else {
		format!("{}\n{}", diagnostic.message, label_messages.join("\n"))
	};

	let tags = diagnostic.code.as_ref().and_then(|code| {
		use std::str::FromStr;
		use wx_compiler::tir::DiagnosticCode;
		DiagnosticCode::from_str(code)
			.ok()
			.and_then(|code| match code {
				DiagnosticCode::UnreachableCode
				| DiagnosticCode::UnusedVariable
				| DiagnosticCode::UnusedTypeParam
				| DiagnosticCode::UnnecessaryMutability
				| DiagnosticCode::UnusedItem
				| DiagnosticCode::UnusedEnumVariant
				| DiagnosticCode::UnusedLabel
				| DiagnosticCode::UnusedStructField => {
					Some(vec![DiagnosticTag::UNNECESSARY])
				}
				_ => None,
			})
	});

	for (path, range) in diagnostic_locations(files, diagnostic) {
		let primary_uri = Uri::from_file_path(&path);
		let related_information = diagnostic_related_information(
			files,
			diagnostic,
			primary_uri.as_ref(),
			range,
		);

		grouped.entry(path).or_default().push(Diagnostic {
			range,
			severity: Some(severity_to_lsp(diagnostic.severity)),
			code: diagnostic
				.code
				.as_ref()
				.map(|code| NumberOrString::String(code.to_string())),
			code_description: None,
			source: Some("wx".to_string()),
			message: message.clone(),
			related_information,
			tags: tags.clone(),
			data: None,
		});
	}
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
		let uri = Uri::from_file_path(&path)?;
		let range = span_to_range(
			files,
			SourceSpan::new(
				label.file_id,
				TextSpan::new(label.range.start as u32, label.range.end as u32),
			),
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

fn push_type_params(
	s: &mut String,
	tir: &TIR,
	interner: &ast::StringInterner,
	type_params: &[TypeParamInfo],
) {
	if type_params.is_empty() {
		return;
	}
	s.push('<');
	for (i, tp) in type_params.iter().enumerate() {
		if i > 0 {
			s.push_str(", ");
		}
		s.push_str(interner.resolve(tp.name).unwrap_or("?"));
		let has_bounds =
			!tp.bounds.traits.is_empty() || tp.bounds.typeset.is_some();
		if has_bounds {
			s.push_str(": ");
			let fmt = tir.formatter(interner);
			s.push_str(&fmt.display_bounds(&tp.bounds).unwrap_or_default());
		}
	}
	s.push('>');
}

fn symbol_hover_text(
	tir: &TIR,
	interner: &ast::StringInterner,
	kind: &SymbolKind,
) -> Option<String> {
	let fmt = tir.formatter(interner);
	match kind {
		SymbolKind::Function(def_id) => {
			let fi = tir.function_index(*def_id)? as usize;
			let func = &tir.functions[fi];
			let fmt = fmt.with_type_params(&func.type_params);
			let name = interner.resolve(func.name.inner).unwrap_or("?");
			let pub_prefix = if func.pub_span.is_some() { "pub " } else { "" };
			let mut s = format!("{pub_prefix}fn {name}");
			push_type_params(&mut s, tir, interner, &func.type_params);
			s.push('(');
			for (i, param) in func.params.iter().enumerate() {
				if i > 0 {
					s.push_str(", ");
				}
				let pname = interner.resolve(param.name.inner).unwrap_or("_");
				s.push_str(pname);
				s.push_str(": ");
				s.push_str(&fmt.display_type(param.ty.inner).unwrap());
			}
			s.push(')');
			s.push_str(" -> ");
			match &func.result {
				Some(result) => {
					s.push_str(&fmt.display_type(result.inner).unwrap())
				}
				None => s.push_str("()"),
			}
			Some(s)
		}
		SymbolKind::Global(def_id) => {
			let gi = tir.global_index(*def_id)? as usize;
			let global = &tir.globals[gi];
			let name = interner.resolve(global.name.inner).unwrap_or("?");
			let type_str = fmt.display_type(global.ty.inner).unwrap();
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
		SymbolKind::Struct(def_id) => {
			let struct_ =
				tir.structs.get(tir.struct_index(*def_id)? as usize)?;
			let name = interner.resolve(struct_.name.inner).unwrap_or("?");
			let pub_prefix = if struct_.pub_span.is_some() {
				"pub "
			} else {
				""
			};
			let mut s = format!("{pub_prefix}struct {name}");
			push_type_params(&mut s, tir, interner, &struct_.type_params);
			Some(s)
		}
		SymbolKind::Enum(def_id) => {
			let enum_ = tir.enums.get(tir.enum_index(*def_id)? as usize)?;
			let name = interner.resolve(enum_.name.inner).unwrap_or("?");
			let pub_prefix = if enum_.pub_span.is_some() { "pub " } else { "" };
			let repr = fmt.display_type(enum_.repr_type).unwrap();
			Some(format!("{pub_prefix}enum {name}: {repr} {{ ... }}"))
		}
		SymbolKind::Local {
			func_id,
			scope_idx,
			local_idx,
		} => {
			let fi = tir.function_index(*func_id)? as usize;
			let body = tir.functions[fi].body.as_ref()?;
			let local = body
				.stack
				.scopes
				.get(*scope_idx as usize)?
				.locals
				.get(*local_idx as usize)?;
			let name = interner.resolve(local.name.inner).unwrap_or("_");
			let type_str = fmt.display_type(local.ty).unwrap();
			let mut_kw = if local.mut_span.is_some() { "mut " } else { "" };
			Some(format!("local {mut_kw}{name}: {type_str}"))
		}
		SymbolKind::Param { func_id, param_idx } => {
			let fi = tir.function_index(*func_id)? as usize;
			let param = tir.functions[fi].params.get(*param_idx as usize)?;
			let name = interner.resolve(param.name.inner).unwrap_or("_");
			let type_str = fmt.display_type(param.ty.inner).unwrap();
			let mut_kw = if param.mut_span.is_some() { "mut " } else { "" };
			Some(format!("{mut_kw}{name}: {type_str}"))
		}
		SymbolKind::EnumVariant {
			enum_id,
			variant_idx,
		} => {
			let enum_ = tir.enums.get(tir.enum_index(*enum_id)? as usize)?;
			let variant = enum_.variants.get(*variant_idx as usize)?;
			let enum_name = interner.resolve(enum_.name.inner).unwrap_or("?");
			let variant_name =
				interner.resolve(variant.name.inner).unwrap_or("?");
			Some(format!("{enum_name}::{variant_name}"))
		}
		SymbolKind::Namespace(ns_idx) => {
			let ns = tir.namespaces.get(*ns_idx as usize)?;
			match ns.declaration {
				ModuleDeclarationKind::Module(decl_idx) => {
					let decl = tir.module_decls.get(decl_idx as usize)?;
					let name = interner.resolve(decl.name.inner).unwrap_or("?");
					let pub_prefix =
						if decl.pub_span.is_some() { "pub " } else { "" };
					Some(format!("{pub_prefix}module {name}"))
				}
				ModuleDeclarationKind::Import(import_idx) => {
					let decl = tir.import_decls.get(import_idx as usize)?;
					let external = interner
						.resolve(decl.external_name.inner)
						.unwrap_or("?");
					match &decl.internal_name {
						Some(alias) => {
							let alias_name =
								interner.resolve(alias.inner).unwrap_or("?");
							Some(format!(
								"import \"{external}\" as {alias_name}"
							))
						}
						None => Some(format!("import \"{external}\"")),
					}
				}
				ModuleDeclarationKind::Crate(_, _) => {
					let name = interner.resolve(ns.name).unwrap_or("?");
					Some(format!("crate {name}"))
				}
			}
		}
		SymbolKind::TypeParam { owner, param_index } => {
			let param_index = *param_index as usize;
			let tp: &TypeParamInfo = match owner {
				TypeParamOwner::Function(def_id) => {
					let fi = tir.function_index(*def_id)? as usize;
					let func = &tir.functions[fi];
					let local = param_index
						.checked_sub(func.inherited_type_param_count)?;
					func.type_params.get(local)?
				}
				TypeParamOwner::Struct(def_id) => {
					let si = tir.struct_index(*def_id)? as usize;
					tir.structs[si].type_params.get(param_index)?
				}
				TypeParamOwner::ImplBlock(block_idx) => tir
					.generic_impl_list
					.get(*block_idx as usize)?
					.type_params
					.get(param_index)?,
				TypeParamOwner::Trait(trait_idx) => {
					let t = tir.traits.get(*trait_idx as usize)?;
					&t.self_type_param
				}
				TypeParamOwner::TypeAlias(def_id) => {
					let ai = tir.type_alias_index(*def_id)? as usize;
					tir.type_aliases[ai].type_params.get(param_index)?
				}
			};
			let name = interner.resolve(tp.name).unwrap_or("?");
			let bounds_str = fmt.display_bounds(&tp.bounds).unwrap_or_default();
			if bounds_str.is_empty() {
				Some(name.to_string())
			} else {
				Some(format!("{name}: {bounds_str}"))
			}
		}
		SymbolKind::Label { .. } => None,
		SymbolKind::Trait(def_id) => {
			let trait_ = tir.traits.get(tir.trait_index(*def_id)? as usize)?;
			let name = interner.resolve(trait_.name.inner).unwrap_or("?");
			let mut s = format!("trait {name}");
			if !trait_.supertraits.is_empty() {
				s.push_str(": ");
				for (i, &st_idx) in trait_.supertraits.iter().enumerate() {
					if i > 0 {
						s.push_str(" + ");
					}
					let st_name = tir
						.traits
						.get(st_idx as usize)
						.and_then(|t| interner.resolve(t.name.inner))
						.unwrap_or("?");
					s.push_str(st_name);
				}
			}
			Some(s)
		}
		SymbolKind::TypeSet(def_id) => {
			let typeset =
				tir.typesets.get(tir.typeset_index(*def_id)? as usize)?;
			let name = interner.resolve(typeset.name.inner).unwrap_or("?");
			Some(format!("typeset {name} {{ ... }}"))
		}
		SymbolKind::Const(def_id) => {
			let ci = tir.const_index(*def_id)? as usize;
			let constant = &tir.constants[ci];
			let name = interner.resolve(constant.name.inner).unwrap_or("?");
			let type_str = fmt.display_type(constant.ty.inner).unwrap();
			let pub_prefix = if constant.pub_span.is_some() {
				"pub "
			} else {
				""
			};
			Some(format!("{pub_prefix}const {name}: {type_str}"))
		}
		SymbolKind::StructField {
			struct_id,
			field_idx,
		} => {
			let struct_ =
				tir.structs.get(tir.struct_index(*struct_id)? as usize)?;
			let field = struct_.fields.get(*field_idx as usize)?;
			let fmt = fmt.with_type_params(&struct_.type_params);
			let name = interner.resolve(field.name.inner).unwrap_or("?");
			let type_str = fmt.display_type(field.ty.inner).unwrap();
			let pub_prefix = if field.pub_span.is_some() { "pub " } else { "" };
			Some(format!("{pub_prefix}{name}: {type_str}"))
		}
		SymbolKind::AssocType {
			trait_id,
			assoc_name,
		} => {
			let trait_ = tir.traits.get(tir.trait_index(*trait_id)? as usize)?;
			let at = trait_.assoc_types.get(assoc_name)?;
			let name = interner.resolve(*assoc_name).unwrap_or("?");
			let bounds_str = fmt.display_bounds(&at.bounds).unwrap_or_default();
			if bounds_str.is_empty() {
				Some(format!("type {name}"))
			} else {
				Some(format!("type {name}: {bounds_str}"))
			}
		}
	}
}

struct ActiveCall {
	func_name_start: usize,
	paren_pos: usize,
	active_param: usize,
}

/// Scans backwards from `offset` to find the innermost open function call.
fn find_active_call(source: &str, offset: usize) -> Option<ActiveCall> {
	let before = &source[..offset];

	// Walk backwards tracking paren depth to find the opening `(`
	let mut depth = 0usize;
	let mut paren_pos = None;
	for (i, ch) in before.char_indices().rev() {
		match ch {
			')' => depth += 1,
			'(' => {
				if depth == 0 {
					paren_pos = Some(i);
					break;
				}
				depth -= 1;
			}
			_ => {}
		}
	}
	let paren_pos = paren_pos?;

	// Find the identifier immediately before `(`
	let before_paren = before[..paren_pos].trim_end();
	let name_end = before_paren.len();
	let name_start = before_paren
		.char_indices()
		.rev()
		.take_while(|(_, ch)| ch.is_alphanumeric() || *ch == '_')
		.last()
		.map(|(i, _)| i)?;
	if name_start >= name_end {
		return None;
	}

	// Count top-level commas between `(` and cursor for the active parameter index
	let mut depth = 0usize;
	let mut active_param = 0usize;
	for ch in source[paren_pos + 1..offset].chars() {
		match ch {
			'(' => depth += 1,
			')' => depth = depth.saturating_sub(1),
			',' if depth == 0 => active_param += 1,
			_ => {}
		}
	}

	Some(ActiveCall {
		func_name_start: name_start,
		paren_pos,
		active_param,
	})
}

fn symbol_kind_to_token_type(kind: &SymbolKind) -> Option<TokenType> {
	let tt = match kind {
		SymbolKind::Function(_) => TokenType::Function,
		SymbolKind::Global(_)
		| SymbolKind::Const(_)
		| SymbolKind::Local { .. }
		| SymbolKind::StructField { .. } => TokenType::Variable,
		SymbolKind::Enum(_) => TokenType::Enum,
		SymbolKind::Struct(_) => TokenType::Struct,
		SymbolKind::Namespace(_) => TokenType::Namespace,
		SymbolKind::Param { .. } => TokenType::Parameter,
		SymbolKind::EnumVariant { .. } => TokenType::EnumMember,
		SymbolKind::Trait(_) => TokenType::Interface,
		SymbolKind::TypeParam { .. } => TokenType::TypeParameter,
		SymbolKind::AssocType { .. } | SymbolKind::TypeSet(_) => {
			TokenType::Type
		}
		SymbolKind::Label { .. } => return None,
	};
	Some(tt)
}

// ── Helpers
// ───────────────────────────────────────────────────────────────────

/// Resolves `(source, offset)` for `uri`/`position`, preferring the live
/// in-memory buffer (unsaved edits) over the last-compiled source. The two
/// can diverge in both length and line/character shape, since TIR is only
/// rebuilt on save (see `did_change`) — resolving `source` and `offset` from
/// two different snapshots of the file is what let a stale, shorter source
/// get sliced with an offset computed for the live, longer one.
fn resolve_source_and_offset<'a>(
	state: &'a ServerState,
	compiled: &'a CompiledRoot,
	uri: &Uri,
	file_id: FileId,
	position: Position,
) -> Option<(&'a str, usize)> {
	let path = uri_to_path(uri);
	if let Some(doc) = path.as_ref().and_then(|p| state.open_documents.get(p)) {
		let source = doc.text.as_str();
		let offset = position_to_offset_in_str(source, position)?;
		Some((source, offset))
	} else {
		let source = compiled.graph.files.get(file_id).ok()?.source.as_str();
		let offset =
			position_to_offset(&compiled.graph.files, file_id, position)?
				as usize;
		Some((source, offset))
	}
}

fn position_to_offset(
	files: &vfs::Files,
	file_id: FileId,
	position: Position,
) -> Option<u32> {
	let line_range = files.line_range(file_id, position.line as usize).ok()?;
	Some((line_range.start + position.character as usize) as u32)
}

/// Converts an LSP `Position` to a byte offset directly in a source string,
/// without needing a compiled file index. Returns `None` if the line is out of range.
fn position_to_offset_in_str(
	source: &str,
	position: Position,
) -> Option<usize> {
	let mut line = 0u32;
	let mut byte_offset = 0;
	for ch in source.chars() {
		if line == position.line {
			break;
		}
		if ch == '\n' {
			line += 1;
		}
		byte_offset += ch.len_utf8();
	}
	if line < position.line {
		return None;
	}
	Some((byte_offset + position.character as usize).min(source.len()))
}

fn span_to_range(files: &vfs::Files, source: SourceSpan) -> Option<Range> {
	let start =
		byte_to_position(files, source.file_id, source.span.start as usize)?;
	let end =
		byte_to_position(files, source.file_id, source.span.end as usize)?;
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
		&& (open_documents.contains_key(file_path) || file_path.exists())
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
		if open_documents.contains_key(&candidate) || candidate.exists() {
			return Some(candidate);
		}
		current = dir.parent();
	}

	None
}

fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
	uri.to_file_path().map(|cow| cow.into_owned())
}

/// Converts a `FileId` to a URI. Real files get a `file://` URI; virtual
/// files (non-absolute names, i.e. stdlib) get a `wx://std/<name>` URI.
fn file_id_to_uri(compiled: &CompiledRoot, file_id: FileId) -> Option<Uri> {
	let name = compiled.graph.files.name(file_id).ok()?;
	if let Some(uri) = Uri::from_file_path(Path::new(name)) {
		Some(uri)
	} else {
		Uri::from_str(&format!("wx://std/{}", name)).ok()
	}
}

#[cfg(test)]
mod tests;
