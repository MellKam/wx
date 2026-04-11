use std::collections::HashMap;
use std::error::Error;
use std::panic;

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};

/// Log only in debug builds
#[cfg(debug_assertions)]
macro_rules! debug_log {
    ($($arg:tt)*) => {
        eprintln!($($arg)*);
    };
}

/// No-op in release builds
#[cfg(not(debug_assertions))]
macro_rules! debug_log {
    ($($arg:tt)*) => {};
}
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionList, CompletionOptions, CompletionParams,
    CompletionResponse, Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity,
    DiagnosticTag, DocumentFormattingParams, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InsertTextFormat,
    LanguageString, Location, MarkedString, NumberOrString, OneOf, ParameterInformation,
    ParameterLabel, Position, PublishDiagnosticsParams, Range, ReferenceParams, RenameParams,
    SemanticToken, SemanticTokenType, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, SignatureHelp, SignatureHelpOptions,
    SignatureHelpParams, SignatureInformation, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextEdit, Uri, WorkDoneProgressOptions, WorkspaceEdit,
};
use string_interner::StringInterner;
use string_interner::backend::StringBackend;

mod symbol_index;

use symbol_index::SymbolIndex;

const KEYWORDS: &[(&str, &str)] = &[
    ("fn", "Function declaration"),
    ("if", "Conditional expression"),
    ("else", "Alternative branch"),
    ("loop", "Infinite loop"),
    ("break", "Break from loop"),
    ("continue", "Continue to next iteration"),
    ("return", "Return from function"),
    ("local", "Local variable declaration"),
    ("global", "Global variable declaration"),
    ("mut", "Mutable binding"),
    ("export", "Export declaration"),
    ("enum", "Enum declaration"),
    ("as", "Type cast"),
    ("unreachable", "Unreachable code marker"),
];

const BUILTIN_TYPES: &[(&str, &str)] = &[
    ("i32", "32-bit signed integer"),
    ("i64", "64-bit signed integer"),
    ("u32", "32-bit unsigned integer"),
    ("u64", "64-bit unsigned integer"),
    ("f32", "32-bit floating point"),
    ("f64", "64-bit floating point"),
    ("bool", "Boolean type"),
    ("unit", "Unit type"),
];

struct DocumentData {
    ast: wx_compiler::ast::AST,
    tir: Option<wx_compiler::tir::TIR>,
    symbol_index: Option<SymbolIndex>,
    interner: StringInterner<StringBackend>,
}

struct ServerState {
    documents: HashMap<Uri, DocumentData>,
    files: wx_compiler::ast::Files,
}

impl ServerState {
    fn new() -> Self {
        ServerState {
            documents: HashMap::new(),
            files: wx_compiler::ast::Files::new(),
        }
    }
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    debug_log!("WX Language Server starting...");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![":".to_string()]),
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
            all_commit_characters: None,
            completion_item: None,
        }),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        }),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![SemanticTokenType::NAMESPACE, SemanticTokenType::ENUM],
                    token_modifiers: vec![],
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: None,
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            },
        )),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            lsp_types::TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(
                    lsp_types::SaveOptions {
                        include_text: Some(true),
                    },
                )),
                ..Default::default()
            },
        )),
        ..Default::default()
    })
    .unwrap();

    debug_log!("Waiting for initialization...");

    let mut state = ServerState::new();

    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    let params: InitializeParams = serde_json::from_value(initialization_params).unwrap();
    debug_log!("WX LSP server initialized!");
    debug_log!("Client info: {:?}", params.client_info);

    main_loop(connection, &mut state)?;
    io_threads.join()?;

    debug_log!("WX LSP server shutting down");
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
                debug_log!("info: received request: {:?}", req.method);

                let req = match cast_request::<lsp_types::request::HoverRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_hover(state, &params);
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => {
                        debug_log!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Try to handle as definition request
                let req = match cast_request::<lsp_types::request::GotoDefinition>(req) {
                    Ok((id, params)) => {
                        let result = handle_definition(state, &params);
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => {
                        // Other error
                        debug_log!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Try to handle as references request
                let req = match cast_request::<lsp_types::request::References>(req) {
                    Ok((id, params)) => {
                        let result = handle_references(state, &params);
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => {
                        debug_log!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Try to handle as formatting request
                let req = match cast_request::<lsp_types::request::Formatting>(req) {
                    Ok((id, params)) => {
                        let result = handle_formatting(state, &params);
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => {
                        // Other error
                        debug_log!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Try to handle as rename request
                let req = match cast_request::<lsp_types::request::Rename>(req) {
                    Ok((id, params)) => {
                        let result = handle_rename(state, &params);
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => {
                        // Other error
                        debug_log!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Try to handle as completion request
                let req = match cast_request::<lsp_types::request::Completion>(req) {
                    Ok((id, params)) => {
                        let result = handle_completion(state, &params);
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => {
                        debug_log!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Try to handle as signature help request
                let req = match cast_request::<lsp_types::request::SignatureHelpRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_signature_help(state, &params);
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => {
                        debug_log!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Try to handle as semantic tokens request
                let req = match cast_request::<lsp_types::request::SemanticTokensFullRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_semantic_tokens(state, &params);
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                    Err(err) => {
                        debug_log!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Unknown request type
                debug_log!("warn: received unknown request: {:?}", req.method);
            }
            Message::Response(resp) => {
                debug_log!("info: received response: {:?}", resp);
            }
            Message::Notification(notif) => {
                debug_log!("info: received notification: {}", notif.method);
                match notif.method.as_str() {
                    "textDocument/didOpen" => {
                        if let Ok(params) =
                            cast_notification::<lsp_types::notification::DidOpenTextDocument>(notif)
                        {
                            let uri = params.text_document.uri;
                            let content = params.text_document.text;
                            let file_name =
                                uri.path().segments().last().unwrap().as_str().to_string();
                            debug_log!("info: compiling newly opened document");
                            let file_id = state.files.add(file_name, content).unwrap();
                            let doc_data = compile_document(&state.files, file_id);

                            // Publish diagnostics
                            let diagnostics = convert_diagnostics(&doc_data, &uri, &state.files);
                            let diag_params = PublishDiagnosticsParams {
                                uri: uri.clone(),
                                diagnostics,
                                version: None,
                            };
                            let notification = Notification {
                                method: "textDocument/publishDiagnostics".to_string(),
                                params: serde_json::to_value(diag_params).unwrap(),
                            };
                            connection
                                .sender
                                .send(Message::Notification(notification))?;

                            state.documents.insert(uri, doc_data);
                        }
                    }
                    "textDocument/didChange" => {
                        if let Ok(params) = cast_notification::<
                            lsp_types::notification::DidChangeTextDocument,
                        >(notif)
                        {
                            let uri = params.text_document.uri;

                            if let Some(change) = params.content_changes.first() {
                                debug_log!(
                                    "info: recompiling document on change (length: {})",
                                    change.text.len()
                                );
                                let file_id = state.documents.get(&uri).map(|d| d.ast.file_id);
                                let file_id = match file_id {
                                    Some(id) => {
                                        state.files.update(id, change.text.clone());
                                        id
                                    }
                                    None => {
                                        let file_name = uri
                                            .path()
                                            .segments()
                                            .last()
                                            .unwrap()
                                            .as_str()
                                            .to_string();
                                        state.files.add(file_name, change.text.clone()).unwrap()
                                    }
                                };
                                let doc_data = compile_document(&state.files, file_id);
                                state.documents.insert(uri, doc_data);
                            }
                        }
                    }
                    "textDocument/didSave" => {
                        if let Ok(params) =
                            cast_notification::<lsp_types::notification::DidSaveTextDocument>(notif)
                        {
                            let uri = params.text_document.uri;
                            let content = params.text.unwrap_or_default();
                            debug_log!("info: recompiling document on save");
                            let file_id = state.documents.get(&uri).map(|d| d.ast.file_id);
                            let file_id = match file_id {
                                Some(id) => {
                                    state.files.update(id, content);
                                    id
                                }
                                None => {
                                    let file_name =
                                        uri.path().segments().last().unwrap().as_str().to_string();
                                    state.files.add(file_name, content).unwrap()
                                }
                            };
                            let doc_data = compile_document(&state.files, file_id);

                            // Publish diagnostics
                            let diagnostics = convert_diagnostics(&doc_data, &uri, &state.files);
                            let diag_params = PublishDiagnosticsParams {
                                uri: uri.clone(),
                                diagnostics,
                                version: None,
                            };
                            let notification = Notification {
                                method: "textDocument/publishDiagnostics".to_string(),
                                params: serde_json::to_value(diag_params).unwrap(),
                            };
                            connection
                                .sender
                                .send(Message::Notification(notification))?;

                            state.documents.insert(uri, doc_data);
                        }
                    }
                    "textDocument/didClose" => {
                        if let Ok(params) = cast_notification::<
                            lsp_types::notification::DidCloseTextDocument,
                        >(notif)
                        {
                            let uri = params.text_document.uri;
                            state.documents.remove(&uri);
                        }
                    }
                    _ => { /* Ignore other notifications for now */ }
                }
            }
        }
    }
    Ok(())
}

fn compile_document(
    files: &wx_compiler::ast::Files,
    file_id: wx_compiler::ast::FileId,
) -> DocumentData {
    let mut interner = StringInterner::new();
    let source = &files.get(file_id).unwrap().source;
    let ast = wx_compiler::ast::Parser::parse(file_id, source, &mut interner);

    let has_errors = ast.diagnostics.iter().any(|d| match d.severity {
        codespan_reporting::diagnostic::Severity::Error
        | codespan_reporting::diagnostic::Severity::Bug => true,
        _ => false,
    });

    if has_errors {
        return DocumentData {
            ast,
            tir: None,
            symbol_index: None,
            interner,
        };
    }

    let tir = wx_compiler::tir::TIR::build(&ast, &mut interner);
    let span_index = symbol_index::build_span_index(&tir);

    DocumentData {
        ast,
        tir: Some(tir),
        symbol_index: Some(span_index),
        interner,
    }
}

fn convert_diagnostic(
    doc: &DocumentData,
    uri: &Uri,
    files: &wx_compiler::ast::Files,
    diagnostic: &codespan_reporting::diagnostic::Diagnostic<wx_compiler::ast::FileId>,
) -> Diagnostic {
    let severity = match diagnostic.severity {
        codespan_reporting::diagnostic::Severity::Error => DiagnosticSeverity::ERROR,
        codespan_reporting::diagnostic::Severity::Warning => DiagnosticSeverity::WARNING,
        codespan_reporting::diagnostic::Severity::Note => DiagnosticSeverity::INFORMATION,
        codespan_reporting::diagnostic::Severity::Help => DiagnosticSeverity::HINT,
        codespan_reporting::diagnostic::Severity::Bug => DiagnosticSeverity::ERROR,
    };

    let primary_label = diagnostic.labels.first();
    let range = span_to_range(
        files,
        doc.ast.file_id,
        primary_label.map(|l| l.range.clone()),
    );

    let mut message = diagnostic.message.clone();
    if let Some(label) = primary_label {
        if !label.message.is_empty() {
            message.push('\n');
            message.push_str(&label.message);
        }
    }

    for note in &diagnostic.notes {
        if !note.is_empty() {
            message.push('\n');
            message.push_str(&note);
        }
    }

    let mut related_information = Vec::new();
    for (index, label) in diagnostic.labels.iter().enumerate() {
        if index == 0 {
            continue;
        }

        let label_range = span_to_range(files, doc.ast.file_id, Some(label.range.clone()));

        related_information.push(DiagnosticRelatedInformation {
            location: Location {
                uri: uri.clone(),
                range: label_range,
            },
            message: label.message.clone(),
        });
    }

    // Add diagnostic tags for special cases
    let tags = diagnostic.code.as_ref().and_then(|code| {
        use std::str::FromStr;

        use wx_compiler::tir::DiagnosticCode;

        DiagnosticCode::from_str(code)
            .ok()
            .and_then(|diag_code| match diag_code {
                DiagnosticCode::UnreachableCode
                | DiagnosticCode::UnusedVariable
                | DiagnosticCode::UnnecessaryMutability
                | DiagnosticCode::UnusedItem => Some(vec![DiagnosticTag::UNNECESSARY]),
                _ => None,
            })
    });

    Diagnostic {
        range,
        severity: Some(severity),
        code: diagnostic
            .code
            .as_ref()
            .map(|c| NumberOrString::String(c.clone())),
        source: Some("wxc".to_string()),
        message,
        related_information: if related_information.is_empty() {
            None
        } else {
            Some(related_information)
        },
        tags,
        ..Default::default()
    }
}

fn convert_diagnostics(
    doc: &DocumentData,
    uri: &Uri,
    files: &wx_compiler::ast::Files,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for diagnostic in &doc.ast.diagnostics {
        diagnostics.push(convert_diagnostic(doc, uri, files, diagnostic));
    }
    if let Some(tir) = &doc.tir {
        for diagnostic in &tir.diagnostics {
            diagnostics.push(convert_diagnostic(doc, uri, files, diagnostic));
        }
    }

    debug_log!(
        "Total diagnostics published: {} ({} warnings)",
        diagnostics.len(),
        diagnostics
            .iter()
            .filter(|d| matches!(d.severity, Some(DiagnosticSeverity::WARNING)))
            .count()
    );

    diagnostics
}

fn span_to_range(
    files: &wx_compiler::ast::Files,
    file_id: wx_compiler::ast::FileId,
    span: Option<core::ops::Range<usize>>,
) -> Range {
    let zero = Position {
        line: 0,
        character: 0,
    };
    match span {
        Some(span) => Range {
            start: offset_to_position(files, file_id, span.start as u32),
            end: offset_to_position(files, file_id, span.end as u32),
        },
        None => Range {
            start: zero,
            end: zero,
        },
    }
}

fn offset_to_position(
    files: &wx_compiler::ast::Files,
    file_id: wx_compiler::ast::FileId,
    offset: u32,
) -> Position {
    use codespan_reporting::files::Files as _;
    let offset = offset as usize;
    let line = files.line_index(file_id, offset).unwrap_or(0);
    let line_start = files
        .line_range(file_id, line)
        .map(|r| r.start)
        .unwrap_or(0);
    Position {
        line: line as u32,
        character: (offset - line_start) as u32,
    }
}

fn position_to_offset(
    files: &wx_compiler::ast::Files,
    file_id: wx_compiler::ast::FileId,
    position: Position,
) -> Option<u32> {
    use codespan_reporting::files::Files as _;
    let line_range = files.line_range(file_id, position.line as usize).ok()?;
    Some((line_range.start + position.character as usize) as u32)
}

fn handle_hover(state: &ServerState, params: &HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = state.documents.get(uri)?;
    let offset = position_to_offset(&state.files, doc.ast.file_id, position)?;

    debug_log!(
        "Hover request at position {:?} (offset: {})",
        position,
        offset
    );

    let (tir, span_index) = match (&doc.tir, &doc.symbol_index) {
        (Some(tir), Some(span_index)) => (tir, span_index),
        _ => {
            debug_log!("TIR or span index not available for document");
            return None;
        }
    };

    debug_log!("Looking up symbol at hover position...");

    let span_info = span_index.find_at_position(offset)?;

    debug_log!("Found symbol: {:?}", span_info.kind);

    // Format hover information based on symbol kind
    let hover_text = match &span_info.kind {
        symbol_index::SymbolKind::Function { func_idx } => {
            let func = &tir.declared_functions[*func_idx as usize];
            let name = doc.interner.resolve(func.name.inner).unwrap();
            let sig = &tir.signatures[func.signature_index as usize];

            let params = func
                .params
                .iter()
                .map(|p| {
                    let param_name = doc.interner.resolve(p.name.inner).unwrap();
                    let param_type = format_type(&tir, &doc.interner, p.ty.inner);
                    format!("{}: {}", param_name, param_type)
                })
                .collect::<Vec<_>>()
                .join(", ");

            let return_type = format_type(&tir, &doc.interner, sig.result());

            format!("fn {}({}) -> {}", name, params, return_type)
        }
        symbol_index::SymbolKind::LocalVariable {
            func_idx,
            scope_idx,
            local_idx,
        } => {
            let local = &tir.defined_functions.get(func_idx).unwrap().stack.scopes
                [*scope_idx as usize]
                .locals[*local_idx as usize];
            let name = doc.interner.resolve(local.name.inner).unwrap();
            let type_str = format_type(&tir, &doc.interner, local.ty);
            let mut_keyword = if local.mut_span.is_some() { "mut " } else { "" };

            format!("local {}{}: {}", mut_keyword, name, type_str)
        }
        symbol_index::SymbolKind::FunctionParam {
            func_idx,
            param_idx,
        } => {
            // TODO: this might be incorrect if the parameter is from only declared function
            let param = &tir.defined_functions.get(func_idx).unwrap().params[*param_idx as usize];
            let name = doc.interner.resolve(param.name.inner).unwrap();
            let type_str = format_type(&tir, &doc.interner, param.ty.inner);
            let mut_keyword = if param.mut_span.is_some() { "mut " } else { "" };

            format!("local {}{}: {}", mut_keyword, name, type_str)
        }
        symbol_index::SymbolKind::GlobalVariable { global_idx } => {
            let global = &tir.declared_globals[*global_idx as usize];
            let name = doc.interner.resolve(global.name.inner).unwrap();
            let type_str = format_type(&tir, &doc.interner, global.ty.inner);
            let mut_keyword = if global.mut_span.is_some() {
                "mut "
            } else {
                ""
            };

            format!("global {}{}: {}", mut_keyword, name, type_str)
        }
        symbol_index::SymbolKind::Type { ty } => format_type(&tir, &doc.interner, *ty),
        symbol_index::SymbolKind::EnumVariant {
            namespace_index,
            variant_idx,
        } => match &tir.namespaces[*namespace_index as usize] {
            wx_compiler::tir::Namespace::Enum(enum_) => {
                let variant = &enum_.variants[*variant_idx as usize];
                let enum_name = doc.interner.resolve(enum_.name.inner).unwrap();
                let variant_name = doc.interner.resolve(variant.name.inner).unwrap();
                format!("{}::{}", enum_name, variant_name)
            }
            _ => unreachable!(),
        },
    };

    let hover_contents = HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
        language: "wx".to_string(),
        value: hover_text,
    }));

    let range = Some(Range {
        start: offset_to_position(&state.files, doc.ast.file_id, span_info.span.start),
        end: offset_to_position(&state.files, doc.ast.file_id, span_info.span.end),
    });

    Some(Hover {
        contents: hover_contents,
        range,
    })
}

fn handle_definition(
    state: &ServerState,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = state.documents.get(uri)?;
    let offset = position_to_offset(&state.files, doc.ast.file_id, position)?;

    debug_log!(
        "Definition request at position {:?} (offset: {})",
        position,
        offset
    );

    let span_index = match &doc.symbol_index {
        Some(span_index) => span_index,
        _ => {
            debug_log!("Span index not available for document");
            return None;
        }
    };

    // Find the symbol at the cursor position
    let span_info = span_index.find_at_position(offset)?;

    debug_log!("Found symbol: {:?}", span_info.kind);

    // Find the definition of this symbol
    let def_span = span_index.find_definition(&span_info.kind)?;

    debug_log!(
        "Found definition at span: {}..{}",
        def_span.start,
        def_span.end
    );

    // Convert the definition span to LSP Location
    let range = span_to_range(
        &state.files,
        doc.ast.file_id,
        Some((def_span.start as usize)..(def_span.end as usize)),
    );

    let location = Location {
        uri: uri.clone(),
        range,
    };

    Some(GotoDefinitionResponse::Scalar(location))
}

fn handle_references(state: &ServerState, params: &ReferenceParams) -> Option<Vec<Location>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let doc = state.documents.get(uri)?;
    let offset = position_to_offset(&state.files, doc.ast.file_id, position)?;

    debug_log!(
        "References request at position {:?} (offset: {})",
        position,
        offset
    );

    let span_index = match &doc.symbol_index {
        Some(span_index) => span_index,
        _ => {
            debug_log!("Span index not available for document");
            return None;
        }
    };

    // Find the symbol at the cursor position
    let span_info = span_index.find_at_position(offset)?;

    debug_log!("Found symbol: {:?}", span_info.kind);

    // Find all references to this symbol
    let all_spans = span_index.find_all_references(&span_info.kind);

    if all_spans.is_empty() {
        debug_log!("No references found for symbol {:?}", span_info.kind);
        return None;
    }

    debug_log!("Found {} references", all_spans.len());

    // Convert spans to LSP Locations
    let locations: Vec<Location> = all_spans
        .iter()
        .map(|span| {
            let range = span_to_range(
                &state.files,
                doc.ast.file_id,
                Some((span.start as usize)..(span.end as usize)),
            );
            Location {
                uri: uri.clone(),
                range,
            }
        })
        .collect();

    // If include_declaration is false, filter out the definition
    if !params.context.include_declaration {
        let def_span = span_index.find_definition(&span_info.kind)?;
        let filtered: Vec<Location> = locations
            .into_iter()
            .filter(|loc| {
                let start =
                    position_to_offset(&state.files, doc.ast.file_id, loc.range.start).unwrap_or(0);
                start != def_span.start
            })
            .collect();
        Some(filtered)
    } else {
        Some(locations)
    }
}

fn handle_formatting(
    state: &ServerState,
    params: &DocumentFormattingParams,
) -> Option<Vec<TextEdit>> {
    let uri = &params.text_document.uri;

    debug_log!("Formatting request for document: {:?}", uri);

    let doc = state.documents.get(uri)?;
    let source = &state.files.get(doc.ast.file_id).unwrap().source;

    // Check if there are any error diagnostics - skip formatting if so
    let has_errors = doc.ast.diagnostics.iter().any(|diag| {
        matches!(
            diag.severity,
            codespan_reporting::diagnostic::Severity::Error
        )
    });

    if has_errors {
        debug_log!(
            "Skipping formatting due to {} AST error(s)",
            doc.ast
                .diagnostics
                .iter()
                .filter(|d| matches!(d.severity, codespan_reporting::diagnostic::Severity::Error))
                .count()
        );
        return None;
    }

    // Format the document using the wx-compiler formatter
    let config = wx_compiler::fmt::RendererConfig {
        indent_width: params.options.tab_size as u8,
        max_line_width: 80,
        trailing_comma: true,
    };

    // Catch panics from unimplemented formatter features
    let formatted = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        wx_compiler::fmt::format(&doc.ast, &doc.interner, source, config)
    }));

    let formatted = match formatted {
        Ok(text) => text,
        Err(err) => {
            let message = err
                .downcast_ref::<&str>()
                .copied()
                .or_else(|| err.downcast_ref::<String>().map(|s| s.as_str()))
                .unwrap_or("unknown error");
            debug_log!("Formatter panicked, skipping formatting: {}", message);
            return None;
        }
    };

    debug_log!(
        "Formatted document (original length: {}, formatted length: {})",
        source.len(),
        formatted.len()
    );

    // Create a single TextEdit that replaces the entire document
    let start_pos = Position {
        line: 0,
        character: 0,
    };
    let end_pos = offset_to_position(&state.files, doc.ast.file_id, source.len() as u32);

    let edit = TextEdit {
        range: Range {
            start: start_pos,
            end: end_pos,
        },
        new_text: formatted,
    };

    Some(vec![edit])
}

fn handle_rename(state: &ServerState, params: &RenameParams) -> Option<WorkspaceEdit> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let new_name = &params.new_name;

    debug_log!(
        "Rename request at position {:?} to '{}'",
        position,
        new_name
    );

    let doc = state.documents.get(uri)?;
    let source = &state.files.get(doc.ast.file_id).unwrap().source;
    let offset = position_to_offset(&state.files, doc.ast.file_id, position);
    if offset.is_none() {
        debug_log!("Rename failed: could not convert position to offset");
        return None;
    }
    let offset = offset.unwrap();

    let span_index = match &doc.symbol_index {
        Some(span_index) => span_index,
        _ => {
            debug_log!("Span index not available for document");
            return None;
        }
    };

    // Find the symbol at the cursor position
    let span_info = span_index.find_at_position(offset);
    if span_info.is_none() {
        debug_log!(
            "Rename failed: no symbol found at position (offset: {})",
            offset
        );
        debug_log!("Available entries in span index:");
        for (i, entry) in span_index.entries().iter().take(20).enumerate() {
            let text = entry.span.extract_str(source);
            debug_log!(
                "  Entry {}: {:?} at {}..{} = '{}'",
                i,
                entry.kind,
                entry.span.start,
                entry.span.end,
                text
            );
        }
        return None;
    }
    let span_info = span_info.unwrap();

    debug_log!("Found symbol to rename: {:?}", span_info.kind);

    let symbol_text = span_info.span.extract_str(source);
    if wx_compiler::ast::Keyword::try_from(symbol_text).is_ok() {
        debug_log!("Rename failed: cannot rename keyword '{}'", symbol_text);
        return None;
    }
    if wx_compiler::tir::Type::try_from(symbol_text).is_ok() {
        debug_log!(
            "Rename failed: cannot rename built-in type '{}'",
            symbol_text
        );
        return None;
    }

    // Find all references (includes definition and all usages)
    let all_spans = span_index.find_all_references(&span_info.kind);

    if all_spans.is_empty() {
        debug_log!(
            "Rename failed: no references found for symbol {:?}",
            span_info.kind
        );
        return None;
    }

    debug_log!("Found {} occurrences to rename", all_spans.len());

    // Create text edits for all occurrences
    let edits: Vec<TextEdit> = all_spans
        .iter()
        .map(|span| {
            let range = span_to_range(
                &state.files,
                doc.ast.file_id,
                Some((span.start as usize)..(span.end as usize)),
            );
            TextEdit {
                range,
                new_text: new_name.clone(),
            }
        })
        .collect();

    // Create WorkspaceEdit
    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    })
}

fn namespace_member_completions(
    tir: &wx_compiler::tir::TIR,
    interner: &StringInterner<StringBackend>,
    ns_name: &str,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for ns in &tir.namespaces {
        match ns {
            wx_compiler::tir::Namespace::ImportModule(module) => {
                let name_sym = module
                    .internal_name
                    .as_ref()
                    .map(|n| n.inner)
                    .unwrap_or(module.external_name.inner);
                if interner.resolve(name_sym).unwrap() != ns_name {
                    continue;
                }
                for (member_sym, value) in &module.lookup {
                    let member_name = interner.resolve(*member_sym).unwrap();
                    let (kind, detail) = match value {
                        wx_compiler::tir::ImportValue::Function { func_index } => {
                            let func = &tir.declared_functions[*func_index as usize];
                            let sig = &tir.signatures[func.signature_index as usize];
                            let params = func
                                .params
                                .iter()
                                .map(|p| {
                                    let n = interner.resolve(p.name.inner).unwrap();
                                    let t = format_type(tir, interner, p.ty.inner);
                                    format!("{}: {}", n, t)
                                })
                                .collect::<Vec<_>>()
                                .join(", ");
                            let ret = format_type(tir, interner, sig.result());
                            (
                                CompletionItemKind::FUNCTION,
                                format!("fn {}({}) -> {}", member_name, params, ret),
                            )
                        }
                        wx_compiler::tir::ImportValue::Global { global_index } => {
                            let global = &tir.declared_globals[*global_index as usize];
                            let ty = format_type(tir, interner, global.ty.inner);
                            let mut_prefix = if global.mut_span.is_some() {
                                "mut "
                            } else {
                                ""
                            };
                            (
                                CompletionItemKind::VARIABLE,
                                format!("global {}{}: {}", mut_prefix, member_name, ty),
                            )
                        }
                    };
                    let is_fn = matches!(kind, CompletionItemKind::FUNCTION);
                    items.push(CompletionItem {
                        label: member_name.to_string(),
                        kind: Some(kind),
                        detail: Some(detail),
                        sort_text: Some(format!("1_{}", member_name)),
                        insert_text: is_fn.then(|| format!("{}($0)", member_name)),
                        insert_text_format: is_fn.then_some(InsertTextFormat::SNIPPET),
                        ..Default::default()
                    });
                }
            }
            wx_compiler::tir::Namespace::Enum(enum_) => {
                if interner.resolve(enum_.name.inner).unwrap() != ns_name {
                    continue;
                }
                let enum_name = interner.resolve(enum_.name.inner).unwrap();
                for variant in &enum_.variants {
                    let variant_name = interner.resolve(variant.name.inner).unwrap();
                    items.push(CompletionItem {
                        label: variant_name.to_string(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("{}::{}", enum_name, variant_name)),
                        sort_text: Some(format!("1_{}", variant_name)),
                        ..Default::default()
                    });
                }
            }
        }
    }

    items
}

/// Returns the namespace name if the cursor is immediately after `name::` (or `name::partial`).
/// Only looks within the current line to avoid false positives.
fn namespace_prefix_at(source: &str, offset: u32) -> Option<&str> {
    let before = &source[..offset as usize];
    let line_start = before.rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line = &before[line_start..];
    let colons = line.rfind("::")?;
    let before_colons = &line[..colons];
    let ns_start = before_colons
        .char_indices()
        .rev()
        .take_while(|(_, ch)| ch.is_alphanumeric() || *ch == '_')
        .last()
        .map(|(i, _)| i)?;
    Some(&before_colons[ns_start..])
}

fn handle_completion(state: &ServerState, params: &CompletionParams) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let doc = state.documents.get(uri)?;
    let offset = position_to_offset(&state.files, doc.ast.file_id, position)?;

    debug_log!(
        "Completion request at position {:?} (offset: {})",
        position,
        offset
    );

    let source = &state.files.get(doc.ast.file_id).unwrap().source;

    // If cursor follows `name::`, return only that namespace's members
    if let Some(tir) = &doc.tir {
        if let Some(ns_name) = namespace_prefix_at(source, offset) {
            let items = namespace_member_completions(tir, &doc.interner, ns_name);
            return Some(CompletionResponse::List(CompletionList {
                is_incomplete: true,
                items,
            }));
        }
    }

    let mut items = Vec::new();

    // Add keywords
    for (keyword, detail) in KEYWORDS {
        items.push(CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            sort_text: Some(format!("3_{}", keyword)),
            ..Default::default()
        });
    }

    // Add built-in types
    for (type_name, detail) in BUILTIN_TYPES {
        items.push(CompletionItem {
            label: type_name.to_string(),
            kind: Some(CompletionItemKind::STRUCT),
            detail: Some(detail.to_string()),
            sort_text: Some(format!("3_{}", type_name)),
            ..Default::default()
        });
    }

    let tir = match &doc.tir {
        Some(tir) => tir,
        _ => {
            debug_log!("TIR not available for document");
            return Some(CompletionResponse::List(CompletionList {
                is_incomplete: true,
                items,
            }));
        }
    };

    // Add all functions
    for func in tir.defined_functions.values() {
        let name = doc.interner.resolve(func.name.inner).unwrap();
        let sig = &tir.signatures[func.signature_index as usize];

        let params: Vec<String> = func
            .params
            .iter()
            .map(|p| {
                let param_name = doc.interner.resolve(p.name.inner).unwrap();
                let param_type = format_type(tir, &doc.interner, p.ty.inner);
                format!("{}: {}", param_name, param_type)
            })
            .collect();

        let return_type = format_type(tir, &doc.interner, sig.result());
        let detail = format!("fn({}) -> {}", params.join(", "), return_type);

        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(detail),
            sort_text: Some(format!("2_{}", name)),
            insert_text: Some(format!("{}($0)", name)),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    // Add imported namespaces and enums
    for ns in &tir.namespaces {
        let (name_symbol, kind, detail) = match ns {
            wx_compiler::tir::Namespace::ImportModule(module) => {
                let name_sym = module
                    .internal_name
                    .as_ref()
                    .map(|n| n.inner)
                    .unwrap_or(module.external_name.inner);
                let ext = doc.interner.resolve(module.external_name.inner).unwrap();
                let detail = if module.internal_name.is_some() {
                    let int = doc.interner.resolve(name_sym).unwrap();
                    format!("import {} as {}", ext, int)
                } else {
                    format!("import {}", ext)
                };
                (name_sym, CompletionItemKind::MODULE, detail)
            }
            wx_compiler::tir::Namespace::Enum(enum_) => {
                let name = doc.interner.resolve(enum_.name.inner).unwrap();
                (
                    enum_.name.inner,
                    CompletionItemKind::ENUM,
                    format!("enum {}", name),
                )
            }
        };
        let name = doc.interner.resolve(name_symbol).unwrap();
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(kind),
            detail: Some(detail),
            sort_text: Some(format!("2_{}", name)),
            ..Default::default()
        });
    }

    // Collect local/param names if cursor is inside a function (they shadow globals)
    let mut shadowed_names: std::collections::HashSet<&str> = std::collections::HashSet::new();
    let mut local_items = Vec::new();

    for func in tir.defined_functions.values() {
        let func_start = func.block.span.start;
        let func_end = func.block.span.end;

        if offset >= func_start && offset <= func_end {
            // Collect parameters
            for param in &func.params {
                let name = doc.interner.resolve(param.name.inner).unwrap();
                let type_str = format_type(&tir, &doc.interner, param.ty.inner);
                let mut_keyword = if param.mut_span.is_some() { "mut " } else { "" };

                shadowed_names.insert(name);
                local_items.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: Some(format!("param {}{}: {}", mut_keyword, name, type_str)),
                    sort_text: Some(format!("1_{}", name)),
                    ..Default::default()
                });
            }

            // Collect locals from all scopes in this function
            for scope in &func.stack.scopes {
                for (local_idx, local) in scope.locals.iter().enumerate() {
                    // Skip parameters in scope 0 (already added above)
                    if scope.parent.is_none() && local_idx < func.params.len() {
                        continue;
                    }

                    let name = doc.interner.resolve(local.name.inner).unwrap();
                    let type_str = format_type(&tir, &doc.interner, local.ty);
                    let mut_keyword = if local.mut_span.is_some() { "mut " } else { "" };

                    shadowed_names.insert(name);
                    local_items.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::VARIABLE),
                        detail: Some(format!("local {}{}: {}", mut_keyword, name, type_str)),
                        sort_text: Some(format!("1_{}", name)),
                        ..Default::default()
                    });
                }
            }

            break;
        }
    }

    // Add global variables, skipping any shadowed by a local/param in the current scope
    for global in tir.defined_globals.values() {
        let name = doc.interner.resolve(global.name.inner).unwrap();
        if shadowed_names.contains(name) {
            continue;
        }
        let type_str = format_type(&tir, &doc.interner, global.ty.inner);
        let mut_prefix = if global.mut_span.is_some() {
            "mut "
        } else {
            ""
        };

        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some(format!("global {}{}: {}", mut_prefix, name, type_str)),
            sort_text: Some(format!("2_{}", name)),
            ..Default::default()
        });
    }

    items.extend(local_items);

    debug_log!("Returning {} completion items", items.len());

    Some(CompletionResponse::List(CompletionList {
        is_incomplete: true,
        items,
    }))
}

fn handle_signature_help(
    state: &ServerState,
    params: &SignatureHelpParams,
) -> Option<SignatureHelp> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = state.documents.get(uri)?;
    let source = &state.files.get(doc.ast.file_id).unwrap().source;
    let offset = position_to_offset(&state.files, doc.ast.file_id, position)?;

    debug_log!(
        "Signature help request at position {:?} (offset: {})",
        position,
        offset
    );

    // Find the function call we're in by parsing backwards
    let (func_name_start, func_name_end, active_param) = find_active_call(source, offset as usize)?;

    debug_log!(
        "Found call at {}..{}, active param {}",
        func_name_start,
        func_name_end,
        active_param
    );

    // Extract function name and find it in the TIR
    let func_name_str = &source[func_name_start..func_name_end];

    // Check for a namespace prefix immediately before the function name (e.g. `console::log`)
    let namespace_str = source[..func_name_start]
        .strip_suffix("::")
        .and_then(|before| {
            let ns_end = before.len();
            let ns_start = before
                .char_indices()
                .rev()
                .take_while(|(_, ch)| ch.is_alphanumeric() || *ch == '_')
                .last()
                .map(|(i, _)| i)?;
            Some(&source[ns_start..ns_end])
        });

    let tir = match &doc.tir {
        Some(tir) => tir,
        _ => {
            debug_log!("TIR not available for document");
            return None;
        }
    };

    // Look up the function — either a namespace import or a locally defined function
    let (params, return_type, display_name): (Vec<_>, String, String) =
        if let Some(ns_name) = namespace_str {
            // Find the matching import module namespace
            let func = tir.namespaces.iter().find_map(|ns| {
                let module = match ns {
                    wx_compiler::tir::Namespace::ImportModule(m) => m,
                    _ => return None,
                };
                let module_name = module
                    .internal_name
                    .as_ref()
                    .map(|s| doc.interner.resolve(s.inner).unwrap())
                    .unwrap_or_else(|| doc.interner.resolve(module.external_name.inner).unwrap());
                if module_name != ns_name {
                    return None;
                }
                let member_symbol = doc.interner.get(func_name_str)?;
                let func_index = match module.lookup.get(&member_symbol)? {
                    wx_compiler::tir::ImportValue::Function { func_index } => *func_index,
                    _ => return None,
                };
                Some(&tir.declared_functions[func_index as usize])
            })?;

            let params = func
                .params
                .iter()
                .map(|p| {
                    let name = doc.interner.resolve(p.name.inner).unwrap();
                    let ty = format_type(tir, &doc.interner, p.ty.inner);
                    format!("{}: {}", name, ty)
                })
                .collect();
            let ret = func
                .result
                .as_ref()
                .map(|r| format_type(tir, &doc.interner, r.inner))
                .unwrap_or_else(|| "unit".to_string());
            let display = format!("fn {}({{}}) -> {}", func_name_str, ret);
            (params, ret, display)
        } else {
            // Locally defined function
            let func = tir
                .defined_functions
                .values()
                .find(|f| doc.interner.resolve(f.name.inner).unwrap() == func_name_str)?;
            let sig = &tir.signatures[func.signature_index as usize];
            let params = func
                .params
                .iter()
                .map(|p| {
                    let name = doc.interner.resolve(p.name.inner).unwrap();
                    let ty = format_type(tir, &doc.interner, p.ty.inner);
                    format!("{}: {}", name, ty)
                })
                .collect();
            let ret = format_type(tir, &doc.interner, sig.result());
            let display = format!("fn {}({{}}) -> {}", func_name_str, ret);
            (params, ret, display)
        };

    let signature_label = display_name.replace("{}", &params.join(", "));
    let parameters = params
        .iter()
        .map(|label| ParameterInformation {
            label: ParameterLabel::Simple(label.clone()),
            documentation: None,
        })
        .collect();

    let _ = return_type; // used in display_name already
    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label: signature_label,
            documentation: None,
            parameters: Some(parameters),
            active_parameter: Some(active_param as u32),
        }],
        active_signature: Some(0),
        active_parameter: Some(active_param as u32),
    })
}

/// Find the active function call at the given offset
/// Returns (function_name_start, function_name_end, active_parameter_index)
fn find_active_call(source: &str, offset: usize) -> Option<(usize, usize, usize)> {
    let before_cursor = &source[..offset];

    // Find the opening parenthesis by scanning backwards
    let mut depth = 0;
    let mut paren_pos = None;

    for (i, ch) in before_cursor.char_indices().rev() {
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

    // Find the function name before the parenthesis
    let before_paren = &before_cursor[..paren_pos].trim_end();

    // Scan backwards to find where the identifier starts
    let mut name_start = paren_pos;
    for (i, ch) in before_paren.char_indices().rev() {
        if ch.is_alphanumeric() || ch == '_' {
            name_start = i;
        } else {
            break;
        }
    }

    if name_start >= paren_pos {
        return None;
    }

    let func_name = &before_paren[name_start..];
    if func_name.is_empty() {
        return None;
    }

    // Count commas between the opening paren and cursor to find active param
    let between = &source[paren_pos + 1..offset];
    let mut depth = 0;
    let mut comma_count = 0;

    for ch in between.chars() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => comma_count += 1,
            _ => {}
        }
    }

    Some((name_start, paren_pos, comma_count))
}

fn format_type(
    tir: &wx_compiler::tir::TIR,
    interner: &StringInterner<StringBackend>,
    ty: wx_compiler::tir::Type,
) -> String {
    use wx_compiler::tir::Type;

    match ty {
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Unit => "unit".to_string(),
        Type::Never => "never".to_string(),
        Type::Unknown => "unknown".to_string(),
        Type::Error => "error".to_string(),
        Type::String => "string".to_string(),
        Type::Char => "char".to_string(),
        Type::Namespace { namespace_index } => {
            let ns = &tir.namespaces[namespace_index as usize];
            match ns {
                wx_compiler::tir::Namespace::ImportModule(module) => {
                    let name = interner
                        .resolve(
                            module
                                .internal_name
                                .clone()
                                .map(|m| m.inner)
                                .unwrap_or(module.external_name.inner),
                        )
                        .unwrap();
                    format!("import {}", name)
                }
                wx_compiler::tir::Namespace::Enum(enum_) => {
                    let name = interner.resolve(enum_.name.inner).unwrap();
                    format!("enum {}", name)
                }
            }
        }
        Type::Function { signature_index } => {
            let sig = &tir.signatures[signature_index as usize];
            let params: Vec<String> = sig
                .params()
                .iter()
                .map(|t| format_type(tir, interner, *t))
                .collect();
            let result = format_type(tir, interner, sig.result());
            format!("fn({}) -> {}", params.join(", "), result)
        }
    }
}

fn handle_semantic_tokens(
    state: &ServerState,
    params: &SemanticTokensParams,
) -> Option<SemanticTokensResult> {
    let doc = state.documents.get(&params.text_document.uri)?;
    let tir = doc.tir.as_ref()?;

    let mut tokens = Vec::new();

    // Collect namespace reference tokens (when namespaces are used, not when
    // defined) Traverse all functions to find NamespaceAccess expressions
    for func in tir.defined_functions.values() {
        collect_namespace_tokens(
            &func.block,
            &tir.namespaces,
            &state.files,
            doc.ast.file_id,
            &mut tokens,
        );
    }

    // Sort tokens by position (line, then character)
    tokens.sort_by(|a, b| {
        a.0.line
            .cmp(&b.0.line)
            .then(a.0.character.cmp(&b.0.character))
    });

    // Convert to LSP format (delta-encoded)
    let semantic_tokens = encode_semantic_tokens(&tokens);

    Some(SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
        result_id: None,
        data: semantic_tokens,
    }))
}

/// Recursively traverse expressions to find namespace accesses
fn collect_namespace_tokens(
    expr: &wx_compiler::tir::Expression,
    namespaces: &[wx_compiler::tir::Namespace],
    files: &wx_compiler::ast::Files,
    file_id: wx_compiler::ast::FileId,
    tokens: &mut Vec<(Position, u32, u32)>,
) {
    use wx_compiler::tir::ExprKind;

    match &expr.kind {
        ExprKind::NamespaceAccess {
            namespace_index,
            namespace_span,
            member,
        } => {
            // Determine token type based on namespace type
            let token_type = match &namespaces[*namespace_index as usize] {
                wx_compiler::tir::Namespace::ImportModule(_) => 0, // NAMESPACE
                wx_compiler::tir::Namespace::Enum(_) => 1,         // ENUM
            };

            let pos = offset_to_position(files, file_id, namespace_span.start);
            let length = namespace_span.end - namespace_span.start;
            tokens.push((pos, length, token_type));

            // Continue traversing the member expression
            collect_namespace_tokens(member, namespaces, files, file_id, tokens);
        }
        ExprKind::Int { .. }
        | ExprKind::Float { .. }
        | ExprKind::Bool { .. }
        | ExprKind::Error
        | ExprKind::Placeholder
        | ExprKind::Unreachable
        | ExprKind::Global { .. }
        | ExprKind::Local { .. }
        | ExprKind::Function { .. }
        | ExprKind::EnumVariant { .. }
        | ExprKind::String { .. }
        | ExprKind::Char { .. }
        | ExprKind::ObjectAccess { .. } => {
            // Terminal expressions, no recursion needed
        }
        ExprKind::LocalDeclaration { value, .. } => {
            collect_namespace_tokens(value, namespaces, files, file_id, tokens);
        }
        ExprKind::Unary { operand, .. } => {
            collect_namespace_tokens(operand, namespaces, files, file_id, tokens);
        }
        ExprKind::Binary { left, right, .. } => {
            collect_namespace_tokens(left, namespaces, files, file_id, tokens);
            collect_namespace_tokens(right, namespaces, files, file_id, tokens);
        }
        ExprKind::Call { callee, arguments } => {
            collect_namespace_tokens(callee, namespaces, files, file_id, tokens);
            for arg in arguments.iter() {
                collect_namespace_tokens(arg, namespaces, files, file_id, tokens);
            }
        }

        ExprKind::IfElse {
            condition,
            then_block,
            else_block,
        } => {
            collect_namespace_tokens(condition, namespaces, files, file_id, tokens);
            collect_namespace_tokens(then_block, namespaces, files, file_id, tokens);
            if let Some(else_expr) = else_block {
                collect_namespace_tokens(else_expr, namespaces, files, file_id, tokens);
            }
        }
        ExprKind::Block {
            expressions,
            result,
            ..
        } => {
            for expr in expressions.iter() {
                collect_namespace_tokens(expr, namespaces, files, file_id, tokens);
            }
            if let Some(result_expr) = result {
                collect_namespace_tokens(result_expr, namespaces, files, file_id, tokens);
            }
        }
        ExprKind::Loop { block, .. } => {
            collect_namespace_tokens(block, namespaces, files, file_id, tokens);
        }
        ExprKind::Break { value, .. } => {
            if let Some(val) = value {
                collect_namespace_tokens(val, namespaces, files, file_id, tokens);
            }
        }
        ExprKind::Continue { .. } => {}
        ExprKind::Return { value } => {
            if let Some(val) = value {
                collect_namespace_tokens(val, namespaces, files, file_id, tokens);
            }
        }
    }
}

fn encode_semantic_tokens(tokens: &[(Position, u32, u32)]) -> Vec<SemanticToken> {
    let mut result = Vec::new();
    let mut prev_line = 0;
    let mut prev_char = 0;

    for &(pos, length, token_type) in tokens {
        let delta_line = pos.line - prev_line;
        let delta_start = if delta_line == 0 {
            pos.character - prev_char
        } else {
            pos.character
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: 0,
        });

        prev_line = pos.line;
        prev_char = pos.character;
    }

    result
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
