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
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag,
    DocumentFormattingParams, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, LanguageString, Location, MarkedString,
    NumberOrString, OneOf, ParameterInformation, ParameterLabel, Position,
    PublishDiagnosticsParams, Range, ReferenceParams, RenameParams, SemanticToken,
    SemanticTokenType, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, SignatureHelp, SignatureHelpOptions, SignatureHelpParams,
    SignatureInformation, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri,
    WorkDoneProgressOptions, WorkspaceEdit,
};
use string_interner::StringInterner;
use string_interner::backend::StringBackend;

mod span_index;

use span_index::SpanIndex;

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
    source: String,
    ast: wx_compiler::ast::AST,
    tir: Option<wx_compiler::tir::TIR>,
    span_index: Option<SpanIndex>,
    interner: StringInterner<StringBackend>,
}

struct ServerState {
    documents: HashMap<Uri, DocumentData>,
}

impl ServerState {
    fn new() -> Self {
        ServerState {
            documents: HashMap::new(),
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
                            debug_log!("info: compiling newly opened document");
                            let doc_data = compile_document(uri.clone(), content);

                            // Publish diagnostics
                            let diagnostics = convert_diagnostics(&doc_data, &uri);
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

                            // Update the document source with the latest changes
                            // We don't recompile yet (wait for save), but we store the updated text
                            // so formatting can work on the current content
                            if let Some(change) = params.content_changes.first() {
                                if let Some(doc) = state.documents.get_mut(&uri) {
                                    debug_log!(
                                        "info: updating document source (length: {} -> {})",
                                        doc.source.len(),
                                        change.text.len()
                                    );
                                    doc.source = change.text.clone();
                                }
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
                            let doc_data = compile_document(uri.clone(), content);

                            // Publish diagnostics
                            let diagnostics = convert_diagnostics(&doc_data, &uri);
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

fn compile_document(uri: Uri, content: String) -> DocumentData {
    let mut interner = StringInterner::new();
    let mut files = wx_compiler::ast::Files::new();
    let file_name = uri.path().segments().last().unwrap().as_str().to_string();
    let main_file = files.add(file_name, content.clone()).unwrap();
    let ast = wx_compiler::ast::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );

    if ast.diagnostics.iter().any(|d| match d.severity {
        codespan_reporting::diagnostic::Severity::Error
        | codespan_reporting::diagnostic::Severity::Bug => true,
        _ => false,
    }) {
        return DocumentData {
            source: content,
            ast,
            tir: None,
            span_index: None,
            interner,
        };
    }

    let tir = wx_compiler::tir::TIR::build(&ast, &mut interner);
    let span_index = span_index::build_span_index(&tir);

    debug_log!(
        "Built span index with {} entries:",
        span_index.entries().len()
    );
    for (i, entry) in span_index.entries().iter().enumerate() {
        debug_log!(
            "  Entry {}: {:?} {:?} at {}..{} = '{}'",
            i,
            entry.usage,
            entry.kind,
            entry.span.start,
            entry.span.end,
            entry.span.extract_str(&content)
        );
    }

    DocumentData {
        source: content,
        ast,
        tir: Some(tir),
        span_index: Some(span_index),
        interner,
    }
}

fn convert_diagnostic(
    doc: &DocumentData,
    uri: &Uri,
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
    let range = span_to_range(&doc.source, primary_label.map(|l| l.range.clone()));

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

        let label_range = span_to_range(&doc.source, Some(label.range.clone()));

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

fn convert_diagnostics(doc: &DocumentData, uri: &Uri) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for diagnostic in &doc.ast.diagnostics {
        diagnostics.push(convert_diagnostic(doc, uri, diagnostic));
    }
    if let Some(tir) = &doc.tir {
        for diagnostic in &tir.diagnostics {
            diagnostics.push(convert_diagnostic(doc, uri, diagnostic));
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

fn span_to_range(source: &str, span: Option<core::ops::Range<usize>>) -> Range {
    if let Some(span) = span {
        let start = offset_to_position(source, span.start as u32);
        let end = offset_to_position(source, span.end as u32);
        Range { start, end }
    } else {
        // Default to line 0 if no span
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        }
    }
}

fn offset_to_position(source: &str, offset: u32) -> Position {
    let mut line = 0u32;
    let mut current_offset = 0u32;

    for line_text in source.lines() {
        let line_len = line_text.len() as u32 + 1; // +1 for newline
        if current_offset + line_len > offset {
            let character = offset - current_offset;
            return Position { line, character };
        }
        current_offset += line_len;
        line += 1;
    }

    // End of file
    Position {
        line: line.max(1) - 1,
        character: 0,
    }
}

fn position_to_offset(position: Position, source: &str) -> Option<u32> {
    let mut offset = 0u32;
    let mut current_line = 0u32;

    for line in source.lines() {
        if current_line == position.line {
            let char_offset = position.character.min(line.len() as u32);
            return Some(offset + char_offset);
        }
        offset += line.len() as u32 + 1; // +1 for newline
        current_line += 1;
    }

    // If we're past all lines, return None
    if current_line == position.line {
        Some(offset)
    } else {
        None
    }
}

fn handle_hover(state: &ServerState, params: &HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = state.documents.get(uri)?;
    let offset = position_to_offset(position, &doc.source)?;

    debug_log!(
        "Hover request at position {:?} (offset: {})",
        position,
        offset
    );

    let (tir, span_index) = match (&doc.tir, &doc.span_index) {
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
        span_index::SymbolKind::Function { func_idx } => {
            let func = &tir.declared_functions[*func_idx as usize];
            let name = doc.interner.resolve(func.name.inner).unwrap();
            let sig = &tir.signatures[func.signature_index as usize];

            let params: String = sig
                .params()
                .iter()
                .copied()
                .map(|ty| format_type(&tir, &doc.interner, ty))
                .collect::<Box<[_]>>()
                .join(", ");

            let return_type = format_type(&tir, &doc.interner, sig.result());

            format!("fn {}({}) -> {}", name, params, return_type)
        }
        span_index::SymbolKind::LocalVariable {
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
        span_index::SymbolKind::FunctionParam {
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
        span_index::SymbolKind::GlobalVariable { global_idx } => {
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
        span_index::SymbolKind::Type { ty } => format_type(&tir, &doc.interner, *ty),
        span_index::SymbolKind::EnumVariant {
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

    Some(Hover {
        contents: hover_contents,
        range: None,
    })
}

fn handle_definition(
    state: &ServerState,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = state.documents.get(uri)?;
    let offset = position_to_offset(position, &doc.source)?;

    debug_log!(
        "Definition request at position {:?} (offset: {})",
        position,
        offset
    );

    let span_index = match &doc.span_index {
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
        &doc.source,
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
    let offset = position_to_offset(position, &doc.source)?;

    debug_log!(
        "References request at position {:?} (offset: {})",
        position,
        offset
    );

    let span_index = match &doc.span_index {
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
                &doc.source,
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
                let start = position_to_offset(loc.range.start, &doc.source).unwrap_or(0);
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

    // Parse the current source to get an up-to-date AST for formatting
    // (the stored AST might be outdated if the user has made changes)
    let mut temp_interner = doc.interner.clone();
    let mut files = wx_compiler::ast::Files::new();
    let file_name = uri.path().segments().last().unwrap().as_str().to_string();
    let main_file = files.add(file_name, doc.source.clone()).unwrap();
    let current_ast = wx_compiler::ast::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut temp_interner,
    );

    debug_log!(
        "Parsed current document for formatting (AST items: {})",
        current_ast.items.len()
    );

    // Check if there are any error diagnostics - skip formatting if so
    let has_errors = current_ast.diagnostics.iter().any(|diag| {
        matches!(
            diag.severity,
            codespan_reporting::diagnostic::Severity::Error
        )
    });

    if has_errors {
        debug_log!(
            "Skipping formatting due to {} AST error(s)",
            current_ast
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
        wx_compiler::fmt::format(&current_ast, &temp_interner, &doc.source, config)
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
            // Return None to indicate formatting is not available
            return None;
        }
    };

    debug_log!(
        "Formatted document (original length: {}, formatted length: {})",
        doc.source.len(),
        formatted.len()
    );

    // Create a single TextEdit that replaces the entire document
    let start_pos = Position {
        line: 0,
        character: 0,
    };

    // Find the end position (last line, last character)
    let line_count = doc.source.lines().count() as u32;
    let last_line = doc.source.lines().last().unwrap_or("");
    let end_pos = Position {
        line: line_count.saturating_sub(1),
        character: last_line.len() as u32,
    };

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

    let doc = state.documents.get(uri);
    if doc.is_none() {
        debug_log!("Rename failed: document not found");
        return None;
    }
    let doc = doc.unwrap();

    let offset = position_to_offset(position, &doc.source);
    if offset.is_none() {
        debug_log!("Rename failed: could not convert position to offset");
        return None;
    }
    let offset = offset.unwrap();

    let span_index = match &doc.span_index {
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
            let text = entry.span.extract_str(&doc.source);
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

    let symbol_text = span_info.span.extract_str(&doc.source);
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
                &doc.source,
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

fn handle_completion(state: &ServerState, params: &CompletionParams) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let doc = state.documents.get(uri)?;
    let offset = position_to_offset(position, &doc.source)?;

    debug_log!(
        "Completion request at position {:?} (offset: {})",
        position,
        offset
    );

    let mut items = Vec::new();

    // Add keywords
    for (keyword, detail) in KEYWORDS {
        items.push(CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            ..Default::default()
        });
    }

    // Add built-in types
    for (type_name, detail) in BUILTIN_TYPES {
        items.push(CompletionItem {
            label: type_name.to_string(),
            kind: Some(CompletionItemKind::STRUCT),
            detail: Some(detail.to_string()),
            ..Default::default()
        });
    }

    let tir = match &doc.tir {
        Some(tir) => tir,
        _ => {
            debug_log!("TIR not available for document");
            return None;
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
            ..Default::default()
        });
    }

    // Add global variables
    for global in tir.defined_globals.values() {
        let name = doc.interner.resolve(global.name.inner).unwrap();
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
            ..Default::default()
        });
    }

    // Try to find locals in scope at the cursor position
    // We need to determine which function we're in
    for func in tir.defined_functions.values() {
        // Check if cursor is within this function's body
        let func_start = func.block.span.start;
        let func_end = func.block.span.end;

        if offset >= func_start && offset <= func_end {
            // Add parameters
            for param in &func.params {
                let name = doc.interner.resolve(param.name.inner).unwrap();
                let type_str = format_type(&tir, &doc.interner, param.ty.inner);
                let mut_keyword = if param.mut_span.is_some() { "mut " } else { "" };

                items.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: Some(format!("param {}{}: {}", mut_keyword, name, type_str)),
                    ..Default::default()
                });
            }

            // Add locals from all scopes in this function
            for scope in &func.stack.scopes {
                for (local_idx, local) in scope.locals.iter().enumerate() {
                    // Skip parameters in scope 0 (already added above)
                    if scope.parent.is_none() && local_idx < func.params.len() {
                        continue;
                    }

                    let name = doc.interner.resolve(local.name.inner).unwrap();
                    let type_str = format_type(&tir, &doc.interner, local.ty);
                    let mut_keyword = if local.mut_span.is_some() { "mut " } else { "" };

                    items.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::VARIABLE),
                        detail: Some(format!("local {}{}: {}", mut_keyword, name, type_str)),
                        ..Default::default()
                    });
                }
            }

            break;
        }
    }

    debug_log!("Returning {} completion items", items.len());

    Some(CompletionResponse::Array(items))
}

fn handle_signature_help(
    state: &ServerState,
    params: &SignatureHelpParams,
) -> Option<SignatureHelp> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = state.documents.get(uri)?;
    let offset = position_to_offset(position, &doc.source)?;

    debug_log!(
        "Signature help request at position {:?} (offset: {})",
        position,
        offset
    );

    // Find the function call we're in by parsing backwards
    let (func_name_start, func_name_end, active_param) =
        find_active_call(&doc.source, offset as usize)?;

    debug_log!(
        "Found call at {}..{}, active param {}",
        func_name_start,
        func_name_end,
        active_param
    );

    // Extract function name and find it in the TIR
    let func_name_str = &doc.source[func_name_start..func_name_end];

    let tir = match &doc.tir {
        Some(tir) => tir,
        _ => {
            debug_log!("TIR not available for document");
            return None;
        }
    };

    // Try to find this function in the TIR
    // TODO: need to handle imported functions as well, currently only defined
    // functions are supported
    for func in tir.defined_functions.values() {
        let name = doc.interner.resolve(func.name.inner).unwrap();
        if name == func_name_str {
            let sig = &tir.signatures[func.signature_index as usize];

            // Build parameter information
            let mut parameters = Vec::new();
            for param in &func.params {
                let param_name = doc.interner.resolve(param.name.inner).unwrap();
                let param_type = format_type(&tir, &doc.interner, param.ty.inner);
                let param_label = format!("{}: {}", param_name, param_type);

                parameters.push(ParameterInformation {
                    label: ParameterLabel::Simple(param_label),
                    documentation: None,
                });
            }

            let return_type = format_type(&tir, &doc.interner, sig.result());
            let param_labels: Vec<String> = func
                .params
                .iter()
                .map(|p| {
                    let param_name = doc.interner.resolve(p.name.inner).unwrap();
                    let param_type = format_type(&tir, &doc.interner, p.ty.inner);
                    format!("{}: {}", param_name, param_type)
                })
                .collect();

            let signature_label = format!(
                "fn {}({}) -> {}",
                name,
                param_labels.join(", "),
                return_type
            );

            let signature = SignatureInformation {
                label: signature_label,
                documentation: None,
                parameters: Some(parameters),
                active_parameter: Some(active_param as u32),
            };

            return Some(SignatureHelp {
                signatures: vec![signature],
                active_signature: Some(0),
                active_parameter: Some(active_param as u32),
            });
        }
    }

    debug_log!("Function '{}' not found in TIR", func_name_str);
    None
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
        Type::Namespace { namespace_index } => {
            let ns = &tir.namespaces[namespace_index as usize];
            let name = match ns {
                wx_compiler::tir::Namespace::ImportModule(module) => interner
                    .resolve(
                        module
                            .internal_name
                            .clone()
                            .map(|m| m.inner)
                            .unwrap_or(module.external_name.inner),
                    )
                    .unwrap(),
                wx_compiler::tir::Namespace::Enum(enum_) => {
                    interner.resolve(enum_.name.inner).unwrap()
                }
            };
            format!("namespace {}", name)
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
        collect_namespace_tokens(&func.block, &tir.namespaces, &doc.source, &mut tokens);
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
    source: &str,
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

            let pos = offset_to_position(source, namespace_span.start);
            let length = namespace_span.end - namespace_span.start;
            tokens.push((pos, length, token_type));

            // Continue traversing the member expression
            collect_namespace_tokens(member, namespaces, source, tokens);
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
        | ExprKind::ObjectAccess { .. } => {
            // Terminal expressions, no recursion needed
        }
        ExprKind::LocalDeclaration { value, .. } => {
            collect_namespace_tokens(value, namespaces, source, tokens);
        }
        ExprKind::Unary { operand, .. } => {
            collect_namespace_tokens(operand, namespaces, source, tokens);
        }
        ExprKind::Binary { left, right, .. } => {
            collect_namespace_tokens(left, namespaces, source, tokens);
            collect_namespace_tokens(right, namespaces, source, tokens);
        }
        ExprKind::Call { callee, arguments } => {
            collect_namespace_tokens(callee, namespaces, source, tokens);
            for arg in arguments.iter() {
                collect_namespace_tokens(arg, namespaces, source, tokens);
            }
        }

        ExprKind::IfElse {
            condition,
            then_block,
            else_block,
        } => {
            collect_namespace_tokens(condition, namespaces, source, tokens);
            collect_namespace_tokens(then_block, namespaces, source, tokens);
            if let Some(else_expr) = else_block {
                collect_namespace_tokens(else_expr, namespaces, source, tokens);
            }
        }
        ExprKind::Block {
            expressions,
            result,
            ..
        } => {
            for expr in expressions.iter() {
                collect_namespace_tokens(expr, namespaces, source, tokens);
            }
            if let Some(result_expr) = result {
                collect_namespace_tokens(result_expr, namespaces, source, tokens);
            }
        }
        ExprKind::Loop { block, .. } => {
            collect_namespace_tokens(block, namespaces, source, tokens);
        }
        ExprKind::Break { value, .. } => {
            if let Some(val) = value {
                collect_namespace_tokens(val, namespaces, source, tokens);
            }
        }
        ExprKind::Continue { .. } => {}
        ExprKind::Return { value } => {
            if let Some(val) = value {
                collect_namespace_tokens(val, namespaces, source, tokens);
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
