use std::collections::HashMap;
use std::error::Error;
use std::panic;

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DocumentFormattingParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, LanguageString, Location, MarkedString, NumberOrString, OneOf, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextEdit, Uri,
};
use string_interner::StringInterner;
use string_interner::backend::StringBackend;

mod span_index;

use span_index::SpanIndex;

struct DocumentData {
    source: String,
    ast: wx_compiler::ast::AST,
    tir: wx_compiler::tir::TIR,
    span_index: SpanIndex,
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
    // Write startup message to stderr (stdout is used for LSP protocol)
    eprintln!("WX Language Server starting...");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
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

    eprintln!("Waiting for initialization...");

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
    eprintln!("WX LSP server initialized!");
    eprintln!("Client info: {:?}", params.client_info);

    main_loop(connection, &mut state)?;
    io_threads.join()?;

    eprintln!("WX LSP server shutting down");
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
                eprintln!("info: received request: {:?}", req.method);

                // Try to handle as hover request
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
                        // Other error
                        eprintln!("error: could not cast request: {:?}", err);
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
                        eprintln!("error: could not cast request: {:?}", err);
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
                        eprintln!("error: could not cast request: {:?}", err);
                        continue;
                    }
                };

                // Unknown request type
                eprintln!("warn: received unknown request: {:?}", req.method);
            }
            Message::Response(resp) => {
                eprintln!("info: received response: {:?}", resp);
            }
            Message::Notification(notif) => {
                eprintln!("info: received notification: {}", notif.method);
                match notif.method.as_str() {
                    "textDocument/didOpen" => {
                        if let Ok(params) =
                            cast_notification::<lsp_types::notification::DidOpenTextDocument>(notif)
                        {
                            let uri = params.text_document.uri;
                            let content = params.text_document.text;
                            eprintln!("info: compiling newly opened document");
                            let doc_data = compile_document(uri.clone(), content);

                            // Publish diagnostics
                            let diagnostics = convert_diagnostics(&doc_data);
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
                                    eprintln!(
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
                            eprintln!("info: recompiling document on save");
                            let doc_data = compile_document(uri.clone(), content);

                            // Publish diagnostics
                            let diagnostics = convert_diagnostics(&doc_data);
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

    eprintln!("AST has {} diagnostics", ast.diagnostics.len());
    for diag in &ast.diagnostics {
        eprintln!("  AST diagnostic: {:?}", diag.message);
    }

    let tir = wx_compiler::tir::TIR::build(&ast, &mut interner);

    eprintln!("TIR has {} diagnostics", tir.diagnostics.len());
    for diag in &tir.diagnostics {
        eprintln!("  TIR diagnostic: {:?}", diag.message);
    }

    eprintln!("TIR has {} functions", tir.functions.len());
    for (i, func) in tir.functions.iter().enumerate() {
        eprintln!("  Function {}: {} scopes", i, func.stack.scopes.len());
        for (j, scope) in func.stack.scopes.iter().enumerate() {
            eprintln!("    Scope {}: {} locals", j, scope.locals.len());
            for (k, local) in scope.locals.iter().enumerate() {
                let name = interner.resolve(local.name.inner).unwrap();
                eprintln!(
                    "      Local {}: {} at {}..{}",
                    k, name, local.name.span.start, local.name.span.end
                );
            }
        }
    }

    let span_index = span_index::build_span_index(&tir);

    eprintln!(
        "Built span index with {} entries:",
        span_index.entries().len()
    );
    for (i, entry) in span_index.entries().iter().enumerate() {
        let text = entry.span.extract_str(&content);
        eprintln!(
            "  Entry {}: {:?} {:?} at {}..{} = '{}'",
            i, entry.usage, entry.kind, entry.span.start, entry.span.end, text
        );
    }

    DocumentData {
        source: content,
        ast,
        tir,
        span_index,
        interner,
    }
}

fn convert_diagnostics(doc: &DocumentData) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Convert AST diagnostics
    for diag in &doc.ast.diagnostics {
        let range = span_to_range(&doc.source, diag.labels.get(0).map(|l| l.range.clone()));

        let severity = match diag.severity {
            codespan_reporting::diagnostic::Severity::Error => DiagnosticSeverity::ERROR,
            codespan_reporting::diagnostic::Severity::Warning => DiagnosticSeverity::WARNING,
            codespan_reporting::diagnostic::Severity::Note => DiagnosticSeverity::INFORMATION,
            codespan_reporting::diagnostic::Severity::Help => DiagnosticSeverity::HINT,
            _ => DiagnosticSeverity::ERROR,
        };

        eprintln!(
            "Publishing AST diagnostic: {:?} - {}",
            severity, diag.message
        );

        diagnostics.push(Diagnostic {
            range,
            severity: Some(severity),
            code: diag
                .code
                .as_ref()
                .map(|c| NumberOrString::String(c.clone())),
            source: Some("wx-compiler".to_string()),
            message: diag.message.clone(),
            ..Default::default()
        });
    }

    // Convert TIR diagnostics
    for diag in &doc.tir.diagnostics {
        let range = span_to_range(&doc.source, diag.labels.get(0).map(|l| l.range.clone()));

        let severity = match diag.severity {
            codespan_reporting::diagnostic::Severity::Error => DiagnosticSeverity::ERROR,
            codespan_reporting::diagnostic::Severity::Warning => DiagnosticSeverity::WARNING,
            codespan_reporting::diagnostic::Severity::Note => DiagnosticSeverity::INFORMATION,
            codespan_reporting::diagnostic::Severity::Help => DiagnosticSeverity::HINT,
            _ => DiagnosticSeverity::ERROR,
        };

        eprintln!(
            "Publishing TIR diagnostic: {:?} - {}",
            severity, diag.message
        );

        diagnostics.push(Diagnostic {
            range,
            severity: Some(severity),
            code: diag
                .code
                .as_ref()
                .map(|c| NumberOrString::String(c.clone())),
            source: Some("wx-compiler".to_string()),
            message: diag.message.clone(),
            ..Default::default()
        });
    }

    eprintln!(
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

    eprintln!(
        "Hover request at position {:?} (offset: {})",
        position, offset
    );

    let span_info = doc.span_index.find_at_position(offset)?;

    eprintln!("Found symbol: {:?}", span_info.kind);

    // Format hover information based on symbol kind
    let hover_text = match &span_info.kind {
        span_index::SymbolKind::Function { func_idx } => {
            let func = &doc.tir.functions[*func_idx as usize];
            let name = doc.interner.resolve(func.name.inner).unwrap();
            let sig = &doc.tir.signatures[func.signature_index as usize];

            let params: Vec<String> = func
                .params
                .iter()
                .map(|p| {
                    let param_name = doc.interner.resolve(p.name.inner).unwrap();
                    let param_type = format_type(&doc.tir, &doc.interner, p.ty.inner);
                    let mut_prefix = if p.mut_span.is_some() { "mut " } else { "" };
                    format!("{}{}: {}", mut_prefix, param_name, param_type)
                })
                .collect();

            let return_type = format_type(&doc.tir, &doc.interner, sig.result());

            format!("fn {}({}) -> {}", name, params.join(", "), return_type)
        }
        span_index::SymbolKind::LocalVariable {
            func_idx,
            scope_idx,
            local_idx,
        } => {
            let func = &doc.tir.functions[*func_idx as usize];
            let local = &func.stack.scopes[*scope_idx as usize].locals[*local_idx as usize];
            let name = doc.interner.resolve(local.name.inner).unwrap();
            let type_str = format_type(&doc.tir, &doc.interner, local.ty);
            let mut_keyword = if local.mut_span.is_some() { "mut " } else { "" };

            format!("local {}{}: {}", mut_keyword, name, type_str)
        }
        span_index::SymbolKind::FunctionParam {
            func_idx,
            param_idx,
        } => {
            let func = &doc.tir.functions[*func_idx as usize];
            let param = &func.params[*param_idx as usize];
            let name = doc.interner.resolve(param.name.inner).unwrap();
            let type_str = format_type(&doc.tir, &doc.interner, param.ty.inner);
            let mut_keyword = if param.mut_span.is_some() { "mut " } else { "" };

            format!("local {}{}: {}", mut_keyword, name, type_str)
        }
        span_index::SymbolKind::GlobalVariable { global_idx } => {
            let global = &doc.tir.globals[*global_idx as usize];
            let name = doc.interner.resolve(global.name.inner).unwrap();
            let type_str = format_type(&doc.tir, &doc.interner, global.ty.inner);
            let mut_keyword = if global.mut_span.is_some() {
                "mut "
            } else {
                ""
            };

            format!("global {}{}: {}", mut_keyword, name, type_str)
        }
        span_index::SymbolKind::EnumType { enum_idx } => {
            let enum_def = &doc.tir.enums[*enum_idx as usize];
            let name = doc.interner.resolve(enum_def.name.inner).unwrap();

            format!("enum {}", name)
        }
        span_index::SymbolKind::EnumVariant {
            enum_idx,
            variant_idx,
        } => {
            let enum_def = &doc.tir.enums[*enum_idx as usize];
            let variant = &enum_def.variants[*variant_idx as usize];
            let enum_name = doc.interner.resolve(enum_def.name.inner).unwrap();
            let variant_name = doc.interner.resolve(variant.name.inner).unwrap();

            format!("{}::{}", enum_name, variant_name)
        }
        span_index::SymbolKind::Type { ty } => format_type(&doc.tir, &doc.interner, *ty),
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

    eprintln!(
        "Definition request at position {:?} (offset: {})",
        position, offset
    );

    // Find the symbol at the cursor position
    let span_info = doc.span_index.find_at_position(offset)?;

    eprintln!("Found symbol: {:?}", span_info.kind);

    // Find the definition of this symbol
    let def_span = doc.span_index.find_definition(&span_info.kind)?;

    eprintln!(
        "Found definition at span: {}..{}",
        def_span.start, def_span.end
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

fn handle_formatting(
    state: &ServerState,
    params: &DocumentFormattingParams,
) -> Option<Vec<TextEdit>> {
    let uri = &params.text_document.uri;

    eprintln!("Formatting request for document: {:?}", uri);

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

    eprintln!(
        "Parsed current document for formatting (AST items: {})",
        current_ast.items.len()
    );

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
            eprintln!("Formatter panicked, skipping formatting: {}", message);
            // Return None to indicate formatting is not available
            return None;
        }
    };

    eprintln!(
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
        Type::Enum { enum_index } => {
            let enum_def = &tir.enums[enum_index as usize];
            interner.resolve(enum_def.name.inner).unwrap().to_string()
        }
    }
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
