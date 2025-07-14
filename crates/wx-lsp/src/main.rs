use std::collections::HashMap;
use std::error::Error;

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    Hover, HoverContents, HoverProviderCapability, InitializeParams, LanguageString, MarkedString,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use string_interner::StringInterner;
use string_interner::backend::StringBackend;

struct ServerState {
    documents: HashMap<Uri, wx_compiler::hir::HIR>,
}

impl ServerState {
    fn new() -> Self {
        ServerState {
            documents: HashMap::new(),
        }
    }
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    })
    .unwrap();

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
    eprint!(
        "info: simple_lsp server initialized with params: {:#?}\n",
        params
    );

    main_loop(connection, &mut state)?;
    io_threads.join()?;

    eprintln!("info: simple_lsp server shutting down");
    Ok(())
}

fn main_loop(
    connection: Connection,
    state: &mut ServerState,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("info: simple_lsp server started");
    let mut interner = string_interner::StringInterner::new();
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("info: received request: {:?}", req.method);
                match cast_request::<lsp_types::request::HoverRequest>(req) {
                    Ok((id, _params)) => {
                        // let x = params.text_document_position_params.position;
                        let hover_contents =
                            HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                                language: "rust".to_string(),
                                value: "i32".to_string(),
                            }));
                        let result = Hover {
                            contents: hover_contents,
                            range: None,
                        };
                        let result = serde_json::to_value(result).ok();
                        let resp = Response {
                            id,
                            result,
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => {
                        // Not a hover request, ignore for now
                        eprintln!("warn: received unknown request: {:?}", req.method);
                    }
                    Err(err) => {
                        // Other error
                        eprintln!("error: could not cast request: {:?}", err);
                    }
                }
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
                            state.documents.insert(
                                uri.clone(),
                                compile_hir(&mut interner, uri, params.text_document.text),
                            );
                        }
                    }
                    "textDocument/didChange" => {
                        if let Ok(params) = cast_notification::<
                            lsp_types::notification::DidChangeTextDocument,
                        >(notif)
                        {
                            let uri = params.text_document.uri;
                            let content = params.content_changes[0].text.clone();
                            state
                                .documents
                                .insert(uri.clone(), compile_hir(&mut interner, uri, content));
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

fn compile_hir(
    mut interner: &mut StringInterner<StringBackend>,
    uri: Uri,
    content: String,
) -> wx_compiler::hir::HIR {
    let mut files = wx_compiler::files::Files::new();
    let file_name = uri.path().segments().last().unwrap().as_str().to_string();
    let main_file = files.add(file_name, content).unwrap();
    let (ast, _) = wx_compiler::ast::parser::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );
    let (hir, _) = wx_compiler::hir::Builder::build(&ast, &mut interner);
    hir
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
