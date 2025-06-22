use std::error::Error;

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{
    Hover, HoverContents, HoverProviderCapability, InitializeParams, LanguageString, MarkedString,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    })
    .unwrap();

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

    main_loop(connection)?;
    io_threads.join()?;

    eprintln!("info: simple_lsp server shutting down");
    Ok(())
}

fn main_loop(connection: Connection) -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("info: simple_lsp server started");
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("info: received request: {:?}", req.method);
                match cast_request::<lsp_types::request::HoverRequest>(req) {
                    Ok((id, _params)) => {
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
                eprintln!("info: received notification: {:?}", notif.method);
            }
        }
    }
    Ok(())
}

fn cast_request<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
