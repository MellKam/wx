//! `wasm-bindgen` entry point. Drives the exact same `Backend`/`LanguageServer`
//! implementation used natively (via [`wx_lsp::build_service`]), but through a
//! message-oriented bridge instead of stdio.
//!
//! `@codemirror/lsp-client`'s `Transport` type is `{ send(message: string),
//! subscribe(handler: (value: string) => void), unsubscribe(handler) }` —
//! plain JSON strings, no LSP `Content-Length` byte framing and no JS object
//! marshaling — so this drives `LspService`/`ClientSocket` directly instead
//! of going through `tower_lsp_server::Server`, which only speaks the
//! byte-stream framing. Every message crossing the wasm boundary here is
//! just a `String` produced by `serde_json`, so there's no need for
//! `serde_wasm_bindgen` at all (and none of the JS `Map`-vs-object or
//! `undefined`-vs-`null` pitfalls that come with marshaling to real JS
//! objects, since JSON has no `undefined` in the first place).

use std::cell::RefCell;
use std::pin::Pin;
use std::rc::Rc;

use futures::sink::{Sink, SinkExt};
use futures::stream::{Stream, StreamExt};
use tower::Service;
use tower_lsp_server::ExitedError;
use tower_lsp_server::jsonrpc::{Request, Response};
use wasm_bindgen::prelude::*;
use wx_lsp::{Backend, build_service, task};

/// Mirrors `tower_lsp_server::jsonrpc::Message` (kept crate-private
/// upstream) so we can tell incoming client responses (to server-initiated
/// requests, e.g. `workspace/configuration`) apart from incoming client
/// requests/notifications. Order matters for `#[serde(untagged)]`: a
/// `Response` always has an `id` plus a `result`/`error`, while a `Request`
/// may lack both (a notification), so `Response` must be tried first or a
/// response would wrongly parse as a paramless request.
#[derive(serde::Deserialize)]
#[serde(untagged)]
enum IncomingMessage {
	Response(Response),
	Request(Request),
}

/// `ClientSocket::split()`'s halves (`RequestStream`/`ResponseSink`) aren't
/// exported by `tower-lsp-server`, so they can't be named as field types —
/// erase them behind the traits they implement instead.
type BoxedResponseSink = Pin<Box<dyn Sink<Response, Error = ExitedError>>>;

/// Without this, a Rust panic just traps as an opaque `unreachable` with no
/// message or location — this routes it through `console.error` instead.
#[wasm_bindgen(start)]
fn init_panic_hook() {
	console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub struct WxLanguageServer {
	service: Rc<RefCell<tower_lsp_server::LspService<Backend>>>,
	response_sink: Rc<RefCell<BoxedResponseSink>>,
}

#[wasm_bindgen]
impl WxLanguageServer {
	/// `on_message` is called with every message the server sends toward the
	/// client (requests like `client/registerCapability`, notifications like
	/// `textDocument/publishDiagnostics`), JSON-encoded. The caller is
	/// expected to `postMessage` it to the main thread unchanged.
	#[wasm_bindgen(constructor)]
	pub fn new(on_message: js_sys::Function) -> WxLanguageServer {
		let (service, socket) = build_service();
		let (mut request_stream, response_sink): (
			Pin<Box<dyn Stream<Item = Request>>>,
			BoxedResponseSink,
		) = {
			let (stream, sink) = socket.split();
			(Box::pin(stream), Box::pin(sink))
		};

		task::spawn(async move {
			while let Some(req) = request_stream.next().await {
				let json = serde_json::to_string(&req).expect("`Request` always serializes");
				let _ = on_message.call1(&JsValue::NULL, &JsValue::from_str(&json));
			}
		});

		WxLanguageServer {
			service: Rc::new(RefCell::new(service)),
			response_sink: Rc::new(RefCell::new(response_sink)),
		}
	}

	/// Feeds one incoming JSON-RPC message (JSON-encoded, from the client's
	/// `Transport.send`) into the server. Requests/notifications are routed
	/// through `Backend`; responses are routed back to whichever
	/// server-initiated request they answer.
	///
	/// Returns the JSON-encoded JSON-RPC response for a request, or `null`
	/// for notifications and client responses.
	pub async fn handle_message(&self, message: String) -> Result<JsValue, JsValue> {
		let incoming: IncomingMessage = serde_json::from_str(&message)
			.map_err(|err| JsValue::from_str(&err.to_string()))?;

		match incoming {
			IncomingMessage::Request(req) => {
				// Borrowed only long enough to construct the (`'static`,
				// self-contained) future `call` returns — never held across
				// the `.await` below, so an overlapping `handle_message`
				// call can still get its own borrow while this one is
				// pending on real work (e.g. a lock inside `Backend`).
				let fut = self.service.borrow_mut().call(req);
				let response = fut
					.await
					.map_err(|err| JsValue::from_str(&err.to_string()))?;
				match response {
					Some(response) => {
						let json = serde_json::to_string(&response)
							.map_err(|err| JsValue::from_str(&err.to_string()))?;
						Ok(JsValue::from_str(&json))
					}
					None => Ok(JsValue::NULL),
				}
			}
			IncomingMessage::Response(res) => {
				// Held across the `.await`, unlike the branch above — but
				// `ResponseSink::poll_ready`/`poll_flush`/`poll_close` are
				// always immediately `Ready` (it just records the response
				// for whichever `Client::send_request` is awaiting it), so
				// this never actually suspends.
				let mut sink = self.response_sink.borrow_mut();
				let _ = sink.send(res).await;
				Ok(JsValue::NULL)
			}
		}
	}
}
