import type {
	InitializeParams,
	InitializeResult,
	NotificationMessage,
	RequestMessage,
	ResponseMessage,
	ServerCapabilities,
} from "vscode-languageserver-protocol";

type NotificationHandler<P = unknown> = (params: P) => void;

type IncomingMessage = ResponseMessage | NotificationMessage | RequestMessage;

/** Structurally matches `monaco.CancellationToken` (and vscode's), without
 * importing either — this module has no editor dependency otherwise. */
export interface CancellationToken {
	isCancellationRequested: boolean;
	onCancellationRequested(listener: () => void): { dispose(): void };
}

/** Monaco's own provider-call machinery checks for this exact shape
 * (`isCancellationError` in `vs/base/common/errors.js`: `error.name ===
 * "Canceled"`) to decide a rejection is an expected cancellation rather
 * than a bug worth logging — constructing it this way is what makes a
 * cancelled request fail silently instead of surfacing as an unhandled
 * rejection in the console. */
function canceledError(): Error {
	return Object.assign(new Error("Canceled"), { name: "Canceled" });
}

interface PendingRequest {
	resolve: (value: unknown) => void;
	reject: (error: unknown) => void;
	cancelSubscription?: { dispose(): void };
}

/**
 * A minimal hand-rolled JSON-RPC client talking to `wx-lsp-wasm` over a
 * Worker, in place of `vscode-languageclient`/`monaco-languageclient`. Those
 * packages buy you automatic wiring from an LSP connection to Monaco's
 * `languages.register*Provider` calls, routed through a full VS Code
 * extension-host emulation — for one hand-written language integration,
 * that machinery costs far more in bundle weight than it saves in glue
 * code, so this reimplements just the JSON-RPC bookkeeping (request ids,
 * pending promises, notification dispatch) directly.
 */
export class LspClient {
	private nextId = 1;
	private pending = new Map<number, PendingRequest>();
	private notificationHandlers = new Map<string, NotificationHandler>();
	capabilities: ServerCapabilities | null = null;

	constructor(private worker: Worker) {
		worker.addEventListener("message", (event: MessageEvent<string>) => {
			this.handleMessage(JSON.parse(event.data) as IncomingMessage);
		});
	}

	private handleMessage(message: IncomingMessage): void {
		if (!("method" in message)) {
			// A `ResponseMessage`: always has an `id` plus `result`/`error`.
			const pending = this.pending.get(message.id as number);
			if (!pending) return;
			this.pending.delete(message.id as number);
			pending.cancelSubscription?.dispose();
			if (message.error) pending.reject(message.error);
			else pending.resolve(message.result);
			return;
		}
		if ("id" in message && message.id != null) {
			// A server-initiated request. wx-lsp doesn't currently send any,
			// but responding with "method not found" keeps the server from
			// waiting forever on a reply that will never come.
			this.worker.postMessage(
				JSON.stringify({
					jsonrpc: "2.0",
					id: message.id,
					error: { code: -32601, message: "Method not implemented" },
				}),
			);
			return;
		}
		this.notificationHandlers.get(message.method)?.(message.params);
	}

	notify(method: string, params: unknown): void {
		this.worker.postMessage(JSON.stringify({ jsonrpc: "2.0", method, params }));
	}

	/**
	 * `token`, when given, lets Monaco's own request lifecycle drive
	 * cancellation: when it fires, the pending entry is dropped, a
	 * `$/cancelRequest` notification is sent (in case the server ever grows
	 * the ability to act on it), and the returned promise rejects with
	 * Monaco's own cancellation error shape immediately, rather than
	 * waiting on a response nothing cares about anymore.
	 */
	request<Result>(method: string, params: unknown, token?: CancellationToken): Promise<Result> {
		const id = this.nextId++;
		return new Promise<Result>((resolve, reject) => {
			const entry: PendingRequest = { resolve: resolve as (value: unknown) => void, reject };
			if (token) {
				entry.cancelSubscription = token.onCancellationRequested(() => {
					if (this.pending.delete(id)) {
						this.notify("$/cancelRequest", { id });
						reject(canceledError());
					}
				});
			}
			this.pending.set(id, entry);
			this.worker.postMessage(JSON.stringify({ jsonrpc: "2.0", id, method, params }));
		});
	}

	onNotification<P>(method: string, handler: NotificationHandler<P>): void {
		this.notificationHandlers.set(method, handler as NotificationHandler);
	}

	/** Runs the LSP `initialize`/`initialized` handshake and returns the
	 * server's advertised capabilities. */
	async initialize(): Promise<ServerCapabilities> {
		const params: InitializeParams = {
			processId: null,
			clientInfo: { name: "wx-playground" },
			rootUri: null,
			capabilities: {},
			workspaceFolders: null,
		};
		const result = await this.request<InitializeResult>("initialize", params);
		this.capabilities = result.capabilities;
		this.notify("initialized", {});
		return result.capabilities;
	}
}
