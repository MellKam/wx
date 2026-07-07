import { LogLevel } from "@codingame/monaco-vscode-api";
import * as monaco from "@codingame/monaco-vscode-editor-api";
import { EditorApp, type EditorAppConfig } from "monaco-languageclient/editorApp";
import {
	LanguageClientWrapper,
	type LanguageClientConfig,
} from "monaco-languageclient/lcwrapper";
import {
	MonacoVscodeApiWrapper,
	type MonacoVscodeApiConfig,
} from "monaco-languageclient/vscodeApiWrapper";
import {
	defineDefaultWorkerLoaders,
	useWorkerFactory,
	type WorkerFactoryConfig,
} from "monaco-languageclient/workerFactory";
import type { ILogger } from "@codingame/monaco-vscode-log-service-override";
import {
	BrowserMessageReader,
	BrowserMessageWriter,
} from "vscode-languageclient/browser.js";
import init, { compile } from "wx-compiler-wasm";
import {
	WX_LANGUAGE_ID,
	configureWxLanguage,
	wxLanguageExtensionConfig,
	wxMonarchLanguage,
} from "./language";
import { WX_DARK_THEME, buildWxDarkThemeData } from "./theme";
import "./style.css";

const ENTRY_URI = "/workspace/main.wx";

const DEFAULT_SOURCE = `fn add(a: i32, b: i32) -> i32 {
	a + b
}

fn main() -> i32 {
	add(2, 3)
}

export { main }
`;

const app = document.querySelector<HTMLDivElement>("#app")!;
app.innerHTML = `
	<div class="toolbar">
		<button id="run">Run ▶</button>
		<span id="status"></span>
	</div>
	<div class="panes">
		<div id="editor"></div>
		<pre id="output"></pre>
	</div>
`;

const outputEl = document.getElementById("output")!;
const statusEl = document.getElementById("status")!;

const log = (line: string, className?: string) => {
	const div = document.createElement("div");
	if (className) div.className = className;
	div.textContent = line;
	outputEl.appendChild(div);
};

const clearOutput = () => {
	outputEl.textContent = "";
};

/** `classic` mode doesn't support the TextMate/extension-host workers —
 * matches monaco-languageclient's own json_classic example. */
const configureClassicWorkerFactory = (logger?: ILogger) => {
	const workerLoaders: WorkerFactoryConfig["workerLoaders"] =
		defineDefaultWorkerLoaders();
	workerLoaders.TextMateWorker = undefined;
	workerLoaders.extensionHostWorkerMain = undefined;
	useWorkerFactory({ workerLoaders, logger });
};

async function main() {
	const htmlContainer = document.getElementById("editor")!;

	const vscodeApiConfig: MonacoVscodeApiConfig = {
		$type: "classic",
		viewsConfig: {
			$type: "EditorService",
			htmlContainer,
		},
		logLevel: LogLevel.Warning,
		monacoWorkerFactory: configureClassicWorkerFactory,
		// We register no real VS Code extensions (our LSP is wired directly
		// via `LanguageClientWrapper`, not through the extension host), so
		// the local extension host + its worker bundle is pure dead weight —
		// this is what pulls in `extensionHost.worker-*.js` (~1.7MB) and the
		// unused `onig-*.wasm` (~460KB) oniguruma binary for TextMate, which
		// we don't use since we're in `'classic'` mode.
		advanced: { loadExtensionServices: false },
	};
	const apiWrapper = new MonacoVscodeApiWrapper(vscodeApiConfig);
	await apiWrapper.start();

	const editorAppConfig: EditorAppConfig = {
		codeResources: {
			modified: {
				text: DEFAULT_SOURCE,
				uri: ENTRY_URI,
			},
		},
		// `editorApp.js` only calls `updateValue('editor.semanticHighlighting.enabled', ...)`
		// when this key is explicitly present — otherwise the setting stays
		// off and the LSP's `textDocument/semanticTokens/full` results never
		// get applied as decorations, even though the provider is registered.
		editorOptions: {
			"semanticHighlighting.enabled": true,
			minimap: { enabled: false },
		},
		languageDef: {
			languageExtensionConfig: wxLanguageExtensionConfig,
			monarchLanguage: wxMonarchLanguage,
			theme: { name: WX_DARK_THEME, data: buildWxDarkThemeData() },
		},
	};
	const editorApp = new EditorApp(editorAppConfig);
	// `languageDef` (incl. `monaco.languages.register(...)`) is only applied
	// inside `start()` — calling `configureWxLanguage` any earlier fails with
	// "Cannot set configuration for unknown language wx".
	await editorApp.start(htmlContainer);
	configureWxLanguage(monaco);

	// The LSP client is wired up independently below and must never block
	// Run — starting the editor and starting the language client are two
	// separate concerns, and a stuck/slow/failed LSP handshake shouldn't
	// take the whole page down with it (which is exactly what happened when
	// this `await`ed inline: a hang here meant the Run button's listener
	// below never even got attached).
	startLanguageClient().catch((error: unknown) => {
		console.error("[wx-lsp] language client failed to start:", error);
	});

	const wasmReady = init();

	async function run() {
		await wasmReady;
		clearOutput();
		statusEl.textContent = "compiling…";

		const source = editorApp.getEditor()!.getValue();
		const result = compile("main.wx", { "main.wx": source });

		if (result.diagnostics.length > 0) {
			for (const d of result.diagnostics) {
				log(
					`[${d.severity}] ${d.message}`,
					`diagnostic-${d.severity.toLowerCase()}`
				);
			}
		}

		if (!result.bytecode) {
			statusEl.textContent = `${result.diagnostics.length} diagnostic(s), nothing to run`;
			return;
		}

		statusEl.textContent = "running…";
		try {
			await runWasm(result.bytecode);
			statusEl.textContent = "done";
		} catch (error) {
			statusEl.textContent = "runtime error";
			log(String(error), "diagnostic-error");
		}
	}

	document.getElementById("run")!.addEventListener("click", () => {
		void run();
	});

	void run();
}

async function startLanguageClient() {
	const worker = new Worker(
		new URL("./worker/wx-lsp-worker.ts", import.meta.url),
		{ type: "module", name: "wx-lsp" }
	);
	worker.onerror = (event) => {
		console.error("[wx-lsp] worker error:", event.message, event);
	};

	const languageClientConfig: LanguageClientConfig = {
		languageId: WX_LANGUAGE_ID,
		clientOptions: {
			documentSelector: [WX_LANGUAGE_ID],
		},
		connection: {
			options: { $type: "WorkerDirect", worker },
			messageTransports: {
				reader: new BrowserMessageReader(worker),
				writer: new BrowserMessageWriter(worker),
			},
		},
	};
	const languageClientWrapper = new LanguageClientWrapper(languageClientConfig);
	console.log("[wx-lsp] starting language client…");
	await languageClientWrapper.start();
	console.log(
		"[wx-lsp] language client started, server capabilities:",
		languageClientWrapper.getLanguageClient()?.initializeResult?.capabilities
	);
}

/** Runs compiled bytecode with a generic `console.log(ptr, len)` host import
 * (the common ABI for `fn log(message: heap::[]u8)`). The exported memory
 * (whatever it's named) is looked up after instantiation — the shim can't
 * assume a name, and if the program never exports one, `console.log` calls
 * simply can't be decoded. If a `main` export exists it's called and its
 * result logged; otherwise the available exports are listed. */
async function runWasm(bytecode: Uint8Array) {
	let memory: WebAssembly.Memory | undefined;

	const importObject = {
		console: {
			log: (ptr: number, len: number) => {
				if (!memory) {
					log("[console.log: module has no exported memory]");
					return;
				}
				const bytes = new Uint8Array(memory.buffer, ptr, len);
				log(new TextDecoder().decode(bytes));
			},
		},
	};

	const module = await WebAssembly.compile(bytecode);
	const instance = await WebAssembly.instantiate(module, importObject);
	const exports = instance.exports as Record<string, unknown>;

	memory = Object.values(exports).find(
		(value): value is WebAssembly.Memory => value instanceof WebAssembly.Memory
	);

	if (typeof exports.main === "function") {
		const result = (exports.main as () => unknown)();
		log(`main() -> ${result}`);
	} else {
		log(`compiled successfully. exports: ${Object.keys(exports).join(", ")}`);
	}
}

void main();
