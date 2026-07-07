<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref, watch } from "vue";
import * as monaco from "./monaco-lite";
import init, { compile } from "wx-compiler-wasm";
import { LspClient } from "./lspClient";
import { openWxDocument, registerWxLanguageFeatures } from "./lspProviders";
import { configureWxLanguage } from "./language";
import { WX_DARK_THEME, buildWxDarkThemeData } from "./theme";
import { registerVirtualFileOpener } from "./virtualFiles";
import { toWat } from "./wat";
import FileExplorer from "./FileExplorer.vue";
import {
	activeFileId,
	findByPath,
	findNode,
	languageForFile,
	listWxFiles,
	nodePath,
	vfsVersion,
	type VfsFile,
} from "./vfs";
import type { RunResponse } from "./worker/run-worker";

// This component owns a lot of state Vue's HMR doesn't know how to tear
// down: Monaco's `languages.register*Provider` calls are global to its own
// registry (not component-scoped, disposed by hand above), and the LSP/run
// Workers are raw browser objects, not reactive state. Vue's normal
// script-change HMR reload re-runs `setup()` in place, so the *new*
// instance's `onMounted` starts a second LSP handshake concurrently with
// whatever the *old* instance's in-flight one was doing, racing over the
// same global Monaco registries — which is consistent with what you'd see
// as "initializing…" logging again but never resolving. Opting this
// component out of in-place HMR in favor of a full reload sidesteps all of
// that by construction: every reload is a genuinely fresh JS realm, so
// there's no old/new instance overlap to race in the first place.
if (import.meta.hot) {
	import.meta.hot.accept(() => {
		location.reload();
	});
}

const editorContainer = ref<HTMLDivElement | null>(null);
const status = ref("");
const viewingVirtualFile = ref(false);
const outputLines = ref<{ text: string; className?: string }[]>([]);
const activeTab = ref<"console" | "wat">("console");
const watText = ref("");

let editor: monaco.editor.IStandaloneCodeEditor | undefined;
let lspClient: LspClient | undefined;
let lspWorker: Worker | undefined;
let runWorker: Worker | undefined;
let lastActiveUserModel: monaco.editor.ITextModel | undefined;
let returnToMainFile = () => {};
let languageFeatures: monaco.IDisposable | undefined;

const models = new Map<string, monaco.editor.ITextModel>();
const openedWxFileIds = new Set<string>();

function log(text: string, className?: string) {
	outputLines.value.push({ text, className });
}

function clearOutput() {
	outputLines.value = [];
}

/** One Monaco model per vfs file, created lazily and kept for the session
 * (there's no delete yet, so nothing ever needs to dispose one early). URI
 * is derived from the file's current tree position, not cached on the file
 * itself — consistent with how `vfs.ts` derives paths. */
function getOrCreateModel(file: VfsFile): monaco.editor.ITextModel {
	const existing = models.get(file.id);
	if (existing) return existing;
	const path = nodePath(file.id) ?? file.name;
	const uri = monaco.Uri.parse(`file:///workspace/${path}`);
	const model = monaco.editor.createModel(file.content, languageForFile(file), uri);
	model.onDidChangeContent(() => {
		file.content = model.getValue();
	});
	models.set(file.id, model);
	return model;
}

/** Every `.wx` file needs wx-lsp to know about it as soon as it exists, not
 * just once it's clicked open — see `openWxDocument`'s doc comment for why.
 * Safe to call repeatedly; `openedWxFileIds` guards against re-registering. */
function ensureWxDocumentsRegistered() {
	if (!lspClient) return;
	for (const { file } of listWxFiles()) {
		if (openedWxFileIds.has(file.id)) continue;
		const model = getOrCreateModel(file);
		openWxDocument(lspClient, model);
		openedWxFileIds.add(file.id);
	}
}

/** Switches the editor to show `file`, unconditionally — called both from
 * the file tree's `open` event (which always fires on click, even for the
 * already-active file — see FileExplorer.vue) and from initial mount. */
function openFileInEditor(file: VfsFile) {
	if (!editor) return;
	const model = getOrCreateModel(file);
	editor.setModel(model);
	editor.updateOptions({ readOnly: false });
	lastActiveUserModel = model;
	viewingVirtualFile.value = false;
}

async function runWithHostScript(bytecode: Uint8Array, script: string) {
	runWorker?.terminate();
	runWorker = new Worker(new URL("./worker/run-worker.ts", import.meta.url), {
		type: "module",
		name: "wx-run",
	});
	runWorker.onmessage = (event: MessageEvent<RunResponse>) => {
		const message = event.data;
		if (message.type === "log") {
			log(message.text, message.level === "error" ? "diagnostic-error" : message.level === "warn" ? "diagnostic-warning" : undefined);
		} else if (message.type === "result") {
			status.value = "done";
		} else {
			status.value = "runtime error";
			log(message.error, "diagnostic-error");
		}
	};
	runWorker.onerror = (event) => {
		status.value = "runtime error";
		log(event.message, "diagnostic-error");
	};
	runWorker.postMessage({ bytecode, script });
}

async function run() {
	if (!editor) return;
	clearOutput();
	watText.value = "";
	status.value = "compiling…";

	const wxFiles = listWxFiles();
	if (!wxFiles.some((f) => f.path === "main.wx")) {
		status.value = "no main.wx";
		log("No main.wx found at the root — create one to compile.", "diagnostic-error");
		return;
	}

	const files = Object.fromEntries(wxFiles.map((f) => [f.path, f.file.content]));
	const result = compile("main.wx", files);

	if (result.diagnostics.length > 0) {
		for (const d of result.diagnostics) {
			log(`[${d.severity}] ${d.message}`, `diagnostic-${d.severity.toLowerCase()}`);
		}
	}

	if (!result.bytecode) {
		status.value = `${result.diagnostics.length} diagnostic(s), nothing to run`;
		return;
	}

	toWat(result.bytecode)
		.then((wat) => {
			watText.value = wat;
		})
		.catch((error: unknown) => {
			watText.value = `failed to disassemble: ${String(error)}`;
		});

	const hostFile = findByPath("host.js");
	if (!hostFile) {
		status.value = "no host.js";
		log("No host.js found at the root — create one to run the compiled module.", "diagnostic-error");
		return;
	}

	status.value = "running…";
	await runWithHostScript(result.bytecode, hostFile.content);
}

async function startLanguageClient(editorInstance: monaco.editor.IStandaloneCodeEditor) {
	lspWorker = new Worker(new URL("./worker/wx-lsp-worker.ts", import.meta.url), {
		type: "module",
		name: "wx-lsp",
	});
	lspWorker.onerror = (event) => {
		console.error("[wx-lsp] worker error:", event.message, event);
	};

	const client = new LspClient(lspWorker);
	client.onNotification<{ type: number; message: string }>("window/logMessage", (params) => {
		if (params.type === 1) console.error("[wx-lsp]", params.message);
		else if (params.type === 2) console.warn("[wx-lsp]", params.message);
	});

	console.log("[wx-lsp] initializing language client…");
	const capabilities = await client.initialize();
	console.log("[wx-lsp] language client initialized, server capabilities:", capabilities);

	lspClient = client;
	languageFeatures = registerWxLanguageFeatures(client);
	ensureWxDocumentsRegistered();

	({ returnToMainFile } = registerVirtualFileOpener(
		editorInstance,
		() => lastActiveUserModel,
		client,
		{
			onEnterVirtualFile: () => {
				viewingVirtualFile.value = true;
			},
			onReturnToMainFile: () => {
				viewingVirtualFile.value = false;
			},
		},
	));
}

onMounted(async () => {
	configureWxLanguage(monaco);
	monaco.editor.defineTheme(WX_DARK_THEME, buildWxDarkThemeData());

	const initialEntry = findNode(activeFileId.value ?? "");
	const initialFile = initialEntry?.node.kind === "file" ? initialEntry.node : undefined;
	const initialModel = initialFile ? getOrCreateModel(initialFile) : undefined;

	editor = monaco.editor.create(editorContainer.value!, {
		model: initialModel,
		theme: WX_DARK_THEME,
		minimap: { enabled: false },
		automaticLayout: true,
	});
	lastActiveUserModel = initialModel;

	// Ctrl/Cmd+S is otherwise unbound in the editor, so the browser's own
	// "Save Page As" dialog handles it by default. Registering our own
	// command for it — rather than the browser intercepting it first — is
	// what actually suppresses that dialog: Monaco's keybinding service
	// calls `preventDefault()` on any keydown it recognizes as a bound
	// command while the editor has focus (the same mechanism that lets it
	// override Ctrl+F for its own find widget instead of the browser's).
	editor.addAction({
		id: "wx.formatDocument",
		label: "Format Document",
		keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS],
		run: (ed) => {
			void ed.getAction("editor.action.formatDocument")?.run();
		},
	});

	watch(vfsVersion, ensureWxDocumentsRegistered);

	// The LSP client is wired up independently below and must never block
	// Run — starting the editor and starting the language client are two
	// separate concerns, and a stuck/slow/failed LSP handshake shouldn't
	// take the whole page down with it.
	startLanguageClient(editor).catch((error: unknown) => {
		console.error("[wx-lsp] language client failed to start:", error);
	});

	await init();
	void run();
});

onBeforeUnmount(() => {
	// `languageFeatures` (the `monaco.languages.register*Provider` calls) are
	// global to Monaco's language registry, not scoped to this component or
	// this LSP client — leaving them registered past unmount means a stale
	// provider still answers hover/completion/etc. by calling `request()` on
	// an already-terminated worker (an immediate rejection, since
	// `Worker.postMessage` throws once terminated), and Monaco merges results
	// from *every* registered provider, so old and new would both fire.
	languageFeatures?.dispose();
	lspWorker?.terminate();
	runWorker?.terminate();
	editor?.dispose();
	models.forEach((model) => model.dispose());
	models.clear();
	openedWxFileIds.clear();
});
</script>

<template>
	<div class="flex h-full font-sans">
		<FileExplorer @open="openFileInEditor" />
		<div class="flex min-w-0 flex-1 flex-col">
			<div class="flex items-center gap-3 border-b border-neutral-700 bg-[#1e1e1e] px-3 py-2">
				<button
					class="cursor-pointer rounded bg-[#0e639c] px-3.5 py-1.5 text-sm text-white hover:bg-[#1177bb]"
					@click="run"
				>
					Run ▶
				</button>
				<span class="text-sm text-neutral-300">{{ status }}</span>
			</div>
			<div class="flex min-h-0 flex-1">
				<div class="flex min-w-0 flex-[3] flex-col">
					<div
						v-if="viewingVirtualFile"
						class="flex items-center gap-3 border-b border-neutral-700 bg-[#3a3d1e] px-3 py-1.5 text-sm text-[#e2c08d]"
					>
						<span>Viewing read-only standard library source</span>
						<button
							class="cursor-pointer rounded border border-[#e2c08d] px-2.5 py-0.5 text-xs hover:bg-[#4a4d28]"
							@click="returnToMainFile"
						>
							← Back to your file
						</button>
					</div>
					<div ref="editorContainer" class="min-h-0 flex-1"></div>
				</div>
				<div class="flex min-w-0 flex-[2] flex-col border-l border-neutral-700 bg-[#1e1e1e]">
					<div class="flex gap-1 border-b border-neutral-700 px-2 pt-2">
						<button
							class="cursor-pointer rounded-t px-3 py-1.5 text-sm"
							:class="
								activeTab === 'console'
									? 'bg-[#1e1e1e] text-white border border-neutral-700 border-b-0'
									: 'text-neutral-400 hover:text-neutral-200'
							"
							@click="activeTab = 'console'"
						>
							Console
						</button>
						<button
							class="cursor-pointer rounded-t px-3 py-1.5 text-sm"
							:class="
								activeTab === 'wat'
									? 'bg-[#1e1e1e] text-white border border-neutral-700 border-b-0'
									: 'text-neutral-400 hover:text-neutral-200'
							"
							@click="activeTab = 'wat'"
						>
							WAT
						</button>
					</div>
					<div
						v-if="activeTab === 'console'"
						class="min-h-0 flex-1 overflow-auto p-3 font-mono text-[0.85rem] whitespace-pre-wrap text-neutral-200"
					>
						<div
							v-for="(line, i) in outputLines"
							:key="i"
							:class="line.className === 'diagnostic-error' ? 'text-[#f14c4c]' : line.className === 'diagnostic-warning' ? 'text-[#cca700]' : ''"
						>
							{{ line.text }}
						</div>
					</div>
					<pre
						v-else
						class="min-h-0 flex-1 overflow-auto p-3 font-mono text-[0.85rem] whitespace-pre text-neutral-200"
					>{{ watText || "(no bytecode compiled yet)" }}</pre>
				</div>
			</div>
		</div>
	</div>
</template>
