import * as fs from "node:fs";
import * as path from "node:path";
import {
	workspace,
	ExtensionContext,
	ExtensionMode,
	commands,
	window,
	Uri,
	FileSystemWatcher,
	EventEmitter,
	TextDocument,
} from "vscode";
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
} from "vscode-languageclient/node";
import {
	AnsiDecorationProvider,
	DIAGNOSTICS_VIEW_SCHEME,
	stripAnsi,
} from "./ansiDecorations";

let client: LanguageClient | null = null;

const resolveServerBinary = (context: ExtensionContext): string => {
	if (context.extensionMode === ExtensionMode.Development) {
		return path.resolve(
			context.extensionPath,
			"..",
			"target",
			"debug",
			process.platform === "win32" ? "wx-lsp.exe" : "wx-lsp",
		);
	}

	return context.asAbsolutePath(
		path.join("bin", process.platform === "win32" ? "wx-lsp.exe" : "wx-lsp"),
	);
};

let fileWatcher: FileSystemWatcher | null = null;

// Fired with a `wx-diagnostics-view` URI whenever the diagnostic behind it
// gets republished, so an already-open virtual doc for that URI re-fetches
// instead of showing whatever was cached the first time it was opened —
// otherwise editing a file that shuffles diagnostics around leaves a stale
// open tab pointed at old content forever, since VS Code only re-invokes
// `provideTextDocumentContent` for a URI already backing an open document
// when told to via this event (a no-op if nothing has it open).
const diagnosticsViewChanged = new EventEmitter<Uri>();

// Re-parses the same raw ANSI text `wx/fullDiagnostic` returns (the content
// provider below strips it for the document's plain text) into
// `TextEditorDecorationType`s, so a "click for full compiler diagnostic"
// view is colored the same way `wx-cli` colors it in a real terminal.
const decorationProvider = new AnsiDecorationProvider(async (uri) => {
	if (!client) return null;
	return client.sendRequest<string>("wx/fullDiagnostic", {
		uri: uri.fragment,
		index: Number(uri.query),
	});
});

async function decorateVisibleEditors(document: TextDocument) {
	for (const editor of window.visibleTextEditors) {
		if (editor.document === document) {
			await decorationProvider.provideDecorations(editor);
		}
	}
}

async function startServer(serverModule: string) {
	if (!fs.existsSync(serverModule)) {
		window.showErrorMessage(
			`WX Language Server binary not found at: ${serverModule}`,
		);
		return;
	}

	const serverOptions: ServerOptions = {
		command: serverModule,
		transport: TransportKind.stdio,
	};

	fileWatcher?.dispose();
	fileWatcher = workspace.createFileSystemWatcher("**/*.wx");

	const clientOptions: LanguageClientOptions = {
		documentSelector: [
			{ scheme: "file", language: "wx" },
			{ scheme: "wx", language: "wx" },
		],
		synchronize: {
			fileEvents: fileWatcher,
		},
		outputChannelName: "WX Language Server",
		middleware: {
			handleDiagnostics(uri, diagnosticList, next) {
				diagnosticList.forEach((diag, idx) => {
					if (diag.source !== "wx" || !diag.code) return;
					const target = Uri.from({
						scheme: DIAGNOSTICS_VIEW_SCHEME,
						path: `/diagnostic-${idx}`,
						fragment: uri.toString(),
						query: idx.toString(),
					});
					diag.code = {
						target,
						value: typeof diag.code === "object" ? diag.code.value : diag.code,
					};
					// No-op if this URI isn't currently backing an open
					// document; otherwise makes an already-open virtual doc
					// re-fetch instead of staying pinned to stale content.
					diagnosticsViewChanged.fire(target);
				});
				next(uri, diagnosticList);
			},
		},
	};

	client = new LanguageClient(
		"wx-lsp",
		"WX Language Server",
		serverOptions,
		clientOptions,
	);

	try {
		await client.start();
	} catch (error) {
		const action = await window.showErrorMessage(
			`Failed to start WX Language Server: ${error}`,
			"Open Output",
		);
		if (action === "Open Output") {
			client.outputChannel.show();
		}
	}
}

export function activate(context: ExtensionContext) {
	const serverModule = resolveServerBinary(context);
	const restartCommand = commands.registerCommand(
		"wx-vscode.restartServer",
		async () => {
			window.showInformationMessage("Restarting WX Language Server...");

			if (client) {
				await client.stop();
			}

			await startServer(serverModule);
		},
	);

	const configListener = workspace.onDidChangeConfiguration((e) => {
		if (e.affectsConfiguration("wx")) {
			commands.executeCommand("wx-vscode.restartServer");
		}
	});

	context.subscriptions.push(restartCommand, configListener);
	startServer(serverModule);

	// Provide content for wx:// virtual stdlib URIs (e.g. wx://std/lib.wx)
	context.subscriptions.push(
		workspace.registerTextDocumentContentProvider("wx", {
			provideTextDocumentContent: (uri: Uri) => {
				if (!client) return null;
				return client.sendRequest("wx/virtualFileContent", {
					uri: uri.toString(),
				});
			},
		}),
	);

	// Provide the full rendered diagnostic text for the "Click for full
	// compiler diagnostic" links installed by the handleDiagnostics
	// middleware above. The document's own text is plain (ANSI codes
	// stripped) — coloring is applied separately as decorations, since VS
	// Code can't render ANSI escapes in a regular editor buffer.
	context.subscriptions.push(
		diagnosticsViewChanged,
		workspace.registerTextDocumentContentProvider(DIAGNOSTICS_VIEW_SCHEME, {
			onDidChange: diagnosticsViewChanged.event,
			provideTextDocumentContent: async (uri: Uri) => {
				if (!client) return null;
				const raw = await client.sendRequest<string>("wx/fullDiagnostic", {
					uri: uri.fragment,
					index: Number(uri.query),
				});
				return stripAnsi(raw);
			},
		}),
	);

	// Filtered to `DIAGNOSTICS_VIEW_SCHEME` right here, before touching
	// `visibleTextEditors` or awaiting anything — these first two fire on
	// every edit/open of *any* document in the editor, not just diagnostics
	// views, so bailing a line earlier than `provideDecorations` would
	// anyway matters here in a way it doesn't for the other two (which only
	// fire on tab/visibility changes, not per keystroke).
	context.subscriptions.push(
		decorationProvider,
		workspace.onDidChangeTextDocument((event) => {
			if (event.document.uri.scheme !== DIAGNOSTICS_VIEW_SCHEME) return;
			decorateVisibleEditors(event.document);
		}),
		workspace.onDidOpenTextDocument((document) => {
			if (document.uri.scheme !== DIAGNOSTICS_VIEW_SCHEME) return;
			decorateVisibleEditors(document);
		}),
		window.onDidChangeActiveTextEditor(async (editor) => {
			if (editor) await decorateVisibleEditors(editor.document);
		}),
		window.onDidChangeVisibleTextEditors(async (editors) => {
			for (const editor of editors) {
				await decorationProvider.provideDecorations(editor);
			}
		}),
	);
}

export function deactivate() {
	fileWatcher?.dispose();
	if (!client) return;
	return client.stop();
}
