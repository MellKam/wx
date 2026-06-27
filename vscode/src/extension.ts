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
} from "vscode";
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | null = null;

const resolveServerBinary = (context: ExtensionContext): string => {
	if (context.extensionMode === ExtensionMode.Development) {
		return path.resolve(
			context.extensionPath,
			"..",
			"target",
			"debug",
			process.platform === "win32" ? "wx-lsp-next.exe" : "wx-lsp-next",
		);
	}

	return context.asAbsolutePath(
		path.join("bin", process.platform === "win32" ? "wx-lsp.exe" : "wx-lsp"),
	);
};

let fileWatcher: FileSystemWatcher | null = null;

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
	console.log("WX Language Server extension is activating...");

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
}

export function deactivate() {
	fileWatcher?.dispose();
	if (!client) return;
	return client.stop();
}
