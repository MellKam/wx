import * as path from "node:path";
import { workspace, ExtensionContext, commands, window } from "vscode";
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

async function startServer(serverModule: string) {
	const serverOptions: ServerOptions = {
		command: serverModule,
		transport: TransportKind.stdio,
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "wx" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/*.wx"),
		},
		outputChannelName: "WX Language Server",
	};

	client = new LanguageClient(
		"wx-lsp",
		"WX Language Server",
		serverOptions,
		clientOptions,
	);

	console.log("Starting WX Language Server...");
	try {
		await client.start();
		console.log("WX Language Server started successfully!");
		window.showInformationMessage("WX Language Server is now active!");
	} catch (error) {
		console.error("Failed to start WX Language Server:", error);
		window.showErrorMessage(`Failed to start WX Language Server: ${error}`);
	}
}

export function activate(context: ExtensionContext) {
	console.log("WX Language Server extension is activating...");

	const binaryName = process.platform === "win32" ? "wx-lsp.exe" : "wx-lsp";
	const serverModule = context.asAbsolutePath(path.join("bin", binaryName));
	console.log("serverModule:", serverModule);

	// Register restart server command
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

	context.subscriptions.push(restartCommand);

	// Start the language server
	startServer(serverModule);

	// Set up format on save for WX files
	context.subscriptions.push(
		workspace.onWillSaveTextDocument((event) => {
			const document = event.document;
			if (document.languageId !== "wx") {
				return;
			}

			const config = workspace.getConfiguration("wx");
			const formatOnSave = config.get<boolean>("formatOnSave", true);

			if (formatOnSave) {
				console.log("Formatting document on save:", document.uri.toString());
				event.waitUntil(
					commands.executeCommand("editor.action.formatDocument"),
				);
			}
		}),
	);
}

export function deactivate() {
	if (!client) return;
	return client.stop();
}
