import * as path from "node:path";
import {
	workspace,
	ExtensionContext,
	commands,
	window,
	TextDocument,
	languages,
} from "vscode";
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	console.log("WX Language Server extension is activating...");

	const serverModule = context.asAbsolutePath(
		path.join("..", "target", "debug", "wx-lsp"),
	);
	console.log("serverModule:", serverModule);

	const disposable = commands.registerCommand("wx-vscode.helloWorld", () => {
		window.showInformationMessage("Hello World from WX Language Server!");
	});

	context.subscriptions.push(disposable);

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
	client.start().then(
		() => {
			console.log("WX Language Server started successfully!");
			window.showInformationMessage("WX Language Server is now active!");
		},
		(error) => {
			console.error("Failed to start WX Language Server:", error);
			window.showErrorMessage(`Failed to start WX Language Server: ${error}`);
		},
	);

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
