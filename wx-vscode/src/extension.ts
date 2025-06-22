import * as path from "node:path";
import { workspace, ExtensionContext, commands, window } from "vscode";
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const serverModule = context.asAbsolutePath(
		path.join("..", "target", "release", "wx-lsp")
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
		documentSelector: [{ scheme: "file", language: "plaintext" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
		},
	};

	client = new LanguageClient(
		"wx-lsp",
		"WX Language Server",
		serverOptions,
		clientOptions
	);

	client.start();
	console.log("WX Language Server is now active!");
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
