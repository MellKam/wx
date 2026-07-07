import * as monaco from "./monaco-lite";
import {
	fromCompletionContext,
	fromPosition,
	toCompletionList,
	toDefinition,
	toHover,
	toLocation,
	toMarkerData,
	toSemanticTokens,
	toSignatureHelp,
	toTextEdit,
	toWorkspaceEdit,
} from "monaco-languageserver-types";
import type {
	CompletionItem,
	CompletionList,
	Definition,
	Diagnostic,
	Hover,
	Location,
	PublishDiagnosticsParams,
	SemanticTokens,
	SignatureHelp,
	TextEdit,
	WorkspaceEdit,
} from "vscode-languageserver-protocol";
import type { LspClient } from "./lspClient";
import { WX_LANGUAGE_ID } from "./language";

/** Mirrors `SEMANTIC_TOKEN_TYPES` in `wx-lsp/src/lib.rs` — order matters,
 * since semantic token data is legend-index-encoded, not name-encoded. */
const SEMANTIC_TOKEN_TYPES = [
	"function",
	"variable",
	"enum",
	"struct",
	"namespace",
	"parameter",
	"enumMember",
	"interface",
	"typeParameter",
	"type",
];

/** Registers every `monaco.languages.register*Provider` wx-lsp's server
 * capabilities advertise, translating each request/response through
 * `monaco-languageserver-types` instead of relying on
 * `monaco-languageclient`'s VS Code extension-host emulation to do it. Also
 * wires `textDocument/publishDiagnostics` to `editor.setModelMarkers`, the
 * one direction that isn't a request/response pair the provider APIs cover.
 *
 * Registered **once** per language id, not per file: Monaco calls each
 * provider with whichever model is currently relevant, so `textDocument` is
 * built from that model on every call rather than captured from a single
 * model up front — with multiple `.wx` files open, a provider that always
 * queried the *first* file's uri would silently answer for the wrong
 * document the moment a second one exists. */
export function registerWxLanguageFeatures(client: LspClient): monaco.IDisposable {
	const disposables: monaco.IDisposable[] = [];

	disposables.push(
		monaco.languages.registerHoverProvider(WX_LANGUAGE_ID, {
			async provideHover(model, position, token) {
				const hover = await client.request<Hover | null>(
					"textDocument/hover",
					{ textDocument: { uri: model.uri.toString() }, position: fromPosition(position) },
					token,
				);
				return hover ? toHover(hover) : null;
			},
		}),
	);

	disposables.push(
		monaco.languages.registerDefinitionProvider(WX_LANGUAGE_ID, {
			async provideDefinition(model, position, token) {
				const definition = await client.request<Definition | null>(
					"textDocument/definition",
					{ textDocument: { uri: model.uri.toString() }, position: fromPosition(position) },
					token,
				);
				return definition ? toDefinition(definition) : null;
			},
		}),
	);

	disposables.push(
		monaco.languages.registerReferenceProvider(WX_LANGUAGE_ID, {
			async provideReferences(model, position, context, token) {
				const locations = await client.request<Location[] | null>(
					"textDocument/references",
					{
						textDocument: { uri: model.uri.toString() },
						position: fromPosition(position),
						context: { includeDeclaration: context.includeDeclaration },
					},
					token,
				);
				return locations?.map(toLocation) ?? null;
			},
		}),
	);

	disposables.push(
		monaco.languages.registerRenameProvider(WX_LANGUAGE_ID, {
			async provideRenameEdits(model, position, newName, token) {
				const edit = await client.request<WorkspaceEdit | null>(
					"textDocument/rename",
					{ textDocument: { uri: model.uri.toString() }, position: fromPosition(position), newName },
					token,
				);
				return edit ? toWorkspaceEdit(edit) : { edits: [] };
			},
		}),
	);

	disposables.push(
		monaco.languages.registerDocumentFormattingEditProvider(WX_LANGUAGE_ID, {
			async provideDocumentFormattingEdits(model, options, token) {
				const edits = await client.request<TextEdit[] | null>(
					"textDocument/formatting",
					{
						textDocument: { uri: model.uri.toString() },
						options: { tabSize: options.tabSize, insertSpaces: options.insertSpaces },
					},
					token,
				);
				return edits?.map(toTextEdit) ?? [];
			},
		}),
	);

	disposables.push(
		monaco.languages.registerSignatureHelpProvider(WX_LANGUAGE_ID, {
			signatureHelpTriggerCharacters: ["(", ","],
			async provideSignatureHelp(model, position, token) {
				const help = await client.request<SignatureHelp | null>(
					"textDocument/signatureHelp",
					{ textDocument: { uri: model.uri.toString() }, position: fromPosition(position) },
					token,
				);
				return help ? { value: toSignatureHelp(help), dispose() {} } : null;
			},
		}),
	);

	disposables.push(
		monaco.languages.registerCompletionItemProvider(WX_LANGUAGE_ID, {
			triggerCharacters: [".", ":"],
			async provideCompletionItems(model, position, context, token) {
				const word = model.getWordUntilPosition(position);
				const range = new monaco.Range(
					position.lineNumber,
					word.startColumn,
					position.lineNumber,
					word.endColumn,
				);
				const result = await client.request<CompletionItem[] | CompletionList | null>(
					"textDocument/completion",
					{
						textDocument: { uri: model.uri.toString() },
						position: fromPosition(position),
						context: fromCompletionContext(context),
					},
					token,
				);
				if (!result) return { suggestions: [] };
				const list: CompletionList = Array.isArray(result)
					? { isIncomplete: false, items: result }
					: result;
				return toCompletionList(list, { range });
			},
		}),
	);

	disposables.push(
		monaco.languages.registerDocumentSemanticTokensProvider(WX_LANGUAGE_ID, {
			getLegend: () => ({ tokenTypes: SEMANTIC_TOKEN_TYPES, tokenModifiers: [] }),
			async provideDocumentSemanticTokens(model, _lastResultId, token) {
				const result = await client.request<SemanticTokens | null>(
					"textDocument/semanticTokens/full",
					{ textDocument: { uri: model.uri.toString() } },
					token,
				);
				return result ? toSemanticTokens(result) : null;
			},
			releaseDocumentSemanticTokens() {},
		}),
	);

	client.onNotification<PublishDiagnosticsParams>("textDocument/publishDiagnostics", (params) => {
		const model = monaco.editor.getModel(monaco.Uri.parse(params.uri));
		if (!model) return;
		monaco.editor.setModelMarkers(
			model,
			"wx-lsp",
			params.diagnostics.map((d: Diagnostic) => toMarkerData(d)),
		);
	});

	return { dispose: () => disposables.forEach((d) => d.dispose()) };
}

/** Tells wx-lsp about one `.wx` document: `didOpen` immediately, `didChange`
 * on every edit. Every `.wx` file needs this as soon as it exists (not just
 * once the user clicks into it) — `wx-lsp`'s file source falls back to a
 * real filesystem read (`crates/wx-lsp/src/lib.rs`'s `read_to_string`) for
 * anything not in `open_documents`, which is a no-op in the browser, so an
 * un-opened sibling `module` would otherwise never resolve. */
export function openWxDocument(client: LspClient, model: monaco.editor.ITextModel): monaco.IDisposable {
	const uri = model.uri.toString();

	client.notify("textDocument/didOpen", {
		textDocument: {
			uri,
			languageId: WX_LANGUAGE_ID,
			version: model.getVersionId(),
			text: model.getValue(),
		},
	});

	const subscription = model.onDidChangeContent(() => {
		client.notify("textDocument/didChange", {
			textDocument: { uri, version: model.getVersionId() },
			contentChanges: [{ text: model.getValue() }],
		});
	});

	return { dispose: () => subscription.dispose() };
}
