import * as monaco from "./monaco-lite";
import type { LspClient } from "./lspClient";
import { WX_LANGUAGE_ID } from "./language";

export interface VirtualFileCallbacks {
	onEnterVirtualFile: () => void;
	onReturnToMainFile: () => void;
}

/**
 * wx-lsp can point hover/definition/references at stdlib source through a
 * `wx://std/<name>` URI (see `file_id_to_uri` in `wx-lsp/src/lib.rs`) — it's
 * not a real file, so there's nothing on disk for an editor to just open.
 * The server exposes its content through a custom `wx/virtualFileContent`
 * request instead (the same mechanism a real editor extension would use via
 * a `TextDocumentContentProvider`).
 *
 * Plain `monaco-editor` has no such provider concept and, with a single
 * editor instance, no built-in notion of "switch to a different file" at
 * all: `StandaloneCodeEditorService`'s default open handler only resolves a
 * target resource if it's already the *current* model (see
 * `standaloneCodeEditorService.js`'s `findModel`) — so jumping to a `wx://`
 * location would otherwise silently do nothing. `monaco.editor
 * .registerEditorOpener` is the extension point for this: it's tried before
 * the default handler (`registerCodeEditorOpenHandler` prepends), so
 * returning `false` for anything that isn't a `wx://` resource cleanly
 * falls through to the normal same-file behavior.
 *
 * Ctrl+hovering a symbol (the "peek definition" preview, as opposed to
 * actually clicking it) goes through a *different* path —
 * `ITextModelService.createModelReference` — that `registerEditorOpener`
 * has no bearing on at all. Plain Monaco's implementation of it
 * (`StandaloneTextModelService` in `standaloneServices.js`) is just
 * `modelService.getModel(resource)`, rejecting with "Model not found" if
 * nothing is registered yet — there's no async content-provider hook the
 * way a real VS Code extension host has. So the model has to already exist
 * by the time that fires, which is why the stdlib model is fetched and
 * registered eagerly below instead of waiting for the first navigation.
 *
 * This module owns no DOM itself — `callbacks` let the caller (an `App.vue`
 * component, in this project) drive its own reactive banner state instead.
 * The returned `returnToMainFile` is meant to be bound to that banner's
 * "back" button.
 *
 * There's no single fixed "main file" any more (the file tree lets the user
 * have any number of open `.wx`/`.js` files) — `getActiveUserModel` is a
 * getter into whatever the caller currently considers "the last file the
 * user was actually editing", so "back" always returns to the right place
 * regardless of which file was open when the stdlib jump happened.
 */
export function registerVirtualFileOpener(
	editor: monaco.editor.IStandaloneCodeEditor,
	getActiveUserModel: () => monaco.editor.ITextModel | undefined,
	client: LspClient,
	callbacks: VirtualFileCallbacks,
): { returnToMainFile: () => void } {
	const virtualModels = new Map<string, monaco.editor.ITextModel>();
	let savedViewState: monaco.editor.ICodeEditorViewState | null = null;

	async function getOrCreateVirtualModel(uri: string): Promise<monaco.editor.ITextModel> {
		const resource = monaco.Uri.parse(uri);
		const existing = virtualModels.get(uri) ?? monaco.editor.getModel(resource);
		if (existing) return existing;
		const text = await client.request<string>("wx/virtualFileContent", { uri });
		const model = monaco.editor.createModel(text, WX_LANGUAGE_ID, resource);
		virtualModels.set(uri, model);
		return model;
	}

	// wx-lsp currently only ever resolves `wx://std/lib.wx` (see
	// `virtual_file_content` in `wx-lsp/src/lib.rs`) — if it grows more
	// virtual files, they'd need prefetching here too.
	void getOrCreateVirtualModel("wx://std/lib.wx").catch((error: unknown) => {
		console.error("[wx-lsp] failed to prefetch stdlib source:", error);
	});

	function returnToMainFile() {
		const model = getActiveUserModel();
		if (!model) return;
		editor.setModel(model);
		editor.updateOptions({ readOnly: false });
		if (savedViewState) editor.restoreViewState(savedViewState);
		callbacks.onReturnToMainFile();
	}

	monaco.editor.registerEditorOpener({
		async openCodeEditor(source, resource, selectionOrPosition) {
			if (resource.scheme !== "wx") return false;

			if (source.getModel()?.uri.scheme !== "wx") {
				savedViewState = source.saveViewState();
			}

			const model = await getOrCreateVirtualModel(resource.toString());
			source.setModel(model);
			source.updateOptions({ readOnly: true });
			callbacks.onEnterVirtualFile();

			if (selectionOrPosition) {
				if ("startLineNumber" in selectionOrPosition) {
					source.setSelection(selectionOrPosition);
					source.revealRangeInCenter(selectionOrPosition);
				} else {
					source.setPosition(selectionOrPosition);
					source.revealPositionInCenter(selectionOrPosition);
				}
			}

			return true;
		},
	});

	return { returnToMainFile };
}
