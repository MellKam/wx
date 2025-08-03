import { defineComponent, ref, watch } from "vue";
import VueMonacoEditor, { useMonaco } from "@guolao/vue-monaco-editor";
import { createMarkerData, type Diagnostic } from "./compilation";
import { compile } from "wx-compiler-wasm";
import {
	decodePlayground,
	encodePlayground,
	getExamplePlaygrounds,
} from "./playground";
import { useClipboard } from "@vueuse/core";
import { useRouteQuery } from "@vueuse/router";

const examplePlaygrounds = getExamplePlaygrounds();

const worker = new Worker(new URL("./worker.ts", import.meta.url), {
	type: "module",
});

export const App = defineComponent({
	setup: () => {
		const { monacoRef: monaco } = useMonaco();
		const diagnostics = ref<Diagnostic[]>();
		const workerResult = ref<unknown>();
		worker.onmessage = (event) => {
			workerResult.value = event.data.result;
		};

		const { copy, copied } = useClipboard();

		const run = async () => {
			const res = compile(
				currentPlayground.value.name + ".wx",
				currentPlayground.value.wx
			) as {
				diagnostics: Diagnostic[];
				bytecode?: number[];
			};

			diagnostics.value = res.diagnostics;
			workerResult.value = undefined;

			if (res.bytecode) {
				worker.postMessage({
					bytecode: new Uint8Array(res.bytecode),
					script: currentPlayground.value.js,
				});
			}
		};

		watch(diagnostics, (diagnostics) => {
			if (!monaco.value) return;
			const model = monaco.value.editor.getModels()[0];
			if (!model) return;

			monaco.value.editor.setModelMarkers(
				model,
				"wx",
				createMarkerData(currentPlayground.value.wx, diagnostics || [])
			);
		});

		const playgroundQuery = useRouteQuery<string | null>("playground");
		const currentPlayground = ref(
			playgroundQuery.value
				? decodePlayground(playgroundQuery.value)
				: examplePlaygrounds[0]!
		);

		watch(
			() => currentPlayground.value.name,
			() => run(),
			{ immediate: true }
		);

		watch(
			currentPlayground,
			() => {
				const res = compile(
					currentPlayground.value.name + ".wx",
					currentPlayground.value.wx
				) as {
					diagnostics: Diagnostic[];
					bytecode?: number[];
				};

				diagnostics.value = res.diagnostics;
				workerResult.value = undefined;
			},
			{ immediate: true, deep: true }
		);

		return () => (
			<div class="h-screen w-full bg-neutral-900">
				<div class="flex h-full flex-col">
					<div class="monaco-component flex bg-(--vscode-editor-background) justify-between border-b border-(--vscode-editorWidget-border)">
						<ul class="flex gap-1 p-2">
							{examplePlaygrounds.map((playground) => (
								<li>
									<button
										class="px-2.5 cursor-pointer h-8 flex items-center justify-center text-sm data-active:bg-(--vscode-menu-background) hover:bg-(--vscode-menu-background)/50 data-active:text-white rounded-md font-medium text-(--vscode-symbolIcon-textForeground)"
										data-active={
											currentPlayground.value.name === playground.name
												? ""
												: undefined
										}
										onClick={() => {
											currentPlayground.value = playground;
										}}
									>
										{playground.name}
									</button>
								</li>
							))}
						</ul>
						<div class="p-2 gap-1.5 flex">
							{workerResult.value !== undefined ? (
								<div class="flex items-center justify-between h-8 rounded-md bg-white/10 px-2.5 gap-1">
									<span class="text-white/50 text-sm">Result:</span>{" "}
									<span class="font-medium text-white font-mono">
										{workerResult.value}
									</span>
								</div>
							) : null}
							<button
								class="px-2.5 h-8 flex items-center justify-center text-white bg-(--vscode-button-background) hover:bg-(--vscode-button-hoverBackground) rounded-md font-medium text-sm cursor-pointer"
								onClick={run}
							>
								Run
							</button>
							<button
								class="px-2.5 h-8 flex items-center justify-center text-white bg-(--vscode-button-background) hover:bg-(--vscode-button-hoverBackground) rounded-md font-medium text-sm cursor-pointer"
								onClick={async () => {
									const encoded = encodePlayground(currentPlayground.value);
									playgroundQuery.value = encoded;
									copy(location.origin + `/?playground=${encoded}`);
								}}
							>
								{copied.value ? "Link Copied!" : "Share"}
							</button>
						</div>
					</div>

					<div class="flex h-full">
						<VueMonacoEditor
							value={currentPlayground.value.wx}
							onUpdate:value={(value) => (currentPlayground.value.wx = value)}
							style="width: 100%; height: 100%; flex: 1;"
							options={{
								theme: "vs-dark",
								language: "wx",
								minimap: { enabled: false },
							}}
						/>
						<VueMonacoEditor
							value={currentPlayground.value.js}
							onUpdate:value={(value) => (currentPlayground.value.js = value)}
							style="width: 100%; height: 100%; flex: 1;"
							defaultLanguage="javascript"
							options={{
								theme: "vs-dark",
								minimap: { enabled: false },
							}}
						/>
					</div>
				</div>
			</div>
		);
	},
});
