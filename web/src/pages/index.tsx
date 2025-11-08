import { z } from "zod/v4";
import { gzipSync, decompressSync } from "fflate";
import {
	createContext,
	createEffect,
	createMemo,
	createResource,
	createSignal,
	For,
	on,
	onCleanup,
	Show,
	splitProps,
	useContext,
	type Accessor,
	type Resource,
	type Setter,
} from "solid-js";
import { useSearchParams } from "@solidjs/router";
import monacoLoader, { type Monaco } from "@monaco-editor/loader";
import type * as monaco from "monaco-editor";
import { effect } from "solid-js/web";
import type { JSX } from "solid-js/jsx-runtime";
import { compile as generateWebAssemblyBytecode } from "wx-compiler-wasm";

export interface Diagnostic {
	code: undefined;
	message: string;
	notes: any[];
	labels: {
		file_id: number;
		message: string;
		range: { start: number; end: number };
		style: "Primary";
	}[];
	severity: "Error";
}

const getLineOffsets = (code: string): number[] => {
	if (code === "") return [0];

	const offsets = [0];
	for (let i = 0; i < code.length; i++) {
		if (code[i] === "\n") {
			offsets.push(i + 1);
		}
	}
	return offsets;
};

const offsetToPosition = (
	offsets: number[],
	offset: number
): [line: number, column: number] => {
	let left = 0;
	let right = offsets.length - 1;

	while (left < right) {
		const mid = Math.floor((left + right + 1) / 2);
		if (offsets[mid]! <= offset) {
			left = mid;
		} else {
			right = mid - 1;
		}
	}

	const lineNumber = left + 1;
	const lineStart = offsets[left]!;
	const column = offset - lineStart + 1;

	return [lineNumber, column];
};

const createMarkerData = (code: string, diagnostics: Diagnostic[]) => {
	if (diagnostics.length === 0) return [];
	const offsets = getLineOffsets(code);

	return diagnostics
		.filter((diagnostics) => diagnostics.labels.length > 0)
		.map((diagnostic): monaco.editor.IMarkerData => {
			const label = diagnostic.labels[0]!;
			const [startLineNumber, startColumn] = offsetToPosition(
				offsets,
				label.range.start
			);
			const [endLineNumber, endColumn] = offsetToPosition(
				offsets,
				label.range.end
			);
			return {
				message: label.message
					? `${diagnostic.message}: ${label.message}`
					: diagnostic.message,
				tags:
					diagnostic.message === "unreachable code"
						? [1 satisfies monaco.MarkerTag.Unnecessary]
						: undefined,
				severity: 8 satisfies monaco.MarkerSeverity.Error,
				startLineNumber,
				endLineNumber,
				startColumn,
				endColumn,
			};
		});
};

const PlaygroundSchema = z.object({
	name: z.string(),
	wx: z.string(),
	js: z.string(),
});

type Playground = z.infer<typeof PlaygroundSchema>;
const EXAMPLE_FILES = import.meta.glob<true, string, string>(
	"../../../examples/**/*",
	{
		import: "default",
		query: "?raw",
		eager: true,
	}
);

const getPlaygroundExamples = (): Playground[] => {
	return Object.entries(
		Object.groupBy(
			Object.entries(EXAMPLE_FILES),
			([path]) => path.split("/").at(-2)!
		)
	)
		.map(([name, files]) => {
			const wxFile = files!.find(([path]) => path.endsWith(".wx"));
			const jsFile = files!.find(([path]) => path.endsWith(".js"));

			if (!wxFile || !jsFile) return null;

			return {
				name,
				wx: wxFile[1],
				js: jsFile[1],
			};
		})
		.filter((p) => p !== null);
};

const PlaygroundContext = createContext<{
	getPlaygrounds: Accessor<Playground[]>;
	setPlaygrounds: Setter<Playground[]>;
	getCurrentPlaygroundIndex: Accessor<number>;
	setCurrentPlaygroundIndex: Setter<number>;
	getResult: Accessor<unknown>;
	execute: () => Promise<void>;
}>();

export const encodePlayground = (playground: Playground) => {
	const json = JSON.stringify(playground);
	const bytes = new TextEncoder().encode(json);
	const compressed = gzipSync(bytes);
	const base64 = btoa(String.fromCharCode(...compressed))
		.replace(/\+/g, "-")
		.replace(/\//g, "_")
		.replace(/=+$/, "");
	return base64;
};

export const decodePlayground = (encoded: string): Playground => {
	const b64 = encoded.replace(/-/g, "+").replace(/_/g, "/");
	const compressed = Uint8Array.from(atob(b64), (c) => c.charCodeAt(0));
	const decompressed = decompressSync(compressed);
	const json = new TextDecoder().decode(decompressed);
	return PlaygroundSchema.parse(JSON.parse(json));
};

const worker = new Worker(new URL("../worker.ts", import.meta.url), {
	type: "module",
});

const Header = () => {
	const [, setSearchParams] = useSearchParams();
	const {
		getPlaygrounds,
		setCurrentPlaygroundIndex,
		getCurrentPlaygroundIndex,
		getResult,
		execute,
	} = useContext(PlaygroundContext)!;

	return (
		<div class="monaco-component flex bg-(--vscode-editor-background) justify-between border-b border-(--vscode-editorWidget-border)">
			<ul class="flex gap-1 p-2">
				<For each={getPlaygrounds()}>
					{(playground, getIndex) => (
						<li>
							<button
								class="px-2.5 cursor-pointer h-8 flex items-center justify-center text-sm data-active:bg-(--vscode-menu-background) hover:bg-(--vscode-menu-background)/50 data-active:text-white rounded-md font-medium text-(--vscode-symbolIcon-textForeground)"
								data-active={
									getCurrentPlaygroundIndex() === getIndex() ? "" : undefined
								}
								onClick={() => {
									setCurrentPlaygroundIndex(getIndex());
								}}
							>
								{playground.name}
							</button>
						</li>
					)}
				</For>
			</ul>
			<div class="p-2 gap-1.5 flex">
				<Show when={getResult()}>
					{(getResult) => (
						<div class="flex items-center justify-between h-8 rounded-md bg-white/10 px-2.5 gap-1">
							<span class="text-white/50 text-sm">Result:</span>{" "}
							<span class="font-medium text-white font-mono">
								{getResult() as any}
							</span>
						</div>
					)}
				</Show>
				<button
					class="px-2.5 h-8 flex items-center justify-center text-white bg-(--vscode-button-background) hover:bg-(--vscode-button-hoverBackground) rounded-md font-medium text-sm cursor-pointer"
					onClick={execute}
				>
					Run
				</button>
				<button
					class="px-2.5 h-8 flex items-center justify-center text-white bg-(--vscode-button-background) hover:bg-(--vscode-button-hoverBackground) rounded-md font-medium text-sm cursor-pointer"
					onClick={async () => {
						const playground = getPlaygrounds()[getCurrentPlaygroundIndex()];
						if (!playground) return;
						const encoded = encodePlayground(playground);
						setSearchParams({ playground: encoded });
						navigator.clipboard.writeText(
							location.origin + `/?playground=${encoded}`
						);
					}}
				>
					Share
				</button>
			</div>
		</div>
	);
};

const MonacoContext = createContext<Resource<Monaco>>();

const EDITOR_OPTIONS: monaco.editor.IStandaloneEditorConstructionOptions = {
	theme: "vs-dark",
	minimap: { enabled: false },
};

const createModelURI = (playground: string, language: "js" | "wx") =>
	`inmemory://model/${playground}.${language}`;

const Editor = (
	args: { language: "wx" | "js" } & JSX.HTMLAttributes<HTMLDivElement>
) => {
	const [props, attrs] = splitProps(args, ["language"]);

	const getMonaco = useContext(MonacoContext)!;
	const { getPlaygrounds, setPlaygrounds, getCurrentPlaygroundIndex } =
		useContext(PlaygroundContext)!;

	const model = createMemo(() => {
		const monaco = getMonaco();
		if (!monaco) return;
		const playground = getPlaygrounds()[getCurrentPlaygroundIndex()];
		if (!playground) return;
		let existingModel = monaco.editor
			.getModels()
			.find(
				(m) =>
					m.uri.toString() === createModelURI(playground.name, props.language)
			);
		if (existingModel) {
			return existingModel;
		}
		return monaco.editor.createModel(
			playground[props.language],
			props.language === "js" ? "javascript" : "wx",
			monaco.Uri.parse(createModelURI(playground.name, props.language))
		);
	});

	let container: HTMLDivElement | undefined;
	createEffect(
		on(getMonaco, (monaco) => {
			if (!monaco || !container) return;
			const editor = monaco.editor.create(container, EDITOR_OPTIONS);

			const currentModel = model();
			if (currentModel) {
				editor.setModel(currentModel);
			}

			effect(
				on(model, (model) => {
					if (!model) return;
					const currentModel = editor.getModel();
					if (currentModel === model) return;
					editor.setModel(model);
				})
			);

			editor.onDidChangeModelContent(() => {
				const currentPlayground = getPlaygrounds()[getCurrentPlaygroundIndex()];
				const newContent = editor.getValue();
				if (
					currentPlayground &&
					currentPlayground[props.language] !== newContent
				) {
					setPlaygrounds((playgrounds) =>
						playgrounds.map((p, i) =>
							i === getCurrentPlaygroundIndex()
								? { ...p, [props.language]: newContent }
								: p
						)
					);
				}
			});

			onCleanup(() => {
				editor.dispose();
			});
		})
	);

	return <div ref={container} class="h-screen w-full" {...attrs} />;
};

export default () => {
	const [getPlaygrounds, setPlaygrounds] = createSignal(
		getPlaygroundExamples()
	);
	const [getCurrentPlaygroundIndex, setCurrentPlaygroundIndex] =
		createSignal(0);

	const [getMonaco] = createResource(() => monacoLoader.init());

	effect(() => {
		const monaco = getMonaco();
		if (!monaco) return;

		monaco.languages.register({ id: "wx" });
		monaco.languages.setLanguageConfiguration("wx", {
			comments: {
				lineComment: "//",
			},
			brackets: [
				["{", "}"],
				["[", "]"],
				["(", ")"],
			],
			autoClosingPairs: [
				{ open: "{", close: "}" },
				{ open: "[", close: "]" },
				{ open: "(", close: ")" },
			],
			surroundingPairs: [
				{ open: "{", close: "}" },
				{ open: "[", close: "]" },
				{ open: "(", close: ")" },
			],
			folding: {
				markers: {
					start: /^\\s*\\{\\s*$/,
					end: /^\\s*\\}\\s*$/,
				},
			},
		});

		monaco.languages.setMonarchTokensProvider("wx", {
			tokenizer: {
				root: [
					[
						/\b(func|local|mut|if|else|loop|break|return|true|false|enum|global|continue|export)\b/,
						"keyword",
					],
					[/\b(i32|i64|f32|f64|u32|u64|bool)\b/, "type"],
					[/\/\/.*$/, "comment"],
					[/\d+\.\d+/, "number.float"],
					[/\d+/, "number"],
					[/[a-zA-Z_]\w*/, "identifier"],
				],
			},
		});
	});

	const setModelMarkers = (diagnostics: Diagnostic[]) => {
		const playground = getPlaygrounds()[getCurrentPlaygroundIndex()];
		if (!playground) return;
		const monaco = getMonaco();
		if (!monaco) return;
		const model = monaco.editor
			.getModels()
			.find(
				(model) =>
					model.uri.toString() === createModelURI(playground.name, "wx")
			);
		if (!model) return;
		monaco.editor.setModelMarkers(
			model,
			"wx",
			createMarkerData(playground.wx, diagnostics)
		);
	};

	const execute = async () => {
		const playground = getPlaygrounds()[getCurrentPlaygroundIndex()]!;

		try {
			const bytecode = generateWebAssemblyBytecode(
				playground.name + ".wx",
				playground.wx
			);
			if (!bytecode) {
				throw new Error("compiler internal error");
			}
			setModelMarkers([]);
			worker.postMessage({ bytecode, script: playground.js });
		} catch (error) {
			if (Array.isArray(error)) {
				setModelMarkers(error as Diagnostic[]);
			}
			throw error;
		}
	};

	const [getResult, setResult] = createSignal<unknown>();
	worker.onmessage = (event) => {
		setResult(event.data);
	};

	return (
		<PlaygroundContext.Provider
			value={{
				getPlaygrounds,
				setPlaygrounds,
				getCurrentPlaygroundIndex,
				setCurrentPlaygroundIndex,
				getResult,
				execute,
			}}
		>
			<Header />
			<MonacoContext.Provider value={getMonaco}>
				<div class="grid grid-cols-2">
					<Editor language="wx" />
					<Editor language="js" />
				</div>
			</MonacoContext.Provider>
		</PlaygroundContext.Provider>
	);
};
