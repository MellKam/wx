import { z } from "zod/v4";
import { gzipSync, decompressSync } from "fflate";

export interface Playground {
	name: string;
	wx: string;
	js: string;
}

const PlaygroundSchema = z.object({
	name: z.string(),
	wx: z.string(),
	js: z.string(),
});

export const getExamplePlaygrounds = (): Playground[] => {
	const EXAMPLES = import.meta.glob<true, string, string>(
		"../../examples/**/*",
		{
			import: "default",
			query: "?raw",
			eager: true,
		}
	);

	const grouped: Record<string, Record<string, string>> = {};

	for (const [path, content] of Object.entries(EXAMPLES)) {
		const folderName = path.split("/").at(-2) || "";
		const fileName = path.split("/").at(-1) || "";

		if (!grouped[folderName]) {
			grouped[folderName] = {};
		}

		grouped[folderName][fileName] = content;
	}

	const examples: Playground[] = [];

	for (const [folderName, files] of Object.entries(grouped)) {
		const fileNames = Object.keys(files);
		const wxFile = fileNames.find((name) => name.endsWith(".wx"));
		const jsFile = fileNames.find((name) => name.endsWith(".js"));

		if (wxFile && jsFile) {
			examples.push({
				name: folderName,
				wx: files[wxFile]!,
				js: files[jsFile]!,
			});
		}
	}

	return examples;
};

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
