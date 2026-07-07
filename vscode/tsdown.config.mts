import { defineConfig } from "tsdown";

export default defineConfig({
	entry: ["src/extension.ts"],
	outDir: "out",
	format: "cjs",
	platform: "node",
	// Needed so F5 debugging (`.vscode/launch.json`'s `outFiles`) can map
	// breakpoints/stack traces back to TS source. `.vscodeignore` already
	// strips `**/*.map` from the packaged vsix, so this doesn't bloat the
	// published extension.
	sourcemap: true,
	deps: {
		neverBundle: ["vscode"],
		// `vscode-languageclient` and `ansi-sequence-parser` are listed in
		// package.json `dependencies`, which tsdown auto-externalizes by
		// default (it's built for library authors whose consumers install
		// those themselves). We publish node_modules-free via `vsce package
		// --no-dependencies`, so both need to be force-bundled instead.
		alwaysBundle: [
			"vscode-languageclient/node",
			"ansi-sequence-parser",
		],
	},
});
