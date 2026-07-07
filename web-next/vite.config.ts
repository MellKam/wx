import { defineConfig } from "vite";
import vue from "@vitejs/plugin-vue";
import tailwindcss from "@tailwindcss/vite";
import wasm from "vite-plugin-wasm";

export default defineConfig({
	plugins: [vue(), tailwindcss(), wasm()],
	worker: {
		format: "es",
		plugins: () => [wasm()],
	},
	build: {
		target: "esnext",
	},
	optimizeDeps: {
		// Both packages are local `file:` deps whose wasm-bindgen glue loads
		// its `.wasm` sibling via `new URL('..._bg.wasm', import.meta.url)` at
		// runtime. esbuild's dep pre-bundling would copy the JS into
		// `.vite/deps/` without that sibling file, breaking the URL — under
		// pnpm this happens even though the real files live outside any
		// `node_modules` (`crates/*/pkg`), because they're reached through a
		// `node_modules/.pnpm/...` symlink/hardlink hop that Vite's
		// linked-package heuristic doesn't see through the way it does npm's
		// direct-to-`crates/*` symlink.
		exclude: ["wx-compiler-wasm", "wx-lsp-wasm"],
	},
});
