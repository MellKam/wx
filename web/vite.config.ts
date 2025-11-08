import { defineConfig } from "vite";
import solid from "vite-plugin-solid";
import wasm from "vite-plugin-wasm";
import tailwindcss from "@tailwindcss/vite";
import { fileURLToPath } from "node:url";

export default defineConfig({
	plugins: [solid(), wasm(), tailwindcss()],
	build: {
		target: "esnext",
	},
	server: {
		fs: {
			allow: [
				fileURLToPath(new URL("./src", import.meta.url)),
				fileURLToPath(new URL("../examples/", import.meta.url)),
			],
		},
	},
});
