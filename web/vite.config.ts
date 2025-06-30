import { defineConfig } from "vite";
import vue from "@vitejs/plugin-vue";
import vueJsx from "@vitejs/plugin-vue-jsx";
import wasm from "vite-plugin-wasm";
import tailwindcss from "@tailwindcss/vite";
import { fileURLToPath } from "node:url";

export default defineConfig({
	plugins: [vue(), vueJsx(), wasm(), tailwindcss()],
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
