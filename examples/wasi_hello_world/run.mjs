// Runs main.wasm (compiled from main.wx) as a WASI Preview 1 "command"
// module using Node's built-in WASI runtime.
//
//   cargo run -p wx-cli -- compile examples/wasi_hello_world/main.wx
//   mv main.wasm examples/wasi_hello_world/main.wasm
//   node examples/wasi_hello_world/run.mjs

import { readFile } from "node:fs/promises";
import { WASI } from "node:wasi";

const wasi = new WASI({
	version: "preview1",
	args: [],
	env: {},
});

const wasmBuffer = await readFile(new URL("./main.wasm", import.meta.url));
const wasmModule = await WebAssembly.compile(wasmBuffer);
const instance = await WebAssembly.instantiate(
	wasmModule,
	wasi.getImportObject(),
);

wasi.start(instance);
