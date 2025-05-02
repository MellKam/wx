const bytecode = await Deno.readFile("./out.wasm");

const wasmInstance = await WebAssembly.compile(bytecode);
const instance = await WebAssembly.instantiate(wasmInstance);
const { main } = instance.exports as {
	main: () => number;
};

console.log(main());
