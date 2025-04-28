const bytecode = await Deno.readFile("./out.wasm");

const wasmInstance = await WebAssembly.compile(bytecode);
const instance = await WebAssembly.instantiate(wasmInstance);
const { add } = instance.exports as {
	add: (a: number, b: number) => number;
};

console.log(add(2, 2));
