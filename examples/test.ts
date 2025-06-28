const bytes = await Deno.readFile("../wx-compiler-cli/fibonacci.wasm");

console.log(JSON.stringify(Array.from(bytes)));

const module = await WebAssembly.compile(bytes);
const instance = await WebAssembly.instantiate(module);
const { test } = instance.exports as {
	test: () => number;
};

console.log(test());
