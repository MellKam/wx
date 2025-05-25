const bytes = await Deno.readFile("./out.wasm");
// console.log(bytes);
const module = await WebAssembly.compile(bytes);
const instance = await WebAssembly.instantiate(module);
const { fibonacci } = instance.exports as {
	fibonacci: (n: number) => number;
};

console.log(fibonacci(10)); // Should print 55
