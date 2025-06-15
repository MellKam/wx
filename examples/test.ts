const bytes = await Deno.readFile("./out.wasm");

const module = await WebAssembly.compile(bytes);
const instance = await WebAssembly.instantiate(module);
const { exponent, fibonacci_iterative, fibonacci_recursive } =
	instance.exports as {
		exponent: (base: number, exp: number) => number;
		fibonacci_recursive: (n: number) => number;
		fibonacci_iterative: (n: number) => number;
	};

console.log(exponent(3, 9));
console.log(fibonacci_recursive(10));
console.log(fibonacci_iterative(10));
