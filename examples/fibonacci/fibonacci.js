async function test(bytecode) {
	const module = await WebAssembly.compile(bytecode);
	const instance = await WebAssembly.instantiate(module);
	const { fibonacci, fibonacci_iterative } = instance.exports;

	return fibonacci_iterative(24);
}

return test;
