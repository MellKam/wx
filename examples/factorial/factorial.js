async function test(bytecode) {
	const module = await WebAssembly.compile(bytecode);
	const instance = await WebAssembly.instantiate(module);
	const factorial = instance.exports.factorial;

	return factorial(5);
}

return test;
