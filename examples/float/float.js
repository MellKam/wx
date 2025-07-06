async function test(bytecode) {
	const module = await WebAssembly.compile(bytecode);
	const instance = await WebAssembly.instantiate(module);
	const { add } = instance.exports;

	return add(2.5, 3.5);
}

return test;
