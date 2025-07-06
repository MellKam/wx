async function test(bytecode) {
	const module = await WebAssembly.compile(bytecode);
	const instance = await WebAssembly.instantiate(module);
	const { get, set } = instance.exports;

	set(Math.PI);
	return get();
}

return test;
