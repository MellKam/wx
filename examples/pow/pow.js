async function test(bytecode) {
	const module = await WebAssembly.compile(bytecode);
	const instance = await WebAssembly.instantiate(module);
	const pow = instance.exports.pow;

	return pow(3, 10);
}

return test;
