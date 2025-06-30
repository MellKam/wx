async function test(bytecode) {
	const module = await WebAssembly.compile(bytecode);
	const instance = await WebAssembly.instantiate(module);
	const main = instance.exports.main;

	return main();
}

return test;
