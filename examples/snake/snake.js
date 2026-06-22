async function test(bytecode) {
	const module = await WebAssembly.compile(bytecode);
	const frames = [];

	let currentCommands = null;

	const importObject = {
		host: {
			draw_rect: (x, y, w, h, color) => {
				if (currentCommands) currentCommands.push({ type: "rect", x, y, w, h, color });
			},
			clear: (color) => {
				if (currentCommands) currentCommands.push({ type: "clear", color });
			},
			get_time_ms: () => {
				return BigInt(Date.now());
			},
			poll_key: () => 0,
		},
		env: {},
	};

	const instance = await WebAssembly.instantiate(module, importObject);

	if (instance.exports.start) instance.exports.start();

	const FRAMES = 200;
	const DT = 100; // ms per simulated step call

	for (let i = 0; i < FRAMES; i++) {
		currentCommands = [];
		// update expects i64 dt — JS numbers are fine, they'll be coerced
		if (instance.exports.update) instance.exports.update(BigInt(DT));
		if (instance.exports.draw) instance.exports.draw();
		frames.push(currentCommands);
		currentCommands = null;
	}

	return frames;
}

return test;
