type WabtModule = Awaited<ReturnType<typeof import("wabt")>>;

let wabtPromise: Promise<WabtModule> | undefined;

function loadWabt() {
	wabtPromise ??= import("wabt").then((mod) => mod.default());
	return wabtPromise;
}

export async function toWat(bytecode: Uint8Array): Promise<string> {
	const wabt = await loadWabt();
	const module = wabt.readWasm(bytecode, { readDebugNames: true });
	try {
		module.applyNames();
		return module.toText({ foldExprs: false, inlineExport: false });
	} finally {
		module.destroy();
	}
}
