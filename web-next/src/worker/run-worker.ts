// Runs a user-written `host.js` driver script against compiled bytecode, in
// its own Worker — modeled on the old playground's `web/src/worker.ts`. A
// dedicated worker (not the LSP one) so a stuck/infinite-looping script
// can't block LSP responsiveness or the main thread; `App.vue` spawns a
// fresh one per run and terminates any still-running previous one first.

export interface RunRequest {
	bytecode: Uint8Array;
	script: string;
}

export type RunResponse =
	| { type: "log"; level: "log" | "warn" | "error"; text: string }
	| { type: "result"; result: unknown }
	| { type: "error"; error: string };

function post(message: RunResponse) {
	self.postMessage(message);
}

/** `host.js` scripts commonly `return instance.exports` (see `DEFAULT_HOST_JS`
 * in `vfs.ts`) — the exported WASM functions in there aren't structured-clone
 * -able, so posting the raw result back to the main thread throws a
 * `DataCloneError` and loses the whole run. `structuredClone` is used purely
 * as a clonability probe; anything that fails it gets replaced with a plain,
 * always-cloneable summary instead. */
function toCloneable(value: unknown): unknown {
	try {
		structuredClone(value);
		return value;
	} catch {
		return summarize(value);
	}
}

function summarize(value: unknown): unknown {
	if (typeof value === "function") return `ƒ ${value.name || "anonymous"}()`;
	if (value === null || typeof value !== "object") return value;
	if (Array.isArray(value)) return value.map(summarize);
	return Object.fromEntries(Object.entries(value).map(([key, entry]) => [key, summarize(entry)]));
}

function stringifyArg(arg: unknown): string {
	if (typeof arg === "string") return arg;
	if (arg instanceof Error) return arg.stack ?? arg.message;
	try {
		return JSON.stringify(arg);
	} catch {
		return String(arg);
	}
}

for (const level of ["log", "warn", "error"] as const) {
	console[level] = (...args: unknown[]) => {
		post({ type: "log", level, text: args.map(stringifyArg).join(" ") });
	};
}

self.onmessage = async (event: MessageEvent<RunRequest>) => {
	const { bytecode, script } = event.data;
	try {
		// `script` is expected to define its own function and `return` it (see
		// `DEFAULT_HOST_JS` in `vfs.ts`) — the outer `bytecode` parameter name
		// is unused by the driver itself, kept only so old `examples/*/*.js`
		// scripts (same convention) still work unmodified.
		const driver = new Function("bytecode", script) as () => (bytecode: Uint8Array) => unknown;
		const result = await driver()(bytecode);
		post({ type: "result", result: toCloneable(result) });
	} catch (error) {
		post({ type: "error", error: error instanceof Error ? (error.stack ?? error.message) : String(error) });
	}
};
