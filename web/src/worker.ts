import { z } from "zod/v4";

const PayloadSchema = z.object({
	bytecode: z.instanceof(Uint8Array),
	script: z.string(),
});

self.onmessage = async (event) => {
	const { bytecode, script } = PayloadSchema.parse(event.data);

	const userFunc = new Function("bytecode", script);
	const result = await userFunc()(bytecode);
	self.postMessage({ result });
};
