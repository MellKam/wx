import { z } from "zod/v4";

const PayloadSchema = z.object({
	bytecode: z.instanceof(Uint8Array),
	script: z.string(),
});

self.onmessage = async (event) => {
	console.log(event.data);
	const data = PayloadSchema.safeParse(event.data);
	if (!data.success) {
		console.error(z.treeifyError(data.error));
		return;
	}
	const { bytecode, script } = data.data;

	const userFunc = new Function("bytecode", script);
	const result = await userFunc()(bytecode);
	self.postMessage({ result });
};
