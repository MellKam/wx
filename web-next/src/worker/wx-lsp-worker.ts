import init, { WxLanguageServer } from "wx-lsp-wasm";

await init();

const server = new WxLanguageServer((message: unknown) => {
	self.postMessage(message);
});

self.onmessage = async (event: MessageEvent) => {
	const response = await server.handle_message(event.data);
	if (response !== null) {
		self.postMessage(response);
	}
};
