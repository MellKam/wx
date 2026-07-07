import init, { WxLanguageServer } from "wx-lsp-wasm";

await init();

const server = new WxLanguageServer((message: string) => {
	self.postMessage(message);
});

self.onmessage = async (event: MessageEvent<string>) => {
	const response = await server.handle_message(event.data);
	if (response !== null) {
		self.postMessage(response);
	}
};
