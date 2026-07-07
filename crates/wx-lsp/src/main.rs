#[tokio::main]
async fn main() {
	let (service, socket) = wx_lsp::build_service();
	tower_lsp_server::Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
		.serve(service)
		.await;
}
