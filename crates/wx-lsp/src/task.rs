//! Shim over spawning a detached task, so call sites don't need their own
//! `#[cfg(target_arch = ...)]` branch. `tokio::sync` types (`Mutex`, etc.)
//! need no such shim — they're pure Rust and work unchanged on every target.

#[cfg(not(target_arch = "wasm32"))]
pub fn spawn(future: impl std::future::Future<Output = ()> + Send + 'static) {
	tokio::spawn(future);
}

#[cfg(target_arch = "wasm32")]
pub fn spawn(future: impl std::future::Future<Output = ()> + 'static) {
	wasm_bindgen_futures::spawn_local(future);
}
