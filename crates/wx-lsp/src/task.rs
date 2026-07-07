//! Shims over the bits of `tokio` that need a real OS reactor (task
//! spawning, timers) so call sites don't need their own
//! `#[cfg(feature = ...)]` branches. `tokio::sync` types (`Mutex`, etc.)
//! need no such shim — they're pure Rust and work unchanged under both the
//! `native` and `wasm` features.
//!
//! Gated on our own `native`/`wasm` features rather than `target_arch`, so
//! this always agrees with which `tower-lsp-server` runtime (and which
//! optional deps) Cargo actually pulled in for the build — see the
//! `[features]` comment in `Cargo.toml`.

#[cfg(feature = "native")]
pub fn spawn(future: impl std::future::Future<Output = ()> + Send + 'static) {
	tokio::spawn(future);
}

#[cfg(feature = "native")]
pub async fn sleep(ms: u64) {
	tokio::time::sleep(std::time::Duration::from_millis(ms)).await;
}

#[cfg(feature = "wasm")]
pub fn spawn(future: impl std::future::Future<Output = ()> + 'static) {
	wasm_bindgen_futures::spawn_local(future);
}

#[cfg(feature = "wasm")]
pub async fn sleep(ms: u64) {
	gloo_timers::future::sleep(std::time::Duration::from_millis(ms)).await;
}
