pub mod error;
pub mod span;

#[cfg(target_family = "wasm")]
#[path = "./output_wasm.rs"]
pub mod output;

#[cfg(not(target_family = "wasm"))]
pub mod output;
