//! # Shared module
//!
//! The shared module contains code that is shared between the different parts of the SAP
//! interpreter. This includes the error data structures, the span module, and the output
//! module.

pub mod error;
pub mod span;

// The output module contains functions for printing the output of the interpreter.
// It is platform-specific, so it has different implementations for the different
// platforms that the interpreter supports.

// "wasm" is a target family that is used to identify WebAssembly (web browser) targets.
// So, if the code is being compiled for WebAssembly, use the `output_wasm.rs` file.
#[cfg(target_family = "wasm")]
#[path = "./output_wasm.rs"]
pub mod output;

// If the code is not being compiled for WebAssembly, use the `output.rs` file.
#[cfg(not(target_family = "wasm"))]
pub mod output;
