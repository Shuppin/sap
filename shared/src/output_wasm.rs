//! This module contains the macros and functions to output text to the standard output
//! and the result output in the browser. It is assumed that the
//! `appendTextToStandardOutput` and `appendTextToResultOutput` functions are defined in
//! the JavaScript environment. In this case, these functions are defined in
//! `sap/playground/web/wasm_bindings.js`.
use wasm_bindgen::prelude::*;

// This code is used to grab the external JavaScript functions and make the available for
// use in Rust code. The `extern "C"` is a sort of placeholder here, since the
// `#[wasm_bindgen]` attribute overrides it.
#[wasm_bindgen]
extern "C" {
    fn appendTextToStandardOutput(text: &str);

    fn appendTextToResultOutput(text: &str);
}

/// Output the given text to the (web) standard output.
#[macro_export]
macro_rules! stdout {
    ($($arg:tt)*) => {
        pub use $crate::output::stdout_fn;
        stdout_fn(&format!($($arg)*));
    }
}

/// Output the given text to the (web) standard output, followed by a newline.
#[macro_export]
macro_rules! stdoutln {
    ($($arg:tt)*) => {
        pub use $crate::output::stdoutln_fn;
        stdoutln_fn(&format!($($arg)*));
    }
}

/// Do not call this function directly. Use the `stdout!` macro instead.
pub fn stdout_fn(text: &str) {
    appendTextToStandardOutput(text)
}

/// Do not call this function directly. Use the `stdoutln!` macro instead.
pub fn stdoutln_fn(text: &str) {
    appendTextToStandardOutput(&format!("{}\n", text))
}
