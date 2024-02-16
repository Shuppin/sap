use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn appendTextToStandardOutput(text: &str);

    fn appendTextToResultOutput(text: &str);
}

#[macro_export]
macro_rules! stdout {
    ($($arg:tt)*) => {
        appendTextToStandardOutput(&format!($($arg)*))
    }
}

#[macro_export]
macro_rules! stdoutln {
    ($($arg:tt)*) => {
        appendTextToStandardOutput(&format!($($arg)*))
    }
}

