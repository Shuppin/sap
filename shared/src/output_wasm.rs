use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn appendTextToStandardOutput(text: &str);

    fn appendTextToResultOutput(text: &str);
}

#[macro_export]
macro_rules! stdout {
    ($($arg:tt)*) => {
        stdout_fn(&format!($($arg)*));
    }
}

#[macro_export]
macro_rules! stdoutln {
    ($($arg:tt)*) => {
        stdoutln_fn(&format!($($arg)*));
    }
}

pub fn stdout_fn(text: &str) {
    appendTextToStandardOutput(text)
}

pub fn stdoutln_fn(text: &str) {
    appendTextToStandardOutput(&format!("{}\n", text))
}
