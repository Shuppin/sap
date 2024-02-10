use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn appendTextToOutput(name: &str);
}

macro_rules! output {
    ($($arg:tt)*) => {
        // #[cfg(target_arg = "wasm32")]
        {
            appendTextToOutput(&format!($($arg)*))
        }

        // #[cfg(not(target_arg = "wasm32"))]
        // {
        //     print!($($arg)*)
        // }
    }
}

#[wasm_bindgen]
pub fn do_some_stuff() {
    output!("Hi, this is {} speaking.\n", example_lib::NAME);
    output!("I'm currently talking to you from a WASM binary.\n");
}
