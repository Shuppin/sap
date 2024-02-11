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
    output!("Hi, this is Ferris speaking.\n");
    output!("I'm currently talking to you from a WASM binary.\n");
}

#[wasm_bindgen]
pub fn interpret(source: String) {
    let env = interpreter::create_env();

    // First, parse the input, then evaluate the parsed AST.
    match parser::parse(&source) {
        Ok(parsed_ast) => match interpreter::eval_program(&env, parsed_ast) {
            Ok(evaluation) => match *evaluation.1 {
                interpreter::value::Value::Null => {}
                _ => output!("{}\n", evaluation.1),
            },
            Err(err) => output!("{:?}: {}\n", err.kind, err.message),
        },
        Err(err) => output!("{:?}: {}\n", err.kind, err.message),
    }
}
