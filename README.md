<h1 align="center">
  SAP (Simple As Possible)
</h1>

SAP is a simple programming language designed to be easy to learn and use. It is a dynamically typed language with a simple syntax and a small set of features.

It is designed to be a good language for beginners to learn programming, sitting somewhere between a language like Python and Scratch.

SAP is implemented in Rust and uses a tree-walking interpreter to execute programs.

## Features

SAP has a small set of simple, yet powerful features, including:

- Dynamic typing and type coercion
- Simple syntax (intuitive for beginners)
- Variables
- Repetition (looping)
- Functions
- Closures
- Control flow (conditionals)
- Displaying to console
- Error messages
- Command line tool

## Quick start

There is an [**online playground**](https://shuppin.github.io/sap/) available if you want try out the language without installing anything. You can also install the `sap` command line tool to run SAP programs locally. You can install it using [Cargo]:

```sh
cargo install sap-cli
```

You can then run a SAP program using the `sap` command:

```sh
sap main.sap
```

### Building from source

If you want to build the interpreter from source, you can clone the repository and build the workspace crate using `build.bat` (Windows) or [Cargo]:

```sh
git clone https://github.com/Shuppin/sap.git
cd sap
cargo build --release
```

This will automatically build the `sap` command line tool.

## Examples

To see some examples of SAP programs, see the [examples](examples) directory.

Here is a simple example of a SAP program that prints the first 10 numbers in the Fibonacci sequence:

```sap
defineFunction fib(n)
  if n < 2 then
    return n
  otherwise
    return fib(n - 1) + fib(n - 2)
  end
end

set i = 0

repeat 10 times
  display fib(i)
  set i = i + 1
end
```

## Implmentation

The SAP interpreter is implemented in Rust. The code is split into a number of crates, including:

- `lexer`: Tokenises the input source code.
- `ast`: Defines the abstract syntax tree (AST) for the language.
- `parser`: Parses the token stream into an abstract syntax tree (AST).
- `interpreter`: Walks the AST and executes the program.
- `cli`: Command line tool for running SAP programs.
- `shared`: Shared code used by multiple modules.
- `playground`: (Not part of the interpreter) Web-based playground for running SAP programs.

Code is parsed using a recursive descent parsing algorithm, and executed using a tree-walking algorithm.

Currently, error messages are not very helpful, but the framework is in place to improve this in the future.

### Dependencies

SAP is designed to have minimal dependencies. The only external dependency is the `serde` crate, which is used for serialising and deserialising the AST. The `cli` crate also uses `os_info`, `ctrlc`, and `clap` for command line argument parsing and handling.

### Testing

SAP has a suite of unit tests. The unit tests are run using `cargo test`. These tests cover all language features.

### Documentation

Since this is a learning project, the code is heavily commented, a mixture of documentation and explanation of the code. I've tried to make the comments as helpful as possible, and the flow of the code as clear as possible, to make it easier for others to understand.

I will admit, the interpreter could be improved, the control flow is a bit of a mess, especially if you aren't familiar with the `ControlFlow` type.

If you are here to learn, these are the resources I used to learn about interpreters and compilers:

- [**Let's Build a Simple Interpreter**](https://ruslanspivak.com/lsbasi-part1/) by Ruslan Spivak
  - This series is aimed at beginners and is a great introduction to interpreters and compilers. (This is actually the series I followed when making the [original version](https://github.com/Shuppin/sap_legacy) of SAP.)
- [**Writing An Interpreter In Go**](https://interpreterbook.com/) by Thorsten Ball
  - This book delves into the details of writing an interpreter a bit quicker than the previous series, but is still very accessible.

Here's what I plan to read next:

- [**Writing a Compiler in Go**](https://compilerbook.com/) by Thorsten Ball
  - This book is a follow-up to the previous book, and covers writing a compiler.
- **A Practical Approach to Compiler Construction** by Des Watson
  - This book is aimed at a more advanced audience, and covers the theory and practice of writing a compiler in much more detail.

## Playground

The `playground` crate wont compile in it's current state, as it is primarily designed for automatic deployment to GitHub Pages. If you wish to run the playground locally, follow these steps:

1. Update line 1 of [/playground/web/script.js](playground/web/script.js) to `import init, {interpret, parse, lex} from "../pkg/playground.js";`
2. Move the contents of [/playground/web](playground/web) into [/playground](playground).
3. Install wasm-pack using `cargo install wasm-pack`
4. Build the `playground` crate using `wasm-pack build ./playground --target web`
5. Serve the `playground` crate using whatever method you prefer (e.g. `python -m http.server`).