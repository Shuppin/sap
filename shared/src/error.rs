//! This module contains the error handling code for the interpreter.
//! It provides the `Error` struct, which is used to represent errors
//! that occur during execution.

/// A macro to create a new error with a kind and a message.
///
/// # Arguments
///
/// * `kind` - The kind of error.
/// * `message` - The error message.
///
/// # Returns
///
/// A new error with the given kind and message.
#[macro_export]
macro_rules! err {
    ($kind:expr, $($arg:tt)*) => {
        Err($crate::error::Error::new(&format!($($arg)*), $kind))
    }
}

/// The error struct represents is used for every error throughout the program.
#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub message: String,
    pub kind: ErrorKind,
}

impl Error {
    /// Creates a new error with a message and a kind.
    pub fn new(message: &str, kind: ErrorKind) -> Self {
        Self {
            message: message.to_string(),
            kind,
        }
    }
}

/// The ErrorKind enum is used to sort errors into different types. This allows
/// error handling code to exhibit different behavior based on the kind of error.
#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    TypeError,
    NameError,
    DivisionByZero,
    OverflowError,
    SyntaxError,
    NotImplemented,
}

/// Implements the `Display` trait for `ErrorKind`.
/// This allows `ErrorKind` to be formatted as a string when using the `write!` macro.
impl std::fmt::Display for ErrorKind {
    // Match the error kind and return the corresponding string.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ErrorKind::TypeError => "TypeError",
            ErrorKind::NameError => "NameError",
            ErrorKind::DivisionByZero => "DivisionByZero",
            ErrorKind::OverflowError => "OverflowError",
            ErrorKind::SyntaxError => "SyntaxError",
            ErrorKind::NotImplemented => "NotImplemented",
        };

        write!(f, "{}", s)
    }
}
