#[macro_export]
macro_rules! err {
    ($kind:expr, $($arg:tt)*) => {{
        Err($crate::error::Error::new(&format!($($arg)*), $kind))
    }}
}

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(message: &str, kind: ErrorKind) -> Self {
        Self {
            message: message.to_string(),
            kind,
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    TypeError,
    DivisionByZero,
    OverflowError,
    // TODO: Include parser errors
}
