#[macro_export]
macro_rules! err {
    ($kind:expr, $($arg:tt)*) => {
        Err($crate::error::Error::new(&format!($($arg)*), $kind))
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    TypeError,
    NameError,
    DivisionByZero,
    OverflowError,
    SyntaxError,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ErrorKind::TypeError => "TypeError",
            ErrorKind::NameError => "NameError",
            ErrorKind::DivisionByZero => "DivisionByZero",
            ErrorKind::OverflowError => "OverflowError",
            ErrorKind::SyntaxError => "SyntaxError",
        };

        write!(f, "{}", s)
    }
}
