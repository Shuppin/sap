/// Identical to the `print!` macro.
#[macro_export]
macro_rules! stdout {
    ($($arg:tt)*) => {
        print!($($arg)*)
    }
}

/// Identical to the `println!` macro.
#[macro_export]
macro_rules! stdoutln {
    ($($arg:tt)*) => {
        println!($($arg)*)
    }
}
