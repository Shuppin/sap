#[macro_export]
macro_rules! stdout {
    ($($arg:tt)*) => {
        print!($($arg)*)
    }
}

#[macro_export]
macro_rules! stdoutln {
    ($($arg:tt)*) => {
        println!($($arg)*)
    }
}
