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

pub fn stdout_fn(text: &str) {
    print!("{}", text)
}

pub fn stdoutln_fn(text: &str) {
    println!("{}", text)
}
