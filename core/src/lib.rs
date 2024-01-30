use std::fmt;

use error::{Error, ErrorKind};

pub mod error;

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
}

impl Value {
    fn variant_name(&self) -> &'static str {
        match &self {
            Self::Integer(_) => "Integer",
            Self::Float(_) => "Float",
            Self::Boolean(_) => "Boolean",
            Self::Null => "null",
        }
    }

    pub fn to_boolean(&self) -> Result<Self, Error> {
        match self {
            Self::Integer(i) => Ok(Self::Boolean(*i != 0)),
            Self::Float(f) => Ok(Self::Boolean(*f != 0.0)),
            Self::Boolean(b) => Ok(Self::Boolean(*b)),
            Self::Null => error!(
                ErrorKind::TypeError,
                "cannot convert {} to {}",
                self.variant_name(),
                Self::Null.variant_name()
            ),
        }
    }

    pub fn eq(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Null, Self::Null) => Ok(Self::Boolean(true)),
            _ => error!(
                ErrorKind::TypeError,
                "invalid operation '==' between {} and {}",
                self.variant_name(),
                other.variant_name()
            ),
        }
    }

    pub fn ne(&self, other: &Self) -> Result<Self, Error> {
        match self.eq(other) {
            Ok(value) => !value,
            Err(mut old_err) => {
                old_err.message = format!(
                    "invalid operation '!=' between {} and {}",
                    self.variant_name(),
                    other.variant_name()
                );
                Err(old_err)
            }
        }
    }

    pub fn and(self, other: &Self) -> Result<Self, Error> {
        match (&self, &other) {
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a && *b)),
            _ => error!(
                ErrorKind::TypeError,
                "invalid operation '&&' between {} and {}",
                self.variant_name(),
                other.variant_name(),
            ),
        }
    }

    pub fn or(self, other: &Self) -> Result<Self, Error> {
        match (&self, &other) {
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a || *b)),
            _ => error!(
                ErrorKind::TypeError,
                "invalid operation '&&' between {} and {}",
                self.variant_name(),
                other.variant_name(),
            ),
        }
    }

    fn compare(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a.partial_cmp(b),
            (Self::Float(a), Self::Float(b)) => a.partial_cmp(b),
            (Self::Boolean(a), Self::Boolean(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

macro_rules! impl_comparison_op {
    ($method_name:ident, $op:tt, $ordering:pat) => {
        impl Value {
            pub fn $method_name(&self, other: &Self) -> Result<Self, Error> {
                match self.compare(other) {
                    Some($ordering) => Ok(Self::Boolean(true)),
                    Some(_) => Ok(Self::Boolean(false)),
                    None => error!(
                        ErrorKind::TypeError,
                        "invalid operation '{}' between {} and {}",
                        stringify!($op),
                        self.variant_name(),
                        other.variant_name()
                    ),
                }
            }
        }
    };
}
impl_comparison_op!(lt, >, std::cmp::Ordering::Less);
impl_comparison_op!(le, >=, std::cmp::Ordering::Less | std::cmp::Ordering::Equal);
impl_comparison_op!(gt, <, std::cmp::Ordering::Greater);
impl_comparison_op!(ge, <=, std::cmp::Ordering::Greater | std::cmp::Ordering::Equal);

macro_rules! impl_binary_arithmetic_op {
    ($trait:ident, $method:ident, $integer_op:tt, $float_op:tt) => {
        impl std::ops::$trait for Value {
            type Output = Result<Self, Error>;

            fn $method(self, other: Self) -> Self::Output {

                let overflow_error = error::Error::new(
                    "this arithmetic operation overflows",
                    ErrorKind::OverflowError
                );

                match (&self, &other) {
                    (Self::Integer(a), Self::Integer(b)) => {
                        Ok(Self::Integer(a.$integer_op(*b).ok_or(overflow_error)?))
                    },
                    (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a $float_op b)),
                    _ => error!(
                        ErrorKind::TypeError,
                        "invalid operation '{}' between {} and {}",
                        stringify!($float_op),
                        self.variant_name(),
                        other.variant_name()
                    ),
                }
            }
        }
    };
}
impl_binary_arithmetic_op!(Add, add, checked_add, +);
impl_binary_arithmetic_op!(Sub, sub, checked_sub, -);
impl_binary_arithmetic_op!(Mul, mul, checked_mul, *);
impl_binary_arithmetic_op!(Rem, rem, checked_rem, %);

impl std::ops::Div for Value {
    type Output = Result<Self, Error>;

    fn div(self, other: Self) -> Self::Output {
        // Check for division by zero
        #[allow(illegal_floating_point_literal_pattern)]
        if matches!(&other, Self::Integer(0) | Self::Float(0.0)) {
            return error!(ErrorKind::DivisionByZero, "cannot divide by zero");
        }

        let overflow_error = error::Error::new(
            "this arithmetic operation overflows",
            ErrorKind::OverflowError,
        );

        match (&self, &other) {
            (Self::Integer(a), Self::Integer(b)) => {
                Ok(Self::Integer(a.checked_div(*b).ok_or(overflow_error)?))
            }
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a / b)),
            _ => error!(
                ErrorKind::TypeError,
                "invalid operation '/' between {} and {}",
                self.variant_name(),
                other.variant_name()
            ),
        }
    }
}

impl std::ops::Neg for Value {
    type Output = Result<Self, Error>;

    fn neg(self) -> Self::Output {
        match self {
            Self::Integer(n) => Ok(Self::Integer(-n)),
            Self::Float(n) => Ok(Self::Float(-n)),
            _ => error!(
                ErrorKind::TypeError,
                "invalid operation '-' for type {}",
                self.variant_name(),
            ),
        }
    }
}

impl std::ops::Not for Value {
    type Output = Result<Self, Error>;

    fn not(self) -> Self::Output {
        match self {
            Self::Boolean(b) => Ok(Self::Boolean(!b)),
            _ => error!(
                ErrorKind::TypeError,
                "invalid operation '!' for type {}",
                self.variant_name(),
            ),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{}", i),
            Self::Float(fl) => write!(f, "{}", fl),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Null => write!(f, "null"),
        }
    }
}
