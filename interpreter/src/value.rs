use std::fmt;

use ast::statement::Statement;
use shared::err;
use shared::error::{Error, ErrorKind};

use crate::runtime::EnvRef;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub parameters: Vec<String>,
    pub body: Vec<Statement>,
    pub env: EnvRef,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Function(Function),
    Null,
}

macro_rules! generate_comparison_op {
    ($method_name:ident, $op:tt, $ordering:pat) => {
        pub fn $method_name(&self, other: &Self) -> Result<Self, Error> {
            match self.compare(other) {
                Some($ordering) => Ok(Self::Boolean(true)),
                Some(_) => Ok(Self::Boolean(false)),
                None => err!(
                    ErrorKind::TypeError,
                    "invalid operation '{}' between {} and {}",
                    stringify!($op),
                    self.variant_name(),
                    other.variant_name()
                ),
            }
        }
    };
}

macro_rules! generate_binary_arithmetic_op {
    ($method:ident, $integer_op:tt, $float_op:tt) => {
        pub fn $method(&self, other: &Self) -> Result<Self, Error> {

            let overflow_error = Error::new(
                "this arithmetic operation overflows",
                ErrorKind::OverflowError
            );

            match (&self, &other) {
                (Self::Integer(a), Self::Integer(b)) => {
                    Ok(Self::Integer(a.$integer_op(*b).ok_or(overflow_error)?))
                },
                (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a $float_op b)),
                _ => err!(
                    ErrorKind::TypeError,
                    "invalid operation '{}' between {} and {}",
                    stringify!($float_op),
                    self.variant_name(),
                    other.variant_name()
                ),
            }
        }
    };
}

impl Value {
    pub fn variant_name(&self) -> &'static str {
        match &self {
            Self::Integer(_) => "Integer",
            Self::Float(_) => "Float",
            Self::Boolean(_) => "Boolean",
            Self::Function(_) => "Function",
            Self::Null => "null",
        }
    }

    // region: type coercion

    pub fn to_boolean(&self) -> Result<Self, Error> {
        match self {
            Self::Integer(i) => Ok(Self::Boolean(*i != 0)),
            Self::Float(f) => Ok(Self::Boolean(*f != 0.0)),
            Self::Boolean(b) => Ok(Self::Boolean(*b)),
            _ => err!(
                ErrorKind::TypeError,
                "cannot convert {} to {}",
                self.variant_name(),
                Self::Boolean(true).variant_name() // i hate this i hate this i hate this
            ),
        }
    }

    // endregion: type coercion

    // region: comparitive operations

    fn compare(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a.partial_cmp(b),
            (Self::Float(a), Self::Float(b)) => a.partial_cmp(b),
            (Self::Boolean(a), Self::Boolean(b)) => a.partial_cmp(b),
            _ => None,
        }
    }

    pub fn eq(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a == *b)),
            (Self::Null, Self::Null) => Ok(Self::Boolean(true)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '==' between {} and {}",
                self.variant_name(),
                other.variant_name()
            ),
        }
    }

    pub fn ne(&self, other: &Self) -> Result<Self, Error> {
        match self.eq(other) {
            Ok(value) => value.not(),
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

    generate_comparison_op!(lt, >, std::cmp::Ordering::Less);
    generate_comparison_op!(le, >=, std::cmp::Ordering::Less | std::cmp::Ordering::Equal);
    generate_comparison_op!(gt, <, std::cmp::Ordering::Greater);
    generate_comparison_op!(ge, <=, std::cmp::Ordering::Greater | std::cmp::Ordering::Equal);

    // endregion: comparitive operations

    // region: boolean operations

    pub fn not(&self) -> Result<Self, Error> {
        match self {
            Self::Boolean(b) => Ok(Self::Boolean(!b)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '!' for type {}",
                self.variant_name(),
            ),
        }
    }

    pub fn and(&self, other: &Self) -> Result<Self, Error> {
        match (&self, &other) {
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a && *b)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '&&' between {} and {}",
                self.variant_name(),
                other.variant_name(),
            ),
        }
    }

    pub fn or(&self, other: &Self) -> Result<Self, Error> {
        match (&self, &other) {
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a || *b)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '&&' between {} and {}",
                self.variant_name(),
                other.variant_name(),
            ),
        }
    }

    // endregion: boolean operations

    // region: arithmetic operations

    generate_binary_arithmetic_op!(add, checked_add, +);
    generate_binary_arithmetic_op!(sub, checked_sub, -);
    generate_binary_arithmetic_op!(mul, checked_mul, *);
    generate_binary_arithmetic_op!(rem, checked_rem, %);

    pub fn div(&self, other: &Self) -> Result<Self, Error> {
        // Check for division by zero
        #[allow(illegal_floating_point_literal_pattern)]
        if matches!(&other, Self::Integer(0) | Self::Float(0.0)) {
            return err!(ErrorKind::DivisionByZero, "cannot divide by zero");
        }

        let overflow_error = Error::new(
            "this arithmetic operation overflows",
            ErrorKind::OverflowError,
        );

        match (&self, &other) {
            (Self::Integer(a), Self::Integer(b)) => {
                Ok(Self::Integer(a.checked_div(*b).ok_or(overflow_error)?))
            }
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a / b)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '/' between {} and {}",
                self.variant_name(),
                other.variant_name()
            ),
        }
    }

    pub fn neg(&self) -> Result<Self, Error> {
        match self {
            Self::Integer(n) => Ok(Self::Integer(-n)),
            Self::Float(n) => Ok(Self::Float(-n)),
            _ => err!(
                ErrorKind::TypeError,
                "invalid operation '-' for type {}",
                self.variant_name(),
            ),
        }
    }

    // endregion: arithmetic operations
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{}", i),
            Self::Float(fl) => write!(f, "{}", fl),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Function(_) => write!(f, "<function>"),
            Self::Null => write!(f, "null"),
        }
    }
}