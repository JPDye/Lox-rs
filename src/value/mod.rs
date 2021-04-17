use crate::tokens::{Kind, Token};

use std::fmt;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl From<Token> for Value {
    fn from(t: Token) -> Self {
        match t.kind {
            Kind::Nil => Self::Nil,
            Kind::False => Self::Boolean(false),
            Kind::True => Self::Boolean(true),
            Kind::Number(n) => Self::Number(n),
            Kind::String(s) => Self::String(s),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "Nil"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_numeric_equality_and_ordering() {
        let x = Value::Number(1.0);
        let y = Value::Number(2.0);

        assert_eq!(x, x);
        assert_ne!(x, y);
        assert!(x < y);
        assert!(x <= y);
    }

    #[test]
    fn test_string_equality_and_ordering() {
        let x = Value::String("Hello ".to_string());
        let y = Value::String("World!".to_string());

        assert_eq!(x, x);
        assert_ne!(x, y);
        assert!(x < y);
        assert!(x <= y);
    }
}
