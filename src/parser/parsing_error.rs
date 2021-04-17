use crate::tokens::Token;

use std::error::Error;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedToken { expected: String, received: Token },
    UnexpectedEOF { expected: String },
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedToken {
                expected: e,
                received: r,
            } => {
                write!(
                    f,
                    "ParsingError: Unexpected Token. Expected {:?} but received '{:?}' at {}..{}.",
                    e, r.kind, r.range.start, r.range.end
                )
            }

            Self::UnexpectedEOF { expected: e } => {
                write!(f, "ParsingError: Unexpected EOF. Expected '{}'.", e)
            }
        }
    }
}

impl Error for ParsingError {}
