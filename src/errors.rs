use crate::tokens::Token;

use std::fmt;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingError {
    // Caught by Parser
    UnexpectedToken { expected: String, received: Token },
    UnexpectedEOF { expected: String },
    InvalidIdentifier { received: Token },

    // Caught by Scanner
    UnterminatedString { source: String, start: usize },
    InvalidCharacter { source: char, pos: usize },
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
                    "ParsingError: Unexpected Token. Expected {:?} but received '{}' at {}..{}.",
                    e, r.kind, r.range.start, r.range.end
                )
            }

            Self::UnexpectedEOF { expected: e } => {
                write!(f, "ParsingError: Unexpected EOF. Expected '{}'.", e)
            }

            Self::InvalidIdentifier { received: r } => {
                write!(
                    f,
                    "ParsingError: Invalid Identifier. Expected identifier but received '{}' at {}..{}.",
                    r.kind, r.range.start, r.range.end
                )
            }

            Self::UnterminatedString { source: s, start } => {
                write!(
                    f,
                    "ParsingError: Unterminated String. '{}' at {}..{}.",
                    s,
                    start,
                    start + s.len()
                )
            }

            Self::InvalidCharacter { source: c, pos } => {
                write!(
                    f,
                    "ParsingError: InvalidCharacter. Found '{}' at {}..{}.",
                    c,
                    pos,
                    pos + 1,
                )
            }
        }
    }
}
