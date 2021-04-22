use crate::tokens::Token;
use crate::value::Value;

use std::fmt;

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

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    UndefinedVariable { received: Token },
    InvalidOperandTypes { op: Token, types: Vec<Value> },
    ZeroDivision { op: Token },
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

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UndefinedVariable { received: ident } => write!(
                    f,
                    "RuntimeError. Undefined Variable. Variable '{}' (at {}..{}) has not been declared.",
                    ident.kind,
                    ident.range.start,
                    ident.range.end
                ),

            Self::InvalidOperandTypes { op, types } => {
                let mut type_list = String::new();

                for t in types.iter().take(types.len() - 1) {
                    type_list.push_str(&format!("{} and ", t))
                }
                type_list.push_str(&format!("{}", types.iter().last().unwrap()));

                write!(
                    f,
                    "RuntimeError. Invalid Operand Types. '{}' (at {}..{}) does not support types of values '{}'",
                    op.kind,
                    op.range.start,
                    op.range.end,
                    type_list,
                )
            }

            Self::ZeroDivision { op } => write!(f, "RuntimeError. Zero Division. Attempted zero division at {}..{}",
                op.range.start,
                op.range.end,
            ),

        }
    }
}
