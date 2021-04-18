use crate::errors::ParsingError;

use std::fmt;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub range: Range<usize>,
}

impl Token {
    pub fn new(kind: Kind, range: Range<usize>) -> Self {
        Self { kind, range }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Kind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquare,
    RightSquare,

    SemiColon,
    Comma,
    Dot,

    Minus,
    Plus,
    Star,
    Slash,

    // One or two character tokens.
    Bang,
    BangEq,
    Eq,
    DoubleEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,

    // Literals.
    True,
    False,
    Number(f64),
    String(String),
    Identifier(String),

    // Keywords.
    And,
    Class,
    Else,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    Var,
    While,

    Error(Box<ParsingError>),
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::LeftSquare => write!(f, "["),
            Self::RightSquare => write!(f, "]"),

            Self::SemiColon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),

            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),

            Self::Bang => write!(f, "!"),
            Self::BangEq => write!(f, "!="),
            Self::Eq => write!(f, "="),
            Self::DoubleEq => write!(f, "=="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),

            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Number(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Identifier(s) => write!(f, "{}", s),

            Self::And => write!(f, "and"),
            Self::Class => write!(f, "class"),
            Self::Else => write!(f, "else"),
            Self::Fun => write!(f, "fun"),
            Self::For => write!(f, "and"),
            Self::If => write!(f, "if"),
            Self::Nil => write!(f, "nil"),
            Self::Or => write!(f, "or"),
            Self::Print => write!(f, "print"),
            Self::Return => write!(f, "return"),
            Self::Super => write!(f, "super"),
            Self::This => write!(f, "this"),
            Self::Var => write!(f, "var"),
            Self::While => write!(f, "while"),

            Self::Error(e) => write!(f, "{}", e),
        }
    }
}
