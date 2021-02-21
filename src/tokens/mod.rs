use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    kind: Kind,
    range: Range<usize>,
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
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error { message: String, source: String },
}
