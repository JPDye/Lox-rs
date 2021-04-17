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

    Error { message: String, source: String },
}
