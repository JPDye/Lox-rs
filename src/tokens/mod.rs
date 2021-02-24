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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = match self {
            Kind::LeftParen => "(".to_string(),
            Kind::RightParen => ")".to_string(),
            Kind::LeftBrace => "{".to_string(),
            Kind::RightBrace => "}".to_string(),
            Kind::LeftSquare => "[".to_string(),
            Kind::RightSquare => "]".to_string(),

            Kind::SemiColon => ";".to_string(),
            Kind::Comma => ",".to_string(),
            Kind::Dot => ".".to_string(),

            Kind::Minus => "-".to_string(),
            Kind::Plus => "+".to_string(),
            Kind::Star => "*".to_string(),
            Kind::Slash => "/".to_string(),

            Kind::Bang => "!".to_string(),
            Kind::BangEq => "!=".to_string(),
            Kind::Eq => "=".to_string(),
            Kind::DoubleEq => "==".to_string(),
            Kind::Greater => ">".to_string(),
            Kind::GreaterEq => ">=".to_string(),
            Kind::Less => "<".to_string(),
            Kind::LessEq => "<=".to_string(),

            Kind::Identifier(ident) => ident.to_string(),
            Kind::String(string) => string.to_string(),
            Kind::Number(num) => num.to_string(),

            Kind::And => "and".to_string(),
            Kind::Class => "class".to_string(),
            Kind::Else => "else".to_string(),
            Kind::False => "false".to_string(),
            Kind::Fun => "fun".to_string(),
            Kind::For => "for".to_string(),
            Kind::If => "if".to_string(),
            Kind::Nil => "nil".to_string(),
            Kind::Or => "or".to_string(),
            Kind::Print => "print".to_string(),
            Kind::Return => "return".to_string(),
            Kind::Super => "super".to_string(),
            Kind::This => "this".to_string(),
            Kind::True => "true".to_string(),
            Kind::Var => "var".to_string(),
            Kind::While => "while".to_string(),

            Kind::Error { .. } => "ERROR".to_string(),
        };

        write!(f, "{}", out)
    }
}
