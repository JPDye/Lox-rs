use crate::tokens::{Kind, Token};

use std::iter::Peekable;
use std::str::Chars;

pub struct Scanner<'a> {
    index: usize,
    string: String,
    chars: Peekable<Chars<'a>>,
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // Remove whitespace and comments before any potential tokens.
        self.discard();

        // Ensure there are tokens to be scanned.
        if self.chars.peek().is_none() {
            return None;
        }

        let kind = match self.consume() {
            // Single character tokens.
            '(' => Kind::LeftParen,
            ')' => Kind::RightParen,
            '{' => Kind::LeftBrace,
            '}' => Kind::RightBrace,
            '[' => Kind::LeftSquare,
            ']' => Kind::RightSquare,

            ';' => Kind::SemiColon,
            ',' => Kind::Comma,
            '.' => Kind::Dot,

            '+' => Kind::Plus,
            '-' => Kind::Minus,
            '*' => Kind::Star,
            '/' => Kind::Slash,

            // Single or double character tokens.
            '!' => self.match_next('=', Kind::BangEq, Kind::Bang),
            '=' => self.match_next('=', Kind::DoubleEq, Kind::Eq),
            '<' => self.match_next('=', Kind::LessEq, Kind::Less),
            '>' => self.match_next('=', Kind::GreaterEq, Kind::Greater),

            // Literals>
            '"' => self.consume_string(),
            '0'..='9' => self.consume_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.consume_identifier(),

            _ => Kind::Error {
                message: "Uh Oh!".to_string(),
                source: self.string.clone(),
            },
        };

        // Calculate range and create token.
        let start = self.index - self.string.len();
        let token = Token::new(kind, start..self.index);

        // Clear current string for next token.
        self.string.clear();

        Some(token)
    }
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            index: 0,
            string: String::new(),
            chars: source.chars().peekable(),
        }
    }

    /// Consumes the next character in the source. Increments index and returns character.
    fn consume(&mut self) -> char {
        self.index += 1;

        let ch = self.chars.next().unwrap();
        self.string.push(ch);
        ch
    }

    /// Consume a string literal. Return a Kind::String or Kind::Error.
    fn consume_string(&mut self) -> Kind {
        while self.chars.peek().is_some() && self.chars.peek() != Some(&'"') {
            self.consume();
        }

        if self.chars.peek().is_none() {
            Kind::Error {
                message: "Unterminated string.".to_string(),
                source: self.string.clone(),
            }
        } else {
            self.consume(); // Capture closing quote.
            Kind::String(self.string[1..self.string.len() - 1].to_string()) // Remove surrounding quotes.
        }
    }

    /// Consume a number literal. Return a Kind::Number or Kind::Error.
    fn consume_number(&mut self) -> Kind {
        // Consume integer portion of number.
        while is_digit(self.chars.peek().copied()) {
            self.consume();
        }

        // Consume fractional portion of number (if it exists).
        if self.chars.peek().copied() == Some('.') && is_digit(self.peek_nth(1)) {
            self.consume();
            self.consume();

            while is_digit(self.chars.peek().copied()) {
                self.consume();
            }
        }

        Kind::Number(self.string.parse().unwrap())
    }

    /// Consume a literal. Match against keywords.
    fn consume_identifier(&mut self) -> Kind {
        while is_ident_char(self.chars.peek().copied()) {
            self.consume();
        }

        match self.string.as_ref() {
            "and" => Kind::And,
            "class" => Kind::Class,
            "else" => Kind::Else,
            "false" => Kind::False,
            "for" => Kind::For,
            "fun" => Kind::Fun,
            "if" => Kind::If,
            "nil" => Kind::Nil,
            "or" => Kind::Or,
            "print" => Kind::Print,
            "return" => Kind::Return,
            "super" => Kind::Super,
            "this" => Kind::This,
            "true" => Kind::True,
            "var" => Kind::Var,
            "while" => Kind::While,

            _ => Kind::Identifier(self.string.clone()),
        }
    }

    /// Peek 'n' number of characters ahead of the current character.
    fn peek_nth(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    /// Match against the next character in the source. Consume upon match and return appropriate Kind.
    fn match_next(&mut self, ch: char, k1: Kind, k2: Kind) -> Kind {
        if Some(&ch) == self.chars.peek() {
            self.consume();
            k1
        } else {
            k2
        }
    }

    /// Consume (and discard) any whitespace characters.
    fn discard(&mut self) {
        while let Some(ch) = self.peek_nth(0) {
            match ch {
                // Whitespace.
                ' ' | '\t' | '\r' | '\n' => {
                    self.chars.next();
                    self.index += 1;
                }

                // Comments. Discard characters until end of the line or end of file.
                '/' if self.peek_nth(1) == Some('/') => {
                    while self.chars.peek().is_some() && self.chars.peek() != Some(&'\n') {
                        self.chars.next();
                        self.index += 1;
                    }
                }

                _ => break,
            }
        }
    }
}

fn is_digit(ch: Option<char>) -> bool {
    if let Some(_d @ '0'..='9') = ch {
        true
    } else {
        false
    }
}

fn is_ident_char(ch: Option<char>) -> bool {
    if let Some(c) = ch {
        match c {
            'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_single_characters() {
        let source = "()[]{};,.-+*";

        let scanner = Scanner::new(source);
        let received: Vec<Token> = scanner.collect();

        let expected = vec![
            Token::new(Kind::LeftParen, 0..1),
            Token::new(Kind::RightParen, 1..2),
            Token::new(Kind::LeftSquare, 2..3),
            Token::new(Kind::RightSquare, 3..4),
            Token::new(Kind::LeftBrace, 4..5),
            Token::new(Kind::RightBrace, 5..6),
            Token::new(Kind::SemiColon, 6..7),
            Token::new(Kind::Comma, 7..8),
            Token::new(Kind::Dot, 8..9),
            Token::new(Kind::Minus, 9..10),
            Token::new(Kind::Plus, 10..11),
        ];

        for (e, r) in expected.into_iter().zip(received) {
            assert_eq!(e, r)
        }
    }

    #[test]
    fn lex_single_or_dual_character_operators() {
        let source = "! != = == < <= > >=";

        let scanner = Scanner::new(source);
        let received: Vec<Token> = scanner.collect();

        let expected = vec![
            Token::new(Kind::Bang, 0..1),
            Token::new(Kind::BangEq, 2..4),
            Token::new(Kind::Eq, 5..6),
            Token::new(Kind::DoubleEq, 7..9),
            Token::new(Kind::Less, 10..11),
            Token::new(Kind::LessEq, 12..14),
            Token::new(Kind::Greater, 15..16),
            Token::new(Kind::GreaterEq, 17..19),
        ];

        for (e, r) in expected.into_iter().zip(received) {
            assert_eq!(e, r);
        }
    }

    #[test]
    fn lex_comments() {
        let mut source = String::new();
        source.push_str(" // This is a comment!\n");
        source.push_str(" ! == ! // Another comment here too!");

        let scanner = Scanner::new(&source);
        let received: Vec<Token> = scanner.collect();

        let expected = vec![
            Token::new(Kind::Bang, 24..25),
            Token::new(Kind::DoubleEq, 26..28),
            Token::new(Kind::Bang, 29..30),
        ];

        for (e, r) in expected.into_iter().zip(received) {
            assert_eq!(e, r);
        }
    }

    #[test]
    fn lex_strings() {
        let source = r#""This is a string!""#;

        let mut scanner = Scanner::new(source);
        let received = scanner.next().unwrap();

        let expected = Token::new(Kind::String("This is a string!".to_string()), 0..19);

        assert_eq!(received, expected);
    }

    #[test]
    fn lex_number() {
        let source = "123.456";

        let mut scanner = Scanner::new(source);
        let received = scanner.next().unwrap();

        let expected = Token::new(Kind::Number(123.456), 0..7);

        assert_eq!(received, expected);
    }

    #[test]
    fn lex_identifier() {
        let source = "ident";

        let mut scanner = Scanner::new(source);
        let received = scanner.next().unwrap();

        let expected = Token::new(Kind::Identifier("ident".to_string()), 0..5);

        assert_eq!(received, expected);
    }

    #[test]
    #[rustfmt::skip]
    fn lex_keywords() {
        let mut source = String::new();
        source.push_str("and or if else\n");
        source.push_str("class super this\n");
        source.push_str("true false nil\n");
        source.push_str("for while\n");
        source.push_str("var fun\n");
        source.push_str("print return\n");

        let scanner = Scanner::new(&source);
        let received: Vec<Token> = scanner.collect();

        let expected = vec![
            // "and or if else"
            Token::new(Kind::And, 0..3),
            Token::new(Kind::Or, 4..6),
            Token::new(Kind::If, 7..9),
            Token::new(Kind::Else, 10..14),
            
            // "class super this"
            Token::new(Kind::Class, 15..20),
            Token::new(Kind::Super, 21..26),
            Token::new(Kind::This, 27..31),
            
            // "true false nil"
            Token::new(Kind::True, 32..36),
            Token::new(Kind::False, 37..42),
            Token::new(Kind::Nil, 43..46),
            
            // "for while"
            Token::new(Kind::For, 47..50),
            Token::new(Kind::While, 51..56),
            
            // "var fun"
            Token::new(Kind::Var, 57..60),
            Token::new(Kind::Fun, 61..64),
            
            // "print return"
            Token::new(Kind::Print, 65..70),
            Token::new(Kind::Return, 71..77),
        ];

        for (e, r) in expected.into_iter().zip(received) {
            assert_eq!(e, r);
        }
    }
}
