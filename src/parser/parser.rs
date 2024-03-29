use super::parsing_error::ParsingError;
use crate::ast::{Expr, Stmt};
use crate::scanner::Scanner;
use crate::tokens::{Kind, Token};

use std::iter::Peekable;

type StmtResult = Result<Stmt, ParsingError>;
type ExprResult = Result<Expr, ParsingError>;

/// Create an Abstract Syntax Tree from a Scanner (an iterator over tokens in a string).
/// This is a top-down, recursive descent parser. Relatively fast, efficient and easy to understand.
pub struct Parser<'a> {
    scanner: Peekable<Scanner<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        Self {
            scanner: scanner.peekable(),
        }
    }

    /// When in panic mode, consume tokens until reaching a new statement so parsing can being again.
    fn synchronise(&mut self) {
        while !self.finished() {
            let peeked = self.peek().unwrap();

            // If next token is a semicolon, advance past it and return.
            if peeked.kind == Kind::SemiColon {
                self.next();
                return;
            }

            // If next token is a keyword, return.
            match peeked.kind {
                Kind::Class | Kind::Fun | Kind::Var | Kind::For | Kind::If | Kind::While => return,
                _ => {
                    self.next();
                }
            }
        }
    }

    /// program ---> statement* EOF
    pub fn parse(&mut self) -> (Vec<Box<Stmt>>, Vec<ParsingError>) {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.finished() {
            match self.statement() {
                Ok(expr) => statements.push(Box::new(expr)),
                Err(err) => errors.push(err),
            }
        }

        (statements, errors)
    }

    /// statement ---> exprStmt | printStmt
    fn statement(&mut self) -> StmtResult {
        if self.check(&[Kind::Print]) {
            self.next();
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    /// printStmt ---> "print" expression ";"
    fn print_statement(&mut self) -> StmtResult {
        let expr = self.expression()?;
        self.consume(Kind::SemiColon);
        Ok(Stmt::Print(expr))
    }

    /// exprStmt ---> expression ";"
    fn expression_statement(&mut self) -> StmtResult {
        let expr = self.expression()?;
        self.consume(Kind::SemiColon);
        Ok(Stmt::Expr(expr))
    }

    /// expression ---> equality
    fn expression(&mut self) -> ExprResult {
        self.equality()
    }

    /// equality ---> comparison ( ( "!=" | "==" ) comparison ) *
    fn equality(&mut self) -> ExprResult {
        let mut lhs = self.comparison()?;

        while self.check(&[Kind::Eq, Kind::BangEq]) {
            let op = self.next();
            let rhs = self.comparison()?;

            lhs = Expr::Binary {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    /// comparison ---> term ( ( ">" | ">=" | "<" | "<=" ) term ) *
    fn comparison(&mut self) -> ExprResult {
        let mut lhs = self.term()?;

        while self.check(&[
            Kind::Greater,
            Kind::GreaterEq,
            Kind::Less,
            Kind::LessEq,
            Kind::DoubleEq,
            Kind::BangEq,
        ]) {
            let op = self.next();
            let rhs = self.term()?;

            lhs = Expr::Binary {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    /// term ---> factor ( ( "-" | "+" ) factor ) *
    fn term(&mut self) -> ExprResult {
        let mut lhs = self.factor()?;

        while self.check(&[Kind::Minus, Kind::Plus]) {
            let op = self.next();
            let rhs = self.factor()?;

            lhs = Expr::Binary {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    /// factor ---> unary ( ( "/" | "*" ) unary ) *
    fn factor(&mut self) -> ExprResult {
        let mut lhs = self.unary()?;

        while self.check(&[Kind::Slash, Kind::Star]) {
            let op = self.next();
            let rhs = self.unary()?;

            lhs = Expr::Binary {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    /// unary ---> ( ( "-" | "!" ) unary ) | primary;
    fn unary(&mut self) -> ExprResult {
        if self.check(&[Kind::Minus, Kind::Bang]) {
            let op = self.next();
            let rhs = self.unary()?;

            let node = Expr::Unary {
                op,
                expr: Box::new(rhs),
            };

            return Ok(node);
        }
        return self.primary();
    }

    /// primary ---> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
    fn primary(&mut self) -> ExprResult {
        // Check if at end of file.
        if self.peek().is_none() {
            return Err(ParsingError::UnexpectedEOF {
                expected: "value or parenthesised expression".to_string(),
            });
        }

        match self.peek().unwrap().kind {
            // Handle literal values.
            Kind::False | Kind::True | Kind::Nil | Kind::String(_) | Kind::Number(_) => {
                Ok(Expr::Literal(self.next()))
            }

            // Handle parenthesised expressions.
            Kind::LeftParen => {
                self.next();

                let expr = self.expression()?;
                self.consume(Kind::RightParen)?;

                Ok(Expr::Group(Box::new(expr)))
            }

            // Handle unexpected tokens.
            _ => Err(ParsingError::UnexpectedToken {
                expected: "value or parenthesised expression".to_string(),
                received: self.next(),
            }),
        }
    }

    /// Check if end of file has been reached.
    fn finished(&mut self) -> bool {
        self.scanner.peek().is_none()
    }

    /// Advance the scanner and return the token.
    fn next(&mut self) -> Token {
        self.scanner.next().unwrap()
    }

    /// Attempt to consume an expected token. Return an error (UnexpectedToken) if it cannot.
    fn consume(&mut self, expected: Kind) -> Result<Token, ParsingError> {
        let peeked = self.peek();

        // Check if at end of file.
        if peeked.is_none() {
            return Err(ParsingError::UnexpectedEOF {
                expected: format!("{:?}", expected),
            });
        }

        // Check if token is expected token.
        if peeked.unwrap().kind != expected {
            return Err(ParsingError::UnexpectedToken {
                expected: format!("{:?}", expected),
                received: peeked.unwrap().clone(),
            });
        }

        Ok(self.next())
    }

    /// Match a collection of token kinds against the next token.
    fn check(&mut self, kinds: &[Kind]) -> bool {
        let peeked = self.peek();

        if peeked.is_none() {
            return false;
        }

        // TODO: This
        // kinds.contains(&peeked.unwrap().kind)

        for kind in kinds {
            if kind == &peeked.unwrap().kind {
                return true;
            }
        }

        false
    }

    /// Peek at the next token.
    fn peek(&mut self) -> Option<&Token> {
        self.scanner.peek()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_input;

    #[test]
    fn parse_invalid_unary_expression() {
        let input = "!*10";
        let (_, received) = parse_input(input);

        let expected = ParsingError::UnexpectedToken {
            expected: "value or parenthesised expression".to_string(),
            received: Token {
                kind: Kind::Star,
                range: 1..2,
            },
        };

        assert_eq!(received[0], expected);
    }

    #[test]
    fn parse_invalid_binary_expression() {
        // Test with invalid syntax.
        let input = "10 + +";
        let (_, received) = parse_input(input);

        let expected = ParsingError::UnexpectedToken {
            expected: "value or parenthesised expression".to_string(),
            received: Token {
                kind: Kind::Plus,
                range: 5..6,
            },
        };

        assert_eq!(received[0], expected);

        // Test with unexpected EOF.
        let input = "10 +";
        let (_, received) = parse_input(input);

        let expected = ParsingError::UnexpectedEOF {
            expected: "value or parenthesised expression".to_string(),
        };

        assert_eq!(received[0], expected);
    }

    #[test]
    fn parse_multiple_unary_operators() {
        let input = "!!!10";
        let (received, _) = parse_input(input);

        let expected = Box::new(Expr::Unary {
            op: Token::new(Kind::Bang, 0..1),
            expr: Box::new(Expr::Unary {
                op: Token::new(Kind::Bang, 1..2),
                expr: Box::new(Expr::Unary {
                    op: Token::new(Kind::Bang, 2..3),
                    expr: Box::new(Expr::Literal(Token::new(Kind::Number(10.0), 3..5))),
                }),
            }),
        });

        assert_eq!(received[0], expected);
    }

    #[test]
    fn parse_simple_expression() {
        let input = "(10 + 5) * (9 - 2)";
        let (received, _) = parse_input(input);

        let expected = Box::new(Expr::Binary {
            left: Box::new(Expr::Group(Box::new(Expr::Binary {
                left: Box::new(Expr::Literal(Token::new(Kind::Number(10.0), 1..3))),

                op: Token::new(Kind::Plus, 4..5),

                right: Box::new(Expr::Literal(Token::new(Kind::Number(5.0), 6..7))),
            }))),

            op: Token::new(Kind::Star, 9..10),

            right: Box::new(Expr::Group(Box::new(Expr::Binary {
                left: Box::new(Expr::Literal(Token::new(Kind::Number(9.0), 12..13))),

                op: Token::new(Kind::Minus, 14..15),

                right: Box::new(Expr::Literal(Token::new(Kind::Number(2.0), 16..17))),
            }))),
        });

        assert_eq!(received[0], expected);
    }

    #[test]
    fn parse_equality() {
        let input = "5 == 10";
        let (received, _) = parse_input(input);

        let expected = Box::new(Expr::Binary {
            left: Box::new(Expr::Literal(Token::new(Kind::Number(5.0), 0..1))),

            op: Token::new(Kind::DoubleEq, 2..4),

            right: Box::new(Expr::Literal(Token::new(Kind::Number(10.0), 5..7))),
        });

        assert_eq!(received[0], expected);

        let input = "5 != 10";
        let (received, _) = parse_input(input);

        let expected = Box::new(Expr::Binary {
            left: Box::new(Expr::Literal(Token::new(Kind::Number(5.0), 0..1))),

            op: Token::new(Kind::BangEq, 2..4),

            right: Box::new(Expr::Literal(Token::new(Kind::Number(10.0), 5..7))),
        });

        assert_eq!(received[0], expected);
    }
}
