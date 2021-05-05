use crate::ast::{Expr, Stmt};
use crate::errors::ParsingError;
use crate::scanner::Scanner;
use crate::tokens::{Kind, Token};

use crate::tokens::Kind::SemiColon;
use std::iter::Peekable;
use std::mem;

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

    /// program ---> declaration* EOF
    pub fn parse(&mut self) -> (Vec<Box<Stmt>>, Vec<ParsingError>) {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.finished() {
            match self.declaration() {
                Ok(stmt) => statements.push(Box::new(stmt)),
                Err(err) => {
                    errors.push(err);
                    self.synchronise();
                }
            }
        }

        (statements, errors)
    }

    /// declaration ---> varDeclaration | statement
    fn declaration(&mut self) -> Result<Stmt, ParsingError> {
        if self.check(&[Kind::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    /// varDecl ---> "var" IDENTIFIER ( "=" expression )? ";"
    fn var_declaration(&mut self) -> Result<Stmt, ParsingError> {
        self.consume(Kind::Var)?;

        let name = self.consume(Kind::Identifier("".to_string()))?; // Yikes!
        let mut initial = Expr::Literal(Token::new(Kind::Nil, 0..0)); // Also, yikes!

        if self.consume(Kind::Eq).is_ok() {
            initial = self.expression()?;
        }

        self.consume(SemiColon)?;
        Ok(Stmt::Variable {
            name,
            initial: Box::new(initial),
        })
    }

    /// statement ---> exprStmt | forStmt | ifStmt | printStmt | whileStmt | block
    fn statement(&mut self) -> Result<Stmt, ParsingError> {
        let peeked = self.peek();

        if peeked.is_none() {
            return Err(ParsingError::UnexpectedEOF {
                expected: "statement".to_string(),
            });
        }

        match peeked.unwrap().kind {
            Kind::LeftBrace => Ok(Stmt::Block(self.block()?)),
            Kind::Print => self.print_statement(),
            Kind::If => self.if_statement(),
            Kind::While => self.while_statement(),
            Kind::For => self.for_statement(),
            _ => self.expression_statement(),
        }
    }

    /// forStmt ---> "for" "(" ( varDecl | exprStmt | ";") expression? ";" expression? ")" statement
    fn for_statement(&mut self) -> Result<Stmt, ParsingError> {
        // Parse the for loop
        // ------------------

        // Consume the required opening tokens
        self.consume(Kind::For)?;
        self.consume(Kind::LeftParen)?;

        // TODO: This is ripe for being made a method. Replace self.is_finished() perhaps? CBA right now.
        // Check previous token consumption hasn't put us at EOF
        let peeked = self.peek().ok_or(ParsingError::UnexpectedEOF {
            expected: "initialiser".to_string(),
        })?;

        // Parse optional initialiser
        let opt_initialiser = match peeked.kind {
            Kind::SemiColon => None,
            Kind::Var => Some(self.var_declaration()?),
            _ => Some(self.expression_statement()?),
        };

        // Check previous token consumption hasn't put us at EOF
        let peeked = self.peek().ok_or(ParsingError::UnexpectedEOF {
            expected: "conditional".to_string(),
        })?;

        // Parse optional condition
        let opt_condition = match peeked.kind {
            Kind::SemiColon => None,
            _ => Some(self.expression()?),
        };

        // Consume semicolon regardless of condition's presence.
        self.consume(Kind::SemiColon)?;

        // Check previous token consumption hasn't put us at EOF
        let peeked = self.peek().ok_or(ParsingError::UnexpectedEOF {
            expected: "incremental expression".to_string(),
        })?;

        // Parse optional increment
        let opt_increment = match peeked.kind {
            Kind::RightParen => None,
            _ => Some(self.expression()?),
        };

        // Consume semicolon regardless of condition's presence.
        self.consume(Kind::SemiColon)?;

        // Consume closing paren of the "header".
        self.consume(Kind::RightParen)?;

        // Parse body of for loop.
        let body = self.statement()?;

        // Desugar the for loop into a while statement.
        // --------------------------------------------

        // Optional incrementer gets placed at end of the body of the for loop.
        let while_body = {
            let mut while_body = vec![Box::new(body)];

            if let Some(increment) = opt_increment {
                while_body.push(Box::new(Stmt::Expr(Box::new(increment))));
            }

            Stmt::Block(while_body)
        };

        // Optional condition gets handled. None is converted to true.
        let while_stmt = {
            let while_cond = opt_condition.unwrap_or(Expr::Literal(Token::new(Kind::True, 0..0)));

            Stmt::While {
                condition: Box::new(while_cond),
                body: Box::new(while_body),
            }
        };

        // Optional initialiser gets place before while loop and both statements get wrapped in a block.
        Ok(match opt_initialiser {
            Some(initialiser) => Stmt::Block(vec![Box::new(initialiser), Box::new(while_stmt)]),
            None => while_stmt,
        })
    }

    /// ifStmt ---> "if" "(" expression ")" statement ( "else" statement )?
    fn if_statement(&mut self) -> Result<Stmt, ParsingError> {
        self.consume(Kind::If)?;
        self.consume(Kind::LeftParen)?;

        let condition = Box::new(self.expression()?);

        self.consume(Kind::RightParen)?;

        let if_branch = Box::new(self.statement()?);

        let else_branch = if self.check(&[Kind::Else]) {
            self.consume(Kind::Else)?;
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            if_branch,
            else_branch,
        })
    }

    ///  whileStmt ---> "while" "(" expression ")" statement
    fn while_statement(&mut self) -> Result<Stmt, ParsingError> {
        self.consume(Kind::While)?;
        self.consume(Kind::LeftParen)?;

        let condition = Box::new(self.expression()?);

        self.consume(Kind::RightParen)?;

        let body = Box::new(self.statement()?);

        Ok(Stmt::While { condition, body })
    }

    /// block ---> "{" declaration* "}"
    fn block(&mut self) -> Result<Vec<Box<Stmt>>, ParsingError> {
        self.consume(Kind::LeftBrace)?;

        let mut stmts = Vec::new();

        while !self.check(&[Kind::RightBrace]) && !self.finished() {
            stmts.push(Box::new(self.declaration()?));
        }

        self.consume(Kind::RightBrace)?;
        Ok(stmts)
    }

    /// printStmt ---> "print" expression ";"
    fn print_statement(&mut self) -> Result<Stmt, ParsingError> {
        self.next();

        let expr = Box::new(self.expression()?);
        self.consume(Kind::SemiColon)?;
        Ok(Stmt::Print(expr))
    }

    /// exprStmt ---> expression ";"
    fn expression_statement(&mut self) -> Result<Stmt, ParsingError> {
        let expr = Box::new(self.expression()?);
        self.consume(Kind::SemiColon)?;
        Ok(Stmt::Expr(expr))
    }

    /// expression ---> assignment
    fn expression(&mut self) -> Result<Expr, ParsingError> {
        self.assignment()
    }

    /// assignment ---> Identifier "=" assignment | logicOr
    fn assignment(&mut self) -> Result<Expr, ParsingError> {
        let expr = self.logic_or();

        if self.check(&[Kind::Eq]) {
            let eq = self.next();
            let value = Box::new(self.logic_or()?);

            return if let Ok(Expr::Variable(name)) = expr {
                Ok(Expr::Assign { name, value })
            } else {
                Err(ParsingError::InvalidAssignmentTarget { source: eq })
            };
        }

        expr
    }

    /// logicOr ---> logicAnd ( "or" logicAnd)*
    fn logic_or(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.logic_and()?;

        while self.check(&[Kind::Or]) {
            let op = self.next();
            let rhs = self.logic_and()?;

            lhs = Expr::Logical {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            }
        }

        Ok(lhs)
    }

    /// logicAnd ---< equality ( "and" equality )*
    fn logic_and(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.equality()?;

        while self.check(&[Kind::And]) {
            let op = self.next();
            let rhs = self.equality()?;

            lhs = Expr::Logical {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            }
        }

        Ok(lhs)
    }

    /// equality ---> comparison ( ( "!=" | "==" ) comparison ) *
    fn equality(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.comparison()?;

        while self.check(&[Kind::BangEq]) {
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
    fn comparison(&mut self) -> Result<Expr, ParsingError> {
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
    fn term(&mut self) -> Result<Expr, ParsingError> {
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
    fn factor(&mut self) -> Result<Expr, ParsingError> {
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
    fn unary(&mut self) -> Result<Expr, ParsingError> {
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

    /// primary ---> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER
    fn primary(&mut self) -> Result<Expr, ParsingError> {
        // Check if at end of file.
        self.peek().ok_or(ParsingError::UnexpectedEOF {
            expected: "value or parenthesised expression".to_string(),
        })?;

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

            // Handle identifier usage.
            Kind::Identifier(_) => Ok(Expr::Variable(self.next())),

            // Handle errors that occurred during lexing.
            Kind::Error(ref e) => Err(*e.clone()),

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
        // Check if at end of file.
        let peeked = self.peek().ok_or(ParsingError::UnexpectedEOF {
            expected: format!("{}", expected),
        })?;

        // Compare expected with next.
        if mem::discriminant(&peeked.kind) != mem::discriminant(&expected) {
            return Err(ParsingError::UnexpectedToken {
                expected: format!("{}", expected),
                received: peeked.clone(),
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

        kinds.contains(&peeked.unwrap().kind)
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
        let input = "!*10;";
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
        let input = "!!!10;";
        let (received, errors) = parse_input(input);

        for error in errors {
            println!("{}", error);
        }

        let expected = Box::new(Stmt::Expr(Box::new(Expr::Unary {
            op: Token::new(Kind::Bang, 0..1),
            expr: Box::new(Expr::Unary {
                op: Token::new(Kind::Bang, 1..2),
                expr: Box::new(Expr::Unary {
                    op: Token::new(Kind::Bang, 2..3),
                    expr: Box::new(Expr::Literal(Token::new(Kind::Number(10.0), 3..5))),
                }),
            }),
        })));

        assert_eq!(received[0], expected);
    }

    #[test]
    fn parse_simple_arithmetic_expression() {
        let input = "(10 + 5) * (9 - 2);";
        let (received, _) = parse_input(input);

        let expected = Box::new(Stmt::Expr(Box::new(Expr::Binary {
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
        })));

        assert_eq!(received[0], expected);
    }

    #[test]
    fn parse_comparison_expression() {
        let input = "5 == 10;";
        let (received, _) = parse_input(input);

        let expected = Box::new(Stmt::Expr(Box::new(Expr::Binary {
            left: Box::new(Expr::Literal(Token::new(Kind::Number(5.0), 0..1))),

            op: Token::new(Kind::DoubleEq, 2..4),

            right: Box::new(Expr::Literal(Token::new(Kind::Number(10.0), 5..7))),
        })));

        assert_eq!(received[0], expected);

        let input = "10 != 5;";
        let (received, _) = parse_input(input);

        let expected = Box::new(Stmt::Expr(Box::new(Expr::Binary {
            left: Box::new(Expr::Literal(Token::new(Kind::Number(10.0), 0..2))),

            op: Token::new(Kind::BangEq, 3..5),

            right: Box::new(Expr::Literal(Token::new(Kind::Number(5.0), 6..7))),
        })));

        assert_eq!(received[0], expected);
    }

    #[test]
    fn parse_variable_declaration_and_usage() {
        let input = "var x = 5;";
        let (stmts, _) = parse_input(input);

        let expected = Box::new(Stmt::Variable {
            name: Token::new(Kind::Identifier("x".to_string()), 4..5),
            initial: Box::new(Expr::Literal(Token::new(Kind::Number(5.0), 8..9))),
        });

        assert_eq!(stmts[0], expected);

        let input = "var x = 5 + 10;";
        let (stmts, _) = parse_input(input);

        let expected = Box::new(Stmt::Variable {
            name: Token::new(Kind::Identifier("x".to_string()), 4..5),
            initial: Box::new(Expr::Binary {
                left: Box::new(Expr::Literal(Token::new(Kind::Number(5.0), 8..9))),
                op: Token::new(Kind::Plus, 10..11),
                right: Box::new(Expr::Literal(Token::new(Kind::Number(10.0), 12..14))),
            }),
        });

        assert_eq!(stmts[0], expected);

        let input = "var x = y;";
        let (stmts, _) = parse_input(input);

        let expected = Box::new(Stmt::Variable {
            name: Token::new(Kind::Identifier("x".to_string()), 4..5),
            initial: Box::new(Expr::Variable(Token::new(
                Kind::Identifier("y".to_string()),
                8..9,
            ))),
        });

        assert_eq!(stmts[0], expected);
    }

    #[test]
    fn parse_variable_assignment() {
        let input = "x = 2;";
        let (stmts, _) = parse_input(input);

        let expected = Box::new(Stmt::Expr(Box::new(Expr::Assign {
            name: Token::new(Kind::Identifier("x".to_string()), 0..1),
            value: Box::new(Expr::Literal(Token::new(Kind::Number(2.0), 4..5))),
        })));

        assert_eq!(stmts[0], expected);
    }

    #[test]
    fn parse_blocks() {
        let input = "{\
                var x = 1;\
                var y = 2;\
            }";

        let (stmts, _) = parse_input(input);

        let expected = Box::new(Stmt::Block(vec![
            Box::new(Stmt::Variable {
                name: Token::new(Kind::Identifier("x".to_string()), 5..6),
                initial: Box::new(Expr::Literal(Token::new(Kind::Number(1.0), 9..10))),
            }),
            Box::new(Stmt::Variable {
                name: Token::new(Kind::Identifier("y".to_string()), 15..16),
                initial: Box::new(Expr::Literal(Token::new(Kind::Number(2.0), 19..20))),
            }),
        ]));

        assert_eq!(stmts[0], expected);
    }

    #[test]
    fn parse_conditionals() {
        let input = "\
            if (2 == 2)\
                print true;\
            else \
                print false;";

        let (stmts, _) = parse_input(input);

        let expected = Box::new(Stmt::If {
            condition: Box::new(Expr::Binary {
                left: Box::new(Expr::Literal(Token::new(Kind::Number(2.0), 4..5))),
                op: Token::new(Kind::DoubleEq, 6..8),
                right: Box::new(Expr::Literal(Token::new(Kind::Number(2.0), 9..10))),
            }),

            if_branch: Box::new(Stmt::Print(Box::new(Expr::Literal(Token::new(
                Kind::True,
                17..21,
            ))))),

            else_branch: Some(Box::new(Stmt::Print(Box::new(Expr::Literal(Token::new(
                Kind::False,
                33..38,
            )))))),
        });

        assert_eq!(stmts[0], expected);
    }

    #[test]
    fn parse_logical_expressions() {
        let input = "nil or \"yes\";";
        let (stmts, _) = parse_input(input);

        let expected = Box::new(Stmt::Expr(Box::new(Expr::Logical {
            left: Box::new(Expr::Literal(Token::new(Kind::Nil, 0..3))),
            op: Token::new(Kind::Or, 4..6),
            right: Box::new(Expr::Literal(Token::new(
                Kind::String("yes".to_string()),
                7..12,
            ))),
        })));

        assert_eq!(stmts[0], expected);
    }

    #[test]
    fn parse_while_statement() {
        let input = "\
            while (1 + 1 == 2) {\
                print \"Hello\";\
                print 13579;\
            }";

        let (stmts, _) = parse_input(input);

        let expected = Box::new(Stmt::While {
            condition: Box::new(Expr::Binary {
                left: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(Token::new(Kind::Number(1.0), 7..8))),
                    op: Token::new(Kind::Plus, 9..10),
                    right: Box::new(Expr::Literal(Token::new(Kind::Number(1.0), 11..12))),
                }),

                op: Token::new(Kind::DoubleEq, 13..15),

                right: Box::new(Expr::Literal(Token::new(Kind::Number(2.0), 16..17))),
            }),

            body: Box::new(Stmt::Block(vec![
                Box::new(Stmt::Print(Box::new(Expr::Literal(Token::new(
                    Kind::String("Hello".to_string()),
                    26..33,
                ))))),
                Box::new(Stmt::Print(Box::new(Expr::Literal(Token::new(
                    Kind::Number(13579.0),
                    40..45,
                ))))),
            ])),
        });

        assert_eq!(stmts[0], expected);
    }

    #[test]
    fn parse_for_statement() {
        let input = "\
        for (var x = 0; x < 5; x = x + 1) {\
            print x;\
        }";

        let (stmts, e) = parse_input(input);

        println!("{:?}", e);

        let expected = Box::new(Stmt::Block(vec![
            Box::new(Stmt::Variable {
                name: Token::new(Kind::Identifier("x".to_string()), 9..10),
                initial: Box::new(Expr::Literal(Token::new(Kind::Number(0.0), 13..14))),
            }),
            Box::new(Stmt::While {
                condition: Box::new(Expr::Binary {
                    left: Box::new(Expr::Variable(Token::new(
                        Kind::Identifier("x".to_string()),
                        16..17,
                    ))),

                    op: Token::new(Kind::Less, 18..19),
                    right: Box::new(Expr::Literal(Token::new(Kind::Number(5.0), 20..21))),
                }),

                body: Box::new(Stmt::Block(vec![
                    Box::new(Stmt::Block(vec![Box::new(Stmt::Print(Box::new(
                        Expr::Variable(Token::new(Kind::Identifier("x".to_string()), 41..42)),
                    )))])),
                    Box::new(Stmt::Expr(Box::new(Expr::Assign {
                        name: Token::new(Kind::Identifier("x".to_string()), 23..24),
                        value: Box::new(Expr::Binary {
                            left: Box::new(Expr::Variable(Token::new(
                                Kind::Identifier("x".to_string()),
                                27..28,
                            ))),
                            op: Token::new(Kind::Plus, 29..30),
                            right: Box::new(Expr::Literal(Token::new(Kind::Number(1.0), 31..32))),
                        }),
                    }))),
                ])),
            }),
        ]));

        assert_eq!(stmts[0], expected);
    }
}
