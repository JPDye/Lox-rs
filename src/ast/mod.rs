use std::fmt::Write;

use crate::tokens::{Kind, Token};

pub enum Node {
    Unary {
        op: Token,
        expr: Box<Node>,
    },

    Binary {
        left: Box<Node>,
        op: Token,
        right: Box<Node>,
    },

    Grouping {
        expr: Box<Node>,
    },

    Literal {
        value: Token,
    },
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pretty_print())
    }
}

impl Node {
    pub fn pretty_print(&self) -> String {
        let mut out = String::new();

        let res = match self {
            Self::Literal { value } => format!("{}", value.kind),
            Self::Grouping { expr } => format!("({})", expr),
            Self::Unary { op, expr } => format!("({} {})", op, expr),
            Self::Binary { left, op, right } => format!("({} {} {})", op, left, right),

            _ => panic!("aahh"),
        };

        out.push_str(&res);
        out
    }

    fn parenthesise(op: String, exprs: &[Node]) -> String {
        let mut out = format!("({}", op);

        for expr in exprs {
            out.push_str(&format!(" {}", expr));
        }

        out.push_str(")");

        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::scanner::Scanner;

    fn assert_output(node: Node, expected: &str) {
        let mut buf = String::new();
        let _ = write!(buf, "{}", node);
        assert_eq!(buf, expected);
    }

    #[test]
    fn pretty_print_number() {
        let token = Token::new(Kind::Number(123.456), 0..7);
        let node = Node::Literal { value: token };

        assert_output(node, "123.456");
    }

    #[test]
    fn pretty_print_string() {
        let token = Token::new(Kind::String("Hello!".to_string()), 0..6);
        let node = Node::Literal { value: token };

        assert_output(node, "Hello!");
    }

    #[test]
    fn pretty_print_booleans() {
        let f = Token::new(Kind::False, 0..5);
        let t = Token::new(Kind::True, 0..4);
        let n = Token::new(Kind::Nil, 0..3);

        let node = Node::Literal { value: f };
        assert_output(node, "false");

        let node = Node::Literal { value: t };
        assert_output(node, "true");

        let node = Node::Literal { value: n };
        assert_output(node, "nil");
    }

    #[test]
    fn pretty_print_grouping() {
        let value = Token::new(Kind::Number(135.79), 0..3);
        let expr = Box::new(Node::Literal { value });
        let group = Node::Grouping { expr };

        assert_output(group, "(135.79)");
    }

    #[test]
    fn pretty_print_binary_expression() {
        let mut scanner = Scanner::new("9 + 10");
        let tokens: Vec<Token> = scanner.collect();

        let binary = Node::Binary {
            left: Box::new(Node::Literal {
                value: tokens[0].clone(),
            }),

            op: tokens[1].clone(),

            right: Box::new(Node::Literal {
                value: tokens[2].clone(),
            }),
        };

        assert_output(binary, "(+ 9 10)");
    }

    #[test]
    fn pretty_print_unary_expression() {
        let mut scanner = Scanner::new("-10");
        let tokens: Vec<Token> = scanner.collect();

        let unary = Node::Unary {
            op: tokens[0].clone(),

            expr: Box::new(Node::Literal {
                value: tokens[1].clone(),
            }),
        };

        assert_output(unary, "(- 10)");
    }

    #[test]
    fn pretty_print_nested_expression() {
        let mut scanner = Scanner::new("-123 * (45.67)");
        let tokens: Vec<Token> = scanner.collect();

        let node = Node::Binary {
            left: Box::new(Node::Unary {
                op: tokens[0].clone(),
                expr: Box::new(Node::Literal {
                    value: tokens[1].clone(),
                }),
            }),

            op: tokens[2].clone(),

            right: Box::new(Node::Grouping {
                expr: Box::new(Node::Literal {
                    value: tokens[4].clone(),
                }),
            }),
        };

        assert_output(node, "(* (- 123) (45.67))");
    }
}
