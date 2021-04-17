use crate::tokens::Token;
use crate::value::Value;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Unary {
        op: Token,
        expr: Box<Expr>,
    },

    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },

    Group(Box<Expr>),
    Literal(Token),
}

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
}
