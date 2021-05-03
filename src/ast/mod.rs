use crate::tokens::Token;

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

    Variable(Token),
    Assign {
        name: Token,
        value: Box<Expr>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Variable { name: Token, initial: Box<Expr> },
    Block(Vec<Box<Stmt>>),
}
