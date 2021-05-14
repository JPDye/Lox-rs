use crate::tokens::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Group(Box<Expr>),
    Literal(Token),
    Unary {
        op: Token,
        expr: Box<Expr>,
    },

    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },

    Variable(Token),
    Assign {
        name: Token,
        value: Box<Expr>,
    },

    Logical {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },

    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Box<Expr>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Box<Expr>),
    Print(Box<Expr>),

    Variable {
        name: Token,
        initial: Box<Expr>,
    },

    Block(Vec<Box<Stmt>>),

    If {
        condition: Box<Expr>,
        if_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },

    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
}
