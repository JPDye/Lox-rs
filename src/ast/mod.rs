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

    Group {
        expr: Box<Expr>,
    },

    Literal {
        value: Token,
    },
}

type RuntimeError = Result<Value, String>;

pub trait ExprVisitor {
    fn evaluate(&mut self, expr: Box<Expr>) -> RuntimeError {
        match *expr {
            Expr::Unary { op, expr } => self.visit_unary_expr(op, expr),
            Expr::Binary { left, op, right } => self.visit_binary_expr(left, op, right),
            Expr::Group { expr } => self.visit_group_expr(expr),
            Expr::Literal { value } => self.visit_literal_expr(value),
        }
    }

    fn visit_unary_expr(&mut self, op: Token, expr: Box<Expr>) -> RuntimeError;
    fn visit_binary_expr(&mut self, left: Box<Expr>, op: Token, right: Box<Expr>) -> RuntimeError;
    fn visit_group_expr(&mut self, expr: Box<Expr>) -> RuntimeError;
    fn visit_literal_expr(&mut self, value: Token) -> RuntimeError;
}
