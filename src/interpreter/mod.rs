use crate::ast::{Expr, Stmt};
use crate::tokens::{Kind, Token};
use crate::value::Value;

type RuntimeExprError = Result<Value, String>;
type RuntimeStmtError = Result<(), String>;

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(&mut self, stmts: Vec<Box<Stmt>>) {
        for stmt in stmts {
            if let Err(e) = self.execute_statement(stmt) {
                println!("RuntimeError: {}", e);
                break;
            }
        }
    }

    // Helper function to determine if a Value is true. Follows Ruby convention. Everything but false and nil are true.
    fn is_truthy(kind: Value) -> bool {
        match kind {
            Value::Nil | Value::Boolean(false) => false,
            _ => true,
        }
    }

    // Helper function for returning an error when an operand expecting two numbers doesn't receive them.
    fn number_operands_error(op: Token, l: Value, r: Value) -> RuntimeExprError {
        Err(format!(
            "{:?} (at {}..{}) requires {} and {} to be numbers.",
            op.kind, op.range.start, op.range.end, l, r
        ))
    }
}

// Statement execution.
impl Interpreter {
    fn execute_statement(&mut self, stmt: Box<Stmt>) -> RuntimeStmtError {
        match *stmt {
            Stmt::Expr(expr) => self.visit_expression_statement(expr),
            Stmt::Print(expr) => self.visit_print_statement(expr),
            Stmt::Variable { name, initial } => todo!(),
        }
    }

    fn visit_expression_statement(&mut self, stmt: Box<Expr>) -> RuntimeStmtError {
        self.evaluate_expression(stmt)?;
        Ok(())
    }

    fn visit_print_statement(&mut self, stmt: Box<Expr>) -> RuntimeStmtError {
        let val = self.evaluate_expression(stmt)?;
        println!("{}", val);
        Ok(())
    }
}

// Expression evaluation.
impl Interpreter {
    fn evaluate_expression(&mut self, expr: Box<Expr>) -> RuntimeExprError {
        match *expr {
            Expr::Unary { op, expr } => self.visit_unary_expr(op, expr),
            Expr::Binary { left, op, right } => self.visit_binary_expr(left, op, right),
            Expr::Group(expr) => self.visit_group_expr(expr),
            Expr::Literal(value) => self.visit_literal_expr(value),
            Expr::Variable(name) => todo!(),
        }
    }

    fn visit_unary_expr(&mut self, op: Token, expr: Box<Expr>) -> RuntimeExprError {
        let val = self.evaluate_expression(expr)?;

        match op.kind {
            Kind::Minus => match val {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err("Unable to negate non-numbers.".to_string()),
            },

            Kind::Bang => Ok(Value::Boolean(!Interpreter::is_truthy(val))),
            _ => unreachable!(),
        }
    }

    fn visit_binary_expr(
        &mut self,
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    ) -> RuntimeExprError {
        let left = self.evaluate_expression(left)?;
        let right = self.evaluate_expression(right)?;

        match op.kind {
            // Minus operator only works on numbers.
            Kind::Minus => match (left.clone(), right.clone()) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                _ => Interpreter::number_operands_error(op, left, right),
            },

            // Star operator only works on numbers.
            Kind::Star => match (left.clone(), right.clone()) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                _ => Interpreter::number_operands_error(op, left, right),
            },

            // Slash operator only works on numbers. Checks for 0 division.
            Kind::Slash => match (left.clone(), right.clone()) {
                (Value::Number(l), Value::Number(r)) if r != 0.0 => Ok(Value::Number(l / r)),

                (Value::Number(_), Value::Number(r)) if r == 0.0 => Err(format!(
                    "Attempted division by zero at {}..{}.",
                    op.range.start, op.range.end
                )),
                _ => Interpreter::number_operands_error(op, left, right),
            },

            // Plus operator works on two numbers or two strings.
            Kind::Plus => match (left.clone(), right.clone()) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),

                _ => Err(format!(
                    "'+' (at {}..{}) is not supported for '{}' and '{}'.",
                    op.range.start, op.range.end, left, right
                )),
            },

            // Multiple nested matches are ugly, but it stops my IDE shouting at me for duplicated code.
            Kind::Less | Kind::LessEq | Kind::Greater | Kind::GreaterEq => {
                match (left.clone(), right.clone()) {
                    (Value::Number(_), Value::Number(_)) => Ok(match op.kind {
                        Kind::Less => Value::Boolean(left < right),
                        Kind::LessEq => Value::Boolean(left <= right),
                        Kind::Greater => Value::Boolean(left > right),
                        Kind::GreaterEq => Value::Boolean(left >= right),
                        _ => unreachable!(),
                    }),

                    _ => Interpreter::number_operands_error(op, left, right),
                }
            }

            Kind::DoubleEq => Ok(Value::Boolean(left == right)),
            Kind::BangEq => Ok(Value::Boolean(left != right)),

            _ => unreachable!(),
        }
    }

    fn visit_group_expr(&mut self, expr: Box<Expr>) -> RuntimeExprError {
        self.evaluate_expression(expr)
    }

    fn visit_literal_expr(&mut self, value: Token) -> RuntimeExprError {
        Ok(value.into())
    }
}

#[cfg(test)]
mod tests {}
