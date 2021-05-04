use crate::ast::{Expr, Stmt};
use crate::environment::Environments;
use crate::errors::RuntimeError;
use crate::tokens::{Kind, Token};
use crate::value::Value;

use std::io::Write;

/// Helper function to determine if a Value is true. Follows Ruby convention. Everything but false and nil are true.
fn is_truthy(kind: &Value) -> bool {
    match kind {
        Value::Nil | Value::Boolean(false) => false,
        _ => true,
    }
}

/// Helper function for returning an error when an operand expecting two numbers doesn't receive them.
fn number_operands_error(op: Token, l: Value, r: Value) -> RuntimeError {
    RuntimeError::InvalidOperandTypes {
        op,
        types: vec![l, r],
    }
}

pub struct Interpreter<'a, W: Write> {
    out: &'a mut W,
    envs: Environments,
}

impl<'a, W: Write> Interpreter<'a, W> {
    pub fn new(out: &'a mut W) -> Self {
        Self {
            out,
            envs: Environments::new(),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Box<Stmt>>) {
        for stmt in stmts {
            if let Err(e) = self.execute_statement(stmt) {
                let _ = self.out.write(format!("{}\n", e).as_bytes());
                break;
            }
        }
    }
}

// Statement execution.
impl<W: Write> Interpreter<'_, W> {
    fn execute_statement(&mut self, stmt: Box<Stmt>) -> Result<(), RuntimeError> {
        match *stmt {
            Stmt::Variable { name, initial } => self.visit_variable_decl_statement(name, initial),
            Stmt::Expr(expr) => self.visit_expression_statement(expr),
            Stmt::Print(expr) => self.visit_print_statement(expr),
            Stmt::Block(stmts) => self.visit_block(stmts),

            Stmt::If {
                condition,
                if_branch,
                else_branch,
            } => self.visit_if_statement(condition, if_branch, else_branch),

            Stmt::While { condition, body } => self.visit_while_statement(condition, body),
        }
    }

    fn visit_if_statement(
        &mut self,
        condition: Box<Expr>,
        if_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    ) -> Result<(), RuntimeError> {
        let bool = self.evaluate_expression(condition)?;

        if is_truthy(&bool) {
            self.execute_statement(if_branch)?;
        } else if else_branch.is_some() {
            self.execute_statement(else_branch.unwrap())?;
        }

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        condition: Box<Expr>,
        body: Box<Stmt>,
    ) -> Result<(), RuntimeError> {
        while is_truthy(&self.evaluate_expression(condition.clone())?) {
            self.execute_statement(body.clone())?;
        }
        Ok(())
    }

    fn visit_expression_statement(&mut self, expr: Box<Expr>) -> Result<(), RuntimeError> {
        self.evaluate_expression(expr)?;
        Ok(())
    }

    fn visit_print_statement(&mut self, expr: Box<Expr>) -> Result<(), RuntimeError> {
        let val = self.evaluate_expression(expr)?;
        let _ = self.out.write(format!("{}\n", val).as_bytes());
        Ok(())
    }

    fn visit_variable_decl_statement(
        &mut self,
        ident: Token,
        expr: Box<Expr>,
    ) -> Result<(), RuntimeError> {
        let value = self.evaluate_expression(expr)?;
        self.envs.define(ident, value);
        Ok(())
    }

    fn visit_block(&mut self, stmts: Vec<Box<Stmt>>) -> Result<(), RuntimeError> {
        self.envs.add_env();

        for stmt in stmts {
            self.execute_statement(stmt)?;
        }

        self.envs.pop_env();
        Ok(())
    }
}

// Expression evaluation.
impl<W: Write> Interpreter<'_, W> {
    fn evaluate_expression(&mut self, expr: Box<Expr>) -> Result<Value, RuntimeError> {
        match *expr {
            Expr::Unary { op, expr } => self.visit_unary_expr(op, expr),
            Expr::Binary { left, op, right } => self.visit_binary_expr(left, op, right),
            Expr::Group(expr) => self.visit_group_expr(expr),
            Expr::Literal(value) => self.visit_literal_expr(value),
            Expr::Variable(ident) => self.visit_variable_usage_expr(ident),
            Expr::Assign { name, value } => self.visit_variable_assign_expr(name, value),
            Expr::Logical { left, op, right } => self.visit_logical_expr(left, op, right),
        }
    }

    fn visit_unary_expr(&mut self, op: Token, expr: Box<Expr>) -> Result<Value, RuntimeError> {
        let val = self.evaluate_expression(expr)?;

        match op.kind {
            Kind::Minus => match val {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(RuntimeError::InvalidOperandTypes {
                    op,
                    types: vec![val],
                }),
            },

            Kind::Bang => Ok(Value::Boolean(!is_truthy(&val))),
            _ => unreachable!(),
        }
    }

    fn visit_binary_expr(
        &mut self,
        l: Box<Expr>,
        op: Token,
        r: Box<Expr>,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate_expression(l)?;
        let right = self.evaluate_expression(r)?;

        match op.kind {
            // Minus operator only works on numbers.
            Kind::Minus => match (left.clone(), right.clone()) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                _ => Err(number_operands_error(op, left, right)),
            },

            // Star operator only works on numbers.
            Kind::Star => match (left.clone(), right.clone()) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                _ => Err(number_operands_error(op, left, right)),
            },

            // Slash operator only works on numbers. Checks for 0 division.
            Kind::Slash => match (left.clone(), right.clone()) {
                (Value::Number(l), Value::Number(r)) if r != 0.0 => Ok(Value::Number(l / r)),

                (Value::Number(_), Value::Number(r)) if r == 0.0 => {
                    Err(RuntimeError::ZeroDivision { op })
                }

                _ => Err(number_operands_error(op, left, right)),
            },

            // Plus operator works on two numbers or two strings.
            Kind::Plus => match (left.clone(), right.clone()) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
                _ => Err(number_operands_error(op, left, right)),
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

                    _ => Err(number_operands_error(op, left, right)),
                }
            }

            Kind::DoubleEq => Ok(Value::Boolean(left == right)),
            Kind::BangEq => Ok(Value::Boolean(left != right)),

            _ => unreachable!(),
        }
    }

    fn visit_group_expr(&mut self, expr: Box<Expr>) -> Result<Value, RuntimeError> {
        self.evaluate_expression(expr)
    }

    fn visit_literal_expr(&mut self, value: Token) -> Result<Value, RuntimeError> {
        Ok(value.into())
    }

    fn visit_variable_usage_expr(&mut self, ident: Token) -> Result<Value, RuntimeError> {
        self.envs.get(ident)
    }

    fn visit_variable_assign_expr(
        &mut self,
        ident: Token,
        value: Box<Expr>,
    ) -> Result<Value, RuntimeError> {
        let value = self.evaluate_expression(value)?;
        self.envs.assign(ident, value)
    }

    fn visit_logical_expr(
        &mut self,
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate_expression(left)?;

        match op.kind {
            Kind::Or => {
                if is_truthy(&left) {
                    return Ok(left);
                }
            }

            Kind::And => {
                if !is_truthy(&left) {
                    return Ok(left);
                }
            }

            _ => unreachable!(),
        }

        self.evaluate_expression(right)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_input;

    fn interpret_and_capture(input: &str) -> String {
        let mut buf = Vec::new();

        let (stmts, _) = parse_input(input);
        let mut interpreter = Interpreter::new(&mut buf);
        interpreter.interpret(stmts);

        String::from_utf8(buf).unwrap()
    }

    #[test]
    fn test_basic_arithmetic() {
        let input = "print 5 + 5;";
        let output = interpret_and_capture(input);
        assert_eq!("10", output.trim());

        let input = "print 10 - 5 * 2;";
        let output = interpret_and_capture(input);
        assert_eq!("0", output.trim());

        let input = "print 3 * ((10 - 6) * (4 + 4)) / 2;";
        let output = interpret_and_capture(input);
        assert_eq!("48", output.trim());
    }

    #[test]
    fn test_comparison_operators() {
        let input = "print 5 < 10;";
        let output = interpret_and_capture(input);
        assert_eq!("true", output.trim());

        let input = "print 5 <= 10;";
        let output = interpret_and_capture(input);
        assert_eq!("true", output.trim());

        let input = "print 5 > 10;";
        let output = interpret_and_capture(input);
        assert_eq!("false", output.trim());

        let input = "print 5 >= 10;";
        let output = interpret_and_capture(input);
        assert_eq!("false", output.trim());

        let input = "print 5 == 10;";
        let output = interpret_and_capture(input);
        assert_eq!("false", output.trim());

        let input = "print 5 != 10;";
        let output = interpret_and_capture(input);
        assert_eq!("true", output.trim());
    }

    #[test]
    fn test_variable_declaration_and_usage() {
        let input = "
            var x = \"Hello, World!\";
            print x;";

        let output = interpret_and_capture(input);
        assert_eq!("Hello, World!", output.trim());

        let input = "
            var x = 5;
            var y = x * 2;
            var z = y * 2 + 5;
            print x;
            print y;
            print z;";

        let output = interpret_and_capture(input);
        assert_eq!("5\n10\n25", output.trim());

        let input = "
            var x = 5;
            var x = 10;
            print x;";

        let output = interpret_and_capture(input);
        assert_eq!("10", output.trim());
    }

    #[test]
    fn test_variable_assignment() {
        let input = "
            var x = 1;
            x = 2;
            print x;";

        let output = interpret_and_capture(input);
        assert_eq!("2", output.trim());
    }

    #[test]
    fn test_blocks() {
        let input = "
            {
                var x = 1;
                var y = 2;
                print x + y;
            }";

        let output = interpret_and_capture(input);
        assert_eq!("3", output.trim());
    }

    #[test]
    fn test_conditional() {
        let input = "
            if (2 == 2)
                print true;";

        let output = interpret_and_capture(input);
        assert_eq!("true", output.trim());

        let input = "
            if (2 != 2)
                print true;";

        let output = interpret_and_capture(input);
        assert_eq!("", output.trim());

        let input = "
            if (2 != 2)
                print true;
            else 
                print false;";

        let output = interpret_and_capture(input);
        assert_eq!("false", output.trim());

        let input = "
            if (2 == 2) {
                print \"true-1\";
                print \"true-2\";
            } else {
                print \"false-1\";
                print \"false-2\";
            }";

        let output = interpret_and_capture(input);
        assert_eq!("true-1\ntrue-2", output.trim());

        let input = "
            if (2 != 2) {
                print \"true-1\";
                print \"true-2\";
            } else {
                print \"false-1\";
                print \"false-2\";
            }";

        let output = interpret_and_capture(input);
        assert_eq!("false-1\nfalse-2", output.trim());
    }

    #[test]
    fn test_logical_operators() {
        let input = "print \"hi\" or 2;";
        let output = interpret_and_capture(input);
        assert_eq!("hi", output.trim());

        let input = "print nil or \"yes\";";
        let output = interpret_and_capture(input);
        assert_eq!("yes", output.trim());

        let input = "print true and true;";
        let output = interpret_and_capture(input);
        assert_eq!("true", output.trim());

        let input = "print true and false;";
        let output = interpret_and_capture(input);
        assert_eq!("false", output.trim());

        let input = "print false and true;";
        let output = interpret_and_capture(input);
        assert_eq!("false", output.trim());
    }

    #[test]
    fn test_while_statement() {
        let input = "
            var x = 0;
            while (x < 5) {
                x = x + 1;
            }
            
            print x;
        ";

        let output = interpret_and_capture(input);
        assert_eq!("5", output.trim());
    }
}
