use crate::ast::Expr;
use crate::tokens::{Kind, Token};
use crate::value::Value;

type RuntimeError = Result<Value, String>;

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(&mut self, expr: Box<Expr>) -> String {
        match self.evaluate(expr) {
            Ok(val) => format!("{}", val),
            Err(e) => format!("RuntimeError: {}", e),
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
    fn number_operands_error(op: Token, l: Value, r: Value) -> RuntimeError {
        Err(format!(
            "{:?} (at {}..{}) requires {} and {} to be numbers.",
            op.kind, op.range.start, op.range.end, l, r
        ))
    }
}

// Expression evaluation.
impl Interpreter {
    fn evaluate(&mut self, expr: Box<Expr>) -> RuntimeError {
        match *expr {
            Expr::Unary { op, expr } => self.visit_unary_expr(op, expr),
            Expr::Binary { left, op, right } => self.visit_binary_expr(left, op, right),
            Expr::Group(expr) => self.visit_group_expr(expr),
            Expr::Literal(value) => self.visit_literal_expr(value),
        }
    }

    fn visit_unary_expr(&mut self, op: Token, expr: Box<Expr>) -> RuntimeError {
        let val = self.evaluate(expr)?;

        match op.kind {
            Kind::Minus => match val {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err("Unable to negate non-numbers.".to_string()),
            },

            Kind::Bang => Ok(Value::Boolean(!Interpreter::is_truthy(val))),
            _ => unreachable!(),
        }
    }

    fn visit_binary_expr(&mut self, left: Box<Expr>, op: Token, right: Box<Expr>) -> RuntimeError {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

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

    fn visit_group_expr(&mut self, expr: Box<Expr>) -> RuntimeError {
        self.evaluate(expr)
    }

    fn visit_literal_expr(&mut self, value: Token) -> RuntimeError {
        Ok(value.into())
    }
}

// Statement evaluation
impl Interpreter {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{parsing_error::ParsingError, Parser};
    use crate::scanner::Scanner;

    use crate::parse_input;

    #[test]
    fn interpret_basic_arithmetic() {
        let mut interpreter = Interpreter;

        let input = "1 + 1";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("2", result);

        let input = "5 / 2";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("2.5", result);

        let input = "1 + 2 - 5 * 6 / 3";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("-7", result);

        let input = "(9 + 5) * (2 + 4) / 3 / (6 / 3)";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("14", result);
    }

    #[test]
    fn interpret_numeric_comparisons() {
        let mut interpreter = Interpreter;

        let input = "1 == 1";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("true", result);

        let input = "1 != 1";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("false", result);

        let input = "1 < 2";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("true", result);

        let input = "1 <= 2";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("true", result);

        let input = "1 > 2";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("false", result);

        let input = "1 >= 2";
        let (exprs, _) = parse_input(input);
        let result = interpreter.interpret(exprs[0].clone());
        assert_eq!("false", result);
    }
}
