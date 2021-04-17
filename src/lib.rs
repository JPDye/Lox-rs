pub mod ast;
pub mod parser;
pub mod scanner;
pub mod tokens;

pub mod interpreter;
pub mod value;

use ast::Stmt;
use parser::{parsing_error::ParsingError, Parser};
use scanner::Scanner;

/// Utility function for testing.
pub fn parse_input(input: &str) -> (Vec<Box<Stmt>>, Vec<ParsingError>) {
    let scanner = Scanner::new(input);
    let mut parser = Parser::new(scanner);
    parser.parse()
}
