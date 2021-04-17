use rlox::interpreter::Interpreter;
use rlox::parser::Parser;
use rlox::scanner::Scanner;

use std::io::{self, Write};

pub fn main() {
    run();
}

fn run() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut interpreter = Interpreter;

    loop {
        print!("> ");
        let _ = stdout.flush();

        let mut source = String::new();
        stdin.read_line(&mut source).unwrap();

        let scanner = Scanner::new(&source);
        let mut parser = Parser::new(scanner);
        let (exprs, errors) = parser.parse();

        if !errors.is_empty() {
            for error in errors {
                println!("{}", error);
            }
        } else {
            for expr in exprs {
                interpreter.interpret(expr);
            }
        }
    }
}
