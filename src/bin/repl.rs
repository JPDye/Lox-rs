use rlox::scanner::Scanner;
use rlox::tokens::Token;

use std::io::{self, Write};

pub fn main() {
    run();
}

fn run() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("> ");
        stdout.flush();

        let mut source = String::new();
        stdin.read_line(&mut source).unwrap();

        let scanner = Scanner::new(&source);
        for token in scanner {
            println!("{:?}", token);
        }
    }
}
