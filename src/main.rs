use rlox::interpreter::Interpreter;
use rlox::parse_input;

use std::env;
use std::fs::File;
use std::io::{self, Read, Write};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Run REPL if no file is provided.
    if args.len() == 1 {
        repl();
        process::exit(0);
    }

    // Execute file. Assumes command line argument is a file path.
    let mut file = File::open(&args[1]).expect("unable to open file");
    let mut input = String::new();
    let _ = file.read_to_string(&mut input);

    let (stmts, errors) = parse_input(&input);

    // If there were any parsing errors, print them and exit.
    if !errors.is_empty() {
        for error in errors {
            println!("{}", error);
        }
        process::exit(1);
    }

    // Execute code if it parsed correctly.
    let mut interpreter = Interpreter;
    interpreter.interpret(stmts);
}

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut input = String::new();
    let mut interpreter = Interpreter;

    loop {
        print!("> ");
        let _ = stdout.flush();

        stdin.read_line(&mut input).unwrap();

        // Provide alternative to CTRL+C for exiting REPL.
        if input.trim() == "exit()" {
            process::exit(0);
        }

        // Parse input. Print errors if they occur. Execute code if there are none.
        let (stmts, errors) = parse_input(&input);

        if !errors.is_empty() {
            for error in errors {
                println!("{}", error);
            }
        } else {
            interpreter.interpret(stmts)
        }

        input.clear();
    }
}
