extern crate rlox;

use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

use rlox::scan;

fn main() {
    let args: Vec<String> = env::args().collect();

    let running_mode = parse_args_for_running_mode(&args).unwrap_or_else(|err| {
        println!("Error: invalid arguments {}", err);
        process::exit(1);
    });

    match running_mode {
        RunningMode::Script(file_name) => run_file(file_name),
        RunningMode::Repl => run_repl(),
    }
}

#[derive(Debug)]
enum RunningMode {
    Script(String),
    Repl,
}

fn parse_args_for_running_mode(args: &Vec<String>) -> Result<RunningMode, &'static str> {
    if args.len() > 2 {
        return Err("Usage: rlox [script]")
    }

    if args.len() > 0 {
        let script_file_name = args[1].clone();
        Ok(RunningMode::Script(script_file_name))
    } else {
        Ok(RunningMode::Repl)
    }
}

fn run_file(file_name: String) {
    let mut file = File::open(file_name).unwrap_or_else(|err| {
        println!("Unable to read script file: {}", err);
        process::exit(2);
    });
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap_or_else(|err| {
        println!("Unable to read script file: {}", err);
        process::exit(2);
    });
    println!("Interpreting file:\n{}", source);
    let tokens = scan::scan_tokens(&source)
        .unwrap_or_else(|_| process::exit(1));
    println!("{:#?}", tokens);
}

fn run_repl() {
    println!("Running REPL!");
}