mod frontend;

use std::fs;
use std::env;

use frontend::*;

fn main() {
    // collect args
    let args: Vec<String> = env::args().collect();
    // program name, filename
    if args.len() != 2 {
        eprintln!("Error: Expected exactly one argument (file path)");
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }
    // get filename
    let filename = &args[1];
    let src = fs::read_to_string(filename).unwrap();
    let program = match parse(&src) {
        Some(program) => program,
        None => {
            eprintln!("Error: Failed to parse file");
            std::process::exit(1);
        }
    };
    // TODO: add passes and codegen here
    println!("{:?}", program);
}