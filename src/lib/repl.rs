use crate::parser::*;
use std::io::{stdin, stdout, Result, Write};

pub fn run() -> Result<()> {
    loop {
        let mut input = String::new();
        print!("mangekyou>> ",);
        stdout().flush()?;
        if let Err(err) = stdin().read_line(&mut input) {
            eprintln!("Error: {}", err);
            continue;
        }
        if input.trim() == "" {
            continue;
        }
        if input == "exit\n" {
            break;
        }

        let mut parser = Parser::from_source(&input);
        match parser.parse_loop() {
            Ok(tree) => println!("AST: {:?}", tree),
            Err(message) => eprintln!("{}", message),
        }
    }
    Ok(())
}
