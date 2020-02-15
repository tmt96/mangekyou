use crate::parser::*;
use std::io::{stdin, stdout, Result, Write};

pub fn run() -> Result<()> {
    loop {
        let mut input = String::new();
        print!("> ",);
        stdout().flush()?;
        match stdin().read_line(&mut input) {
            Ok(_) => (),
            Err(_) => break,
        }
        if input.trim() == "" {
            continue;
        }
        if input == "exit\n" {
            break;
        }

        let mut parser = Parser::from_source(&input);
        if let Some(node) = parser.parse() {
            println!("AST Node {:?}", node);
        }
    }
    Ok(())
}
