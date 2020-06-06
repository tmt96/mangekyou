use crate::codegen::*;
use inkwell::context::Context;
use std::io::{stdin, stdout, Result, Write};

pub fn run() -> Result<()> {
    let context = Context::create();

    let mut generator = IRGenerator::new(&context);
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

        match generator.compile(&input) {
            Ok(ir_values) => println!("IR: {:#?}", ir_values),
            Err(message) => eprintln!("{}", message),
        }
    }
    Ok(())
}
