extern crate lib;

use lib::repl::*;
use std::io::Result;

fn main() -> Result<()> {
    run()?;
    Ok(())
}
