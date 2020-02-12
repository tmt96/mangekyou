extern crate lib;

use lib::lexer::Lexer;

fn main() {
    let lexer = Lexer::new("Hello World!");
    println!("{}", lexer.count());
}
