extern crate lib;

use lib::lexer::Lexer;

fn main() {
    let stream = "Hello World".chars().peekable();
    let lexer = Lexer { stream };
    println!("{}", lexer.count());
}
