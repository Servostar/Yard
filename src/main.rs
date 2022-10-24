
// tokenizer
mod token;
// ro parse a queue of tokens into functions with expressions
mod parser;
// translate a tree of functions and expressions to pseudo assembly
// designed for a virtual stack machiene

use token::*;
use parser::*;
use colored::{Colorize};

pub fn message(typ: MessageType, msg: String) {
    println!("{}: {}", typ.to_colored(), msg.bold().bright_white());
}

fn main() {

    let source =
r"
pi = 3.1415926535

foo(x:i4, y:f4) {
    
}

main() {
    a:i4 = 8
    b:f4 = 9
    
    foo(a, 6)
}
";

    parse(&mut tokenize(source), source);
}
