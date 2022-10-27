
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
# this is pi
pi = rat 3.1415926535

foo(x :int, y: rat) = bool {
    yield maybe
}

main {

    unless 3 > 4 {
        a = 4 - 5;
        b:rat = 0.3 + 7

       t = foo(a, b)
    }
}
";

    parse(&mut tokenize(source), source);
}
