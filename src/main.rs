
// tokenizer
mod token;
// ro parse a queue of tokens into functions with expressions
mod parser;
// translate a tree of functions and expressions to pseudo assembly
// designed for a virtual stack machiene
mod inter;

use token::*;
use parser::*;
use colored::Colorize;

pub fn message(typ: MessageType, msg: String) {
    println!("{}: {}", typ.to_colored(), msg.bold().bright_white());
}

fn main() {

    let source =
r"
# this is pi
pi = rat 5.1415926535

foo(x:int, y:rat) = bool {
    x:int = 5 + 6

    unless (x < 6) {
        yield true

        please {
            
        }

        unless(x < 6) {
            yield true
        }
    }
    
    -- comment
    yield false
}

main() = int {
    a = 4
    b = 5.5
    c = true
    r = foo(3, 3.4)
    0
}
";

    parse(&mut tokenize(source), source);
}
