// tokenizer
mod token;
// ro parse a queue of tokens into functions with expressions
mod parser;
// translate a tree of functions and expressions to pseudo assembly
// designed for a virtual stack machiene
mod inter;
mod conf;
mod vmrt;

use colored::Colorize;
use parser::*;
use token::*;

use crate::parser::data::Diagnostics;

pub fn message<S>(typ: MessageType, msg: S)
where
    S: Into<String>,
{
    println!("{}: {}", typ.to_colored(), msg.into().bold().bright_white());
}

fn main() {
    let source = r"
# surely this is pi
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
   yield true 
}

main() = int {
    
    a = 4
    b = pi
    c = true
    r = foo(3, 4.0)
    h = foo(3,5.0)
    b:int = 4

    9
}
";

    let mut diagnostics = Diagnostics::new(source);

    let settings = conf::parse_args(&mut diagnostics);

    if let Ok(mut tokens) = tokenize(source, &mut diagnostics) {
        if let Ok((fs, ds)) = parse(&mut tokens, &mut diagnostics, &settings) {
            vmrt::compile(&fs, &ds);
        }
    }

    println!("{}", diagnostics);
}
