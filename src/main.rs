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
    println!("{}: {}\n", typ.to_colored(), msg.into().bold().bright_white());
}

fn main() {
    let source = r"
-- structs are tables

main() = int {

    unless 2 == 4 {
        yield 1;
    }
    
    yield 0;
}
";

    let mut diagnostics = Diagnostics::new(source);

    let settings = conf::parse_args(&mut diagnostics);

    if let Ok(mut tokens) = tokenize(source, &mut diagnostics) {
        if let Ok((fs, ds)) = parse(&mut tokens, &mut diagnostics, &settings) {
            if let Ok(prog) = vmrt::compile(&fs, &ds, &settings) {
                if let Ok(exit_code) = vmrt::execute(&prog) {
                    crate::message(MessageType::Info, format!("Program exited with {}", exit_code));
                }
            }
        }
    }

    println!("{}", diagnostics);
}
