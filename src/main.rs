// tokenizer
mod token;
// ro parse a queue of tokens into functions with expressions
mod parser;
// translate a tree of functions and expressions to pseudo assembly
// designed for a virtual stack machiene
mod inter;
mod conf;
mod vmrt;
mod srcio;
mod builtin;
mod direct;

use builtin::BuiltinFun;
use colored::Colorize;
use conf::Settings;
use parser::{*, data::{Declr, Func}};
use token::*;

use crate::parser::data::Diagnostics;

pub fn message<S>(typ: MessageType, msg: S)
where
    S: Into<String>,
{
    println!("{}: {}\n", typ.to_colored(), msg.into().bold().bright_white());
}

fn compile(settings: &Settings) -> Option<(Vec<Func>, Vec<Declr>, Vec<BuiltinFun>)> {

    for src in settings.get_source().iter() {
        let code = src.code();

        let mut diagnostics = Diagnostics::new(&settings, code);
        
        if let Ok(mut tokens) = tokenize(code, &mut diagnostics) {
            let specs = crate::direct::resolve_directives(&mut tokens);

            let mut parser = Parser::new(&specs);

            if let Ok((funcs, declrs, builtin)) = parser.parse(&mut tokens, &mut diagnostics, &settings) {
                if let Ok(prog) = vmrt::compile(&funcs, &declrs, builtin, &settings) {
                    if let Ok(exit_code) = vmrt::execute(&prog) {
                        crate::message(MessageType::Info, format!("Program exited with {}", exit_code));
                    }
                }
            }
        }

        println!("{}", diagnostics);
        continue;
    }

    None
}

fn main() {

    if let Ok(settings) = conf::parse_args() {
        compile(&settings);
    }
}
