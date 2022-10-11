
// tokenizer
mod token;
// ro parse a queue of tokens into functions with expressions
mod parser;
// translate a tree of functions and expressions to pseudo assembly
// designed for a virtual stack machiene

use token::*;
use parser::*;

fn main() {

    let source =
r"
pi = 3.1415926535

sin(x: f4) = {
    x
}

main() {

    x:i4 = 0
    loop {
        x = x + 1
        if sin(x > 5) {
            break
        }
    }
}
";

    parse(&mut tokenize(source));
}
