
mod token;
mod parser;

use token::*;
use parser::*;

fn main() {

    let source =
r"
foo = 5 * 6 + 4

foo() = {
    c
}

main()(x) {
    3 * 5 # comment
}
";

    parse(&mut tokenize(source));
}
