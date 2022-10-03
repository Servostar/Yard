
mod token;
mod parser;

use token::*;
use parser::*;

fn main() {

    let source =
r"
foo(c) = 3 * c

main() {
    x = foo(5 * 6)

    4 * 3
}
";

    parse(&mut tokenize(source));
}
