
mod token;
mod parser;

use token::*;
use parser::*;

fn main() {

    let source =
r"
pi = 3.1415926

main() {
    if 4 > 2 {
        val = 9 / 5
    }
}
";

    parse(&mut tokenize(source));
}
