
mod token;
mod parser;

use token::*;
use parser::*;

fn main() {

    let source =
r"
pi = 3.1415926

main() {
    
}
";

    parse(&mut tokenize(source));
}
