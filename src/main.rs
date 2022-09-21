
mod token;
mod parser;

use token::*;

fn main() {
    
    let source =
r"
main() {
    3 * 5 # comment
}
";

    tokenize(source).iter().for_each(|t| print!("{:?}", t));
}
