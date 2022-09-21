
#[derive(Debug)]
/// A token represents a basic building block for source code.
/// They give a meaning to patterns of chars allowing to interpret them.
pub enum Token<'a> {
    Word(&'a str),
    Delemiter(char),
    Operator,
    Number(&'a str),
}

const TOKEN_REGEX_SRC: &'static str = r"(#.*)|([A-Za-z_]+)|(\d*\.?\d+)|([+\-*])|([(){}])";
 
lazy_static::lazy_static! {
    static ref TOKEN_REGEX: regex::Regex = regex::Regex::new(TOKEN_REGEX_SRC).unwrap();
}

/// creates a vector of tokens from the specified str.
pub fn tokenize<'a>(source: &'a str) -> Vec<Token<'a>> {
    let mut tokens = vec![];

    for cap in TOKEN_REGEX.captures_iter(source) {
        for (i, group) in cap.iter().enumerate() {

            // ignore first group as its the entire match,
            // as well as the 1st group (= comments)
            if i <= 1 {
                continue;
            }

            // if we have a match, save it as token
            if let Some(mat) = group  {
                tokens.push(match i  {
                    2 => Token::Word(mat.as_str()),
                    3 => Token::Number(mat.as_str()),
                    4 => Token::Operator,
                    5 => Token::Delemiter(mat.as_str().chars().nth(0).unwrap()),

                    _ => panic!("Unknown match to tokenize: {}", mat.as_str())
                });
            }
        }
    }

    return tokens;
}