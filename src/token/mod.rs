use std::collections::{VecDeque};

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Operator {
    Assign,

    Add,
    Sub,
    Mul,
    Div
}

impl Operator {
    pub fn parse<'a>(str: &'a str) -> Self {
        return match str {
            "=" => Operator::Assign,

            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,

            _ => panic!("Unspecified operator")
        };
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
/// A token represents a basic building block for source code.
/// They give a meaning to patterns of chars allowing to interpret them.
pub enum Token<'a> {
    // base tokens that can simply be split to from raw source code
    Word(&'a str),
    Delemiter(char),
    Operator(Operator),
    Number(&'a str),
    LineBreak
}

const TOKEN_REGEX_SRC: &'static str = r"(#.*)|([A-Za-z_]+)|(\d*\.?\d+)|([+\-*=])|([(){}])|(\n)";
 
lazy_static::lazy_static! {
    static ref TOKEN_REGEX: regex::Regex = regex::Regex::new(TOKEN_REGEX_SRC).unwrap();
}

/// creates a vector of tokens from the specified str.
pub fn tokenize<'a>(source: &'a str) -> VecDeque<Token<'a>> {
    let mut tokens = VecDeque::new();

    for cap in TOKEN_REGEX.captures_iter(source) {
        for (i, group) in cap.iter().enumerate() {

            // ignore first group as its the entire match,
            // as well as the 1st group (= comments)
            if i <= 1 {
                continue;
            }

            // if we have a match, save it as token
            if let Some(mat) = group  {
                tokens.push_back(match i  {
                    2 => Token::Word(mat.as_str()),
                    3 => Token::Number(mat.as_str()),
                    4 => Token::Operator(Operator::parse(mat.as_str())),
                    5 => Token::Delemiter(mat.as_str().chars().nth(0).unwrap()),
                    6 => Token::LineBreak,

                    _ => panic!("Unknown match to tokenize: {}", mat.as_str())
                });
            }
        }
    }

    return tokens;
}