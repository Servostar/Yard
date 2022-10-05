use std::{collections::{VecDeque}};

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Operator {
    Or,
    And,
    Xor,

    Eq,
    Lt,
    Gt,
    GtEq,
    LtEq,
    NotEq,

    Add,
    Sub,
    Mul,
    Div,

    Assign
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Assoc {
    Right,
    Left
}

impl Operator {
    pub fn parse<'a>(str: &'a str) -> Self {
        return match str {
            "|" => Operator::Or,
            "&" => Operator::And,
            "^" => Operator::Xor,

            "==" => Operator::Eq,
            "<" => Operator::Lt,
            ">" => Operator::Gt,
            "<=" => Operator::LtEq,
            ">=" => Operator::GtEq,
            "!=" => Operator::NotEq,

            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "=" => Operator::Assign,

            _ => panic!("Unspecified operator")
        };
    }

    pub fn prec(&self) -> usize {
        return match self {
            Operator::Eq => 2,
            Operator::Lt => 2,
            Operator::Gt => 2,
            Operator::LtEq => 2,
            Operator::GtEq => 2,
            Operator::NotEq => 2,

            Operator::Or => 0,
            Operator::Xor => 0,
            Operator::And => 1,

            Operator::Add => 3,
            Operator::Sub => 3,

            Operator::Mul => 4,
            Operator::Div => 4,

            _ => 0
        }
    }

    pub fn assoc(&self) -> Assoc {
        match self {
            _ => Assoc::Right
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Keyword {
    If,
    While
}

impl Keyword {
    pub fn parse<'a>(text: &'a str) -> Keyword {
        return match text {
            "if" => Keyword::If,
            "while" => Keyword::While,
            _ => panic!("Text not a known keyword {text}")
        }
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
    LineBreak,
    Func(&'a str),
    Var(&'a str),
    Arg(&'a str),
    Assign(&'a str),
    Bool(bool),
    Keyword(Keyword)
}

const TOKEN_REGEX_SRC: &'static str = r"(#.*)|(if|while)|(true|false|yes|no|maybe)|([A-Za-z_]+)\s*=|([A-Za-z_]+)|(\d*\.?\d+)|(!=|==|<=|<=|[&|+\-*/<>])|([(){}])|(\n)";
 
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
                    2 => Token::Keyword(Keyword::parse(mat.as_str())),
                    3 => Token::Bool(parse_bool(mat.as_str())),
                    4 => Token::Assign(mat.as_str()),
                    5 => Token::Word(mat.as_str()),
                    6 => Token::Number(mat.as_str()),
                    7 => Token::Operator(Operator::parse(mat.as_str())),
                    8 => Token::Delemiter(mat.as_str().chars().nth(0).unwrap()),
                    9 => Token::LineBreak,

                    _ => panic!("Unknown match to tokenize: {}", mat.as_str())
                });
            }
        }
    }

    return tokens;
}

fn parse_bool(text: &str) -> bool {
    return match text.to_ascii_lowercase().as_str() {
        "true" | "ye" => true,
        "false" |"no" => false,
        "maybe" => rand::random(),
        _ => panic!("Not a recognized boolean {text}")
    }
}