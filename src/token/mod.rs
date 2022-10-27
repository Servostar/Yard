use std::{collections::{VecDeque}};
use colored::{Colorize, ColoredString};

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
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

    fn present_types(operands: &[Prim], types: &[Prim], r#yield: Prim, dbginf: &DebugInfo, source: &str) -> Option<Prim> {
        if operands.len() < types.len() {
            dbginf.print(MessageType::Error, format!("Missing {} operands", types.len() - operands.len()).as_str(), source);
            panic!()
        }

        for (x, typ) in types.iter().enumerate() {
            if typ != &operands[x] {
                return None
            }
        }
        Some(r#yield)
    }

    fn check_types(operands: &[Prim], types: &[(Vec<Prim>, Prim)], dbginf: &DebugInfo, source: &str) -> Option<Prim> {
        for combination in types.iter() {

            if let Some(result) = Self::present_types(operands, &combination.0, combination.1, dbginf, source) {
                return Some(result);
            }
        }
        None
    }

    pub fn operate(&self, operands: &mut Vec<Prim>, dbginf: &DebugInfo, source: &str) {
        match self {
            Operator::Add | Operator::Sub | Operator::Mul | Operator::Div=> {
                let types_valid = Self::check_types(operands, &[
                    //   +-------------------------------------------------------------------------------+---------------------------------+
                    //   |  Parameter list of types                                                      | result type                     |
                    //   +-------------------------------------------------------------------------------+---------------------------------+
                    (vec![Prim::Int,                               Prim::Int                             ], Prim::Int),
                    (vec![Prim::Rat,                               Prim::Rat                             ], Prim::Rat),
                    (vec![Prim::UntypedNum(NumberClassHint::Int),  Prim::Int                             ], Prim::Int),
                    (vec![Prim::UntypedNum(NumberClassHint::Int),  Prim::Rat                             ], Prim::Rat),
                    (vec![Prim::UntypedNum(NumberClassHint::Rat),  Prim::Rat                             ], Prim::Rat),
                    (vec![Prim::Int,                               Prim::UntypedNum(NumberClassHint::Int)], Prim::Int),
                    (vec![Prim::Rat,                               Prim::UntypedNum(NumberClassHint::Rat)], Prim::Rat),
                    (vec![Prim::Rat,                               Prim::UntypedNum(NumberClassHint::Int)], Prim::Rat),
                    (vec![Prim::UntypedNum(NumberClassHint::Rat),  Prim::UntypedNum(NumberClassHint::Rat)], Prim::Rat),
                    (vec![Prim::UntypedNum(NumberClassHint::Int),  Prim::UntypedNum(NumberClassHint::Rat)], Prim::Rat),
                    (vec![Prim::UntypedNum(NumberClassHint::Rat),  Prim::UntypedNum(NumberClassHint::Int)], Prim::Rat),
                    (vec![Prim::UntypedNum(NumberClassHint::Int),  Prim::UntypedNum(NumberClassHint::Int)], Prim::Int)
                ], dbginf, source);

                if let Some(result) = types_valid {
                    operands.pop();
                    operands.pop();
                    operands.push(result);
                } else {
                    dbginf.print(MessageType::Error, format!("Missmatched types for {:?}, expected either two integer or rationals", self).as_str(), source);
                    panic!()
                }
            },
            Operator::And | Operator::Or | Operator::Xor => {
                let types_valid = Self::check_types(operands, &[
                    (vec![Prim::Bool, Prim::Bool ], Prim::Bool),
                ], dbginf, source);

                if let Some(result) = types_valid {
                    operands.pop();
                    operands.pop();
                    operands.push(result);
                } else {
                    dbginf.print(MessageType::Error, format!("Missmatched types for {:?}, expected two booleans", self).as_str(), source);
                    panic!()
                }
            },
            Operator::Eq | Operator::NotEq | Operator::Lt | Operator::Gt | Operator::GtEq | Operator::LtEq   => {
                let types_valid = Self::check_types(operands, &[
                   //   +-------------------------------------------------------------------------------+---------------------------------+
                   //   |  Parameter list of types                                                      | result type                     |
                   //   +-------------------------------------------------------------------------------+---------------------------------+
                    (vec![Prim::Int,                              Prim::Int                             ], Prim::Bool ),
                    (vec![Prim::Rat,                              Prim::Rat                             ], Prim::Bool ),
                    (vec![Prim::UntypedNum(NumberClassHint::Int), Prim::Int                             ], Prim::Bool ),
                    (vec![Prim::UntypedNum(NumberClassHint::Rat), Prim::Rat                             ], Prim::Bool ),
                    (vec![Prim::Int,                              Prim::UntypedNum(NumberClassHint::Int)], Prim::Bool ),
                    (vec![Prim::Rat,                              Prim::UntypedNum(NumberClassHint::Rat)], Prim::Bool ),
                    (vec![Prim::UntypedNum(NumberClassHint::Rat), Prim::UntypedNum(NumberClassHint::Rat)], Prim::Bool ),
                    (vec![Prim::UntypedNum(NumberClassHint::Int), Prim::UntypedNum(NumberClassHint::Int)], Prim::Bool )
                ], dbginf, source);

                if let Some(result) = types_valid {

                    operands.pop();
                    operands.pop();
                    operands.push(result);
                } else {
                    dbginf.print(MessageType::Error, format!("Missmatched types for {:?}, expected two numbers", self).as_str(), source);
                    panic!()
                }
            },
            _ => panic!("Unknown operator: {:?}", self)
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Keyword {
    If,
    While,
    /// while(true) loop
    Loop,
    Break,
    Continue,
}

impl Keyword {
    pub fn parse<'a>(text: &'a str) -> Keyword {
        return match text {
            "unless" => Keyword::If,
            "while" => Keyword::While,
            "loop" => Keyword::Loop,
            "break" => Keyword::Break,
            "continue" => Keyword::Continue,
            _ => panic!("Text not a known keyword {text}")
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum NumberClassHint {
    Int,
    Rat
}

impl NumberClassHint {
    
    pub fn from(str: &str) -> NumberClassHint {
        if str.parse::<i32>().is_err() {
            return NumberClassHint::Rat;
        }

        NumberClassHint::Int
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
/// primitve types
pub enum Prim {
    Int,
    Rat,
    Bool,
    UntypedNum(NumberClassHint)
}

impl Prim {
    fn from<'a>(text: &'a str, dbginf: &DebugInfo, source: &str) -> Prim {
        return match text {
            "int" => Prim::Int,
            "rat" => Prim::Rat,
            "bool" => Prim::Bool,

            _ => {
                dbginf.print(MessageType::Error, "Unknown type declaration", source);
                panic!()
            }
        }
    }

    pub fn is_equal(&self, value: Prim) -> bool {
        return match self {
            Prim::Bool => *self == value,
            Prim::Rat => return match value {
                Prim::UntypedNum(NumberClassHint::Int) => true,
                Prim::UntypedNum(NumberClassHint::Rat) => true,
                _ => *self == value,
            },
            Prim::Int => return match value {
                Prim::UntypedNum(NumberClassHint::Int) => true,
                _ => *self == value,
            },
            Prim::UntypedNum(NumberClassHint::Rat) => return match value {
                Prim::Rat | Prim::Int => true,
                _ => *self == value,
            }, 
            Prim::UntypedNum(NumberClassHint::Int) => return match value {
                Prim::Int => true,
                _ => *self == value,
            }, 
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct DebugInfo {
    /// index in source string where the token begins
    start: usize,
    /// index in source string where the token ends
    end: usize,
    /// line number where the line in which the token is begins
    line: usize
}

#[derive(Debug)]
pub enum MessageType {
    Error,
    Warning,
    Info
}

impl MessageType {
    /// return a colorized string representation:
    /// - Error (in red)
    /// - Warning (in yellow)
    /// - Info (in blue) 
    pub fn to_colored(&self) -> ColoredString {
        let raw = format!("{:#?}", self);
        return match self {
            MessageType::Error => raw.red().bold(),
            MessageType::Warning => raw.yellow().bold(),
            MessageType::Info => raw.blue().bold()
        };
    }
}

impl DebugInfo {
    /// print message in the form of:
    /// ```text
    /// Error (message) in line 7: token `code`
    ///  somewhere in here:
    ///   --> `code line`
    /// ```
    pub fn print<'a>(&self, typ: MessageType, msg: &str, source: &'a str) {
        println!("{} ({}) in line {}: token `{}`", typ.to_colored(), msg.bold().bright_white(), self.line, &source[self.start..self.end].bold());
        println!(" somewhere in here:\n  --> `{}`\n", source.lines().nth(self.line).unwrap().trim().bold().bright_white())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
/// A token represents a basic building block for source code.
/// They give a meaning to patterns of chars allowing to interpret them.
pub enum Token<'a> {
    // base tokens that can simply be split to from raw source code
    Word(&'a str, DebugInfo),
    /// Single symbol delemiter like ```(```,```}```
    Delemiter(char, DebugInfo),
    Operator(Operator, DebugInfo),
    Number(&'a str, NumberClassHint, DebugInfo),
    LineBreak(DebugInfo),
    Func(&'a str, DebugInfo),
    /// Variable
    Var(&'a str, DebugInfo),
    /// Function argument
    Arg(&'a str, DebugInfo),
    /// Variable assignment in the form of ```name = ```
    Assign(&'a str, Option<Prim>, DebugInfo),
    /// Variable type declaration in the form of ```name:type```
    Decl(&'a str, Prim, DebugInfo),
    Bool(bool, DebugInfo),
    /// Keywords like ```if```,```break```,```while```
    Keyword(Keyword, DebugInfo),
    Type(Prim, DebugInfo)
}

impl<'a> Token<'a> {
    /// redirect for ```DebugInfo.print()```
    pub fn print(&self, error: MessageType, arg: &str, source: &str) {
        match self {
            Token::Type(_, dbginf) => dbginf.print(error, arg, source),
            Token::Word(_, dbginf) => dbginf.print(error, arg, source),
            Token::Delemiter(_, dbginf) => dbginf.print(error, arg, source),
            Token::Operator(_, dbginf) => dbginf.print(error, arg, source),
            Token::Number(_, _, dbginf) => dbginf.print(error, arg, source),
            Token::LineBreak(dbginf) => dbginf.print(error, arg, source),
            Token::Func(_, dbginf) => dbginf.print(error, arg, source),
            Token::Var(_, dbginf) => dbginf.print(error, arg, source),
            Token::Arg(_, dbginf) => dbginf.print(error, arg, source),
            Token::Assign(_, _, dbginf) => dbginf.print(error, arg, source),
            Token::Decl(_, _, dbginf) => dbginf.print(error, arg, source),
            Token::Bool(_, dbginf) => dbginf.print(error, arg, source),
            Token::Keyword(_, dbginf) => dbginf.print(error, arg, source),
        }
    }
}

const TOKEN_REGEX_SRC: &'static str = r"(#.*)|(unless|while|loop|break|continue)|(int|rat|bool)|(true|false|ye|no|maybe)|([A-Za-z_]+)\s*(?::\s*([a-zA-Z0-9]+))?\s*=|([A-Za-z_]+)\s*(?::\s*([a-zA-Z0-9]+))|([A-Za-z_]+)|(\d*\.?\d+)|(!=|==|<=|<=|[&|+\-*/<>=])|([(){}])|(\n|;)";

lazy_static::lazy_static! {
    static ref TOKEN_REGEX: regex::Regex = regex::Regex::new(TOKEN_REGEX_SRC).unwrap();
}

/// creates a vector of tokens from the specified str.
pub fn tokenize<'a>(source: &'a str) -> VecDeque<Token<'a>> {
    let mut tokens = VecDeque::new();

    let mut line_count = 0;

    for cap in TOKEN_REGEX.captures_iter(source) {
        let mut enumerator = cap.iter().enumerate();
        loop {
            let next = enumerator.next();
            if next.is_none() {
                break
            }

            let (i, group) = next.unwrap();

            // ignore first group as its the entire match,
            // as well as the 1st group (= comments)
            if i <= 1 {
                continue;
            }

            // if we have a match, save it as token
            if let Some(mat) = group {
                let debug_info = DebugInfo {
                    start: mat.start(),
                    end: mat.end(),
                    line: line_count
                };

                tokens.push_back(match i {
                    2 => Token::Keyword(Keyword::parse(mat.as_str()), debug_info),
                    3 => {
                        Token::Type(Prim::from(mat.as_str(), &debug_info, source), debug_info)
                    },
                    4 => Token::Bool(parse_bool(mat.as_str()), debug_info),
                    5 => {
                        let var_type = if let Some(mat) = enumerator.next().unwrap().1 {
                            Some(Prim::from(mat.as_str(), &debug_info, source))
                        } else {
                            None
                        };
                        Token::Assign(mat.as_str(), var_type, debug_info)
                    },
                    7 => {
                        let var_type = Prim::from(enumerator.next().unwrap().1.unwrap().as_str(), &debug_info, source);
                        Token::Decl(mat.as_str(), var_type, debug_info)
                    },
                    9 => Token::Word(mat.as_str(), debug_info),
                    10 => Token::Number(mat.as_str(), NumberClassHint::from(mat.as_str()), debug_info),
                    11 => Token::Operator(Operator::parse(mat.as_str()), debug_info),
                    12 => Token::Delemiter(mat.as_str().chars().nth(0).unwrap(), debug_info),
                    13 => {
                        line_count += 1;
                        Token::LineBreak(debug_info)
                    },

                    _ => {
                        debug_info.print(MessageType::Error, "Unable to identify sequence as token", source);
                        panic!()
                    }
                });
                break;
            }
        }
    }

    return tokens;
}

fn parse_bool(text: &str) -> bool {
    return match text {
        "true" | "ye" => true,
        "false" |"no" => false,
        "maybe" => rand::random(),
        _ => panic!("Not a recognized boolean {text}")
    }
}
