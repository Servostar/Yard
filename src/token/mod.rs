use colored::{ColoredString, Colorize};
use std::collections::VecDeque;

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub enum Operator {
    // bitwise boolean operations
    Or,
    And,
    Xor,

    // comparisons
    Eq,
    Lt,
    Gt,
    GtEq,
    LtEq,
    NotEq,

    // arithmetic
    Add,
    Sub,
    Mul,
    Div,

    Assign,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Assoc {
    Right,
    Left,
}

const ARITHMETIC_TYPES: &'static [(&[Prim], Prim)] = &[
    (&[Prim::Int, Prim::Int], Prim::Int),
    (&[Prim::Rat, Prim::Rat], Prim::Rat),
    (&[Prim::Num(NumHint::Int), Prim::Int], Prim::Int),
    (&[Prim::Num(NumHint::Int), Prim::Rat], Prim::Rat),
    (&[Prim::Num(NumHint::Rat), Prim::Rat], Prim::Rat),
    (&[Prim::Int, Prim::Num(NumHint::Int)], Prim::Int),
    (&[Prim::Rat, Prim::Num(NumHint::Rat)], Prim::Rat),
    (&[Prim::Rat, Prim::Num(NumHint::Int)], Prim::Rat),
    (
        &[Prim::Num(NumHint::Rat), Prim::Num(NumHint::Rat)],
        Prim::Rat,
    ),
    (
        &[Prim::Num(NumHint::Int), Prim::Num(NumHint::Rat)],
        Prim::Rat,
    ),
    (
        &[Prim::Num(NumHint::Rat), Prim::Num(NumHint::Int)],
        Prim::Rat,
    ),
    (
        &[Prim::Num(NumHint::Int), Prim::Num(NumHint::Int)],
        Prim::Int,
    ),
];

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

            _ => {
                crate::message(MessageType::Critical, "Unknown operator");
                panic!();
            }
        };
    }

    pub fn prec(&self) -> usize {
        return match self {
            Operator::Eq => 2,
            Operator::Lt => 2,
            Operator::Gt => 2,
            Operator::NotEq => 2,
            Operator::LtEq => 2,
            Operator::GtEq => 2,

            Operator::Or => 0,
            Operator::Xor => 0,
            Operator::And => 1,

            Operator::Add => 3,
            Operator::Sub => 3,
            Operator::Mul => 4,
            Operator::Div => 4,

            Operator::Assign => 0,
        };
    }

    pub fn assoc(&self) -> Assoc {
        match self {
            _ => Assoc::Right,
        }
    }

    fn present_types(
        operands: &[Prim],
        types: &[Prim],
        r#yield: Prim,
        dbginf: &DebugInfo,
        diagnostics: &mut crate::parser::data::Diagnostics,
    ) -> Option<Prim> {
        if operands.len() < types.len() {
            /*
            println!(
                "{}",
                dbginf.make_msg_w_ext(
                    crate::msg::ERR74,
                    format!(
                        "required {} got {}",
                        types.len() - operands.len(),
                        operands.len()
                    )
                )
            );*/
            panic!()
        }

        for (x, typ) in types.iter().enumerate() {
            if typ != &operands[x] {
                return None;
            }
        }
        Some(r#yield)
    }

    fn check_types(
        operands: &[Prim],
        types: &[(&[Prim], Prim)],
        dbginf: &DebugInfo,
        diagnostics: &mut crate::parser::data::Diagnostics,
    ) -> Option<Prim> {
        for combination in types.iter() {
            if let Some(result) =
                Self::present_types(operands, combination.0, combination.1, dbginf, diagnostics)
            {
                return Some(result);
            }
        }
        None
    }

    pub fn operate(
        &self,
        operands: &mut Vec<Prim>,
        dbginf: &DebugInfo,
        diagnostics: &mut crate::parser::data::Diagnostics,
    ) {
        match self {
            Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                let types_valid =
                    Self::check_types(operands, ARITHMETIC_TYPES, dbginf, diagnostics);

                if let Some(result) = types_valid {
                    operands.pop();
                    operands.pop();
                    operands.push(result);
                } else {
                    /*
                    println!(
                        "{}",
                        dbginf.make_msg_w_ext(crate::msg::ERR73, "expected two numbers")
                    );*/
                    panic!()
                }
            }
            Operator::And | Operator::Or | Operator::Xor => {
                let types_valid = Self::check_types(
                    operands,
                    &[(&[Prim::Bool, Prim::Bool], Prim::Bool)],
                    dbginf,
                    diagnostics,
                );

                if let Some(result) = types_valid {
                    operands.pop();
                    operands.pop();
                    operands.push(result);
                } else {
                    /*
                    println!(
                        "{}",
                        dbginf.make_msg_w_ext(crate::msg::ERR73, "expected two booleans")
                    );*/
                    panic!()
                }
            }
            Operator::Eq
            | Operator::NotEq
            | Operator::Lt
            | Operator::Gt
            | Operator::GtEq
            | Operator::LtEq => {
                let types_valid = Self::check_types(
                    operands,
                    &[
                        (&[Prim::Int, Prim::Int], Prim::Bool),
                        (&[Prim::Rat, Prim::Rat], Prim::Bool),
                        (&[Prim::Num(NumHint::Int), Prim::Int], Prim::Bool),
                        (&[Prim::Num(NumHint::Rat), Prim::Rat], Prim::Bool),
                        (&[Prim::Int, Prim::Num(NumHint::Int)], Prim::Bool),
                        (&[Prim::Rat, Prim::Num(NumHint::Rat)], Prim::Bool),
                        (
                            &[Prim::Num(NumHint::Rat), Prim::Num(NumHint::Rat)],
                            Prim::Bool,
                        ),
                        (
                            &[Prim::Num(NumHint::Int), Prim::Num(NumHint::Int)],
                            Prim::Bool,
                        ),
                    ],
                    dbginf,
                    diagnostics,
                );

                if let Some(result) = types_valid {
                    operands.pop();
                    operands.pop();
                    operands.push(result);
                } else {
                    /*
                    println!(
                        "{}",
                        dbginf.make_msg_w_ext(crate::msg::ERR73, "expected two numbers")
                    );*/
                    panic!()
                }
            }
            _ => {
                panic!("Unknown operator");
            }
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
    Return,
    Yield,
    Please,
}

impl Keyword {
    pub fn parse<'a>(text: &'a str) -> Keyword {
        return match text {
            "unless" => Keyword::If,
            "while" => Keyword::While,
            "loop" => Keyword::Loop,
            "break" => Keyword::Break,
            "cont" => Keyword::Continue,
            "ret" => Keyword::Return,
            "yield" => Keyword::Yield,
            "please" => Keyword::Please,
            _ => {
                crate::message(
                    MessageType::Critical,
                    format!("not a known keyword: {}", text),
                );
                panic!()
            }
        };
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum NumHint {
    Int,
    Rat,
}

impl NumHint {
    pub fn from(str: &str) -> NumHint {
        if str.parse::<i32>().is_err() {
            return NumHint::Rat;
        }

        NumHint::Int
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
/// primitve types
pub enum Prim {
    Int,
    Rat,
    Bool,
    Num(NumHint),
}

impl Prim {
    fn from<'a>(text: &'a str, dbginf: &DebugInfo) -> Prim {
        return match text {
            "int" => Prim::Int,
            "rat" => Prim::Rat,
            "bool" => Prim::Bool,
            _ => {
                //println!("{}", dbginf.make_msg(&crate::msg::ERR70));
                panic!()
            }
        };
    }

    pub fn is_equal(&self, value: Prim) -> bool {
        return match self {
            Prim::Bool => *self == value,
            Prim::Rat => {
                return match value {
                    Prim::Num(NumHint::Int) => true,
                    Prim::Num(NumHint::Rat) => true,
                    _ => *self == value,
                }
            }
            Prim::Int => {
                return match value {
                    Prim::Num(NumHint::Int) => true,
                    _ => *self == value,
                }
            }
            Prim::Num(NumHint::Rat) => {
                return match value {
                    Prim::Rat | Prim::Int => true,
                    _ => *self == value,
                }
            }
            Prim::Num(NumHint::Int) => {
                return match value {
                    Prim::Int => true,
                    _ => *self == value,
                }
            }
        };
    }
}

pub struct DebugMsg {
    pub code: i32,
    pub typ: MessageType,
    pub msg: &'static str,
}

pub struct DebugNotice<'a> {
    pub info: DebugInfo,
    /// generic error description
    pub msg: &'static DebugMsg,
    /// extra message which is case specific
    pub ext: String,
    pub source: &'a str,
}

impl<'a> std::fmt::Display for DebugNotice<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write header as:
        // `Error (56) some syntax error message in line 5:`
        f.write_fmt(format_args!(
            "{} ({}) {} in line {}: {}\n",
            self.msg.typ.to_colored(),
            self.msg.code,
            self.msg.msg.bold().bright_white(),
            self.info.line,
            self.ext
        ));
        // write additional information
        f.write_fmt(format_args!(
            " somewhere in here:\n  --> `{}`\n",
            self.source
                .lines()
                .nth(self.info.line)
                .unwrap()
                .trim()
                .bold()
                .bright_white()
        ))
        .unwrap();

        (0..self.info.start + 6)
            .into_iter()
            .for_each(|_| f.write_str(" ").unwrap());
        (self.info.start..self.info.end)
            .into_iter()
            .for_each(|_| f.write_str("^").unwrap());

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct DebugInfo {
    /// index in source string where the token begins in the current line
    pub start: usize,
    /// index in source string where the token ends  in the current line
    pub end: usize,
    /// line number where the line in which the token is begins
    pub line: usize,
}

#[derive(Debug)]
pub enum MessageType {
    /// For internal compiler critical error
    Critical,
    /// compile errors that won't allow for further compilation
    Error,
    Warning,
    Info,
}

impl MessageType {
    /// return a colorized string representation:
    /// - Error (in red)
    /// - Warning (in yellow)
    /// - Info (in blue)
    pub fn to_colored(&self) -> ColoredString {
        let raw = format!("{:#?}", self);
        return match self {
            MessageType::Critical => raw.on_red().bold(),
            MessageType::Error => raw.red().bold(),
            MessageType::Warning => raw.yellow().bold(),
            MessageType::Info => raw.blue().bold(),
        };
    }
}

impl std::fmt::Display for DebugInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// A token represents a basic building block for source code.
/// They give a meaning to patterns of chars allowing to interpret them.
pub enum Token<'a> {
    // base tokens that can simply be split to from raw source code
    Word(&'a str, DebugInfo),
    /// Single symbol delemiter like ```(```,```}```
    Delemiter(char, DebugInfo),
    Operator(Operator, DebugInfo),
    Number(&'a str, NumHint, DebugInfo),
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
    Type(Prim, DebugInfo),
    /// Semicolon
    Terminator(DebugInfo),
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Type(t, _) => f.write_fmt(format_args!("__Type {:?}", t)),
            Token::Word(w, _) => f.write_fmt(format_args!("__Word {:?}", w)),
            Token::Delemiter(d, _) => f.write_fmt(format_args!("__Delemiter {:?}", d)),
            Token::Operator(o, _) => f.write_fmt(format_args!("{:?}", o)),
            Token::Number(n, hint, _) => f.write_fmt(format_args!("Load {:?} {}", hint, n)),
            Token::LineBreak(_) => f.write_str("__Line-break"),
            Token::Func(name, _) => f.write_fmt(format_args!("Call {}", name)),
            Token::Var(v, _) => f.write_fmt(format_args!("Load Var {}", v)),
            Token::Arg(a, _) => f.write_fmt(format_args!("Load Arg {}", a)),
            Token::Assign(a, typ, _) => f.write_fmt(format_args!("Store {:?} {}", typ, a)),
            Token::Decl(d, typ, _) => f.write_fmt(format_args!("__Decl {:?} as {}", typ, d)),
            Token::Bool(b, _) => f.write_fmt(format_args!("Load Bool {}", b)),
            Token::Keyword(k, _) => f.write_fmt(format_args!("{:?}", k)),
            Token::Terminator(_) => f.write_str("__Terminator"),
        }
    }
}

impl<'a> Into<DebugInfo> for Token<'a> {
    fn into(self) -> DebugInfo {
        match self {
            Token::Type(_, d) => d,
            Token::Word(_, d) => d,
            Token::Delemiter(_, d) => d,
            Token::Operator(_, d) => d,
            Token::Number(_, _, d) => d,
            Token::LineBreak(d) => d,
            Token::Func(_, d) => d,
            Token::Var(_, d) => d,
            Token::Arg(_, d) => d,
            Token::Assign(_, _, d) => d,
            Token::Decl(_, _, d) => d,
            Token::Bool(_, d) => d,
            Token::Keyword(_, d) => d,
            Token::Terminator(d) => d,
        }
    }
}

const TOKEN_REGEX_SRC: &'static str = r"(#.*|--.*)|(unless|while|loop|break|cont|ret|yield|please)|(int|rat|bool)|(true|false|ye|no|maybe)|([A-Za-z_]+)\s*(?::\s*([a-zA-Z0-9]+))?\s*=|([A-Za-z_]+)\s*(?::\s*([a-zA-Z0-9]+))|([A-Za-z_]+)|(\d*\.?\d+)|(!=|==|<=|<=|[&|+\-*/<>=])|([(){}])|(\n)|(;)";

lazy_static::lazy_static! {
    static ref TOKEN_REGEX: regex::Regex = regex::Regex::new(TOKEN_REGEX_SRC).unwrap();
}

/// creates a vector of tokens from the specified str.
pub fn tokenize(source: &str) -> VecDeque<Token> {
    let mut tokens = VecDeque::new();

    let mut line_count = 0;
    let mut line_start = 0;

    for cap in TOKEN_REGEX.captures_iter(source.as_ref()) {
        let mut enumerator = cap.iter().enumerate();
        loop {
            let next = enumerator.next();
            if next.is_none() {
                break;
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
                    start: mat.start() - line_start,
                    end: mat.end() - line_start,
                    line: line_count,
                };

                tokens.push_back(match i {
                    2 => Token::Keyword(Keyword::parse(mat.as_str()), debug_info),
                    3 => Token::Type(Prim::from(mat.as_str(), &debug_info), debug_info),
                    4 => Token::Bool(parse_bool(mat.as_str()), debug_info),
                    5 => {
                        let var_type = if let Some(mat) = enumerator.next().unwrap().1 {
                            Some(Prim::from(mat.as_str(), &debug_info))
                        } else {
                            None
                        };
                        Token::Assign(mat.as_str(), var_type, debug_info)
                    }
                    7 => {
                        let var_type =
                            Prim::from(enumerator.next().unwrap().1.unwrap().as_str(), &debug_info);
                        Token::Decl(mat.as_str(), var_type, debug_info)
                    }
                    9 => Token::Word(mat.as_str(), debug_info),
                    10 => Token::Number(mat.as_str(), NumHint::from(mat.as_str()), debug_info),
                    11 => Token::Operator(Operator::parse(mat.as_str()), debug_info),
                    12 => Token::Delemiter(mat.as_str().chars().nth(0).unwrap(), debug_info),
                    13 => {
                        line_count += 1;
                        line_start = mat.start();
                        Token::LineBreak(debug_info)
                    }
                    14 => Token::Terminator(debug_info),

                    _ => {
                        //println!("{}", debug_info.make_msg(crate::msg::ERR71));
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
        "false" | "no" => false,
        "maybe" => rand::random(),
        _ => panic!("Not a recognized boolean {text}"),
    };
}
