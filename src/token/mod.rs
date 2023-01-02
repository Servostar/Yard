use colored::{ColoredString, Colorize};
use std::{collections::VecDeque};

use crate::parser::data::Diagnostics;

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

    // concatonate primitve data types to string
    Cat
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
            ".." => Operator::Cat,

            _ => {
                crate::message(MessageType::Critical, format!("Unknown operator: {}", str));
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

            Operator::Cat => 0
        };
    }

    pub fn assoc(&self) -> Assoc {
        match self {
            _ => Assoc::Right,
        }
    }

    fn operation_type(operands: &[Prim]) -> Option<Prim> {
        let mut typ = None;
        for op in operands.iter() {
            op.merge_types(&mut typ);
        }

        typ
    }

    fn present_types(
        &self,
        operands: &[Prim],
        types: &[Prim],
        r#yield: Prim,
        info: &DebugInfo,
        diagnostics: &mut crate::parser::data::Diagnostics,
    ) -> Result<(Option<Prim>, Option<Prim>), ()> {

        if operands.len() < types.len() {
            diagnostics.set_err(info, crate::msg::ERR74, format!(
                        "required {} got {}",
                        types.len() - operands.len(),
                        operands.len()
                    )
                );
            return Err(());
        }

        for (x, typ) in types.iter().enumerate() {
            if *typ != operands[x] {
                return Err(());
            }
        }

        // this combination fits
        Ok((Some(r#yield), Self::operation_type(types) ))
    }

    fn check_types(
        &self,
        operands: &[Prim],
        types: &[(&[Prim], Prim)],
        dbginf: &DebugInfo,
        diagnostics: &mut crate::parser::data::Diagnostics,
    ) -> Result<(Option<Prim>, Option<Prim>),()> {
        for combination in types.iter() {
            if let Ok((result, hint)) =
                self.present_types(operands, combination.0, combination.1, dbginf, diagnostics) {
                    return Ok((result, hint));
                }
            
        }
        Ok((None, None))
    }

    pub fn operate(
        &self,
        operands: &mut Vec<Prim>,
        info: &DebugInfo,
        diagnostics: &mut crate::parser::data::Diagnostics,
    ) -> Result<Prim,()> {
        // TODO: insert type hint
        match self {
            Operator::Cat => {
                operands.pop();
                operands.pop();
                operands.push(Prim::Str);
                return Ok(Prim::Str);
            },
            Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {

                let (types_valid, _) =
                    self.check_types(operands, ARITHMETIC_TYPES, info, diagnostics)?;

                if let Some(result) = types_valid {
                    operands.pop();
                    operands.pop();
                    operands.push(result);
                    return Ok(types_valid.unwrap());
                } else {
                    diagnostics.set_err(info, crate::msg::ERR73, "expected two numbers");
                    return Err(());
                }
            }
            Operator::And | Operator::Or | Operator::Xor => {
                let types_valid = self.check_types(
                    operands,
                    &[(&[Prim::Bool, Prim::Bool], Prim::Bool)],
                    info,
                    diagnostics,
                )?;

                if let (Some(result), Some(hint)) = types_valid {
                    operands.pop();
                    operands.pop();
                    operands.push(result);
                    return Ok(hint)
                } else {
                    diagnostics.set_err(info, crate::msg::ERR73, "expected two booleans");
                    return Err(());
                }
            }
            Operator::Eq
            | Operator::NotEq
            | Operator::Lt
            | Operator::Gt
            | Operator::GtEq
            | Operator::LtEq => {
                let types_valid = self.check_types(
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
                    info,
                    diagnostics,
                )?;

                if let (Some(result), Some(hint)) = types_valid {
                    operands.pop();
                    operands.pop();
                    operands.push(result);
                    return Ok(hint);
                } else {
                    diagnostics.set_err(info, crate::msg::ERR73, "expected either two booleans or two numbers");
                    return Err(());
                }
            }
            _ => {
                diagnostics.set_err(info, crate::msg::ERR75, format!("token {:?}", self));
                return Err(());
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Keyword<'a> {
    Unless,
    While,
    /// while(true) loop
    Loop,
    Break,
    Continue,
    Return,
    Yield,
    Please,
    Goto(&'a str),
}

const GOTO_REGEX_SRC: &'static str = r"goto\s+([a-zA-Z0-9]+)";

lazy_static::lazy_static! {
    static ref GOTO_REGEX: regex::Regex = regex::Regex::new(GOTO_REGEX_SRC).unwrap();
}


impl<'a> Keyword<'a> {
    pub fn parse(text: &'a str) -> Keyword<'a> {
        return match text {
            "despite" => Keyword::Unless,
            "until" => Keyword::While,
            "loop" => Keyword::Loop,
            "break" => Keyword::Break,
            "cont" => Keyword::Continue,
            "ret" => Keyword::Return,
            "yield" => Keyword::Yield,
            "please" => Keyword::Please,
            _ => {
               
                for cap in GOTO_REGEX.captures_iter(text) {
                    return Keyword::Goto(cap.get(1).unwrap().as_str());
                }
        
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
    Str,
}

impl Prim {
    fn from<'a>(text: &'a str, info: &DebugInfo, diagnostics: &mut Diagnostics) -> Result<Prim, ()> {
        return match text {
            "int" => Ok(Prim::Int),
            "rat" => Ok(Prim::Rat),
            "bool" => Ok(Prim::Bool),
            "str" => Ok(Prim::Str),
            _ => {
                diagnostics.set_err(info, crate::msg::ERR70, format!("token is not a type: {}", text));
                return Err(())
            }
        };
    }

    pub fn merge_types(&self, current: &mut Option<Prim>) {
        if let Some(prim) = current {
            if !prim.is_equal(*self) {
                *current = None;
            }
        } else {
            *current = Some(*self);
        }
    }

    pub fn is_equal(&self, value: Prim) -> bool {
        return match self {
            Prim::Str => *self == value,
            Prim::Bool => *self == value,
            Prim::Rat => {
                return match value {
                    Prim::Num(NumHint::Int) => true,
                    Prim::Num(NumHint::Rat) => true,
                    Prim::Rat => true,
                    _ => false,
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

impl std::fmt::Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prim::Int => f.write_str("Int")?,
            Prim::Rat => f.write_str("Rat")?,
            Prim::Bool => f.write_str("Bool")?,
            Prim::Num(_) => f.write_fmt(format_args!("{:?}", self))?,
            Prim::Str => f.write_str("Str")?,
        }
        Ok(())
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
            self.info.line + 1,
            self.ext
        ))?;
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

        (0..self.info.start + 7)
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
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    Operator(Operator, Option<Prim>, DebugInfo),
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
    Keyword(Keyword<'a>, DebugInfo),
    Type(Prim, DebugInfo),
    /// Semicolon
    Terminator(DebugInfo),
    Label(&'a str, DebugInfo),
    CompilerDirective(&'a str, DebugInfo),
    String(&'a str, DebugInfo),
    LoopStart,
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Label(name, _) => f.write_fmt(format_args!(" {}:", name)),
            Token::Type(t, _) => f.write_fmt(format_args!("__Type {:?}", t)),
            Token::Word(w, _) => f.write_fmt(format_args!("__Word {:?}", w)),
            Token::Delemiter(d, _) => f.write_fmt(format_args!("__Delemiter {:?}", d)),
            Token::Operator(o, hint, _) => f.write_fmt(format_args!("{:?} {:?}", o, hint.unwrap())),
            Token::Number(n, hint, _) => f.write_fmt(format_args!("Load {:?} {}", hint, n)),
            Token::LineBreak(_) => f.write_str("__Line-break"),
            Token::Func(name, _) => f.write_fmt(format_args!("Call {}", name)),
            Token::Var(v, _) => f.write_fmt(format_args!("Load Var {}", v)),
            Token::Arg(a, _) => f.write_fmt(format_args!("Load Arg {}", a)),
            Token::Assign(a, typ, _) => f.write_fmt(format_args!("Store {} {}", typ.unwrap(), a)),
            Token::Decl(d, typ, _) => f.write_fmt(format_args!("__Decl {:?} as {}", typ, d)),
            Token::Bool(b, _) => f.write_fmt(format_args!("Load Bool {}", b)),
            Token::Keyword(k, _) => f.write_fmt(format_args!("{:?}", k)),
            Token::Terminator(_) => f.write_str("__Terminator"),
            Token::CompilerDirective(_, _) => f.write_str("compiler-directive"),
            Token::String(mat, _) => f.write_fmt(format_args!("Load String \"{}\"", mat)),
            Token::LoopStart => f.write_str("Loopstart"),
        }
    }
}

impl<'a> Into<DebugInfo> for Token<'a> {
    fn into(self) -> DebugInfo {
        match self {
            Token::Type(_, d) => d,
            Token::Word(_, d) => d,
            Token::Delemiter(_, d) => d,
            Token::Operator(_, _, d) => d,
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
            Token::Label(_, d) => d,
            Token::CompilerDirective(_, d) => d,
            Token::String(_, d) => d,
            Token::LoopStart => panic!("loop start has no debug info"),
        }
    }
}

const TOKEN_REGEX_SRC: &'static str = concat!(
    r"(#.*|--.*|//.*)",  // comments
    r"|(@.*)",      // interpreter directives
     "|\"([^\"]*)\"",   // string literal
    r"|'([a-zA-Z0-9_]+)",    // labels: 'example
    r"|(goto\s+[a-zA-Z0-9_]+",   // goto example
    r"|despite|until|loop|break|cont|ret|yield|please)", // keywords
    r"|\W(int|rat|bool|str)\W", // raw data types
    r"|(true|false|ye|no|maybe)",   // boolean values
    r"|([A-Za-z_]+)\s*(?::\s*([a-zA-Z0-9_]+))?\s*=[^=]", // assignment var:int=
    r"|([A-Za-z_]+)\s*(?::\s*([a-zA-Z0-9_]+))",  // declaration  var:int
    r"|([A-Za-z_]+)",   // symbol
    r"|(\d*\.?\d+)",    // number
    r"|(!=|==|<=|<=|\.\.|[&|+\-*/<>=])", // operator
    r"|([(){}])",   // delemeiter
    r"|(\n)",   // line break
    r"|(;)" // expression terminator
);

lazy_static::lazy_static! {
    static ref TOKEN_REGEX: regex::Regex = regex::Regex::new(TOKEN_REGEX_SRC).unwrap();
}

/// creates a vector of tokens from the specified str.
pub fn tokenize<'a>(source: &'a str, diagnostics: &mut Diagnostics) -> Result<VecDeque<Token<'a>>, ()> {
    let mut tokens = VecDeque::new();

    let mut line_count = 0;
    let mut line_start = 0;

    let mut new_line = false;

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
                if new_line {
                    line_start = mat.start();
                    new_line = false;
                }

                let debug_info = DebugInfo {
                    start: mat.start() - line_start,
                    end: mat.end() - line_start,
                    line: line_count,
                };

                tokens.push_back(match i {
                    2 => Token::CompilerDirective(mat.as_str(), debug_info),
                    3 => Token::String(mat.as_str(), debug_info),
                    4 => Token::Label(mat.as_str(), debug_info),
                    5 => Token::Keyword(Keyword::parse(mat.as_str()), debug_info),
                    6 => Token::Type(Prim::from(mat.as_str(), &debug_info, diagnostics)?, debug_info),
                    7 => Token::Bool(parse_bool(mat.as_str()), debug_info),
                    8 => {
                        let var_type = if let Some(mat) = enumerator.next().unwrap().1 {
                            Some(Prim::from(mat.as_str(), &debug_info, diagnostics)?)
                        } else {
                            None
                        };
                        Token::Assign(mat.as_str(), var_type, debug_info)
                    }
                    10 => {
                        let var_type =
                            Prim::from(enumerator.next().unwrap().1.unwrap().as_str(), &debug_info, diagnostics)?;
                        Token::Decl(mat.as_str(), var_type, debug_info)
                    }
                    12 => Token::Word(mat.as_str(), debug_info),
                    13 => Token::Number(mat.as_str(), NumHint::from(mat.as_str()), debug_info),
                    14 => Token::Operator(Operator::parse(mat.as_str()), None, debug_info),
                    15 => Token::Delemiter(mat.as_str().chars().nth(0).unwrap(), debug_info),
                    16 => {
                        line_count += 1;
                        new_line = true;
                        Token::LineBreak(debug_info)
                    }
                    17 => Token::Terminator(debug_info),

                    _ => {
                        diagnostics.set_err(&debug_info, crate::msg::ERR71, format!("token: {}", mat.as_str()));
                        return Err(());
                    }
                });
                break;
            }
        }
    }

    return Ok(tokens);
}

fn parse_bool(text: &str) -> bool {
    return match text {
        "true" | "ye" => true,
         "false" | "no" => false,
        "maybe" => rand::random(),
        _ => {
            crate::message(MessageType::Critical, format!("token is not a boolean value: {}", text));
            panic!();
        },
    };
}
