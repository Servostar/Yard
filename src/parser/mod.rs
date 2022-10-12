use core::{panic};
use std::{collections::{VecDeque}, vec};
use crate::token::{Token, Operator, Assoc, Prim, MessageType};

pub mod data;

use data::*;

/// simple brace-counting parser to detect functions
fn discover_functions<'a>(tokens: &mut VecDeque<crate::Token<'a>>, source: &str) -> Vec<Func<'a>> {
    let mut funcs = Vec::new();

    // function to currently identifiy
    let mut func = Func::new();

    // count open brackets
    let mut brace_cnt = 0;  
    let mut paren_cnt = 0;

    let mut single_line = false;

    macro_rules! finish_func {
        () => {
            if funcs.contains(&func) {
                panic!("Function defined multiple times: {func}")
            }

            funcs.push(func);
            func = Func::new();
            single_line = false;
        };
    }

    while let Some(top) = tokens.pop_front() {

        // function body detection
        // has highest priority
        match &top {
            Token::Delemiter(char, dbginf) => match char {
                '{' => {
                    brace_cnt += 1;
                    if brace_cnt == 1 {
                        if func.name.is_none() {
                            dbginf.print(MessageType::Error, "Anonymous function not permitted", source);
                            panic!();
                        }
                        single_line = false;
                        func.raw = Some(VecDeque::new());
                        continue;
                    }
                },
                '}' => {
                    brace_cnt -= 1;
                    if brace_cnt == 0 {
                        finish_func!();
                        continue;
                    }
                }
                _ => ()
            }
            
            Token::LineBreak(_) => if single_line {
                finish_func!();
                continue;
            }

            _ => if single_line && func.raw.is_none() {
                func.raw = Some(VecDeque::new());
            }
        }
        
        if func.raw.is_none() {
            match &top {
                Token::Operator(op, dbginf) => match op {
                    Operator::Assign => {
                        if func.results {
                            dbginf.print(MessageType::Error, "double function assignment", source);
                            panic!();
                        }
                        if func.name.is_none() {
                            dbginf.print(MessageType::Error, "Anonymous function", source);
                            panic!();
                        }

                        func.results = true;
                        single_line = true;
                        continue;
                    }
                    _ => ()
                }

                Token::Assign(name, _, dbginf) => {
                    if func.results {
                        dbginf.print(MessageType::Error, "double function assignment", source);
                        panic!();
                    }
                    if func.name.is_some() {
                        dbginf.print(MessageType::Error, "multiple function names", source);
                        panic!();
                    }

                    func.raw = Some(VecDeque::new());
                    func.name = Some(name);
                    func.results = true;
                    single_line = true;
                    continue;
                }
    
                Token::Delemiter(char, dbginf) => match char {
    
                    '(' => if func.raw.is_none() {
                        paren_cnt += 1;
                        if paren_cnt == 1 {
    
                            if func.args.is_some() {
                                dbginf.print(MessageType::Error, "double parameter list", source);
                                panic!();
                            }
    
                            func.args = Some(Vec::new());
                            continue;
                        }
                    },
                    ')' => {
                        paren_cnt -= 1;
                        if paren_cnt == 0 {
                            continue;
                        }
                    }
                    _ => ()
                }
    
                Token::Word(text, dbginf) => {
    
                    if func.name.is_some() {
                        if func.args.is_none() {
                            dbginf.print(MessageType::Error, "multiple function names", source);
                            panic!();
                        }
                    }
                    else {
                        func.name = Some(text);
                        continue;
                    }
                }
                _ => ()
            }
        }

        if let Some(body) = &mut func.raw {
            body.push_back(top);
            continue;
        }
        else if let Some(args) = &mut func.args {

            if paren_cnt == 0 {
                top.print(MessageType::Error, "token is no parameter", source);
                panic!();
            }

            match &top {
                Token::Decl(name, typ, _dbginf) => args.push((name, *typ)),
                Token::Word(_, dbginf) => {
                    dbginf.print(MessageType::Error, "type declaration missing", source);
                    panic!()
                },
                _ => {
                    top.print(MessageType::Error, "argument must be declaration", source);
                    panic!()
                }
            }
            continue;
        }

        // if we have anything left it might be an error
        match &top {
            Token::LineBreak(_) => (), // valid whitespace
            _ => {
                top.print(MessageType::Error, "unresolvable token", source);
                panic!()
            }
        }
    }
    
    funcs
}

/// parse the functions raw content to expr for easy compilation using a brace-counter.
/// - ```{...}``` surround a block
/// - line breaks seperate expressions
fn discover_exprs<'a>(functions: &mut Vec<Func<'a>>, source: &'a str) {
    for func in functions.iter_mut() {

        let mut blocks = vec![Block::new()];

        let mut expr = VecDeque::new();
        
        while let Some(top) = func.raw.as_mut().unwrap().pop_front() {

            match &top {
                Token::LineBreak(dbginf) => if !expr.is_empty() {
                    blocks.last_mut().unwrap_or_else(|| {
                        dbginf.print(MessageType::Error, "curly brace missmatch", source);
                        panic!()
                    }).push_back(Expr::Term(expr));
                    expr = VecDeque::new();
                    continue;
                }
                Token::Delemiter(char, dbginf) => match char {
                    '{' => {
                        blocks.last_mut().unwrap_or_else(|| {
                            dbginf.print(MessageType::Error, "curly brace missmatch", source);
                            panic!()
                        }).push_back(Expr::Term(expr));
                        expr = VecDeque::new();
                        blocks.push(Block::new());
                        continue;
                    },
                    '}' => {
                        // pop topmost block of the stack, storing it in the next lower block
                        if let Some(block) = blocks.pop() {
                            blocks.last_mut().unwrap_or_else(|| {
                                dbginf.print(MessageType::Error, "curly brace missmatch", source);
                                panic!()
                            }).push_back(Expr::Block(block));
                        } else {
                            panic!("Curly brace missmatch")
                        }
                        continue;
                    },
                    _ => ()
                },
                _ => ()
            }
            
            expr.push_back(top)
        }

        if !expr.is_empty() {
            blocks.last_mut().unwrap_or_else(|| {
                expr.back().unwrap().print(MessageType::Error, "curly brace missmatch", source);
                panic!()
            }).push_back(Expr::Term(expr));
        }

        if let Some(block) = blocks.pop() {
            func.expr = Some(Expr::Block(block));
        } else {
            panic!("curly brace missmatch")
        }
    }
}

/// parse a single term using a modified shunting yard
fn parse_term<'a, 'b>(term: &mut VecDeque<Token<'a>>, scope: &mut Scope, source: &'b str) {
    let mut op_stack = vec![];
    let mut output = VecDeque::with_capacity(term.len());
    let mut value_stack = vec![];

    /*
    
    Token::Number(text) => value_stack.push(CompileTimeType::UntypedNum(text)),
    Token::Bool(_) => value_stack.push(CompileTimeType::Prim(Prim::Bool)),
    
    */

    'outer:
    while let Some(token) = term.pop_front() {
        match &token {
            Token::Word(text, dbginf) => {
                if scope.is_func(text) {
                    op_stack.push(Token::Func(text, *dbginf));
                    continue;
                } else if scope.is_arg(text) {
                    output.push_back(Token::Arg(text, *dbginf));
                    continue;
                } else if scope.is_var(text).is_some() {
                    output.push_back(Token::Var(text, *dbginf));
                    continue;
                }
                dbginf.print(MessageType::Error, "Unknown word", source);
                panic!()
            }
            Token::Number(_, _) => {
                output.push_back(token);
                value_stack.push(CompileTimeType::UntypedNum)
            },
            Token::Assign(text, typ, _) => {
                scope.decl_var((*text).to_owned(), typ.to_owned());
                op_stack.push(token);
            },
            Token::Keyword(_, _) => op_stack.push(token),

            Token::Delemiter(char, _) => {
                match char {
                    '(' => op_stack.push(token),
                    ')' => {
                        while let Some(token) = op_stack.pop() {
                            match &token {
                                Token::Delemiter(char, _) => if *char == '(' {
                                    if let Some(next) = op_stack.last() {
                                        match &next {
                                            Token::Func(_, _) => output.push_back(op_stack.pop().unwrap()),
                                            _ => ()
                                        }
                                    }
                                    continue 'outer;
                                },
                                _ => output.push_back(token)
                            }
                        }
                        panic!("Mismatched right parenthesis")
                    },
                    _ => panic!("Misplaced character: '{char}'")
                }
            }

            Token::Operator(op, _) => {
                let prec0 = op.prec();
                while let Some(top) = op_stack.last(){
                    match &top {
                        Token::Operator(op1, _) => {
                            let prec1 = op1.prec();

                            if prec1 > prec0 || prec0 == prec1 && op.assoc() == Assoc::Left {
                                output.push_back(op_stack.pop().unwrap());
                                continue
                            }
                            break
                        },
                        _ => break
                    }
                }
                op_stack.push(token);
            }
            _ => ()
        }
    }

    while let Some(token) = op_stack.pop() {
        match &token {
            Token::Delemiter(char, _) => if *char == '(' {
                panic!("Mismatched parenthesis")
            },
            _ => output.push_back(token)
        }
    }

    term.append(&mut output);
}

enum CompileTimeType {
    Prim(Prim),
    UntypedNum,
}

fn parse_block<'a>(block: &mut Block, scope: &mut Scope, source: &'a str) {
    scope.alloc_scope();
    for expr in block.iter_mut() {
        match expr {
            Expr::Block(block) => parse_block(block, scope, source),
            Expr::Term(term) => parse_term(term, scope, source)
        }
    }
    scope.pop_scope();
}

fn parse_exprs<'a>(funcs: &mut Vec<Func<'a>>, source: &'a str) {
    let mut scope = Scope {
        funcs: funcs.iter().map(|f| f.name.unwrap()).collect(),
        args: None,
        vars: vec![]
    };

    for func in funcs.iter_mut() {
        match func.expr.as_mut().expect("Function has no body") {
            Expr::Block(block) => {
                scope.args = func.args.as_ref();

                parse_block(block, &mut scope, source)
            },
            _ => panic!("Fatal-Compilier-Error: function must have a block")
        }
    }
}

/// reorder and organize a listing of instructions to a RPN based format:
/// any program is made out of functions.
/// A function has a name followed by an optional parameter list, followed by an optional equal sign and block.
pub fn parse<'a>(tokens: &mut VecDeque<crate::Token<'a>>, source: &'a str) -> Vec<Func<'a>> {
    let mut funcs = discover_functions(tokens, source);

    discover_exprs(&mut funcs, source);
    parse_exprs(&mut funcs, source);

    funcs.iter().for_each(|f| println!("{:?}", f));

    funcs
}