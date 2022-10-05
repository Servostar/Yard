use core::panic;
use std::{collections::{VecDeque}, vec};
use crate::token::{Token, Operator, Assoc};

mod data;

use data::*;

/// simple brace-counting parser to detect functions
fn discover_functions<'a>(tokens: &mut VecDeque<crate::Token<'a>>) -> Vec<Func<'a>> {
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
                panic!("Function already defined: {func}")
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
            Token::Delemiter(char) => match char {
                '{' => {
                    brace_cnt += 1;
                    if brace_cnt == 1 {
                        if func.name.is_none() {
                            panic!("Anonymous function not permitted");
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
            
            Token::LineBreak => if single_line {
                finish_func!();
                continue;
            }

            _ => if single_line && func.raw.is_none() {
                func.raw = Some(VecDeque::new());
            }
        }
        
        if func.raw.is_none() {
            match &top {
                Token::Operator(op) => match op {
                    Operator::Assign => {
                        if func.results {
                            panic!("double function assignment not permitted")
                        }
                        if func.name.is_none() {
                            panic!("Anonymous function not permitted");
                        }

                        func.results = true;
                        single_line = true;
                        continue;
                    }
                    _ => ()
                }

                Token::Assign(name) => {
                    if func.results {
                        panic!("double function assignment not permitted")
                    }
                    if func.name.is_some() {
                        panic!("function already named");
                    }

                    func.raw = Some(VecDeque::new());
                    func.name = Some(name);
                    func.results = true;
                    single_line = true;
                    continue;
                }
    
                Token::Delemiter(char) => match char {
    
                    '(' => if func.raw.is_none() {
                        paren_cnt += 1;
                        if paren_cnt == 1 {
    
                            if func.args.is_some() {
                                panic!("double parameter list not permitted");
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
    
                Token::Word(text) => {
    
                    if func.name.is_some() {
                        if func.args.is_none() {
                            panic!("Function name already set: {text}")
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
                panic!("Token is not in parameter list: {:?}", top)
            }

            match &top {
                Token::Word(text) => args.push(text),
                _ => panic!("Argument is not a word {:?}", &top)
            }
            continue;
        }

        // if we have anything left it might be an error
        match &top {
            Token::LineBreak => (), // valid whitespace
            _ => panic!("Invalid token: {:?}", top)
        }
    }
    
    funcs
}

/// parse the functions raw content to expr for easy compilation using a brace-counter.
/// - ```{...}``` surround a block
/// - line breaks seperate expressions
fn discover_exprs<'a>(functions: &mut Vec<Func<'a>>) {
    for func in functions.iter_mut() {

        let mut blocks = vec![Block::new()];

        let mut expr = VecDeque::new();
        
        while let Some(top) = func.raw.as_mut().unwrap().pop_front() {

            match &top {
                Token::LineBreak => if !expr.is_empty() {
                    blocks.last_mut().expect("Curly brace missmatch").push_back(Expr::Term(expr));
                    expr = VecDeque::new();
                    continue;
                }
                Token::Delemiter(char) => match char {
                    '{' => {
                        blocks.last_mut().expect("Curly brace missmatch").push_back(Expr::Term(expr));
                        expr = VecDeque::new();
                        blocks.push(Block::new());
                        continue;
                    },
                    '}' => {
                        // pop topmost block of the stack, storing it in the next lower block
                        if let Some(block) = blocks.pop() {
                            blocks.last_mut().expect("Curly brace missmatch").push_back(Expr::Block(block));
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
            blocks.last_mut().expect("Curly brace missmatch").push_back(Expr::Term(expr));
        }

        func.expr = Some(Expr::Block(blocks.pop().expect("Curly brace missmmatch")));
    }
}

/// parse a single term using a modified shunting yard
fn parse_term<'a>(term: &mut VecDeque<Token<'a>>, scope: &mut Scope) {
    let mut op_stack = vec![];
    let mut output = VecDeque::with_capacity(term.len());

    'outer:
    while let Some(token) = term.pop_front() {
        match &token {
            Token::Word(text) => {
                if scope.is_func(text) {
                    op_stack.push(Token::Func(text));
                    continue;
                } else if scope.is_arg(text) {
                    output.push_back(Token::Arg(text));
                    continue;
                } else if scope.is_var(text) {
                    output.push_back(Token::Var(text));
                    continue;
                }
                panic!("Unknwon word: {text}")
            }
            Token::Number(_) => output.push_back(token),
            Token::Assign(_) => op_stack.push(token),
            Token::Keyword(_) => op_stack.push(token),

            Token::Delemiter(char) => {
                match char {
                    '(' => op_stack.push(token),
                    ')' => {
                        while let Some(token) = op_stack.pop() {
                            match &token {
                                Token::Delemiter(char) => if *char == '(' {
                                    if let Some(next) = op_stack.last() {
                                        match &next {
                                            Token::Func(_) => output.push_back(op_stack.pop().unwrap()),
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

            Token::Operator(op) => {
                let prec0 = op.prec();
                while let Some(top) = op_stack.last(){
                    match &top {    
                        Token::Operator(op1) => {
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
            Token::Delemiter(char) => if *char == '(' {
                panic!("Mismatched parenthesis")
            },
            _ => output.push_back(token)
        }
    }

    term.append(&mut output);
}

fn parse_block(block: &mut Block, scope: &mut Scope) {
    for expr in block.iter_mut() {
        match expr {
            Expr::Block(block) => parse_block(block, scope),
            Expr::Term(term) => parse_term(term, scope)
        }
    }
}

fn parse_exprs<'a>(funcs: &mut Vec<Func<'a>>) {
    let mut scope = Scope {
        funcs: funcs.iter().map(|f| f.name.unwrap()).collect(),
        args: None,
        vars: vec![]
    };

    for func in funcs.iter_mut() {
        match func.expr.as_mut().expect("Function has no body") {
            Expr::Block(block) => {
                scope.args = func.args.as_ref();

                parse_block(block, &mut scope)
            },
            _ => panic!("Fatal-Compilier-Error: function must have a block")
        }
    }
}

/// reorder and organize a listing of instructions to a RPN based format:
/// any program is made out of functions.
/// A function has a name followed by an optional parameter list, followed by an optional equal sign and block.
pub fn parse<'a>(tokens: &mut VecDeque<crate::Token<'a>>)  {
    let mut funcs = discover_functions(tokens);

    discover_exprs(&mut funcs);
    parse_exprs(&mut funcs);

    funcs.iter().for_each(|f| println!("{:?}", f));
}