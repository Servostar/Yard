use core::{panic};
use std::{collections::{VecDeque}, vec};
use crate::token::{Token, Operator, Assoc, Prim, MessageType, Keyword};

pub mod data;
pub mod msg;

use data::*;

/// simple brace-counting parser to detect functions
fn discover_functions<'a>(tokens: &mut VecDeque<crate::Token<'a>>, source: &str) -> (Vec<Func<'a>>, Vec<Declr<'a>>) {
    let mut funcs = Vec::new();
    let mut declrs = Vec::new();

    // function to currently identifiy
    let mut func = Func::new();
    let mut declr = Declr::new();

    // count open brackets
    let mut brace_cnt = 0;  
    let mut paren_cnt = 0;

    let mut single_line = false;

    macro_rules! finish_func {
        ($dbginf:expr) => {
            if declrs.contains(&declr) {
                println!("{}", $dbginf.make_msg_w_ext(crate::msg::ERR10, format!("Multiple definitions: {declr}")));
                panic!()
            }

            if declr.results && declr.result_typ.is_none() {
                println!("{}", $dbginf.make_msg_w_ext(crate::msg::ERR11, format!("for function {declr}")));
                panic!();
            }

            funcs.push(func);
            declrs.push(declr);
            declr = Declr::new();
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

                    // we have an found open function body
                    if brace_cnt == 1 {
                        if declr.name.is_none() {
                            println!("{}", dbginf.make_msg(crate::msg::ERR12));
                            panic!();
                        }
                        
                        if paren_cnt > 0  {
                            println!("{}", dbginf.make_msg(crate::msg::ERR13));
                            panic!();
                        }

                        single_line = false;
                        func.raw = Some(VecDeque::new());
                        continue;
                    }
                },
                '}' => {
                    brace_cnt -= 1;

                    // we closed one to much
                    if brace_cnt < 0 {
                        println!("{}", dbginf.make_msg(crate::msg::ERR14));
                        panic!();
                    }

                    // end of function body
                    if brace_cnt == 0 {
                        finish_func!(dbginf);
                        continue;
                    }
                }
                _ => ()
            }

            Token::Type(typ, dbginf) => if declr.result_typ.is_none() {
                if declr.results {
                    declr.result_typ = Some(*typ);
                    continue;
                } else {
                    println!("{}", dbginf.make_msg(crate::msg::ERR16));
                    panic!();
                }
            },
            
            Token::LineBreak(dbginf) => if single_line {
                finish_func!(dbginf);
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
                        if declr.results {
                            println!("{}", dbginf.make_msg(crate::msg::ERR17));
                            panic!();
                        }
                        if declr.name.is_none() {
                            println!("{}", dbginf.make_msg(crate::msg::ERR12));
                            panic!();
                        }

                        declr.results = true;
                        single_line = true;
                        continue;
                    }
                    _ => ()
                }

                Token::Word(text, dbginf) => {

                    if declr.name.is_some() {
                        if declr.args.is_none() {
                            println!("{}", dbginf.make_msg(crate::msg::ERR18));
                            panic!();
                        }
                    } else if brace_cnt > 0 {
                        println!("{}", dbginf.make_msg(crate::msg::ERR13));
                        panic!();
                    } else {
                        declr.name = Some(text);
                        continue;
                    }
                },

                Token::Assign(name, _, dbginf) => {
                    if declr.results {
                        println!("{}", dbginf.make_msg(crate::msg::ERR17));
                        panic!();
                    }
                    if declr.name.is_some() {
                        println!("{}", dbginf.make_msg(crate::msg::ERR18));
                        panic!();
                    }

                    func.raw = Some(VecDeque::new());
                    declr.name = Some(name);
                    declr.results = true;
                    single_line = true;
                    continue;
                }
    
                Token::Delemiter(char, dbginf) => match char {
    
                    '(' => if func.raw.is_none() {
                        paren_cnt += 1;
                        if paren_cnt == 1 {
    
                            if declr.args.is_some() {
                                println!("{}", dbginf.make_msg(crate::msg::ERR19));
                                panic!();
                            }
    
                            declr.args = Some(Vec::new());
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
                _ => ()
            }
        }

        if let Some(body) = &mut func.raw {
            body.push_back(top);
            continue;
        }
        else if let Some(args) = &mut declr.args {

            if paren_cnt == 0 {
                println!("{}", top.create_msg(crate::msg::ERR20));
                panic!();
            }

            match &top {
                Token::Decl(name, typ, _dbginf) => args.push((name, *typ)),
                Token::Word(_, dbginf) => {
                    println!("{}", dbginf.make_msg(crate::msg::ERR21));
                    panic!()
                },
                _ => {
                    println!("{}", top.create_msg(crate::msg::ERR23));
                    panic!()
                }
            }
            continue;
        }

        // if we have anything left it might be an error
        match &top {
            Token::LineBreak(_) | Token::Terminator(_) => (), // valid whitespace
            _ => {
                println!("{}", top.create_msg(crate::msg::ERR22));
                panic!()
            }
        }
    }

    if let Some(raw) = func.raw {
        if let Some(front) = raw.front() {
            println!("{}", front.create_msg(crate::msg::ERR15));
            panic!();
        }
    }
    
    (funcs, declrs)
}

/// parse the functions raw content to expr for easy compilation using a brace-counter.
/// - ```{...}``` surround a block
/// - line breaks seperate expressions
fn discover_exprs<'a>(functions: &mut Vec<Func<'a>>, _: &Vec<Declr<'a>>, source: &'a str) {
    for func in functions.iter_mut() {

        let mut blocks = vec![Block::new()];

        let mut expr = VecDeque::new();
        
        while let Some(top) = func.raw.as_mut().unwrap().pop_front() {

            match &top {
                Token::LineBreak(dbginf) | Token::Terminator(dbginf) => if !expr.is_empty() {
                    blocks.last_mut().unwrap_or_else(|| {
                        println!("{}", dbginf.make_msg(crate::msg::ERR41));
                        panic!()
                    }).push_back(Expr::Term(expr));
                    expr = VecDeque::new();
                    continue;
                }
                Token::Delemiter(char, dbginf) => match char {
                    '{' => {
                        blocks.last_mut().unwrap_or_else(|| {
                            println!("{}", dbginf.make_msg(crate::msg::ERR41));
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
                                println!("{}", dbginf.make_msg(crate::msg::ERR41));
                                panic!()
                            }).push_back(Expr::Block(block));
                        } else {
                            println!("{}", dbginf.make_msg(crate::msg::ERR40));
                            panic!()
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
                println!("{}", expr.back().unwrap().create_msg(crate::msg::ERR40));
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

fn check_var_typ(typ: &mut Option<Prim>, operands: &mut Vec<Prim>, dbginf: &crate::token::DebugInfo, source: &str) {

    if let Some(value) = operands.pop() {
        if !operands.is_empty() {
            println!("{}", dbginf.make_msg(crate::msg::ERR50));
            panic!();
        }

        if let Some(typ) = typ {
            if !typ.is_equal(value) {
                println!("{}", dbginf.make_msg_w_ext(crate::msg::ERR51, format!("needed {:?} got {:?}", typ, value) ));
                panic!();
            }
        } else {
            // assign a type to untyped variable
            println!("{}", dbginf.make_msg_w_ext(crate::msg::INF51, format!("gessed type: {:?}", value) ));
            *typ = Some(value);
        }
        
    } else {
        println!("{}", dbginf.make_msg(crate::msg::ERR52));
        panic!();
    }
}

fn process_keyword(keyword: Keyword, _: &Vec<Declr>, scope: &mut Scope, operands: &mut Vec<Prim>, dbginf: &crate::token::DebugInfo, source: &str) {
    match keyword {
        Keyword::If | Keyword::While => {
            if operands.len() != 1 {
                println!("{}", dbginf.make_msg_w_ext(crate::msg::ERR53, format!("got {:?} values", operands.len()) ));
                panic!();
            }

            if let Some(operand) = operands.pop() {
                match operand {
                    Prim::Bool => (),
                    _ => {
                        println!("{}", dbginf.make_msg_w_ext(crate::msg::ERR53, format!("got {:?}", operand) ));
                        panic!();
                    }
                }
            }
        },
        Keyword::Return => {
            if scope.func_return_typ.is_some() {
                println!("{}", dbginf.make_msg_w_ext(crate::msg::ERR54, "perhaps use `yield`" ));
                panic!();
            }
        }
        Keyword::Yield => {
            if operands.len() != 1 {
                println!("{}", dbginf.make_msg_w_ext(crate::msg::ERR55, format!("got {:?} instead of 1", operands.len()) ));
                panic!();
            }

            if let Some(operand) = operands.pop() {
                if let Some(typ) = scope.func_return_typ {
                    if typ != operand {
                        println!("{}", dbginf.make_msg_w_ext(crate::msg::ERR59, format!("expected {:?} got {:?}", typ, operand) ));
                        panic!();
                    }
                    println!("{}", scope.cond_count);
                    scope.yields = scope.cond_count == 1;
                } else {
                    println!("{}", dbginf.make_msg(crate::msg::ERR57));
                    panic!();
                }
            } else {
                println!("{}", dbginf.make_msg(crate::msg::ERR58));
                panic!();
            }
        }
        _ => ()
    }
}

fn collapse_operation(operation: &mut Token, declrs: &Vec<Declr>, scope: &mut Scope, operands: &mut Vec<Prim>, source: &str) {
    match operation {
        Token::Operator(op, dbginf) => op.operate(operands, &dbginf, source),
        Token::Assign(name, mut typ, dbginf) => {
            check_var_typ(&mut typ, operands, &dbginf, source);
            scope.decl_var((*name).to_owned(), typ);
        },
        Token::Func(name, dbginf) => call_func(name, declrs, scope, operands, &dbginf, source),
        Token::Keyword(keyword, dbginf) => process_keyword(*keyword, declrs, scope, operands, &dbginf, source),
        _ => ()
    }
}

fn call_func(name: &str, declrs: &Vec<Declr>, _: &mut Scope, operands: &mut Vec<Prim>, dbginf: &crate::token::DebugInfo, source: &str) {
    for declr in declrs {
        if declr.name.is_some() && declr.name.unwrap() == name {

            if let Some(args) = &declr.args {

                if args.len() > operands.len() {
                    println!("{}", dbginf.make_msg_w_ext(crate::msg::ERR60, format!("expected {:?} got {:?}", args.len(), operands.len()) ));
                    panic!()
                }

                for (x, arg) in args.iter().enumerate() {
                    let operand = operands.first().unwrap();

                    if !operand.is_equal(arg.1) {
                        println!("{}", dbginf.make_msg_w_ext(crate::msg::ERR61, format!("expected {:?} got {:?}", arg, operand) ));
                        panic!()
                    }

                    operands.remove(0);
                }
            }

            if let Some(typ) = declr.result_typ {
                operands.push(typ);
            }

            break
        }
    }
}

/// parse a single term using a modified shunting yard
fn parse_term<'a>(term: &mut VecDeque<Token<'a>>, declrs: &Vec<Declr<'a>>, scope: &mut Scope, source: & str) {
    let mut op_stack = vec![];
    let mut output = VecDeque::with_capacity(term.len());
    let mut value_stack = vec![];

    'outer:
    while let Some(token) = term.pop_front() {
        match &token {
            Token::Word(text, dbginf) => {
                if is_func(declrs, text) {
                    op_stack.push(Token::Func(text, *dbginf));
                    continue;
                } else if scope.is_arg(text) {
                    value_stack.push(scope.get_arg_type(text));
                    output.push_back(Token::Arg(text, *dbginf));
                    continue;
                } else if scope.is_var(text).is_some() {
                    value_stack.push(scope.get_var_type(text));
                    output.push_back(Token::Var(text, *dbginf));
                    continue;
                }
                println!("{}", dbginf.make_msg(crate::msg::ERR62));
                panic!()
            }
            Token::Bool(_, _) => {
                output.push_back(token);
                value_stack.push(Prim::Bool)
            },
            Token::Number(_, hint, _) => {
                output.push_back(token.clone());
                value_stack.push(Prim::UntypedNum(*hint))
            },
            Token::Assign(_, _, _) => {
                op_stack.push(token);
            },
            Token::Keyword(_, _) => op_stack.push(token),

            Token::Delemiter(char, dbginf) => {
                match char {
                    '(' => op_stack.push(token),
                    ')' => {
                        while let Some(mut token) = op_stack.pop() {
                            match &token {
                                Token::Delemiter(char, _) => if *char == '(' {
                                    if let Some(next) = op_stack.last() {
                                        match &next {
                                            Token::Func(_, _) => {
                                                let mut token = op_stack.pop().unwrap();
                                                collapse_operation(&mut token, declrs, scope, &mut value_stack, source);
                                                output.push_back(token);
                                            },
                                            _ => ()
                                        }
                                    }
                                    continue 'outer;
                                },
                                _ => {
                                    collapse_operation(&mut token, declrs, scope, &mut value_stack, source);
                                    output.push_back(token)
                                }
                            }
                        }
                        println!("{}", dbginf.make_msg(crate::msg::ERR64));
                        panic!();
                    },
                    _ => {
                        println!("{}", dbginf.make_msg(crate::msg::ERR65));
                        panic!()
                    }
                }
            }

            Token::Operator(op, _) => {
                let prec0 = op.prec();
                while let Some(mut top) = op_stack.last_mut(){
                    match &top {
                        Token::Operator(op1, _) => {
                            let prec1 = op1.prec();

                            if prec1 > prec0 || prec0 == prec1 && op.assoc() == Assoc::Left {
                                collapse_operation(&mut top, declrs, scope, &mut value_stack, source);
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

    while let Some(mut token) = op_stack.pop() {
        match &token {
            Token::Delemiter(char, dbginf) => if *char == '(' {
                println!("{}", dbginf.make_msg(crate::msg::ERR63));
            },
            _ => {
                collapse_operation(&mut token, declrs, scope, &mut value_stack, source);
                output.push_back(token)
            }
        }
    }

    if value_stack.len() > 1 {
        println!("{}", output[0].create_msg(crate::msg::ERR50));
        panic!();
    }

    scope.expr_yield = value_stack.len() == 1;
    if scope.expr_yield {
        let yielded = value_stack.pop().unwrap();
        if !yielded.is_equal(scope.func_return_typ.unwrap()) {
            println!("{}", output[0].create_msg_w_ext(crate::msg::ERR59, format!("expected: {:?} got {:?}", scope.func_return_typ.unwrap(), yielded) ));
            panic!();
        }
    }

    term.append(&mut output);
}

fn is_func(declrs: &[Declr], text: &str) -> bool {
    for declr in declrs {
        if declr.name.is_some() && declr.name.unwrap() == text {
            return true;
        }
    }
    return false;
}

fn parse_block<'a>(block: &mut Block<'a>, declrs: &Vec<Declr<'a>>, scope: &mut Scope, source: &str) {
    scope.cond_count += 1;
    scope.alloc_scope();
    for expr in block.iter_mut() {
        match expr {
            Expr::Block(block) => parse_block(block, declrs, scope, source),
            Expr::Term(term) => parse_term(term, declrs, scope, source)
        }
    }
    scope.pop_scope();
    scope.cond_count -= 1;
}

fn parse_exprs<'a>(funcs: &mut Vec<Func<'a>>, declrs: &Vec<Declr<'a>>, source: &'a str) {
    let mut scope = Scope::new();

    for (x, func) in funcs.iter_mut().enumerate() {
        match func.expr.as_mut().expect("Function has no body") {
            Expr::Block(block) => {
                scope.args = declrs[x].args.as_ref();
                scope.func_return_typ = declrs[x].result_typ;
                scope.yields = false;
                scope.cond_count = 0;
                
                parse_block(block, declrs, &mut scope, source);

                if scope.func_return_typ.is_some() && !scope.yields && !scope.expr_yield {
                    //token.create_msg_w_ext(crate::msg::ERR56, format!("at function {}", declrs[x]));
                    panic!();
                }
            },
            _ => panic!("Fatal-Compilier-Error: function must have a block")
        }
    }
}

/// reorder and organize a listing of instructions to a RPN based format:
/// any program is made out of functions.
/// A function has a name followed by an optional parameter list, followed by an optional equal sign and block.
pub fn parse<'a>(tokens: &mut VecDeque<crate::Token<'a>>, source: &'a str) -> Vec<Func<'a>> {
    let (mut funcs, declrs) = discover_functions(tokens, source);
    
    // make all of this shit return a mutable diagnostics struct.
    discover_exprs(&mut funcs, &declrs, source);
    parse_exprs(&mut funcs, &declrs, source);

    crate::inter::convert_to_erpn(&mut funcs, &declrs);
 
    funcs
}
