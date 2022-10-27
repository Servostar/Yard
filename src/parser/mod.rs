use core::{panic};
use std::{collections::{VecDeque}, vec};
use crate::token::{Token, Operator, Assoc, Prim, MessageType, Keyword};

pub mod data;

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
                panic!("Function defined multiple times: {declr}")
            }

            if declr.results && declr.result_typ.is_none() {
                $dbginf.print(MessageType::Error, format!("Function is missing return type: {}", declr).as_str(), source);
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
                    if brace_cnt == 1 {
                        if declr.name.is_none() {
                            dbginf.print(MessageType::Error, "Anonymous function not permitted", source);
                            panic!();
                        }
                        
                        if paren_cnt > 0  {
                            dbginf.print(MessageType::Error, "Unclosed parameter list", source);
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
                        finish_func!(dbginf);
                        continue;
                    }
                }
                _ => ()
            }

            Token::Type(typ, dbginf) => {
                if declr.results {
                    if declr.result_typ.is_some() {
                        dbginf.print(MessageType::Error, "Function must return either nothing or a single type", source);
                        panic!();
                    }

                    declr.result_typ = Some(*typ);
                    continue;
                } else {
                    dbginf.print(MessageType::Error, "Missing equal sign", source);
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
                            dbginf.print(MessageType::Error, "double function assignment", source);
                            panic!();
                        }
                        if declr.name.is_none() {
                            dbginf.print(MessageType::Error, "Anonymous function", source);
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
                            dbginf.print(MessageType::Error, "multiple function names", source);
                            panic!();
                        }
                    } else if brace_cnt > 0 {
                        dbginf.print(MessageType::Error, "brace count missmatch", source);
                        panic!();
                    }
                    else {
                        declr.name = Some(text);
                        continue;
                    }
                },

                Token::Assign(name, _, dbginf) => {
                    if declr.results {
                        dbginf.print(MessageType::Error, "double function assignment", source);
                        panic!();
                    }
                    if declr.name.is_some() {
                        dbginf.print(MessageType::Error, "multiple function names", source);
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
                                dbginf.print(MessageType::Error, "double parameter list", source);
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
            Token::LineBreak(_) | Token::Terminator(_) => (), // valid whitespace
            _ => {
                top.print(MessageType::Error, "unresolvable token", source);
                panic!()
            }
        }
    }

    if let Some(raw) = func.raw {
        if let Some(front) = raw.front() {
            front.print(MessageType::Error, "Open function body", source);
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

fn check_var_typ(typ: &mut Option<Prim>, operands: &mut Vec<Prim>, dbginf: &crate::token::DebugInfo, source: &str) {
    if let Some(value) = operands.pop() {
        if !operands.is_empty() {
            dbginf.print(MessageType::Error, format!("Expr does't resolve to a single value but multiple").as_str(), source);
            panic!();
        }

        if let Some(typ) = typ {
            if !typ.is_equal(value) {
                dbginf.print(MessageType::Error, format!("Variable has type {:?} but {:?} was given", typ, value).as_str(), source);
                panic!();
            }
        } else {
            // assign a type to untyped variable
            dbginf.print(MessageType::Info, format!("Variable has no fixed type, guessing type: {:?}", value).as_str(), source);

            *typ = Some(value);
        }
        
    } else {
        dbginf.print(MessageType::Error, "No result to bind variable to", source);
        panic!();
    }
}

fn process_keyword(keyword: Keyword, _: &Vec<Declr>, scope: &mut Scope, operands: &mut Vec<Prim>, dbginf: &crate::token::DebugInfo, source: &str) {
    match keyword {
        Keyword::If | Keyword::While => {
            if operands.len() != 1 {
                dbginf.print(MessageType::Error, format!("Expected single boolean got {} values", operands.len()).as_str(), source);
                panic!();
            }

            if let Some(operand) = operands.pop() {
                match operand {
                    Prim::Bool => (),
                    _ => {
                        dbginf.print(MessageType::Error, format!("Expected boolean, got {:?}", operand).as_str(), source);
                        panic!();
                    }
                }
            }
        },
        Keyword::Return => {
            if scope.func_return_typ.is_some() {
                dbginf.print(MessageType::Error, "cannot return function, did u mean to use `yield`?", source);
                panic!();
            }
        }
        Keyword::Yield => {
            if operands.len() != 1 {
                dbginf.print(MessageType::Error, format!("Expected single value but got {} values", operands.len()).as_str(), source);
                panic!();
            }

            if let Some(operand) = operands.pop() {
                if let Some(typ) = scope.func_return_typ {
                    if typ != operand {
                        dbginf.print(MessageType::Error, format!("Expected {:?} but got {:?}", typ, operand).as_str(), source);
                        panic!();
                    }
                    if scope.cond_scope {
                        scope.yields = true;
                    }
                } else {
                    dbginf.print(MessageType::Error, format!("Function does not return anything").as_str(), source);
                    panic!();
                }
            } else {
                dbginf.print(MessageType::Error, format!("Yield must return something").as_str(), source);
                panic!();
            }
        }
        _ => ()
    }
}

fn collapse_operation(operation: &Token, declrs: &Vec<Declr>, scope: &mut Scope, operands: &mut Vec<Prim>, source: &str) {
    match operation {
        Token::Operator(op, dbginf) => op.operate(operands, &dbginf, source),
        Token::Assign(name, mut typ, dbginf) => {
            check_var_typ(&mut typ, operands, &dbginf, source);
            scope.decl_var((*name).to_owned(), typ.to_owned());
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
                    dbginf.print(MessageType::Error, format!("Expected {} parameters but got {}", args.len(), operands.len()).as_str(), source);
                    panic!()
                }

                for (x, arg) in args.iter().enumerate() {
                    let operand = operands.first().unwrap();

                    if !operand.is_equal(arg.1) {
                        dbginf.print(MessageType::Error, format!("Expected {:?} as parameter {x}, but got {:?}", arg, operand).as_str(), source);
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
                dbginf.print(MessageType::Error, "Unknown word", source);
                panic!()
            }
            Token::Bool(_, _) => {
                output.push_back(token);
                value_stack.push(Prim::Bool)
            },
            Token::Number(_, hint, _) => {
                output.push_back(token);
                value_stack.push(Prim::UntypedNum(*hint))
            },
            Token::Assign(_, _, _) => {
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
                                            Token::Func(_, _) => {
                                                let token = op_stack.pop().unwrap();
                                                collapse_operation(&token, declrs, scope, &mut value_stack, source);
                                                output.push_back(token);
                                            },
                                            _ => ()
                                        }
                                    }
                                    continue 'outer;
                                },
                                _ => {
                                    collapse_operation(&token, declrs, scope, &mut value_stack, source);
                                    output.push_back(token)
                                }
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
                                collapse_operation(top, declrs, scope, &mut value_stack, source);
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
            _ => {
                collapse_operation(&token, declrs, scope, &mut value_stack, source);
                output.push_back(token)
            }
        }
    }

    if value_stack.len() > 1 {
        output[0].print(MessageType::Error, "expression resolves to multiple results", source);
        panic!();
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
    scope.alloc_scope();
    for expr in block.iter_mut() {
        match expr {
            Expr::Block(block) => parse_block(block, declrs, scope, source),
            Expr::Term(term) => parse_term(term, declrs, scope, source)
        }
    }
    scope.pop_scope();
}

fn parse_exprs<'a>(funcs: &mut Vec<Func<'a>>, declrs: &Vec<Declr<'a>>, source: &'a str) {
    let mut scope = Scope {
        args: None,
        vars: vec![],
        func_return_typ: None,
        cond_scope: false,
        yields: false,
    };

    for (x, func) in funcs.iter_mut().enumerate() {
        match func.expr.as_mut().expect("Function has no body") {
            Expr::Block(block) => {
                scope.args = declrs[x].args.as_ref();
                scope.func_return_typ = declrs[x].result_typ;
                scope.cond_scope = false;
                scope.yields = false;
                
                parse_block(block, declrs, &mut scope, source);

                if scope.func_return_typ.is_some() && !scope.yields {
                    crate::message(MessageType::Error, format!("Function {} missing return value at some point", declrs[x]));
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

    discover_exprs(&mut funcs, &declrs, source);
    parse_exprs(&mut funcs, &declrs, source);

    for (x, f) in funcs.iter().enumerate() {
        println!("{:#?}{:#?}", declrs[x], f);
    }
 
    funcs
}