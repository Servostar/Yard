use crate::{token::{Assoc, DebugInfo, Keyword, Operator, Prim, Token}, conf::Settings};
use core::panic;
use std::{collections::VecDeque, vec};

pub mod data;
pub mod msg;

use data::*;

/// simple brace-counting parser to detect functions
fn discover_functions<'a>(
    tokens: &mut VecDeque<crate::Token<'a>>,
    diagnostics: &mut data::Diagnostics,
) -> Result<(Vec<Func<'a>>, Vec<Declr<'a>>), ()> {

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
        ($token:expr) => {
            // check if the function is already declared
            if declrs.contains(&declr) {
                diagnostics.set_err(
                    $token,
                    crate::msg::ERR10,
                    format!("Multiple definitions: {declr}"),
                );
                return Err(());
            }

            // check if the function returns sth but no return value is given
            if declr.results && declr.result_typ.is_none() {
                diagnostics.set_err($token, crate::msg::ERR11, format!("for function {declr}"));
                return Err(());
            }

            declr.gen_uuid();

            // store new function and its declaration
            funcs.push(func);
            declrs.push(declr);

            // create new empty function
            declr = Declr::new();
            func = Func::new();
            single_line = false;
        };
    }

    while let Some(top) = tokens.pop_front() {
        // function body detection
        // has highest priority
        match &top {
            Token::Delemiter(char, _) => match char {
                // open body
                '{' => {
                    brace_cnt += 1;

                    // we have an found open function body
                    if brace_cnt == 1 {
                        // check if we have a name for our function
                        if declr.name.is_none() {
                            diagnostics.set_err(&top, crate::msg::ERR12, "");
                            return Err(());
                        }

                        // check if the parameter list was closed 
                        if paren_cnt > 0 {
                            diagnostics.set_err(&top, crate::msg::ERR13, "");
                            return Err(());
                        }
                        
                        single_line = false;
                        func.raw = Some(VecDeque::new());
                        continue;
                    }
                }
                '}' => {
                    brace_cnt -= 1;

                    // we closed one to much
                    if brace_cnt < 0 {
                        diagnostics.set_err(&top, crate::msg::ERR14, "");
                        return Err(());
                    }

                    // end of function body
                    if brace_cnt == 0 {
                        finish_func!(&top);
                        continue;
                    }
                }
                _ => (),
            },

            Token::Type(typ, _) => {
                // check if we already have a result type
                if declr.result_typ.is_none() {
                    // then check if we even need to return sth.
                    if declr.results {
                        declr.result_typ = Some(*typ);
                        continue;
                    } else {
                        diagnostics.set_err(&top, crate::msg::ERR16, "");
                        return Err(());
                    }
                } else {
                    diagnostics.set_err(&top, crate::msg::ERR24, "");
                    return Err(());
                }
            }

            Token::LineBreak(_) => {
                if single_line {
                    finish_func!(&top);
                    continue;
                }
            }

            _ => {
                if single_line && func.raw.is_none() {
                    func.raw = Some(VecDeque::new());
                }
            }
        }

        if func.raw.is_none() {
            match &top {
                Token::Operator(op, _, _) => match op {
                    Operator::Assign => {
                        // check if we already have an assign
                        if declr.results {
                            diagnostics.set_err(&top, crate::msg::ERR17, "");
                            return Err(());
                        }
                        // check if we even have a function name
                        // which must always be first
                        if declr.name.is_none() {
                            diagnostics.set_err(&top, crate::msg::ERR12, "");
                            return Err(());
                        }

                        declr.results = true;
                        single_line = true;
                        continue;
                    }
                    _ => (),
                },

                Token::Word(text, info) => {
                    // check if we have a function name
                    if declr.name.is_some() {
                        // we have a function name check if we have no open argument list.
                        // if so this word is missplaced
                        if declr.args.is_none() {
                            diagnostics.set_err(&top, crate::msg::ERR18, "");
                            return Err(());
                        }

                    // function body is open and we have no function name
                    } else if brace_cnt > 0 {
                        diagnostics.set_err(&top, crate::msg::ERR13, "");
                        return Err(());

                    // this must be a valid function name
                    } else {
                        declr.name = Some(text);
                        declr.info = Some(info.to_owned());
                        continue;
                    }
                }

                Token::Assign(name, _, info) => {
                    // check if we already marked a return type
                    // we dont want a double assignment
                    if declr.results {
                        diagnostics.set_err(&top, crate::msg::ERR17, "");
                        return Err(());
                    }
                    // check if we already set function name
                    // then we dont want to reassign
                    if declr.name.is_some() {
                        diagnostics.set_err(&top, crate::msg::ERR18, "");
                        return Err(());
                    }

                    func.raw = Some(VecDeque::new());
                    declr.name = Some(name);
                    declr.info = Some(info.to_owned());
                    declr.results = true;
                    single_line = true;
                    continue;
                }

                Token::Delemiter(char, _) => match char {
                    '(' => {
                        // check for no existing function body
                        // if we have none we can open an parameter list
                        if func.raw.is_none() {
                            paren_cnt += 1;
                            // check if we have one open parenthsis
                            if paren_cnt == 1 {
                                // we have some arguments then we have a double parameter list
                                // dont want that
                                if declr.args.is_some() {
                                    diagnostics.set_err(&top, crate::msg::ERR19, "");
                                    return Err(());
                                }

                                declr.args = Some(Vec::new());
                                continue;
                            }
                        }
                    }
                    ')' => {
                        paren_cnt -= 1;
                        if paren_cnt == 0 {
                            continue;
                        }
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        if let Some(body) = &mut func.raw {
            // if we have an open function body
            // drop all token in there till it closes
            body.push_back(top);
            continue;

        // check for open parameter list
        } else if let Some(args) = &mut declr.args {
            // the parameter list is closed
            if paren_cnt == 0 {
                diagnostics.set_err(&top, crate::msg::ERR20, "");
                return Err(());
            }

            match &top {
                Token::Decl(name, typ, _dbginf) => args.push((name, *typ)),

                // as parameter we only accept declarations
                Token::Word(_, _) => {
                    diagnostics.set_err(&top, crate::msg::ERR21, "");
                    return Err(());
                }
                _ => {
                    diagnostics.set_err(&top, crate::msg::ERR23, "");
                    return Err(());
                }
            }
            continue;
        }

        // if we have anything left it might be an error
        match &top {
            Token::LineBreak(_) | Token::Terminator(_) => (), // valid whitespace
            _ => {
                diagnostics.set_err(&top, crate::msg::ERR22, "");
                return Err(());
            }
        }
    }

    if let Some(raw) = func.raw {
        // still some raw unused tokens left over
        if let Some(front) = raw.front() {
            diagnostics.set_err(front, crate::msg::ERR15, "");
            return Err(());
        }
    }

    Ok((funcs, declrs))
}

/// parse the functions raw content to expr for easy compilation using a brace-counter.
/// - ```{...}``` surround a block
/// - line breaks seperate expressions
fn discover_exprs<'a>(
    functions: &mut Vec<Func<'a>>,
    _: &Vec<Declr<'a>>,
    diagnostics: &mut data::Diagnostics,
) -> Result<(),()> {

    for func in functions.iter_mut() {
        let mut blocks = vec![Block::new()];

        let mut expr = VecDeque::new();

        while let Some(top) = func.raw.as_mut().unwrap().pop_front() {
            match &top {
                Token::LineBreak(_) | Token::Terminator(_) => {
                    if !expr.is_empty() {
                        if let Some(blocks) = blocks.last_mut() {
                            blocks.push_back(Expr::Term(expr))
                        } else {
                            diagnostics.set_err(&top, crate::msg::ERR41, "");
                            return Err(());
                        }
                        expr = VecDeque::new();
                        continue;
                    }
                }
                Token::Delemiter(char, _) => match char {
                    '{' => {

                        if let Some(block) = blocks.last_mut() {
                            block.push_back(Expr::Term(expr));
                        } else {
                            diagnostics.set_err(&top, crate::msg::ERR41, "");
                            return Err(());
                        }
                        expr = VecDeque::new();
                        blocks.push(Block::new());
                        continue;
                    }
                    '}' => {
                        // pop topmost block of the stack, storing it in the next lower block
                        if let Some(block) = blocks.pop() {
                            if let Some(dst) = blocks.last_mut() {
                                dst.push_back(Expr::Block(block));
                            } else {
                                diagnostics.set_err(&top, crate::msg::ERR41, "");
                                return Err(());
                            }
                        } else {
                            diagnostics.set_err(&top, crate::msg::ERR40, "");
                            return Err(());
                        }
    
                        if blocks.is_empty() {
                            diagnostics.set_err(&top, crate::msg::ERR41, "");
                            return Err(());
                        }

                        continue;
                    }
                    _ => (),
                },
                _ => (),
            }

            expr.push_back(top)
        }

        if !expr.is_empty() {
            if let Some(block) = blocks.last_mut() {
                block.push_back(Expr::Term(expr));
            } else {
                diagnostics.set_err(expr.back().unwrap(), crate::msg::ERR40, "");
                return Err(());
            }
        }

        if let Some(block) = blocks.pop() {
            func.expr = Some(Expr::Block(block));
        }
    }
    Ok(())
}

fn check_var_typ(
    typ: &mut Option<Prim>,
    operands: &mut Vec<Prim>,
    info: &crate::token::DebugInfo,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {
    if let Some(mut value) = operands.pop() {
        if !operands.is_empty() {
            diagnostics.set_err(info, crate::msg::ERR50, "");
            return Err(());
        }

        if let Some(typ) = typ {
            if !typ.is_equal(value) {
                diagnostics.set_err(
                    info,
                    crate::msg::ERR51,
                    format!("needed {:?} got {:?}", typ, value),
                );
                return Err(());
            }
        } else {
            diagnostics.hint(info, crate::msg::INF51, format!("gessed type: {:?}", value));

            // resolve to concrete typ
            // i.e all Prim::Num(hint) will convert to their hints.
            value = match value {
                Prim::Num(hint) => match hint {
                    crate::token::NumHint::Int => Prim::Int,
                    crate::token::NumHint::Rat => Prim::Rat,
                }
                _ => value
            };

            *typ = Some(value);
        }
    } else {
        diagnostics.set_err(info, crate::msg::ERR52, "");
        return Err(());
    }
    Ok(())
}

fn process_keyword(
    info: &DebugInfo,
    keyword: Keyword,
    scope: &mut Scope,
    operands: &mut Vec<Prim>,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {
    match keyword {
        Keyword::Unless | Keyword::While => {
            if operands.len() != 1 {
                diagnostics.set_err(
                    info,
                    crate::msg::ERR53,
                    format!("got {:?} values", operands.len()),
                );
                return Err(());
            }

            if let Some(operand) = operands.pop() {
                match operand {
                    Prim::Bool => (),
                    _ => {
                        diagnostics.set_err(info, crate::msg::ERR53, format!("got {:?}", operand));
                        return Err(());
                    }
                }
            }
        }
        Keyword::Return => {
            if scope.func_return_typ.is_some() {
                diagnostics.set_err(info, crate::msg::ERR54, "perhaps use `yield`");
                return Err(());
            }
        }
        Keyword::Yield => {
            if operands.len() != 1 {
                diagnostics.set_err(
                    info,
                    crate::msg::ERR55,
                    format!("got {:?} instead of 1", operands.len()),
                );
                return Err(());
            }

            if let Some(operand) = operands.pop() {
                if let Some(typ) = scope.func_return_typ {
                    if !typ.is_equal(operand) {
                        diagnostics.set_err(
                            info,
                            crate::msg::ERR59,
                            format!("expected {:?} got {:?}", typ, operand),
                        );
                        return Err(());
                    }
                    scope.yields = scope.cond_count == 1;
                } else {
                    diagnostics.set_err(info, crate::msg::ERR57, "");
                    return Err(());
                }
            } else {
                diagnostics.set_err(info, crate::msg::ERR58, "");
                return Err(());
            }
        }
        _ => (),
    }

    Ok(())
}

fn collapse_operation(
    operation: &mut Token,
    declrs: &Vec<Declr>,
    scope: &mut Scope,
    operands: &mut Vec<Prim>,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {

    match operation {
        Token::Operator(op, ref mut typehint, dbginf) => *typehint = Some(op.operate(operands, &dbginf, diagnostics)?),
        Token::Assign(name, ref mut typ, dbginf) => {
            check_var_typ(typ, operands, &dbginf, diagnostics)?;
            scope.decl_var((*name).to_owned(), typ.clone());
        }
        Token::Func(name, dbginf) => {
            call_func(name, declrs, operands, &dbginf, diagnostics)?;
        },
        Token::Keyword(keyword, dbginf) => {
            process_keyword(dbginf, *keyword, scope, operands, diagnostics)?;
        }
        _ => (),
    }

    return Ok(());
}

fn call_func(
    name: &str,
    declrs: &Vec<Declr>,
    operands: &mut Vec<Prim>,
    info: &DebugInfo,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {
    
    // find the function in our function declarations by its name
    for declr in declrs {
        // check if declaration name matches the function name
        if let Some(declr_name) = declr.name {
            if declr_name != name {
                continue;
            }
        } else {
            // some critical compiler error
            crate::message(crate::token::MessageType::Critical, "Missing function name in declaration");
            panic!();
        }

        if let Some(args) = &declr.args {
            // check if we have enough parameter on the stack
            // note that the stack may contain more than just the functions
            // // parameters
            if args.len() > operands.len() {
                diagnostics.set_err(info, crate::msg::ERR60, format!("expected {:?} got {:?}", args.len(), operands.len()));
                return Err(());
            }
                
            // check parameter types
            for (x, arg) in args.iter().enumerate() {
                // parameter are in reverse order on the stack
                // so they are placed at the bottom
                let operand = operands.first().unwrap();
                // check types
                if !operand.is_equal(arg.1) {
                    diagnostics.set_err(info, crate::msg::ERR61, format!("expected {:?} got {:?} as {}th argument", arg, operand, x));
                    return Err(());
                }
               operands.remove(0);
            }
        }

        // if the function returns sth. we will add
        // its type to the stack
        // assuming the execution was successfull
        if let Some(typ) = declr.result_typ {
            operands.push(typ);
        }
        break;
    }
    Ok(())
}

/// parse a single term using a modified shunting yard
fn parse_term<'a>(
    term: &mut VecDeque<Token<'a>>,
    declrs: &Vec<Declr<'a>>,
    scope: &mut Scope,
    diagnostics: &mut data::Diagnostics,
) -> Result<(),()> {

    let mut op_stack = vec![];
    let mut output = VecDeque::with_capacity(term.len());
    let mut value_stack = vec![];

    'outer: while let Some(token) = term.pop_front() {
        match &token {
            // resolve word to either a function, parameter or variable
            Token::Word(text, dbginf) => {
                // test for function
                if is_func(declrs, text) {
                    op_stack.push(Token::Func(text, *dbginf));
                    continue;

                // test for function parameter
                } else if scope.is_arg(text) {
                    value_stack.push(scope.get_arg_type(text));
                    output.push_back(Token::Arg(text, *dbginf));
                    continue;

                // test for variable in scope
                } else if scope.is_var(text).is_some() {
                    value_stack.push(scope.get_var_type(text));
                    output.push_back(Token::Var(text, *dbginf));
                    continue;
                }
                // word was not reolved to anything
                diagnostics.set_err(&token, crate::msg::ERR62, "");
                return Err(());
            }
            Token::Bool(_, _) => {
                output.push_back(token);
                value_stack.push(Prim::Bool)
            }
            Token::Number(_, hint, _) => {
                output.push_back(token.clone());
                value_stack.push(Prim::Num(*hint))
            }
            Token::Assign(_, _, _) => {
                op_stack.push(token);
            }
            Token::Keyword(_, _) => op_stack.push(token),

            Token::Delemiter(char, _) => match char {
                '(' => op_stack.push(token),
                ')' => {
                    while let Some(mut token) = op_stack.pop() {
                        match &token {
                            Token::Delemiter(char, _) => {
                                if *char == '(' {
                                    if let Some(next) = op_stack.last() {
                                        match &next {
                                            Token::Func(_, _) => {
                                                let mut token = op_stack.pop().unwrap();
                                                collapse_operation(
                                                    &mut token,
                                                    declrs,
                                                    scope,
                                                    &mut value_stack,
                                                    diagnostics,
                                                )?;
                                                output.push_back(token);
                                            }
                                            _ => (),
                                        }
                                    }
                                    continue 'outer;
                                }
                            }
                            _ => {
                                collapse_operation(
                                    &mut token,
                                    declrs,
                                    scope,
                                    &mut value_stack,
                                    diagnostics,
                                )?;
                                output.push_back(token)
                            }
                        }
                    }
                    diagnostics.set_err(&token, crate::msg::ERR64, "");
                    return Err(());
                }
                _ => {
                    diagnostics.set_err(&token, crate::msg::ERR65, "");
                    return Err(());
                }
            },

            Token::Operator(op, _, _) => {
                let prec0 = op.prec();
                while let Some(mut top) = op_stack.last_mut() {
                    match &top {
                        Token::Operator(op1, _, _) => {
                            let prec1 = op1.prec();

                            if prec1 > prec0 || prec0 == prec1 && op.assoc() == Assoc::Left {
                                collapse_operation(
                                    &mut top,
                                    declrs,
                                    scope,
                                    &mut value_stack,
                                    diagnostics,
                                )?;
                                output.push_back(op_stack.pop().unwrap());
                                continue;
                            }
                            break;
                        }
                        _ => break,
                    }
                }
                op_stack.push(token);
            }
            _ => (),
        }
    }

    while let Some(mut token) = op_stack.pop() {
        match &token {
            Token::Delemiter(char, _) => {
                if *char == '(' {
                    diagnostics.set_err(&token, crate::msg::ERR63, "");
                    return Err(());
                }
            }
            _ => {
                collapse_operation(&mut token, declrs, scope, &mut value_stack, diagnostics)?;
                output.push_back(token)
            }
        }
    }

    if value_stack.len() > 1 {
        diagnostics.set_err(&output[0], crate::msg::ERR50, "");
        return Err(());
    }

    scope.expr_yield = value_stack.len() == 1;
    if scope.expr_yield {
        let yielded = value_stack.pop().unwrap(); 

        if !yielded.is_equal(scope.func_return_typ.unwrap()) { 

            diagnostics.set_err(
                &output[0],
                crate::msg::ERR59,
                format!(
                    "expected {:?} got {:?}",
                    scope.func_return_typ.unwrap(),
                    yielded
                ),
            );
            return Err(());
        }
    }

    term.append(&mut output);

    Ok(())
}

fn is_func(declrs: &[Declr], text: &str) -> bool {
    for declr in declrs {
        if declr.name.is_some() && declr.name.unwrap() == text {
            return true;
        }
    }
    return false;
}

fn parse_block<'a>(
    block: &mut Block<'a>,
    declrs: &Vec<Declr<'a>>,
    scope: &mut Scope,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {
    scope.cond_count += 1;
    scope.alloc_scope();
    for expr in block.iter_mut() {
        match expr {
            Expr::Block(block) => parse_block(block, declrs, scope, diagnostics)?,
            Expr::Term(term) => parse_term(term, declrs, scope, diagnostics)?,
        }
    }
    scope.pop_scope();
    scope.cond_count -= 1;
    Ok(())
}

fn parse_exprs<'a>(
    funcs: &mut Vec<Func<'a>>,
    declrs: &Vec<Declr<'a>>,
    diagnostics: &mut data::Diagnostics,
) -> Result<(),()> {
    let mut scope = Scope::new();

    for (x, func) in funcs.iter_mut().enumerate() {
        match func.expr.as_mut().expect("Function has no body") {
            Expr::Block(block) => {
                scope.args = declrs[x].args.as_ref();
                scope.func_return_typ = declrs[x].result_typ;
                scope.yields = false;
                scope.cond_count = 0;

                parse_block(block, declrs, &mut scope, diagnostics)?;

                if scope.func_return_typ.is_some() && !scope.yields && !scope.expr_yield {
                    diagnostics.set_err(declrs[x].info.as_ref().unwrap(), crate::msg::ERR56, format!("for function: {}", declrs[x]));
                    return Err(());
                }
            }
            _ => {
                crate::message(crate::token::MessageType::Critical, "Fatal-Compilier-Error: function must have a block");
                panic!();
            },
        }
    }
    Ok(())
}

/// reorder and organize a listing of instructions to a RPN based format:
/// any program is made out of functions.
/// A function has a name followed by an optional parameter list, followed by an optional equal sign and block.
pub fn parse<'a>(tokens: &mut VecDeque<crate::Token<'a>>, diagnostics: &mut data::Diagnostics, settings: &Settings) -> Result<(Vec<Func<'a>>, Vec<Declr<'a>>), ()> {

    let (mut funcs, declrs) = discover_functions(tokens, diagnostics)?;

    discover_exprs(&mut funcs, &declrs, diagnostics)?;
    parse_exprs(&mut funcs, &declrs, diagnostics)?;

    if settings.gen_erpn() {
        crate::inter::convert_to_erpn(&mut funcs, &declrs);
    }

    return Ok((funcs, declrs));   
}
