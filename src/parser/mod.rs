use crate::{token::{Assoc, DebugInfo, Keyword, Operator, Prim, Token}, conf::Settings, builtin::{BuiltinFun, get_builtin_funs}, direct::LangSpecs};
use core::panic;
use std::{collections::VecDeque, vec};

pub mod data;
pub mod msg;

use data::*;

pub struct Parser<'a> {
    pub declrs: Vec<Declr<'a>>,
    pub funcs: Vec<Func<'a>>,
    pub scope: Scope,

    pub builtin: Vec<BuiltinFun>,
}

impl<'a> Parser<'a> {

    pub fn new(specs: &LangSpecs) -> Parser<'a> {
        let builtin = get_builtin_funs(specs.features());

        Self { 
            declrs: builtin.iter().map(|bf| bf.declr()).collect(),
            funcs: builtin.iter().map(|bf| bf.func()).collect(),
            scope: Scope::new(),
            builtin
        }
    }


/// simple brace-counting parser to detect functions
fn discover_functions(
    &mut self,
    tokens: &mut VecDeque<crate::Token<'a>>,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {
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
            if self.declrs.contains(&declr) {
                diagnostics.set_err(
                    $token,
                    crate::msg::ERR10,
                    format!("Multiple definitions: {declr}"),
                );
                return Err(());
            } 

            // check if the function returns sth but no return value is given
            if declr.results &&declr.result_typ.is_none() {
                diagnostics.set_err($token, crate::msg::ERR11, format!("for function {declr}"));
                return Err(());
            }

            declr.gen_uuid();

            // store new function and its declaration
            self.funcs.push(func);
            self.declrs.push(declr);

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
                    // then check if we even need to return Err(()) sth.
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
                    // check if we already marked a return Err(()) type
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
                    return Err(()) ;
                }
            }
            continue;
        }

        // if we have anything left it might be an error
        match &top {
            Token::LineBreak(_) | Token::Terminator(_) => (), // valid whitespace
            _ => {
                diagnostics.set_err(&top, crate::msg::ERR22, "");
                return Err(()) ;
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

    Ok(())
}

/// parse the functions raw content to expr for easy compilation using a brace-counter.
/// - ```{...}``` surround a block
/// - line breaks seperate expressions
fn discover_exprs(
    &mut self,
    diagnostics: &mut data::Diagnostics,
 ) -> Result<(), ()> {

    for func in self.funcs.iter_mut() {
        if func.is_builtin {
            continue;
        }

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
                            return Err(()) ;
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
                                return Err(()) ;
                            }
                        } else {
                            diagnostics.set_err(&top, crate::msg::ERR40, "");
                            return Err(()) ;
                        }
    
                        if blocks.is_empty() {
                            diagnostics.set_err(&top, crate::msg::ERR41, "");
                            return Err(()) ;
                        }

                        continue;
                    },
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
                return Err(()) ;
            }
        }

        if let Some(block) = blocks.pop() {
            func.expr = Some(Expr::Block(block));
        }
    }

    return Ok(())
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
        return Err(()) ;
    }

    Ok(())
}

fn process_keyword(
    &mut self,
    info: &DebugInfo,
    keyword: Keyword,
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
                return Err(()) ;
            }

            if let Some(operand) = operands.pop() {
                match operand {
                    Prim::Bool => (),
                    _ => {
                        diagnostics.set_err(info, crate::msg::ERR53, format!("got {:?}", operand));
                        return Err(()) ;
                    }
                }
            }
        }
        Keyword::Return => {
            if self.declrs[self.scope.declr].result_typ.is_some() {
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
                if let Some(typ) = self.declrs[self.scope.declr].result_typ {
                    if !typ.is_equal(operand) {
                        diagnostics.set_err(
                            info,
                            crate::msg::ERR59,
                            format!("expected {:?} got {:?}", typ, operand),
                        );
                        return Err(());
                    }
                    self.scope.yields = self.scope.cond_count == 1;
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
    &mut self,
    operation: &mut Token,
    operands: &mut Vec<Prim>,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {

    match operation {
        Token::Operator(op, ref mut typehint, dbginf) => *typehint = Some(op.operate(operands, &dbginf, diagnostics)?),
        Token::Assign(name, ref mut typ, dbginf) => {
            Self::check_var_typ(typ, operands, &dbginf, diagnostics)?;
            self.scope.decl_var((*name).to_owned(), typ.clone());
        }
        Token::Func(name, dbginf) => {
            self.call_func(name, operands, &dbginf, diagnostics)?;
        },
        Token::Keyword(keyword, dbginf) => {
            self.process_keyword(dbginf, *keyword, operands, diagnostics)?;
        }
        _ => (),
    }

    return Ok(());
}

fn call_func(
    &mut self,
    name: &str,
    operands: &mut Vec<Prim>,
    info: &DebugInfo,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {
    
    // find the function in our function declarations by its name
    for declr in self.declrs.iter() {
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
                // fetch next operand which is ontop of the stack
                let operand = operands.last().unwrap();
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
fn parse_term(
    &mut self,
    term: &mut VecDeque<Token<'a>>,
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
                if self.is_func(&self.declrs, text) || self.is_func(&self.declrs, text){
                    op_stack.push(Token::Func(text, *dbginf));
                    continue;

                // test for function parameter
                } else if self.scope.is_arg(text, &self.declrs) {
                    value_stack.push(self.scope.get_arg_type(text, &self.declrs));
                    output.push_back(Token::Arg(text, *dbginf));
                    continue;

                // test for variable in scope
                } else if self.scope.is_var(text).is_some() {
                    value_stack.push(self.scope.get_var_type(text));
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
            },
            Token::String(_, _) => {
                output.push_back(token.clone());
                value_stack.push(Prim::Str)
            }
            Token::Assign(_, _, _) => {
                op_stack.push(token);
            }
            Token::Label(_, _) => output.push_back(token),
            Token::Keyword(key, _) => {
                match key {
                    Keyword::While => output.push_back(Token::LoopStart),
                    _ => ()
                }
                op_stack.push(token)
            },

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
                                                self.collapse_operation(
                                                    &mut token,
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
                                self.collapse_operation(
                                    &mut token,
                                    &mut value_stack,
                                    diagnostics,
                                )?;
                                output.push_back(token)
                            }
                        }
                    }
                    diagnostics.set_err(&token, crate::msg::ERR64, "");
                    return  Err(());
                }
                _ => {
                    diagnostics.set_err(&token, crate::msg::ERR65, "");
                    return  Err(());
                }
            },

            Token::Operator(op, _, _) => {
                let prec0 = op.prec();
                while let Some(mut top) = op_stack.last_mut() {
                    match &top {
                        Token::Operator(op1, _, _) => {
                            let prec1 = op1.prec();

                            if prec1 > prec0 || prec0 == prec1 && op.assoc() == Assoc::Left {
                                self.collapse_operation(
                                    &mut top,
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
                self.collapse_operation(&mut token, &mut value_stack, diagnostics)?;
                output.push_back(token)
            }
        }
    }

    if value_stack.len() > 1 {
        diagnostics.set_err(&output[0], crate::msg::ERR50, "");
        return Err(());
    }

    self.scope.expr_yield = value_stack.len() == 1;
    if self.scope.expr_yield {
        let yielded = value_stack.pop().unwrap(); 

        if !yielded.is_equal(self.declrs[self.scope.declr].result_typ.unwrap()) { 

            diagnostics.set_err(
                &output[0],
                crate::msg::ERR59,
                format!(
                    "expected {:?} got {:?}",
                    self.declrs[self.scope.declr].result_typ.unwrap(),
                    yielded
                ),
            );
            return Err(());
        }
    }

    term.append(&mut output);

    Ok(())
}

fn is_func(&self, declrs: &[Declr], text: &str) -> bool {
    for declr in declrs {
        if declr.name.is_some() && declr.name.unwrap() == text {
            return true;
        }
    }
    return false;
}

fn parse_block(
    &mut self,
    block: &mut Block<'a>,
    diagnostics: &mut data::Diagnostics,
) -> Result<(), ()> {
    self.scope.cond_count += 1;
    self.scope.alloc_scope();
    for expr in block.iter_mut() {
        match expr {
            Expr::Block(block) => self.parse_block(block,  diagnostics)?,
            Expr::Term(term) => self.parse_term(term,  diagnostics)?,
        }
    }
    self.scope.pop_scope();
    self.scope.cond_count -= 1;
    Ok(())
}

fn parse_exprs(
    &mut self,
    diagnostics: &mut data::Diagnostics,
) -> Result<(),()> {

    for x in 0..self.funcs.len() {
        if self.declrs[x].is_builtin {
            continue;
        }

        let mut block = match self.funcs[x].expr.as_mut().expect("Function has no body") {
            Expr::Block(block) => {
                block.clone()
            }
            _ => {
                crate::message(crate::token::MessageType::Critical, "Fatal-Compiler-Error: function must have a block");
                panic!();
            },
        };

        self.scope.declr = x;
        self.scope.yields = false;
        self.scope.cond_count = 0;

        self.parse_block(&mut block, diagnostics)?;

        self.funcs[x].expr = Some(Expr::Block(block));

        if self.declrs[self.scope.declr].result_typ.is_some() && !self.scope.yields && !self.scope.expr_yield {
            diagnostics.set_err(self.declrs[x].info.as_ref().unwrap(), crate::msg::ERR56, format!("for function: {}", self.declrs[x]));
            return Err(());
        }
    }
    Ok(())
}

/// reorder and organize a listing of instructions to a RPN based format:
/// any program is made out of functions.
/// A function has a name followed by an optional parameter list, followed by an optional equal sign and block.
pub fn parse(&'a mut self, tokens: &mut VecDeque<crate::Token<'a>>, diagnostics: &mut data::Diagnostics, settings:  &Settings) -> Result<(&mut Vec<Func>, &mut Vec<Declr>, &mut Vec<BuiltinFun>), ()> {

    self.discover_functions(tokens, diagnostics)?;

    self.discover_exprs(diagnostics)?;
    self.parse_exprs(diagnostics)?;

    if settings.gen_erpn() {
        crate::inter::convert_to_erpn(self);
    }

    return Ok((&mut self.funcs, &mut self.declrs, &mut self.builtin));
}
}