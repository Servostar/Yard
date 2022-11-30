use std::collections::{VecDeque, HashMap};

use crate::{parser::data::*, token::{Token, NumHint, Prim}};

#[derive(Debug, Clone, Copy)]
enum Data {
    Int(i64),
    Rat(f64),
    Bool(bool),
    Off(u64),
}

#[derive(Debug)]
enum IntOp {
    CmpEq,
    CmpNEq,
    CmpLt,
    CmpGt,
    CmpLtEq,
    CmpGtEq,

    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug)]
enum RatOp {
    CmpEq,
    CmpNEq,
    CmpLt,
    CmpGt,
    CmpLtEq,
    CmpGtEq,

    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug)]
enum BoolOp {
    CmpEq,
    CmpNEq,
    
    And,
    Or,
    Xor
}

#[derive(Debug)]
enum Operation {
    Int(IntOp),
    Rat(RatOp),
    Bool(BoolOp) 
}

#[derive(Debug)]
enum Instr {
    /// load some data onto the stack
    Push(Data),
    /// delete the last value from stack
    Pop,
    
    /// push the value stored at offset from the local stack onto the local stack
    Load(usize),
    /// store the value stored at the stack[0](offset) stack[1](value) onto the stack
    Store,

    Call(u64),

    Return,

    Operation(Operation),

    Exit
}


/// function stack layout
/// +----------------------------------+
/// | Return address                   |
/// +----------------------------------+
/// | Parameter (0)                    |
/// +----------------------------------+
/// | Parameter (1)                    |
/// +----------------------------------+
/// | Parameter (2)                    |
/// +----------------------------------+
/// | Parameter (n)                    |
/// +----------------------------------+
struct Proc {
    code: Vec<Instr>,
    args: usize,
    addr: u64,
}

#[derive(Default)]
pub struct Program {
    code: HashMap<u64, Proc>
}

#[derive(Default)]
struct Compiletime<'a> {
    vartable: HashMap<&'a str, usize>,
    stacksize: usize,
}

fn parse_term<'a>(term: &VecDeque<Token<'a>>, x: usize, declr: &Vec<Declr<'a>>, ct: &mut Compiletime<'a>, code: &mut Vec<Instr>) -> Result<(), ()>{
    for token in term.iter() {
        let instr = match token {
            Token::Number(value, hint, _) => {
                code.push(Instr::Push(match hint {
                        NumHint::Int => Data::Int(value.parse::<i64>().unwrap()),
                        NumHint::Rat => Data::Rat(value.parse::<f64>().unwrap()),
                    }));
                ct.stacksize += 1;
            },
            Token::Bool(value, _) =>  {
                code.push(Instr::Push(Data::Bool(*value)));
                ct.stacksize += 1;
            },
            Token::Arg(name, _) => {
                let off = declr[x].get_arg_ord(name);

                code.push(Instr::Load(off + 1));
                ct.stacksize += 1;
            },
            Token::Assign(name, _, _) => {
                ct.vartable.insert(name.clone(), ct.stacksize - 1);
            },
            Token::Var(name, _) => {
                code.push(Instr::Load(*ct.vartable.get(name).unwrap()));
                ct.stacksize += 1;
            }
            Token::Operator(op, hint, _) => {
                
                code.push(match op {
                    crate::token::Operator::Or => Instr::Operation(Operation::Bool(BoolOp::Or)),
                    crate::token::Operator::And => Instr::Operation(Operation::Bool(BoolOp::And)),
                    crate::token::Operator::Xor => Instr::Operation(Operation::Bool(BoolOp::Xor)),

                    crate::token::Operator::Add => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::Add)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::Add)),
                        _ => panic!()
                    },
                    crate::token::Operator::Sub => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::Sub)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::Sub)),
                        _ => panic!()
                    },
                    crate::token::Operator::Mul => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::Mul)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::Mul)),
                        _ => panic!()
                    },
                    crate::token::Operator::Div => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::Div)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::Div)),
                        _ => panic!()
                    },
                    
                    crate::token::Operator::Eq => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpEq)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpEq)),
                        Prim::Bool => Instr::Operation(Operation::Bool(BoolOp::CmpEq)),
                        _ => panic!()
                    },
                    crate::token::Operator::NotEq => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpNEq)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpNEq)),
                        Prim::Bool => Instr::Operation(Operation::Bool(BoolOp::CmpNEq)),
                        _ => panic!()
                    },
                    crate::token::Operator::Lt => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpLt)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpLt)),
                        _ => panic!()
                    },
                    crate::token::Operator::Gt => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpGt)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpGt)),
                        _ => panic!()
                    },
                    crate::token::Operator::GtEq => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpGtEq)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpGtEq)),
                        _ => panic!()
                    },
                    crate::token::Operator::LtEq => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpLtEq)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpLtEq)),
                        _ => panic!()
                    },

                    crate::token::Operator::Assign => {
                        crate::message(crate::token::MessageType::Critical, format!("Invalid operator: {:?}", op));
                        return Err(());
                    }
                });

                // TODO: operatiors
                // we pop 2 values as arguments and push 1 as result (-2 + 1 = 1)
                ct.stacksize -= 1;
            },
            Token::Func(name, _) => {
                for decl in declr.iter() {
                    if decl.name.as_ref().unwrap() == name {
                        code.push(Instr::Call(decl.uuid()))
                    }
                }
            }
            Token::Keyword(keyword, _) => {
                match keyword {
                    crate::token::Keyword::Yield | crate::token::Keyword::Return  => code.push(Instr::Return),
                    _ => ()
                }
            }
            

            _ => ()
        };
    }
    Ok(())
}

fn parse_block<'a>(block: &VecDeque<Expr<'a>>, x: usize, declr: &Vec<Declr<'a>>, ct: &mut Compiletime<'a>, prog: &mut Vec<Instr>) -> Result<(), ()> {
    for expr in block.iter() {
        compile_expr(expr, x, declr, ct, prog)?;
    }
    Ok(())
}

fn compile_expr<'a>(expr: &Expr<'a>, x: usize, declr: &Vec<Declr<'a>>, ct: &mut Compiletime<'a>, prog: &mut Vec<Instr>) -> Result<(), ()> {
    match expr {
        Expr::Block(block) => parse_block(block, x, declr, ct, prog)?,
        Expr::Term(term) => parse_term(term, x, declr, ct, prog)?,
    }

    Ok(())
}

pub fn compile<'a>(funcs: &Vec<Func<'a>>, declrs: &Vec<Declr<'a>>) -> Result<Program, ()> {
    let mut prog = Program::default();

    for (x, func) in funcs.iter().enumerate() {
        let mut code = vec![];
        let mut ct = Compiletime::default();

        // at beginn the is the return address and the parameter on the stack
        ct.stacksize = declrs[x].args.as_ref().unwrap_or(&vec![]).len() + 1;

        compile_expr(func.expr.as_ref().unwrap(), x, declrs, &mut ct, &mut code)?;

        println!("{:#?}", code);

        prog.code.insert(declrs[x].uuid(), code);
    }
    Ok(prog)
}

pub fn execute(prog: &Program) -> Result<i64, ()> {
    // declaration of entry function
    let main_fn_declr = crate::parser::data::Declr::main();

    if let Some(main_fn) = prog.code.get(&main_fn_declr.uuid()) {
         
        Ok(0)
    } else {
        crate::message(crate::token::MessageType::Critical, "Program has no main() = int function");
        return Err(());
    }
}
