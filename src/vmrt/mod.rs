mod output;

use std::collections::{HashMap, VecDeque};

use crate::{
    parser::data::*,
    token::{NumHint, Prim, Token},
};

#[derive(Debug, Clone, Copy)]
enum Data {
    Int(i64),
    Rat(f64),
    Bool(bool),
}

impl std::fmt::Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Int(v) => f.write_fmt(format_args!("int {:#x}", v))?,
            Data::Rat(v) => f.write_fmt(format_args!("float {}", v))?,
            Data::Bool(v) => f.write_fmt(format_args!("bool {}", v))?
        }
        Ok(())
    }
}

impl Data {
    fn to_int(&self) -> Result<i64, ()> {
        return match self {
            Data::Int(v) => Ok(*v),
            _ => Err(()),
        };
    }

    fn to_float(&self) -> Result<f64, ()> {
        return match self {
            Data::Rat(v) => Ok(*v),
            _ => Err(()),
        };
    }

    fn to_bool(&self) -> Result<bool, ()> {
        return match self {
            Data::Bool(v) => Ok(*v),
            _ => Err(()),
        };
    }
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
    Div,
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
    Div,
}

#[derive(Debug)]
enum BoolOp {
    CmpEq,
    CmpNEq,

    And,
    Or,
    Xor,
}

#[derive(Debug)]
enum Operation {
    Int(IntOp),
    Rat(RatOp),
    Bool(BoolOp),
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
    Store(usize),

    Call(u64),

    Return,

    Operation(Operation),

    Jump(usize),
    JumpUnless(usize),

    Exit,
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Pop => f.write_str("\tpop")?,
            Instr::Exit => f.write_str("\texit")?,
            Instr::Return => f.write_str("\tret")?,
            Instr::Load(offset) => f.write_fmt(format_args!("\tload {:#x}", offset))?,
            Instr::Push(value) => f.write_fmt(format_args!("\tpush {}", value))?,
            Instr::Call(uuid) => f.write_fmt(format_args!("\tcall {:#x}", uuid))?,
            Instr::Jump(offset) => f.write_fmt(format_args!("\tjump {:#x}", offset))?,
            Instr::JumpUnless(offset) => f.write_fmt(format_args!("\tjump-unless {:#x}", offset))?,
            Instr::Operation(op) => {
                match op {
                    Operation::Int(op) => match op {
                        IntOp::Add => f.write_str("\tadd int")?,
                        IntOp::Sub => f.write_str("\tsub int")?,
                        IntOp::Mul => f.write_str("\tmul int")?,
                        IntOp::Div => f.write_str("\tdiv int")?,

                        IntOp::CmpEq => f.write_str("\tcmp eq int")?,
                        IntOp::CmpNEq => f.write_str("\tcmp not-eq int")?,
                        IntOp::CmpLt => f.write_str("\tcmp lt eq int")?,
                        IntOp::CmpGt => f.write_str("\tcmp gt int")?,
                        IntOp::CmpLtEq => f.write_str("\tcmp lt-eq int")?,
                        IntOp::CmpGtEq => f.write_str("\tcmp gt-eq int")?,
                    },
                    Operation::Rat(op) => match op {
                        RatOp::Add => f.write_str("\tadd float")?,
                        RatOp::Sub => f.write_str("\tsub float")?,
                        RatOp::Mul => f.write_str("\tmul float")?,
                        RatOp::Div => f.write_str("\tdiv float")?,

                        RatOp::CmpEq => f.write_str("\tcmp eq float")?,
                        RatOp::CmpNEq => f.write_str("\tcmp not-eq float")?,
                        RatOp::CmpLt => f.write_str("\tcmp lt eq float")?,
                        RatOp::CmpGt => f.write_str("\tcmp gt float")?,
                        RatOp::CmpLtEq => f.write_str("\tcmp lt-eq float")?,
                        RatOp::CmpGtEq => f.write_str("\tcmp gt-eq float")?,
                    },
                    Operation::Bool(op) => match op {
                        BoolOp::And => f.write_str("\tand")?,
                        BoolOp::Or => f.write_str("\tor")?,
                        BoolOp::Xor => f.write_str("\txor")?,

                        BoolOp::CmpNEq => f.write_str("\tcmp not-eq bool")?,
                        BoolOp::CmpEq => f.write_str("\tcmp eq bool")?,
                    }
                }
            },
            Instr::Store(offset) => f.write_fmt(format_args!("\tstore {:#x}", offset))?,
        }
        Ok(())
    }
}

/// function stack layout
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
    // executable code
    code: Vec<Instr>,
    // number of expected arguments
    args: usize,
    // hashed declaration is used as "address"
    addr: u64,
}

#[derive(Default)]
pub struct Program {
    procs: HashMap<u64, Proc>,
}

enum LabelType {
    Unless(usize),
    Loop(usize),
    Break(usize),
    Pad
}

#[derive(Default)]
struct Compiletime<'a> {
    vartable: HashMap<&'a str, usize>,
    labels: Vec<LabelType>,
    lopctl: Vec<LabelType>,
    marker: HashMap<&'a str, usize>,
    stacksize: usize,
}

fn parse_term<'a>(
    term: &VecDeque<Token<'a>>,
    x: usize,
    declr: &Vec<Declr<'a>>,
    ct: &mut Compiletime<'a>,
    code: &mut Vec<Instr>,
) -> Result<(), ()> {
    
    for token in term.iter() {
        match token {
            Token::Number(value, hint, _) => {
                code.push(Instr::Push(match hint {
                    NumHint::Int => Data::Int(value.parse::<i64>().unwrap()),
                    NumHint::Rat => Data::Rat(value.parse::<f64>().unwrap()),
                }));
                ct.stacksize += 1;
            }
            Token::Bool(value, _) => {
                code.push(Instr::Push(Data::Bool(*value)));
                ct.stacksize += 1;
            }
            Token::Arg(name, _) => {
                let off = declr[x].get_arg_ord(name);
                code.push(Instr::Load(off));
                ct.stacksize += 1;
            }
            Token::Assign(name, _, _) => {
                if ct.vartable.get(name).is_none() {
                    ct.vartable.insert(name.clone(), ct.stacksize - 1);
                    code.push(Instr::Store(ct.stacksize - 1));
                } else {
                    code.push(Instr::Store(*ct.vartable.get(name).unwrap()));
                }
            }
            Token::Var(name, _) => {
                code.push(Instr::Load(*ct.vartable.get(name).unwrap()));
                ct.stacksize += 1;
            },
            Token::Label(name, _) => {
                ct.marker.insert(name, ct.stacksize + 1);
            },
            Token::Operator(op, hint, _) => {
                code.push(match op {
                    crate::token::Operator::Or => Instr::Operation(Operation::Bool(BoolOp::Or)),
                    crate::token::Operator::And => Instr::Operation(Operation::Bool(BoolOp::And)),
                    crate::token::Operator::Xor => Instr::Operation(Operation::Bool(BoolOp::Xor)),

                    crate::token::Operator::Add => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::Add)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::Add)),
                        _ => panic!(),
                    },
                    crate::token::Operator::Sub => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::Sub)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::Sub)),
                        _ => panic!(),
                    },
                    crate::token::Operator::Mul => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::Mul)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::Mul)),
                        _ => panic!(),
                    },
                    crate::token::Operator::Div => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::Div)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::Div)),
                        _ => panic!(),
                    },

                    crate::token::Operator::Eq => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpEq)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpEq)),
                        Prim::Bool => Instr::Operation(Operation::Bool(BoolOp::CmpEq)),
                        _ => panic!(),
                    },
                    crate::token::Operator::NotEq => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpNEq)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpNEq)),
                        Prim::Bool => Instr::Operation(Operation::Bool(BoolOp::CmpNEq)),
                        _ => panic!(),
                    },
                    crate::token::Operator::Lt => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpLt)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpLt)),
                        _ => panic!(),
                    },
                    crate::token::Operator::Gt => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpGt)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpGt)),
                        _ => panic!(),
                    },
                    crate::token::Operator::GtEq => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpGtEq)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpGtEq)),
                        _ => panic!(),
                    },
                    crate::token::Operator::LtEq => match hint.unwrap() {
                        Prim::Int => Instr::Operation(Operation::Int(IntOp::CmpLtEq)),
                        Prim::Rat => Instr::Operation(Operation::Rat(RatOp::CmpLtEq)),
                        _ => panic!(),
                    },

                    crate::token::Operator::Assign => {
                        crate::message(
                            crate::token::MessageType::Critical,
                            format!("Invalid operator: {:?}", op),
                        );
                        return Err(());
                    }
                });
 
                ct.stacksize -= 1;
            }
            Token::Func(name, _) => {
                for decl in declr.iter() {
                    if decl.name.as_ref().unwrap() == name {
                        code.push(Instr::Call(decl.uuid()));

                        if decl.results {
                            ct.stacksize += 1;
                        }
                    }
                }
            }
            Token::Keyword(keyword, _) => match keyword {
                crate::token::Keyword::Yield | crate::token::Keyword::Return => {
                    code.push(Instr::Return)
                },
                crate::token::Keyword::Loop => {
                    ct.labels.push(LabelType::Loop(code.len()));
                },
                crate::token::Keyword::Unless => {
                    ct.labels.push(LabelType::Unless(code.len()));
                    code.push(Instr::JumpUnless(0));
                },
                crate::token::Keyword::Goto(label) => {
                    let index = ct.marker.get(label).unwrap();
                    code.push(Instr::Jump(*index));
                },
                crate::token::Keyword::Break => {
                    ct.lopctl.push(LabelType::Break(code.len()));
                    code.push(Instr::Jump(0));
                }
                _ => (),
            },
            _ => (),
        };
    }
    Ok(())
}

fn parse_block<'a>(
    block: &VecDeque<Expr<'a>>,
    x: usize,
    declr: &Vec<Declr<'a>>,
    ct: &mut Compiletime<'a>,
    prog: &mut Vec<Instr>,
) -> Result<(), ()> {

    ct.labels.push(LabelType::Pad);

    for expr in block.iter() {
        compile_expr(expr, x, declr, ct, prog)?;
    }
    // resolve JumpUnless Labels
    ct.labels.pop();

    if let Some(label) = ct.labels.pop() {
        match label {
            LabelType::Unless(line) => {
                prog[line] = Instr::JumpUnless(prog.len());
            },
            LabelType::Loop(line) => {
                prog.push(Instr::Jump(line));

                if let Some(label) = ct.lopctl.pop() {
                    match label {
                        LabelType::Break(line) => {
                            prog[line] = Instr::Jump(prog.len());
                        },
                        _ => ()
                    }
                }
            }, 
            _ => ()
        }
    }

    Ok(())
}

fn compile_expr<'a>(
    expr: &Expr<'a>,
    x: usize,
    declr: &Vec<Declr<'a>>,
    ct: &mut Compiletime<'a>,
    prog: &mut Vec<Instr>,
) -> Result<(), ()> {
    match expr {
        Expr::Block(block) => parse_block(block, x, declr, ct, prog)?,
        Expr::Term(term) => parse_term(term, x, declr, ct, prog)?,
    }

    Ok(())
}

fn create_proc(declr: &Declr, code: Vec<Instr>) -> Proc {
    Proc {
        code,
        args: if let Some(args) = &declr.args {
            args.len()
        } else {
            0
        },
        addr: declr.uuid(),
    }
}

pub fn compile<'a>(funcs: &Vec<Func<'a>>, declrs: &Vec<Declr<'a>>, settings: &crate::conf::Settings) -> Result<Program, ()> {
    let mut prog = Program::default();

    for (x, func) in funcs.iter().enumerate() {
        let mut code = vec![];
        let mut ct = Compiletime::default();

        //at the beginning there are all parameters on the stack
        ct.stacksize = declrs[x].args.as_ref().unwrap_or(&vec![]).len();

        compile_expr(func.expr.as_ref().unwrap(), x, declrs, &mut ct, &mut code)?;

        prog.procs
            .insert(declrs[x].uuid(), create_proc(&declrs[x], code));
    }

    if settings.gen_vsasm() {
        output::dump_program(&prog);
    } 

    Ok(prog)
}

struct Runtimestack {
    stack: Vec<Data>,
}

impl Runtimestack {
    pub fn new() -> Runtimestack {
        Self { stack: vec![] }
    }

    pub fn push(&mut self, data: Data) {
        self.stack.push(data);
    }

    pub fn pop(&mut self) -> Option<Data> {
        self.stack.pop()
    }

    pub fn peek(&mut self, index: usize) -> Data {
        self.stack[index]
    }

    pub fn top(&self) -> &Data {
        &self.stack[self.stack.len() - 1]
    }

    pub fn put(&mut self, offset: usize, value: Data)  {
        self.stack[offset] = value;
    }
}

fn call_fn(prog: &Program, proc: &Proc, superstack: &[Data]) -> Result<Option<Data>, ()> {
    let mut stack = Runtimestack::new();

    // build local procedure stack
    for i in 0..proc.args {
        stack.push(superstack[superstack.len() - i - 1].clone());
    }
    
    let mut x = 0;
    while x < proc.code.len() {
        match &proc.code[x] {
            Instr::Pop => {
                stack.pop();
            }
            Instr::Return => {
                return Ok(stack.pop());
            }
            Instr::Push(data) => stack.push(*data),
            Instr::Load(offset) => {
                let v = stack.peek(*offset);
                stack.push(v);
            },
            Instr::Store(offset) => {
                let data = stack.top().clone();
                stack.put(*offset, data);
            },
            Instr::Call(addr) => {
                if let Some(value) = call_fn(prog, prog.procs.get(addr).unwrap(), &stack.stack)? {
                    stack.push(value);
                }
            },
            Instr::Jump(addr) => {
                x = *addr - 1;
            },
            Instr::JumpUnless(addr) => {
                match stack.pop() {
                    Some(Data::Bool(true)) => x = *addr - 1,
                    Some(Data::Bool(false)) => (),
                    _ => {
                        crate::message(crate::token::MessageType::Critical, "no condition for unless on stack");
                        panic!();
                    }
                }
            },
            Instr::Operation(op) => {
                let op0:Data =  if let Some(data) = stack.pop() {
                    data
                } else {
                    return Err(());
                };
                let op1:Data = if let Some(data) = stack.pop() {
                    data
                } else {
                    return Err(());
                };

                match op {
                    Operation::Int(op) => match op {
                        IntOp::Add => stack.push(Data::Int(op1.to_int()? + op0.to_int()?)),
                        IntOp::Sub => stack.push(Data::Int(op1.to_int()? - op0.to_int()?)),
                        IntOp::Mul => stack.push(Data::Int(op1.to_int()? * op0.to_int()?)),
                        IntOp::Div => stack.push(Data::Int(op1.to_int()? / op0.to_int()?)),

                        IntOp::CmpEq => {
                            stack.push(Data::Bool(op1.to_int()? == op0.to_int()?))
                        }
                        IntOp::CmpNEq => {
                            stack.push(Data::Bool(op1.to_int()? != op0.to_int()?))
                        }
                        IntOp::CmpLt => stack.push(Data::Bool(op1.to_int()? < op0.to_int()?)),
                        IntOp::CmpGt => stack.push(Data::Bool(op1.to_int()? > op0.to_int()?)),
                        IntOp::CmpGtEq => {
                            stack.push(Data::Bool(op1.to_int()? <= op0.to_int()?))
                        }
                        IntOp::CmpLtEq => {
                            stack.push(Data::Bool(op1.to_int()? >= op0.to_int()?))
                        }
                    },
                    Operation::Rat(op) => match op {
                        RatOp::Add => {
                            stack.push(Data::Rat(op1.to_float()? + op0.to_float()?))
                        }
                        RatOp::Sub => {
                            stack.push(Data::Rat(op1.to_float()? - op0.to_float()?))
                        }
                        RatOp::Mul => {
                            stack.push(Data::Rat(op1.to_float()? * op0.to_float()?))
                        }
                        RatOp::Div => {
                            stack.push(Data::Rat(op1.to_float()? / op0.to_float()?))
                        }

                        RatOp::CmpEq => {
                            stack.push(Data::Bool(op1.to_float()? == op0.to_float()?))
                        }
                        RatOp::CmpNEq => {
                            stack.push(Data::Bool(op1.to_float()? != op0.to_float()?))
                        }
                        RatOp::CmpLt => {
                            stack.push(Data::Bool(op1.to_float()? < op0.to_float()?))
                        }
                        RatOp::CmpGt => {
                            stack.push(Data::Bool(op1.to_float()? > op0.to_float()?))
                        }
                        RatOp::CmpGtEq => {
                            stack.push(Data::Bool(op1.to_float()? <= op0.to_float()?))
                        }
                        RatOp::CmpLtEq => {
                            stack.push(Data::Bool(op1.to_float()? >= op0.to_float()?))
                        }
                    },
                    Operation::Bool(op) => match op {
                        BoolOp::Or => {
                            stack.push(Data::Bool(op1.to_bool()? || op0.to_bool()?))
                        }
                        BoolOp::And => {
                            stack.push(Data::Bool(op1.to_bool()? && op0.to_bool()?))
                        }
                        BoolOp::Xor => {
                            stack.push(Data::Bool(op1.to_bool()? ^ op0.to_bool()?))
                        }
                        BoolOp::CmpEq => {
                            stack.push(Data::Bool(op1.to_bool()? == op0.to_bool()?))
                        }
                        BoolOp::CmpNEq => {
                            stack.push(Data::Bool(op1.to_bool()? != op0.to_bool()?))
                        }
                    },
                }
            }
            _ => (),
        }
        x += 1;
    }

    Ok(stack.pop())
}

pub fn execute(prog: &Program) -> Result<i64, ()> {
    // declaration of entry function
    let main_fn_declr = crate::parser::data::Declr::main();

    if let Some(main_fn) = prog.procs.get(&main_fn_declr.uuid()) {

        if let Some(exit_code) = call_fn(prog, main_fn, &[])? {
            return Ok(exit_code.to_int()?);
        }

        crate::message(crate::token::MessageType::Critical, "main procedure did not return exit code");
        return Err(());
    } else {
        crate::message(
            crate::token::MessageType::Error,
            "Program has no main() = int function",
        );
        return Err(());
    }
}
