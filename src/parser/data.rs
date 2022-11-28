use crate::token::{DebugInfo, DebugNotice, Token, MessageType};
use crate::Prim;
use core::panic;
use std::collections::VecDeque;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LogLvl {
    /// print everything
    Info,
    /// print only errors and warning
    Warn,
    /// print only errors
    Err,
}

pub struct Diagnostics<'a> {
    /// terminating factor on error
    err: Option<DebugNotice<'a>>,
    /// additional warning and informations
    /// all non critical
    hints: Vec<DebugNotice<'a>>,
    /// source string
    source: &'a str,
    /// flags
    loglvl: LogLvl,
}

impl<'a> Diagnostics<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            err: None,
            hints: vec![],
            source,
            loglvl: LogLvl::Info
        }
    }

    pub fn set_loglvl(&mut self, lvl: LogLvl) {
        self.loglvl = lvl;
    }

    pub fn set_err<T, S>(&mut self, source: &S, message: &'static crate::token::DebugMsg, ext: T)
    where
        T: Into<String>,
        S: Into<DebugInfo> + Clone,
    {
        if self.err.is_some() {
            panic!("Error already set");
        }

        let info: DebugInfo = source.clone().into();

        self.err = Some(DebugNotice {
            info,
            msg: message,
            ext: ext.into(),
            source: self.source,
        });
    }

    pub fn hint<T, S>(&mut self, source: &S, message: &'static crate::token::DebugMsg, ext: T)
    where
        T: Into<String>,
        S: Into<DebugInfo> + Clone,
    {
        let info: DebugInfo = source.clone().into();

        self.hints.push(DebugNotice {
            info,
            msg: message,
            ext: ext.into(),
            source: self.source,
        });
    }
}

impl<'a> std::fmt::Display for Diagnostics<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for hint in self.hints.iter() {
            match hint.msg.typ {
                MessageType::Info => if self.loglvl != LogLvl::Info {
                    continue;
                },
                MessageType::Warning => if self.loglvl == LogLvl::Err {
                    continue;
                },
                _ => (),
            }

            f.write_fmt(format_args!("{}\n\n", hint)).unwrap();
        }

        if let Some(err) = self.err.as_ref() {
            f.write_fmt(format_args!("{}\n\n", err)).unwrap();
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Func<'a> {
    /// raw tokens
    pub raw: Option<VecDeque<Token<'a>>>,
    /// parsed content
    pub expr: Option<Expr<'a>>,
}

impl<'a> Func<'a> {
    pub fn new() -> Self {
        Self {
            raw: None,
            expr: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declr<'a> {
    /// name of this function
    pub name: Option<&'a str>,
    /// parameter names
    pub args: Option<Vec<(&'a str, Prim)>>,
    /// if the function returns a single value
    pub results: bool,
    /// type of result
    pub result_typ: Option<Prim>,

    /// debug info
    pub info: Option<DebugInfo>
}

impl<'a> Declr<'a> {
    pub fn new() -> Self {
        Self {
            name: None,
            args: None,
            results: false,
            result_typ: None,
            info: None,
        }
    }
}

impl<'a> std::fmt::Display for Declr<'a> {
    /// print this functions declaration in the form of ```foo(x,y) = {}```
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", &self.name.unwrap()))?;

        // format the arguments
        if let Some(args) = &self.args {
            f.write_str("(")?;
            for (x, arg) in args.iter().enumerate() {
                if x == 0 {
                    f.write_fmt(format_args!("{}", arg.0))?;
                    continue;
                }

                f.write_fmt(format_args!(", {}", arg.0))?;
            }
            f.write_str(")")?;
        }

        if self.results {
            f.write_str(" =")?;
        }

        if let Some(typ) = self.result_typ {
            f.write_fmt(format_args!(" {:?}", typ))?;
        }

        f.write_str(" {}")
    }
}

pub type Block<'a> = VecDeque<Expr<'a>>;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    /// group of more expressions
    Block(Block<'a>),
    /// single term
    Term(VecDeque<Token<'a>>),
}

pub struct Scope<'a> {
    pub args: Option<&'a Vec<(&'a str, Prim)>>,
    /// stack of scoped block variables
    pub vars: Vec<Vec<(String, Option<Prim>)>>,
    pub func_return_typ: Option<Prim>,
    /// if we safely yielded sth
    pub yields: bool,
    /// if the last expr yielded a result
    pub expr_yield: bool,
    pub cond_count: usize,
}

impl<'a> Scope<'a> {
    pub fn alloc_scope(&mut self) {
        self.vars.push(Vec::new())
    }

    pub fn pop_scope(&mut self) {
        self.vars.pop();
    }

    pub fn decl_var(&mut self, name: String, typ: Option<Prim>) {
        self.vars.last_mut().unwrap().push((name, typ))
    }

    pub fn is_arg(&self, name: &'a str) -> bool {
        if let Some(args) = self.args {
            for arg in args.iter() {
                if arg.0 == name {
                    return true;
                }
            }
        }
        false
    }

    pub fn get_arg_type(&self, name: &'a str) -> Prim {
        if let Some(args) = self.args {
            for arg in args.iter() {
                if arg.0 == name {
                    return arg.1;
                }
            }
        }
        panic!("No argument of name: {name}");
    }

    pub fn get_var_type(&self, name: &'a str) -> Prim {
        // create an owned version of the string
        let owned = &name.to_owned();

        for vars in self.vars.iter() {
            for var in vars.iter() {
                if &var.0 == owned {
                    return var.1.expect("Untyped variable");
                }
            }
        }
        panic!("No variable of name: {name}");
    }

    pub fn is_var(&self, name: &'a str) -> Option<Prim> {
        // create an owned version of the string
        let owned = &name.to_owned();

        // search
        for vars in self.vars.iter() {
            for var in vars.iter() {
                if &var.0 == owned {
                    return var.1;
                }
            }
        }
        None
    }

    pub fn new<'b>() -> Scope<'b> {
        Scope {
            args: None,
            vars: vec![],
            func_return_typ: None,
            expr_yield: false,
            yields: false,
            cond_count: 0,
        }
    }
}
