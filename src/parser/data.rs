use rand::RngCore;

use crate::conf::Settings;
use crate::token::{DebugInfo, DebugNotice, Token, MessageType};
use crate::Prim;
use core::panic;
use std::collections::VecDeque;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LogLvl {
    /// print everything
    Info,
    /// print only errors and warning
    Warn,
    /// print only errors
    Err,
}

impl Default for LogLvl {
    fn default() -> Self {
        Self::Info
    }
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
    pub fn new(settings: &Settings, source: &'a str) -> Diagnostics<'a> {
        Self {
            err: None,
            hints: vec![],
            source,
            loglvl: settings.loglvl()
        }
    }

    pub fn set_err<T, S>(&mut self, source: &S, message: &'static crate::token::DebugMsg, ext: T)
    where
        T: Into<String>,
        S: Into<DebugInfo> + Clone,
    {
        if self.err.is_some() {
            crate::message(MessageType::Warning, "Multiple Errors occured during compilation");
            return;
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

#[derive(Debug, Clone)]
pub struct Func<'a> {
    /// raw tokens
    pub raw: Option<VecDeque<Token<'a>>>,
    /// parsed content
    pub expr: Option<Expr<'a>>,
    
    pub is_builtin:bool,
}

impl<'a> Func<'a> {
    pub fn new() -> Self {
        Self {
            raw: None,
            expr: None,
            is_builtin: false
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
    pub info: Option<DebugInfo>,

    pub is_builtin: bool,

    uuid: u64
}

impl<'a> Declr<'a> {

    pub fn generate_builtin(name: &'static str, args: Vec<(&'static str, Prim)>, ret: Option<Prim>) -> Declr {
        Declr {
            name: Some(name),
            args: if args.is_empty() {
                None
            } else {
                Some(args)
            }, 
            results: ret.is_some(), 
            result_typ: ret, 
            info: None, 
            is_builtin: true,
            uuid: rand::thread_rng().next_u64()
        }
    }

    pub fn new() -> Self {
        Self {
            name: None,
            args: None,
            results: false,
            result_typ: None,
            info: None,
            is_builtin: false,
            uuid: 0,
        }
    }

    pub fn main() -> Self {
        let mut main = Declr {
            name: Some("main"),
            args: None,
            results: true,
            result_typ: Some(Prim::Int),
            info: None,
            is_builtin: false,
            uuid: 0
        };

        main.gen_uuid();

        main
    }

    pub fn get_arg_ord(&self, name: &str) -> usize {
        if let Some(args) = self.args.as_ref() {
            for (x, arg) in args.iter().enumerate() {
                if arg.0 == name {
                    return x;
                }
            }
        }
        return 0;
    }

    pub fn gen_uuid(&mut self) {
        let mut hasher = DefaultHasher::default();

        self.name.unwrap().hash(&mut hasher);

        if let Some(args) = self.args.as_ref() {
            for arg in args.iter() {
                arg.0.hash(&mut hasher);
                arg.1.hash(&mut hasher);
            }
        }

        self.uuid = hasher.finish()
    }

    pub fn uuid(&self) -> u64 {
        self.uuid
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

pub struct Scope {
    pub declr: usize,
    /// stack of scoped block variables
    pub vars: Vec<Vec<(String, Option<Prim>)>>,
    /// if we safely yielded sth
    pub yields: bool,
    /// if the last expr yielded a result
    pub expr_yield: bool,
    pub cond_count: usize,
}

impl Scope {
    pub fn alloc_scope(&mut self) {
        self.vars.push(Vec::new())
    }

    pub fn pop_scope(&mut self) {
        self.vars.pop();
    }

    pub fn decl_var(&mut self, name: String, typ: Option<Prim>) {
        self.vars.last_mut().unwrap().push((name, typ))
    }

    pub fn is_arg(&self, name: &str, declrs: &Vec<Declr>) -> bool {
        if let Some(args) = &declrs[self.declr].args {
            for arg in args.iter() {
                if arg.0 == name {
                    return true;
                }
            }
        }
        false
    }

    pub fn get_arg_type(&self, name: &str, declrs: &Vec<Declr>) -> Prim {
        if let Some(args) = &declrs[self.declr].args {
            for arg in args.iter() {
                if arg.0 == name {
                    return arg.1;
                }
            }
        }
        panic!("No argument of name: {name}");
    }

    pub fn get_var_type(&self, name: &str) -> Prim {
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

    pub fn is_var(&self, name: &str) -> Option<Prim> {
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

    pub fn new() -> Scope {
        Scope {
            declr: 0,
            vars: vec![],
            expr_yield: false,
            yields: false,
            cond_count: 0,
        }
    }
}
