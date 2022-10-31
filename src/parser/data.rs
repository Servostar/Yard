use core::panic;
use std::collections::{VecDeque};
use crate::token::{Token};
use crate::Prim;

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
    pub result_typ: Option<Prim>
}

impl<'a> Declr<'a> {
    pub fn new() -> Self {
        Self {
            name: None,
            args: None,
            results: false,
            result_typ: None
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

        f.write_str(" {}")
    }
}

pub type Block<'a> = VecDeque<Expr<'a>>;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    /// group of more expressions
    Block(Block<'a>),
    /// single term
    Term(VecDeque<Token<'a>>)
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
    pub cond_count: usize
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

    pub fn get_arg_type(&self, name: &'a str) -> Prim  {
        if let Some(args) = self.args {
            for arg in args.iter() {
                if arg.0 == name {
                    return arg.1;
                }
            }
        }
        panic!("No argument of name: {name}");
    }

    pub fn get_var_type(&self, name: &'a str) -> Prim  {
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
}