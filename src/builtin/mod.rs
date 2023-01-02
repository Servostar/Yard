/// module for builtin functons
/// and the SENI-module
use crate::{parser::data::{Declr, Func}, vmrt::Data};

use self::modules::Module;

pub mod modules;

type Function = &'static dyn Fn(&[Data]) -> Result<Option<Data>, ()>;

#[derive(Clone)]
pub struct BuiltinFun {
    declr: Declr<'static>,
    func: Function
}

impl BuiltinFun {
    
    pub fn declr(&self) -> Declr<'static> {
        self.declr.clone()
    }

    pub fn func(&self) -> crate::parser::data::Func<'static> {
        Func {
            expr: None,
            is_builtin: true,
            raw: None
        }
    }

    pub fn get_function(&self) -> Function {
        self.func
    }
}

pub fn get_builtin_funs(mods: &[Module]) -> Vec<BuiltinFun> {
    let mut funs = vec![
        BuiltinFun {
            declr: Declr::generate_builtin("say_hello", vec![], None),
            func: &say_hello
        },
    ];

    funs.append(&mut modules::get_submodules(mods));

    return funs;
}

fn say_hello(_args: &[Data]) -> Result<Option<Data>, ()> {
    println!("hello!");
    Ok(None)
}