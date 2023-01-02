use crate::{token::Prim, parser::data::{Declr}, vmrt::Data, builtin::BuiltinFun};

pub fn int_to_rat(args: &[Data]) -> Result<Option<Data>, ()> {
    match args [0] {
        Data::Int(val) => Ok(Some(Data::Rat(val as f64))),
        _ => Err(())
    }
}

pub fn rat_to_int(args: &[Data]) -> Result<Option<Data>, ()> {
    match args[0] {
        Data::Rat(val) => Ok(Some(Data::Int(val as i64))),
        _ => Err(())
    }
}

pub fn str_to_int(args: &[Data]) -> Result<Option<Data>, ()> {
    match &args[0] {
        Data::Str(val) => Ok(Some(Data::Int(val.parse().unwrap_or(0)))),
        _ => Err(())
    }
}

pub fn str_to_rat(args: &[Data]) -> Result<Option<Data>, ()> {
    match &args[0] {
        Data::Str(val) => Ok(Some(Data::Rat(val.parse().unwrap_or(0.0)))),
        _ => Err(())
    }
}

pub fn get_module_funs<'a>() -> Vec<crate::builtin::BuiltinFun> {
    vec![
        BuiltinFun {
            declr: Declr::generate_builtin("to_rat", vec![("x", Prim::Int)], Some(Prim::Rat)),
            func: &int_to_rat
        },
        BuiltinFun {
            declr: Declr::generate_builtin("to_int", vec![("x", Prim::Rat)], Some(Prim::Int)),
            func: &rat_to_int
        },
        BuiltinFun {
            declr: Declr::generate_builtin("parse_int", vec![("text", Prim::Str)], Some(Prim::Int)),
            func: &str_to_int
        },
        BuiltinFun {
            declr: Declr::generate_builtin("parse_rat", vec![("text", Prim::Str)], Some(Prim::Rat)),
            func: &str_to_rat
        }
    ]
}