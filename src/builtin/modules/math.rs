use crate::{token::Prim, parser::data::{Declr}, vmrt::Data, builtin::BuiltinFun};

pub fn cos(args: &[Data]) -> Result<Option<Data>, ()> {
    Ok(Some(Data::Rat(args[0].to_float()?.cos())))
}

pub fn sin(args: &[Data]) -> Result<Option<Data>, ()> {
    Ok(Some(Data::Rat(args[0].to_float()?.sin())))
}

pub fn tan(args: &[Data]) -> Result<Option<Data>, ()> {
    Ok(Some(Data::Rat(args[0].to_float()?.tan())))
}

pub fn ln(args: &[Data]) -> Result<Option<Data>, ()> {
    Ok(Some(Data::Rat(args[0].to_float()?.ln())))
}

pub fn sqrt(args: &[Data]) -> Result<Option<Data>, ()> {
    Ok(Some(Data::Rat(args[0].to_float()?.sqrt())))
}

pub fn pow(args: &[Data]) -> Result<Option<Data>, ()> {
    let x = args[0].to_float()?;
    let y = args[1].to_float()?;

    Ok(Some(Data::Rat(y.powf(x))))
}

pub fn get_module_funs<'a>() -> Vec<crate::builtin::BuiltinFun> {
    vec![
        BuiltinFun {
            declr: Declr::generate_builtin("cos", vec![("x", Prim::Int)], Some(Prim::Rat)),
            func: &cos
        },
        BuiltinFun {
            declr: Declr::generate_builtin("pow", vec![("x", Prim::Rat), ("y", Prim::Rat)], Some(Prim::Rat)),
            func: &pow
        },
        BuiltinFun {
            declr: Declr::generate_builtin("sin", vec![("x", Prim::Int)], Some(Prim::Rat)),
            func: &sin
        },
        BuiltinFun {
            declr: Declr::generate_builtin("tan", vec![("x", Prim::Int)], Some(Prim::Rat)),
            func: &tan
        },
        BuiltinFun {
            declr: Declr::generate_builtin("ln", vec![("x", Prim::Int)], Some(Prim::Rat)),
            func: &ln
        },
        BuiltinFun {
            declr: Declr::generate_builtin("sqrt", vec![("x", Prim::Int)], Some(Prim::Rat)),
            func: &sqrt
        },
    ]
}