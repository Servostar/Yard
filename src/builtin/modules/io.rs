use crate::{token::Prim, parser::data::{Declr}, vmrt::Data, builtin::BuiltinFun};

pub fn print(args: &[Data]) -> Result<Option<Data>, ()> {
    let text = args[0].to_str()?;
    print!("{}", text);
    Ok(None)
}

pub fn println(args: &[Data]) -> Result<Option<Data>, ()> {
    let text = args[0].to_str()?;
    println!("{}", text);
    Ok(None)
}

pub fn get_module_funs<'a>() -> Vec<crate::builtin::BuiltinFun> {
    vec![
        BuiltinFun {
            declr: Declr::generate_builtin("print", vec![("text", Prim::Str)], None),
            func: &print
        },
        BuiltinFun {
            declr: Declr::generate_builtin("println", vec![("text", Prim::Str)], None),
            func: &println
        },
    ]
}