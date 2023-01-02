use crate::{token::Prim, parser::data::{Declr}, vmrt::Data, builtin::BuiltinFun};

pub fn get_os(_: &[Data]) -> Result<Option<Data>, ()> {
    
    if cfg!(windows) {
        return Ok(Some(Data::Str(String::from("Windows"))))
    } else if cfg!(unix) {
        return Ok(Some(Data::Str(String::from("UNIX"))))
    }

    Ok(None)
}

pub fn get_module_funs<'a>() -> Vec<crate::builtin::BuiltinFun> {
    vec![
        BuiltinFun {
            declr: Declr::generate_builtin("get_os", vec![], Some(Prim::Str)),
            func: &get_os
        },
    ]
}