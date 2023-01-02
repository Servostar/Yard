
pub mod conv;
pub mod math;
pub mod io;
pub mod os;

#[derive(PartialEq, Eq)]
pub enum Module {
    Conv,
    Math,
    IO,
    OS,
}

impl Module {
    
    pub fn from_list(text: &str) -> Vec<Module> {
        let mut vec = vec![];

        for word in text.split(',') {
            match word.trim() {
                "conv" => vec.push(Module::Conv),
                "math" => vec.push(Module::Math),
                "io" => vec.push(Module::IO),
                "os" => vec.push(Module::OS),
                _ => crate::message(crate::token::MessageType::Warning, format!("unknown module: `{}`", word))
            }
        }

        vec
    }
}

pub fn get_submodules<'a>(modules: &[Module]) -> Vec<super::BuiltinFun> {
    let mut funs = vec![];

    for module in modules {
        match module {
            Module::Conv => funs.append(&mut conv::get_module_funs()),
            Module::Math => funs.append(&mut math::get_module_funs()),
            Module::IO => funs.append(&mut io::get_module_funs()),
            Module::OS => funs.append(&mut os::get_module_funs()),
        }
    }

    funs
}