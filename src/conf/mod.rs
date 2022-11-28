use crate::parser::data::Diagnostics;

pub struct Settings {
    gen_erpn: bool
}

impl Settings {
    fn new() -> Self {
        Settings {
            gen_erpn: false
        }
    }

    pub fn gen_erpn(&self) -> bool {
        self.gen_erpn
    }
}

pub fn parse_args(diagnostics: &mut Diagnostics) -> Settings {
    let args = std::env::args().collect::<Vec<String>>();

    let mut settings = Settings::new();

    for arg in args.iter() {
        match arg.as_str() {
            "--no-info" => diagnostics.set_loglvl(crate::parser::data::LogLvl::Warn),
            "--no-warn" => diagnostics.set_loglvl(crate::parser::data::LogLvl::Err),
            "--erpn" => settings.gen_erpn = true,
            _ => ()
        }
    }

    settings
}
