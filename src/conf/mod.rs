use crate::parser::data::Diagnostics;

#[derive(Default)]
pub struct Settings {
    gen_erpn: bool,
    gen_vsasm: bool,
}

impl Settings {
    pub fn gen_erpn(&self) -> bool {
        self.gen_erpn
    }

    pub fn gen_vsasm(&self) -> bool {
        self.gen_vsasm
    }
}

pub fn parse_args(diagnostics: &mut Diagnostics) -> Settings {
    let args = std::env::args().collect::<Vec<String>>();

    let mut settings = Settings::default();

    for arg in args.iter() {
        match arg.as_str() {
            "--no-info" => diagnostics.set_loglvl(crate::parser::data::LogLvl::Warn),
            "--no-warn" => diagnostics.set_loglvl(crate::parser::data::LogLvl::Err),
            "--erpn" => settings.gen_erpn = true,
            "--vsasm" => settings.gen_vsasm = true,
            "-h" => println!(concat!(
                "help:\n",
                "--no-info: print only warning and errors\n",
                "--no-warn: print only errors\n",
               "--erpn: write a *.erpn (extended reverse polish notation) summary to disk\n",
                "--vsasm: write a .vsasm (virtual simplified assembly language) summary to disk\n",
                "-h: print this dialog"
            )),
            _ => ()
        }
    }

    settings
}
