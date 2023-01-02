use std::io::Write;

fn print_to_file(file: &mut std::fs::File, prog: &crate::vmrt::Program) -> std::io::Result<()> {
    writeln!(file, "section[text]")?;
    for proc in prog.procs.iter() {
        if proc.1.builtin.is_some() {
            writeln!(file, "\nextern builtin {:#x}", proc.0)?;
            continue;
        }

        writeln!(file, "\n{:#x}:", proc.0)?;

        for instr in proc.1.code.iter() {
            writeln!(file, "{}", instr)?;
        }
    }
    Ok(())
}

pub fn dump_program(prog: &crate::vmrt::Program) {
    let mut file = std::fs::OpenOptions::new().write(true).create(true).truncate(true).open("prog.vsasm").unwrap();

    if print_to_file(&mut file, prog).is_err() {
        crate::message(crate::token::MessageType::Critical, "Failed dumping program");
        panic!();       
    }
}
