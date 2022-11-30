use std::io::Write;

fn print_to_file(file: &mut std::fs::File, prog: &crate::vmrt::Program) -> std::io::Result<()> {
    for proc in prog.procs.iter() {
        writeln!(file, "\n0x{:#x}:", proc.0)?;

        for instr in proc.1.code.iter() {
            writeln!(file, "{}", instr)?;
        }
    }
    Ok(())
}

pub fn dump_program(prog: &crate::vmrt::Program) {
    let mut file = std::fs::OpenOptions::new().write(true).create(true).truncate(true).open("prog.vsasm").unwrap();

    if print_to_file(&mut file, prog).is_err() {
        
    }
}
