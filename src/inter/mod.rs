use std::io::Write;

use crate::data::*;

fn write_expr(file: &mut std::fs::File, indent: &mut String, expr: &Expr) {
    match expr {
        Expr::Block(block) => write_block(file, indent, block),
        Expr::Term(term) => write_term(file, indent,  term)
    }
}

fn write_term(file: &mut std::fs::File, indent: &mut String, term: &std::collections::VecDeque<crate::token::Token>) {
    for token in term.iter() {
        write!(file, "{}{}\n", indent, token).unwrap();
    }
}

fn write_block(file: &mut std::fs::File, indent: &mut String, block: &std::collections::VecDeque<Expr>) {
    indent.push('\t');

    block.iter().for_each(|expr| write_expr(file, indent,  expr));

    indent.pop();
}

pub fn convert_to_erpn<'a>(funcs: &mut Vec<Func<'a>>, declrs: &Vec<Declr<'a>>) {

    let mut file = std::fs::OpenOptions::new().write(true).create(true).truncate(true).open("test.erpn").unwrap();

    let mut indent = String::new();

    for (x, func) in funcs.iter().enumerate() {
        // write function name
        write!(&mut file, "{}:\n", declrs[x].name.unwrap()).unwrap();

        // write down function body
        write_expr(&mut file, &mut indent, func.expr.as_ref().unwrap());
        writeln!(&mut file).unwrap();
    }
}
