#![feature(let_else)]
#![feature(box_patterns)]

extern crate llvm_sys as llvm;
use std::ops::Deref;

mod token;
mod ast;
mod parse;

mod exec;
pub use exec::exec;

mod simplify;
use simplify::simplify;

mod ir;
use ir::IR;

// TODO
// mod compile;
// pub use compile::*;

enum Mode {
    Exec,
    Simp,
}

fn default_mode() -> Mode { Mode::Simp }

fn cli() -> Option<(Mode, /*filename: */ String)> {
    let mut args: Vec<String> = std::env::args()
                                .skip(1)
                                .collect();
    let filename = args.pop()?;
    let argsref: Vec<&str> = args.iter().map(|x| x.deref()).collect();
    let mode = match &argsref[..] {
        ["exec"] => Mode::Exec,
        ["simp"] => Mode::Simp,
        [] => default_mode(),
        _ => return None,
    };

    Some((mode, filename))
}

fn usage() {
    println!("usage: lua2llvm [<mode>] <filename>");
    println!("mode ::= simp | exec");
}

fn main() {
    let Some((mode, filename)) = cli() else {
        usage(); 
        std::process::exit(1);
    };

    let code = std::fs::read_to_string(filename).unwrap();
    let tokens = token::tokenize(&code);
    let mut ast = parse::parse(&tokens).expect("Ast::parse failed!");

    if let Mode::Simp = mode {
        ast = simplify(&ast);
    }
    exec(&ast);
}
