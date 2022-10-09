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

mod lower;
use lower::lower;

// TODO
// mod compile;
// pub use compile::*;

enum Mode {
    Exec,
    Simp,
    Lower,
}

fn default_mode() -> Mode { Mode::Lower }

fn cli() -> Option<(Mode, /*filename: */ String)> {
    let mut args: Vec<String> = std::env::args()
                                .skip(1)
                                .collect();
    let filename = args.pop()?;
    let argsref: Vec<&str> = args.iter().map(|x| x.deref()).collect();
    let mode = match &argsref[..] {
        ["exec"] => Mode::Exec,
        ["simp"] => Mode::Simp,
        ["lower"] => Mode::Lower,
        [] => default_mode(),
        _ => return None,
    };

    Some((mode, filename))
}

fn usage() {
    println!("usage: lua2llvm [<mode>] <filename>");
    println!("mode ::= simp | exec | lower");
}

fn main() {
    let Some((mode, filename)) = cli() else {
        usage(); 
        std::process::exit(1);
    };

    let code = std::fs::read_to_string(filename).unwrap();
    let tokens = token::tokenize(&code);
    let ast = parse::parse(&tokens).expect("Ast::parse failed!");

    match mode {
        Mode::Simp => {
            let ast = simplify(&ast);
            exec(&ast);
        },
        Mode::Exec => {
            exec(&ast);
        },
        Mode::Lower => {
            let ir = lower(&ast);
            dbg!(ir);
        },
    }
}
