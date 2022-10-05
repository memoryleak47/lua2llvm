#![feature(let_else)]
#![feature(box_patterns)]

extern crate llvm_sys as llvm;

mod token;
mod ast;
mod parse;

mod exec;
pub use exec::exec;

// TODO re-add
// mod ir;
// use ir::IR;

// TODO
// mod compile;
// pub use compile::*;

fn main() {
    let mut exec_flag = false;
    let args: Vec<String> = std::env::args().skip(1).map(|x| {
        if x == "--exec" {
            exec_flag = true;
            None
        } else {
            Some(x)
        }
    }).flatten()
    .collect();

    if let [filename] = &args[..] {
        let code = std::fs::read_to_string(filename).unwrap();
        let tokens = token::tokenize(&code);
        let ast = parse::parse(&tokens).expect("Ast::parse failed!");

        if exec_flag {
            exec(&ast);
        } else {
            // TODO
            // let ir = IR::lower(&ast);
            // compile(&ast);
        }
    } else {
        println!("usage: lua2llvm <filename>");
        println!("       lua2llvm --exec <filename>");
    }
}
