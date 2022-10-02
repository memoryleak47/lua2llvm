#![feature(let_else)]

extern crate llvm_sys as llvm;

mod ast;
use ast::Ast;

mod ir;
use ir::IR;


// TODO(WIP): these modules don't currently work.
// mod exec;
// pub use exec::*;

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
        let ast = Ast::parse(&code).unwrap();

        if exec_flag { // TODO re-enable
            // exec(&ast);
        } else {
            // compile(&ast);
        }
    } else {
        println!("usage: lua2llvm <filename>");
        println!("       lua2llvm --exec <filename>");
    }
}
