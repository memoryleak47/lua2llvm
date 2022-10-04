#![feature(let_else)]
#![feature(box_patterns)]

extern crate llvm_sys as llvm;

mod ast;
use ast::Ast;

// TODO re-add
// mod ir;
// use ir::IR;

// mod exec;
// pub use exec::exec;

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
        let ast = Ast::parse(&code).expect("Ast::parse failed!");
        dbg!(&ast);
        // TODO
        // let ir = IR::lower(&ast);

        if exec_flag {
            // exec(&ir);
        } else {
            // compile(&ast);
        }
    } else {
        println!("usage: lua2llvm <filename>");
        println!("       lua2llvm --exec <filename>");
    }
}
