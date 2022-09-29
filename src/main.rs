#![feature(let_else)]

extern crate llvm_sys as llvm;

mod ast;
pub use ast::*;

mod exec;
pub use exec::*;

mod compile;
pub use compile::*;

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    let code = std::fs::read_to_string(filename).unwrap();
    let ast = parse(&code).unwrap();
    compile(&ast);
}
