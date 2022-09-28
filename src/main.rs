#![feature(let_else)]

extern crate llvm_sys as llvm;

mod ast;
use ast::Ast;

mod exec;

mod compile;

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    let code = std::fs::read_to_string(filename).unwrap();
    let ast = Ast::parse(&code).unwrap();
    ast.compile();
}
