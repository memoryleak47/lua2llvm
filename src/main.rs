#![feature(let_else)]
#![feature(box_patterns)]

extern crate llvm_sys as llvm;

mod token;
mod ast;
mod parse;

mod ir;

mod lower;
use lower::lower;

mod exec_ir;

// TODO
// mod compile;

fn main() {
    let args: Vec<String> = std::env::args()
                                .skip(1)
                                .collect();
    let filename = &args[0];

    let code = std::fs::read_to_string(filename).unwrap();
    let tokens = token::tokenize(&code);
    let ast = parse::parse(&tokens).expect("Ast::parse failed!");
    let ir = lower(&ast);
    eprintln!("{}", &ir);
    //compile::compile(&ir);
    exec_ir::exec(&ir);
}
