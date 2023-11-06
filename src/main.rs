#![feature(box_patterns)]
#![feature(fmt_internals)]
#![feature(type_alias_impl_trait)]
#![feature(try_blocks)]

extern crate llvm_sys as llvm;

mod token;
mod ast;
mod parse;
mod visit;

mod ir;
mod display;
use display::{ir_to_string, infer_to_string};

mod prepare;
use prepare::prepare;

mod lower;
use lower::lower;

mod infer;
use infer::infer;

mod exec_ir;

mod compile;

mod optimize;
use optimize::optimize;

fn main() {
    let args: Vec<String> = std::env::args()
                                .skip(1)
                                .collect();
    let arg = |s| args.iter().find(|x| **x == s).is_some();

    let filename = args.iter().find(|x| !x.starts_with("--")).expect("no input file given!");
    let code = std::fs::read_to_string(filename).unwrap();

    let mut ir = if arg("--ir") {
        ir::parser::parse_ir(&code)
    } else {
        let tokens = token::tokenize(&code);
        let mut ast = parse::parse(&tokens).expect("Ast::parse failed!");

        prepare(&mut ast);

        lower(&ast)
    };

    let mut inf = infer(&ir);

    optimize(&mut ir, &mut inf);

    if arg("--dump-ir") {
        eprintln!("{}", ir_to_string(&ir));
    } else if arg("--dump-infer") {
        eprintln!("{}", infer_to_string(&ir, &inf));
    } else if arg("--compile") {
        compile::compile(&ir);
    } else {
        exec_ir::exec(&ir);
    }
}
