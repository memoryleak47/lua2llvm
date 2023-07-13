#![feature(box_patterns)]

extern crate llvm_sys as llvm;

mod token;
mod ast;
mod parse;

mod ir;

mod lower;
use lower::lower;

mod exec_ir;

mod compile;

fn test_ir() -> ir::IR {
    use ir::*;

    let body = vec![
        Statement::Compute(0, Expr::Intrinsic(Intrinsic::Throw("ok".to_string()))),
        Statement::Return(0),
    ];

    IR {
        main_fn: 0,
        fns: vec![ LitFunction { body } ]
    }
}

fn main() {
    let args: Vec<String> = std::env::args()
                                .skip(1)
                                .collect();
    let arg = |s| args.iter().find(|x| **x == s).is_some();

    let ir = if arg("--test-ir") {
        test_ir()
    } else {
        let filename = args.iter().find(|x| !x.starts_with("--")).expect("no input file given!");
        let code = std::fs::read_to_string(filename).unwrap();
        let tokens = token::tokenize(&code);
        let ast = parse::parse(&tokens).expect("Ast::parse failed!");

        lower(&ast)
    };

    if arg("--dump-ir") {
        eprintln!("{}", &ir);
    } else if arg("--compile") {
        compile::compile(&ir);
     } else {
        exec_ir::exec(&ir);
    }
}
