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

mod compile;

#[allow(unused)]
fn test_ir() -> ir::IR {
    use ir::*;

    let print_fn = 2;
    let t = 3;
    let n = 4;
    let out = 5;

    let body = vec![
        Statement::Compute(0, Expr::Num(0.0)),
        Statement::Compute(1, Expr::Num(1.0)),
        Statement::Compute(print_fn, Expr::NativeFn(0)),
        Statement::Compute(t, Expr::NewTable),
        Statement::Compute(n, Expr::Num(22.4)),
        Statement::Store(t, 0, 1),
        Statement::Store(t, 1, n),
        Statement::Compute(out, Expr::FnCall(print_fn, t)),
        Statement::Return(out),
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
    let filename = &args[0];

    let code = std::fs::read_to_string(filename).unwrap();
    let tokens = token::tokenize(&code);
    let ast = parse::parse(&tokens).expect("Ast::parse failed!");
    let ir = lower(&ast);
    // let ir = test_ir();
    // eprintln!("{}", &ir);
    compile::compile(&ir);
    exec_ir::exec(&ir);
}
