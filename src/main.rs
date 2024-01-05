#![feature(box_patterns)]
#![feature(fmt_internals)]
#![feature(type_alias_impl_trait)]
#![feature(try_blocks)]

extern crate llvm_sys as llvm;

mod token;
mod ast;
mod parse;
mod visit;

mod hir;

mod ir;
mod display;
use display::*;

mod prepare;
use prepare::prepare;

mod lower;
use lower::lower;

mod infer;
use infer::infer;

mod exec_hir;

mod compile;

mod optimize;
use optimize::optimize;

mod normalize;
use normalize::normalize;

mod ll;

fn print_help() {
    println!("lua2llvm [OPTION]... [FILE]");
    println!("FILE should be a Lua file. Without any further arguments, lua2llvm will parse the lua file, lower it to IR, optimize it, and run it using the IR-interpreter.\n");
    println!("--ir: the FILE contains IR code, instead of Lua");
    println!("--no-optimize: apply no optimizations");
    println!("--no-normalize: apply no normalizations");
    println!("--dump-ir: Instead of running the program, we will emit IR code");
    println!("--dump-infer: Instead of running the program, run inference and print the output");
    println!("--compile: Instead of running the program, we will emit LLVM IR code");
    println!("--help: shows this");
}

fn main() {
    let args: Vec<String> = std::env::args()
                                .skip(1)
                                .collect();
    let arg = |s| args.iter().find(|x| **x == s).is_some();

    if arg("--help") {
        print_help();
        return;
    }

    let filename = args.iter().find(|x| !x.starts_with("--")).expect("no input file given!");
    let code = std::fs::read_to_string(filename).unwrap();

    let hir = if arg("--hir") {
        hir::parser::parse_hir(&code)
    } else {
        let tokens = token::tokenize(&code);
        let mut ast = parse::parse(&tokens).expect("Ast::parse failed!");

        prepare(&mut ast);

        lower(&ast)
    };

    let mut ir = hir::lower_hir(&hir);

    if !arg("--no-optimize") {
        optimize(&mut ir);
    }
    if !arg("--no-normalize") {
        normalize(&mut ir);
    }

    let inf = infer(&ir);
    ir::layout(&mut ir, &inf);

    if arg("--dump-hir") {
        eprintln!("{}", &hir);
    } else if arg("--dump-ir") {
        eprintln!("{}", &ir);
    } else if arg("--dump-infer") {
        eprintln!("{}", infer_to_string(&ir, &inf));
    } else if arg("--compile") {
        compile::compile(&ir, &inf);
    } else {
        exec_hir::exec(&hir);
    }
}
