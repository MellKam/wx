use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use std::time::Instant;

use ast::{Diagnostic, DiagnosticKind, Parser, SpanSingleLinePrinter};
use hir::builder::HIRBuilder;
use mir::builder::MIRBuilder;
use owo_colors::OwoColorize;
use string_interner::StringInterner;
use wasm::builder::WASMBuilder;
use wasm::encoder::WASMEncoder;

use crate::ast::SpanPrinter;

mod ast;
mod hir;
mod mir;
mod wasm;

pub fn print_diagnostic(source: &str, diagnostic: &Diagnostic) {
    let mut printer = SpanSingleLinePrinter::default();
    match diagnostic.kind {
        DiagnosticKind::Error => {
            println!("{}: {}", "error".red().bold(), diagnostic.message);
        }
        DiagnosticKind::Warning => {
            println!("{}: {}", "warning".yellow().bold(), diagnostic.message);
        }
    }
    let _ = printer.print(diagnostic.span.clone(), source);
}

fn main() {
    let source = r#"
        fn add(a: i32, b: i32): i32 {
            return a + b;
        }
    "#;

    let start_time = Instant::now();

    let mut interner = StringInterner::new();
    let diagnostics = Rc::new(RefCell::new(Vec::new()));
    let ast = Parser::parse(source, diagnostics.clone(), &mut interner);

    for diagnostic in diagnostics.borrow().iter() {
        print_diagnostic(source, diagnostic);
    }
    // println!("{:#?}", ast);

    let hir = HIRBuilder::build(&ast, &interner);
    // println!("{:#?}", hir);
    let mir = MIRBuilder::build(&hir);
    println!("{:#?}", mir);
    let wasm = WASMBuilder::build(&mir, &interner);
    let mut file = std::fs::File::create("out.wat").unwrap();
    file.write(wasm.encode_wat().as_bytes()).unwrap();

    let bytecode = WASMEncoder::encode(&wasm);
    let mut file = std::fs::File::create("out.wasm").unwrap();
    file.write(&bytecode).unwrap();
    println!("Wrote {} bytes to out.wasm", bytecode.len());

    let duration = start_time.elapsed();
    println!("Time taken to parse source to MIR: {:?}", duration);
}
