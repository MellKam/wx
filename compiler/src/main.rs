use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use std::time::Instant;

use ast::{Parser, Report};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use hir::builder::Builder;
use indoc::indoc;
use mir::builder::MIRBuilder;
use string_interner::StringInterner;
use wasm::builder::WASMBuilder;
use wasm::encoder::WASMEncoder;

mod ast;
mod hir;
mod mir;
mod wasm;

fn main() {
    let source = indoc! { r#"
        // This is a comment
        fn add(a: i32, b: i32): i32 {
            return a + b;
        }

        fn main(): i32 {
            // every value must be used or dropped
            _ = add(2, 2); 

            // const for deeply immutable values
            const result: i32 = add(2, 2);

            // mut for mutable values
            mut x: i32 = 0;
            x = x + 5;

            return result;
        }
    "# };

    let start_time = Instant::now();

    let mut interner = StringInterner::new();
    let diagnostics = Rc::new(RefCell::new(Vec::new()));
    let ast = Parser::parse(source, diagnostics.clone(), &mut interner);

    let file = SimpleFile::new("add.wx", source);

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    // println!("Diagnostics: {:#?}", diagnostics.borrow());
    for diagnostic in diagnostics.borrow().iter() {
        let _ = term::emit(&mut writer.lock(), &config, &file, &diagnostic.report()).unwrap();
    }
    // println!("{:#?}", ast);

    let hir = Builder::build(&ast, &interner);
    // println!("{:#?}", hir);
    let mir = MIRBuilder::build(&hir);
    // println!("{:#?}", mir);
    let wasm = WASMBuilder::build(&mir, &interner);
    let mut file = std::fs::File::create("out.wat").unwrap();
    file.write(wasm.encode_wat().as_bytes()).unwrap();

    let bytecode = WASMEncoder::encode(&wasm);
    let mut file = std::fs::File::create("out.wasm").unwrap();
    file.write(&bytecode).unwrap();
    // println!("Wrote {} bytes to out.wasm", bytecode.len());

    let duration = start_time.elapsed();
    println!("Time taken to parse source to MIR: {:?}", duration);
}
