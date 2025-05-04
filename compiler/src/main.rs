use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use std::time::Instant;

use ast::{Files, Parser};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
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
    let start_time = Instant::now();
    let mut interner = StringInterner::new();
    let diagnostics = Rc::new(RefCell::new(Vec::new()));

    let mut files = Files::new();
    let std_file_id = files
        .add(
            "std.wax",
            indoc! { r#"
            enum bool: i32 {
                false = 0,
                true = 1,
            }
            "# },
        )
        .unwrap();
    let main_file_id = files
        .add(
            "main.wax",
            indoc! { r#"
            export fn main(): bool { 
                const x: bool = bool::false;
                x == bool::false
            }
            "# },
        )
        .unwrap();

    let std_ast = Parser::parse(
        std_file_id,
        files.get(std_file_id).unwrap().source.as_ref(),
        diagnostics.clone(),
        &mut interner,
    );
    // println!("{:#?}", std_ast);
    let main_ast = Parser::parse(
        main_file_id,
        files.get(main_file_id).unwrap().source.as_ref(),
        diagnostics.clone(),
        &mut interner,
    );

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    for diagnostic in diagnostics.borrow().iter() {
        let _ = term::emit(
            &mut writer.lock(),
            &config,
            &files,
            &diagnostic.to_diagnostic(),
        )
        .unwrap();
    }

    let hir = hir::builder::Builder::build(&main_ast, &std_ast, &interner);
    // println!("{:#?}", hir);
    let mir = MIRBuilder::build(&hir);
    // println!("{:#?}", mir);
    let wasm = WASMBuilder::build(&mir, &interner);
    println!("{:#?}", wasm);
    let mut file = std::fs::File::create("out.wat").unwrap();
    file.write(wasm.encode_wat().as_bytes()).unwrap();

    let bytecode = WASMEncoder::encode(&wasm);
    let mut file = std::fs::File::create("out.wasm").unwrap();
    file.write(&bytecode).unwrap();
    println!("Wrote {} bytes to out.wasm", bytecode.len());

    let duration = start_time.elapsed();
    println!("Time to compile: {:?}", duration);
}
