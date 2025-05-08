use std::io::Write;
use std::time::Instant;

use ast::Parser;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use files::Files;
use indoc::indoc;
use string_interner::StringInterner;

mod ast;
mod files;
mod hir;
mod mir;
mod span;
mod wasm;

fn main() {
    let start_time = Instant::now();
    let mut interner = StringInterner::new();

    let mut files = Files::new();
    let main_file_id = files
        .add(
            "main.wax".to_string(),
            indoc! { r#"
            enum bool: i32 {
                false = 0,
                true = 1,
            }

            export fn main(): bool { 
                const x: bool = bool::false;
                x == bool::false
            }
            "# }
            .to_string(),
        )
        .unwrap();

    let (ast, diagnostics) = Parser::parse(
        main_file_id,
        files.get(main_file_id).unwrap().source.as_ref(),
        &mut interner,
    );

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    for diagnostic in diagnostics.iter() {
        term::emit(
            &mut writer.lock(),
            &config,
            &files,
            &diagnostic.clone().to_diagnostic(),
        )
        .unwrap();
    }

    let (hir, diagnostics) = hir::Builder::build(&ast, &interner);
    for diagnostic in diagnostics.iter() {
        term::emit(
            &mut writer.lock(),
            &config,
            &files,
            &diagnostic.clone().to_diagnostic(),
        )
        .unwrap();
    }
    let mir = mir::Builder::build(&hir);
    // println!("{:#?}", mir);
    let wasm = wasm::Builder::build(&mir, &interner);
    println!("{:#?}", wasm);
    let mut file = std::fs::File::create("out.wat").unwrap();
    file.write(wasm.encode_wat().as_bytes()).unwrap();

    let bytecode = wasm::Encoder::encode(&wasm);
    let mut file = std::fs::File::create("out.wasm").unwrap();
    file.write(&bytecode).unwrap();
    println!("Wrote {} bytes to out.wasm", bytecode.len());

    let duration = start_time.elapsed();
    println!("Time to compile: {:?}", duration);
}
