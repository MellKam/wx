#![feature(allocator_api)]

use std::io::Write;
use std::time::Instant;

use ast::Parser;
use bumpalo::Bump;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use files::Files;
use indoc::indoc;
use string_interner::StringInterner;

mod ast;
mod files;
mod hir;
mod span;
// mod mir;
// mod wasm;

fn main() {
    let start_time = Instant::now();
    let mut interner = StringInterner::new();

    let bump = Bump::new();

    let mut files = Files::new();
    let main_file_id = files
        .add(
            "main.wax".to_string(),
            indoc! { r#"
            fn main(): i32 {
                5 < 10 && 5 > 20
            }
            "# }
            .to_string(),
        )
        .unwrap();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let ast = {
        let (ast, diagnostics) = Parser::parse(
            &bump,
            main_file_id,
            files.get(main_file_id).unwrap().source.as_ref(),
            &mut interner,
        );
        println!("{:#?}", ast);

        for diagnostic in diagnostics.iter() {
            term::emit(
                &mut writer.lock(),
                &config,
                &files,
                &diagnostic.clone().to_diagnostic(),
            )
            .unwrap();
        }

        ast
    };

    // let hir = {
    //     let (hir, diagnostics) = hir::Builder::build(&ast, &interner);
    //     // println!("{:#?}", hir);
    //     for diagnostic in diagnostics.iter() {
    //         term::emit(
    //             &mut writer.lock(),
    //             &config,
    //             &files,
    //             &diagnostic.clone().to_diagnostic(),
    //         )
    //         .unwrap();
    //     }

    //     hir
    // };

    // let mir = mir::Builder::build(&hir);
    // // println!("{:#?}", mir);
    // let wasm = wasm::Builder::build(&mir, &interner);
    // println!("{:#?}", wasm);
    // let mut file = std::fs::File::create("out.wat").unwrap();
    // file.write(wasm.to_wat().as_bytes()).unwrap();

    // let bytecode = wasm::Encoder::encode(&wasm);
    // let mut file = std::fs::File::create("out.wasm").unwrap();
    // file.write(&bytecode).unwrap();
    // println!("Wrote {} bytes to out.wasm", bytecode.len());

    let duration = start_time.elapsed();
    println!("Time to compile: {:?}", duration);
}
