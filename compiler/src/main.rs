use std::io::Write;
use std::time::Instant;

use ast::Parser;
use codespan_reporting::diagnostic::Severity;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use files::Files;
use indoc::indoc;
use string_interner::StringInterner;

mod ast;
mod files;
mod hir;
// mod mir;
mod span;
// mod wasm;

// fn x() {
//     let x = 2 == 2;

//     let x = {
//         if false {
//             break 1;
//         };
//         break 2;
//     } + 4;
// }

fn main() {
    let start_time = Instant::now();
    let mut interner = StringInterner::new();

    let mut files = Files::new();
    let main_file_id = files
        .add(
            "main.wax".to_string(),
            indoc! { r#"
            func exponent(base: i32, exp: i32): i32 {
                if exp == 0 { return 1 };
                if exp < 0 { return 0 };

                local mut result: i32 = 1;
                loop {
                    // if exp == 0 { break result };
                    result *= base;
                    exp -= 1;
                }
            }

            export func main(): i32 {
                exponent(2, 3)
            }
            "# }
            .to_string(),
        )
        .unwrap();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let ast = {
        let (ast, diagnostics) = Parser::parse(
            main_file_id,
            files.get(main_file_id).unwrap().source.as_ref(),
            &mut interner,
        );
        // println!("{:#?}", ast);
        let diagnostics = diagnostics
            .into_iter()
            .map(|d| d.to_diagnostic())
            .collect::<Vec<_>>();

        for diagnostic in diagnostics.iter() {
            term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
        }

        if diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error || d.severity == Severity::Bug)
        {
            std::process::exit(1);
        }

        ast
    };

    let hir = {
        let (hir, diagnostics) = hir::Builder::build(&ast, &mut interner);
        // println!("{:#?}", hir);
        for diagnostic in diagnostics.iter() {
            term::emit(
                &mut writer.lock(),
                &config,
                &files,
                &diagnostic.clone().to_diagnostic(),
            )
            .unwrap();
        }

        hir
    };

    // let mir = {
    //     let mir = mir::Builder::build(&hir);
    //     // println!("{:#?}", mir);

    //     mir
    // };

    // let wasm_module = wasm::Builder::build(&mir, &interner);
    // let bytecode = wasm::Encoder::encode(&wasm_module);

    let duration = start_time.elapsed();
    println!("Time to compile: {:?}", duration);

    // let mut file = std::fs::File::create("out.wat").unwrap();
    // file.write(wasm_module.to_wat().as_bytes()).unwrap();

    // let mut file = std::fs::File::create("out.wasm").unwrap();
    // file.write(&bytecode).unwrap();
    // println!("Wrote {} bytes to out.wasm", bytecode.len());
}
