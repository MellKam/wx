use std::fs;
use std::io::Write;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self};
use string_interner::StringInterner;
use wx_compiler::*;

fn main() {
    let matches = clap::Command::new("WX Compiler")
        .name("wx-compiler")
        .arg(
            clap::Arg::new("path")
                .help("Path to the source file to compile")
                .required(true)
                .index(1),
        )
        // .arg(
        //     clap::Arg::new("wat")
        //         .long("wat")
        //         .required(false)
        //         .default_value("false")
        //         .action(clap::ArgAction::SetTrue)
        //         .help("Output additional WebAssembly Text Format file"),
        // )
        .author(clap::crate_authors!())
        .version(clap::crate_version!())
        .get_matches();

    let file_path = matches
        .get_one::<String>("path")
        .expect("Path argument is required");

    let file_content = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {}: {}", file_path, e);
            std::process::exit(1);
        }
    };

    let filename = file_path.split('/').last().unwrap().to_string();

    let mut files = ast::Files::new();
    let main_file = files.add(filename.clone(), file_content).unwrap();

    let mut interner = StringInterner::new();
    let ast = ast::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );

    if !ast.diagnostics.is_empty() {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let has_errors = ast.diagnostics.iter().any(|d| {
            matches!(
                d.severity,
                codespan_reporting::diagnostic::Severity::Error
                    | codespan_reporting::diagnostic::Severity::Bug
            )
        });

        for diagnostic in ast.diagnostics.iter() {
            term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
        }

        if has_errors {
            std::process::exit(1);
        }
    }

    let tir = tir::TIR::build(&ast, &mut interner);

    if !tir.diagnostics.is_empty() {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let has_errors = tir.diagnostics.iter().any(|d| {
            matches!(
                d.severity,
                codespan_reporting::diagnostic::Severity::Error
                    | codespan_reporting::diagnostic::Severity::Bug
            )
        });

        for diagnostic in tir.diagnostics.iter() {
            term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
        }

        if has_errors {
            std::process::exit(1);
        }
    }

    let mir = mir::MIR::build(&tir, &interner);
    let module = codegen::Builder::build(&mir).unwrap();
    let bytecode = module.encode();

    let parts = filename.split('.').collect::<Vec<&str>>();
    let filename = parts[0..parts.len() - 1].join(".");
    let mut file = fs::File::create(filename.clone() + ".wasm").unwrap();
    file.write(&bytecode).unwrap();
    println!("Wrote {} bytes to {}.wasm", bytecode.len(), filename);
}
