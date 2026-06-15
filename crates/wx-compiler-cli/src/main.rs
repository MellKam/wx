use std::collections::HashMap;
use std::fs;
use std::io::Write;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self};
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
        .author(clap::crate_authors!())
        .version(clap::crate_version!())
        .get_matches();

    let file_path = matches
        .get_one::<String>("path")
        .expect("Path argument is required");

    let filename = file_path.split('/').last().unwrap().to_string();

    let mut builder = vfs::CompilationGraphBuilder::new();
    let stdlib_id = builder
        .load_crate(
            "std.wx".to_string(),
            &vfs::VirtualFileSource::new(HashMap::from([(
                "std.wx".to_string(),
                STDLIB_SOURCE.to_string(),
            )])),
        )
        .unwrap();
    let mut compilation = match builder.load_crate(file_path.clone(), &vfs::NativeFileSource) {
        Ok(root_id) => builder.build(root_id, stdlib_id),
        Err(vfs::LoadError::ReadFailed { path }) => {
            eprintln!("Error reading file {path}");
            std::process::exit(1);
        }
        Err(_) => {
            eprintln!("Failed to load compilation");
            std::process::exit(1);
        }
    };

    let ast_diagnostics: Vec<_> = compilation
        .crates
        .iter()
        .flat_map(|crate_graph| crate_graph.diagnostics.iter())
        .collect();

    if !ast_diagnostics.is_empty() {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let has_errors = ast_diagnostics.iter().any(|d| {
            matches!(
                d.severity,
                codespan_reporting::diagnostic::Severity::Error
                    | codespan_reporting::diagnostic::Severity::Bug
            )
        });

        for diagnostic in ast_diagnostics {
            term::emit_to_write_style(&mut writer.lock(), &config, &compilation.files, diagnostic)
                .unwrap();
        }

        if has_errors {
            std::process::exit(1);
        }
    }

    let tir = tir::TIR::build(&mut compilation);

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
            term::emit_to_write_style(&mut writer.lock(), &config, &compilation.files, diagnostic)
                .unwrap();
        }

        if has_errors {
            std::process::exit(1);
        }
    }

    let mir = mir::MIR::build(&tir, &compilation.interner, compilation.id_generator);
    let module = codegen::Builder::build(&mir, &compilation.interner).unwrap();
    let bytecode = module.encode();

    let parts = filename.split('.').collect::<Vec<&str>>();
    let filename = parts[0..parts.len() - 1].join(".");
    let mut file = fs::File::create(filename.clone() + ".wasm").unwrap();
    file.write(&bytecode).unwrap();
    println!("Wrote {} bytes to {}.wasm", bytecode.len(), filename);
}
