use std::fs;
use std::io::Write;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self};
use string_interner::StringInterner;
use wx_compiler::*;

fn main() {
    let matches = clap::Command::new("WX Compiler")
        .name("wxc")
        .arg(
            clap::Arg::new("path")
                .help("Path to the source file to compile")
                .required(true)
                .index(1),
        )
        .arg(
            clap::Arg::new("wat")
                .long("wat")
                .required(false)
                .default_value("false")
                .action(clap::ArgAction::SetTrue)
                .help("Output additional WebAssembly Text Format file"),
        )
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

    let mut files = files::Files::new();
    let main_file = files.add(filename.clone(), file_content).unwrap();

    let mut interner = StringInterner::new();
    let (ast, diagnostics) = ast::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );

    if !diagnostics.is_empty() {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for diagnostic in diagnostics.iter() {
            term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
        }
        std::process::exit(1);
    }

    let (hir, diagnostics) = hir::Builder::build(&ast, &mut interner);

    if !diagnostics.is_empty() {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for diagnostic in diagnostics.iter() {
            term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
        }
        std::process::exit(1);
    }

    let mir = mir::Builder::build(&hir);
    let module = wasm::Builder::build(&mir, &mut interner);
    let bytecode = wasm::Encoder::encode(&module);

    let parts = filename.split('.').collect::<Vec<&str>>();
    let filename = parts[0..parts.len() - 1].join(".");
    let mut file = fs::File::create(filename.clone() + ".wasm").unwrap();
    file.write(&bytecode).unwrap();
    println!("Wrote {} bytes to out.wasm", bytecode.len());

    if matches.get_flag("wat") {
        let wat = module.to_wat();
        let mut file = fs::File::create(filename + ".wat").unwrap();
        file.write(wat.as_bytes()).unwrap();
        println!("Wrote {} bytes to out.wat", wat.len());
    }
}
