use std::fs;
use std::io::Write;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self};
use string_interner::StringInterner;
use wx_compiler::files::Files;
use wx_compiler::{compile_module, encode_module};

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

    let mut files = Files::new();
    let main_file = files.add(filename.clone(), file_content).unwrap();

    let mut interner = StringInterner::new();
    let result = compile_module(&mut interner, &files, main_file);

    let module = match result.module {
        Some(module) => module,
        None => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();

            for diagnostic in result.diagnostics.iter() {
                term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
            }

            std::process::exit(1);
        }
    };

    let bytecode = encode_module(&module);
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
