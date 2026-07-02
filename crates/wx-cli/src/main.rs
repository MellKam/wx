use std::fs;
use std::io::Write;

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::term;
use codespan_reporting::term::DisplayStyle;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use wx_compiler::*;

fn main() {
	let diag_style_arg = clap::Arg::new("diagnostics")
		.long("diagnostics")
		.value_name("STYLE")
		.value_parser(clap::builder::PossibleValuesParser::new([
			clap::builder::PossibleValue::new("rich")
				.help("Full source preview with context (default)"),
			clap::builder::PossibleValue::new("medium")
				.help("Line number and notes"),
			clap::builder::PossibleValue::new("short")
				.help("One line per diagnostic"),
		]))
		.default_value("rich")
		.help("Diagnostic display style");

	let matches = clap::Command::new("wx")
		.name("wx")
		.author(clap::crate_authors!())
		.version(clap::crate_version!())
		.subcommand_required(true)
		.arg_required_else_help(true)
		.subcommand(
			clap::Command::new("compile")
				.about("Compile a WX source file to WebAssembly")
				.arg(clap::Arg::new("path").required(true).index(1))
				.arg(diag_style_arg.clone()),
		)
		.subcommand(
			clap::Command::new("check")
				.about("Type-check a WX source file without emitting output")
				.arg(clap::Arg::new("path").required(true).index(1))
				.arg(diag_style_arg),
		)
		.subcommand(
			clap::Command::new("format")
				.about("Format a WX source file in-place")
				.arg(clap::Arg::new("path").required(true).index(1)),
		)
		.get_matches();

	match matches.subcommand() {
		Some(("compile", sub)) => {
			let path = sub.get_one::<String>("path").unwrap();
			let style = parse_display_style(
				sub.get_one::<String>("diagnostics").unwrap(),
			);
			cmd_compile(path, style);
		}
		Some(("check", sub)) => {
			let path = sub.get_one::<String>("path").unwrap();
			let style = parse_display_style(
				sub.get_one::<String>("diagnostics").unwrap(),
			);
			cmd_check(path, style);
		}
		Some(("format", sub)) => {
			cmd_format(sub.get_one::<String>("path").unwrap())
		}
		_ => unreachable!(),
	}
}

fn parse_display_style(s: &str) -> DisplayStyle {
	match s {
		"medium" => DisplayStyle::Medium,
		"short" => DisplayStyle::Short,
		_ => DisplayStyle::Rich,
	}
}

fn load_compilation(file_path: &str) -> vfs::CompilationGraph {
	let mut builder = vfs::CompilationGraphBuilder::new();
	let stdlib_id = builder.load_stdlib();
	match builder.load_binary(file_path.to_string(), &vfs::NativeFileSource) {
		Ok(root_id) => builder.build(root_id, stdlib_id),
		Err(()) => {
			eprintln!("error: cannot read file '{file_path}'");
			std::process::exit(1);
		}
	}
}

/// Emits diagnostics to stderr. Returns `true` if any errors were reported.
fn emit_diagnostics(
	compilation: &vfs::CompilationGraph,
	diagnostics: &[codespan_reporting::diagnostic::Diagnostic<vfs::FileId>],
	style: DisplayStyle,
) -> bool {
	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = term::Config {
		display_style: style,
		..term::Config::default()
	};
	let mut has_errors = false;
	for d in diagnostics {
		if matches!(d.severity, Severity::Error | Severity::Bug) {
			has_errors = true;
		}
		term::emit_to_write_style(
			&mut writer.lock(),
			&config,
			&compilation.files,
			d,
		)
		.unwrap();
	}
	has_errors
}

fn cmd_compile(file_path: &str, style: DisplayStyle) {
	let mut compilation = load_compilation(file_path);

	for crate_graph in &compilation.crates {
		if emit_diagnostics(
			&compilation,
			&crate_graph.diagnostics,
			style.clone(),
		) {
			std::process::exit(1);
		}
	}

	let tir = tir::TIR::build(&mut compilation);
	if emit_diagnostics(&compilation, &tir.diagnostics, style) {
		std::process::exit(1);
	}

	let mir =
		mir::MIR::build(&tir, &compilation.interner, compilation.id_generator);
	let module = codegen::Builder::build(&mir, &compilation.interner).unwrap();
	let bytecode = module.encode();

	let stem = output_stem(file_path);
	let out_path = format!("{stem}.wasm");
	let mut file = fs::File::create(&out_path).unwrap();
	file.write_all(&bytecode).unwrap();
	println!("Wrote {} bytes to {out_path}", bytecode.len());
}

fn cmd_check(file_path: &str, style: DisplayStyle) {
	let mut compilation = load_compilation(file_path);

	for crate_graph in &compilation.crates {
		if emit_diagnostics(
			&compilation,
			&crate_graph.diagnostics,
			style.clone(),
		) {
			std::process::exit(1);
		}
	}

	let tir = tir::TIR::build(&mut compilation);
	if emit_diagnostics(&compilation, &tir.diagnostics, style) {
		std::process::exit(1);
	}

	println!("No errors found.");
}

fn cmd_format(file_path: &str) {
	let source = match fs::read_to_string(file_path) {
		Ok(s) => s,
		Err(e) => {
			eprintln!("error: cannot read '{file_path}': {e}");
			std::process::exit(1);
		}
	};

	let mut files = vfs::Files::new();
	let file_id = files.add(file_path.to_string(), source).unwrap();
	let mut interner = ast::StringInterner::new();
	let mut id_gen = ast::DefIdGenerator::new();

	let parsed =
		ast::Parser::parse(file_id, &files, &mut interner, &mut id_gen);
	let source = &files.get(file_id).unwrap().source;
	let formatted = wx_fmt::format(
		&parsed,
		&interner,
		source,
		wx_fmt::RendererConfig::default(),
	);

	fs::write(file_path, &formatted).unwrap();
	println!("Formatted {file_path}");
}

fn output_stem(file_path: &str) -> String {
	let filename = file_path.split('/').last().unwrap();
	let parts: Vec<&str> = filename.split('.').collect();
	parts[..parts.len() - 1].join(".")
}
