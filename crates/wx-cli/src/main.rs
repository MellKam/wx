use std::fs;
use std::io::Write;

use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::files::Files as _;
use codespan_reporting::term;
use codespan_reporting::term::DisplayStyle;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use wx_compiler::*;

fn main() {
	let message_format = clap::Arg::new("message-format")
		.long("message-format")
		.value_name("FMT")
		.value_parser(clap::builder::PossibleValuesParser::new([
			clap::builder::PossibleValue::new("human"),
			clap::builder::PossibleValue::new("short"),
			clap::builder::PossibleValue::new("json"),
		]))
		.default_value("human");

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
				.arg(
					clap::Arg::new("output")
						.short('o')
						.long("output")
						.value_name("PATH")
						.help(
							"Output path for the compiled .wasm; use `-` \
							 for stdout (default: <input>.wasm)",
						),
				)
				.arg(message_format.clone()),
		)
		.subcommand(
			clap::Command::new("check")
				.about("Type-check a WX source file without emitting output")
				.arg(clap::Arg::new("path").required(true).index(1))
				.arg(message_format),
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
			let output = sub.get_one::<String>("output").map(String::as_str);
			let format = parse_message_format(
				sub.get_one::<String>("message-format").unwrap(),
			);
			cmd_compile(path, output, format);
		}
		Some(("check", sub)) => {
			let path = sub.get_one::<String>("path").unwrap();
			let format = parse_message_format(
				sub.get_one::<String>("message-format").unwrap(),
			);
			cmd_check(path, format);
		}
		Some(("format", sub)) => {
			cmd_format(sub.get_one::<String>("path").unwrap())
		}
		_ => unreachable!(),
	}
}

enum MessageFormat {
	Text(DisplayStyle),
	Json,
}

fn parse_message_format(s: &str) -> MessageFormat {
	match s {
		"json" => MessageFormat::Json,
		"medium" => MessageFormat::Text(DisplayStyle::Medium),
		"short" => MessageFormat::Text(DisplayStyle::Short),
		_ => MessageFormat::Text(DisplayStyle::Rich),
	}
}

/// One diagnostic label resolved to a human-facing line/column, for JSON output.
#[derive(serde::Serialize)]
struct JsonLabel {
	style: &'static str,
	file: String,
	line: usize,
	column: usize,
	message: String,
}

/// A single diagnostic in `--message-format=json` output. One JSON object
/// per line (NDJSON), matching `rustc --error-format=json`'s convention.
#[derive(serde::Serialize)]
struct JsonDiagnostic {
	severity: &'static str,
	code: Option<String>,
	message: String,
	labels: Vec<JsonLabel>,
	notes: Vec<String>,
}

fn severity_str(severity: Severity) -> &'static str {
	match severity {
		Severity::Bug => "bug",
		Severity::Error => "error",
		Severity::Warning => "warning",
		Severity::Note => "note",
		Severity::Help => "help",
	}
}

fn diagnostic_to_json(
	files: &vfs::Files,
	d: &Diagnostic<vfs::FileId>,
) -> JsonDiagnostic {
	let labels = d
		.labels
		.iter()
		.map(|label| {
			let file = files
				.name(label.file_id)
				.map(str::to_string)
				.unwrap_or_default();
			let location = files
				.location(label.file_id, label.range.start)
				.unwrap_or(codespan_reporting::files::Location {
					line_number: 0,
					column_number: 0,
				});
			JsonLabel {
				style: match label.style {
					codespan_reporting::diagnostic::LabelStyle::Primary => {
						"primary"
					}
					codespan_reporting::diagnostic::LabelStyle::Secondary => {
						"secondary"
					}
				},
				file,
				line: location.line_number,
				column: location.column_number,
				message: label.message.clone(),
			}
		})
		.collect();

	JsonDiagnostic {
		severity: severity_str(d.severity),
		code: d.code.clone(),
		message: d.message.clone(),
		labels,
		notes: d.notes.clone(),
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

/// Emits every diagnostic in `diagnostics` to stderr in the given format.
/// Does not inspect severity — call `abort_if_errors` separately, after all
/// diagnostics across every stage have been emitted.
fn emit_diagnostics(
	compilation: &vfs::CompilationGraph,
	diagnostics: &[Diagnostic<vfs::FileId>],
	format: &MessageFormat,
) {
	match format {
		MessageFormat::Json => {
			let stderr = std::io::stderr();
			let mut lock = stderr.lock();
			for d in diagnostics {
				let json = diagnostic_to_json(&compilation.files, d);
				writeln!(lock, "{}", serde_json::to_string(&json).unwrap())
					.unwrap();
			}
		}
		MessageFormat::Text(style) => {
			let writer = StandardStream::stderr(ColorChoice::Always);
			let config = term::Config {
				display_style: style.clone(),
				..term::Config::default()
			};
			for d in diagnostics {
				term::emit_to_write_style(
					&mut writer.lock(),
					&config,
					&compilation.files,
					d,
				)
				.unwrap();
			}
		}
	}
}

/// Prints a rustc-style summary and exits the process if `count` is nonzero.
#[inline]
fn abort_if_errors(count: usize) {
	if count == 0 {
		return;
	}
	let noun = if count == 1 { "error" } else { "errors" };
	eprintln!("error: aborting due to {count} previous {noun}");
	std::process::exit(1);
}

fn cmd_compile(file_path: &str, output: Option<&str>, format: MessageFormat) {
	let mut compilation = load_compilation(file_path);

	for crate_graph in &compilation.crates {
		emit_diagnostics(&compilation, &crate_graph.diagnostics, &format);
	}
	abort_if_errors(
		compilation
			.crates
			.iter()
			.flat_map(|crate_graph| crate_graph.diagnostics.iter())
			.filter(|d| matches!(d.severity, Severity::Error | Severity::Bug))
			.count(),
	);

	let tir = tir::TIR::build(&mut compilation);
	emit_diagnostics(&compilation, &tir.diagnostics, &format);
	abort_if_errors(
		tir.diagnostics
			.iter()
			.filter(|d| matches!(d.severity, Severity::Error | Severity::Bug))
			.count(),
	);

	let mir =
		mir::MIR::build(&tir, &compilation.interner, compilation.id_generator);
	let module = codegen::Builder::build(&mir, &compilation.interner).unwrap();
	let bytecode = module.encode();

	if output == Some("-") {
		std::io::stdout().write_all(&bytecode).unwrap();
		return;
	}

	let out_path = match output {
		Some(path) => path.to_string(),
		None => format!("{}.wasm", output_stem(file_path)),
	};
	let mut file = fs::File::create(&out_path).unwrap();
	file.write_all(&bytecode).unwrap();
	eprintln!("Wrote {} bytes to {out_path}", bytecode.len());
}

fn cmd_check(file_path: &str, format: MessageFormat) {
	let mut compilation = load_compilation(file_path);

	for crate_graph in &compilation.crates {
		emit_diagnostics(&compilation, &crate_graph.diagnostics, &format);
	}
	abort_if_errors(
		compilation
			.crates
			.iter()
			.flat_map(|crate_graph| crate_graph.diagnostics.iter())
			.filter(|d| matches!(d.severity, Severity::Error | Severity::Bug))
			.count(),
	);

	let tir = tir::TIR::build(&mut compilation);
	emit_diagnostics(&compilation, &tir.diagnostics, &format);
	abort_if_errors(
		tir.diagnostics
			.iter()
			.filter(|d| matches!(d.severity, Severity::Error | Severity::Bug))
			.count(),
	);

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
	let filename = file_path.split('/').next_back().unwrap();
	let parts: Vec<&str> = filename.split('.').collect();
	parts[..parts.len() - 1].join(".")
}
