use ast::{Diagnostic, DiagnosticKind, Parser, SpanMultilinePrinter, SpanSingleLinePrinter};
use hir::builder::HIRBuilder;
use mir::builder::MIRBuilder;
use owo_colors::OwoColorize;

use crate::ast::SpanPrinter;

mod ast;
mod hir;
mod mir;

pub fn print_diagnostic(source: &str, diagnostic: &Diagnostic) {
    let mut printer = SpanSingleLinePrinter::default();
    match diagnostic.kind {
        DiagnosticKind::Error => {
            println!("{}: {}", "error".red().bold(), diagnostic.message);
        }
        DiagnosticKind::Warning => {
            println!("{}: {}", "warning".yellow().bold(), diagnostic.message);
        }
    }
    let _ = printer.print(diagnostic.span.clone(), source);
}

fn main() {
    let source = r#"
        fn main(a: i64): i64 {
            return 2 * 5 * a - 2;
        }
    "#;
    let mut parser = Parser::new(source);
    parser.parse();

    for diagnostic in parser.diagnostics.borrow().diagnostics.iter() {
        print_diagnostic(source, diagnostic);
    }
    // println!("{:#?}", parser.ast);

    let hir = HIRBuilder::build(&parser.ast, &parser.interner);
    println!("{:#?}", hir);
    let mir = MIRBuilder::build(&hir);
    println!("{:#?}", mir);
}
