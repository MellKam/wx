use ast::{Diagnostic, DiagnosticKind, Parser, SpanMultilinePrinter, SpanSingleLinePrinter};
use hir::builder::HIRBuilder;
use owo_colors::OwoColorize;

use crate::ast::SpanPrinter;

mod ast;
mod hir;

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
        fn main(): i32 {
            mut x: i32 = 2;
            return x;
        }
    "#;
    let mut parser = Parser::new(source);
    parser.parse();

    for diagnostic in parser.diagnostics.borrow().diagnostics.iter() {
        print_diagnostic(source, diagnostic);
    }

    let hir = HIRBuilder::new(&parser.ast, &parser.interner).build();
    println!("{:#?}", hir);
}
