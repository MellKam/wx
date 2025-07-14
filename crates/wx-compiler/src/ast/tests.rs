use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use string_interner::StringInterner;
use string_interner::backend::StringBackend;

use super::parser::*;
use super::*;
use crate::ast::diagnostics::{
    InvalidItemDiagnostic, MissingLocalInitializerDiagnostic, MissingReturnTypeDiagnostic,
    MissingSeparatorDiagnostic, UnclosedGroupingDiagnotic,
};
use crate::files::Files;

struct TestCase {
    interner: StringInterner<StringBackend>,
    files: Files,
    ast: Ast,
    diagnostics: Vec<Diagnostic<FileId>>,
}

impl TestCase {
    fn create(source: &str) -> Self {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add("test.wx".to_string(), source.to_string())
            .unwrap();
        let (ast, diagnostics) =
            Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        TestCase {
            interner,
            files,
            ast,
            diagnostics,
        }
    }

    fn print_diagnostics(&self) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for diagnostic in self.diagnostics.iter() {
            term::emit(&mut writer.lock(), &config, &self.files, diagnostic).unwrap();
        }
    }
}

#[test]
fn should_recover_from_invalid_item() {
    let case = TestCase::create("foo func test(): unit {}");

    assert_eq!(case.diagnostics.len(), 1);
    assert_eq!(
        case.diagnostics[0].code.as_ref().unwrap(),
        InvalidItemDiagnostic::CODE
    );
    assert_eq!(case.ast.items.len(), 1);
}

#[test]
fn should_recover_from_missing_function_return_type() {
    let case = TestCase::create("func test() {}");

    assert_eq!(case.diagnostics.len(), 1);
    assert_eq!(
        case.diagnostics.get(0).unwrap().code.as_ref().unwrap(),
        MissingReturnTypeDiagnostic::CODE
    );
    assert_eq!(case.ast.items.len(), 1);
}

#[test]
fn should_recover_from_unclosed_function_block() {
    let case = TestCase::create("func test(): unit { 5 + 5 ");

    assert_eq!(case.ast.items.len(), 1);
    match &case.ast.items[0].kind {
        ItemKind::FunctionDefinition { block, .. } => match &block.kind {
            ExprKind::Block(block) => {
                assert_eq!(block.inner.len(), 1);
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
    assert_eq!(case.diagnostics.len(), 1);
    assert_eq!(
        case.diagnostics[0].code.as_ref().unwrap(),
        UnclosedGroupingDiagnotic::CODE
    );
}

#[test]
fn should_recover_from_missing_param_separator() {
    let case = TestCase::create("func test(a: i32  b: i32): i32 { a + b }");

    match &case.ast.items[0].kind {
        ItemKind::FunctionDefinition { signature, block } => {
            assert_eq!(signature.params.inner.len(), 2);
            assert_eq!(signature.params.inner[0].separator, None);
            assert_eq!(signature.params.inner[1].separator, None);
            match &block.kind {
                ExprKind::Block(block) => {
                    assert_eq!(block.inner.len(), 1);
                    assert_eq!(block.inner[0].separator, None);
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
    assert_eq!(case.diagnostics.len(), 1);
    assert_eq!(
        case.diagnostics[0].code.as_ref().unwrap(),
        MissingSeparatorDiagnostic::CODE
    );
}

#[test]
fn should_recover_from_missing_statement_separator() {
    let case = TestCase::create("func test(): i32 { local x = 10 x }");

    match &case.ast.items[0].kind {
        ItemKind::FunctionDefinition { block, .. } => match &block.kind {
            ExprKind::Block(block) => {
                assert_eq!(block.inner.len(), 2);
                assert_eq!(block.inner[0].separator, None);
                assert_eq!(block.inner[1].separator, None);
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
    assert_eq!(case.diagnostics.len(), 1);
    assert_eq!(
        case.diagnostics[0].code.as_ref().unwrap(),
        MissingSeparatorDiagnostic::CODE
    );
}

#[test]
fn should_report_local_definition_without_value() {
    let case = TestCase::create("func test(): unit { local x }");

    assert_eq!(case.ast.items.len(), 1);
    assert_eq!(case.diagnostics.len(), 1);
    assert_eq!(
        case.diagnostics[0].code.as_ref().unwrap(),
        MissingLocalInitializerDiagnostic::CODE
    );
}

#[test]
fn should_parse_exported_items() {
    let case = TestCase::create("export func test(): unit { }");

    assert_eq!(case.ast.items.len(), 1);
    match &case.ast.items[0].kind {
        ItemKind::ExportModifier { item, .. } => match &item.kind {
            ItemKind::FunctionDefinition { .. } => {}
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
    assert!(case.diagnostics.is_empty());

    let case = TestCase::create("export global X: i32 = 0");

    assert_eq!(case.ast.items.len(), 1);
    match &case.ast.items[0].kind {
        ItemKind::ExportModifier { item, .. } => match &item.kind {
            ItemKind::GlobalDefinition { .. } => {}
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
    assert!(case.diagnostics.is_empty());
}
