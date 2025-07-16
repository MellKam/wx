use string_interner::StringInterner;
use string_interner::backend::StringBackend;

use super::parser::*;
use crate::files::Files;
use indoc::indoc;
use insta::assert_yaml_snapshot;

#[allow(unused)]
struct TestCase {
    interner: StringInterner<StringBackend>,
    files: Files,
    result: ParserResult,
}

impl<'case> TestCase {
    fn new(source: &str) -> Self {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add("main.wx".to_string(), source.to_string())
            .unwrap();
        let result = Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        TestCase {
            interner,
            files,
            result,
        }
    }
}

#[test]
fn should_recover_from_invalid_item() {
    let case = TestCase::new("foo func test(): unit {}");
    assert_yaml_snapshot!(case.result);
}

#[test]
fn should_recover_from_missing_function_return_type() {
    let case = TestCase::new("func test() {}");
    assert_yaml_snapshot!(case.result);
}

#[test]
fn should_recover_from_unclosed_function_block() {
    let case = TestCase::new("func test(): unit { 5 + 5 ");
    assert_yaml_snapshot!(case.result);
}

#[test]
fn should_recover_from_missing_param_separator() {
    let case = TestCase::new(indoc! {"
        func test(a: i32  b: i32): i32 { 
            a + b
        }
    "});
    assert_yaml_snapshot!(case.result);
}

#[test]
fn should_recover_from_missing_statement_separator() {
    let case = TestCase::new(indoc! {"
        func test(): i32 {
            local x = 10
            x + 5
        }
    "});
    assert_yaml_snapshot!(case.result);
}

#[test]
fn should_report_missing_local_initializer() {
    let case = TestCase::new(indoc! {"
        func test(): i32 { 
            local x: i32;
            x
        }
    "});
    assert_yaml_snapshot!(case.result);
}
