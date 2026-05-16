use indoc::indoc;

use super::*;

#[allow(unused)]
struct TestCase {
    interner: StringInterner,
    files: Files,
    ast: AST,
}

impl<'case> TestCase {
    fn new(source: &str) -> Self {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add("main.wx".to_string(), source.to_string())
            .unwrap();
        let ast = Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        TestCase {
            interner,
            files,
            ast,
        }
    }
}

#[test]
fn test_parse_export_with_alias() {
    let case = TestCase::new(indoc! {"
        fn test() -> i32 {
            local x: i32 = a: loop {
                break :a 1 as i32;
            };

            x
        }

        export {
            test as \"test\",
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_init_basic() {
    let case = TestCase::new(indoc! {"
        fn main() {
            local v = Vec3::{ x: 1, y: 2, z: 3 }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_init_shorthand() {
    // { field } is shorthand for { field: field }
    let case = TestCase::new(indoc! {"
        fn main() {
            local x = 1
            local y = 2
            local p = Point::{ x, y }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_init_mixed_fields() {
    // mix of shorthand and explicit fields
    let case = TestCase::new(indoc! {"
        fn main() {
            local z = 0
            local v = Vec3::{ x: 1, y: 2, z }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_init_trailing_comma() {
    let case = TestCase::new(indoc! {"
        fn main() {
            local v = Vec3::{ x: 1, y: 2, z: 3, }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_init_empty() {
    let case = TestCase::new(indoc! {"
        fn main() {
            local u = Unit::{}
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_init_in_if_condition() {
    // Foo::{ } in an if condition must not be confused with the if's block
    let case = TestCase::new(indoc! {"
        fn check(p: Point) -> bool {
            local q = Point::{ x: 0, y: 0 }
            if q.x == p.x {
                return 1
            }
            0
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_init_label_not_confused() {
    // b1: { } is a labelled block, NOT a struct init — different token (: vs ::)
    let case = TestCase::new(indoc! {"
        fn main() {
            b1: {
                break :b1
            }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}
