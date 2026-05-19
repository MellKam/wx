use indoc::indoc;

use super::*;

#[allow(unused)]
struct TestCase {
    interner: StringInterner,
    files: Files,
    ast: AST,
}

impl TestCase {
    fn new(source: &str) -> Self {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add("main.wx".to_string(), source.to_string())
            .unwrap();
        let ast = Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
        TestCase { interner, files, ast }
    }
}

// ── Top-level items ──────────────────────────────────────────────────────────

#[test]
fn test_function() {
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 {
            a + b
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_function_mut_param() {
    // mut on a parameter; mut local with compound assignment
    let case = TestCase::new(indoc! {"
        fn sum_down(mut n: i32) -> i32 {
            local mut acc: i32 = 0;
            acc += n;
            acc
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_fn_pointer_param() {
    // fn(…) -> … as a parameter type
    let case = TestCase::new(indoc! {"
        fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
            f(x)
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_impl_trait_param() {
    // `impl Trait` as a parameter type and return type
    let case = TestCase::new(indoc! {"
        fn grow(mem: impl Memory32, delta: u32) -> u32 {
            delta
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_memory() {
    let case = TestCase::new(indoc! {"
        memory MEM: Memory32;
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_global() {
    let case = TestCase::new(indoc! {"
        global mut counter: i32 = 0
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_const() {
    let case = TestCase::new(indoc! {"
        const MAX: i32 = 100
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_definition() {
    let case = TestCase::new(indoc! {"
        struct Point {
            pub x: i32,
            y: i32,
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_import() {
    let case = TestCase::new(indoc! {"
        import \"env\" {
            fn log(message: string)
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_impl() {
    // impl block with an attribute and a pub method
    let case = TestCase::new(indoc! {"
        impl i32 {
            #[inline]
            pub fn double(self) -> i32 {
                self * 2
            }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_trait_abstract() {
    // trait with const declarations and an abstract method (no default body)
    let case = TestCase::new(indoc! {"
        trait Sized {
            const SIZE: u32;
            const ALIGN: u32;
            fn measure(self) -> u32;
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_trait_default_method() {
    // trait method with a default implementation and an attribute
    let case = TestCase::new(indoc! {"
        trait Memory32 {
            const OFFSET: u32;

            #[inline]
            fn grow(self, delta: u32) -> u32 {
                delta
            }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_enum() {
    // variant with and without an explicit discriminant value
    let case = TestCase::new(indoc! {"
        enum Color {
            Red,
            Green = 1,
            Blue,
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_export_alias() {
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { a + b }
        export { add as \"wasm_add\" }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

// ── Expressions ──────────────────────────────────────────────────────────────

#[test]
fn test_literals() {
    // float, char, and string literals (int is covered everywhere else)
    let case = TestCase::new(indoc! {"
        fn f() {
            local a = 3.14;
            local b = 'z';
            local c = \"hello\";
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_if_else() {
    let case = TestCase::new(indoc! {"
        fn sign(x: i32) -> i32 {
            if x > 0 {
                1
            } else {
                0
            }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_loop_break_label() {
    // labeled loop, break with label and a value, continue
    let case = TestCase::new(indoc! {"
        fn first_positive(mut n: i32) -> i32 {
            result: loop {
                if n > 0 {
                    break :result n;
                }
                n += 1;
                continue;
            }
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_struct_init() {
    // explicit fields, shorthand ({ field } == { field: field }), and empty
    let case = TestCase::new(indoc! {"
        fn make(x: i32, y: i32) {
            local full  = Point::{ x: x, y: y };
            local short = Point::{ x, y };
            local empty = Unit::{};
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_call_field_namespace() {
    // function call, field access, namespace access, unary ops
    let case = TestCase::new(indoc! {"
        fn ops(x: i32, p: Point) -> i32 {
            local neg   = -x;
            local inv   = ^x;
            local field = p.x;
            local ns    = console::log;
            add(field, neg)
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_operator_precedence() {
    // * binds tighter than +; the AST must reflect this without extra parens
    let case = TestCase::new(indoc! {"
        fn f(a: i32, b: i32, c: i32) -> i32 {
            a + b * c
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_left_associativity() {
    // a - b - c must be (a - b) - c, not a - (b - c)
    // tests that repeated same-precedence operators nest left, not right
    let case = TestCase::new(indoc! {"
        fn f(a: i32, b: i32, c: i32) -> i32 {
            a - b - c
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_cast_precedence() {
    // `as` binds tighter than arithmetic: a + b as i32  =>  a + (b as i32)
    // `as` binds tighter than unary:     -x as i32      =>  -(x as i32)
    let case = TestCase::new(indoc! {"
        fn arith(a: i32, b: i32) -> i32 { a + b as i32 }
        fn unary(x: i32) -> i32 { -x as i32 }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_chained_member_access() {
    // member access is left-associative: p.x.y  =>  (p.x).y
    // a call result can be immediately accessed:  p.foo().z  =>  (p.foo()).z
    let case = TestCase::new(indoc! {"
        fn f(p: Point) {
            local a = p.x.y;
            local b = p.foo().z;
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_numeric_literal_forms() {
    // hex, binary, and underscore-separated literals all parse to their i64 value
    let case = TestCase::new(indoc! {"
        fn f() {
            local hex    = 0xFF;
            local binary = 0b1010;
            local sep    = 1_000_000;
        }
    "});
    insta::assert_yaml_snapshot!(case.ast);
}

// ── Diagnostics ──────────────────────────────────────────────────────────────

#[test]
fn test_missing_semicolon_warns_but_parses() {
    // Semicolons are recommended but optional — the parser recovers and emits
    // E0003 warnings without corrupting the AST.
    let case = TestCase::new(indoc! {"
        fn f(x: i32) -> i32 {
            local y: i32 = x
            y
        }
    "});
    assert!(case.ast.diagnostics.iter().any(|d| d.code.as_deref() == Some("E0003")));
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_unclosed_delimiter() {
    // Missing `}` on the function body — parser recovers, emits E0004, and
    // still produces the items it managed to parse before EOF.
    let case = TestCase::new(indoc! {"
        fn f() {
            local x: i32 = 1;
    "});
    assert!(case.ast.diagnostics.iter().any(|d| d.code.as_deref() == Some("E0004")));
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_invalid_integer_literal() {
    // A number that overflows i64 is caught at parse time (E0005).
    let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            99999999999999999999
        }
    "});
    assert!(case.ast.diagnostics.iter().any(|d| d.code.as_deref() == Some("E0005")));
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_incomplete_expression() {
    // Dangling binary operator and standalone unary operator both produce E0006.
    let case = TestCase::new(indoc! {"
        fn binary() -> i32 { 1 + }
        fn unary()  -> i32 { -   }
    "});
    assert!(case.ast.diagnostics.iter().any(|d| d.code.as_deref() == Some("E0006")));
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_reserved_identifier() {
    // Using a keyword as a local variable name produces E0008.
    let case = TestCase::new(indoc! {"
        fn f() {
            local fn = 1;
        }
    "});
    assert!(case.ast.diagnostics.iter().any(|d| d.code.as_deref() == Some("E0008")));
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_missing_initializer() {
    // Both `local` and `global` require `= value`; omitting it produces E0010.
    let case = TestCase::new(indoc! {"
        fn f() {
            local x: i32
        }
        global y: i32
    "});
    let e0010_count = case.ast.diagnostics.iter()
        .filter(|d| d.code.as_deref() == Some("E0010"))
        .count();
    assert_eq!(e0010_count, 2, "expected one E0010 for local and one for global");
    insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_chained_comparison_error() {
    // a < b < c must produce an E0007 diagnostic; the parser must not crash.
    let case = TestCase::new(indoc! {"
        fn f(a: i32, b: i32, c: i32) -> bool {
            a < b < c
        }
    "});
    assert!(case.ast.diagnostics.iter().any(|d| d.code.as_deref() == Some("E0007")));
    insta::assert_yaml_snapshot!(case.ast);
}
