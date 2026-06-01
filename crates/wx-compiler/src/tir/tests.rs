use std::collections::HashMap;
use std::path::PathBuf;

use codespan_reporting::diagnostic::Severity;
use indoc::indoc;

use super::*;
use crate::tir::builder::{CharLiteralError, parse_char_literal, unescape_string};
use crate::vfs;

#[allow(unused)]
struct TestCase {
    interner: ast::StringInterner,
    graph: vfs::CompilationGraph,
    tir: TIR,
}

impl<'case> TestCase {
    fn new(source: &str) -> Self {
        let mut interner = ast::StringInterner::new();
        let graph = vfs::load_single_file_compilation(
            "main.wx".to_string(),
            source.to_string(),
            &mut interner,
        )
        .unwrap();
        let tir = TIR::build(&graph, &mut interner);

        TestCase {
            interner,
            graph,
            tir,
        }
    }

    fn new_multi_file(entry_path: &str, source: &str, extra_files: &[(&str, &str)]) -> Self {
        let mut workspace_files = HashMap::new();
        for (path, source) in extra_files {
            workspace_files.insert(PathBuf::from(path), (*source).to_string());
        }
        let file_source = vfs::VirtualFileSource::new(workspace_files);

        let mut interner = ast::StringInterner::new();
        let graph = vfs::load_single_file_compilation_with_source(
            entry_path.to_string(),
            source.to_string(),
            &file_source,
            &mut interner,
        )
        .unwrap();
        let tir = TIR::build(&graph, &mut interner);

        TestCase {
            interner,
            graph,
            tir,
        }
    }
}

#[test]
fn test_unescape_string() {
    assert_eq!(unescape_string(r#""hello""#), "hello");
    assert_eq!(unescape_string(r#""hello\nworld""#), "hello\nworld");
    assert_eq!(unescape_string(r#""tab\tthere""#), "tab\tthere");
    assert_eq!(unescape_string(r#""quote\"here""#), "quote\"here");
    assert_eq!(unescape_string(r#""backslash\\here""#), "backslash\\here");
    assert_eq!(unescape_string(r#""null\0byte""#), "null\0byte");
    assert_eq!(unescape_string(r#""carriage\rreturn""#), "carriage\rreturn");
    // Multiple escapes
    assert_eq!(
        unescape_string(r#""line1\nline2\nline3""#),
        "line1\nline2\nline3"
    );
    // No quotes (should return as-is)
    assert_eq!(unescape_string("hello"), "hello");
}

#[test]
fn test_parse_char_literal() {
    // Plain characters
    assert_eq!(parse_char_literal("'a'"), Ok('a'));
    assert_eq!(parse_char_literal("'Z'"), Ok('Z'));
    assert_eq!(parse_char_literal("'0'"), Ok('0'));
    assert_eq!(parse_char_literal("' '"), Ok(' '));

    // Named escape sequences
    assert_eq!(parse_char_literal(r"'\n'"), Ok('\n'));
    assert_eq!(parse_char_literal(r"'\r'"), Ok('\r'));
    assert_eq!(parse_char_literal(r"'\t'"), Ok('\t'));
    assert_eq!(parse_char_literal(r"'\\'"), Ok('\\'));
    assert_eq!(parse_char_literal(r"'\''"), Ok('\''));
    assert_eq!(parse_char_literal(r"'\0'"), Ok('\0'));

    // Hex escapes
    assert_eq!(parse_char_literal(r"'\x41'"), Ok('A')); // 0x41 = 65 = 'A'
    assert_eq!(parse_char_literal(r"'\x0A'"), Ok('\n')); // 0x0A = 10 = '\n'
    assert_eq!(parse_char_literal(r"'\x00'"), Ok('\0'));

    // Without surrounding quotes — content passed directly
    assert_eq!(parse_char_literal("a"), Ok('a'));

    // Errors
    assert!(matches!(
        parse_char_literal("''"),
        Err(CharLiteralError::Empty)
    ));
    assert!(matches!(
        parse_char_literal("'ab'"),
        Err(CharLiteralError::TooLong)
    ));
}

#[test]
fn test_build_with_crate_graph_lowers_child_module_items() {
    let case = TestCase::new_multi_file(
        "src/main.wx",
        "module math;",
        &[("src/math.wx", "fn add() -> i32 { 1 }")],
    );

    assert!(
        case.tir
            .functions
            .iter()
            .any(|function| case.interner.resolve(function.name.inner) == Some("add"))
    );
}

#[test]
fn test_build_with_crate_graph_resolves_cross_file_module_function_call() {
    let case = TestCase::new_multi_file(
        "src/main.wx",
        indoc! {"
            module math;

            fn main() -> i32 {
                math::add()
            }

            export { main }
        "},
        &[("src/math.wx", "pub fn add() -> i32 { 1 }")],
    );

    no_errors(&case);
}

#[test]
fn test_build_with_crate_graph_resolves_cross_file_module_type_access() {
    let case = TestCase::new_multi_file(
        "src/main.wx",
        indoc! {"
            module shapes;

            fn use_circle(circle: shapes::Circle) {
                unreachable
            }
        "},
        &[("src/shapes.wx", "pub struct Circle {}")],
    );

    no_errors(&case);
}

#[test]
fn test_duplicate_export() {
    let case = TestCase::new(indoc! {"
        fn foo() -> i32 { 42 }
        fn bar() -> i32 { 43 }

        export {
            foo as \"add\",
            bar as \"add\",
        }
    "});

    // Should have a duplicate export diagnostic (but not duplicate definition)
    assert!(!case.tir.diagnostics.is_empty());

    // Check that we have a duplicate export diagnostic
    let has_duplicate_export = case
        .tir
        .diagnostics
        .iter()
        .any(|d| d.message.contains("exported multiple times"));
    assert!(has_duplicate_export, "Expected duplicate export diagnostic");
}

#[test]
fn test_duplicate_export_with_alias() {
    let case = TestCase::new(indoc! {"
        fn foo() -> i32 { 42 }
        fn bar() -> i32 { 43 }

        export {
            foo,
            bar as \"foo\",
        }
    "});

    // Should have a duplicate export diagnostic (but not duplicate definition)
    assert!(!case.tir.diagnostics.is_empty());

    // Check that we have a duplicate export diagnostic
    let has_duplicate_export = case
        .tir
        .diagnostics
        .iter()
        .any(|d| d.message.contains("exported multiple times"));
    assert!(has_duplicate_export, "Expected duplicate export diagnostic");
}

#[test]
fn test_parse_simple_addition() {
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { a + b }

        export { add, add as \"plus\" }
    "});
    insta::assert_yaml_snapshot!(case.tir);
}

#[test]
fn test_parse_import_with_alias() {
    let case = TestCase::new(indoc! {"
        import \"console\" as console {
            fn log(ptr: u32, len: u32) -> unit;
        }

        fn main() -> unit {
            console::log(0, 0);
        }

        export { main }
    "});
    insta::assert_yaml_snapshot!(case.tir);
}

#[test]
fn test_local_variable_used_in_import_call() {
    let case = TestCase::new(indoc! {"
        pub struct string {
            bytes: []u8,
        }

        import \"console\" {
            fn log(ptr: u32, len: u32);
        }

        fn main() {
            local length = \"test\".len();
            console::log(0, length);
        }

        export { main }
    "});
    insta::assert_yaml_snapshot!(case.tir);
}

fn has_error_code(tir: &TIR, code: &str) -> bool {
    tir.diagnostics.iter().any(|d| {
        d.code
            .as_ref()
            .map(|c| c.to_string().contains(code))
            .unwrap_or(false)
    })
}

// ── coerce_untyped_int_expr ──────────────────────────────────────────────

#[test]
fn test_coerce_int_to_i32() {
    let case = TestCase::new("fn f() -> i32 { 42 } export { f }");
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_int_to_i64() {
    let case = TestCase::new("fn f() -> i64 { 9999999999 } export { f }");
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_int_to_u32() {
    let case = TestCase::new("fn f() -> u32 { 100 } export { f }");
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_int_to_u64() {
    let case = TestCase::new("fn f() -> u64 { 0 } export { f }");
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_int_overflow_i32() {
    // i32::MAX + 1 = 2147483648 overflows i32
    let case = TestCase::new("fn f() -> i32 { 2147483648 } export { f }");
    assert!(
        has_error_code(&case.tir, "E1004"),
        "expected E1004 (out of range), got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_int_negative_for_u32() {
    let case = TestCase::new("fn f() -> u32 { -1 } export { f }");
    // -1 can't fit in u32 — expect out-of-range OR unable-to-coerce error
    assert!(
        !case.tir.diagnostics.is_empty(),
        "expected a diagnostic for negative u32 literal"
    );
}

#[test]
fn test_coerce_int_literal_for_float_type_errors() {
    // An untyped integer literal cannot be coerced to f32 (must write 1.0)
    let case = TestCase::new("fn f() -> f32 { 1 } export { f }");
    assert!(
        has_error_code(&case.tir, "E1006"),
        "expected E1006 (int literal for float type), got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_int_to_bool_errors() {
    let case = TestCase::new("fn f() -> bool { 1 } export { f }");
    // int literal is not coercible to bool — expect E1005 (unable to coerce)
    assert!(
        has_error_code(&case.tir, "E1005"),
        "expected E1005 (unable to coerce), got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

// ── coerce_untyped_float_expr ────────────────────────────────────────────

#[test]
fn test_coerce_float_to_f32() {
    let case = TestCase::new("fn f() -> f32 { 3.14 } export { f }");
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_float_to_f64() {
    let case = TestCase::new("fn f() -> f64 { 2.718 } export { f }");
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_float_to_i32_errors() {
    let case = TestCase::new("fn f() -> i32 { 1.5 } export { f }");
    assert!(
        has_error_code(&case.tir, "E1005"),
        "expected E1005 (unable to coerce float to i32), got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

// ── binary arithmetic coercion ───────────────────────────────────────────

#[test]
fn test_coerce_binary_arithmetic_i32() {
    let case = TestCase::new("fn f() -> i32 { 1 + 2 } export { f }");
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_coerce_binary_bitwise_i32() {
    let case = TestCase::new("fn f() -> i32 { 10 & 12 } export { f }");
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

// ── struct definition & initialization ──────────────────────────────────

/// Basic valid struct definition and initialization.
#[test]
fn test_struct_valid_init() {
    let case = TestCase::new(indoc! {"
        struct Point {
            pub x: i32,
            pub y: i32,
        }

        fn make() -> Point {
            Point::{ x: 1, y: 2 }
        }

        export { make }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    insta::assert_yaml_snapshot!(case.tir);
}

/// Shorthand field init `{ x }` should behave like `{ x: x }`.
#[test]
fn test_struct_shorthand_init() {
    let case = TestCase::new(indoc! {"
        struct Point {
            pub x: i32,
            pub y: i32,
        }

        fn make(x: i32, y: i32) -> Point {
            Point::{ x, y }
        }

        export { make }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Fields may be provided in any order.
#[test]
fn test_struct_init_out_of_order() {
    let case = TestCase::new(indoc! {"
        struct Point {
            pub x: i32,
            pub y: i32,
        }

        fn make() -> Point {
            Point::{ y: 2, x: 1 }
        }

        export { make }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Duplicate field in the struct *definition* should produce E1022.
#[test]
fn test_struct_duplicate_field_definition() {
    let case = TestCase::new(indoc! {"
        struct Bad {
            pub x: i32,
            pub x: i32,
        }

        export { }
    "});
    assert!(
        has_error_code(&case.tir, "E1022"),
        "expected E1022 (duplicate struct field), got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Using an undeclared name in struct init position should produce an error.
#[test]
fn test_struct_init_undeclared_name() {
    let case = TestCase::new(indoc! {"
        fn main() {
            Unknown::{ }
        }

        export { main }
    "});
    assert!(
        !case.tir.diagnostics.is_empty(),
        "expected an error for unknown struct name"
    );
}

/// Unknown field name in struct init should produce E1025.
#[test]
fn test_struct_init_unknown_field() {
    let case = TestCase::new(indoc! {"
        struct Point {
            pub x: i32,
            pub y: i32,
        }

        fn make() -> Point {
            Point::{ x: 1, y: 2, z: 3 }
        }

        export { make }
    "});
    assert!(
        has_error_code(&case.tir, "E1025"),
        "expected E1025 (unknown struct field), got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Specifying the same field twice in init should produce E1026 but NOT
/// E1027 (the field was mentioned, just duplicated — it should not
/// appear as missing).
#[test]
fn test_struct_init_duplicate_field() {
    let case = TestCase::new(indoc! {"
        struct Point {
            pub x: i32,
            pub y: i32,
        }

        fn make() -> Point {
            Point::{ x: 1, y: 2, x: 3 }
        }

        export { make }
    "});
    assert!(
        has_error_code(&case.tir, "E1026"),
        "expected E1026 (duplicate field in init), got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    // x was mentioned (just duplicated) — must NOT also appear as missing
    assert!(
        !has_error_code(&case.tir, "E1027"),
        "E1027 must not fire for a duplicated field (it was mentioned)"
    );
}

/// Omitting required fields in init should produce E1027.
#[test]
fn test_struct_init_missing_fields() {
    let case = TestCase::new(indoc! {"
        struct Point {
            pub x: i32,
            pub y: i32,
        }

        fn make() -> Point {
            Point::{ x: 1 }
        }

        export { make }
    "});
    assert!(
        has_error_code(&case.tir, "E1027"),
        "expected E1027 (missing fields), got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// A field whose value fails type-checking should NOT cause that field to
/// appear in the missing-fields list (E1027).
#[test]
fn test_struct_init_errored_field_not_reported_as_missing() {
    let case = TestCase::new(indoc! {"
        struct Point {
            pub x: i32,
            pub y: i32,
        }

        fn make() -> Point {
            Point::{ x: true, y: 2 }
        }

        export { make }
    "});
    // Should have a type mismatch error for `x`…
    assert!(
        !case.tir.diagnostics.is_empty(),
        "expected a type-mismatch diagnostic"
    );
    // …but must NOT report `x` as a missing field
    let missing_x = case.tir.diagnostics.iter().any(|d| {
        d.code
            .as_ref()
            .map(|c| c.to_string().contains("E1027"))
            .unwrap_or(false)
            && d.message.contains('x')
    });
    assert!(
        !missing_x,
        "errored field `x` must not be reported as missing"
    );
}

/// Snapshot test for the duplicate-field-in-init case to lock in diagnostic
/// details.
#[test]
fn test_structs() {
    let case = TestCase::new(indoc! {"
        struct str {
            pub ptr: u32,
            pub len: u32,
        }

        fn main() -> str {
            str::{ ptr: 0, ptr: 10 }
        }

        export { main }
    "});
    insta::assert_yaml_snapshot!(case.tir);
}

// ── char / primitive type tests ──────────────────────────────────────────

/// `char` is a built-in primitive — comparisons work without any stdlib.
#[test]
fn test_stdlib_types_available() {
    let case = TestCase::new(indoc! {"
        fn is_lower(c: char) -> bool {
            c >= 'a' && c <= 'z'
        }

        export { is_lower }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// char is a primitive type — arithmetic on chars should resolve correctly.
#[test]
fn test_stdlib_struct_field_access() {
    let case = TestCase::new(indoc! {"
        fn shift(c: char) -> char {
            c - 32
        }

        export { shift }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Methods on built-in types can be defined via inline `impl` blocks.
#[test]
fn test_stdlib_method_callable() {
    let case = TestCase::new(indoc! {"
        impl char {
            pub fn is_ascii_lowercase(self) -> bool {
                self >= 'a' && self <= 'z'
            }

            pub fn to_ascii_uppercase(self) -> char {
                if self.is_ascii_lowercase() {
                    ((self as u8) ^ 0b0010_0000) as char
                } else {
                    self
                }
            }
        }

        fn uppercase(c: char) -> char {
            c.to_ascii_uppercase()
        }

        export { uppercase }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// `u32::SIZE` and `u32::ALIGN` are auto-generated associated constants.
#[test]
fn test_size_align_constants() {
    let case = TestCase::new(indoc! {"
        fn sizes() -> u32 {
            u32::SIZE
        }

        fn aligns() -> u32 {
            u32::ALIGN
        }

        export { sizes, aligns }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    // u32 is 4 bytes, aligned to 4
    let size_sym = case.interner.get("SIZE").unwrap();
    let align_sym = case.interner.get("ALIGN").unwrap();
    let members = case.tir.impl_members.get(&Type::U32_IDX).unwrap();
    assert!(matches!(
        members[&size_sym],
        ImplEntry::AssociatedConst { .. }
    ));
    assert!(matches!(
        members[&align_sym],
        ImplEntry::AssociatedConst { .. }
    ));
}

/// impl methods and associated functions are registered in `impl_members` under
/// the correct type key with the correct `ImplEntry` variant.
#[test]
fn test_impl_members_registered() {
    let case = TestCase::new(indoc! {"
        impl i32 {
            pub fn abs(self) -> i32 {
                if self < 0 { -self } else { self }
            }

            pub fn from_bool(b: bool) -> i32 {
                if b { 1 } else { 0 }
            }
        }

        fn use_them(x: i32, b: bool) -> i32 {
            x.abs() + i32::from_bool(b)
        }

        export { use_them }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );

    let members = case
        .tir
        .impl_members
        .get(&Type::I32_IDX)
        .expect("impl_members should have an entry for i32");

    let abs_sym = case.interner.get("abs").expect("symbol `abs` not interned");
    let from_bool_sym = case
        .interner
        .get("from_bool")
        .expect("symbol `from_bool` not interned");

    // `abs` takes `self` → Method; `from_bool` has no receiver → AssociatedFn
    let abs_entry = members
        .get(&abs_sym)
        .expect("`abs` missing from impl_members");
    let from_bool_entry = members
        .get(&from_bool_sym)
        .expect("`from_bool` missing from impl_members");

    assert!(
        matches!(abs_entry, ImplEntry::Method(_)),
        "`abs` should be ImplEntry::Method, got {:?}",
        abs_entry
    );
    assert!(
        matches!(from_bool_entry, ImplEntry::AssociatedFn(_)),
        "`from_bool` should be ImplEntry::AssociatedFn, got {:?}",
        from_bool_entry
    );

    // Both entries must point to valid function indices
    let &ImplEntry::Method(abs_idx) = abs_entry else {
        unreachable!()
    };
    let &ImplEntry::AssociatedFn(from_bool_idx) = from_bool_entry else {
        unreachable!()
    };
    assert!(
        (abs_idx as usize) < case.tir.functions.len(),
        "abs func_index out of bounds"
    );
    assert!(
        (from_bool_idx as usize) < case.tir.functions.len(),
        "from_bool func_index out of bounds"
    );
}

/// `pub fn` on a user-defined function suppresses the unused warning.
#[test]
fn test_pub_fn_no_unused_warning() {
    let case = TestCase::new(indoc! {"
        pub fn helper() -> i32 {
            42
        }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "expected no diagnostics for pub fn, got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// TIR preserves struct fields in declaration order; physical reordering for
/// optimal memory layout is a MIR concern (tested in mir::tests).
#[test]
fn test_struct_fields_kept_in_declaration_order() {
    let case = TestCase::new(indoc! {"
        struct Mixed {
            a: bool,
            b: i64,
            c: u32,
            d: f64,
        }

        fn dummy(m: Mixed) -> Mixed { m }
        export { dummy }
    "});
    eprintln!(
        "diags: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    assert!(case.tir.diagnostics.is_empty());

    let mixed_sym = case.interner.get("Mixed").unwrap();
    let struct_index = case
        .tir
        .type_pool
        .iter()
        .find_map(|t| {
            if let Type::Struct { struct_index } = t {
                if case.tir.structs[*struct_index as usize].name.inner == mixed_sym {
                    Some(*struct_index)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .unwrap();
    let field_names: Vec<&str> = case.tir.structs[struct_index as usize]
        .fields
        .iter()
        .map(|f| case.interner.resolve(f.name.inner).unwrap())
        .collect();
    assert_eq!(field_names, vec!["a", "b", "c", "d"]);
}

/// A non-pub function that is never called should still produce a warning.
#[test]
fn test_non_pub_fn_unused_warning() {
    let case = TestCase::new(indoc! {"
        fn unused() -> i32 {
            42
        }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.message == "function `unused` is never used"),
        "expected unused-function diagnostic"
    );
}

/// Functions declared inside a `module` block are intrinsics/imports and must
/// not trigger an unused-function warning even if they are never called.
#[test]
fn test_module_fn_no_unused_warning() {
    let case = TestCase::new(indoc! {"
        module math {
            #[intrinsic]
            fn add(a: i32, b: i32) -> i32;
        }
    "});
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.message.contains("is never used")),
        "module functions should not warn as unused, got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// User-defined struct with `pub struct` should not warn as unused.
#[test]
fn test_pub_struct_no_unused_warning() {
    // Structs don't currently emit unused warnings; this test just
    // verifies that `pub struct` parses and compiles without error.
    let case = TestCase::new(indoc! {"
        pub struct Point {
            pub x: i32,
            pub y: i32,
        }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_memory_declaration_registers_kind() {
    let case32 = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
    "}
    ));
    assert!(case32.tir.diagnostics.is_empty(), "unexpected diagnostics");
    assert_eq!(
        case32
            .tir
            .memories
            .iter()
            .map(|m| m.kind)
            .collect::<Vec<_>>(),
        vec![MemoryKind::Memory32]
    );

    let case64 = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory64;
    "}
    ));
    assert!(case64.tir.diagnostics.is_empty(), "unexpected diagnostics");
    assert_eq!(
        case64
            .tir
            .memories
            .iter()
            .map(|m| m.kind)
            .collect::<Vec<_>>(),
        vec![MemoryKind::Memory64]
    );
}

#[test]
fn test_memory_invalid_kind_is_error() {
    let case = TestCase::new("memory MEM: i32;");
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1029")),
        "expected invalid memory kind diagnostic"
    );
}

#[test]
fn test_fn_declaration_without_body_is_error() {
    // A bare `fn` with no body and no #[intrinsic] must produce E0011.
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1028")),
        "expected E0011 diagnostic for missing function body"
    );
}

#[test]
fn test_intrinsic_fn_declaration_is_valid() {
    // #[intrinsic] fn without a body must be accepted (no E0011).
    let case = TestCase::new(indoc! {"
        #[intrinsic]
        fn i32_clz(x: i32) -> i32
    "});
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1028")),
        "unexpected E0011 for #[intrinsic] declaration"
    );
}

// ── Memory namespace resolution
// ───────────────────────────────────────────────
//
// Minimal standard library preamble used by memory-related tests.

const STD: &str = indoc! {"
    trait PointerSize {}
    impl PointerSize for u32 {}
    impl PointerSize for u64 {}

    trait Memory {
        type Size: PointerSize;
        const MEMORY_INDEX: u32;

        fn grow(self, delta: Self::Size) -> Self::Size;
        fn size(self) -> Self::Size;
    }

    trait Memory32: Memory<Size = u32> {}
    trait Memory64: Memory<Size = u64> {}

    module wasm {
        #[intrinsic]
        pub fn memory_grow<M: Memory>(mem: M, delta: M::Size) -> M::Size;
        #[intrinsic]
        pub fn memory_size<M: Memory>(mem: M) -> M::Size;
    }
"};

#[test]
fn test_memory_index_const_resolves() {
    // `MEM::MEMORY_INDEX` — namespace access to a memory constant resolves cleanly.
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        pub fn f() -> u32 { MEM::MEMORY_INDEX }
    "}
    );
    let case = TestCase::new(&src);
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_memory_size_call_resolves() {
    // `.size()` is a method from the Memory trait; calling it should produce no
    // errors.
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        pub fn f() { _ = MEM.size(); }
    "}
    );
    let case = TestCase::new(&src);
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_memory_grow_call_resolves() {
    // `.grow()` is a method from the Memory trait; calling it should produce no
    // errors.
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        pub fn f() { _ = MEM.grow(1); }
    "}
    );
    let case = TestCase::new(&src);
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_memory_unknown_member_is_error() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        fn f() { _ = MEM::pages; }
    "}
    );
    let case = TestCase::new(&src);
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1007")),
        "expected undeclared identifier diagnostic for unknown memory member"
    );
}

#[test]
fn test_memory_as_value_in_expression() {
    // Memory identifiers are valid value expressions (for method calls like
    // MEM.grow(1)).
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        fn f() { _ = MEM; }
    "}
    );
    let case = TestCase::new(&src);
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1030")),
        "memory identifier should be usable as a value expression"
    );
}

// ── impl Trait errors
// ─────────────────────────────────────────────────────────

#[test]
fn test_impl_struct_is_error() {
    // Using a struct in `impl` position must emit E1031.
    let case = TestCase::new(indoc! {"
        struct S {}
        fn f(x: impl S) -> i32 { 5 }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1031")),
        "expected E1031 for struct used in impl position"
    );
}

#[test]
fn test_impl_undeclared_type_is_error() {
    // A name that doesn't resolve at all should emit E1021.
    let case = TestCase::new(indoc! {"
        fn f(x: impl Unknown) -> i32 { 5 }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1021")),
        "expected E1021 for undeclared name in impl position"
    );
}

// ── impl trait for type
// ───────────────────────────────────────────────────────

#[test]
fn test_impl_trait_for_type_registers_trait_impl() {
    let case = TestCase::new(indoc! {"
        trait Drawable {
            fn draw(self);
        }

        struct Point {
            x: i32,
            y: i32,
        }

        impl Drawable for Point {
            fn draw(self) {
                unreachable
            }
        }
    "});
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "unexpected errors: {:?}",
        case.tir
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );

    assert_eq!(case.tir.trait_impls.len(), 1, "expected one TraitImpl");

    let ti = &case.tir.trait_impls[0];

    // target type is Point (a struct)
    assert!(
        matches!(case.tir.type_pool[ti.target as usize], Type::Struct { .. }),
        "target should be a struct type"
    );

    // trait_impl_lookup contains (Point, Drawable) → 0
    let point_type = ti.target;
    let drawable_index = ti.trait_index;
    assert_eq!(
        case.tir
            .trait_impl_lookup
            .get(&(point_type, drawable_index)),
        Some(&0),
        "trait_impl_lookup should map (Point, Drawable) → 0"
    );

    // type_trait_impls maps Point → [0]
    assert_eq!(
        case.tir.type_trait_impls.get(&point_type),
        Some(&vec![0u32]),
        "type_trait_impls should map Point → [0]"
    );

    // draw method is registered in TraitImpl.members
    let draw_sym = case
        .interner
        .get("draw")
        .expect("symbol `draw` not interned");
    assert!(
        matches!(ti.members.get(&draw_sym), Some(ImplEntry::Method(_))),
        "`draw` should be ImplEntry::Method in TraitImpl.members"
    );

    // draw method also appears in impl_members for Point (for dispatch)
    let impl_members = case
        .tir
        .impl_members
        .get(&point_type)
        .expect("impl_members should have an entry for Point");
    assert!(
        matches!(impl_members.get(&draw_sym), Some(ImplEntry::Method(_))),
        "`draw` should also be in impl_members for method dispatch"
    );
}

#[test]
fn test_impl_trait_function_origin_is_trait_impl() {
    let case = TestCase::new(indoc! {"
        trait Greet {
            fn hello(self);
        }

        struct Foo {}

        impl Greet for Foo {
            fn hello(self) {
                unreachable
            }
        }
    "});
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    );

    let hello_sym = case
        .interner
        .get("hello")
        .expect("symbol `hello` not interned");
    let ti = &case.tir.trait_impls[0];

    let func_index = match ti.members.get(&hello_sym) {
        Some(ImplEntry::Method(fi)) => *fi,
        other => panic!("expected Method entry, got {:?}", other),
    };

    assert!(
        matches!(
            case.tir.functions[func_index as usize].origin,
            FunctionOrigin::TraitImpl { .. }
        ),
        "function origin should be FunctionOrigin::TraitImpl"
    );
}

// ── trait conformance check
// ───────────────────────────────────────────────────

#[test]
fn test_trait_conformance_missing_fn() {
    // impl block omits the required abstract method → E1033
    let case = TestCase::new(indoc! {"
        trait Drawable {
            fn draw(self);
        }

        struct Point {
            x: i32,
            y: i32,
        }

        impl Drawable for Point {}
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1033")),
        "expected E1033 for missing trait item, got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| (d.code.as_deref(), &d.message))
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_trait_conformance_missing_const() {
    // impl block omits a required associated const → E1033
    let case = TestCase::new(indoc! {"
        trait Sized {
            const SIZE: u32;
        }

        struct Foo {}

        impl Sized for Foo {}
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1033")),
        "expected E1033 for missing const, got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| (d.code.as_deref(), &d.message))
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_trait_conformance_default_fn_not_required() {
    // Trait methods with a default body are optional to override — no E1033
    let case = TestCase::new(indoc! {"
        trait Greet {
            fn hello(self) {
                unreachable
            }
        }

        struct Bar {}

        impl Greet for Bar {}
    "});
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "unexpected errors: {:?}",
        case.tir
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

// ── supertrait bounds
// ─────────────────────────────────────────────────────────

#[test]
fn test_supertrait_resolved() {
    // `Drawable: Sized` — the TIR Trait should carry Sized in its supertraits
    let case = TestCase::new(indoc! {"
        trait Sized {
            const SIZE: u32;
        }

        trait Drawable: Sized {
            fn draw(self);
        }
    "});
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "unexpected errors: {:?}",
        case.tir
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );

    let drawable_idx = case
        .tir
        .traits
        .iter()
        .position(|t| case.interner.resolve(t.name.inner) == Some("Drawable"))
        .expect("Drawable not found") as u32;
    let sized_idx = case
        .tir
        .traits
        .iter()
        .position(|t| case.interner.resolve(t.name.inner) == Some("Sized"))
        .expect("Sized not found") as u32;

    assert_eq!(
        case.tir.traits[drawable_idx as usize].supertraits,
        vec![sized_idx],
        "Drawable should list Sized as a supertrait"
    );
}

#[test]
fn test_supertrait_missing_impl_errors() {
    // impl Drawable for Point without impl Sized for Point → E1034
    let case = TestCase::new(indoc! {"
        trait Sized {
            const SIZE: u32;
        }

        trait Drawable: Sized {
            fn draw(self);
        }

        struct Point {
            x: i32,
            y: i32,
        }

        impl Drawable for Point {
            fn draw(self) {
                unreachable
            }
        }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1034")),
        "expected E1034 for missing supertrait impl, got: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| (d.code.as_deref(), &d.message))
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_supertrait_satisfied_impl_no_errors() {
    // Both Sized and Drawable implemented for Point — no E1034
    let case = TestCase::new(indoc! {"
        trait Sized {
            const SIZE: u32;
        }

        trait Drawable: Sized {
            fn draw(self);
        }

        struct Point {
            x: i32,
            y: i32,
        }

        impl Sized for Point {
            const SIZE: u32 = 8
        }

        impl Drawable for Point {
            fn draw(self) {
                unreachable
            }
        }
    "});
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "unexpected errors: {:?}",
        case.tir
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

// ── wasm module intrinsics
// ────────────────────────────────────────────────────

#[test]
fn test_wasm_module_intrinsics_declare_cleanly() {
    // Mirrors the wasm module in std.wx. The trait must be declared before the
    // module because define_item resolves types eagerly (no forward references).
    let case = TestCase::new(indoc! {"
        trait Memory32 {
            const OFFSET: u32;
            const MEMORY_INDEX: u32;
            fn size(self) -> u32;
            fn grow(self, delta: u32) -> u32;
        }

        module wasm {
            #[intrinsic]
            fn memory32_grow(mem: impl Memory32, delta: u32) -> u32;
            #[intrinsic]
            fn memory32_size(mem: impl Memory32) -> u32;
        }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_wasm_module_intrinsics_forward_ref_resolves() {
    // The query system resolves trait forward-references on demand, so declaring
    // a module before the trait it references is now valid.
    let case = TestCase::new(indoc! {"
        module wasm {
            #[intrinsic]
            fn memory32_grow(mem: impl Memory32, delta: u32) -> u32;
        }

        trait Memory32 {
            fn grow(self, delta: u32) -> u32;
        }
    "});
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1021")),
        "E1021 should not be emitted: the query system resolves traits on demand"
    );
}

// ── cyclic type dependency tests
// ──────────────────────────────────────────────

#[test]
fn test_struct_direct_cycle_is_error() {
    // A struct that contains itself by value has infinite size — E1032.
    let case = TestCase::new(indoc! {"
        struct A {
            field: A
        }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1032")),
        "expected E1032 for direct self-referential struct"
    );
}

#[test]
fn test_struct_mutual_cycle_is_error() {
    // A <-> B by value is an infinite-size cycle — E1032.
    let case = TestCase::new(indoc! {"
        struct A {
            b: B
        }
        struct B {
            a: A
        }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1032")),
        "expected E1032 for mutually recursive structs"
    );
}

#[test]
fn test_struct_three_way_cycle_is_error() {
    // A -> B -> C -> A cycle — E1032.
    let case = TestCase::new(indoc! {"
        struct A { b: B }
        struct B { c: C }
        struct C { a: A }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1032")),
        "expected E1032 for three-way struct cycle"
    );
}

#[test]
fn test_struct_forward_reference_resolves() {
    // B used as a field type before B is declared — no cycle, no diagnostic.
    let case = TestCase::new(indoc! {"
        struct A { b: B }
        struct B { val: i32 }
    "});
    let errors: Vec<_> = case
        .tir
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .map(|d| &d.message)
        .collect::<Vec<_>>();
    assert!(
        errors.is_empty(),
        "unexpected errors for valid forward reference: {:?}",
        errors
    );
}

#[test]
fn test_struct_forward_reference_reversed_order_resolves() {
    // Same as above but B declared first — both orderings must work.
    let case = TestCase::new(indoc! {"
        struct B { val: i32 }
        struct A { b: B }
    "});
    let errors: Vec<_> = case
        .tir
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .map(|d| &d.message)
        .collect::<Vec<_>>();
    assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
}

#[test]
fn test_fn_uses_struct_declared_after_is_ok() {
    // A function's parameter/return type that references a struct defined later
    // in the file must resolve cleanly — no type errors.
    let case = TestCase::new(indoc! {"
        fn f(x: Point) -> Point { x }
        struct Point { x: i32, y: i32 }
    "});
    let type_errors: Vec<_> = case
        .tir
        .diagnostics
        .iter()
        .filter(|d| d.code.as_deref().map_or(false, |c| c.starts_with('E')))
        .collect();
    assert!(
        type_errors.is_empty(),
        "unexpected type errors for forward-referenced struct in function: {:?}",
        type_errors.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_struct_cycle_does_not_prevent_other_structs_from_resolving() {
    // Even with a cyclic struct present, independent structs should resolve fine.
    let case = TestCase::new(indoc! {"
        struct Bad { bad: Bad }
        struct Good { val: i32 }
        fn uses_good(x: Good) -> i32 { x.val }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1032")),
        "expected E1032 for Bad"
    );
    // Good should still be registered; the function should compile without
    // an undeclared-type error.
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1021")),
        "Good struct should still resolve despite Bad being cyclic"
    );
}

// ── Generics ─────────────────────────────────────────────────────────────────

#[test]
fn test_generic_identity_resolves() {
    // identity<T>(t: T) -> T called with i32 — TIR must have no diagnostics
    // and the function must carry one TypeParamInfo named "T".
    let case = TestCase::new(indoc! {"
        pub fn identity<T>(t: T) -> T {
            t
        }
    "});
    let errors: Vec<_> = case
        .tir
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "unexpected errors (count: {})",
        errors.len()
    );
    let func = case.tir.functions.iter().find(|f| {
        case.interner
            .resolve(f.name.inner)
            .map(|n| n == "identity")
            .unwrap_or(false)
    });
    let func = func.expect("function 'identity' not found in TIR");
    assert_eq!(func.type_params.len(), 1, "expected one type param");
    assert_eq!(case.interner.resolve(func.type_params[0].name), Some("T"));
    assert!(
        func.type_params[0].bounds.is_empty(),
        "T should have no bounds"
    );
    insta::assert_yaml_snapshot!(case.tir);
}

#[test]
fn test_generic_call_return_type_substituted() {
    // Calling identity(42) must produce no diagnostics — the return type
    // is substituted from TypeParam{0} → i32 via the argument.
    let case = TestCase::new(indoc! {"
        pub fn identity<T>(t: T) -> T {
            t
        }
        pub fn main() -> i32 {
            identity(42)
        }
    "});
    let errors: Vec<_> = case
        .tir
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "unexpected errors (count: {})",
        errors.len()
    );
}

#[test]
fn test_generic_with_bound_resolves() {
    // fn with a trait bound — TypeParamInfo.bounds must contain the trait index.
    let case = TestCase::new(indoc! {"
        trait Scalable {
            fn scale(self, factor: i32) -> i32;
        }
        fn call_scale<T: Scalable>(t: T, n: i32) -> i32 {
            t.scale(n)
        }
    "});
    let errors: Vec<_> = case
        .tir
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "unexpected errors (count: {})",
        errors.len()
    );
    let func = case.tir.functions.iter().find(|f| {
        case.interner
            .resolve(f.name.inner)
            .map(|n| n == "call_scale")
            .unwrap_or(false)
    });
    let func = func.expect("function 'call_scale' not found in TIR");
    assert_eq!(func.type_params.len(), 1);
    assert_eq!(
        func.type_params[0].bounds.len(),
        1,
        "T should have one bound (Scalable)"
    );
}

#[test]
fn test_generic_unknown_bound_is_error() {
    // A bound that names an undeclared type should produce a diagnostic.
    let case = TestCase::new(indoc! {"
        fn f<T: Nonexistent>(t: T) -> T {
            t
        }
    "});
    assert!(
        !case.tir.diagnostics.is_empty(),
        "expected a diagnostic for unknown bound 'Nonexistent'"
    );
}

// ── NamespaceAccess / associated type projection ────────────────────────────

fn no_errors(case: &TestCase) {
    assert!(
        !case
            .tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "unexpected errors: {:?}",
        case.tir
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

fn has_error_matching(case: &TestCase, substring: &str) {
    assert!(
        case.tir.diagnostics.iter().any(|d| {
            d.severity == Severity::Error
                && (d.message.contains(substring) || d.notes.iter().any(|n| n.contains(substring)))
        }),
        "expected an error containing {:?}; got: {:#?}",
        substring,
        case.tir
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

fn has_error(case: &TestCase, msg: &str) {
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "expected error: {}",
        msg
    );
}

#[test]
fn test_assoc_type_declared_in_trait() {
    // A trait with an associated type must register it in `members` and
    // `assoc_type_bounds`.
    let case = TestCase::new(indoc! {"
        trait PointerSize {}
        trait Memory {
            type Size: PointerSize;
        }
    "});
    no_errors(&case);

    let memory_trait = case
        .tir
        .traits
        .iter()
        .find(|t| case.interner.resolve(t.name.inner) == Some("Memory"))
        .expect("trait 'Memory' not found");

    let size_sym = case
        .interner
        .get("Size")
        .expect("symbol 'Size' not interned");

    assert!(
        matches!(
            memory_trait.members.get(&size_sym),
            Some(ImplEntry::AssociatedType { .. })
        ),
        "expected 'Size' in Memory::members as AssociatedType"
    );
    assert!(
        memory_trait.assoc_type_bounds.contains_key(&size_sym),
        "expected 'Size' in Memory::assoc_type_bounds"
    );
}

#[test]
fn test_assoc_type_projection_in_return_type() {
    // `fn foo<M: Memory>() -> M::Size` — the return type must resolve to
    // `AssocTypeProjection` (no error diagnostics).
    let case = TestCase::new(indoc! {"
        trait PointerSize {}
        trait Memory {
            type Size: PointerSize;
        }
        fn foo<M: Memory>() -> M::Size {
            unreachable
        }
    "});
    no_errors(&case);

    let func = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("foo"))
        .expect("function 'foo' not found");

    let result_ty = func.result.as_ref().expect("expected a return type").inner;
    assert!(
        matches!(
            case.tir.type_pool[result_ty as usize],
            Type::AssocTypeProjection { .. }
        ),
        "return type should be AssocTypeProjection for M::Size, got type index {}",
        result_ty
    );
}

#[test]
fn test_assoc_type_projection_in_param_type() {
    // `fn foo<M: Memory>(size: M::Size)` — the parameter type resolves to
    // `AssocTypeProjection` without errors.
    let case = TestCase::new(indoc! {"
        trait PointerSize {}
        trait Memory {
            type Size: PointerSize;
        }
        fn consume<M: Memory>(size: M::Size) {
            unreachable
        }
    "});
    no_errors(&case);
}

#[test]
fn test_assoc_type_unknown_member_is_error() {
    // `M::Nonexistent` where `Memory` has no such associated type → diagnostic.
    let case = TestCase::new(indoc! {"
        trait Memory {
            type Size;
        }
        fn bad<M: Memory>() -> M::Nonexistent {
            unreachable
        }
    "});
    has_error(&case, "undeclared associated type 'Nonexistent'");
}

#[test]
fn test_assoc_type_bare_name_suggests_self_prefix() {
    // Using the associated type name directly (e.g. `Size` instead of
    // `Self::Size`) must produce a targeted error with a `Self::` suggestion.
    let case = TestCase::new(indoc! {"
        trait Memory {
            type Size;
            fn alloc(n: Size) -> *mut u8;
        }
    "});
    has_error_matching(&case, "cannot find type `Size` in this scope");
    has_error_matching(&case, "Self::Size");
}

#[test]
fn test_assoc_type_impl_registers_in_trait_impl() {
    // `impl Memory for Heap { type Size = u32; }` — the impl must store
    // a concrete type in both `TraitImpl::members` and `impl_members`.
    let case = TestCase::new(indoc! {"
        trait PointerSize {}
        impl PointerSize for u32 {}
        trait Memory {
            type Size: PointerSize;
        }
        struct Heap {}
        impl Memory for Heap {
            type Size = u32;
        }
    "});
    no_errors(&case);

    let ti = case
        .tir
        .trait_impls
        .iter()
        .find(|ti| {
            case.tir
                .traits
                .get(ti.trait_index as usize)
                .and_then(|t| case.interner.resolve(t.name.inner))
                == Some("Memory")
        })
        .expect("TraitImpl for Memory not found");

    let size_sym = case
        .interner
        .get("Size")
        .expect("symbol 'Size' not interned");

    assert!(
        matches!(
            ti.members.get(&size_sym),
            Some(ImplEntry::AssociatedType { ty }) if *ty == Type::U32_IDX
        ),
        "expected 'Size' → u32 in TraitImpl::members"
    );
}

#[test]
fn test_assoc_type_impl_bound_violation_is_error() {
    // `type Size = bool` where `Size: PointerSize` and `bool` does not
    // implement `PointerSize` → diagnostic.
    let case = TestCase::new(indoc! {"
        trait PointerSize {}
        impl PointerSize for u32 {}
        trait Memory {
            type Size: PointerSize;
        }
        struct Heap {}
        impl Memory for Heap {
            type Size = bool;
        }
    "});
    has_error(&case, "bool does not implement PointerSize");
}

#[test]
fn test_assoc_type_unconstrained_no_error() {
    // An associated type with no bounds accepts any concrete type.
    let case = TestCase::new(indoc! {"
        trait Container {
            type Item;
        }
        struct Bag {}
        impl Container for Bag {
            type Item = i32;
        }
    "});
    no_errors(&case);
}

#[test]
fn test_assoc_type_projection_forwarded_in_generic_wrapper() {
    // A generic wrapper that passes a `C::Item` argument to another function
    // also expecting `C::Item` must compile without errors.
    // Previously, the expected_type was silently dropped to None when the
    // receiver was itself a TypeParam, skipping the check entirely.
    let case = TestCase::new(indoc! {"
        trait Container {
            type Item;
        }
        fn process<C: Container>(item: C::Item) {
            unreachable
        }
        fn wrap<C: Container>(item: C::Item) {
            process(item)
        }
    "});
    no_errors(&case);
}

#[test]
fn test_assoc_type_projection_concrete_mismatch_in_generic_wrapper() {
    // Passing a concrete `i32` where `C::Item` is expected must be a type
    // error — even inside a generic wrapper where the receiver is a TypeParam.
    let case = TestCase::new(indoc! {"
        trait Container {
            type Item;
        }
        fn process<C: Container>(item: C::Item) {
            unreachable
        }
        fn wrap<C: Container>(item: C::Item, n: i32) {
            process(n)
        }
    "});
    has_error(&case, "type mismatch: expected C::Item, got i32");
}

#[test]
fn test_assoc_type_projection_in_nested_function_type_wrapper() {
    // Recursive substitution must also rebind projections nested inside
    // function types, not only top-level parameter and result types.
    let case = TestCase::new(indoc! {"
        trait Container {
            type Item;
        }
        fn process<C: Container>(f: fn(C::Item) -> C::Item) {
            unreachable
        }
        fn wrap<C: Container>(f: fn(C::Item) -> C::Item) {
            process(f)
        }
    "});
    no_errors(&case);
}

#[test]
fn test_assoc_type_projection_in_tuple_wrapper() {
    // Recursive substitution must also preserve projections nested inside
    // tuple elements.
    let case = TestCase::new(indoc! {"
        trait Container {
            type Item;
        }
        fn process<C: Container>(pair: (C::Item, C::Item)) {
            unreachable
        }
        fn wrap<C: Container>(pair: (C::Item, C::Item)) {
            process(pair)
        }
    "});
    no_errors(&case);
}

#[test]
fn test_assoc_type_projection_in_pointer_wrapper() {
    // Recursive substitution must also preserve projections nested under
    // pointer types.
    let case = TestCase::new(indoc! {"
        trait Container {
            type Item;
        }
        fn process<C: Container>(ptr: *C::Item) {
            unreachable
        }
        fn wrap<C: Container>(ptr: *C::Item) {
            process(ptr)
        }
    "});
    no_errors(&case);
}

#[test]
fn test_module_namespace_type_access() {
    // `module::Type` — a type accessed through a module namespace resolves
    // to the module's declared type without errors.
    let case = TestCase::new(indoc! {"
        module shapes {
            pub struct Circle {}
        }
        fn use_circle(c: shapes::Circle) {
            unreachable
        }
    "});
    no_errors(&case);
}

// ── Memory-tagged pointer types ──────────────────────────────────────────────

#[test]
fn test_memory_tagged_pointer() {
    // `heap::*i32` resolves to Type::Pointer { memory: Some(heap_id) }
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
            memory heap: Memory32;
            fn f(p: heap::*i32) {
                unreachable
            }
        "}
    ));
    no_errors(&case);

    let heap_id = case.tir.memories[0].id;
    let f = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("f"))
        .expect("function 'f' not found");

    let param_ty = f.params[0].ty.inner;
    assert!(
        matches!(
            case.tir.type_pool[param_ty as usize],
            Type::Pointer { mutable: false, memory: Some(id), .. } if id == heap_id
        ),
        "expected heap::*i32 (immutable pointer tagged with heap), got index {}",
        param_ty
    );
}

#[test]
fn test_memory_tagged_mut_pointer() {
    // `heap::*mut i64` — mutable pointer tagged with heap memory
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
            memory heap: Memory32;
            fn f(p: heap::*mut i64) {
                unreachable
            }
        "}
    ));
    no_errors(&case);

    let heap_id = case.tir.memories[0].id;
    let f = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("f"))
        .expect("function 'f' not found");

    let param_ty = f.params[0].ty.inner;
    assert!(
        matches!(
            case.tir.type_pool[param_ty as usize],
            Type::Pointer { mutable: true, memory: Some(id), .. } if id == heap_id
        ),
        "expected heap::*mut i64 (mutable pointer tagged with heap), got index {}",
        param_ty
    );
}

#[test]
fn test_memory_tagged_slice() {
    // `heap::[]u8` resolves to Type::Slice { memory: Some(heap_id) }
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
            memory heap: Memory32;
            fn f(s: heap::[]u8) {
                unreachable
            }
        "}
    ));
    no_errors(&case);

    let heap_id = case.tir.memories[0].id;
    let f = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("f"))
        .expect("function 'f' not found");

    let param_ty = f.params[0].ty.inner;
    assert!(
        matches!(
            case.tir.type_pool[param_ty as usize],
            Type::Slice { memory: Some(id), .. } if id == heap_id
        ),
        "expected heap::[]u8 (slice tagged with heap), got index {}",
        param_ty
    );
}

#[test]
fn test_memory_tagged_array() {
    // `heap::[4]u8` resolves to Type::Array { size: 4, memory: Some(heap_id) }
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
            memory heap: Memory32;
            fn f(a: heap::[4]u8) {
                unreachable
            }
        "}
    ));
    no_errors(&case);

    let heap_id = case.tir.memories[0].id;
    let f = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("f"))
        .expect("function 'f' not found");

    let param_ty = f.params[0].ty.inner;
    assert!(
        matches!(
            case.tir.type_pool[param_ty as usize],
            Type::Array { size: 4, memory: Some(id), .. } if id == heap_id
        ),
        "expected heap::[4]u8 (array tagged with heap), got index {}",
        param_ty
    );
}

#[test]
fn test_memory_tagged_nested_array() {
    // `heap::[4]heap::[4]u8` — outer array in heap, elements are heap-tagged arrays
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
            memory heap: Memory32;
            fn f(a: heap::[4]heap::[4]u8) {
                unreachable
            }
        "}
    ));
    no_errors(&case);

    let heap_id = case.tir.memories[0].id;
    let f = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("f"))
        .expect("function 'f' not found");

    let outer_ty = f.params[0].ty.inner;
    let (inner_ty, outer_tagged) = match &case.tir.type_pool[outer_ty as usize] {
        Type::Array {
            of,
            size: 4,
            memory: Some(id),
            ..
        } if *id == heap_id => (*of, true),
        _ => (0, false),
    };
    assert!(
        outer_tagged,
        "outer array should be tagged with heap memory"
    );
    assert!(
        matches!(
            case.tir.type_pool[inner_ty as usize],
            Type::Array { size: 4, memory: Some(id), .. } if id == heap_id
        ),
        "inner array should also be tagged with heap memory"
    );
}

#[test]
fn test_memory_tagged_non_pointer_is_error() {
    // `heap::i32` — memory namespace before a scalar type should error
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
            memory heap: Memory32;
            fn f(x: heap::i32) {
                unreachable
            }
        "}
    ));
    has_error(
        &case,
        "memory namespace can only prefix pointer, slice, or array",
    );
}

#[test]
fn test_untagged_and_tagged_pointer_are_distinct_types() {
    // `*i32` and `heap::*i32` must intern to different TypeIndex values.
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
            memory heap: Memory32;
            fn f(a: *i32, b: heap::*i32) {
                unreachable
            }
        "}
    ));
    no_errors(&case);

    let f = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("f"))
        .expect("function 'f' not found");

    let untagged = f.params[0].ty.inner;
    let tagged = f.params[1].ty.inner;
    assert_ne!(
        untagged, tagged,
        "*i32 and heap::*i32 must be distinct types"
    );
}

// ── FunctionItem type tests
// ───────────────────────────────────────────────────

#[test]
fn test_function_reference_has_function_item_type() {
    // When a function name is used as a value (not immediately called), the
    // resulting expression type must be `FunctionItem`, not `Function`. This
    // ensures the compiler preserves the function's identity rather than
    // exposing its raw (potentially TypeParam-polluted) signature.
    let case = TestCase::new(indoc! {"
        fn square(n: i32) -> i32 { n * n }
        fn main() {
            local f = square
        }
    "});
    no_errors(&case);

    let square_id = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("square"))
        .expect("function 'square' not found")
        .id;

    let has_function_item = case.tir.type_pool.iter().any(|t| {
        if let Type::FunctionItem { id, type_args } = t {
            *id == square_id && type_args.is_empty()
        } else {
            false
        }
    });
    assert!(
        has_function_item,
        "expected Type::FunctionItem for 'square' in the type pool"
    );
}

#[test]
fn test_generic_function_reference_has_function_item_not_fn_pointer() {
    // A reference to a generic function must produce `FunctionItem`, not
    // `Function { signature: fn(TypeParam{0}) -> TypeParam{0} }`. The old
    // representation leaked TypeParam internals and made it impossible to
    // distinguish which function was being referenced.
    let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T { t }
        fn main() {
            local f = identity
        }
    "});
    no_errors(&case);

    let identity_id = case
        .tir
        .functions
        .iter()
        .find(|f| case.interner.resolve(f.name.inner) == Some("identity"))
        .expect("function 'identity' not found")
        .id;

    let has_function_item = case
        .tir
        .type_pool
        .iter()
        .any(|t| matches!(t, Type::FunctionItem { id, .. } if *id == identity_id));
    assert!(
        has_function_item,
        "expected Type::FunctionItem for generic 'identity'"
    );

    // The function's own signature_index is still fn(TypeParam{0}) ->
    // TypeParam{0} in the pool (needed for the function body), but function
    // *reference* expressions must use FunctionItem, not expose that raw
    // signature as their value type.
}

#[test]
fn test_indirect_call_via_function_item_local_compiles() {
    // Storing a function in a local and calling it via the local is valid.
    // `f` has type `FunctionItem`, but calling it works because
    // `build_call_expression` resolves the signature through the function id.
    let case = TestCase::new(indoc! {"
        fn square(n: i32) -> i32 { n * n }
        fn main() -> i32 {
            local f = square
            f(5)
        }
    "});
    no_errors(&case);
}

#[test]
fn test_function_item_type_error_label_names_function() {
    // When a `FunctionItem` is passed where a concrete function-pointer type is
    // expected, the error label must name the function ("identity"), not show
    // its raw signature ("fn(T0) -> T0"). This verifies `display_type` for
    // `Type::FunctionItem` returns the function name.
    let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T { t }
        fn take_fn(f: fn(i32) -> i32) -> i32 { f(0) }
        fn main() -> i32 {
            take_fn(identity)
        }
    "});
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "expected a type error when passing FunctionItem where fn pointer expected"
    );
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| { d.labels.iter().any(|l| l.message.contains("identity")) }),
        "error label must name the function 'identity', not show raw TypeParam signature"
    );
}

#[test]
fn test_missing_argument_uses_callee_type_param_names() {
    let case = TestCase::new(indoc! {"
        fn take<T>(value: T) {
            unreachable
        }

        fn wrap<U>() {
            take()
        }
    "});

    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "expected missing argument error"
    );
    assert!(
        case.tir.diagnostics.iter().any(|d| {
            d.notes
                .iter()
                .any(|note| note.contains("argument #1 of type `T` is missing"))
        }),
        "missing argument diagnostic should use callee type parameter name `T`"
    );
}

#[test]
fn test_two_functions_have_distinct_function_item_types() {
    // Each distinct function must intern to a distinct `FunctionItem` TypeIndex.
    // Sharing a type between different functions would break identity-based
    // dispatch and type checking.
    let case = TestCase::new(indoc! {"
        fn square(n: i32) -> i32 { n * n }
        fn double(n: i32) -> i32 { n + n }
        fn main() {
            local a = square
            local b = double
        }
    "});
    no_errors(&case);

    let find_id = |name: &str| {
        case.tir
            .functions
            .iter()
            .find(|f| case.interner.resolve(f.name.inner) == Some(name))
            .unwrap_or_else(|| panic!("function '{}' not found", name))
            .id
    };
    let square_id = find_id("square");
    let double_id = find_id("double");

    let type_idx = |id: DefId| {
        case.tir
            .type_pool
            .iter()
            .enumerate()
            .find_map(|(i, t)| {
                if matches!(t, Type::FunctionItem { id: fid, .. } if *fid == id) {
                    Some(i as TypeIndex)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("FunctionItem for {:?} not found", id))
    };
    assert_ne!(
        type_idx(square_id),
        type_idx(double_id),
        "square and double must have distinct FunctionItem TypeIndex values"
    );
}

#[test]
fn test_function_item_coerces_to_matching_fn_pointer_type() {
    // A FunctionItem must be implicitly coercible to a `fn(...)` parameter
    // whose signature matches exactly. This is the `func_pointers.wx` pattern:
    // passing a named function where a function-pointer argument is expected.
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn sub(a: i32, b: i32) -> i32 { a - b }
        fn apply(binop: fn(i32, i32) -> i32, a: i32, b: i32) -> i32 {
            binop(a, b)
        }
        fn main() -> i32 {
            local a = apply(add, 5, 10)
            local b = apply(sub, 10, 5)
            a + b
        }
    "});
    no_errors(&case);
}

#[test]
fn test_function_item_wrong_signature_is_error() {
    // A FunctionItem must NOT coerce to a `fn(...)` type with a different
    // signature — the arity or parameter types must match exactly.
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn apply(binop: fn(i32) -> i32, n: i32) -> i32 { binop(n) }
        fn main() -> i32 {
            apply(add, 5)
        }
    "});
    has_error(
        &case,
        "type mismatch: add (fn(i32,i32)->i32) passed where fn(i32)->i32 expected",
    );
}

// ── Pointer dereference ──────────────────────────────────────────────────────

#[test]
fn test_deref_load_through_pointer() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;
        fn read(ptr: heap::*i32) -> i32 { ptr.* }
    "}
    );
    let case = TestCase::new(&src);
    no_errors(&case);
}

#[test]
fn test_deref_store_through_mutable_pointer() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;
        fn write(ptr: heap::*mut i32) { ptr.* = 42 }
    "}
    );
    let case = TestCase::new(&src);
    no_errors(&case);
}

#[test]
fn test_deref_arithmetic_assignment_through_mutable_pointer() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;
        fn increment(ptr: heap::*mut i32) { ptr.* += 1 }
    "}
    );
    let case = TestCase::new(&src);
    no_errors(&case);
}

#[test]
fn test_deref_non_pointer_type_is_error() {
    let case = TestCase::new(indoc! {"
        fn bad(x: i32) -> i32 { x.* }
    "});
    assert!(
        has_error_code(&case.tir, "E1037"),
        "expected E1037 (dereference of non-pointer type)"
    );
}

#[test]
fn test_deref_no_memory_is_error() {
    let case = TestCase::new(indoc! {"
        fn bad(ptr: *i32) -> i32 { ptr.* }
    "});
    assert!(
        has_error_code(&case.tir, "E1038"),
        "expected E1038 (no memory for pointer)"
    );
}

#[test]
fn test_deref_store_through_immutable_pointer_is_error() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;
        fn bad(ptr: heap::*i32) { ptr.* = 42 }
    "}
    );
    let case = TestCase::new(&src);
    has_error_matching(&case, "immutable pointer");
}

#[test]
fn test_deref_arithmetic_assignment_through_immutable_pointer_is_error() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;
        fn bad(ptr: heap::*i32) { ptr.* += 1 }
    "}
    );
    let case = TestCase::new(&src);
    has_error_matching(&case, "immutable pointer");
}

#[test]
fn test_deref_type_mismatch_on_store_is_error() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;
        fn bad(ptr: heap::*mut i32) { ptr.* = true }
    "}
    );
    let case = TestCase::new(&src);
    has_error_matching(&case, "cannot assign");
}
