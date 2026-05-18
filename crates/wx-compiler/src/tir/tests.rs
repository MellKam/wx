use indoc::indoc;

use super::*;
use crate::ast::{AST, Files};

#[allow(unused)]
struct TestCase {
    interner: ast::StringInterner,
    files: Files,
    ast: AST,
    tir: TIR,
}

impl<'case> TestCase {
    fn new(source: &str) -> Self {
        let mut interner = ast::StringInterner::new();
        let mut files = Files::new();

        let file_id = files
            .add("main.wx".to_string(), source.to_string())
            .unwrap();
        let ast = ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        let tir = TIR::build(&[&ast], &mut interner);

        TestCase {
            interner,
            files,
            ast,
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
    assert!(matches!(members[&size_sym], ImplEntry::AssociatedConst { .. }));
    assert!(matches!(members[&align_sym], ImplEntry::AssociatedConst { .. }));
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

/// Struct fields are reordered for optimal memory layout.
/// `struct Mixed { a: bool, b: i64, c: u32, d: f64 }` declared in that order
/// has C-layout size 28B; after reordering (i64, f64, u32, bool) it is 24B.
#[test]
fn test_struct_field_reorder_reduces_size() {
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
    assert!(case.tir.diagnostics.is_empty());

    let mixed_sym = case.interner.get("Mixed").unwrap();
    let mixed_ti = case
        .tir
        .type_pool
        .iter()
        .position(|t| {
            if let Type::Struct { struct_index } = t {
                case.tir.structs[*struct_index as usize].name.inner == mixed_sym
            } else {
                false
            }
        })
        .unwrap() as u32;

    let layout = case.tir.layout_of(mixed_ti, PointerSize::P32);
    // Optimal order: i64(8), f64(8), u32(4), bool(1) + 3B padding = 24B
    assert_eq!(
        layout.size, 24,
        "expected 24B after reordering, got {}",
        layout.size
    );
    assert_eq!(layout.align, 8);

    // Verify physical field order in the struct
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
    let s = &case.tir.structs[struct_index as usize];
    let field_names: Vec<&str> = s
        .fields
        .iter()
        .map(|f| case.interner.resolve(f.name.inner).unwrap())
        .collect();
    // Largest alignment (8B: i64, f64) first, then 4B (u32), then 1B (bool)
    assert_eq!(field_names, vec!["b", "d", "c", "a"]);
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
    let case32 = TestCase::new("memory MEM: Memory32;");
    assert!(case32.tir.diagnostics.is_empty(), "unexpected diagnostics");
    assert_eq!(case32.tir.declared_memories, vec![MemoryKind::Memory32]);

    let case64 = TestCase::new("memory MEM: Memory64;");
    assert!(case64.tir.diagnostics.is_empty(), "unexpected diagnostics");
    assert_eq!(case64.tir.declared_memories, vec![MemoryKind::Memory64]);
}

#[test]
fn test_memory_invalid_kind_is_error() {
    let case = TestCase::new("memory MEM: i32;");
    assert!(
        case.tir.diagnostics.iter().any(|d| d.code.as_deref() == Some("E1001")),
        "expected type mismatch diagnostic for invalid memory kind"
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

// ── Memory namespace resolution ───────────────────────────────────────────────

#[test]
fn test_memory_offset_resolves_to_u32() {
    let case = TestCase::new(indoc! {"
        memory MEM: Memory32;
        fn f() -> u32 {
            MEM::OFFSET
        }
    "});
    assert!(case.tir.diagnostics.is_empty(), "unexpected diagnostics: {:?}", case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
}

#[test]
fn test_memory_size_call_resolves() {
    let case = TestCase::new(indoc! {"
        memory MEM: Memory32;
        fn f() -> u32 {
            MEM::size()
        }
    "});
    assert!(case.tir.diagnostics.is_empty(), "unexpected diagnostics: {:?}", case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
}

#[test]
fn test_memory_grow_call_resolves() {
    let case = TestCase::new(indoc! {"
        memory MEM: Memory32;
        fn f() -> u32 {
            MEM::grow(1)
        }
    "});
    assert!(case.tir.diagnostics.is_empty(), "unexpected diagnostics: {:?}", case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
}

#[test]
fn test_memory64_size_and_grow_use_u64() {
    // Memory64 grow/size take and return u64, not u32.
    let case = TestCase::new(indoc! {"
        memory MEM: Memory64;
        fn f() -> u64 {
            MEM::grow(1)
        }
    "});
    assert!(case.tir.diagnostics.is_empty(), "unexpected diagnostics: {:?}", case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
}

#[test]
fn test_memory_unknown_member_is_error() {
    let case = TestCase::new(indoc! {"
        memory MEM: Memory32;
        fn f() {
            _ = MEM::pages;
        }
    "});
    assert!(
        case.tir.diagnostics.iter().any(|d| d.code.as_deref() == Some("E1007")),
        "expected undeclared identifier diagnostic for unknown memory member"
    );
}

#[test]
fn test_memory_used_as_value_is_error() {
    let case = TestCase::new(indoc! {"
        memory MEM: Memory32;
        fn f() {
            _ = MEM;
        }
    "});
    assert!(
        case.tir.diagnostics.iter().any(|d| d.code.as_deref() == Some("E1030")),
        "expected namespace-used-as-value diagnostic"
    );
}

#[test]
fn test_memory_grow_wrong_arg_type_is_error() {
    // grow expects u32 for Memory32; passing i32 must produce a type mismatch.
    let case = TestCase::new(indoc! {"
        memory MEM: Memory32;
        fn f() {
            _ = MEM::grow(1i32);
        }
    "});
    // The argument literal `1i32` resolves to i32; grow expects u32 → type mismatch.
    assert!(
        case.tir.diagnostics.iter().any(|d| d.code.as_deref() == Some("E1001")),
        "expected type mismatch for wrong grow argument type"
    );
}
