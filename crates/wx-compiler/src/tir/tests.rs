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
    let abs_entry = members.get(&abs_sym).expect("`abs` missing from impl_members");
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
    let &ImplEntry::Method(abs_idx) = abs_entry else { unreachable!() };
    let &ImplEntry::AssociatedFn(from_bool_idx) = from_bool_entry else { unreachable!() };
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

// ── Memory namespace resolution ───────────────────────────────────────────────
//
// Memory32/Memory64 must now be declared as traits before use. These preamble
// definitions are the minimal form (abstract methods, no default bodies) for
// testing purposes.

const STD: &str = indoc! {"
    trait Memory32 {
        const OFFSET: u32;
        const MEMORY_INDEX: u32;

        #[inline]
        fn size(self) -> u32 {
            wasm::memory32_size(self)
        }
        #[inline]
        fn grow(self, delta: u32) -> u32 {
            wasm::memory32_grow(self, delta)
        }
    }

    trait Memory64 {
        const OFFSET: u64;
        const MEMORY_INDEX: u64;

        #[inline]
        fn size(self) -> u64 {
            wasm::memory64_size(self)
        }
        #[inline]
        fn grow(self, delta: u64) -> u64 {
            wasm::memory64_grow(self, delta)
        }
    }

    module wasm {
        #[intrinsic]
        pub fn memory64_grow(mem: impl Memory64, delta: u64) -> u64;
        #[intrinsic]
        pub fn memory64_size(mem: impl Memory64) -> u64;

        #[intrinsic]
        pub fn memory32_grow(mem: impl Memory32, delta: u32) -> u32;
        #[intrinsic]
        pub fn memory32_size(mem: impl Memory32) -> u32;
    }
"};

#[test]
fn test_memory_offset_resolves_to_u32() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        pub fn f() -> u32 { MEM::OFFSET }
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
    // Method-call syntax: MEM is a value; size is a method from the Memory32 trait.
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        pub fn f() -> u32 { MEM.size() }
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
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        pub fn f() -> u32 { MEM.grow(1) }
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
fn test_memory64_size_and_grow_use_u64() {
    // Memory64 grow/size take and return u64, not u32.
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory64;
        pub fn f() -> u64 { MEM.grow(1) }
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
    // Memory identifiers are valid value expressions (for method calls like MEM.grow(1)).
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

#[test]
fn test_memory_grow_wrong_arg_type_is_error() {
    // grow expects u32 for Memory32; passing i32 must produce a type mismatch.
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory MEM: Memory32;
        fn f(delta: i32) { _ = MEM.grow(delta); }
    "}
    );
    let case = TestCase::new(&src);
    assert!(
        case.tir
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("E1001")),
        "expected type mismatch for wrong grow argument type"
    );
}

// ── impl Trait errors ─────────────────────────────────────────────────────────

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

// ── wasm module intrinsics ────────────────────────────────────────────────────

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

// ── cyclic type dependency tests ──────────────────────────────────────────────

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
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics for valid forward reference: {:?}",
        case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_struct_forward_reference_reversed_order_resolves() {
    // Same as above but B declared first — both orderings must work.
    let case = TestCase::new(indoc! {"
        struct B { val: i32 }
        struct A { b: B }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
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
