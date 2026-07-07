use std::collections::HashMap;

use codespan_reporting::diagnostic::Severity;
use indoc::indoc;

use super::*;
use crate::tir::builder::{
	CharLiteralError, parse_char_literal, unescape_string,
};
use crate::vfs;

#[allow(unused)]
struct TestCase {
	graph: vfs::CompilationGraph,
	tir: TIR,
}

impl TestCase {
	fn new(source: &str) -> Self {
		let mut builder = vfs::CompilationGraphBuilder::new();
		let stdlib_id = builder.load_stdlib();
		let prefixed = format!("use std::*;\n{source}");
		let root_id = builder
			.load_binary(
				"main.wx".to_string(),
				&vfs::VirtualFileSource::new(HashMap::from([(
					"main.wx".to_string(),
					prefixed,
				)])),
			)
			.unwrap();
		let mut graph = builder.build(root_id, stdlib_id);
		let tir = TIR::build(&mut graph);
		TestCase { graph, tir }
	}

	fn new_multi_file(
		entry_path: &str,
		source: &str,
		extra_files: &[(&str, &str)],
	) -> Self {
		let prefixed_entry = format!("use std::*;\n{source}");
		let mut workspace_files =
			HashMap::from([(entry_path.to_string(), prefixed_entry)]);
		for (path, source) in extra_files {
			workspace_files.insert((*path).to_string(), (*source).to_string());
		}

		let mut builder = vfs::CompilationGraphBuilder::new();
		let stdlib_id = builder.load_stdlib();
		let root_id = builder
			.load_binary(
				entry_path.to_string(),
				&vfs::VirtualFileSource::new(workspace_files),
			)
			.unwrap();
		let mut graph = builder.build(root_id, stdlib_id);
		let tir = TIR::build(&mut graph);
		TestCase { graph, tir }
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
		case.tir.functions.iter().any(|function| case
			.graph
			.interner
			.resolve(function.name.inner)
			== Some("add"))
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

	assert!(
		has_error_code(&case.tir, DiagnosticCode::DuplicateExport),
		"expected E1018 (DuplicateExport), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
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

	assert!(
		has_error_code(&case.tir, DiagnosticCode::DuplicateExport),
		"expected E1018 (DuplicateExport), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
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
            fn log(ptr: u32, len: u32) -> ();
        }

        fn main() {
            console::log(0, 0);
        }

        export { main }
    "});
	insta::assert_yaml_snapshot!(case.tir);
}

#[test]
fn test_imported_global() {
	let case = TestCase::new(indoc! {"
        import \"env\" {
            global counter: i32;
            global mut flag: bool;
        }

        fn read() -> i32 {
            env::counter
        }

        export { read }
    "});
	// TODO: change to diagnostics.is_empty() once unused-warning for lib/stdlib
	// items is fixed
	assert!(
		!case
			.tir
			.diagnostics
			.iter()
			.any(|d| d.severity
				== codespan_reporting::diagnostic::Severity::Error)
	);
	// Both imported globals land in tir.globals with no value and namespace
	// pointing to the import block.
	assert_eq!(case.tir.globals.len(), 2);
	assert!(case.tir.globals.iter().all(|g| g.value.is_none()));
	assert!(
		case.tir
			.globals
			.iter()
			.all(|g| case.tir.is_import_namespace(g.namespace))
	);
	// They appear in the import_decl lookup.
	let decl = &case.tir.import_decls[0];
	assert_eq!(decl.lookup.len(), 2);
}

#[test]
fn test_local_variable_used_in_import_call() {
	let case = TestCase::new(indoc! {"
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

#[test]
fn test_local_with_type_annotation_invalid_rhs_recovers() {
	// When the RHS of a `local` declaration fails to resolve (e.g. unknown function),
	// the checker must still register the local with the declared type so that
	// subsequent uses don't cascade into "undeclared identifier" errors.
	let case = TestCase::new(indoc! {"
        fn use_ptr(x: u32) -> u32 { x }
        fn main() -> u32 {
            local p: u32 = unknown_fn()
            use_ptr(p)
        }
        export { main }
    "});
	// Exactly one error: unknown_fn is undeclared. No cascading error for `p`.
	assert_eq!(case.tir.diagnostics.len(), 1);
	assert!(has_error_code(
		&case.tir,
		DiagnosticCode::UndeclaredIdentifier
	));
}

#[test]
fn test_local_with_pointer_type_annotation_dereference_recovers() {
	// When the RHS errors (e.g. `alloc` is undeclared), the local must still carry
	// the declared pointer type so that `n.*` doesn't cascade into a "not a pointer" error.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 }
        struct Node { x: i32 }
        fn write(x: i32) {
            local p: heap::*mut Node = alloc_node()
            p.*.x = x
        }
        export { write }
    "});
	// Only one error: alloc_node is undeclared. No cascading pointer/field errors.
	assert_eq!(case.tir.diagnostics.len(), 1);
	assert!(has_error_code(
		&case.tir,
		DiagnosticCode::UndeclaredIdentifier
	));
}

#[test]
fn test_assign_to_undeclared_identifier_no_e1013() {
	// Assignment to an undeclared variable should produce only E1007 (undeclared
	// identifier), not a cascading E1013 (invalid assignment target).
	let case = TestCase::new(indoc! {"
        fn f() {
            undeclared_var = 42
        }
        export { f }
    "});
	assert_eq!(case.tir.diagnostics.len(), 1);
	assert!(has_error_code(
		&case.tir,
		DiagnosticCode::UndeclaredIdentifier
	));
}

#[test]
fn test_compare_mutable_pointer_with_null() {
	// `cur == ptr::null()` must infer M and T for null<M,T>() from the type of `cur`
	// (`heap::*Node`), even though null()'s return type is an immutable pointer.
	// Previously `infer_type_args` required matching mutability, causing E1002.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 }
        struct Node { x: i32 }
        fn is_null(p: heap::*Node) -> bool {
            p == ptr::null()
        }
        export { is_null }
    "});
	assert!(case.tir.diagnostics.is_empty());
}

fn has_error_code(tir: &TIR, code: DiagnosticCode) -> bool {
	tir.diagnostics
		.iter()
		.any(|d| d.code.as_deref() == Some(code.code()))
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
		has_error_code(&case.tir, DiagnosticCode::IntegerLiteralOutOfRange),
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
	// `-1` is `Unary { InvertSign, Int(1) }` — coerce_untyped_unary_expr only
	// allows InvertSign for i32/i64, so u32 produces E1005 (UnableToCoerce).
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UnableToCoerce),
		"expected E1005 (UnableToCoerce) for negated literal coerced to u32, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_coerce_int_literal_for_float_type_errors() {
	// An untyped integer literal cannot be coerced to f32 (must write 1.0)
	let case = TestCase::new("fn f() -> f32 { 1 } export { f }");
	assert!(
		has_error_code(&case.tir, DiagnosticCode::LiteralTypeMismatch),
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
		has_error_code(&case.tir, DiagnosticCode::UnableToCoerce),
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
		has_error_code(&case.tir, DiagnosticCode::UnableToCoerce),
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

// ── direct coercion to small integer types ───────────────────────────────

#[test]
fn test_coerce_int_to_i8() {
	let case = TestCase::new("fn f() -> i8 { 127 } export { f }");
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
fn test_coerce_int_to_u8() {
	let case = TestCase::new("fn f() -> u8 { 255 } export { f }");
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
fn test_coerce_int_to_i16() {
	let case = TestCase::new("fn f() -> i16 { 1000 } export { f }");
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
fn test_coerce_int_to_u16() {
	let case = TestCase::new("fn f() -> u16 { 65535 } export { f }");
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

// ── float binary arithmetic propagation ─────────────────────────────────

#[test]
fn test_coerce_binary_arithmetic_f32() {
	let case = TestCase::new("fn f() -> f32 { 1.5 + 0.5 } export { f }");
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
fn test_coerce_binary_arithmetic_f64() {
	let case = TestCase::new("fn f() -> f64 { 1.0 + 2.0 } export { f }");
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
fn test_coerce_binary_float_multiply() {
	let case = TestCase::new("fn f() -> f64 { 2.0 * 3.0 } export { f }");
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

// ── INTEGER + FLOAT mismatch ─────────────────────────────────────────────

#[test]
fn test_integer_plus_float_literal_errors() {
	// 1 is INTEGER, 1.0 is FLOAT — different comptime kinds → type mismatch
	let case = TestCase::new("fn f() -> i32 { 1 + 1.0 } export { f }");
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeMistmatch),
		"expected E1001 (type mismatch for INTEGER + FLOAT), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_float_plus_integer_literal_errors() {
	// Symmetric: FLOAT on the left, INTEGER on the right
	let case = TestCase::new("fn f() -> f64 { 1.0 + 1 } export { f }");
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeMistmatch),
		"expected E1001 (type mismatch for FLOAT + INTEGER), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

// ── if without else ──────────────────────────────────────────────────────

#[test]
fn test_if_without_else_returning_value_is_error() {
	let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            local x: i32 = if true { 5 };
            x
        }
    "});
	assert!(has_error_code(&case.tir, DiagnosticCode::MissingElseBlock));
}

#[test]
fn test_if_without_else_unit_body_is_ok() {
	let case = TestCase::new(indoc! {"
        fn f() {
            if true { local x: i32 = 1; }
        }
    "});
	no_errors(&case);
}

// ── chained (nested) comptime binary expressions ─────────────────────────

#[test]
fn test_coerce_chained_integer_arithmetic() {
	// All three literals are INTEGER; type propagates through both additions
	let case = TestCase::new("fn f() -> i32 { 1 + 2 + 3 } export { f }");
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
fn test_coerce_chained_float_arithmetic() {
	let case = TestCase::new("fn f() -> f64 { 1.0 + 2.0 + 3.0 } export { f }");
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

// ── typed operand drives coercion of comptime literal ────────────────────

#[test]
fn test_comptime_right_operand_coerced_by_typed_left() {
	// x has concrete type i32; literal `1` (INTEGER) on the right gets coerced
	let case = TestCase::new("fn f(x: i32) -> i32 { x + 1 } export { f }");
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
fn test_comptime_left_operand_coerced_by_typed_right() {
	// literal `1` (INTEGER) on the left, x has concrete type i32 on the right
	let case = TestCase::new("fn f(x: i32) -> i32 { 1 + x } export { f }");
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
fn test_comptime_float_operand_coerced_by_typed_variable() {
	let case = TestCase::new("fn f(x: f64) -> f64 { x + 1.0 } export { f }");
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

// ── coercion through local variable binding ──────────────────────────────

#[test]
fn test_comptime_integer_coerced_in_local_binding() {
	let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            local x: i32 = 1 + 2;
            x
        }
        export { f }
    "});
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
fn test_comptime_float_coerced_in_local_binding() {
	let case = TestCase::new(indoc! {"
        fn f() -> f64 {
            local x: f64 = 1.0 + 2.0;
            x
        }
        export { f }
    "});
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
fn test_comptime_integer_local_missing_annotation_errors() {
	// No type annotation on the binding and no outer context → type annotation
	// required
	let case = TestCase::new(indoc! {"
        fn f() {
            local x = 1 + 2;
        }
        export { f }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeAnnotationRequired),
		"expected E1002 (type annotation required), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

// ── coercion through function call arguments ─────────────────────────────

#[test]
fn test_comptime_literal_coerced_by_fn_param_type() {
	let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn f() -> i32 { add(1, 2) }
        export { f }
    "});
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
fn test_comptime_float_literal_coerced_by_fn_param_type() {
	let case = TestCase::new(indoc! {"
        fn scale(x: f32, factor: f32) -> f32 { x * factor }
        fn f(x: f32) -> f32 { scale(x, 2.0) }
        export { f }
    "});
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

// ── comparison operators with comptime literals ──────────────────────────

#[test]
fn test_comptime_integers_standalone_comparison_requires_annotation() {
	// `1 == 2` has no type context: cannot decide i32.eq vs i64.eq → E1014
	let case = TestCase::new("fn f() -> bool { 1 == 2 } export { f }");
	assert!(
		has_error_code(
			&case.tir,
			DiagnosticCode::ComparisonTypeAnnotationRequired
		),
		"expected E1014 (comparison annotation required), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_comptime_integer_coerced_by_typed_comparand() {
	// Typed variable on the left drives coercion of the literal on the right
	let case = TestCase::new("fn f(x: i32) -> bool { x == 1 } export { f }");
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
fn test_comptime_integer_coerced_by_typed_comparand_on_right() {
	let case = TestCase::new("fn f(x: i32) -> bool { 1 == x } export { f }");
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
fn test_comptime_float_coerced_by_typed_comparand() {
	let case = TestCase::new("fn f(x: f64) -> bool { x < 1.0 } export { f }");
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
fn test_comptime_integer_vs_float_comparison_errors() {
	// When both sides are comptime numbers (INTEGER and FLOAT), the comparison
	// builder emits E1014 (ComparisonTypeAnnotationRequired) since neither side
	// has a concrete type to drive resolution.
	let case = TestCase::new("fn f() -> bool { 1 == 1.0 } export { f }");
	assert!(
		has_error_code(
			&case.tir,
			DiagnosticCode::ComparisonTypeAnnotationRequired
		),
		"expected E1014 (ComparisonTypeAnnotationRequired) for INTEGER == FLOAT, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
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
		has_error_code(&case.tir, DiagnosticCode::DuplicateStructField),
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
		has_error_code(&case.tir, DiagnosticCode::UndeclaredType),
		"expected E1021 (UndeclaredType) for unknown struct name, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
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
		has_error_code(&case.tir, DiagnosticCode::UnknownStructField),
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
		has_error_code(&case.tir, DiagnosticCode::DuplicateStructFieldInit),
		"expected E1026 (duplicate field in init), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
	// x was mentioned (just duplicated) — must NOT also appear as missing
	assert!(
		!has_error_code(&case.tir, DiagnosticCode::MissingStructFields),
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
		has_error_code(&case.tir, DiagnosticCode::MissingStructFields),
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
	// Should have E1001 (TypeMistmatch) for field `x` receiving a bool instead of i32.
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeMistmatch),
		"expected E1001 (TypeMistmatch) for bool assigned to i32 field, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
	// …but must NOT report `x` as a missing field
	let missing_x = case.tir.diagnostics.iter().any(|d| {
		d.code.as_deref() == Some(DiagnosticCode::MissingStructFields.code())
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

/// Methods on built-in types defined in stdlib are callable from user code.
#[test]
fn test_stdlib_method_callable() {
	let case = TestCase::new(indoc! {"
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
		.get(&TypeIndex::I32)
		.expect("impl_members should have an entry for i32");

	let abs_sym = case
		.graph
		.interner
		.get("abs")
		.expect("symbol `abs` not interned");
	let from_bool_sym = case
		.graph
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

	let mixed_sym = case.graph.interner.get("Mixed").unwrap();
	let struct_index = case
		.tir
		.types
		.iter()
		.find_map(|t| {
			if let Type::Struct { struct_index, .. } = t {
				if case.tir.structs[*struct_index as usize].name.inner
					== mixed_sym
				{
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
		.map(|f| case.graph.interner.resolve(f.name.inner).unwrap())
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
	let case32 = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory MEM: Memory where { Size = u32 };
    "},
		&[],
	);
	assert!(case32.tir.diagnostics.is_empty(), "unexpected diagnostics");
	assert_eq!(
		case32
			.tir
			.memories
			.iter()
			.map(|m| m.kind)
			.collect::<Vec<_>>(),
		vec![TypeIndex::U32]
	);

	let case64 = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory MEM: Memory where { Size = u64 };
    "},
		&[],
	);
	assert!(case64.tir.diagnostics.is_empty(), "unexpected diagnostics");
	assert_eq!(
		case64
			.tir
			.memories
			.iter()
			.map(|m| m.kind)
			.collect::<Vec<_>>(),
		vec![TypeIndex::U64]
	);
}

#[test]
fn test_memory_invalid_kind_is_error() {
	let case = TestCase::new("memory MEM: i32;");
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::InvalidMemoryKind.code())),
		"expected invalid memory kind diagnostic"
	);
}

#[test]
fn test_memory_missing_std_import_does_not_panic() {
	// Regression test: without `use std::*;` in scope, `Memory` is an
	// unresolved trait bound, so the memory's kind can't be determined.
	// This must not leave `MEM` stuck as `SymbolKind::Pending`, which
	// used to panic (`unreachable!`) as soon as anything referenced it.
	let mut builder = vfs::CompilationGraphBuilder::new();
	let stdlib_id = builder.load_stdlib();
	let root_id = builder
		.load_binary(
			"main.wx".to_string(),
			&vfs::VirtualFileSource::new(HashMap::from([(
				"main.wx".to_string(),
				indoc! {"
                    memory MEM: Memory where { Size = u32 };
                    pub fn f() -> u32 { MEM::MEMORY_INDEX }
                "}
				.to_string(),
			)])),
		)
		.unwrap();
	let mut graph = builder.build(root_id, stdlib_id);
	let tir = TIR::build(&mut graph);
	assert!(
		tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::InvalidMemoryKind.code())),
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
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::MissingFunctionBody.code())),
		"expected E0011 diagnostic for missing function body"
	);
}

#[test]
fn test_memory_index_const_resolves() {
	// `MEM::MEMORY_INDEX` — namespace access to a memory constant resolves cleanly.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory MEM: Memory where { Size = u32 };
        pub fn f() -> u32 { MEM::MEMORY_INDEX }
    "},
		&[],
	);
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
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory MEM: Memory where { Size = u32 };
        pub fn f() { _ = MEM.size(); }
    "},
		&[],
	);
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
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory MEM: Memory where { Size = u32 };
        pub fn f() { _ = MEM.grow(1); }
    "},
		&[],
	);
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
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory MEM: Memory where { Size = u32 };
        fn f() { _ = MEM::pages; }
    "},
		&[],
	);
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UndeclaredIdentifier.code())),
		"expected undeclared identifier diagnostic for unknown memory member"
	);
}

#[test]
fn test_memory_as_value_in_expression() {
	// Memory identifiers are valid value expressions (for method calls like
	// MEM.grow(1)).
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory MEM: Memory where { Size = u32 };
        fn f() { _ = MEM; }
    "},
		&[],
	);
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::NamespaceUsedAsValue.code())),
		"memory identifier should be usable as a value expression"
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

	let draw_sym = case
		.graph
		.interner
		.get("draw")
		.expect("symbol `draw` not interned");

	// Find the impl that contains `draw` — avoids hardcoding impl indices
	// (stdlib adds its own impls before user ones).
	let ti = case
		.tir
		.trait_impls
		.iter()
		.find(|ti| ti.members.contains_key(&draw_sym))
		.expect("no TraitImpl has 'draw' method");

	// target type is Point (a struct)
	assert!(
		matches!(case.tir.types[ti.target.as_usize()], Type::Struct { .. }),
		"target should be a struct type"
	);

	let point_type = ti.target;
	let drawable_index = ti.trait_index;

	// trait_impl_lookup is queryable for (Point, Drawable)
	assert!(
		case.tir
			.trait_impl_lookup
			.contains_key(&(point_type, drawable_index)),
		"trait_impl_lookup should contain (Point, Drawable)"
	);

	// type_trait_impls maps Point → a list that includes this impl
	let ti_index = case.tir.trait_impl_lookup[&(point_type, drawable_index)];
	assert!(
		case.tir
			.type_trait_impls
			.get(&point_type)
			.map(|v| v.contains(&ti_index))
			.unwrap_or(false),
		"type_trait_impls should include the Drawable impl for Point"
	);

	// draw method is registered in TraitImpl.members
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
		.graph
		.interner
		.get("hello")
		.expect("symbol `hello` not interned");
	let ti = case
		.tir
		.trait_impls
		.iter()
		.find(|ti| ti.members.contains_key(&hello_sym))
		.expect("no TraitImpl has 'hello' method");

	let func_index = match ti.members.get(&hello_sym) {
		Some(ImplEntry::Method(fi)) => *fi,
		other => panic!("expected Method entry, got {:?}", other),
	};
	assert!(
		matches!(
			case.tir.functions[func_index as usize].type_param_parent,
			None
		),
		"method inside trait impl block doens't need to inherit self"
	);
}

// ── trait duplicate definition ────────────────────────────────────────────────

#[test]
fn test_duplicate_trait_definition_is_error() {
	let case = TestCase::new(indoc! {"
        trait Foo { }
        trait Foo { }
        export { }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::DuplicateDefinition),
		"expected duplicate definition error for two traits with same name, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_local_trait_silently_shadows_wildcard_import() {
	// Defining a trait with the same name as one from `use std::*` is allowed —
	// local definitions always win over wildcard imports without a diagnostic.
	let case = TestCase::new(indoc! {"
        trait PointerSize { }
        export { }
    "});
	assert!(
		!has_error_code(&case.tir, DiagnosticCode::DuplicateDefinition),
		"local trait shadowing wildcard import should not produce a duplicate error"
	);
}

#[test]
fn test_trait_default_body_referencing_sibling_method_does_not_panic() {
	// Regression: ensure_signature for TraitFunction left a Pending entry in the
	// namespace. When a default method body referenced another trait method by
	// name, lookup_global_symbol returned that Pending, reaching an unreachable!()
	// in global_symbol_to_expression.
	let _case = TestCase::new(indoc! {"
        trait Counter {
            fn step() -> i32;
            fn doubled() -> i32 { step() + step() }
        }
    "});
	// Test passes as long as it does not panic.
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
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::MissingTraitImplItem.code())),
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
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::MissingTraitImplItem.code())),
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
		.position(|t| {
			case.graph.interner.resolve(t.name.inner) == Some("Drawable")
		})
		.expect("Drawable not found") as u32;
	let sized_idx = case
		.tir
		.traits
		.iter()
		.position(|t| {
			case.graph.interner.resolve(t.name.inner) == Some("Sized")
		})
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
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::MissingSupertraitImpl.code())),
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

// ── demand-driven forward reference resolution
// ─────────────────────────────────────────────

#[test]
fn test_forward_ref_resolves_on_demand() {
	// The query system resolves trait forward-references on demand. Using a trait
	// name directly as a type is now invalid (traits are bounds, not types), but
	// the resolution must still find the trait — producing ExpectedTrait (E1031),
	// NOT UndeclaredType (E1021). E1021 would mean the forward reference was
	// never resolved at all.
	let case = TestCase::new(indoc! {"
        fn uses_memory32(mem: Memory32, delta: u32) -> u32 {
            mem.grow(delta)
        }

        trait Memory32 {
            fn grow(self, delta: u32) -> u32;
        }
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UndeclaredType.code())),
		"E1021 should not be emitted: the query system resolves traits on demand"
	);
	assert!(
		case.tir
			.diagnostics
			.iter()
			.any(|d| d.code.as_deref()
				== Some(DiagnosticCode::ExpectedTrait.code())),
		"E1031 should be emitted: traits cannot be used directly as types"
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
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::CyclicTypeDependency.code())),
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
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::CyclicTypeDependency.code())),
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
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::CyclicTypeDependency.code())),
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
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::CyclicTypeDependency.code())),
		"expected E1032 for Bad"
	);
	// Good should still be registered; the function should compile without
	// an undeclared-type error.
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UndeclaredType.code())),
		"Good struct should still resolve despite Bad being cyclic"
	);
}

// ── Generic structs
// ───────────────────────────────────────────────────────────

#[test]
fn test_generic_struct_definition_stores_type_params() {
	let case = TestCase::new(indoc! {"
        struct Point<T> { x: T, y: T }
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| {
			d.severity == codespan_reporting::diagnostic::Severity::Error
		})
		.collect();
	assert!(errors.is_empty(), "{:?}", errors);
	let s = case
		.tir
		.structs
		.iter()
		.find(|s| case.graph.interner.resolve(s.name.inner) == Some("Point"))
		.expect("Point struct not found");
	assert_eq!(s.type_params.len(), 1);
}

#[test]
fn test_generic_struct_field_type_is_type_param() {
	let case = TestCase::new(indoc! {"
        struct Wrapper<T> { value: T }
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| {
			d.severity == codespan_reporting::diagnostic::Severity::Error
		})
		.collect();
	assert!(errors.is_empty(), "{:?}", errors);
	let s = case
		.tir
		.structs
		.iter()
		.find(|s| case.graph.interner.resolve(s.name.inner) == Some("Wrapper"))
		.expect("Wrapper struct not found");
	// Field `value` should have type TypeParam { param_index: 0 }.
	assert!(
		matches!(
			case.tir.types[s.fields[0].ty.inner.as_usize()],
			Type::TypeParam { param_index: 0, .. }
		),
		"expected TypeParam, got {:?}",
		case.tir.types[s.fields[0].ty.inner.as_usize()]
	);
}

#[test]
fn test_generic_struct_in_type_position_resolves() {
	let case = TestCase::new(indoc! {"
        struct Wrapper<T> { value: T }
        fn get(w: Wrapper<i32>) -> i32 { w.value }
        export { get }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
	insta::assert_yaml_snapshot!(case.tir);
}

#[test]
fn test_generic_struct_init_with_type_args() {
	let case = TestCase::new(indoc! {"
        struct Pair<T> { pub first: T, pub second: T }
        fn make() -> Pair<i32> {
            Pair::<i32>::{ first: 1, second: 2 }
        }
        export { make }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
	insta::assert_yaml_snapshot!(case.tir);
}

#[test]
fn test_generic_struct_field_access_substitutes_type() {
	let case = TestCase::new(indoc! {"
        struct Wrapper<T> { value: T }
        fn get_i32(w: Wrapper<i32>) -> i32 { w.value }
        fn get_f64(w: Wrapper<f64>) -> f64 { w.value }
        export { get_i32, get_f64 }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_generic_struct_wrong_type_arg_count_is_error() {
	let case = TestCase::new(indoc! {"
        struct Point<T> { x: T, y: T }
        fn bad(p: Point<i32, f64>) -> i32 { p.x }
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::TypeArgCountMismatch.code())),
		"expected E1040 for wrong type arg count"
	);
}

#[test]
fn test_generic_struct_init_wrong_type_arg_count_is_error() {
	let case = TestCase::new(indoc! {"
        struct Wrapper<T> { value: T }
        fn bad() -> Wrapper<i32> {
            Wrapper::<i32, f64>::{ value: 1 }
        }
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::TypeArgCountMismatch.code())),
		"expected E1040 for wrong type arg count in init"
	);
}

#[test]
fn test_generic_struct_fewer_type_args_in_signature_is_error() {
	// Padding with TypeIndex::INFER is only useful where something can later
	// fill the gap in (e.g. from a local's initializer). A function signature
	// has no such follow-up step, so a padded param type must still be
	// rejected — same rule as bare `_`.
	let case = TestCase::new(indoc! {"
        struct Pair<T, U> { a: T, b: U }
        fn f(p: Pair<i32>) -> i32 { p.a }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::InferInSignature),
		"expected E1051 for partially-applied generic struct in signature, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_generic_struct_bare_reference_in_signature_is_error() {
	// No turbofish at all is the extreme case of "fewer args than declared":
	// every slot is padded, not just the trailing ones — still rejected in
	// signature position.
	let case = TestCase::new(indoc! {"
        struct Pair<T, U> { a: T, b: U }
        fn f(p: Pair) { }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::InferInSignature),
		"expected E1051 for bare generic struct reference in signature, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_generic_struct_fewer_type_args_infers_from_local_initializer() {
	// Outside signature position, a padded INFER slot is filled in later —
	// here from the local's own initializer, which supplies both args.
	let case = TestCase::new(indoc! {"
        struct Pair<T, U> { a: T, b: U }
        fn f() {
            local p: Pair<i32> = Pair::<i32, bool>::{ a: 1, b: true }
        }
    "});
	no_errors(&case);
}

#[test]
fn test_generic_struct_too_many_type_args_still_errors() {
	let case = TestCase::new(indoc! {"
        struct Pair<T, U> { a: T, b: U }
        fn f(p: Pair<i32, f64, bool>) { }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeArgCountMismatch),
		"expected E1040 for too many type args"
	);
}

// ── Type aliases
// ─────────────────────────────────────────────────────────

#[test]
fn test_type_alias_simple() {
	let case = TestCase::new(indoc! {"
        type Foo = i32;
        fn f(x: Foo) -> Foo { x }
    "});
	no_errors(&case);
	let alias = case
		.tir
		.type_aliases
		.iter()
		.find(|a| case.graph.interner.resolve(a.name.inner) == Some("Foo"))
		.expect("Foo alias not found");
	assert_eq!(alias.template, TypeIndex::I32);
}

#[test]
fn test_type_alias_to_struct_is_transparent() {
	// Field access through the alias must type-check with no special-casing.
	let case = TestCase::new(indoc! {"
        struct Bar { field: i32 }
        type Foo = Bar;
        fn f(x: Foo) -> i32 { x.field }
    "});
	no_errors(&case);
}

#[test]
fn test_type_alias_generic_rhs() {
	// The alias's RHS is itself a generic instantiation; the alias just
	// names the concrete `Wrapper<i32>` struct type.
	let case = TestCase::new(indoc! {"
        struct Wrapper<T> { data: T }
        type WrapperI32 = Wrapper<i32>;
        fn f(w: WrapperI32) -> i32 { w.data }
    "});
	no_errors(&case);
	let alias = case
		.tir
		.type_aliases
		.iter()
		.find(|a| {
			case.graph.interner.resolve(a.name.inner) == Some("WrapperI32")
		})
		.expect("WrapperI32 alias not found");
	match &case.tir.types[alias.template.as_usize()] {
		Type::Struct { args, .. } => {
			assert_eq!(args.len(), 1);
			assert_eq!(case.tir.types[args[0].as_usize()], Type::I32);
		}
		other => panic!("expected Type::Struct template, got {:?}", other),
	}
}

#[test]
fn test_parametric_type_alias_instantiated_at_use_site() {
	// The alias itself is generic; instantiating it with args must substitute
	// through the tuple template, producing a plain concrete tuple type.
	let case = TestCase::new(indoc! {"
        type Pair<T> = (T, T);
        fn make() -> Pair<i32> {
            (1, 2)
        }
        export { make }
    "});
	no_errors(&case);
	let func = case
		.tir
		.functions
		.iter()
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("make"))
		.expect("make function not found");
	let result_ty = case.tir.types[func.signature_index.as_usize()].clone();
	let Type::Function { signature } = result_ty else {
		panic!("expected Type::Function, got {:?}", result_ty);
	};
	match &case.tir.types[signature.result().as_usize()] {
		Type::Tuple { elements } => {
			assert_eq!(elements.len(), 2);
			assert_eq!(case.tir.types[elements[0].as_usize()], Type::I32);
			assert_eq!(case.tir.types[elements[1].as_usize()], Type::I32);
		}
		other => panic!("expected Type::Tuple, got {:?}", other),
	}
}

#[test]
fn test_type_alias_direct_cycle_is_error() {
	let case = TestCase::new(indoc! {"
        type A = A;
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CyclicTypeDependency),
		"expected E1032 for direct self-referential alias"
	);
}

#[test]
fn test_type_alias_mutual_cycle_is_error() {
	let case = TestCase::new(indoc! {"
        type A = B;
        type B = A;
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CyclicTypeDependency),
		"expected E1032 for mutually recursive aliases"
	);
}

#[test]
fn test_type_alias_wrong_type_arg_count_is_error() {
	let case = TestCase::new(indoc! {"
        type Pair<T> = (T, T);
        fn f(p: Pair<i32, f64>) { }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeArgCountMismatch),
		"expected E1040 for wrong type arg count on alias"
	);
}

#[test]
fn test_type_alias_fewer_type_args_in_signature_is_error() {
	// Same rule as generic structs: a padded INFER slot has no follow-up
	// step in signature position, so it's still rejected there.
	let case = TestCase::new(indoc! {"
        type Pair<T, U> = (T, U);
        fn f(p: Pair<i32>) { }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::InferInSignature),
		"expected E1051 for partially-applied alias in signature, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_type_alias_fewer_type_args_infers_from_local_initializer() {
	let case = TestCase::new(indoc! {"
        type Pair<T, U> = (T, U);
        fn f() {
            local p: Pair<i32> = (1, true)
        }
    "});
	no_errors(&case);
}

#[test]
fn test_type_alias_forward_reference_resolves() {
	// Foo is used before its declaration further down the file.
	let case = TestCase::new(indoc! {"
        fn f(x: Foo) -> i32 { x.field }
        struct Bar { field: i32 }
        type Foo = Bar;
    "});
	no_errors(&case);
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
		case.graph
			.interner
			.resolve(f.name.inner)
			.map(|n| n == "identity")
			.unwrap_or(false)
	});
	let func = func.expect("function 'identity' not found in TIR");
	assert_eq!(func.type_params.len(), 1, "expected one type param");
	assert_eq!(
		case.graph.interner.resolve(func.type_params[0].name),
		Some("T")
	);
	assert!(
		func.type_params[0].bounds.traits.is_empty(),
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
		case.graph
			.interner
			.resolve(f.name.inner)
			.map(|n| n == "call_scale")
			.unwrap_or(false)
	});
	let func = func.expect("function 'call_scale' not found in TIR");
	assert_eq!(func.type_params.len(), 1);
	assert_eq!(
		func.type_params[0].bounds.traits.len(),
		1,
		"T should have one bound (Scalable)"
	);
}

#[test]
fn test_type_param_referenced_in_binding_rhs_records_access() {
	// When a type param appears as the RHS of a `where { AssocType = TypeParam }` binding,
	// that reference must be recorded in TypeParamInfo.accesses so that:
	//   (a) the "unused type param" warning is suppressed, and
	//   (b) callers relying on accesses for liveness are correct.
	//
	// `T` below is only used in the binding — not in any param type or return type —
	// so its accesses count must be exactly 1 after type-checking.
	let case = TestCase::new(indoc! {"
        trait Container {
            type Item;
        }
        fn wrap<T, C: Container where { Item = T }>(c: C) -> C {
            c
        }
    "});
	no_errors(&case);
	let func = case
		.tir
		.functions
		.iter()
		.find(|f| {
			case.graph
				.interner
				.resolve(f.name.inner)
				.map(|n| n == "wrap")
				.unwrap_or(false)
		})
		.expect("function 'wrap' not found in TIR");
	assert_eq!(func.type_params.len(), 2, "expected two type params (T, C)");
	assert_eq!(
		func.type_params[0].accesses.len(),
		1,
		"T should have exactly 1 access recorded (from the binding `Item = T`)"
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
		has_error_code(&case.tir, DiagnosticCode::UndeclaredType),
		"expected E1021 (UndeclaredType) for unknown trait bound 'Nonexistent', got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

// ── NamespaceAccess / associated type projection ────────────────────────────

fn no_errors(case: &TestCase) {
	use codespan_reporting::diagnostic::Severity;
	use codespan_reporting::term;
	use codespan_reporting::term::DisplayStyle;
	use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = term::Config {
		display_style: DisplayStyle::Rich,
		..term::Config::default()
	};
	for crate_ in &case.graph.crates {
		for diagnostic in crate_.diagnostics.iter().filter(|diagnostic| {
			match diagnostic.severity {
				Severity::Error | Severity::Bug => true,
				_ => false,
			}
		}) {
			term::emit_to_write_style(
				&mut writer.lock(),
				&config,
				&case.graph.files,
				diagnostic,
			)
			.unwrap();
		}
	}
	for diagnostic in case.tir.diagnostics.iter().filter(|diagnostic| {
		match diagnostic.severity {
			Severity::Error | Severity::Bug => true,
			_ => false,
		}
	}) {
		term::emit_to_write_style(
			&mut writer.lock(),
			&config,
			&case.graph.files,
			diagnostic,
		)
		.unwrap();
	}

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
				&& (d.message.contains(substring)
					|| d.notes.iter().any(|n| n.contains(substring)))
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

#[test]
fn test_assoc_type_declared_in_trait() {
	// A trait with an associated type must register it in `members` and
	// `assoc_type_bounds`.
	let case = TestCase::new(indoc! {"
        trait Bound {}
        trait Container {
            type Elem: Bound;
        }
    "});
	no_errors(&case);

	let container_trait = case
		.tir
		.traits
		.iter()
		.find(|t| {
			case.graph.interner.resolve(t.name.inner) == Some("Container")
		})
		.expect("trait 'Container' not found");

	let elem_sym = case
		.graph
		.interner
		.get("Elem")
		.expect("symbol 'Elem' not interned");

	assert!(
		matches!(
			container_trait.members.get(&elem_sym),
			Some(ImplEntry::AssociatedType { .. })
		),
		"expected 'Elem' in Container::members as AssociatedType"
	);
	assert!(
		container_trait.assoc_types.contains_key(&elem_sym),
		"expected 'Elem' in Container::assoc_types"
	);
}

#[test]
fn test_assoc_type_projection_in_return_type() {
	// `fn foo<C: Container>() -> C::Elem` — the return type must resolve to
	// `AssocTypeProjection` (no error diagnostics).
	let case = TestCase::new(indoc! {"
        trait Bound {}
        trait Container {
            type Elem: Bound;
        }
        fn foo<C: Container>() -> C::Elem {
            unreachable
        }
    "});
	no_errors(&case);

	let func = case
		.tir
		.functions
		.iter()
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("foo"))
		.expect("function 'foo' not found");

	let result_ty = func.result.as_ref().expect("expected a return type").inner;
	assert!(
		matches!(
			case.tir.types[result_ty.as_usize()],
			Type::AssocTypeProjection { .. }
		),
		"return type should be AssocTypeProjection for C::Elem, got type index {}",
		result_ty.as_u32()
	);
}

#[test]
fn test_assoc_type_projection_in_param_type() {
	// `fn consume<C: Container>(elem: C::Elem)` — the parameter type resolves to
	// `AssocTypeProjection` without errors.
	let case = TestCase::new(indoc! {"
        trait Bound {}
        trait Container {
            type Elem: Bound;
        }
        fn consume<C: Container>(elem: C::Elem) {
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
	// TODO: improve to "undeclared associated type 'Nonexistent'" for better diagnostics
	has_error_matching(&case, "undeclared type");
}

#[test]
fn test_assoc_type_bare_name_suggests_self_prefix() {
	// Using the associated type name directly (e.g. `Size` instead of
	// `Self::Size`) must produce a targeted error with a `Self::` suggestion.
	let case = TestCase::new(indoc! {"
        trait Memory {
            type Size;
            fn alloc(n: Size) -> *u8;
        }
    "});
	// report_bare_assoc_type emits E1021 with message "cannot find type `Size` in
	// this scope" and a note containing the "Self::Size" suggestion.
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UndeclaredType),
		"expected E1021 (UndeclaredType) for bare associated type name, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_assoc_type_impl_registers_in_trait_impl() {
	// `impl Container for Heap { type Elem = u32; }` — the impl must store
	// a concrete type in both `TraitImpl::members` and `impl_members`.
	let case = TestCase::new(indoc! {"
        trait Bound {}
        impl Bound for u32 {}
        trait Container {
            type Elem: Bound;
        }
        struct Heap {}
        impl Container for Heap {
            type Elem = u32;
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
				.and_then(|t| case.graph.interner.resolve(t.name.inner))
				== Some("Container")
		})
		.expect("TraitImpl for Container not found");

	let elem_sym = case
		.graph
		.interner
		.get("Elem")
		.expect("symbol 'Elem' not interned");

	assert!(
		matches!(
			ti.members.get(&elem_sym),
			Some(ImplEntry::AssociatedType { ty }) if *ty == TypeIndex::U32
		),
		"expected 'Elem' → u32 in TraitImpl::members"
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
	has_error_matching(
		&case,
		"associated type `Size` must implement `PointerSize`",
	);
}

#[test]
fn test_assoc_type_where_binding_out_of_order_type_param_resolves() {
	// `Src: Memory where { Size = S }` where `S` is defined AFTER `Src` —
	// the two-pass resolution must find `S` in the full scope even though
	// it wasn't yet built when the bound was first parsed.
	let case = TestCase::new(indoc! {"
        trait PointerSize {}
        trait Memory {
            type Size: PointerSize;
        }
        fn copy<Src: Memory where { Size = S }, S: PointerSize>() {
            unreachable
        }
    "});
	no_errors(&case);
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
	assert!(has_error_code(&case.tir, DiagnosticCode::TypeMistmatch));
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
	// pointer types. Untagged `*C::Item` resolves memory from the single
	// ambient memory declaration.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            trait Container {
                type Item;
            }
            fn process<C: Container>(ptr: *C::Item) {
                unreachable
            }
            fn wrap<C: Container>(ptr: *C::Item) {
                process(ptr)
            }
        "},
		&[],
	);
	no_errors(&case);
}

// ── generic functions over Memory ────────────────────────────────────────────

#[test]
fn test_generic_over_memory_size_in_signature() {
	// A generic fn<M: Memory>(M::Size) → M::Size must resolve without errors:
	// M::Size stays as AssocTypeProjection in the generic signature.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn pass<M: Memory>(mem: M, n: M::Size) -> M::Size {
                n
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_generic_over_memory_called_with_concrete_memory() {
	// Calling pass(heap, 42u32) must unify M=heap → M::Size=u32 with no errors.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn pass<M: Memory>(mem: M, n: M::Size) -> M::Size {
                n
            }
            pub fn caller(n: u32) -> u32 {
                pass(heap, n)
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_generic_over_memory_wrong_size_type_is_error() {
	// Passing i64 where M::Size=u32 is expected must be a type error.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn pass<M: Memory>(mem: M, n: M::Size) -> M::Size {
                n
            }
            pub fn caller(n: i64) -> u32 {
                pass(heap, n)
            }
        "},
		&[],
	);
	assert!(has_error_code(&case.tir, DiagnosticCode::TypeMistmatch));
}

#[test]
fn test_generic_over_memory_two_concrete_memories() {
	// The same generic fn called with both a Memory32 and a Memory64 must
	// resolve correctly for each — M::Size = u32 and M::Size = u64.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            memory stack: Memory where { Size = u64 };
            fn pass<M: Memory>(mem: M, n: M::Size) -> M::Size {
                n
            }
            pub fn use_heap(n: u32) -> u32 {
                pass(heap, n)
            }
            pub fn use_stack(n: u64) -> u64 {
                pass(stack, n)
            }
        "},
		&[],
	);
	no_errors(&case);
}

// ── Type::Infer / underscore type placeholder ────────────────────────────────

#[test]
fn test_infer_placeholder_in_generic_type_arg() {
	// `Layout<_>` where the local variable's assigned value fully constrains
	// the type arg — the `_` is the user-written inference placeholder and
	// must not cause an error when the context resolves it.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            struct Layout<M: Memory> { size: M::Size }
            impl <M: Memory> Layout<M> {
                pub fn new(size: M::Size) -> Layout<M> {
                    Layout::{ size }
                }
            }
            pub fn demo() {
                local x: Layout<_> = Layout::<heap>::new(4 as u32);
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_cannot_infer_generic_type_param_error() {
	// Calling a generic constructor without enough context to infer the type
	// parameter must be an error with a "cannot infer type for type parameter"
	// diagnostic.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            struct Layout<M: Memory> { size: M::Size }
            impl <M: Memory> Layout<M> {
                pub fn array<T>(count: M::Size) -> Layout<M> {
                    Layout::{ size: count }
                }
            }
            pub fn demo() {
                local y = Layout::array::<i32>(4);
            }
        "},
		&[],
	);
	has_error_matching(&case, "cannot infer type for type parameter `M`");

	// When M is also specified via turbofish on the first segment, both params
	// are fully resolved and there should be no errors.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            struct Layout<M: Memory> { size: M::Size }
            impl <M: Memory> Layout<M> {
                pub fn array<T>(count: M::Size) -> Layout<M> {
                    Layout::{ size: count }
                }
            }
            pub fn demo() {
                local y = Layout::<heap>::array::<i32>(10);
            }
        "},
		&[],
	);
	no_errors(&case);
}

// ── `_` (infer placeholder) edge cases ──────────────────────────────────────

#[test]
fn test_infer_in_function_param_type_is_error() {
	let case = TestCase::new(indoc! {"
        fn foo(x: _) -> i32 { 0 }
    "});
	has_error_matching(&case, "`_` is not allowed in item type signatures");
}

#[test]
fn test_infer_in_function_return_type_is_error() {
	let case = TestCase::new(indoc! {"
        fn foo() -> _ { 0 }
    "});
	has_error_matching(&case, "`_` is not allowed in item type signatures");
}

#[test]
fn test_infer_in_struct_field_is_error() {
	let case = TestCase::new(indoc! {"
        struct Foo { x: _ }
    "});
	has_error_matching(&case, "`_` is not allowed in item type signatures");
}

#[test]
fn test_infer_in_global_declaration_is_error() {
	let case = TestCase::new(indoc! {"
        global x: _ = 0;
    "});
	has_error_matching(&case, "`_` is not allowed in item type signatures");
}

#[test]
fn test_infer_in_cast_without_context_is_error() {
	// `42 as _` with no type context — target type cannot be inferred.
	let case = TestCase::new(indoc! {"
        fn foo() { local x = 42 as _; }
    "});
	assert!(has_error_code(
		&case.tir,
		DiagnosticCode::TypeAnnotationRequired
	));
}

#[test]
fn test_infer_in_cast_with_context_succeeds() {
	// `42 as _` where context supplies the target type — should lower cleanly.
	let case = TestCase::new(indoc! {"
        fn foo() -> i32 { 42 as _ }
    "});
	no_errors(&case);
}

#[test]
fn test_infer_multi_wildcard_tuple_annotation() {
	// Both `_` slots should be filled from the RHS — no error.
	let case = TestCase::new(indoc! {"
        fn foo() {
            local x: (i32, f32) = (1 as i32, 2.0 as f32);
            local y: (_, _) = (1 as i32, 2.0 as f32);
        }
    "});
	no_errors(&case);
}

#[test]
fn test_infer_annotation_type_mismatch_still_errors() {
	// `local x: i32 = 1.0 as f32` should still produce a type mismatch even
	// when using `_` for the other slot.
	let case = TestCase::new(indoc! {"
        fn foo() {
            local x: (i32, _) = (1.0 as f32, 2 as i32);
        }
    "});
	assert!(has_error_code(&case.tir, DiagnosticCode::TypeMistmatch));
}

#[test]
fn test_infer_local_no_rhs_annotation_only_is_error() {
	// `local x: _` with no initializer is not valid syntax, but `local x: _ = expr`
	// where expr is also completely unconstrained should produce an error.
	let case = TestCase::new(indoc! {"
        fn foo() -> i32 {
            local x: _ = 42;
            x
        }
    "});
	// The integer literal 42 by itself with no constraint should require annotation.
	assert!(has_error_code(
		&case.tir,
		DiagnosticCode::TypeAnnotationRequired
	));
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
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(p: heap::*i32) {
                unreachable
            }
        "},
		&[],
	);
	no_errors(&case);

	let heap_id = case.tir.memories[0].id;
	let f = case
		.tir
		.functions
		.iter()
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("f"))
		.expect("function 'f' not found");

	let param_ty = f.params[0].ty.inner;
	let is_heap_ptr = match &case.tir.types[param_ty.as_usize()] {
		Type::Pointer { memory, .. } => {
			matches!(&case.tir.types[memory.as_usize()], Type::Memory { id, .. } if *id == heap_id)
		}
		_ => false,
	};
	assert!(
		is_heap_ptr,
		"expected heap::*i32 (pointer tagged with heap), got index {}",
		param_ty.as_u32()
	);
}

#[test]
fn test_memory_tagged_slice() {
	// `heap::[]u8` resolves to Type::Slice { memory: Some(heap_id) }
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(s: heap::[]u8) {
                unreachable
            }
        "},
		&[],
	);
	no_errors(&case);

	let heap_id = case.tir.memories[0].id;
	let f = case
		.tir
		.functions
		.iter()
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("f"))
		.expect("function 'f' not found");

	let param_ty = f.params[0].ty.inner;
	let is_heap_slice = match &case.tir.types[param_ty.as_usize()] {
		Type::Slice { memory, .. } => {
			matches!(&case.tir.types[memory.as_usize()], Type::Memory { id, .. } if *id == heap_id)
		}
		_ => false,
	};
	assert!(
		is_heap_slice,
		"expected heap::[]u8 (slice tagged with heap), got index {}",
		param_ty.as_u32()
	);
}

#[test]
fn test_memory_tagged_array() {
	// `heap::[4]u8` resolves to Type::Array { size: 4, memory: Some(heap_id) }
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]u8) {
                unreachable
            }
        "},
		&[],
	);
	no_errors(&case);

	let heap_id = case.tir.memories[0].id;
	let f = case
		.tir
		.functions
		.iter()
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("f"))
		.expect("function 'f' not found");

	let param_ty = f.params[0].ty.inner;
	let is_heap_array = match &case.tir.types[param_ty.as_usize()] {
		Type::Array {
			size: 4, memory, ..
		} => {
			matches!(&case.tir.types[memory.as_usize()], Type::Memory { id, .. } if *id == heap_id)
		}
		_ => false,
	};
	assert!(
		is_heap_array,
		"expected heap::[4]u8 (array tagged with heap), got index {}",
		param_ty.as_u32()
	);
}

#[test]
fn test_memory_tagged_nested_array() {
	// `heap::[4]heap::[4]u8` — outer array in heap, elements are heap-tagged arrays
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]heap::[4]u8) {
                unreachable
            }
        "},
		&[],
	);
	no_errors(&case);

	let heap_id = case.tir.memories[0].id;
	let f = case
		.tir
		.functions
		.iter()
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("f"))
		.expect("function 'f' not found");

	let outer_ty = f.params[0].ty.inner;
	let is_heap_mem = |memory: &TypeIndex| matches!(&case.tir.types[memory.as_usize()], Type::Memory { id, .. } if *id == heap_id);
	let (inner_ty, outer_tagged) = match &case.tir.types[outer_ty.as_usize()] {
		Type::Array {
			of,
			size: 4,
			memory,
			..
		} if is_heap_mem(memory) => (*of, true),
		_ => (TypeIndex::ERROR, false),
	};
	assert!(
		outer_tagged,
		"outer array should be tagged with heap memory"
	);
	let inner_tagged = match &case.tir.types[inner_ty.as_usize()] {
		Type::Array {
			size: 4, memory, ..
		} => is_heap_mem(memory),
		_ => false,
	};
	assert!(
		inner_tagged,
		"inner array should also be tagged with heap memory"
	);
}

#[test]
fn test_memory_tagged_non_pointer_is_error() {
	// `heap::i32` — memory namespace before a scalar type should error
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(x: heap::i32) {
                unreachable
            }
        "},
		&[],
	);
	assert!(has_error_code(&case.tir, DiagnosticCode::UndeclaredType)); // heap::i32 — `i32` is not a member of the memory namespace
}

#[test]
fn test_untagged_and_tagged_pointer_resolve_to_same_type() {
	// With one memory in scope, `*i32` and `heap::*i32` resolve to the same type.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: *i32, b: heap::*i32) {
                unreachable
            }
        "},
		&[],
	);
	no_errors(&case);

	let f = case
		.tir
		.functions
		.iter()
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("f"))
		.expect("function 'f' not found");

	let untagged = f.params[0].ty.inner;
	let tagged = f.params[1].ty.inner;
	assert_eq!(
		untagged, tagged,
		"with one memory, *i32 and heap::*i32 should intern to the same TypeIndex"
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
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("square"))
		.expect("function 'square' not found")
		.id;

	let has_function_item = case.tir.types.iter().any(|t| {
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
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("identity"))
		.expect("function 'identity' not found")
		.id;

	let has_function_item = case.tir.types.iter().any(
		|t| matches!(t, Type::FunctionItem { id, .. } if *id == identity_id),
	);
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
		case.tir.diagnostics.iter().any(|d| {
			d.labels.iter().any(|l| l.message.contains("identity"))
		}),
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
			.find(|f| case.graph.interner.resolve(f.name.inner) == Some(name))
			.unwrap_or_else(|| panic!("function '{}' not found", name))
			.id
	};
	let square_id = find_id("square");
	let double_id = find_id("double");

	let type_idx = |id: DefId| {
		case.tir
			.types
			.iter()
			.enumerate()
			.find_map(|(i, t)| {
				if matches!(t, Type::FunctionItem { id: fid, .. } if *fid == id)
				{
					Some(TypeIndex(i as u32))
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
	assert!(has_error_code(&case.tir, DiagnosticCode::TypeMistmatch));
}

// ── Type application expressions ─────────────────────────────────────────────

#[test]
fn test_type_application_coerces_to_fn_pointer() {
	// `identity::<i32>` must coerce to `fn(i32) -> i32`.
	let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T { t }
        fn main() {
            local f: fn(i32) -> i32 = identity::<i32>
        }
    "});
	no_errors(&case);
}

#[test]
fn test_type_application_wrong_arg_count_is_error() {
	let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T { t }
        fn main() {
            local f = identity::<i32, i64>
        }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeArgCountMismatch),
		"expected E1040 (TypeArgCountMismatch) for too many type args, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_type_application_on_non_generic_is_error() {
	let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn main() {
            local f = add::<i32>
        }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeArgCountMismatch),
		"expected E1040 (TypeArgCountMismatch) for type args on non-generic fn, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_type_application_in_if_else_unifies() {
	// Two distinct generic instantiations with the same signature unify.
	let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T { t }
        fn wrap<T>(t: T) -> T { t }
        fn main() -> fn(i32) -> i32 {
            local f: fn(i32) -> i32 = if true { identity::<i32> } else { wrap::<i32> }
            f
        }
    "});
	no_errors(&case);
}

// ── Pointer dereference ──────────────────────────────────────────────────────

#[test]
fn test_deref_load_through_pointer() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn read(ptr: heap::*i32) -> i32 { ptr.* }
    "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_deref_store_through_mutable_pointer() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn write(ptr: heap::*mut i32) { ptr.* = 42 }
    "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_deref_arithmetic_assignment_through_mutable_pointer() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn increment(ptr: heap::*mut i32) { ptr.* += 1 }
    "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_deref_non_pointer_type_is_error() {
	let case = TestCase::new(indoc! {"
        fn bad(x: i32) -> i32 { x.* }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotDerefNonPointer),
		"expected E1037 (dereference of non-pointer type)"
	);
}

#[test]
fn test_deref_no_memory_is_error() {
	let case = TestCase::new(indoc! {"
        fn bad(ptr: *i32) -> i32 { ptr.* }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::NoMemoryForPointer),
		"expected E1038 (no memory for pointer)"
	);
}

#[test]
fn test_deref_store_through_immutable_pointer_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn bad(ptr: heap::*i32) { ptr.* = 42 }
    "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"expected W1000 (CannotMutateImmutable) for store through immutable pointer, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_deref_arithmetic_assignment_through_immutable_pointer_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn bad(ptr: heap::*i32) { ptr.* += 1 }
    "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"expected W1000 (CannotMutateImmutable) for arithmetic-assign through immutable pointer, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_deref_type_mismatch_on_store_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn bad(ptr: heap::*mut i32) { ptr.* = true }
    "},
		&[],
	);
	has_error_matching(&case, "cannot assign");
}

// ── Multi-segment path resolution
// ─────────────────────────────────────────────

#[test]
fn test_path_type_associated_fn_ufcs() {
	// `i32::abs(x)` — 2-segment path where the first segment is a type and the
	// second is an associated function; all params (including self) are explicit.
	let case = TestCase::new(indoc! {"
        impl i32 {
            pub fn abs(self) -> i32 {
                if self < 0 { -self } else { self }
            }
        }
        fn f(x: i32) -> i32 {
            i32::abs(x)
        }
        export { f }
    "});
	no_errors(&case);
}

#[test]
fn test_path_struct_associated_fn_no_params() {
	// `Counter::zero()` — zero-parameter associated function on a user-defined struct.
	let case = TestCase::new(indoc! {"
        struct Counter { value: u32 }
        impl Counter {
            pub fn zero() -> Counter { Counter::{ value: 0 } }
        }
        fn test() -> Counter { Counter::zero() }
        export { test }
    "});
	no_errors(&case);
}

#[test]
fn test_path_generic_struct_associated_fn() {
	// `Wrapper::<u32>::new(42)` — associated function on a generic struct via generic impl.
	let case = TestCase::new(indoc! {"
        struct Wrapper<T> { value: T }
        impl<T> Wrapper<T> {
            pub fn new(value: T) -> Wrapper<T> { Wrapper::{ value } }
        }
        fn test() -> Wrapper<u32> { Wrapper::<u32>::new(42) }
        export { test }
    "});
	no_errors(&case);
}

#[test]
fn test_path_inline_module_type_associated_fn() {
	// `math::Point::zero()` — 3-segment path through an inline module to an
	// associated function: module → type → fn.
	let case = TestCase::new(indoc! {"
        module math {
            pub struct Point {}
            impl Point {
                pub fn zero() -> i32 { 0 }
            }
        }
        fn f() -> i32 {
            math::Point::zero()
        }
        export { f }
    "});
	no_errors(&case);
}

#[test]
fn test_path_cross_module_struct_init() {
	// `shapes::Point::{ x: 1, y: 2 }` — struct literal via a cross-module path.
	let case = TestCase::new_multi_file(
		"src/main.wx",
		indoc! {"
            module shapes;

            fn make() -> shapes::Point {
                shapes::Point::{ x: 1, y: 2 }
            }

            export { make }
        "},
		&[("src/shapes.wx", "pub struct Point { x: i32, y: i32 }")],
	);
	no_errors(&case);
}

#[test]
fn test_path_cross_module_generic_struct_init() {
	// `containers::Wrapper::<i32>::{ value: 42 }` — generic struct literal via a
	// cross-module path with explicit type args on the last segment.
	let case = TestCase::new_multi_file(
		"src/main.wx",
		indoc! {"
            module containers;

            fn make() -> containers::Wrapper::<i32> {
                containers::Wrapper::<i32>::{ value: 42 }
            }

            export { make }
        "},
		&[("src/containers.wx", "pub struct Wrapper<T> { value: T }")],
	);
	no_errors(&case);
}

#[test]
fn test_generic_struct_concrete_impl() {
	// impl Point<i32> — concrete monomorphic impl, no type params needed
	let case = TestCase::new(indoc! {"
        struct Point<T> {
            x: T,
            y: T,
        }

        impl Point<i32> {
            pub fn sum(self) -> i32 { self.x + self.y }
        }

        fn run() -> i32 {
            local p: Point<i32> = Point::{ x: 3, y: 4 };
            p.sum()
        }

        export { run }
    "});
	no_errors(&case);
}

#[test]
fn test_generic_struct_method() {
	// impl Point<T> — generic impl; T in scope, field access returns T
	let case = TestCase::new(indoc! {"
        struct Point<T> {
            x: T,
            y: T,
        }

        impl<T> Point<T> {
            pub fn get_x(self) -> T { self.x }
        }

        fn run() -> i32 {
            local p: Point<i32> = Point::{ x: 3, y: 4 };
            p.get_x()
        }

        export { run }
    "});
	no_errors(&case);
}

// ── Array literals
// ────────────────────────────────────────────────────────────

#[test]
fn test_array_literal_basic() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() -> heap::[3]i32 {
                local x: heap::[3]i32 = [1, 2, 3];
                x
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_array_literal_mutable() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() -> heap::[3]i32 {
                local x: heap::[3]i32 = [1, 2, 3];
                x
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_array_literal_float_elements() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() -> heap::[2]f32 {
                local x: heap::[2]f32 = [1.0, 2.0];
                x
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_array_literal_empty_with_annotation() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() -> heap::[0]i32 {
                local x: heap::[0]i32 = [];
                x
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_array_literal_size_mismatch_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() {
                local x: heap::[3]i32 = [1, 2];
            }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::ArraySizeMismatch),
		"expected E1043 (array size mismatch)"
	);
}

#[test]
fn test_array_literal_no_annotation_is_error() {
	// Without a type annotation the element type cannot be inferred from comptime
	// ints.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() {
                local x = [1, 2, 3];
            }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeAnnotationRequired),
		"expected E1002 (type annotation required)"
	);
}

#[test]
fn test_array_literal_no_memory_is_error() {
	// No memory declaration — array cannot be placed in linear memory.
	let case = TestCase::new(indoc! {"
        fn f() {
            local x: [3]i32 = [1, 2, 3];
        }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::NoMemoryForPointer),
		"expected E1038 (no memory for pointer)"
	);
}

#[test]
fn test_array_literal_non_numeric_element_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() {
                local x: heap::[2]i32 = [true, false];
            }
        "},
		&[],
	);
	has_error_matching(&case, "array element type must be a numeric type");
}

#[test]
fn test_array_literal_mixed_element_types_is_error() {
	// Mixing a typed expression (true: bool) after a comptime int should mismatch.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(b: bool) {
                local x: heap::[2]bool = [b, b];
            }
        "},
		&[],
	);
	has_error_matching(&case, "array element type must be a numeric type");
}

// ── Array repeat
// ──────────────────────────────────────────────────────────────

#[test]
fn test_array_repeat_basic() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() -> heap::[4]i32 {
                local x: heap::[4]i32 = [0; 4];
                x
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_array_repeat_float() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() -> heap::[8]f64 {
                local x: heap::[8]f64 = [0.0; 8];
                x
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_array_repeat_count_not_const_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(n: u32) {
                local x: heap::[4]i32 = [0; n];
            }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::ArrayRepeatCountNotConst),
		"expected E1044 (array repeat count not const)"
	);
}

#[test]
fn test_array_repeat_size_mismatch_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() {
                local x: heap::[4]i32 = [0; 3];
            }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::ArraySizeMismatch),
		"expected E1043 (array size mismatch)"
	);
}

#[test]
fn test_array_repeat_no_annotation_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f() {
                local x = [0; 4];
            }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeAnnotationRequired),
		"expected E1002 (type annotation required)"
	);
}

#[test]
fn test_array_repeat_non_numeric_value_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(b: bool) {
                local x: heap::[4]i32 = [b; 4];
            }
        "},
		&[],
	);
	has_error_matching(&case, "array element type must be a numeric type");
}

// ── Index operator
// ────────────────────────────────────────────────────────────

#[test]
fn test_index_read_from_array() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]i32) -> i32 { a[0] }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_read_from_slice() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[]i32) -> i32 { a[0] }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_on_non_indexable_is_error() {
	let case = TestCase::new(indoc! {"
        fn f(x: i32) -> i32 { x[0] }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::IndexOnNonIndexable),
		"expected E1042 (index on non-indexable type)"
	);
}

#[test]
fn test_index_wrong_index_type_is_error() {
	// Memory where { Size = u32 } requires u32 index; passing i64 should be a type mismatch.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]i32, i: i64) -> i32 { a[i] }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeMistmatch),
		"expected E1001 (TypeMistmatch) for wrong index type, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_index_store_through_mutable_array() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]mut i32) { a[0] = 42 }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_store_through_immutable_array_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]i32) { a[0] = 42 }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"expected W1000 (CannotMutateImmutable) for store through immutable array, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_index_arithmetic_assignment_through_mutable_array() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]mut i32) { a[0] += 1 }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_arithmetic_assignment_through_immutable_array_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]i32) { a[0] += 1 }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"expected W1000 (CannotMutateImmutable) for arithmetic-assign through immutable array, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_index_store_type_mismatch_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]mut i32) { a[0] = true }
        "},
		&[],
	);
	has_error_matching(&case, "cannot assign");
}

#[test]
fn test_index_memory64_requires_u64_index() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u64 };
            fn f(a: heap::[4]i32) -> i32 { a[0] }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_ambiguous_memory_is_error() {
	// Two memories and no tag on the array type — cannot resolve memory for
	// indexing.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            memory stack: Memory where { Size = u32 };
            fn f(a: heap::[4]i32) -> i32 { a[0] }
        "},
		&[],
	);
	// The array type already carries heap's memory id, so indexing it should
	// succeed even with two memories declared.
	no_errors(&case);
}

#[test]
fn test_array_literal_runtime_element_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(x: i32) {
                local arr: heap::[2]i32 = [x, 1];
            }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::ArrayElementNotConst),
		"expected E1045 (array element not const)"
	);
}

#[test]
fn test_array_repeat_runtime_value_is_error() {
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(x: i32) {
                local arr: heap::[4]i32 = [x; 4];
            }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::ArrayElementNotConst),
		"expected E1045 (array element not const)"
	);
}

// ── abstract memory indexing ──────────────────────────────────────────────────

#[test]
fn test_index_concrete_memory32_typed_variable() {
	// Typed u32 variable (not a literal) as index into a Memory32 array.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]i32, i: u32) -> i32 { a[i] }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_concrete_memory64_typed_variable() {
	// Typed u64 variable as index into a Memory64 array.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u64 };
            fn f(a: heap::[4]i32, i: u64) -> i32 { a[i] }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_memory32_with_u64_variable_is_error() {
	// Typed u64 index on a Memory32 (u32-indexed) array must be rejected.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn f(a: heap::[4]i32, i: u64) -> i32 { a[i] }
        "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeMistmatch),
		"expected E1001 (TypeMistmatch) for u64 index on Memory32 array, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_index_generic_array_with_assoc_size_type() {
	// Generic fn over M: Memory indexing M::[4]i32 with M::Size — the index
	// type must accept M::Size rather than requiring a concrete integer type.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn read<M: Memory>(arr: M::[4]i32, i: M::Size) -> i32 {
                arr[i]
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_generic_slice_with_assoc_size_type() {
	// Same as above but for a slice (runtime-length).
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn read<M: Memory>(s: M::[]i32, i: M::Size) -> i32 {
                s[i]
            }
        "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_index_generic_array_call_with_concrete_memory() {
	// The generic indexing fn must be callable with a concrete memory so
	// M::Size is substituted to u32 at the call site.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            memory heap: Memory where { Size = u32 };
            fn read<M: Memory>(arr: M::[4]i32, i: M::Size) -> i32 {
                arr[i]
            }
            fn caller(arr: heap::[4]i32, i: u32) -> i32 {
                read(arr, i)
            }
        "},
		&[],
	);
	no_errors(&case);
}

// ── generic trait bound checking
// ─────────────────────────────────────────────────────────

#[test]
fn test_generic_call_with_satisfying_type_is_ok() {
	let case = TestCase::new(indoc! {"
        trait UnsignedInteger {}
        impl UnsignedInteger for u32 {}
        fn test<U: UnsignedInteger>(u: U) {}
        fn main() { test(42 as u32); }
        export { main }
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| {
			d.severity == codespan_reporting::diagnostic::Severity::Error
		})
		.collect();
	assert!(
		errors.is_empty(),
		"expected no errors when calling test() with u32 (implements UnsignedInteger): {:?}",
		errors
	);
}

// ── enum tests
// ────────────────────────────────────────────────────────────────

#[test]
fn test_enum_variants_are_populated() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            Red = 1,
            Green,
            Blue,
        }
        export {}
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| d.severity == Severity::Error)
		.collect();
	assert!(errors.is_empty(), "expected no errors: {:?}", errors);

	let enum_ = case
		.tir
		.enums
		.iter()
		.find(|e| case.graph.interner.resolve(e.name.inner) == Some("Color"))
		.expect("Color enum not found");

	assert_eq!(enum_.variants.len(), 3, "expected 3 variants");
	assert!(enum_.variant_lookup.len() == 3);

	let red_idx = *enum_.variant_lookup.values().min().unwrap();
	let red = &enum_.variants[red_idx as usize];
	assert_eq!(red.const_value, Some(ConstValue::Int(1)));

	let green = &enum_.variants[1];
	assert_eq!(green.const_value, Some(ConstValue::Int(2)));

	let blue = &enum_.variants[2];
	assert_eq!(blue.const_value, Some(ConstValue::Int(3)));
}

#[test]
fn test_enum_all_implicit_variants() {
	let case = TestCase::new(indoc! {"
        enum Direction: u32 {
            North,
            East,
            South,
            West,
        }
        export {}
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| d.severity == Severity::Error)
		.collect();
	assert!(errors.is_empty(), "expected no errors: {:?}", errors);

	let enum_ = case
		.tir
		.enums
		.iter()
		.find(|e| {
			case.graph.interner.resolve(e.name.inner) == Some("Direction")
		})
		.expect("Direction enum not found");

	assert_eq!(enum_.variants.len(), 4);
	for (i, variant) in enum_.variants.iter().enumerate() {
		assert_eq!(
			variant.const_value,
			Some(ConstValue::Int(i as i64)),
			"variant {} should have value {}",
			i,
			i
		);
	}
}

#[test]
fn test_enum_variant_access_resolves() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            Red = 1,
            Green,
            Blue,
        }
        fn get_red() -> Color {
            Color::Red
        }
        export { get_red }
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| d.severity == Severity::Error)
		.collect();
	assert!(
		errors.is_empty(),
		"expected no errors accessing Color::Red: {:?}",
		errors
	);
}

#[test]
fn test_enum_comparison() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            Red = 1,
            Green,
            Blue,
        }
        fn is_red(c: Color) -> bool {
            c == Color::Red
        }
        export { is_red }
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| d.severity == Severity::Error)
		.collect();
	assert!(
		errors.is_empty(),
		"expected no errors comparing enum values: {:?}",
		errors
	);
}

#[test]
fn test_enum_missing_repr_is_error() {
	let case = TestCase::new(indoc! {"
        enum Color {
            Red,
            Green,
        }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::MissingEnumRepr),
		"expected E1036 (MissingEnumRepr), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_missing_repr_with_explicit_values_reports_once() {
	// Explicit variant values used to be type-checked against the repr type
	// even when the repr itself failed to resolve, cascading into a spurious
	// "unable to coerce"/"type annotation required" pair per variant on top
	// of the one real "enum requires a repr type" error.
	let case = TestCase::new(indoc! {"
        enum Direction {
            Right = 0,
            Down = 1,
            Left = 2,
            Up = 3,
        }
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| d.severity == Severity::Error)
		.collect();
	assert_eq!(
		errors.len(),
		1,
		"expected exactly one diagnostic, got: {:?}",
		errors.iter().map(|d| &d.message).collect::<Vec<_>>()
	);
	assert!(has_error_code(&case.tir, DiagnosticCode::MissingEnumRepr));
}

#[test]
fn test_enum_duplicate_variant_is_error() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            Red,
            Red,
        }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::DuplicateDefinition),
		"expected E1000 (DuplicateDefinition), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_repr_not_integer_is_error() {
	let case = TestCase::new(indoc! {"
        enum Color: bool {
            Red,
            Green,
        }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::EnumReprNotInteger),
		"expected E1055 (EnumReprNotInteger), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_constant_folds_arithmetic_for_auto_increment() {
	// Regression test for the motivating bug: `next_auto_value` used to only
	// special-case a bare `Int` literal, so `B` got a stale value instead of the
	// correctly-folded `1 + 1 = 2` from `A`.
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            A = 1 + 1,
            B,
        }
        export {}
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| d.severity == Severity::Error)
		.collect();
	assert!(errors.is_empty(), "{:?}", errors);

	let enum_ = case
		.tir
		.enums
		.iter()
		.find(|e| case.graph.interner.resolve(e.name.inner) == Some("Color"))
		.expect("Color enum not found");

	assert_eq!(enum_.variants[0].const_value, Some(ConstValue::Int(2)));
	assert_eq!(enum_.variants[1].const_value, Some(ConstValue::Int(3)));
}

#[test]
fn test_enum_negation_folds_for_signed_repr() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            A = -1,
        }
        export {}
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| d.severity == Severity::Error)
		.collect();
	assert!(errors.is_empty(), "{:?}", errors);
	let enum_ = case
		.tir
		.enums
		.iter()
		.find(|e| case.graph.interner.resolve(e.name.inner) == Some("Color"))
		.expect("Color enum not found");
	assert_eq!(enum_.variants[0].const_value, Some(ConstValue::Int(-1)));
}

#[test]
fn test_enum_duplicate_value_is_error() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            A,
            B = 0,
        }
        export {}
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::EnumDuplicateValue),
		"expected E1056 (EnumDuplicateValue) for auto-value colliding with explicit value, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_duplicate_value_groups_all_colliding_variants() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            A = 1,
            B = 1,
            C = 1,
        }
        export {}
    "});
	let dup_diags: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| {
			d.code.as_deref() == Some(DiagnosticCode::EnumDuplicateValue.code())
		})
		.collect();
	assert_eq!(
		dup_diags.len(),
		1,
		"expected exactly one grouped diagnostic for all three colliding variants, got: {:?}",
		case.tir.diagnostics
	);
	// Primary label (enum name) + one secondary label per colliding variant (3).
	assert_eq!(dup_diags[0].labels.len(), 4);
}

#[test]
fn test_enum_range_check_explicit_literal_is_error() {
	let case = TestCase::new(indoc! {"
        enum Color: i8 {
            A = 300,
        }
        export {}
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::IntegerLiteralOutOfRange),
		"expected E1004 (IntegerLiteralOutOfRange), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_auto_increment_overflow_is_error() {
	let case = TestCase::new(indoc! {"
        enum Color: u8 {
            A = 255,
            B,
        }
        export {}
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::IntegerLiteralOutOfRange),
		"expected E1004 (IntegerLiteralOutOfRange) for auto-increment overflowing u8, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_negative_value_on_unsigned_repr_is_error() {
	let case = TestCase::new(indoc! {"
        enum Color: u32 {
            A = -1,
        }
        export {}
    "});
	assert!(
		case.tir
			.diagnostics
			.iter()
			.any(|d| d.severity == Severity::Error),
		"expected an error for negative value on unsigned repr, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_type_mismatched_value_is_error() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            A = true,
        }
        export {}
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeMistmatch),
		"expected E1001 (TypeMistmatch), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_not_const_evaluatable_value_is_error() {
	// `%` by a literal `0` builds fine (it's a valid integer expression) but
	// doesn't fold — must not reuse `report_non_constant_global_initializer`'s
	// "add `mut`" wording, since enum variants (like consts) can never be `mut`.
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            A = 32 % 0,
        }
        export {}
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::NotConstEvaluatable),
		"expected E1057 (NotConstEvaluatable), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_const_not_const_evaluatable_value_is_error() {
	let case = TestCase::new(indoc! {"
        const GRID_W: i32 = 32 % 0;
        export {}
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::NotConstEvaluatable),
		"expected E1057 (NotConstEvaluatable), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_unused_is_warned() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            Red,
            Green,
        }
        export {}
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedItem.code())
			&& d.message.contains("Color")),
		"expected W1004 for unused enum `Color`, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_pub_enum_no_unused_warn() {
	let case = TestCase::new(indoc! {"
        pub enum Color: i32 {
            Red,
            Green,
        }
        export {}
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedItem.code())
			&& d.message.contains("Color")),
		"pub enum should not warn as unused even with no in-crate references"
	);
}

#[test]
fn test_enum_variant_unused_is_warned() {
	// The enum itself is used (so it doesn't get the whole-enum warning),
	// but `Green` is never referenced through `Color::Green`.
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            Red,
            Green,
        }
        fn get_red() -> Color {
            Color::Red
        }
        export { get_red }
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedEnumVariant.code())
			&& d.message.contains("Green")),
		"expected W1009 for unused variant `Green`, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedEnumVariant.code())
			&& d.message.contains("Red")),
		"`Red` is referenced and should not warn"
	);
}

#[test]
fn test_enum_all_variants_used_no_warn() {
	let case = TestCase::new(indoc! {"
        enum Color: i32 {
            Red,
            Green,
        }
        fn both(c: Color) -> bool {
            c == Color::Red || c == Color::Green
        }
        export { both }
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedEnumVariant.code())),
		"all variants referenced should not warn, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_two_unused_variants_grouped_without_oxford_comma() {
	let case = TestCase::new(indoc! {"
        enum Direction: i32 {
            Right,
            Down,
            Left,
        }
        fn get_right() -> Direction {
            Direction::Right
        }
        export { get_right }
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedEnumVariant.code())
			&& d.message == "variants `Down` and `Left` are never constructed"),
		"expected exact grouped message for 2 unused variants, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_five_unused_variants_grouped_with_oxford_comma() {
	let case = TestCase::new(indoc! {"
        enum Direction: i32 {
            Right,
            Down,
            Left,
            Up,
            Boo,
            Bar,
        }
        fn get_right() -> Direction {
            Direction::Right
        }
        export { get_right }
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedEnumVariant.code())
			&& d.message
				== "variants `Down`, `Left`, `Up`, `Boo`, and `Bar` are never constructed"),
		"expected exact grouped message for 5 unused variants, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_enum_six_unused_variants_collapses_to_generic_message() {
	let case = TestCase::new(indoc! {"
        enum Direction: i32 {
            Right,
            Down,
            Left,
            Up,
            Boo,
            Bar,
            Baz,
        }
        fn get_right() -> Direction {
            Direction::Right
        }
        export { get_right }
    "});
	let matches: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| {
			d.code.as_deref() == Some(DiagnosticCode::UnusedEnumVariant.code())
		})
		.collect();
	assert_eq!(
		matches.len(),
		1,
		"expected exactly one grouped diagnostic, got: {:?}",
		matches
	);
	assert_eq!(
		matches[0].message,
		"multiple variants are never constructed"
	);
	assert_eq!(
		matches[0].labels.len(),
		6,
		"expected one label per unused variant"
	);
}

#[test]
fn test_tagged_items_registered() {
	let case = TestCase::new(indoc! {"
        #[tag = \"my_trait\"]
        pub trait MyTrait {}

        #[tag = \"my_fn\"]
        pub fn my_function() {}
    "});
	assert!(case.tir.diagnostics.is_empty());
	let trait_key = case
		.graph
		.interner
		.get("my_trait")
		.expect("tag key not interned");
	let fn_key = case
		.graph
		.interner
		.get("my_fn")
		.expect("tag key not interned");
	let fn_def_id = *case
		.tir
		.tagged_items
		.get(&fn_key)
		.expect("fn tagged item not registered");
	assert!(
		case.tir.tagged_items.contains_key(&trait_key),
		"trait tagged item not registered"
	);
	assert!(case.tir.function_index(fn_def_id).is_some());
}

#[test]
fn test_tagged_items_registered_for_trait_members() {
	let case = TestCase::new(indoc! {"
        pub trait MyTrait {
            #[tag = \"my_assoc_const\"]
            const FOO: i32;
            #[tag = \"my_assoc_type\"]
            type Bar;
        }
    "});
	assert!(case.tir.diagnostics.is_empty());
	let const_key = case
		.graph
		.interner
		.get("my_assoc_const")
		.expect("tag key not interned");
	let type_key = case
		.graph
		.interner
		.get("my_assoc_type")
		.expect("tag key not interned");
	assert!(
		case.tir.tagged_items.contains_key(&const_key),
		"assoc const tagged item not registered"
	);
	assert!(
		case.tir.tagged_items.contains_key(&type_key),
		"assoc type tagged item not registered"
	);
}

#[test]
fn test_generic_impl_block_registers_and_dispatches() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };

        fn get_len(s: heap::[]u8) -> u32 {
            s.len()
        }

        export { get_len }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"unexpected diagnostics: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_generic_impl_bare_type_param_is_error() {
	let case = TestCase::new(indoc! {"
        impl<T> T {
            pub fn nope(self) {}
        }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::TypeMistmatch),
		"expected E1001 (TypeMistmatch/no nominal type) for bare type param impl, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_slice_range_full_is_ok() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        fn f(s: heap::[]u8) -> heap::[]u8 {
            s[..]
        }
        export { f }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_slice_range_with_bounds_is_ok() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        fn f(s: heap::[]u8, i: u32, n: u32) -> heap::[]u8 {
            s[i..n]
        }
        export { f }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_slice_range_on_array_is_ok() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        fn f(arr: heap::[4]u8) -> heap::[]u8 {
            arr[1..3]
        }
        export { f }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_slice_range_on_non_indexable_is_error() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        fn f(x: i32) -> heap::[]i32 {
            x[..]
        }
        export { f }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::IndexOnNonIndexable),
		"expected E1042 (IndexOnNonIndexable) for range-index on i32, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

// ── global initializers
// ──────────────────────────────────────────────────────────

#[test]
fn test_global_init_function_call_resolves() {
	let case = TestCase::new(indoc! {"
        fn compute() -> i32 { 42 as i32 }
        global mut result: i32 = compute()
        export { result }
    "});
	no_errors(&case);
	assert!(case.tir.globals[0].value.is_some());
}

#[test]
fn test_global_init_block_with_locals_resolves() {
	let case = TestCase::new(indoc! {"
        global mut x: i32 = {
            local a = 3 as i32;
            local b = 4 as i32;
            a + b
        }
        export { x }
    "});
	no_errors(&case);
	assert!(case.tir.globals[0].value.is_some());
}

#[test]
fn test_global_init_arithmetic_resolves() {
	let case = TestCase::new(indoc! {"
        global mut x: i32 = 2 + 3
        export { x }
    "});
	no_errors(&case);
	assert!(case.tir.globals[0].value.is_some());
}

#[test]
fn test_global_init_type_mismatch_reports_error() {
	let case = TestCase::new(indoc! {"
        global mut x: i32 = true
        export { x }
    "});
	assert!(has_error_code(&case.tir, DiagnosticCode::TypeMistmatch));
}

#[test]
fn test_global_init_cross_global_reference_resolves() {
	// g2's initializer reads g1 — g1 is in scope, so this type-checks cleanly.
	// At runtime g1 is already set before g2 (declaration order), so g2 = 10 + 1.
	let case = TestCase::new(indoc! {"
        global mut g1: i32 = 10
        global mut g2: i32 = g1 + 1
        export { g1, g2 }
    "});
	no_errors(&case);
	assert_eq!(case.tir.globals.len(), 2);
	assert!(case.tir.globals.iter().all(|g| g.value.is_some()));
}

#[test]
fn test_global_init_reverse_cross_reference_resolves() {
	// g2 is declared before g1, so when g2's initializer reads g1 the value
	// will be the WASM zero-default (g1 hasn't been set yet).
	// This is defined behaviour: type-checks clean, init order is declaration order.
	let case = TestCase::new(indoc! {"
        global mut g2: i32 = g1 + 1
        global mut g1: i32 = 10
        export { g1, g2 }
    "});
	no_errors(&case);
}

#[test]
fn test_global_init_if_expression_resolves() {
	let case = TestCase::new(indoc! {"
        fn flag() -> bool { true }
        global mut x: i32 = if flag() { 1 as i32 } else { 2 as i32 }
        export { x }
    "});
	no_errors(&case);
	assert!(case.tir.globals[0].value.is_some());
}

#[test]
fn test_global_initialized_to_data_end_tir() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };
        global mut bump: heap::*u8 = heap::DATA_END;
        export { heap }
    "});
	// Only warning expected: "never used" (no functions read bump in this test).
	assert!(
		case.tir
			.diagnostics
			.iter()
			.all(|d| d.severity
				!= codespan_reporting::diagnostic::Severity::Error)
	);
	assert_eq!(case.tir.globals.len(), 1);
}

#[test]
fn test_typeset_definition_registers_in_tir() {
	let case = TestCase::new(indoc! {"
        typeset Numbers { u8, i8, u16, i16, u32, i32, u64, i64 }
        fn identity<N: Numbers>(x: N) -> N { x }
        fn use_it() -> i32 { identity(42 as i32) }
        export { use_it }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
	// At least stdlib Integer + user Numbers typesets are registered
	assert!(!case.tir.typesets.is_empty());
	// The user-defined identity function has one type param with one typeset bound
	let identity = case
		.tir
		.functions
		.iter()
		.find(|f| {
			case.graph.interner.resolve(f.name.inner) == Some("identity")
				&& f.type_params.iter().any(|tp| tp.bounds.typeset.is_some())
		})
		.expect("no identity function with typeset bounds found");
	assert_eq!(identity.type_params.len(), 1);
	assert!(identity.type_params[0].bounds.typeset.is_some());
}

#[test]
fn test_typeset_bound_violation_reports_error() {
	let case = TestCase::new(indoc! {"
        typeset Numbers { u8, i8, u16, i16, u32, i32, u64, i64 }
        fn identity<N: Numbers>(x: N) -> N { x }
        fn main() -> f32 {
            identity(1.0 as f32)
        }
        export { main }
    "});
	assert!(case.tir.diagnostics.iter().any(|d| d.code.as_deref()
		== Some(DiagnosticCode::TypesetBoundViolation.code())));
}

#[test]
fn test_typeset_member_not_integer_reports_error() {
	let case = TestCase::new(indoc! {"
        typeset BadSet { u32, f32 }
        export { }
    "});
	assert!(case.tir.diagnostics.iter().any(|d| d.code.as_deref()
		== Some(DiagnosticCode::TypesetMemberNotInteger.code())));
}

#[test]
fn test_stdlib_integer_typeset_exists() {
	let case = TestCase::new(indoc! {"
        fn double<N: Integer>(x: N) -> N { x }
        fn use_it() -> i32 { double(21 as i32) }
        export { use_it }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_typeset_intersection_range_in_bounds() {
	// Integer intersection is [0, 127]; literals within that range are accepted
	let case = TestCase::new(indoc! {"
        fn make<N: Integer>(x: N) -> N { x }
        fn use_zero() -> i32 { make(0 as i32) }
        fn use_mid() -> u8 { make(100 as u8) }
        fn use_max() -> i8 { make(127 as i8) }
        export { use_zero, use_mid, use_max }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_typeset_intersection_range_literal_in_local() {
	// 0 and 100 are within Integer intersection [0, 127]; locals typed as TypeParam should be fine
	let case = TestCase::new(indoc! {"
        fn with_bounds<N: Integer>(x: N) -> N {
            local _lo: N = 0;
            local _hi: N = 100;
            x
        }
        fn use_it() -> i32 { with_bounds(50 as i32) }
        export { use_it }
    "});
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| {
			d.severity == codespan_reporting::diagnostic::Severity::Error
		})
		.collect();
	assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
}

#[test]
fn test_typeset_intersection_range_out_of_bounds_reports_error() {
	// Integer intersection max is 127; 200 is outside the safe range
	// This fires when assigning an untyped literal to a local of TypeParam type
	let case = TestCase::new(indoc! {"
        fn test<N: Integer>() {
            local x: N = 200;
        }
        fn use_it() { test() }
        export { use_it }
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::TypesetBoundViolation.code())),
		"expected E1047, got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_generic_slice_first_with_pointer_size_index() {
	// `self[0]` inside `impl<M: Memory, T> M::[]T` — the literal `0` must be
	// coerced to `M::Size`, whose typeset bound is `PointerSize { u32, u64 }`.
	// The intersection range for PointerSize is [0, u32::MAX], so `0` is valid.
	let case = TestCase::new(indoc! {"
        impl<M: Memory, T> M::[]T {
            pub fn first(self) -> T { self[0] }
        }
        export { }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"{:?}",
		case.tir.diagnostics
	);
}

// ── type-position namespace resolution ─────────────────────────────────────

#[test]
fn test_type_position_inline_module_registers_module_access() {
	// Accessing a type via an inline module path should register an LSP access
	// on the module declaration.
	let case = TestCase::new(indoc! {"
        module math {
            pub struct Vec2 { pub x: i32, pub y: i32 }
        }

        fn f(v: math::Vec2) { }

        export { f }
    "});
	no_errors(&case);
	assert!(
		case.tir.namespaces.iter().any(|ns| !ns.accesses.is_empty()),
		"expected at least one access registered on a namespace, got: {:?}",
		case.tir
			.namespaces
			.iter()
			.map(|ns| ns.accesses.len())
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_type_position_three_segment_inline_module_path() {
	// `outer::inner::Point` — three-segment type path through two nested inline
	// modules. Exercises the intermediate loop in resolve_type.
	let case = TestCase::new(indoc! {"
        module outer {
            pub module inner {
                pub struct Point { pub x: i32, pub y: i32 }
            }
        }

        fn f(p: outer::inner::Point) { }

        export { f }
    "});
	no_errors(&case);
}

#[test]
fn test_type_position_undeclared_in_module_path_is_error() {
	// `shapes::NonExistent` — the module exists but the type does not.
	// Should produce exactly one error (not a cascade) and not panic.
	let case = TestCase::new_multi_file(
		"src/main.wx",
		indoc! {"
            module shapes;

            fn f(x: shapes::NonExistent) { }

            export { f }
        "},
		&[("src/shapes.wx", "pub struct Point { pub x: i32 }")],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UndeclaredType),
		"expected E1021 (UndeclaredType) for missing type in module path, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_type_position_non_namespace_as_intermediate_is_error() {
	// `i32::Foo` — `i32` is a primitive, not a module; looking up an associated
	// type that doesn't exist should produce E1021 (UndeclaredType).
	let case = TestCase::new(indoc! {"
        fn f(x: i32::Foo) { }

        export { f }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UndeclaredType),
		"expected E1021 (UndeclaredType) when a primitive is used as a type namespace, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

#[test]
fn test_struct_init_three_segment_inline_module_path() {
	// `outer::inner::Point::{ x: 1, y: 2 }` — struct literal via a 3-segment
	// inline module path. Exercises the namespace_span tracking loop added to
	// build_struct_init_expression.
	let case = TestCase::new(indoc! {"
        module outer {
            pub module inner {
                pub struct Point { pub x: i32, pub y: i32 }
            }
        }

        fn make() -> outer::inner::Point {
            outer::inner::Point::{ x: 1, y: 2 }
        }

        export { make }
    "});
	no_errors(&case);
}

#[test]
fn test_struct_init_undeclared_type_in_module_path_is_error() {
	// `shapes::Ghost::{ }` — the module exists but `Ghost` is not defined there.
	let case = TestCase::new_multi_file(
		"src/main.wx",
		indoc! {"
            module shapes;

            fn f() -> shapes::Ghost {
                shapes::Ghost::{ }
            }

            export { f }
        "},
		&[("src/shapes.wx", "pub struct Point { pub x: i32 }")],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UndeclaredType),
		"expected E1021 (UndeclaredType) for missing struct in module path, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>(),
	);
}

/// `@size_of::<T, M>` returns `M::Size` projected from @size_of's own param list
/// (where M is at index 1).  The struct field `size: M::Size` is projected from
/// Layout's param list (where M is at index 0).  After substitution both must
/// normalise to the same TypeIndex so the struct init type-checks cleanly.
#[test]
fn test_assoc_type_projection_normalised_across_functions() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };

        struct Layout<M: Memory> {
            pub size: M::Size,
            pub align: M::Size,
        }

        impl<M: Memory> Layout<M> {
            pub fn of<T>() -> Layout<M> {
                Layout::{ size: size_of::<T, M>(), align: align_of::<T, M>() }
            }

            pub fn array<T>(count: M::Size) -> Layout<M> {
                Layout::{ size: size_of::<T, M>() * count, align: align_of::<T, M>() }
            }
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
fn test_assoc_type_as_memory_tag_in_trait() {
	// `Self::M` where M: Memory should be valid as a memory tag in pointer types
	// inside a trait definition.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };

        struct Layout<M: Memory> {
            size: M::Size,
            align: M::Size,
        }

        trait Allocator {
            type M: Memory;

            fn alloc(self: *Self, layout: Layout<Self::M>) -> Self::M::*u8;

            fn dealloc(self: *Self, ptr: Self::M::*u8, layout: Layout<Self::M>);
        }
    "});
	no_errors(&case);
}

#[test]
fn test_assoc_type_memory_bound_satisfied_by_memory_decl() {
	// `impl Allocator for BumpAllocator { type M = heap; }` — concrete memory
	// satisfies the `M: Memory` bound on the associated type.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };

        struct Layout<M: Memory> {
            size: M::Size,
            align: M::Size,
        }

        trait Allocator {
            type M: Memory;

            fn alloc(self: *Self, layout: Layout<Self::M>) -> Self::M::*u8;
        }

        struct BumpAllocator {}

        impl Allocator for BumpAllocator {
            type M = heap;

            fn alloc(self: *Self, layout: Layout<Self::M>) -> Self::M::*u8 {
                unreachable
            }
        }
    "});
	no_errors(&case);
}

// ── loop type inference ───────────────────────────────────────────────────────

#[test]
fn test_infinite_loop_has_never_type() {
	// `loop {}` with no break coerces to any return type — proves Never type.
	let case = TestCase::new(indoc! {"
        pub fn f() -> i32 { loop {} }
    "});
	no_errors(&case);
}

#[test]
fn test_loop_with_break_has_unit_type() {
	// bare `break` makes the loop yield Unit; returning it from a () fn is fine.
	let case = TestCase::new(indoc! {"
        pub fn f() { loop { break; } }
    "});
	no_errors(&case);
}

#[test]
fn test_loop_with_break_value_has_that_type() {
	// `break 42` makes the loop yield i32.
	let case = TestCase::new(indoc! {"
        pub fn f() -> i32 { loop { break 42; } }
    "});
	no_errors(&case);
}

#[test]
fn test_loop_with_continue_only_has_never_type() {
	// `continue` does not count as a break — loop still has Never type.
	let case = TestCase::new(indoc! {"
        pub fn f() -> i32 { loop { continue; } }
    "});
	no_errors(&case);
}

#[test]
fn test_break_outside_of_loop_reports_diagnostic() {
	let case = TestCase::new(indoc! {"
        pub fn f() { break; }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::BreakOutsideOfLoop),
		"expected E1012 (BreakOutsideOfLoop), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_continue_outside_of_loop_reports_diagnostic() {
	let case = TestCase::new(indoc! {"
        pub fn f() { continue; }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::ContinueOutsideOfLoop),
		"expected E1054 (ContinueOutsideOfLoop), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_break_with_undeclared_label_reports_only_that_diagnostic() {
	// No loop anywhere in `f`, so the label-less "outside of loop" check
	// must not also fire alongside the undeclared-label error.
	let case = TestCase::new(indoc! {"
        pub fn f() { break :outer; }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UndeclaredLabel),
		"expected E1011 (UndeclaredLabel), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
	assert_eq!(
		case.tir.diagnostics.len(),
		1,
		"expected only UndeclaredLabel, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_continue_with_undeclared_label_reports_only_that_diagnostic() {
	// Same as above but for `continue`.
	let case = TestCase::new(indoc! {"
        pub fn f() { continue :outer; }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UndeclaredLabel),
		"expected E1011 (UndeclaredLabel), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
	assert_eq!(
		case.tir.diagnostics.len(),
		1,
		"expected only UndeclaredLabel, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_unused_label_reports_diagnostic() {
	// `outer` is declared but never referenced by a `break`/`continue`.
	let case = TestCase::new(indoc! {"
        pub fn f() {
            outer: loop {
                break;
            }
        }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UnusedLabel),
		"expected W1008 (UnusedLabel), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_used_label_reports_no_unused_label_diagnostic() {
	// `outer` is referenced by `break :outer;`, so it must not be flagged.
	let case = TestCase::new(indoc! {"
        pub fn f() {
            outer: loop {
                break :outer;
            }
        }
    "});
	assert!(
		!has_error_code(&case.tir, DiagnosticCode::UnusedLabel),
		"did not expect W1008 (UnusedLabel), got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

// ── supertrait constraint tests ───────────────────────────────────────────────

#[test]
fn test_supertrait_single_level_satisfies_bound() {
	// T: B where B: A — passing T to a fn requiring A should type-check.
	let case = TestCase::new(indoc! {"
        trait A {}
        trait B: A {}
        fn requires_a<T: A>(x: T) {}
        fn call_with_b<T: B>(x: T) { requires_a(x); }
        export {}
    "});
	no_errors(&case);
}

#[test]
fn test_supertrait_two_levels_deep_satisfies_bound() {
	// T: C where C: B and B: A — passing T to a fn requiring A should type-check.
	let case = TestCase::new(indoc! {"
        trait A {}
        trait B: A {}
        trait C: B {}
        fn requires_a<T: A>(x: T) {}
        fn call_with_c<T: C>(x: T) { requires_a(x); }
        export {}
    "});
	no_errors(&case);
}

#[test]
fn test_nested_assoc_type_projection_resolves() {
	// `A::M::Size` — associated type of an associated type — must resolve
	// without error when `type M: Memory` is declared in the Allocator trait.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        trait Allocator {
            type M: Memory;
            fn alloc(mut self, size: Self::M::Size) -> Self::M::*u8;
        }
        struct Vec<T, A: Allocator> {
            len: A::M::Size,
        }
        export {}
    "});
	no_errors(&case);
}

#[test]
fn test_phantom_type_param_as_infer_is_error() {
	// T does not appear in the return type of `phantom`, so `_` for T
	// cannot be verified by the result-type check — it should still error.
	let case = TestCase::new(indoc! {"
        fn phantom<T>() -> i32 { 0 }
        fn f() -> i32 { phantom::<_>() }
    "});
	assert!(has_error_code(
		&case.tir,
		DiagnosticCode::TypeAnnotationRequired
	));
}

#[test]
fn test_phantom_type_param_suppressed_by_type_mismatch() {
	// When the argument for a NON-phantom param causes TypeMistmatch, the
	// phantom-param check is skipped to avoid double-reporting on the same
	// call site.  Only TypeMistmatch should appear.
	let case = TestCase::new(indoc! {"
        fn phantom<T>(x: i32) -> i32 { x }
        fn f() -> i32 { phantom::<_>(true) }
    "});
	assert!(has_error_code(&case.tir, DiagnosticCode::TypeMistmatch));
	assert!(!has_error_code(
		&case.tir,
		DiagnosticCode::TypeAnnotationRequired
	));
}

#[test]
fn test_phantom_type_param_suppressed_when_unrelated_arg_mismatches() {
	// Phantom param U is unrelated to the TypeMistmatch on y — but the check
	// is still suppressed.  Known limitation: fixing the TypeMistmatch will
	// then reveal the phantom error on U in a second compilation.
	//
	// `true` is a concrete bool (not a comptime literal), so T is properly
	// inferred as bool without triggering the comptime-literal annotation path.
	let case = TestCase::new(indoc! {"
        fn f<T, U>(x: T, y: i32) -> i32 { y }
        fn g() -> i32 { f(true, true) }
    "});
	assert!(has_error_code(&case.tir, DiagnosticCode::TypeMistmatch));
	assert!(!has_error_code(
		&case.tir,
		DiagnosticCode::TypeAnnotationRequired
	));
}

// ── unused type parameter warnings ───────────────────────────────────────────

#[test]
fn test_unused_type_param_warns() {
	let case = TestCase::new(indoc! {"
        pub fn phantom<T>() -> i32 { 0 }
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedTypeParam.code())
			&& d.message.contains('T')),
		"expected W1006 for phantom T, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_used_type_param_in_param_no_warn() {
	let case = TestCase::new(indoc! {"
        pub fn identity<T>(x: T) -> T { x }
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedTypeParam.code())),
		"T used in param and result should not warn"
	);
}

#[test]
fn test_implicit_self_type_param_in_trait_method_no_warn() {
	let case = TestCase::new(indoc! {"
        pub trait PointerSize {
            fn size() -> u32;
        }
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedTypeParam.code())),
		"implicit Self type param should not warn"
	);
}

#[test]
fn test_used_type_param_in_return_only_no_warn() {
	let case = TestCase::new(indoc! {"
        pub fn produce<T>() -> T { loop {} }
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedTypeParam.code())),
		"T used in return type should not warn"
	);
}

// ── unused struct field warnings ──────────────────────────────────────────────

#[test]
fn test_unused_field_init_but_not_read_warns() {
	let case = TestCase::new(indoc! {"
        pub struct Pair { pub x: i32, y: i32 }
        pub fn make(x: i32) -> Pair {
            Pair::{ x: x, y: 0 }
        }
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedStructField.code())
			&& d.message.contains('y')),
		"expected W1007 for private field `y` which is initialized but never read, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_unused_field_read_suppresses_warn() {
	let case = TestCase::new(indoc! {"
        pub struct Pair { x: i32, y: i32 }
        pub fn make(x: i32, y: i32) -> Pair { Pair::{ x: x, y: y } }
        pub fn get_x(p: Pair) -> i32 { p.x }
        pub fn get_y(p: Pair) -> i32 { p.y }
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedStructField.code())),
		"fields that are read should not warn"
	);
}

#[test]
fn test_pub_field_no_unused_warn() {
	let case = TestCase::new(indoc! {"
        pub struct Pair { pub x: i32, pub y: i32 }
        pub fn make() -> Pair { Pair::{ x: 1, y: 2 } }
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedStructField.code())),
		"pub fields should not warn even if never read in this file"
	);
}

#[test]
fn test_never_initialized_field_no_warn() {
	let case = TestCase::new(indoc! {"
        pub struct Node { value: i32, next: i32 }
        pub fn is_zero(n: Node) -> bool { false }
    "});
	assert!(
		!case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::UnusedStructField.code())),
		"fields that are never initialized should not warn (struct itself may be unused)"
	);
}

#[test]
fn test_type_param_multiple_bounds_both_enforced() {
	// `T: Scalable + Printable` — BoundList must be flattened so both bounds
	// end up in TypeParamInfo.bounds (exercises resolve_type_param_bounds).
	let case = TestCase::new(indoc! {"
        trait Scalable { fn scale(self, n: i32) -> i32; }
        trait Printable { fn print(self); }
        fn do_both<T: Scalable + Printable>(t: T) -> i32 {
            t.print();
            t.scale(1)
        }
    "});
	no_errors(&case);

	let func = case
		.tir
		.functions
		.iter()
		.find(|f| case.graph.interner.resolve(f.name.inner) == Some("do_both"))
		.expect("function 'do_both' not found");

	assert_eq!(func.type_params.len(), 1);
	assert_eq!(
		func.type_params[0].bounds.traits.len(),
		2,
		"T should have two bounds (Scalable and Printable)"
	);
}

#[test]
#[ignore = "TODO: TIR does not currently check trait bound satisfaction at generic call sites"]
fn test_type_param_multiple_bounds_missing_impl_is_error() {
	// Pass a type that only satisfies one of two bounds — should error once
	// call-site trait bound checking is implemented.
	let case = TestCase::new(indoc! {"
        trait Scalable { fn scale(self, n: i32) -> i32; }
        trait Printable { fn print(self); }
        fn do_both<T: Scalable + Printable>(t: T) {}
        struct Num {}
        impl Scalable for Num { fn scale(self, n: i32) -> i32 { n } }
        fn call() { do_both(Num::{}); }
    "});
	assert!(
		case.tir
			.diagnostics
			.iter()
			.any(|d| d.severity == Severity::Error),
		"expected an error: Num does not implement Printable"
	);
}

#[test]
fn test_multiple_supertraits_both_resolved() {
	// `trait Widget: Drawable + Sized` — both supertraits must appear in
	// tir.traits[widget_idx].supertraits (exercises BoundList flattening in
	// the supertrait resolution handler).
	let case = TestCase::new(indoc! {"
        trait Drawable { fn draw(self); }
        trait Sized { const SIZE: u32; }
        trait Widget: Drawable + Sized {}
    "});
	no_errors(&case);

	let widget_idx = case
		.tir
		.traits
		.iter()
		.position(|t| {
			case.graph.interner.resolve(t.name.inner) == Some("Widget")
		})
		.expect("trait 'Widget' not found");
	let drawable_idx = case
		.tir
		.traits
		.iter()
		.position(|t| {
			case.graph.interner.resolve(t.name.inner) == Some("Drawable")
		})
		.expect("trait 'Drawable' not found") as u32;
	let sized_idx = case
		.tir
		.traits
		.iter()
		.position(|t| {
			case.graph.interner.resolve(t.name.inner) == Some("Sized")
		})
		.expect("trait 'Sized' not found") as u32;

	let supertraits = &case.tir.traits[widget_idx].supertraits;
	assert_eq!(supertraits.len(), 2, "Widget should have two supertraits");
	assert!(
		supertraits.contains(&drawable_idx),
		"Drawable missing from supertraits"
	);
	assert!(
		supertraits.contains(&sized_idx),
		"Sized missing from supertraits"
	);
}

#[test]
fn test_multiple_supertraits_missing_one_impl_is_error() {
	// impl Widget for Point without impl Sized for Point — must error.
	let case = TestCase::new(indoc! {"
        trait Drawable { fn draw(self); }
        trait Sized { const SIZE: u32; }
        trait Widget: Drawable + Sized {}
        struct Point { x: i32 }
        impl Drawable for Point { fn draw(self) {} }
        impl Widget for Point {}
    "});
	assert!(
		case.tir.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::MissingSupertraitImpl.code())),
		"expected E1034 for missing Sized impl"
	);
}

#[test]
fn test_assoc_type_multiple_bounds_both_stored() {
	// `type Elem: A + B` — both bounds must be stored in the associated-type
	// entry (exercises BoundList flattening in TraitAssociatedType handler).
	let case = TestCase::new(indoc! {"
        trait A {}
        trait B {}
        trait Container {
            type Elem: A + B;
        }
    "});
	no_errors(&case);

	let container = case
		.tir
		.traits
		.iter()
		.find(|t| {
			case.graph.interner.resolve(t.name.inner) == Some("Container")
		})
		.expect("trait 'Container' not found");

	let elem_sym = case
		.graph
		.interner
		.get("Elem")
		.expect("symbol 'Elem' not interned");
	let assoc = container
		.assoc_types
		.get(&elem_sym)
		.expect("assoc type 'Elem' not found");
	assert_eq!(
		assoc.bounds.traits.len(),
		2,
		"Elem should have two trait bounds (A and B)"
	);
}

#[test]
fn test_assoc_type_multiple_bounds_violation_is_error() {
	// Provide a concrete type that satisfies A but not B — must error.
	let case = TestCase::new(indoc! {"
        trait A {}
        trait B {}
        impl A for i32 {}
        trait Container {
            type Elem: A + B;
        }
        struct Bag {}
        impl Container for Bag {
            type Elem = i32;
        }
    "});
	assert!(
		case.tir
			.diagnostics
			.iter()
			.any(|d| d.severity == Severity::Error),
		"expected an error: i32 does not implement B"
	);
}

#[test]
fn test_impl_module_trait_for_type_resolves() {
	// `impl module::Drawable for Point` — multi-segment trait_name must be
	// resolved via resolve_path_segments_as_type (not resolve_type).
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
            module shapes;
            use shapes::*;
            struct Point { x: i32 }
            impl shapes::Drawable for Point {
                fn draw(self) {}
            }
        "},
		&[(
			"shapes.wx",
			indoc! {"
                pub trait Drawable {
                    pub fn draw(self);
                }
            "},
		)],
	);
	no_errors(&case);

	let draw_sym = case
		.graph
		.interner
		.get("draw")
		.expect("symbol 'draw' not interned");
	let ti = case
		.tir
		.trait_impls
		.iter()
		.find(|ti| ti.members.contains_key(&draw_sym))
		.expect("no TraitImpl has 'draw' method");
	assert!(
		matches!(case.tir.types[ti.target.as_usize()], Type::Struct { .. }),
		"target should be Point (a struct)"
	);
}

#[test]
fn test_invalid_self_type_rejected() {
	let case = TestCase::new(indoc! {"
        struct Foo { x: i32 }
        impl Foo {
            pub fn bad(self: u32) -> i32 { 0 }
        }
        export { }
    "});
	assert!(
		case.tir
			.diagnostics
			.iter()
			.any(|d| d.code.as_deref() == Some("E1053")),
		"expected InvalidSelfType diagnostic, got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_valid_self_types_accepted() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Foo { x: i32 }
        impl Foo {
            pub fn by_value(self) -> i32 { 0 }
            pub fn by_const_ptr(self: heap::*Foo) -> i32 { 0 }
            pub fn by_mut_ptr(self: heap::*Foo) -> i32 { 0 }
        }
        export { }
    "});
	no_errors(&case);
}

#[test]
fn test_duplicate_method_name_in_impl_rejected() {
	let case = TestCase::new(indoc! {"
        struct Foo { x: i32 }
        impl Foo {
            pub fn bar(self) -> i32 { 0 }
            pub fn bar(self) -> i32 { 1 }
        }
        export { }
    "});
	assert!(
		case.tir
			.diagnostics
			.iter()
			.any(|d| d.code.as_deref() == Some("E1000")),
		"expected DuplicateDefinition diagnostic, got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_duplicate_method_name_in_generic_impl_rejected() {
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Vec<T> { len: u32 }
        impl<T> Vec<T> {
            pub fn len(self) -> u32 { 0 }
            pub fn len(self) -> u32 { 1 }
        }
        export { }
    "});
	assert!(
		case.tir
			.diagnostics
			.iter()
			.any(|d| d.code.as_deref() == Some("E1000")),
		"expected DuplicateDefinition diagnostic, got: {:?}",
		case.tir.diagnostics
	);
}

// ── Tree mutability verification ──────────────────────────────────────────────

#[test]
fn test_tree_mut_binding_mut_does_not_grant_write_through() {
	// `mut` on binding does NOT grant write-through — pointer type must be `*mut T`.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn bad(mut ptr: heap::*i32) { ptr.* = 42 }
    "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"mut binding + *i32 should NOT allow write-through; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_tree_mut_immutable_binding_mutable_pointer_write_ok() {
	// Immutable binding + `*mut T` IS sufficient for write-through.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn ok(ptr: heap::*mut i32) { ptr.* = 42 }
    "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_tree_mut_nested_inner_immutable_blocks_deep_write() {
	// `p: *mut *i32` — outer `*mut` allows storing a pointer, but inner `*i32` blocks write-through.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn bad(p: heap::*mut heap::*i32) { p.*.* = 99 }
    "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"p.*.* write should error: inner *i32 is immutable; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_tree_mut_nested_both_mutable_write_ok() {
	// `p: *mut *mut i32` — both levels mutable, p.*.* = val should work.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn ok(p: heap::*mut heap::*mut i32) { p.*.* = 99 }
    "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_tree_mut_struct_field_through_immutable_ptr_is_error() {
	// `ptr: *Node` — cannot write any field through it.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { x: i32 }
        fn bad(ptr: heap::*Node) { ptr.*.x = 1 }
    "},
		&[],
	);
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"field write through *Node should error; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_tree_mut_struct_field_through_mutable_ptr_ok() {
	// `ptr: *mut Node` — can write fields.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { x: i32 }
        fn ok(ptr: heap::*mut Node) { ptr.*.x = 1 }
    "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_tree_mut_mutable_ptr_coerces_to_immutable_param() {
	// Passing `*mut T` where `*T` is expected is allowed (safe downgrade).
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn read(ptr: heap::*i32) -> i32 { ptr.* }
        fn call(p: heap::*mut i32) -> i32 { read(p) }
    "},
		&[],
	);
	no_errors(&case);
}

#[test]
fn test_tree_mut_immutable_ptr_cannot_satisfy_mutable_param() {
	// Passing `*T` where `*mut T` is expected is NOT allowed.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn write(ptr: heap::*mut i32) { ptr.* = 1 }
        fn call(p: heap::*i32) { write(p) }
    "},
		&[],
	);
	assert!(
		!case.tir.diagnostics.is_empty(),
		"passing *i32 to *mut i32 param must error; got no diagnostics"
	);
}

#[test]
fn test_tree_mut_binding_mut_allows_reassign_but_not_write_through() {
	// `local mut p: *i32` — can reassign p, but cannot write through p.*.
	let case = TestCase::new_multi_file(
		"main.wx",
		indoc! {"
        memory heap: Memory where { Size = u32 };
        fn bad(a: heap::*i32, b: heap::*i32) {
            local mut p: heap::*i32 = a;
            p = b;
            p.* = 99
        }
    "},
		&[],
	);
	// Reassign `p = b` is ok (mut binding). Write `p.* = 99` must error (pointer type immutable).
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"write through *i32 must error even with mut binding; got: {:?}",
		case.tir.diagnostics
	);
	let errors: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| {
			d.severity == codespan_reporting::diagnostic::Severity::Error
		})
		.collect();
	assert_eq!(
		errors.len(),
		1,
		"expected exactly 1 error (write-through only, not the reassign); got: {:?}",
		errors
	);
}

#[test]
fn test_ptr_autoderef_calls_method_on_inner_type() {
	// `ptr.value()` on a concrete struct should resolve via auto-deref.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { value: i32 }
        impl Node {
            pub fn value(self: heap::*Node) -> i32 { self.*.value }
        }
        fn get(n: heap::*Node) -> i32 { n.value() }
        export { get }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"auto-deref method call should succeed; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_ptr_autoderef_mutable_ptr_calls_mut_self_method() {
	// `ptr.set()` on `*mut Node` should resolve to a method taking `self: heap::*mut Self`.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { value: i32 }
        impl Node {
            pub fn set(self: heap::*mut Node, v: i32) { self.*.value = v }
        }
        fn update(n: heap::*mut Node) { n.set(42) }
        export { update }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"auto-deref mut method call should succeed; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_ptr_autoderef_mutable_ptr_coerces_to_immutable_self() {
	// `*mut T` calling a method with `self: *T` should succeed via `*mut T → *T` coercion.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { val: i32 }
        impl Node {
            pub fn read(self: heap::*Node) -> i32 { self.*.val }
        }
        fn get(n: heap::*mut Node) -> i32 { n.read() }
        export { get }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"mutable pointer calling immutable-self method must succeed; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_ptr_autoderef_immutable_ptr_rejects_mut_self_method() {
	// `ptr.set()` on `*Node` (immutable) with `self: *mut Node` should be a type mismatch.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { value: i32 }
        impl Node {
            pub fn set(self: heap::*mut Node, v: i32) { self.*.value = v }
        }
        fn bad(n: heap::*Node) { n.set(42) }
        export { bad }
    "});
	assert!(
		!case.tir.diagnostics.is_empty(),
		"immutable pointer calling mut-self method must error"
	);
}

#[test]
fn test_ptr_autoderef_owned_self_reports_mismatch() {
	// Calling a method with `self: Node` (owned) via a pointer should report a type mismatch,
	// not "method not found". The user is expected to write `ptr.*.method()` to deref first.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { value: i32 }
        impl Node {
            pub fn owned(self: Node) -> i32 { self.value }
        }
        fn bad(n: heap::*Node) -> i32 { n.owned() }
        export { bad }
    "});
	assert!(
		!case.tir.diagnostics.is_empty(),
		"calling owned-self method via pointer must error"
	);
}

#[test]
fn test_ptr_autoderef_generic_impl_method() {
	// Auto-deref through a pointer to a generic struct should find the generic impl method.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Wrapper<T> { inner: T }
        impl <T> Wrapper<T> {
            pub fn get(self: heap::*Wrapper<T>) -> T { self.*.inner }
        }
        fn unwrap(w: heap::*Wrapper<i32>) -> i32 { w.get() }
        export { unwrap }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"auto-deref on generic impl method should succeed; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_ptr_autoderef_memory_qualifier_mismatch_errors() {
	// `other::*Node` calling a method with `self: heap::*Node` — the inner type `Node`
	// is found, but the self-param check fails because the memory qualifiers differ.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        memory other: Memory where { Size = u32 };
        struct Node { val: i32 }
        impl Node {
            pub fn read(self: heap::*Node) -> i32 { self.*.val }
        }
        fn bad(n: other::*Node) -> i32 { n.read() }
        export { bad }
    "});
	assert!(
		!case.tir.diagnostics.is_empty(),
		"calling heap method via other-memory pointer must error"
	);
}

#[test]
fn test_ptr_autoderef_double_pointer_not_found() {
	// `**Node` — auto-deref is one level only. The inner type is `*Node`, which has no
	// impl block, so the method is not found.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { val: i32 }
        impl Node {
            pub fn read(self: heap::*Node) -> i32 { self.*.val }
        }
        fn bad(n: heap::*heap::*Node) -> i32 { n.read() }
        export { bad }
    "});
	assert!(
		!case.tir.diagnostics.is_empty(),
		"double-pointer auto-deref must error — only one level deep"
	);
}

#[test]
fn test_ptr_field_access_does_not_auto_deref() {
	// `ptr.field` (no `.*`) — field access does NOT auto-deref pointers.
	// The user must write `ptr.*.field`.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { val: i32 }
        fn bad(n: heap::*Node) -> i32 { n.val }
        export { bad }
    "});
	assert!(
		!case.tir.diagnostics.is_empty(),
		"field access through pointer without deref must error"
	);
}

#[test]
fn test_ptr_autoderef_chained_calls() {
	// `ptr.next().get_val()` — `next()` returns `*Node`, then `get_val()` auto-derefs
	// the returned pointer. Each call goes through resolve_method_call independently.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Node { val: i32 }
        impl Node {
            pub fn next(self: heap::*Node) -> heap::*Node { self }
            pub fn get_val(self: heap::*Node) -> i32 { self.*.val }
        }
        fn chain(n: heap::*Node) -> i32 { n.next().get_val() }
        export { chain }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"chained auto-deref method calls should succeed; got: {:?}",
		case.tir.diagnostics
	);
}

// ── AddressOf (.& / .&mut) ─────────────────────────────────────────────────

#[test]
fn test_address_of_non_place_rejected() {
	// `.&` on a stack value (not a memory place) must emit a diagnostic.
	let case = TestCase::new(indoc! {"
        fn bad() -> i32 { (5 as i32).& }
        export { bad }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::InvalidAssignmentTarget),
		"expected InvalidAssignmentTarget for .& on a temporary; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_address_of_mut_through_immutable_pointer_rejected() {
	// `.&mut` through an immutable pointer must emit a diagnostic.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        fn bad(ptr: heap::*i32) -> heap::*mut i32 { ptr.*.&mut }
        export { bad }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::CannotMutateImmutable),
		"expected CannotMutateImmutable for .&mut through *i32; got: {:?}",
		case.tir.diagnostics
	);
}

#[test]
fn test_address_of_place_has_correct_pointer_type() {
	// `arr[i].&` on a heap array must resolve to a `heap::*i32` pointer type,
	// and `ptr.*.field.&` on a struct field must resolve to the field's pointer type.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        struct Point { x: i32, y: i32 }
        fn arr_elem_ptr(arr: heap::[4]i32, i: u32) -> heap::*i32 { arr[i].& }
        fn field_ptr(ptr: heap::*Point) -> heap::*i32 { ptr.*.x.& }
        export { arr_elem_ptr, field_ptr }
    "});
	assert!(
		case.tir.diagnostics.is_empty(),
		"unexpected diagnostics: {:?}",
		case.tir.diagnostics
	);
}

// ── Phase-1 duplicate detection (struct/enum/memory) ──────────────────────

#[test]
fn test_struct_triple_duplicate_attributes_to_first_definition() {
	// Regression test: `uses_foo` references `Foo` before any of the three
	// same-named structs are reached in the natural Phase-2 sweep, forcing
	// early resolution. Before the Phase-1 first-wins fix, this forced
	// resolution of whichever struct's `Pending` marker happened to survive
	// Phase 1's blind overwrite (the *last* one, C) — so B and C's
	// diagnostics both misattributed to C instead of the true first
	// definition, A. Both duplicates must now attribute to the same
	// (first) definition.
	let case = TestCase::new(indoc! {"
        fn uses_foo(x: Foo) -> i32 { 0 }
        struct Foo { a: i32 }
        struct Foo { b: i32 }
        struct Foo { c: i32 }
        export { }
    "});
	let dup_diags: Vec<_> = case
		.tir
		.diagnostics
		.iter()
		.filter(|d| {
			d.code.as_deref()
				== Some(DiagnosticCode::DuplicateDefinition.code())
		})
		.collect();
	assert_eq!(
		dup_diags.len(),
		2,
		"expected exactly 2 duplicate diagnostics (B and C dup of A), got: {:?}",
		case.tir.diagnostics
	);
	let previous_definition_ranges: Vec<_> = dup_diags
		.iter()
		.map(|d| {
			d.labels
				.iter()
				.find(|l| l.message.starts_with("previous definition"))
				.expect("missing previous-definition label")
				.range
				.clone()
		})
		.collect();
	assert_eq!(
		previous_definition_ranges[0], previous_definition_ranges[1],
		"both duplicates must attribute to the same (first) definition, got: {:?}",
		previous_definition_ranges
	);
}

#[test]
fn test_duplicate_enum_definition_is_error() {
	// Regression test: Enum's duplicate check used to have no `else`
	// branch reporting a diagnostic at all — two same-named enums were
	// silently accepted with the second one just dropped.
	let case = TestCase::new(indoc! {"
        enum Foo: i32 { A }
        enum Foo: i32 { B }
        export { }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::DuplicateDefinition),
		"expected duplicate definition error for two enums with same name, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_duplicate_memory_definition_is_error() {
	// Regression test: Memory had no duplicate check anywhere — two
	// same-named memories were silently accepted.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };
        memory heap: Memory where { Size = u32 };
        export { }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::DuplicateDefinition),
		"expected duplicate definition error for two memories with same name, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}

#[test]
fn test_true_false_are_keywords_not_shadowable() {
	// Regression test: `true`/`false` used to be resolved through the
	// ordinary identifier/symbol-table path, so a local named `true` or
	// `false` would silently shadow the boolean literal. They are now
	// keywords parsed directly into dedicated `Bool` expressions, so a
	// same-named local can never be referenced and is flagged as unused.
	let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            local true = false;
            if true { 1 } else { 2 }
        }
        export { f }
    "});
	assert!(
		has_error_code(&case.tir, DiagnosticCode::UnusedVariable),
		"expected the shadowed `local true` to be reported as unused, got: {:?}",
		case.tir
			.diagnostics
			.iter()
			.map(|d| &d.message)
			.collect::<Vec<_>>()
	);
}
