use indoc::indoc;

use super::*;
use crate::vfs::Files;

#[allow(unused)]
struct TestCase {
	interner: StringInterner,
	files: Files,
	ast: AST,
}

impl TestCase {
	fn new(source: &str) -> Self {
		let mut interner = StringInterner::new();
		let mut id_generator = DefIdGenerator::new();
		let mut files = Files::new();
		let file_id = files
			.add("main.wx".to_string(), source.to_string())
			.unwrap();
		let ast =
			Parser::parse(file_id, &files, &mut interner, &mut id_generator);
		TestCase {
			interner,
			files,
			ast,
		}
	}
}

fn diagnostic_codes(ast: &AST) -> Vec<&str> {
	ast.diagnostics
		.iter()
		.filter_map(|diagnostic| diagnostic.code.as_deref())
		.collect()
}

fn item(ast: &AST, index: usize) -> &Item {
	&ast.items[index].inner.inner
}

fn function_item(ast: &AST, index: usize) -> &Item {
	item(ast, index)
}

fn function_block(ast: &AST, index: usize) -> &[Separated<Spanned<Statement>>] {
	let Item::Function { block, .. } = function_item(ast, index) else {
		panic!("expected function item")
	};

	let Expression::Block { statements } = &block.inner else {
		panic!("expected function body block")
	};

	statements
}

fn statement_expression<'a>(
	statements: &'a [Separated<Spanned<Statement>>],
	index: usize,
) -> &'a Expression {
	let Statement::Expression(expr) = &statements[index].inner.inner else {
		panic!("expected expression statement")
	};

	&expr.inner
}

fn local_definition_value<'a>(
	statements: &'a [Separated<Spanned<Statement>>],
	index: usize,
) -> &'a Expression {
	let Statement::LocalDefinition { value, .. } =
		&statements[index].inner.inner
	else {
		panic!("expected local definition")
	};

	&value.inner
}

fn local_definition_pattern<'a>(
	statements: &'a [Separated<Spanned<Statement>>],
	index: usize,
) -> &'a Pattern {
	let Statement::LocalDefinition { pattern, .. } =
		&statements[index].inner.inner
	else {
		panic!("expected local definition")
	};

	&pattern.inner
}

fn struct_fields(
	ast: &AST,
	index: usize,
) -> &[Separated<Spanned<StructField>>] {
	let Item::Struct { fields, .. } = item(ast, index) else {
		panic!("expected struct item")
	};

	fields
}

// ── Top-level items ──────────────────────────────────────────────────────────

#[test]
fn test_top_level_items() {
	let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 {
            a + b
        }

        memory MEM: Memory where { Size = u32 };
        global mut counter: i32 = 0;
        const MAX: i32 = 100;

        struct Point {
            pub x: i32,
            y: i32,
        }

        import \"env\" {
            fn log(message: string)
        }

        enum Color {
            Red,
            Green = 1,
            Blue,
        }
    "});
	insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_enum_repr_after_name() {
	let case = TestCase::new(indoc! {"
        enum Status: i32 {
            Foo,
            Bar,
        }
    "});

	assert!(case.ast.diagnostics.is_empty());

	let Item::Enum {
		repr,
		name,
		variants,
		..
	} = item(&case.ast, 0)
	else {
		panic!("expected enum item")
	};

	assert_eq!(case.interner.resolve(name.inner), Some("Status"));
	assert!(matches!(
		repr.as_deref().map(|repr| &repr.inner),
		Some(TypeExpression::Path(p))
			if p.len() == 1
				&& case.interner.resolve(p[0].ident.inner) == Some("i32")
	));
	assert_eq!(variants.len(), 2);
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
fn test_type_expression_forms() {
	let case = TestCase::new(indoc! {"
        struct TypeForms {
            ptr: *u8,
            slice: []u8,
            array: [4]u8,
            tuple: (i32, u32),
            namespaced: math::Number,
        }
    "});

	assert!(
		case.ast.diagnostics.is_empty(),
		"unexpected diagnostics: {:?}",
		diagnostic_codes(&case.ast)
	);
	let Item::Struct { fields, .. } = item(&case.ast, 0) else {
		panic!("expected struct item")
	};

	assert!(matches!(
		Some(&fields[0].inner.inner.ty.inner),
		Some(TypeExpression::Pointer { .. })
	));
	assert!(matches!(
		Some(&fields[1].inner.inner.ty.inner),
		Some(TypeExpression::Slice { .. })
	));
	assert!(matches!(
		Some(&fields[2].inner.inner.ty.inner),
		Some(TypeExpression::Array {
			size: Spanned { inner: 4, .. },
			..
		})
	));
	assert!(matches!(
		Some(&fields[3].inner.inner.ty.inner),
		Some(TypeExpression::Tuple { elements }) if elements.len() == 2
	));
	assert!(matches!(
		Some(&fields[4].inner.inner.ty.inner),
		Some(TypeExpression::Path(p)) if p.len() == 2
	));
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
fn test_impl_trait_for_type() {
	// impl Trait for Type — trait implementation block
	let case = TestCase::new(indoc! {"
        impl Drawable for Point {
            fn draw(self) {
                draw_point(self.x, self.y)
            }
        }
    "});
	insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_trait_items() {
	let case = TestCase::new(indoc! {"
        trait Widget: Drawable + Sized {
            const SIZE: u32;

            fn render(self);

            #[inline]
            fn grow(self, delta: u32) -> u32 {
                delta
            }
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
fn test_label_requires_block_like_expression() {
	let case = TestCase::new(indoc! {"
        fn f(value: i32) {
            target: value
        }
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0006"]);
	assert!(function_block(&case.ast, 0).is_empty());
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
fn test_generic_struct_init() {
	let case = TestCase::new(indoc! {"
        fn make(x: f32, y: f32) {
            local p = Point::<f32>::{ x: x, y: y };
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let stmts = function_block(&case.ast, 0);
	let init = local_definition_value(stmts, 0);
	let Expression::StructInit { path, fields } = init else {
		panic!("expected StructInit");
	};
	// path must be a single-segment path with one type arg: `Point::<f32>`
	assert_eq!(path.len(), 1);
	assert_eq!(path[0].type_args.len(), 1);
	assert_eq!(fields.len(), 2);
}

#[test]
fn test_grouping_and_tuple_expressions() {
	let case = TestCase::new(indoc! {"
        fn shapes(x: i32, y: i32) {
            local grouped = (x);
            local single = (x,);
            local pair = (x, y);
        }
    "});

	assert!(case.ast.diagnostics.is_empty());
	let statements = function_block(&case.ast, 0);
	assert!(matches!(
		local_definition_value(statements, 0),
		Expression::Grouping { .. }
	));
	assert!(matches!(
		local_definition_value(statements, 1),
		Expression::Tuple { elements } if elements.len() == 1
	));
	assert!(matches!(
		local_definition_value(statements, 2),
		Expression::Tuple { elements } if elements.len() == 2
	));
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
	let case = TestCase::new(indoc! {"
        fn f(a: i32, b: i32, c: i32) -> i32 {
            a + b * c
        }
    "});

	let statements = function_block(&case.ast, 0);
	let Expression::Binary { right, .. } = statement_expression(statements, 0)
	else {
		panic!("expected outer binary expression")
	};
	assert!(
		matches!(
			&right.inner,
			Expression::Binary {
				operator: Spanned {
					inner: BinaryOp::Mul,
					..
				},
				..
			}
		),
		"expected multiplication on the right-hand side of addition"
	);
}

#[test]
fn test_left_associativity() {
	let case = TestCase::new(indoc! {"
        fn f(a: i32, b: i32, c: i32) -> i32 {
            a - b - c
        }
    "});

	let statements = function_block(&case.ast, 0);
	let Expression::Binary { left, operator, .. } =
		statement_expression(statements, 0)
	else {
		panic!("expected outer binary expression")
	};
	assert_eq!(operator.inner, BinaryOp::Sub);
	assert!(
		matches!(
			&left.inner,
			Expression::Binary {
				operator: Spanned {
					inner: BinaryOp::Sub,
					..
				},
				..
			}
		),
		"expected subtraction to associate to the left"
	);
}

#[test]
fn test_cast_precedence() {
	let case = TestCase::new(indoc! {"
        fn arith(a: i32, b: i32) -> i32 { a + b as i32 }
        fn unary(x: i32) -> i32 { -x as i32 }
    "});

	let arithmetic = function_block(&case.ast, 0);
	let Expression::Binary { right, .. } = statement_expression(arithmetic, 0)
	else {
		panic!("expected arithmetic binary expression")
	};
	assert!(
		matches!(&right.inner, Expression::Cast { .. }),
		"expected cast to bind tighter than addition"
	);

	let unary = function_block(&case.ast, 1);
	let Expression::Unary { operand, .. } = statement_expression(unary, 0)
	else {
		panic!("expected unary expression")
	};
	assert!(
		matches!(&operand.inner, Expression::Cast { .. }),
		"expected cast to bind tighter than unary negation"
	);
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
fn test_address_of_immutable() {
	// ptr.*.& — immutable address-of; value is Deref, mut_span is None
	let case = TestCase::new(indoc! {"
        fn f(ptr: *i32) {
            local a = ptr.*.&;
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let stmts = function_block(&case.ast, 0);
	let expr = local_definition_value(stmts, 0);
	let Expression::AddressOf { value, mut_span } = expr else {
		panic!("expected AddressOf, got {expr:?}");
	};
	assert!(mut_span.is_none(), "expected no mut_span");
	assert!(
		matches!(value.inner, Expression::Deref { .. }),
		"expected Deref as AddressOf operand"
	);
}

#[test]
fn test_address_of_mutable() {
	// ptr.*.&mut — mutable address-of; mut_span is Some
	let case = TestCase::new(indoc! {"
        fn f(ptr: *mut i32) {
            local a = ptr.*.&mut;
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let stmts = function_block(&case.ast, 0);
	let expr = local_definition_value(stmts, 0);
	let Expression::AddressOf { mut_span, .. } = expr else {
		panic!("expected AddressOf, got {expr:?}");
	};
	assert!(mut_span.is_some(), "expected mut_span for .&mut");
}

#[test]
fn test_address_of_through_field() {
	// ptr.*.field.& — address-of a field: AddressOf > ObjectAccess > Deref
	let case = TestCase::new(indoc! {"
        fn f(ptr: *Point) {
            local a = ptr.*.x.&;
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let stmts = function_block(&case.ast, 0);
	let expr = local_definition_value(stmts, 0);
	let Expression::AddressOf { value, mut_span } = expr else {
		panic!("expected AddressOf, got {expr:?}");
	};
	assert!(mut_span.is_none());
	let Expression::ObjectAccess { object, .. } = &value.inner else {
		panic!("expected ObjectAccess inside AddressOf");
	};
	assert!(
		matches!(object.inner, Expression::Deref { .. }),
		"expected Deref as root of field place"
	);
}

#[test]
fn test_numeric_literal_forms() {
	let case = TestCase::new(indoc! {"
        fn f() {
            local hex    = 0xFF;
            local binary = 0b1010;
            local sep    = 1_000_000;
        }
    "});

	let statements = function_block(&case.ast, 0);
	assert_eq!(statements.len(), 3);
	assert!(matches!(
		local_definition_value(statements, 0),
		Expression::Int { value: 255 }
	));
	assert!(matches!(
		local_definition_value(statements, 1),
		Expression::Int { value: 10 }
	));
	assert!(matches!(
		local_definition_value(statements, 2),
		Expression::Int { value: 1_000_000 }
	));
}

// ── Patterns ─────────────────────────────────────────────────────────────────

#[test]
fn test_pattern_simple_binding() {
	let case = TestCase::new(indoc! {"
        fn f(v: i32) {
            local x = v;
            local mut y = v;
            local _ = v;
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let stmts = function_block(&case.ast, 0);

	assert!(matches!(
		local_definition_pattern(stmts, 0),
		Pattern::Binding { mut_span: None, name } if case.interner.resolve(name.inner) == Some("x")
	));
	assert!(matches!(
		local_definition_pattern(stmts, 1),
		Pattern::Binding { mut_span: Some(_), name } if case.interner.resolve(name.inner) == Some("y")
	));
	assert!(matches!(
		local_definition_pattern(stmts, 2),
		Pattern::Wildcard
	));
}

#[test]
fn test_pattern_tuple_destructuring() {
	let case = TestCase::new(indoc! {"
        fn f(pair: (i32, i32)) {
            local (a, b) = pair;
            local (mut c, _) = pair;
            local (x, (y, z)) = pair;
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let stmts = function_block(&case.ast, 0);

	let Pattern::Tuple { elements } = local_definition_pattern(stmts, 0) else {
		panic!("expected tuple pattern")
	};
	assert_eq!(elements.len(), 2);
	assert!(matches!(
		&elements[0].inner.inner,
		Pattern::Binding { mut_span: None, .. }
	));
	assert!(matches!(
		&elements[1].inner.inner,
		Pattern::Binding { mut_span: None, .. }
	));

	let Pattern::Tuple { elements } = local_definition_pattern(stmts, 1) else {
		panic!("expected tuple pattern")
	};
	assert!(matches!(
		&elements[0].inner.inner,
		Pattern::Binding {
			mut_span: Some(_),
			..
		}
	));
	assert!(matches!(&elements[1].inner.inner, Pattern::Wildcard));

	let Pattern::Tuple { elements } = local_definition_pattern(stmts, 2) else {
		panic!("expected tuple pattern")
	};
	assert!(matches!(&elements[1].inner.inner, Pattern::Tuple { .. }));
}

#[test]
fn test_pattern_struct_destructuring() {
	let case = TestCase::new(indoc! {"
        fn f(p: Point) {
            local Point { x, y } = p;
            local Point { x: a, y: b } = p;
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let stmts = function_block(&case.ast, 0);

	let Pattern::Struct { name, fields } = local_definition_pattern(stmts, 0)
	else {
		panic!("expected struct pattern")
	};
	assert_eq!(case.interner.resolve(name.inner), Some("Point"));
	assert_eq!(fields.len(), 2);
	assert!(
		fields[0].inner.inner.pattern.is_none(),
		"shorthand field should have no sub-pattern"
	);
	assert!(
		fields[1].inner.inner.pattern.is_none(),
		"shorthand field should have no sub-pattern"
	);

	let Pattern::Struct { fields, .. } = local_definition_pattern(stmts, 1)
	else {
		panic!("expected struct pattern")
	};
	assert!(
		fields[0].inner.inner.pattern.is_some(),
		"renamed field should have sub-pattern"
	);
	assert!(
		fields[1].inner.inner.pattern.is_some(),
		"renamed field should have sub-pattern"
	);
}

#[test]
fn test_pattern_with_type_annotation() {
	let case = TestCase::new(indoc! {"
        fn f(pair: (i32, i32)) {
            local (a, b): (i32, i32) = pair;
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let stmts = function_block(&case.ast, 0);

	let Statement::LocalDefinition { pattern, ty, .. } = &stmts[0].inner.inner
	else {
		panic!("expected local definition")
	};
	assert!(matches!(pattern.inner, Pattern::Tuple { .. }));
	assert!(ty.is_some(), "type annotation should be present");
}

// ── Diagnostics ──────────────────────────────────────────────────────────────

#[test]
fn test_missing_semicolon_warns_but_parses() {
	let case = TestCase::new(indoc! {"
        fn f(x: i32) -> i32 {
            local y: i32 = x
            y
        }
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0003"]);
	assert_eq!(function_block(&case.ast, 0).len(), 2);
}

#[test]
fn test_unclosed_delimiter() {
	let case = TestCase::new(indoc! {"
        fn f() {
            local x: i32 = 1;
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0004"]);
	assert_eq!(function_block(&case.ast, 0).len(), 1);
}

#[test]
fn test_invalid_function_param_type_reports_parse_error() {
	let case = TestCase::new(indoc! {"
        fn f(x: =) {}
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0002"]);
}

#[test]
fn test_invalid_integer_literal() {
	let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            99999999999999999999
        }
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0005"]);
	assert!(matches!(
		statement_expression(function_block(&case.ast, 0), 0),
		Expression::Int { value: 0 }
	));
}

#[test]
fn test_incomplete_expression() {
	let case = TestCase::new(indoc! {"
        fn binary() -> i32 { 1 + }
        fn unary()  -> i32 { -   }
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0006", "E0006"]);
	assert!(function_block(&case.ast, 0).is_empty());
	assert!(function_block(&case.ast, 1).is_empty());
}

#[test]
fn test_reserved_identifier() {
	let case = TestCase::new(indoc! {"
        fn f() {
            local fn = 1;
        }
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0008"]);
	assert_eq!(function_block(&case.ast, 0).len(), 1);
}

#[test]
fn test_invalid_attribute_and_namespace_diagnostics() {
	let case = TestCase::new(indoc! {"
        #[123]
        fn attr() {}

        fn namespace_error(x: i32, y: i32) {
            (x + y)::value
        }
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0012", "E0009", "E0013"]);
	assert_eq!(case.ast.items.len(), 2);
	assert!(matches!(item(&case.ast, 0), Item::Function { .. }));
}

#[test]
fn test_missing_initializer() {
	let case = TestCase::new(indoc! {"
        fn f() {
            local x: i32
        }
        global y: i32
    "});
	let e0010_count = case
		.ast
		.diagnostics
		.iter()
		.filter(|d| d.code.as_deref() == Some("E0010"))
		.count();
	assert_eq!(
		e0010_count, 2,
		"expected one E0010 for local and one for global"
	);
	assert_eq!(case.ast.items.len(), 1);
	assert!(function_block(&case.ast, 0).is_empty());
}

#[test]
fn test_missing_comma_between_struct_fields_warns_but_parses() {
	let case = TestCase::new(indoc! {"
        struct Pair {
            left: i32
            right: i32,
        }
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0003"]);
	assert_eq!(struct_fields(&case.ast, 0).len(), 2);
}

#[test]
fn test_module_pub_items_and_associated_types() {
	let case = TestCase::new(indoc! {"
        pub module math {
            pub fn zero() -> i32 {
                0
            }
        }

        pub struct Counter {
            value: i32,
        }

        pub trait Iterator {
            type Item: Show + Clone;
            fn next(self) -> Self::Item;
        }

        impl Iterator for Range {
            type Item = i32;

            fn next(self) -> Self::Item {
                0
            }
        }
    "});

	assert!(case.ast.diagnostics.is_empty());

	let Item::Module {
		pub_span, items, ..
	} = item(&case.ast, 0)
	else {
		panic!("expected public module")
	};
	assert!(pub_span.is_some());
	assert!(matches!(
		items[0].inner.inner,
		Item::Function {
			pub_span: Some(_),
			..
		}
	));

	let Item::Struct { pub_span, .. } = item(&case.ast, 1) else {
		panic!("expected public struct")
	};
	assert!(pub_span.is_some());

	let Item::Trait {
		pub_span,
		items: trait_items,
		..
	} = item(&case.ast, 2)
	else {
		panic!("expected public trait")
	};
	assert!(pub_span.is_some());
	assert!(matches!(
		trait_items[0].inner.inner,
		TraitItem::AssociatedType { ref bounds, .. }
			if matches!(
				bounds.as_ref().map(|b| &b.inner),
				Some(BoundExpression::BoundList(list)) if list.len() == 2
			)
	));

	let Item::ImplTrait {
		items: impl_items, ..
	} = item(&case.ast, 3)
	else {
		panic!("expected trait impl")
	};
	assert!(matches!(
		impl_items[0].inner.inner,
		ImplItem::AssociatedType { .. }
	));
}

#[test]
fn test_external_module_item() {
	let case = TestCase::new("module math;");

	assert!(case.ast.diagnostics.is_empty());

	let Item::ModuleDeclaration { pub_span, name } = item(&case.ast, 0) else {
		panic!("expected external module")
	};

	assert!(pub_span.is_none());
	assert_eq!(case.interner.resolve(name.inner), Some("math"));
}

#[test]
fn test_chained_comparison_error() {
	let case = TestCase::new(indoc! {"
        fn f(a: i32, b: i32, c: i32) -> bool {
            a < b < c
        }
    "});

	assert_eq!(diagnostic_codes(&case.ast), vec!["E0007"]);
	let Expression::Binary { left, operator, .. } =
		statement_expression(function_block(&case.ast, 0), 0)
	else {
		panic!("expected outer comparison")
	};
	assert_eq!(operator.inner, BinaryOp::Less);
	assert!(matches!(
		&left.inner,
		Expression::Binary {
			operator: Spanned {
				inner: BinaryOp::Less,
				..
			},
			..
		}
	));
}

// ── Generics ─────────────────────────────────────────────────────────────────

#[test]
fn test_generic_signatures() {
	let case = TestCase::new(indoc! {"
        fn zip<T, U: Show + Clone, V: Scalable>(left: T, middle: U, right: V) -> U {
            middle
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_generic_struct() {
	let case = TestCase::new(indoc! {"
        struct Pair<T, U> {
            first: T,
            second: U,
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let Item::Struct {
		name,
		type_params,
		fields,
		..
	} = item(&case.ast, 0)
	else {
		panic!("expected struct item")
	};
	assert_eq!(case.interner.resolve(name.inner), Some("Pair"));
	assert_eq!(type_params.len(), 2);
	assert_eq!(case.interner.resolve(type_params[0].name.inner), Some("T"));
	assert!(type_params[0].bounds.is_none());
	assert_eq!(case.interner.resolve(type_params[1].name.inner), Some("U"));
	assert!(type_params[1].bounds.is_none());
	assert_eq!(fields.len(), 2);
}

#[test]
fn test_generic_struct_with_bounds() {
	let case = TestCase::new(indoc! {"
        struct Wrapper<T: Add + Clone> {
            value: T,
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let Item::Struct { type_params, .. } = item(&case.ast, 0) else {
		panic!("expected struct item")
	};
	assert_eq!(type_params.len(), 1);
	assert!(matches!(
		type_params[0].bounds.as_ref().map(|b| &b.inner),
		Some(BoundExpression::BoundList(list)) if list.len() == 2
	));
}

#[test]
fn test_import_alias_and_entry_kinds() {
	let case = TestCase::new(indoc! {"
        import \"env\" as host {
            fn log(message: string);
            global mut counter: i32;
            memory MEM: Memory where { Size = u32 };
        }
    "});

	assert!(case.ast.diagnostics.is_empty());
	let Item::Import { alias, entries, .. } = item(&case.ast, 0) else {
		panic!("expected import block")
	};
	assert!(alias.is_some());
	assert!(matches!(
		entries[0].inner.inner.declaration,
		ImportDeclaration::Function { .. }
	));
	assert!(matches!(
		entries[1].inner.inner.declaration,
		ImportDeclaration::Global {
			mut_span: Some(_),
			..
		}
	));
	assert!(matches!(
		entries[2].inner.inner.declaration,
		ImportDeclaration::Memory { .. }
	));
}

#[test]
fn test_turbofish_call() {
	let case = TestCase::new(indoc! {"
        fn main() {
            identity::<i32>(42)
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_turbofish_method_call() {
	let case = TestCase::new(indoc! {"
        fn main(obj: Foo) {
            obj.transform::<i32>()
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	insta::assert_yaml_snapshot!(case.ast);
}

#[test]
fn test_generic_application_type_args() {
	let case = TestCase::new(indoc! {"
        struct Pair<T, U> {
            first: T,
            second: U,
        }
        fn make(x: Pair<i32, f64>) {}
    "});
	assert!(case.ast.diagnostics.is_empty());
	let Item::Function { signature, .. } = item(&case.ast, 1) else {
		panic!("expected function")
	};
	let param_ty = &signature.params[0].inner.inner.ty.as_ref().unwrap().inner;
	assert!(matches!(
		param_ty,
		TypeExpression::GenericApplication { args, .. } if args.len() == 2
	));
	if let TypeExpression::GenericApplication { args, .. } = param_ty {
		assert!(matches!(
			&args[0].inner,
			Spanned {
				inner: TypeExpression::Path(_),
				..
			}
		));
		assert!(matches!(
			&args[1].inner,
			Spanned {
				inner: TypeExpression::Path(_),
				..
			}
		));
	}
}

#[test]
fn test_double_right_arrow_split() {
	// Regression: `>>` in nested generics was eagerly lexed as `DoubleRightArrow`
	// instead of two separate `>` tokens. Test across type expressions and
	// turbofish in bounds.
	let case = TestCase::new(indoc! {"
        fn type_expr(x: Outer<Inner<u32>>) {}
        fn bound_turbofish<T: Wrapper::<Inner<u32>>>(t: T) {}
        fn method_turbofish(obj: Foo) { obj.transform::<Vec<u32>>() }
    "});
	assert!(case.ast.diagnostics.is_empty());
}

#[test]
fn test_where_binding_parses_as_bound_with_bindings() {
	let case = TestCase::new(indoc! {"
        fn f<T: Memory where { Size = u32 }>(t: T) {}
    "});
	assert!(case.ast.diagnostics.is_empty());
	let Item::Function { signature, .. } = item(&case.ast, 0) else {
		panic!("expected function")
	};
	let bounds = signature.type_params[0]
		.bounds
		.as_ref()
		.expect("expected bounds");
	let BoundExpression::WithBindings { path, bindings } = &bounds.inner else {
		panic!("expected WithBindings")
	};
	assert!(matches!(path.as_ref(), BoundExpression::Path(_)));
	assert_eq!(bindings.len(), 1);
}

#[test]
fn test_impl_trait_multi_segment_trait_name() {
	// `impl gfx::Drawable for Point` — trait_name must be two path segments.
	let case = TestCase::new(indoc! {"
        impl gfx::Drawable for Point {
            fn draw(self) {}
        }
    "});
	assert!(case.ast.diagnostics.is_empty());
	let Item::ImplTrait { trait_name, .. } = item(&case.ast, 0) else {
		panic!("expected ImplTrait")
	};
	assert_eq!(
		trait_name.len(),
		2,
		"expected two path segments: gfx, Drawable"
	);
}
