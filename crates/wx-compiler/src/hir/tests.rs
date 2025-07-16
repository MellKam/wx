// use codespan_reporting::{
//     diagnostic::Diagnostic,
//     term::{
//         self,
//         termcolor::{ColorChoice, StandardStream},
//     },
// };
// use string_interner::StringInterner;

// use super::diagnostics::*;
// use super::*;
// use crate::files::Files;

// #[allow(unused)]
// fn print_diagnostics(diagnostics: &[Diagnostic<FileId>], files: &Files) {
//     let writer = StandardStream::stderr(ColorChoice::Always);
//     let config = codespan_reporting::term::Config::default();

//     for diagnostic in diagnostics.iter() {
//         term::emit(&mut writer.lock(), &config, files, diagnostic).unwrap();
//     }
// }

// #[allow(dead_code)]
// struct TestCase {
//     interner: StringInterner<StringBackend>,
//     files: Files,
//     ast: ast::Ast,
//     ast_diagnostics: Vec<Diagnostic<FileId>>,
//     hir: HIR,
//     hir_diagnostics: Vec<Diagnostic<FileId>>,
// }

// fn build_test_case(source: &str) -> TestCase {
//     let mut interner = StringInterner::new();
//     let mut files = Files::new();
//     let file_id = files
//         .add("test.wx".to_string(), source.to_string())
//         .unwrap();

//     let (ast, ast_diagnostics) =
//         ast::parser::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

//     let (hir, hir_diagnostics) = Builder::build(&ast, &mut interner);

//     TestCase {
//         interner,
//         files,
//         ast,
//         ast_diagnostics,
//         hir,
//         hir_diagnostics,
//     }
// }

// #[test]
// fn should_coerce_never_expression() {
//     let TestCase {
//         hir,
//         hir_diagnostics,
//         ..
//     } = build_test_case("func test(): unit { unreachable }");

//     assert_eq!(hir_diagnostics.len(), 0);
//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(
//         hir.functions[0].stack.scopes[0].expected_type,
//         Some(Type::Unit)
//     );
//     assert_eq!(
//         hir.functions[0].stack.scopes[0].inferred_type,
//         Some(Type::Never)
//     );
//     assert_eq!(hir.functions[0].block.ty, Some(Type::Never));
// }

// #[test]
// fn should_infer_local_type() {
//     let TestCase {
//         hir,
//         hir_diagnostics,
//         ..
//     } = build_test_case("func test(a: i32): i32 { local x = a; x }");

//     assert_eq!(hir_diagnostics.len(), 0);
//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(
//         hir.functions[0].stack.scopes[0].locals[0].ty,
//         Type::Primitive(PrimitiveType::I32)
//     );
//     assert_eq!(
//         hir.functions[0].stack.scopes[0].inferred_type,
//         Some(Type::Primitive(PrimitiveType::I32))
//     );
// }

// #[test]
// fn should_report_invalid_assignment_target() {
//     let TestCase {
//         hir,
//         hir_diagnostics,
//         ..
//     } = build_test_case("func test(): unit { 5 = 5 }");

//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         InvalidAssignmentTargetDiagnostic::CODE
//     );
// }

// #[test]
// fn should_report_break_outside_of_loop() {
//     let TestCase {
//         hir,
//         hir_diagnostics,
//         ..
//     } = build_test_case("func test(): unit { break }");

//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].block.ty.unwrap(), Type::Never);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         BreakOutsideOfLoopDiagnostic::CODE
//     );
// }

// #[test]
// fn should_report_operator_cannot_be_applied() {
//     let TestCase {
//         hir,
//         hir_diagnostics,
//         ..
//     } = build_test_case("func test(): unit { 5 + true }");

//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         BinaryOperatorCannotBeAppliedDiagnostic::CODE
//     );
// }

// #[test]
// fn should_report_unable_to_coerce() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case("func test(): unit { 5 }");

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].block.ty, Some(Type::Unit));
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         UnableToCoerceDiagnostic::CODE
//     );
// }

// #[test]
// fn should_report_unreachable_code() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case(
//         "func test(): unit {
//                 loop {};
//                 hello();
//                 world();
//                 unreachable
//             }",
//     );

//     assert_eq!(hir.functions.len(), 1);
//     match &hir.functions[0].block.kind {
//         ExprKind::Block { expressions, .. } => {
//             assert_eq!(expressions.len(), 1);
//         }
//         _ => unreachable!(),
//     }
//     assert_eq!(hir.functions[0].block.ty, Some(Type::Never));
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         UnreachableCodeDiagnostic::CODE
//     );
// }

// #[test]
// fn should_report_comparison_type_annotation_required() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case("func test(): bool { 5 < 5 }");

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].block.ty, Some(Type::Bool));
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         ComparisonTypeAnnotationRequiredDiagnostic::CODE
//     );
// }

// #[test]
// fn should_report_type_mistmatch_in_local_definition() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case(
//         "func test(): unit {
//                 local x: i32 = 1;
//                 local y: i64 = x;
//             }",
//     );

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].stack.scopes[0].locals.len(), 2);
//     assert_eq!(
//         hir.functions[0].stack.scopes[0].locals[0].ty,
//         Type::Primitive(PrimitiveType::I32)
//     );
//     assert_eq!(
//         hir.functions[0].stack.scopes[0].locals[1].ty,
//         Type::Primitive(PrimitiveType::I64)
//     );
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         TypeMistmatchDiagnostic::CODE
//     );
// }

// #[test]
// fn should_report_type_mistmatch_for_func_argument() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case(
//         "func add(a: i32, b: i32): i32 { a + b }
//             func test(): i32 {
//                 local x: i32 = 5;
//                 local y: i64 = 10;
//                 add(x, y)
//             }",
//     );

//     assert_eq!(hir.functions.len(), 2);
//     assert_eq!(
//         hir.functions[1].block.ty,
//         Some(Type::Primitive(PrimitiveType::I32))
//     );
//     assert_eq!(hir.functions[1].stack.scopes[0].locals.len(), 2);
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         TypeMistmatchDiagnostic::CODE
//     );
// }

// #[test]
// fn should_recover_from_unused_value() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case("func test(): unit { 5; }");

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].block.ty, Some(Type::Unit));
//     match &hir.functions[0].block.kind {
//         ExprKind::Block { expressions, .. } => {
//             assert_eq!(expressions.len(), 0);
//         }
//         _ => unreachable!(),
//     }
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         TypeAnnotationRequiredDiagnostic::CODE
//     );

//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case("func test(): unit { 5 as i32; }");

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].block.ty, Some(Type::Unit));
//     match &hir.functions[0].block.kind {
//         ExprKind::Block { expressions, .. } => {
//             assert_eq!(expressions.len(), 1);
//             match &expressions[0].kind {
//                 ExprKind::Binary { operator, left, .. } => {
//                     assert_eq!(operator.kind, ast::BinOpKind::Assign);
//                     match left.kind {
//                         ExprKind::Placeholder => {}
//                         _ => unreachable!(),
//                     }
//                 }
//                 _ => unreachable!(),
//             }
//         }
//         _ => unreachable!(),
//     }
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         UnusedValueDiagnostic::CODE
//     );
// }

// #[test]
// fn should_recover_when_using_not_with_untyped_literal() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case("func test(): unit { local a = !5 }");

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].block.ty, Some(Type::Unit));
//     assert_eq!(hir.functions[0].stack.scopes[0].locals.len(), 1);
//     assert_eq!(hir.functions[0].stack.scopes[0].locals[0].ty, Type::Bool);
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         UnableToCoerceDiagnostic::CODE
//     );
// }

// #[test]
// fn should_recover_from_non_callable_expression() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case(
//         "func test(): i32 {
//                 local x: i32 = 5;
//                 local y = x();

//                 y
//             }",
//     );

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].block.ty, Some(Type::Unknown));
//     assert_eq!(hir.functions[0].stack.scopes[0].locals.len(), 2);
//     assert_eq!(hir.functions[0].stack.scopes[0].locals[1].ty, Type::Unknown);
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         CannotCallExpressionDiagnostic::CODE
//     );
// }

// #[test]
// fn should_recover_from_operator_cannot_be_applied() {
//     let TestCase {
//         hir_diagnostics,
//         hir,
//         ..
//     } = build_test_case("func test(): unit { local h = 5 + true }");

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].stack.scopes[0].locals.len(), 1);
//     assert_eq!(hir.functions[0].stack.scopes[0].locals[0].ty, Type::Unknown);
//     assert_eq!(hir_diagnostics.len(), 1);
//     assert_eq!(
//         hir_diagnostics[0].code.as_ref().unwrap(),
//         BinaryOperatorCannotBeAppliedDiagnostic::CODE
//     );
// }

// #[test]
// fn should_infert_block_type() {
//     let TestCase { hir, .. } = build_test_case(
//         "func test(): i32 {
//                 a: { break :a 5 }
//             }",
//     );

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].stack.scopes.len(), 2);
//     match &hir.functions[0].block.kind {
//         ExprKind::Block { result, .. } => {
//             assert_eq!(
//                 result.as_ref().unwrap().ty,
//                 Some(Type::Primitive(PrimitiveType::I32))
//             );
//             assert_eq!(
//                 hir.functions[0].stack.scopes[1].inferred_type,
//                 Some(Type::Primitive(PrimitiveType::I32))
//             );
//         }
//         _ => unreachable!(),
//     }

//     let TestCase { hir, .. } = build_test_case(
//         "func test(): i32 {
//                 { 5 }
//             }",
//     );

//     assert_eq!(hir.functions.len(), 1);
//     assert_eq!(hir.functions[0].stack.scopes.len(), 2);
//     match &hir.functions[0].block.kind {
//         ExprKind::Block { result, .. } => {
//             assert_eq!(
//                 result.as_ref().unwrap().ty,
//                 Some(Type::Primitive(PrimitiveType::I32))
//             );
//             assert_eq!(
//                 hir.functions[0].stack.scopes[1].inferred_type,
//                 Some(Type::Primitive(PrimitiveType::I32))
//             );
//         }
//         _ => unreachable!(),
//     }
// }
