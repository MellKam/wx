use std::collections::HashMap;

use crate::vfs::{CrateId, Files};
use crate::{ast::MethodCallExpr, tir::*};

struct ExprContext {
	lookup: HashMap<(ScopeIndex, SymbolU32), LocalIndex>,
	scope_index: ScopeIndex,
	stack: StackFrame,
	resolve_context: ResolveContext,
	scope: Option<GenericScope>,
}

impl ExprContext {
	fn push_local(&mut self, local: Local) -> LocalIndex {
		let name_symbol = local.name.inner;
		let index = self.stack.push_local(self.scope_index, local);
		self.lookup.insert((self.scope_index, name_symbol), index);
		index
	}

	fn resolve_local(
		&self,
		symbol: SymbolU32,
	) -> Option<(ScopeIndex, LocalIndex)> {
		let mut scope_index = self.scope_index;

		loop {
			if let Some(&value) = self.lookup.get(&(scope_index, symbol)) {
				return Some((scope_index, value));
			}

			scope_index = self.stack.scopes[scope_index as usize].parent?;
		}
	}

	fn enter_block<T>(
		&mut self,
		block: BlockScope,
		handler: impl FnOnce(&mut Self) -> T,
	) -> T {
		let parent_scope_index = self.scope_index;
		self.scope_index = self.stack.scopes.len() as u32;
		self.stack.scopes.push(block);

		let result = handler(self);

		self.scope_index = parent_scope_index;
		result
	}

	fn resolve_label(
		&self,
		symbol: SymbolU32,
	) -> Option<(ScopeIndex, LabelIndex)> {
		let mut scope_index = self.scope_index;

		loop {
			let scope = &self.stack.scopes[scope_index as usize];
			match scope.label {
				Some(label_index)
					if self.stack.labels[label_index as usize].name.inner
						== symbol =>
				{
					return Some((scope_index, label_index));
				}
				_ => {}
			}

			scope_index = scope.parent?;
		}
	}

	fn get_closest_loop_block(&self) -> Option<ScopeIndex> {
		let mut scope_index = self.scope_index;

		loop {
			let scope = &self.stack.scopes[scope_index as usize];
			if scope.kind == BlockKind::Loop {
				return Some(scope_index);
			}

			scope_index = scope.parent?
		}
	}
}

pub fn unescape_string(s: &str) -> String {
	// Remove surrounding quotes
	let s = if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
		&s[1..s.len() - 1]
	} else {
		s
	};

	let mut result = String::with_capacity(s.len());
	let mut chars = s.chars();

	while let Some(ch) = chars.next() {
		if ch == '\\' {
			match chars.next() {
				Some('n') => result.push('\n'),
				Some('r') => result.push('\r'),
				Some('t') => result.push('\t'),
				Some('\\') => result.push('\\'),
				Some('"') => result.push('"'),
				Some('0') => result.push('\0'),
				// If we encounter an unknown escape, keep the backslash and the character
				Some(c) => {
					result.push('\\');
					result.push(c);
				}
				None => result.push('\\'),
			}
		} else {
			result.push(ch);
		}
	}

	result
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum CharLiteralError {
	Empty,
	TooLong,
}

pub fn parse_char_literal(s: &str) -> Result<char, CharLiteralError> {
	let content = if s.starts_with('\'') && s.ends_with('\'') && s.len() >= 2 {
		&s[1..s.len() - 1]
	} else {
		s
	};

	let mut chars = content.chars();
	let value = match chars.next() {
		None => return Err(CharLiteralError::Empty),
		Some('\\') => match chars.next() {
			None => return Err(CharLiteralError::Empty),
			Some('n') => '\n',
			Some('r') => '\r',
			Some('t') => '\t',
			Some('\\') => '\\',
			Some('\'') => '\'',
			Some('0') => '\0',
			Some('x') => {
				let hi = chars.next().and_then(|c| c.to_digit(16));
				let lo = chars.next().and_then(|c| c.to_digit(16));
				match (hi, lo) {
					(Some(h), Some(l)) => {
						let codepoint = h * 16 + l;
						char::from_u32(codepoint).unwrap()
					}
					_ => return Err(CharLiteralError::TooLong),
				}
			}
			Some(c) => c,
		},
		Some(c) => c,
	};

	if chars.next().is_some() {
		return Err(CharLiteralError::TooLong);
	}

	Ok(value)
}

struct Builder<'ast, 'graph> {
	interner: &'graph mut ast::StringInterner,
	id_generator: &'graph mut ast::DefIdGenerator,
	files: &'graph Files,
	symbol_lookup: HashMap<(SymbolNamespace, SymbolU32), SymbolKind>,
	type_index_lookup: HashMap<Type, TypeIndex>,
	tir: TIR,
	/// Namespaces brought into the root scope via `use path::*;` at the binary
	/// crate root (where `namespace = None`).  Parallel to `ModuleNamespace::wildcard_imports`.
	root_wildcard_imports: Vec<NamespaceIndex>,
	/// Populated in Phase 1, in parse order. Index matches `sig_state` entries.
	ast_nodes: Vec<AstEntry<'ast>>,
	/// Maps DefId → SigEntry; populated after Phase 1 with exact capacity.
	sig_state: HashMap<ast::DefId, SigEntry>,
}

enum BlockState {
	Exhaustive(Box<[Expression]>),
	Incomplete(Box<[Expression]>),
}

#[derive(Clone, Copy, PartialEq)]
enum ComputeState {
	Pending,
	InProgress,
	Done,
}

/// Outcome of [`Builder::claim_name_binding`].
#[derive(Clone, Copy, PartialEq)]
enum PendingClaim {
	/// No prior definition existed in this scope; `Pending(id)` was
	/// installed and this item owns the name.
	Claimed,
	/// The scope already had a binding; a duplicate-definition diagnostic
	/// was pushed against it and nothing was installed for this item.
	Duplicate,
}

enum BoundKind {
	Trait(TraitBound),
	TypeSet(TypesetIndex),
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
struct AstEntry<'ast> {
	def_id: ast::DefId,
	file_id: FileId,
	namespace: Option<NamespaceIndex>,
	node: AstNodeRef<'ast>,
}

#[derive(Clone, Copy)]
struct SigEntry {
	node_idx: usize,
	state: ComputeState,
}

#[derive(Clone, Copy)]
struct GenericScope {
	owner: TypeParamOwner,
	self_type: Option<TypeIndex>,
}

#[derive(Clone, Copy)]
struct ResolveContext {
	file_id: FileId,
	namespace: Option<NamespaceIndex>,
}

impl ResolveContext {
	fn new(file_id: FileId, namespace: Option<NamespaceIndex>) -> Self {
		Self { file_id, namespace }
	}
}

/// AST node reference for demand-driven resolution.
/// Resolution context (`file_id`, `namespace`) lives in `AstEntry`.
#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
enum AstNodeRef<'ast> {
	/// Top-level `fn` or `#[intrinsic]` declaration.
	Function {
		item: &'ast ast::Item,
	},
	ImplMethod {
		impl_target: &'ast ast::Spanned<ast::TypeExpression>,
		item: &'ast ast::ImplItem,
	},
	/// Resolves the impl block's type-param bounds and target type.
	/// Must run before any `GenericImplMethod` in the same block.
	GenericImplBlock {
		impl_type_params: &'ast [ast::TypeParam],
		impl_target: &'ast ast::Spanned<ast::TypeExpression>,
		block_index: u32,
	},
	GenericImplMethod {
		/// Synthetic DefId of the `GenericImplBlock` entry; used to
		/// demand-drive block initialization before processing this method.
		block_id: ast::DefId,
		item: &'ast ast::ImplItem,
		block_index: u32,
	},
	/// `trait` function with or without a default body.
	TraitFunction {
		trait_index: u32,
		item: &'ast ast::TraitItem,
	},
	TraitConst {
		trait_index: u32,
		item: &'ast ast::TraitItem,
	},
	TraitAssociatedType {
		trait_index: u32,
		item: &'ast ast::TraitItem,
	},
	Struct {
		item: &'ast ast::Item,
	},
	Enum {
		item: &'ast ast::Item,
	},
	Global {
		item: &'ast ast::Item,
	},
	Memory {
		item: &'ast ast::Item,
	},
	Const {
		item: &'ast ast::Item,
	},
	ImplConst {
		impl_target: &'ast ast::Spanned<ast::TypeExpression>,
		item: &'ast ast::ImplItem,
	},
	/// Creates the `TraitImpl` entry when resolved.
	ImplTraitBlock {
		item: &'ast ast::Item,
	},
	ImplTraitMethod {
		/// Parent `ImplTraitBlock` must be resolved before this can insert into
		/// `TraitImpl.members`.
		parent_id: ast::DefId,
		item: &'ast ast::ImplItem,
	},
	ImplTraitConst {
		parent_id: ast::DefId,
		item: &'ast ast::ImplItem,
	},
	ImplTraitAssociatedType {
		parent_id: ast::DefId,
		item: &'ast ast::ImplItem,
	},
	Trait {
		trait_index: TraitIndex,
		item: &'ast ast::Item,
	},
	TypeSet {
		typeset_index: TypesetIndex,
		item: &'ast ast::Item,
	},
	ImportedFunction {
		import_module_index: u32,
		decl: &'ast ast::ImportDeclaration,
	},
	ImportedGlobal {
		import_module_index: u32,
		decl: &'ast ast::ImportDeclaration,
	},
	TypeAlias {
		item: &'ast ast::Item,
	},
}

fn report_missing_enum_repr(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::MissingEnumRepr.code())
		.with_message("enum requires a repr type")
		.with_label(span.primary_label().with_message("add `: <type>` here"))
}

fn report_enum_repr_not_integer(
	fmt: TypeFormatter,
	ty: TypeIndex,
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::EnumReprNotInteger.code())
		.with_message("enum repr type must be an integer type")
		.with_label(span.primary_label().with_message(format!(
			"`{}` is not an integer type",
			fmt.display_type(ty).unwrap_or_default()
		)))
}

/// One diagnostic per colliding value, not pairwise: primary label on the enum's own
/// name, one secondary label per variant that shares `value` (all of them, not just
/// the 2nd/3rd onward) — mirrors rustc's grouped `E0081` presentation.
fn report_enum_duplicate_value(
	enum_name_span: SourceSpan,
	value: i64,
	variant_spans: &[SourceSpan],
) -> Diagnostic<FileId> {
	let mut diagnostic = Diagnostic::error()
		.with_code(DiagnosticCode::EnumDuplicateValue.code())
		.with_message(format!(
			"multiple variants of this enum have the same value `{value}`"
		))
		.with_label(enum_name_span.primary_label());
	for span in variant_spans {
		diagnostic = diagnostic.with_label(
			span.secondary_label()
				.with_message(format!("value `{value}` assigned here")),
		);
	}
	diagnostic
}

fn report_unused_enum_variants(
	interner: &ast::StringInterner,
	file_id: FileId,
	unused_variants: &[Spanned<SymbolU32>],
) -> Diagnostic<FileId> {
	let message = match unused_variants.len() {
		1 => {
			let name = interner.resolve(unused_variants[0].inner).unwrap();
			format!("variant `{name}` is never constructed")
		}
		2 => {
			let a = interner.resolve(unused_variants[0].inner).unwrap();
			let b = interner.resolve(unused_variants[1].inner).unwrap();
			format!("variants `{a}` and `{b}` are never constructed")
		}
		3..=5 => {
			let (last, rest) = unused_variants.split_last().unwrap();
			let rest = rest
				.iter()
				.map(|name| {
					format!("`{}`", interner.resolve(name.inner).unwrap())
				})
				.collect::<Vec<_>>()
				.join(", ");
			let last = interner.resolve(last.inner).unwrap();
			format!("variants {rest}, and `{last}` are never constructed")
		}
		_ => "multiple variants are never constructed".to_string(),
	};
	let mut diagnostic = Diagnostic::warning()
		.with_code(DiagnosticCode::UnusedEnumVariant.code())
		.with_message(message);
	for name in unused_variants {
		diagnostic = diagnostic
			.with_label(SourceSpan::new(file_id, name.span).secondary_label());
	}
	diagnostic
}

/// For const initializers and enum variant values (never `mut`-able) that build
/// successfully but don't fold — distinct from `report_non_constant_global_initializer`,
/// whose "add `mut`" suggestion only makes sense for globals.
fn report_not_const_evaluatable(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::NotConstEvaluatable.code())
		.with_message(
			"expression cannot be evaluated as a compile-time constant",
		)
		.with_label(span.primary_label())
}

fn report_missing_function_body(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::MissingFunctionBody.code())
		.with_message("free function without a body")
		.with_label(span.primary_label())
		.with_note("provide a definition for the function: `{ <body> }`")
}

fn report_invalid_memory_kind(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::InvalidMemoryKind.code())
		.with_message("invalid memory kind")
		.with_label(span.primary_label())
		.with_note(
			"expected `Memory where { Size = u32 }` or `Memory where { Size = u64 }`",
		)
}

struct DuplicateDefinitionDiagnostic<'a> {
	name: &'a str,
	namespace: SymbolNamespace,
	first_definition: SourceSpan,
	second_definition: SourceSpan,
}

fn report_duplicate_definition(
	diagnostic: DuplicateDefinitionDiagnostic<'_>,
) -> Diagnostic<FileId> {
	let namespace = match diagnostic.namespace {
		SymbolNamespace::Type => "type",
		SymbolNamespace::Value => "value",
	};
	Diagnostic::error()
		.with_code(DiagnosticCode::DuplicateDefinition.code())
		.with_message(format!(
			"the name `{}` is defined multiple times",
			diagnostic.name
		))
		.with_label(diagnostic.second_definition.primary_label())
		.with_label(diagnostic.first_definition.primary_label().with_message(
			format!(
				"previous definition of the {} `{}` here",
				diagnostic.name, namespace
			),
		))
}

fn report_duplicate_parameter(
	name: &str,
	first_definition: SourceSpan,
	second_definition: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::DuplicateDefinition.code())
		.with_message(format!(
			"identifier `{}` is bound more than once in this parameter list",
			name
		))
		.with_label(second_definition.primary_label())
		.with_label(
			first_definition
				.secondary_label()
				.with_message(format!("first use of `{}` as parameter", name)),
		)
}

fn report_non_constant_global_initializer(
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::NonConstantGlobalInitializer.code())
		.with_message(
			"immutable global initializer must be an integer or float literal",
		)
		.with_label(
			span.primary_label()
				.with_message("add `mut` to use a computed initializer"),
		)
}

fn report_empty_char_literal(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::InvalidLiteral.code())
		.with_message("empty character literal")
		.with_label(span.primary_label())
}

fn report_char_literal_too_long(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::InvalidLiteral.code())
		.with_message("character literal may only contain one codepoint")
		.with_label(span.primary_label())
		.with_note(
			"if you meant to write a string literal, use double quotes: `\"`, `\"`",
		)
}

struct TypeMistmatchDiagnostic {
	expected_type: TypeIndex,
	actual_type: TypeIndex,
	span: SourceSpan,
}

fn report_missing_else_block(
	fmt: TypeFormatter,
	then_ty: TypeIndex,
	then_span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::MissingElseBlock.code())
		.with_message("`if` may be missing an `else` clause")
		.with_label(then_span.primary_label().with_message(format!(
			"expected `()`, found `{}`",
			fmt.display_type(then_ty).unwrap()
		)))
		.with_note("`if` expressions without `else` evaluate to `()`")
		.with_note(
			"consider adding an `else` block that evaluates to the expected type",
		)
}

fn report_type_mistmatch(
	fmt: TypeFormatter,
	diagnostic: TypeMistmatchDiagnostic,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::TypeMistmatch.code())
		.with_message("type mismatch")
		.with_label(diagnostic.span.primary_label().with_message(format!(
			"expected `{}`, found `{}`",
			fmt.display_type(diagnostic.expected_type).unwrap(),
			fmt.display_type(diagnostic.actual_type).unwrap()
		)))
}

fn report_type_annotation_required(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::TypeAnnotationRequired.code())
		.with_message("type annotation required")
		.with_label(span.primary_label())
}

fn report_unused_variable(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::warning()
		.with_code(DiagnosticCode::UnusedVariable.code())
		.with_message("unused variable")
		.with_label(span.primary_label())
}

fn report_unnecessary_mutability(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::warning()
		.with_code(DiagnosticCode::UnnecessaryMutability.code())
		.with_message("unnecessary mutability")
		.with_label(span.primary_label())
}

fn report_unreachable_code(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::warning()
		.with_code(DiagnosticCode::UnreachableCode.code())
		.with_message("unreachable code")
		.with_label(
			span.primary_label()
				.with_message("this code will never be executed"),
		)
}

fn report_unused_value(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::UnusedValue.code())
		.with_message("value must be used")
		.with_label(span.primary_label().with_message("value never used"))
		.with_note(
			"if you don't need the value, consider dropping it with assignment to `_`",
		)
}

struct IntegerLiteralOutOfRangeDiagnostic {
	ty: TypeIndex,
	value: i64,
	span: SourceSpan,
}

fn report_integer_literal_out_of_range(
	fmt: TypeFormatter,
	diagnostic: IntegerLiteralOutOfRangeDiagnostic,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::IntegerLiteralOutOfRange.code())
		.with_message(format!(
			"literal `{}` out of range for `{}`",
			diagnostic.value,
			fmt.display_type(diagnostic.ty).unwrap()
		))
		.with_label(diagnostic.span.primary_label())
}

fn report_unable_to_coerce(
	fmt: TypeFormatter,
	target_type: TypeIndex,
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::UnableToCoerce.code())
		.with_message(format!(
			"unable to coerce to type `{}`",
			fmt.display_type(target_type).unwrap()
		))
		.with_label(span.primary_label())
}

fn report_integer_literal_out_of_typeset_range(
	value: i64,
	typeset_name: &str,
	range: &IntegerRange,
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
        .with_code(DiagnosticCode::TypesetBoundViolation.code())
        .with_message(format!(
            "integer literal `{value}` is out of the safe range for typeset `{typeset_name}`"
        ))
        .with_label(span.primary_label().with_message(format!(
            "safe range for `{typeset_name}` is `{}..={}`",
            range.min_i64(),
            range.max_u64(),
        )))
}

fn report_integer_literal_for_float_type(
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::LiteralTypeMismatch.code())
		.with_message("cannot use an integer literal for a float type")
		.with_label(span.primary_label())
		.with_note("consider adding a decimal point, e.g. `1.0` instead of `1`")
}

fn report_invalid_self_type(
	span: SourceSpan,
	formatter: TypeFormatter<'_>,
	ty: TypeIndex,
) -> Diagnostic<FileId> {
	let type_name = formatter.display_type(ty).unwrap();
	Diagnostic::error()
		.with_code(DiagnosticCode::InvalidSelfType.code())
		.with_message(format!("invalid `self` parameter type: `{type_name}`"))
		.with_label(span.primary_label().with_message(
			"type of `self` must be `Self` or a type that dereferences to it",
		))
		.with_note("consider changing to `Self` or `*Self`")
}

fn report_method_not_found(
	span: SourceSpan,
	formatter: TypeFormatter<'_>,
	method: SymbolU32,
	ty: TypeIndex,
) -> Diagnostic<FileId> {
	let method_name = formatter.interner.resolve(method).unwrap_or("?");
	let type_name = formatter.display_type(ty).unwrap();
	Diagnostic::error()
		.with_code(DiagnosticCode::MethodNotFound.code())
		.with_message(format!(
			"no method `{method_name}` found for type `{type_name}`"
		))
		.with_label(span.primary_label())
}

fn report_not_a_method(
	span: SourceSpan,
	formatter: TypeFormatter<'_>,
	method: SymbolU32,
	ty: TypeIndex,
) -> Diagnostic<FileId> {
	let member_name = formatter.interner.resolve(method).unwrap_or("?");
	let type_name = formatter.display_type(ty).unwrap();
	Diagnostic::error()
		.with_code(DiagnosticCode::NotAMethod.code())
		.with_message(format!(
			"`{member_name}` is not a method on type `{type_name}`"
		))
		.with_label(span.primary_label())
		.with_note("use `::` to access associated items")
}

fn report_undeclared_identifier(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::UndeclaredIdentifier.code())
		.with_message("undeclared identifier")
		.with_label(span.primary_label())
}

fn report_namespace_used_as_value(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::NamespaceUsedAsValue.code())
		.with_message("expected a value, found a namespace")
		.with_label(span.primary_label())
		.with_note("use `::` to access members of this namespace")
}

fn report_infer_in_signature(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::InferInSignature.code())
		.with_message("`_` is not allowed in item type signatures")
		.with_label(
			span.primary_label()
				.with_message("type must be specified explicitly"),
		)
}

fn report_undeclared_type(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::UndeclaredType.code())
		.with_message("undeclared type")
		.with_label(span.primary_label())
}

fn report_bare_assoc_type(span: SourceSpan, name: &str) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::UndeclaredType.code())
		.with_message(format!("cannot find type `{name}` in this scope"))
		.with_label(span.primary_label())
		.with_note(format!(
			"you might have meant to use the associated type: `Self::{name}`"
		))
}

struct BinaryOperatorCannotBeAppliedDiagnostic {
	file_id: FileId,
	operator: Spanned<ast::BinaryOp>,
	operand: Spanned<TypeIndex>,
}

fn report_binary_operator_cannot_be_applied(
	fmt: TypeFormatter,
	diagnostic: BinaryOperatorCannotBeAppliedDiagnostic,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::BinaryOperatorCannotBeApplied.code())
		.with_message(format!(
			"operator `{}` cannot be applied to type `{}`",
			diagnostic.operator.inner,
			fmt.display_type(diagnostic.operand.inner).unwrap()
		))
		.with_label(Label::primary(diagnostic.file_id, diagnostic.operand.span))
		.with_label(Label::secondary(
			diagnostic.file_id,
			diagnostic.operator.span,
		))
}

struct BinaryExpressionMistmatchDiagnostic {
	file_id: FileId,
	left_type: Spanned<TypeIndex>,
	operator: Spanned<ast::BinaryOp>,
	right_type: Spanned<TypeIndex>,
}

fn report_binary_expression_mistmatch(
	fmt: TypeFormatter,
	diagnostic: BinaryExpressionMistmatchDiagnostic,
) -> Diagnostic<FileId> {
	let left_type_name = fmt.display_type(diagnostic.left_type.inner).unwrap();
	let right_type_name =
		fmt.display_type(diagnostic.right_type.inner).unwrap();

	let message = match diagnostic.operator.inner {
		ast::BinaryOp::Add => {
			format!("cannot add `{}` to `{}`", left_type_name, right_type_name)
		}
		ast::BinaryOp::Sub => format!(
			"cannot subtract `{}` from `{}`",
			left_type_name, right_type_name
		),
		ast::BinaryOp::Assign => format!(
			"cannot assign `{}` to `{}`",
			right_type_name, left_type_name
		),
		ast::BinaryOp::Mul => format!(
			"cannot multiply `{}` by `{}`",
			left_type_name, right_type_name
		),
		ast::BinaryOp::Div => format!(
			"cannot divide `{}` by `{}`",
			left_type_name, right_type_name
		),
		ast::BinaryOp::Rem => format!(
			"cannot calculate the remainder of `{}` by `{}`",
			left_type_name, right_type_name
		),
		ast::BinaryOp::Eq
		| ast::BinaryOp::NotEq
		| ast::BinaryOp::Less
		| ast::BinaryOp::LessEq
		| ast::BinaryOp::Greater
		| ast::BinaryOp::GreaterEq => {
			format!(
				"cannot compare `{}` to `{}`",
				left_type_name, right_type_name
			)
		}
		ast::BinaryOp::MulAssign => {
			format!(
				"cannot multiply-assign `{}` to `{}`",
				right_type_name, left_type_name
			)
		}
		ast::BinaryOp::DivAssign => {
			format!(
				"cannot divide-assign `{}` by `{}`",
				right_type_name, left_type_name
			)
		}
		ast::BinaryOp::RemAssign => format!(
			"cannot remainder-assign `{}` by `{}`",
			right_type_name, left_type_name
		),
		ast::BinaryOp::AddAssign => {
			format!(
				"cannot add-assign `{}` to `{}`",
				right_type_name, left_type_name
			)
		}
		ast::BinaryOp::SubAssign => format!(
			"cannot subtract-assign `{}` from `{}`",
			right_type_name, left_type_name
		),
		_ => format!(
			"cannot perform operation on `{}` and `{}`",
			left_type_name, right_type_name
		),
	};

	Diagnostic::error()
		.with_message(message)
		.with_label(
			Label::secondary(diagnostic.file_id, diagnostic.left_type.span)
				.with_message(format!("`{}`", left_type_name)),
		)
		.with_label(
			Label::primary(diagnostic.file_id, diagnostic.right_type.span)
				.with_message(format!("`{}`", right_type_name)),
		)
}

fn report_undeclared_label(
	label_name: &str,
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::UndeclaredLabel.code())
		.with_message(format!("use of undeclared label `{}`", label_name))
		.with_label(span.primary_label().with_message("undeclared label"))
}

fn report_break_outside_of_loop(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::BreakOutsideOfLoop.code())
		.with_message("`break` outside of a loop or labeled block")
		.with_label(span.primary_label())
		.with_note("cannot `break` outside of a loop or labeled block")
}

fn report_cannot_mutate_immutable(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::CannotMutateImmutable.code())
		.with_message("cannot mutate immutable binding")
		.with_label(span.primary_label())
}

fn report_cannot_store_through_immutable_pointer(
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::CannotMutateImmutable.code())
		.with_message("cannot write through an immutable pointer")
		.with_label(span.primary_label())
		.with_note("consider changing the pointer type to `*mut T`")
}

fn report_cannot_deref_non_pointer(
	span: SourceSpan,
	ty_display: String,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::CannotDerefNonPointer.code())
		.with_message("dereference of non-pointer type")
		.with_label(
			span.primary_label().with_message(format!(
				"type `{}` is not a pointer",
				ty_display
			)),
		)
}

fn report_cannot_take_address_of_value(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
        .with_code(DiagnosticCode::InvalidAssignmentTarget.code())
        .with_message("cannot take address of a temporary or stack value")
        .with_label(
            span.primary_label()
                .with_message("this expression is a value, not a location in memory"),
        )
        .with_note(
            "`.&` is only valid on places reachable through a pointer, e.g. `ptr.*` or `ptr.*.field`",
        )
}

fn report_cannot_take_mutable_address_of_immutable(
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
        .with_code(DiagnosticCode::CannotMutateImmutable.code())
        .with_message("cannot take a mutable address of an immutable place")
        .with_label(span.primary_label())
        .with_note(
            "the pointer chain leading here is immutable; use `*mut T` to allow mutable access",
        )
}

fn report_no_memory_for_pointer(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
        .with_code(DiagnosticCode::NoMemoryForPointer.code())
        .with_message("pointer dereference requires a linear memory")
        .with_label(span.primary_label())
        .with_note("declare a memory in this module: `memory <name>: Memory where { Size = u32 };`")
}

fn report_ambiguous_pointer_memory(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::AmbiguousPointerMemory.code())
		.with_message(
			"pointer dereference is ambiguous: multiple memories defined",
		)
		.with_label(span.primary_label())
		.with_note("specify which memory with `<memory_name>::*T` syntax")
}

fn report_index_on_non_indexable(
	span: SourceSpan,
	type_name: String,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::IndexOnNonIndexable.code())
		.with_message(format!(
			"cannot index into a value of type `{type_name}`"
		))
		.with_label(span.primary_label())
		.with_note(
			"indexing is only supported on array `[N]T` and slice `[]T` types",
		)
}

fn report_array_size_mismatch(
	span: SourceSpan,
	expected: u32,
	actual: usize,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::ArraySizeMismatch.code())
		.with_message(format!(
			"array literal has {actual} element(s) but the type expects {expected}"
		))
		.with_label(span.primary_label())
}

fn report_array_repeat_count_not_const(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::ArrayRepeatCountNotConst.code())
		.with_message(
			"array repeat count must be a compile-time integer constant",
		)
		.with_label(span.primary_label())
}

fn report_array_element_not_const(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::ArrayElementNotConst.code())
		.with_message("array literal elements must be compile-time constants")
		.with_label(span.primary_label())
		.with_note(
			"only integer and float literals are allowed in array literals",
		)
}

fn report_invalid_assignment_target(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::InvalidAssignmentTarget.code())
		.with_message("invalid assignment target")
		.with_label(
			span.primary_label()
				.with_message("cannot assign to this expression"),
		)
		.with_note("assignment only allowed to a variable or `_`")
}

fn report_duplicate_export(
	name: &str,
	first_export: SourceSpan,
	second_export: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::DuplicateExport.code())
		.with_message(format!("the name `{}` is exported multiple times", name))
		.with_label(second_export.primary_label())
		.with_label(
			first_export
				.secondary_label()
				.with_message(format!("previous export of `{}` here", name)),
		)
		.with_note(format!(
			"`{}` can only be exported once from this module",
			name
		))
}

fn report_comparison_type_annotation_required(
	left: SourceSpan,
	right: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::ComparisonTypeAnnotationRequired.code())
		.with_message("type annotation required")
		.with_label(left.primary_label())
		.with_label(right.primary_label())
		.with_note("at least one side of the comparison must have a known type")
}

fn report_cannot_export_item(
	name: &str,
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::CannotExportItem.code())
		.with_message(format!("cannot export `{}`", name))
		.with_label(span.primary_label())
		.with_note(
			"only functions, global variables, and memories can be exported",
		)
}

fn report_cannot_export_generic_function(
	name: &str,
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::CannotExportItem.code())
		.with_message(format!("cannot export generic function `{}`", name))
		.with_label(span.primary_label())
		.with_note(
			"exported functions must be non-generic; call the generic function from a concrete wrapper instead",
		)
}

fn report_cyclic_type_dependency(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
        .with_code(DiagnosticCode::CyclicTypeDependency.code())
        .with_message("cyclic type dependency")
        .with_label(span.primary_label())
        .with_note("types cannot have infinite size; consider using a pointer to break the cycle")
}

fn report_recursive_type(
	name: &str,
	struct_span: SourceSpan,
	field_span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::CyclicTypeDependency.code())
		.with_message(format!("recursive type `{name}` has infinite size"))
		.with_label(struct_span.primary_label())
		.with_label(
			field_span
				.secondary_label()
				.with_message("recursive without indirection"),
		)
		.with_note(
			"insert some indirection (e.g. a pointer) to break the cycle",
		)
}

struct ArgumentCountMismatchDiagnostic<'a> {
	actual_count: usize,
	params: &'a [TypeIndex],
	call_span: SourceSpan,
	is_method: bool,
}

fn report_argument_count_mismatch(
	fmt: TypeFormatter,
	details: ArgumentCountMismatchDiagnostic<'_>,
) -> Diagnostic<FileId> {
	let mut diagnostic = Diagnostic::error()
		.with_code(DiagnosticCode::ArgumentCountMismatch.code())
		.with_message(format!(
			"this {} takes {} {} but {} {} supplied",
			if details.is_method {
				"method"
			} else {
				"function"
			},
			details.params.len(),
			if details.params.len() == 1 {
				"argument"
			} else {
				"arguments"
			},
			details.actual_count,
			if details.actual_count == 1 {
				"argument was"
			} else {
				"arguments were"
			},
		))
		.with_label(details.call_span.primary_label());

	if details.actual_count < details.params.len() {
		let missing_count = details.params.len() - details.actual_count;
		let missing_types: Vec<String> = details.params[details.actual_count..]
			.iter()
			.map(|ty| fmt.display_type(*ty).unwrap())
			.collect();

		if missing_count == 1 {
			diagnostic = diagnostic.with_note(format!(
				"argument #{} of type `{}` is missing",
				details.actual_count + 1,
				missing_types[0]
			));
		} else {
			let types_str = missing_types.join("`, `");
			diagnostic = diagnostic.with_note(format!(
				"{} arguments of type `{}` are missing",
				missing_count, types_str
			));
		}
	} else {
		let extra_count = details.actual_count - details.params.len();
		if extra_count == 1 {
			diagnostic = diagnostic.with_note(format!(
				"unexpected argument #{}",
				details.actual_count
			));
		} else {
			diagnostic = diagnostic
				.with_note(format!("{} unexpected arguments", extra_count));
		}
	}

	diagnostic
}

fn report_missing_import_alias(span: SourceSpan) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::MissingImportAlias.code())
		.with_message("import requires an `as` alias")
		.with_label(
			span.primary_label()
				.with_message("expected `as <name>` here"),
		)
}

fn report_duplicate_struct_field(
	name: &str,
	first_span: SourceSpan,
	second_span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::DuplicateStructField.code())
		.with_message(format!("field `{}` is already declared", name))
		.with_label(second_span.primary_label())
		.with_label(
			first_span
				.secondary_label()
				.with_message(format!("`{}` first declared here", name)),
		)
}

fn report_not_a_struct_type(
	file_id: FileId,
	name: String,
	span: ast::TextSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::TypeMistmatch.code())
		.with_message(format!("expected struct, found `{}`", name))
		.with_label(Label::primary(file_id, span))
}

struct UnknownStructFieldDiagnostic<'a> {
	file_id: FileId,
	struct_name: &'a str,
	field_name: &'a str,
	field_span: ast::TextSpan,
}

fn report_unknown_struct_field(
	details: UnknownStructFieldDiagnostic<'_>,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::UnknownStructField.code())
		.with_message(format!(
			"no such field `{}` in struct `{}`",
			details.field_name, details.struct_name
		))
		.with_label(Label::primary(details.file_id, details.field_span))
}

fn report_duplicate_struct_field_init(
	field_name: &str,
	first_span: SourceSpan,
	second_span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::DuplicateStructFieldInit.code())
		.with_message(format!(
			"field `{}` specified more than once",
			field_name
		))
		.with_label(second_span.primary_label())
		.with_label(
			first_span
				.secondary_label()
				.with_message("first use of this field"),
		)
}

struct MissingStructFieldsDiagnostic<'a> {
	file_id: FileId,
	struct_name: &'a str,
	missing_fields: Box<[&'a str]>,
	init_span: ast::TextSpan,
}

fn report_missing_struct_fields(
	details: MissingStructFieldsDiagnostic<'_>,
) -> Diagnostic<FileId> {
	let fields_str = details
		.missing_fields
		.iter()
		.map(|field| format!("`{}`", field))
		.collect::<Vec<_>>()
		.join(", ");
	Diagnostic::error()
		.with_code(DiagnosticCode::MissingStructFields.code())
		.with_message(format!(
			"missing fields {} in initializer of `{}`",
			fields_str, details.struct_name
		))
		.with_label(Label::primary(details.file_id, details.init_span))
}

fn report_associated_type_in_inherent_impl(
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::AssociatedTypeInInherentImpl.code())
		.with_message(
			"associated types are not allowed in inherent impl blocks",
		)
		.with_label(span.primary_label())
		.with_note(
			"associated types can only be defined in `impl Trait for Type` blocks",
		)
}

fn report_invalid_cast(
	fmt: TypeFormatter,
	from_type: TypeIndex,
	to_type: TypeIndex,
	span: SourceSpan,
) -> Diagnostic<FileId> {
	Diagnostic::error()
		.with_code(DiagnosticCode::InvalidCast.code())
		.with_message(format!(
			"cannot cast `{}` to `{}`",
			fmt.display_type(from_type).unwrap(),
			fmt.display_type(to_type).unwrap(),
		))
		.with_label(span.primary_label())
}

pub fn build(graph: &mut CompilationGraph) -> TIR {
	let source_modules: Vec<_> = graph
		.crates
		.iter()
		.flat_map(|crate_graph| {
			crate_graph
				.modules
				.iter()
				.map(move |module| (crate_graph, module))
		})
		.collect();
	assert!(
		!source_modules.is_empty(),
		"TIR::build requires at least one AST"
	);

	let tir = TIR {
		diagnostics: Vec::new(),
		types: vec![
			// Order MUST match the IDX constants defined at the top of this file.
			Type::Error,
			Type::Infer,
			Type::Unit,
			Type::Never,
			Type::Integer,
			Type::Float,
			Type::U8,
			Type::I8,
			Type::U16,
			Type::I16,
			Type::U32,
			Type::I32,
			Type::U64,
			Type::I64,
			Type::F32,
			Type::F64,
			Type::Bool,
			Type::Char,
		],
		functions: Vec::new(),
		globals: Vec::new(),
		exports: HashMap::new(),
		namespaces: Vec::new(),
		root_symbols: HashMap::new(),
		root_wildcard_imports: Vec::new(),
		module_decls: Vec::new(),
		import_decls: Vec::new(),
		enums: Vec::new(),
		impl_members: HashMap::new(),
		generic_impl_list: Vec::new(),
		generic_impl_dispatch: HashMap::new(),
		structs: Vec::new(),
		memories: Vec::new(),
		traits: Vec::new(),
		trait_impls: Vec::new(),
		trait_impl_lookup: HashMap::new(),
		type_trait_impls: HashMap::new(),
		constants: Vec::new(),
		tagged_items: HashMap::new(),
		typesets: Vec::new(),
		type_aliases: Vec::new(),
		item_lookup: HashMap::new(),
	};
	let type_index_lookup = HashMap::from_iter(
		tir.types
			.iter()
			.enumerate()
			.map(|(idx, ty)| (ty.clone(), TypeIndex(idx as u32))),
	);
	let symbol_lookup = HashMap::new();
	let mut builder = Builder {
		symbol_lookup,
		interner: &mut graph.interner,
		id_generator: &mut graph.id_generator,
		files: &graph.files,
		tir,
		type_index_lookup,
		sig_state: HashMap::new(),
		root_wildcard_imports: Vec::new(),
		ast_nodes: Vec::new(),
	};

	// Create a top-level namespace for each named (library) crate before prescanning.
	// This lets modules inside stdlib live under `std::` rather than the root namespace.
	let mut crate_namespaces: HashMap<CrateId, NamespaceIndex> = HashMap::new();
	for crate_graph in &graph.crates {
		if let Some(crate_name) = crate_graph.name {
			let namespace_idx = builder.tir.namespaces.len() as NamespaceIndex;
			builder.tir.namespaces.push(ModuleNamespace {
				name: crate_name,
				parent: None,
				declaration: ModuleDeclarationKind::Crate(
					crate_graph.id,
					crate_graph.modules[crate_graph.root.as_usize()].file_id,
				),
				symbols: HashMap::new(),
				wildcard_imports: Vec::new(),
				accesses: Vec::new(),
			});
			builder.symbol_lookup.insert(
				(SymbolNamespace::Type, crate_name),
				SymbolKind::Module { namespace_idx },
			);
			crate_namespaces.insert(crate_graph.id, namespace_idx);
		}
	}

	// Phase 1: register all top-level items into ast_nodes / pending
	for (crate_graph, source_module) in source_modules.iter().copied() {
		let crate_base = crate_namespaces.get(&crate_graph.id).copied();
		let module_path = crate_graph.module_symbol_path(source_module.id);
		let namespace = builder.ensure_module_path(
			source_module.file_id,
			crate_base,
			&module_path,
		);
		for item in source_module.ast.items.iter() {
			builder.pre_scan_item(
				source_module.file_id,
				namespace,
				&item.inner.inner,
			);
		}
	}

	// Build sig_state from ast_nodes with exact capacity; all start as Pending.
	builder.sig_state = HashMap::with_capacity(builder.ast_nodes.len());
	for (node_idx, entry) in builder.ast_nodes.iter().enumerate() {
		builder.sig_state.insert(
			entry.def_id,
			SigEntry {
				node_idx,
				state: ComputeState::Pending,
			},
		);
	}

	// Phase 2: demand-resolve signatures in parse order (vec is already ordered).
	for i in 0..builder.ast_nodes.len() {
		builder.ensure_signature(builder.ast_nodes[i].def_id);
	}

	// Phase 3: demand-resolve bodies in parse order.
	for i in 0..builder.ast_nodes.len() {
		builder.ensure_body(builder.ast_nodes[i].def_id);
	}

	// Phase 3.5: verify every trait impl provides all required items
	builder.check_trait_conformance();

	// Phase 4: process exports (must run after all signatures are resolved)
	for (_, source_module) in source_modules.iter().copied() {
		for item in source_module.ast.items.iter() {
			if let ast::Item::Export { entries } = &item.inner.inner {
				builder.build_exports(source_module.file_id, entries);
			}
		}
	}

	builder.report_unused_items();

	builder.tir.root_symbols = builder.symbol_lookup;
	builder.tir.root_wildcard_imports = builder.root_wildcard_imports;

	builder.tir
}

impl<'ast> Builder<'ast, '_> {
	fn is_valid_self_type(&self, ty: TypeIndex, self_type: TypeIndex) -> bool {
		if ty == self_type {
			return true;
		}
		match &self.tir.types[ty.as_usize()] {
			Type::Pointer { to, .. } => *to == self_type,
			_ => false,
		}
	}

	fn intern_type(&mut self, ty: Type) -> TypeIndex {
		if let Some(&idx) = self.type_index_lookup.get(&ty) {
			idx
		} else {
			let idx = TypeIndex(self.tir.types.len() as u32);
			self.tir.types.push(ty.clone());
			self.type_index_lookup.insert(ty, idx);
			idx
		}
	}

	pub fn coercible_to(&mut self, a: TypeIndex, b: TypeIndex) -> bool {
		if a == b
			|| a == TypeIndex::NEVER
			|| a == TypeIndex::ERROR
			|| b == TypeIndex::ERROR
		{
			return true;
		}
		match (&self.tir.types[a.as_usize()], &self.tir.types[b.as_usize()]) {
			// *mut T coerces to *T (dropping write permission is always safe).
			(
				Type::Pointer {
					to: a_to,
					memory: a_mem,
					mutable: true,
				},
				Type::Pointer {
					to: b_to,
					memory: b_mem,
					mutable: false,
				},
			) => a_to == b_to && a_mem == b_mem,
			// []mut T coerces to []T (dropping write permission is always safe).
			(
				Type::Slice {
					of: a_of,
					memory: a_mem,
					mutable: true,
				},
				Type::Slice {
					of: b_of,
					memory: b_mem,
					mutable: false,
				},
			) => a_of == b_of && a_mem == b_mem,
			// [N]mut T coerces to [N]T (dropping write permission is always safe).
			(
				Type::Array {
					of: a_of,
					size: a_size,
					memory: a_mem,
					mutable: true,
				},
				Type::Array {
					of: b_of,
					size: b_size,
					memory: b_mem,
					mutable: false,
				},
			) => a_of == b_of && a_size == b_size && a_mem == b_mem,
			// FunctionItem coerces implicitly to its matching Function type.
			(Type::FunctionItem { id, type_args }, Type::Function { .. }) => {
				let func_index = self.tir.expect_function_index(*id) as usize;
				let generic_sig =
					self.tir.functions[func_index].signature_index;
				self.substitute_type(generic_sig, &type_args.clone()) == b
			}
			_ => false,
		}
	}

	fn unify(&mut self, a: TypeIndex, b: TypeIndex) -> Result<TypeIndex, ()> {
		if a == b {
			return Ok(a);
		}
		if a == TypeIndex::NEVER {
			return Ok(b);
		}
		if b == TypeIndex::NEVER {
			return Ok(a);
		}
		if a == TypeIndex::ERROR || b == TypeIndex::ERROR {
			return Ok(TypeIndex::ERROR);
		}
		// Two FunctionItems (generic or not) unify to their common concrete Function
		// type. Handles: `if cond { fn_a } else { fn_b }` and `if cond {
		// f::<i32> } else { g::<i32> }`.
		if let (
			&Type::FunctionItem {
				id: a_id,
				type_args: ref a_args,
			},
			&Type::FunctionItem {
				id: b_id,
				type_args: ref b_args,
			},
		) = (&self.tir.types[a.as_usize()], &self.tir.types[b.as_usize()])
		{
			let a_args = a_args.clone();
			let b_args = b_args.clone();
			let a_sig = self.tir.functions
				[self.tir.expect_function_index(a_id) as usize]
				.signature_index;
			let b_sig = self.tir.functions
				[self.tir.expect_function_index(b_id) as usize]
				.signature_index;
			let concrete_a = self.substitute_type(a_sig, &a_args);
			let concrete_b = self.substitute_type(b_sig, &b_args);
			if concrete_a == concrete_b {
				return Ok(concrete_a);
			}
		}
		Err(())
	}

	pub fn intern_function(
		&mut self,
		params: &[FunctionParam],
		result: Option<Spanned<TypeIndex>>,
	) -> TypeIndex {
		self.intern_type(Type::Function {
			signature: FunctionSignature {
				items: params
					.iter()
					.map(|p| p.ty.inner)
					.chain(Some(match result {
						Some(ty) => ty.inner,
						None => TypeIndex::UNIT,
					}))
					.collect(),
				params_count: params.len() as u32,
			},
		})
	}

	fn ensure_module(
		&mut self,
		file_id: FileId,
		namespace: Option<NamespaceIndex>,
		name: ast::Spanned<SymbolU32>,
		pub_span: Option<ast::TextSpan>,
	) -> NamespaceIndex {
		let symbol = name.inner;
		if let Some(SymbolKind::Module { namespace_idx }) = self
			.lookup_global_symbol(namespace, (SymbolNamespace::Type, symbol))
		{
			if let ModuleDeclarationKind::Module(decl_idx) =
				self.tir.namespaces[namespace_idx as usize].declaration
			{
				let decl = &mut self.tir.module_decls[decl_idx as usize];
				if decl.pub_span.is_none() {
					decl.pub_span = pub_span;
				}
			}
			return namespace_idx;
		}

		let namespace_idx = self.tir.namespaces.len() as u32;
		let decl_idx = self.tir.module_decls.len() as u32;
		self.tir.namespaces.push(ModuleNamespace {
			name: symbol,
			parent: namespace,
			declaration: ModuleDeclarationKind::Module(decl_idx),
			symbols: HashMap::new(),
			wildcard_imports: Vec::new(),
			accesses: Vec::new(),
		});
		self.tir.module_decls.push(ModuleDecl {
			namespace_idx,
			declaring_file_id: file_id,
			own_file_id: None,
			name,
			pub_span,
		});
		self.insert_symbol(
			namespace,
			(SymbolNamespace::Type, symbol),
			SymbolKind::Module { namespace_idx },
		);
		namespace_idx
	}

	fn ensure_module_path(
		&mut self,
		file_id: FileId,
		base: Option<NamespaceIndex>,
		path: &[SymbolU32],
	) -> Option<NamespaceIndex> {
		let mut namespace = base;

		for (i, segment) in path.iter().copied().enumerate() {
			let namespace_idx = self.ensure_module(
				file_id,
				namespace,
				ast::Spanned {
					inner: segment,
					span: ast::TextSpan::new(0, 0),
				},
				None,
			);
			// Set own_file_id on the last segment — that's the source module's actual file.
			if i == path.len() - 1 {
				if let ModuleDeclarationKind::Module(decl_idx) =
					self.tir.namespaces[namespace_idx as usize].declaration
				{
					self.tir.module_decls[decl_idx as usize].own_file_id =
						Some(file_id);
				}
			}
			namespace = Some(namespace_idx);
		}

		namespace
	}

	/// Walk `ty` without crossing pointer/slice boundaries and return the span
	/// of the first field whose type directly contains `root_struct_index`.
	/// `visited` prevents re-entering structs already on the walk path.
	fn find_direct_struct_recursion(
		&self,
		ty: TypeIndex,
		root_struct_index: u32,
		visited: &mut Vec<u32>,
	) -> bool {
		match &self.tir.types[ty.as_usize()] {
			Type::Struct { struct_index, .. } => {
				if *struct_index == root_struct_index {
					return true;
				}
				if visited.contains(struct_index) {
					return false;
				}
				visited.push(*struct_index);
				let found = self.tir.structs[*struct_index as usize]
					.fields
					.iter()
					.map(|field| field.ty.inner)
					.any(|field_type| {
						self.find_direct_struct_recursion(
							field_type,
							root_struct_index,
							visited,
						)
					});
				visited.pop();
				found
			}
			Type::Tuple { elements } => {
				elements.iter().copied().any(|element| {
					self.find_direct_struct_recursion(
						element,
						root_struct_index,
						visited,
					)
				})
			}
			// Pointer and slice are indirection — stop here.
			Type::Pointer { .. } | Type::Slice { .. } | Type::Array { .. } => {
				false
			}
			_ => false,
		}
	}

	/// Report an error if any field of the struct at `struct_index` directly
	/// (without pointer/slice indirection) contains the struct itself.
	/// Cycles through generic struct instantiation are not detected here; see
	/// the TODO in `mir::Builder::ensure_aggregate_for_struct`.
	fn check_struct_fields_for_direct_recursion(
		&mut self,
		struct_index: u32,
		struct_span: SourceSpan,
	) {
		let mut visited = vec![struct_index];
		for (field_ty, field_span) in self.tir.structs[struct_index as usize]
			.fields
			.iter()
			.map(|field| {
				(
					field.ty.inner,
					SourceSpan::new(
						self.tir.structs[struct_index as usize].file_id,
						field.ty.span,
					),
				)
			}) {
			if self.find_direct_struct_recursion(
				field_ty,
				struct_index,
				&mut visited,
			) {
				let name = self
					.interner
					.resolve(self.tir.structs[struct_index as usize].name.inner)
					.unwrap();
				self.tir.diagnostics.push(report_recursive_type(
					name,
					struct_span,
					field_span,
				));
				return;
			}
			visited.truncate(1);
		}
	}

	fn insert_symbol(
		&mut self,
		namespace: Option<NamespaceIndex>,
		key: (SymbolNamespace, SymbolU32),
		kind: SymbolKind,
	) {
		if let Some(idx) = namespace {
			self.tir.namespaces[idx as usize].symbols.insert(key, kind);
		} else {
			self.symbol_lookup.insert(key, kind);
		}
	}

	/// Looks up `key` in `namespace`'s own symbol map only — no parent-scope
	/// or wildcard-import fallback. Used for Phase-1 duplicate-definition
	/// checks (locals must silently shadow wildcard imports, matching
	/// `Trait`'s existing pre-scan check) and for the Phase-2 "do I still
	/// hold my own `Pending` slot" check that decides whether an item wins
	/// its name binding.
	fn direct_scope_lookup(
		&self,
		namespace: Option<NamespaceIndex>,
		key: (SymbolNamespace, SymbolU32),
	) -> Option<SymbolKind> {
		if let Some(idx) = namespace {
			self.tir.namespaces[idx as usize].symbols.get(&key).copied()
		} else {
			self.symbol_lookup.get(&key).copied()
		}
	}

	/// Phase-1 registration for a name that may collide with an earlier
	/// item in the same direct scope. `pre_scan_item` never installs
	/// anything but `Pending`, so a collision here can only be a same-scope
	/// duplicate — never a wildcard import, which lives in a different
	/// scope's own map and is only ever consulted through the fallback
	/// chain, not this direct lookup.
	///
	/// Callers must unconditionally allocate the item's stub/index/
	/// `ast_nodes` entry regardless of the outcome — only the name binding
	/// is exclusive; every syntactic occurrence still gets fully resolved.
	fn claim_name_binding(
		&mut self,
		namespace: Option<NamespaceIndex>,
		key: (SymbolNamespace, SymbolU32),
		id: ast::DefId,
		definition_span: SourceSpan,
	) -> PendingClaim {
		if let Some(existing) = self.direct_scope_lookup(namespace, key) {
			let first_definition = self.get_symbol_location(existing);
			let name_str = self.interner.resolve(key.1).unwrap();
			self.tir.diagnostics.push(report_duplicate_definition(
				DuplicateDefinitionDiagnostic {
					name: name_str,
					namespace: key.0,
					first_definition,
					second_definition: definition_span,
				},
			));
			PendingClaim::Duplicate
		} else {
			self.insert_symbol(namespace, key, SymbolKind::Pending(id));
			PendingClaim::Claimed
		}
	}

	/// Binds a `memory` declaration whose kind bound failed to resolve
	/// (e.g. an unresolved/invalid trait bound, often because `Memory`
	/// wasn't brought into scope) to a `TypeIndex::ERROR` placeholder.
	/// Without this, the name stays `SymbolKind::Pending` forever and any
	/// later use of it hits the "signature resolved but symbol still
	/// pending" unreachable in `resolve_symbol_kind_to_expression`. The
	/// stub itself (already defaulted to `TypeIndex::ERROR`, no bounds) was
	/// allocated in `pre_scan_item`; this only needs to bind the name. This
	/// placeholder is never actually reached downstream: `report_invalid_
	/// memory_kind` is an error diagnostic, and the real compile pipeline
	/// (`wx-cli`) aborts before `MIR::build` whenever TIR has any errors.
	fn register_placeholder_memory(
		&mut self,
		resolve_context: ResolveContext,
		id: ast::DefId,
		name: &ast::Spanned<SymbolU32>,
	) {
		let memory_index = self.tir.expect_memory_index(id);
		let kind = TypeIndex::ERROR;
		let type_key = (SymbolNamespace::Type, name.inner);
		if matches!(
			self.direct_scope_lookup(resolve_context.namespace, type_key),
			Some(SymbolKind::Pending(pending_id)) if pending_id == id
		) {
			self.insert_symbol(
				resolve_context.namespace,
				type_key,
				SymbolKind::Memory { memory_index, kind },
			);
		}
		let value_key = (SymbolNamespace::Value, name.inner);
		if matches!(
			self.direct_scope_lookup(resolve_context.namespace, value_key),
			Some(SymbolKind::Pending(pending_id)) if pending_id == id
		) {
			self.insert_symbol(
				resolve_context.namespace,
				value_key,
				SymbolKind::Memory { memory_index, kind },
			);
		}
	}

	fn lookup_global_symbol(
		&self,
		namespace: Option<NamespaceIndex>,
		key: (SymbolNamespace, SymbolU32),
	) -> Option<SymbolKind> {
		let mut current = namespace;
		while let Some(idx) = current {
			let namespace = &self.tir.namespaces[idx as usize];
			if let Some(kind) = namespace.symbols.get(&key).copied() {
				return Some(kind);
			}
			for namespace_idx in namespace.wildcard_imports.iter().copied() {
				if let Some(kind) = self.tir.namespaces[namespace_idx as usize]
					.symbols
					.get(&key)
					.copied()
				{
					return Some(kind);
				}
			}
			current = namespace.parent;
		}
		if let Some(kind) = self.symbol_lookup.get(&key).copied() {
			return Some(kind);
		};
		for namespace_idx in self.root_wildcard_imports.iter().copied() {
			if let Some(kind) = self.tir.namespaces[namespace_idx as usize]
				.symbols
				.get(&key)
				.copied()
			{
				return Some(kind);
			}
		}
		None
	}

	/// Looks up `key` via [`Self::lookup_global_symbol`], forcing a `Pending`
	/// result through `ensure_signature` and re-looking it up. Returns
	/// `Err(())` (with a cyclic-dependency diagnostic already pushed) if the
	/// pending item is still being computed on the current call stack.
	fn resolve_pending_global_symbol(
		&mut self,
		namespace: Option<NamespaceIndex>,
		key: (SymbolNamespace, SymbolU32),
		file_id: FileId,
		span: TextSpan,
	) -> Result<Option<SymbolKind>, ()> {
		match self.lookup_global_symbol(namespace, key) {
			Some(SymbolKind::Pending(def_id)) => {
				if matches!(
					self.sig_state.get(&def_id),
					Some(SigEntry {
						state: ComputeState::InProgress,
						..
					})
				) {
					self.tir.diagnostics.push(report_cyclic_type_dependency(
						SourceSpan::new(file_id, span),
					));
					return Err(());
				}
				self.ensure_signature(def_id);
				Ok(self.lookup_global_symbol(namespace, key))
			}
			other => Ok(other),
		}
	}

	/// Looks up `key` in `namespace_idx`'s own symbol map — no parent-scope
	/// or wildcard-import fallback, for `module::Name` qualified lookups —
	/// forcing a `Pending` result through `ensure_signature` and re-looking
	/// it up. Same cyclic-dependency handling as
	/// [`Self::resolve_pending_global_symbol`].
	fn resolve_pending_namespace_symbol(
		&mut self,
		namespace_idx: NamespaceIndex,
		key: (SymbolNamespace, SymbolU32),
		span: SourceSpan,
	) -> Result<Option<SymbolKind>, ()> {
		match self.tir.namespaces[namespace_idx as usize]
			.symbols
			.get(&key)
			.copied()
		{
			Some(SymbolKind::Pending(def_id)) => {
				if matches!(
					self.sig_state.get(&def_id),
					Some(SigEntry {
						state: ComputeState::InProgress,
						..
					})
				) {
					self.tir
						.diagnostics
						.push(report_cyclic_type_dependency(span));
					return Err(());
				}
				self.ensure_signature(def_id);
				Ok(self.tir.namespaces[namespace_idx as usize]
					.symbols
					.get(&key)
					.copied())
			}
			other => Ok(other),
		}
	}

	/// Resolves a single symbol name, checking local variables first, then
	/// the global symbol table. Use `lookup_global_symbol` directly in
	/// contexts without a function scope (const/global initializers).
	fn resolve_symbol(
		&self,
		func_ctx: &ExprContext,
		symbol: SymbolU32,
	) -> Option<ResolvedSymbol> {
		if let Some((scope_index, local_index)) = func_ctx.resolve_local(symbol)
		{
			return Some(ResolvedSymbol::Local {
				scope_index,
				local_index,
			});
		}
		self.lookup_global_symbol(
			func_ctx.resolve_context.namespace,
			(SymbolNamespace::Value, symbol),
		)
		.map(ResolvedSymbol::Global)
	}

	/// Converts a `ResolvedSymbol` into an `Expression`, registering any
	/// access entries along the way. The caller is responsible for emitting
	/// the not-found diagnostic when `resolve_symbol` returned `None`.
	fn resolved_symbol_to_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		resolved: ResolvedSymbol,
		expr_span: TextSpan,
	) -> Result<Expression, ()> {
		match resolved {
			ResolvedSymbol::Local {
				scope_index,
				local_index,
			} => {
				func_ctx.stack.record_local_access(
					scope_index,
					local_index,
					LocalAccess {
						kind: access_ctx.access_kind,
						span: expr_span,
					},
				);
				let local = func_ctx.stack.get_local(scope_index, local_index);
				if matches!(
					access_ctx.access_kind,
					AccessKind::Write | AccessKind::ReadWrite
				) && local.mut_span.is_none()
				{
					self.tir.diagnostics.push(report_cannot_mutate_immutable(
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							expr_span,
						),
					));
				}
				Ok(Expression {
					kind: ExprKind::Local {
						local_index,
						scope_index,
					},
					ty: local.ty,
					span: expr_span,
				})
			}
			ResolvedSymbol::Global(kind) => self.global_symbol_to_expression(
				func_ctx.resolve_context,
				access_ctx,
				kind,
				expr_span,
			),
		}
	}

	/// Converts a global `SymbolKind` into an `Expression`. Takes only a
	/// `ResolveContext` so it is usable from const/global initializer resolution
	/// which has no enclosing function.
	fn global_symbol_to_expression(
		&mut self,
		resolve_ctx: ResolveContext,
		access_ctx: AccessContext,
		kind: SymbolKind,
		expr_span: TextSpan,
	) -> Result<Expression, ()> {
		match kind {
			SymbolKind::Function { func_index } => {
				let func_id = self.tir.functions[func_index as usize].id;
				let type_params_len =
					self.tir.functions[func_index as usize].type_params.len();
				self.tir.functions[func_index as usize]
					.accesses
					.push(SourceSpan::new(resolve_ctx.file_id, expr_span));
				let ty = self.intern_type(Type::FunctionItem {
					id: func_id,
					type_args: vec![TypeIndex::INFER; type_params_len]
						.into_boxed_slice(),
				});
				Ok(Expression {
					kind: ExprKind::Function { id: func_id },
					ty,
					span: expr_span,
				})
			}
			SymbolKind::Global { global_index } => {
				let global = &mut self.tir.globals[global_index as usize];
				global
					.accesses
					.push(SourceSpan::new(resolve_ctx.file_id, expr_span));
				if matches!(
					access_ctx.access_kind,
					AccessKind::Write | AccessKind::ReadWrite
				) && global.mut_span.is_none()
				{
					self.tir.diagnostics.push(report_cannot_mutate_immutable(
						SourceSpan::new(resolve_ctx.file_id, expr_span),
					));
				}
				let id = global.id;
				let ty = global.ty.inner;
				Ok(Expression {
					kind: ExprKind::Global { id },
					ty,
					span: expr_span,
				})
			}
			SymbolKind::Const { const_index } => {
				let constant = &mut self.tir.constants[const_index as usize];
				constant
					.accesses
					.push(SourceSpan::new(resolve_ctx.file_id, expr_span));
				let id = constant.id;
				let ty = constant.ty.inner;
				Ok(Expression {
					kind: ExprKind::Const { id },
					ty,
					span: expr_span,
				})
			}
			SymbolKind::Memory { memory_index, kind } => {
				let id = self.tir.memories[memory_index as usize].id;
				let ty = self.intern_type(Type::Memory { kind, id });
				Ok(Expression {
					kind: ExprKind::Memory { id },
					ty,
					span: expr_span,
				})
			}
			SymbolKind::Enum { .. }
			| SymbolKind::Module { .. }
			| SymbolKind::Struct { .. }
			| SymbolKind::Trait { .. }
			| SymbolKind::TypeSet { .. }
			| SymbolKind::TypeAlias { .. } => {
				self.tir.diagnostics.push(report_namespace_used_as_value(
					SourceSpan::new(resolve_ctx.file_id, expr_span),
				));
				Ok(Expression {
					kind: ExprKind::Error,
					ty: TypeIndex::ERROR,
					span: expr_span,
				})
			}
			#[cfg(debug_assertions)]
			symbol @ (SymbolKind::TraitAssocType { .. }
			| SymbolKind::Pending(_)) => match symbol {
				SymbolKind::Pending(def_id) => {
					let item = self
						.ast_nodes
						.iter()
						.find(|item| item.def_id == def_id);
					unreachable!("{:#?}", item);
				}
				_ => unreachable!(),
			},
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	fn symbol_kind_to_type(&mut self, kind: SymbolKind) -> Option<TypeIndex> {
		match kind {
			SymbolKind::Memory { kind, memory_index } => {
				let id = self.tir.memories[memory_index as usize].id;
				Some(self.intern_type(Type::Memory { kind, id }))
			}
			SymbolKind::Module { namespace_idx } => {
				Some(self.intern_type(Type::Namespace { namespace_idx }))
			}
			SymbolKind::Enum { enum_index } => {
				Some(self.intern_type(Type::Enum { enum_index }))
			}
			SymbolKind::Struct { struct_index } => {
				Some(self.intern_type(Type::Struct {
					struct_index,
					args: Box::new([]),
				}))
			}
			SymbolKind::Const { const_index } => {
				let constant = &self.tir.constants[const_index as usize];
				Some(constant.ty.inner)
			}
			SymbolKind::Global { global_index } => {
				let global = &self.tir.globals[global_index as usize];
				Some(global.ty.inner)
			}
			SymbolKind::Function { func_index } => {
				let function = &self.tir.functions[func_index as usize];
				Some(function.signature_index)
			}
			SymbolKind::Trait { .. }
			| SymbolKind::TypeSet { .. }
			| SymbolKind::TraitAssocType { .. }
			| SymbolKind::Pending(_) => None,
			SymbolKind::TypeAlias { type_alias_index } => {
				Some(self.tir.type_aliases[type_alias_index as usize].template)
			}
		}
	}

	/// Resolve a bare identifier symbol to a `TypeIndex`.
	/// Extracted from the `TypeExpression::Identifier` arm so it can be called
	/// directly from path-walking code without constructing AST nodes.
	fn resolve_type_identifier(
		&mut self,
		resolve_context: ResolveContext,
		scope: Option<GenericScope>,
		identifier: Spanned<SymbolU32>,
	) -> Result<TypeIndex, ()> {
		// `Self` — the concrete type of the enclosing impl block.
		// For impl blocks, self_type holds the target type directly. For trait
		// methods, self_type is None and `Self` is instead found by name via the
		// parent-chain lookup below (TypeParamOwner::Trait owns it).
		if self.interner.resolve(identifier.inner) == Some("Self") {
			if let Some(self_ty) = scope.and_then(|s| s.self_type) {
				if let Type::TypeParam { owner, param_index } =
					self.tir.types[self_ty.as_usize()]
				{
					self.record_type_param_access(
						owner,
						param_index,
						SourceSpan::new(
							resolve_context.file_id,
							identifier.span,
						),
					);
				}
				return Ok(self_ty);
			}
			// Fall through — trait functions find `Self` via the parent chain below.
		}
		if let Ok(ty) =
			Type::try_from(self.interner.resolve(identifier.inner).unwrap())
		{
			return Ok(self.intern_type(ty));
		}
		if let Some(scope) = scope {
			// Search the owner's own type params first (innermost scope wins).
			let own_params: &[TypeParamInfo] = match scope.owner {
				TypeParamOwner::ImplBlock(block_idx) => {
					&self.tir.generic_impl_list[block_idx as usize].type_params
				}
				TypeParamOwner::Function(id) => {
					self.tir.function_index(id).map_or(&[], |idx| {
						&self.tir.functions[idx as usize].type_params
					})
				}
				TypeParamOwner::Struct(id) => {
					self.tir.struct_index(id).map_or(&[], |idx| {
						&self.tir.structs[idx as usize].type_params
					})
				}
				TypeParamOwner::Trait(_) => &[],
				TypeParamOwner::TypeAlias(id) => {
					self.tir.type_alias_index(id).map_or(&[], |idx| {
						&self.tir.type_aliases[idx as usize].type_params
					})
				}
			};
			if let Some(own_idx) =
				own_params.iter().position(|p| p.name == identifier.inner)
			{
				let owner = scope.owner;
				let abs_index =
					(self.inherited_type_param_count(owner) + own_idx) as u32;
				self.record_type_param_access(
					owner,
					abs_index,
					SourceSpan::new(resolve_context.file_id, identifier.span),
				);
				return Ok(self.intern_type(Type::TypeParam {
					owner,
					param_index: abs_index,
				}));
			}
			// Not found in own params — check the parent impl block (if any).
			if let TypeParamOwner::Function(fn_id) = scope.owner {
				if let Some(fn_idx) = self.tir.function_index(fn_id) {
					if let Some(parent_owner) =
						self.tir.functions[fn_idx as usize].type_param_parent
					{
						let parent_params =
							self.owner_type_params(parent_owner);
						if let Some(i) = parent_params
							.iter()
							.position(|p| p.name == identifier.inner)
						{
							// ImplBlock has no grandparent, so abs_index == i.
							let abs_index = i as u32;
							self.record_type_param_access(
								parent_owner,
								abs_index,
								SourceSpan::new(
									resolve_context.file_id,
									identifier.span,
								),
							);
							return Ok(self.intern_type(Type::TypeParam {
								owner: parent_owner,
								param_index: abs_index,
							}));
						}
					}
				}
			}
		}
		let kind = self.resolve_pending_global_symbol(
			resolve_context.namespace,
			(SymbolNamespace::Type, identifier.inner),
			resolve_context.file_id,
			identifier.span,
		)?;
		match kind {
			Some(SymbolKind::TraitAssocType { assoc_name, .. }) => {
				let name = self.interner.resolve(assoc_name).unwrap();
				self.tir.diagnostics.push(report_bare_assoc_type(
					SourceSpan::new(resolve_context.file_id, identifier.span),
					name,
				));
				Err(())
			}
			Some(SymbolKind::Trait { .. } | SymbolKind::TypeSet { .. }) => {
				self.tir.diagnostics.push(
                    Diagnostic::error()
                        .with_code(DiagnosticCode::ExpectedTrait.code())
                        .with_message("cannot use a trait or typeset as a type; use it as a bound: `<T: TraitName>`")
                        .with_label(Label::primary(resolve_context.file_id, identifier.span)),
                );
				Err(())
			}
			Some(
				kind @ (SymbolKind::Struct { .. }
				| SymbolKind::TypeAlias { .. }),
			) => {
				let ty = self.resolve_generic_type_application(
					resolve_context,
					kind,
					&[],
					identifier.span,
				);
				if ty == TypeIndex::ERROR {
					Err(())
				} else {
					Ok(ty)
				}
			}
			Some(kind) => {
				self.record_type_kind_access(
					resolve_context.file_id,
					kind,
					identifier.span,
				);
				if let Some(ty) = self.symbol_kind_to_type(kind) {
					return Ok(ty);
				}
				self.tir.diagnostics.push(report_undeclared_type(
					SourceSpan::new(resolve_context.file_id, identifier.span),
				));
				Err(())
			}
			None => {
				self.tir.diagnostics.push(report_undeclared_type(
					SourceSpan::new(resolve_context.file_id, identifier.span),
				));
				Err(())
			}
		}
	}

	/// If `name` names a type parameter reachable from `scope` (its own
	/// owner, or — for a function nested in an impl block — the parent impl
	/// block), returns its absolute index. Mirrors the type-param branch of
	/// [`Self::resolve_type_identifier`] but without any of its resolution
	/// side effects (no interning, no access recording): used purely to
	/// detect type-param/global shadowing before deciding whether turbofish
	/// applies to a global struct/alias.
	fn identifier_type_param_index(
		&self,
		scope: GenericScope,
		name: SymbolU32,
	) -> Option<u32> {
		let own_params: &[TypeParamInfo] = match scope.owner {
			TypeParamOwner::ImplBlock(block_idx) => {
				&self.tir.generic_impl_list[block_idx as usize].type_params
			}
			TypeParamOwner::Function(id) => {
				self.tir.function_index(id).map_or(&[], |idx| {
					&self.tir.functions[idx as usize].type_params
				})
			}
			TypeParamOwner::Struct(id) => self
				.tir
				.struct_index(id)
				.map_or(&[], |idx| &self.tir.structs[idx as usize].type_params),
			TypeParamOwner::Trait(_) => &[],
			TypeParamOwner::TypeAlias(id) => {
				self.tir.type_alias_index(id).map_or(&[], |idx| {
					&self.tir.type_aliases[idx as usize].type_params
				})
			}
		};
		if let Some(own_idx) = own_params.iter().position(|p| p.name == name) {
			return Some(
				(self.inherited_type_param_count(scope.owner) + own_idx) as u32,
			);
		}
		if let TypeParamOwner::Function(fn_id) = scope.owner {
			if let Some(fn_idx) = self.tir.function_index(fn_id) {
				if let Some(parent_owner) =
					self.tir.functions[fn_idx as usize].type_param_parent
				{
					// ImplBlock has no grandparent, so abs_index == i.
					if let Some(i) = self
						.owner_type_params(parent_owner)
						.iter()
						.position(|p| p.name == name)
					{
						return Some(i as u32);
					}
				}
			}
		}
		None
	}

	/// Like [`resolve_type`], but rejects `_` in positions where a concrete type
	/// is required (item signatures, struct fields, globals). Emits a diagnostic
	/// and returns `ERROR` instead of `INFER` so the rest of the compiler stays clean.
	fn resolve_signature_type(
		&mut self,
		resolve_context: ResolveContext,
		scope: Option<GenericScope>,
		type_expr: &Spanned<ast::TypeExpression>,
	) -> TypeIndex {
		let ty = self.resolve_type(resolve_context, scope, type_expr);
		if self.contains_infer(ty) {
			self.tir.diagnostics.push(report_infer_in_signature(
				SourceSpan::new(resolve_context.file_id, type_expr.span),
			));
			return TypeIndex::ERROR;
		}
		ty
	}

	pub fn resolve_type(
		&mut self,
		resolve_context: ResolveContext,
		scope: Option<GenericScope>,
		type_expr: &Spanned<ast::TypeExpression>,
	) -> TypeIndex {
		match &type_expr.inner {
			ast::TypeExpression::Infer => TypeIndex::INFER,
			ast::TypeExpression::Path(path) => self.resolve_path_type(
				resolve_context,
				scope,
				path,
				type_expr.span,
			),
			ast::TypeExpression::Function { params, result } => {
				let result_idx = match result {
					Some(result) => {
						self.resolve_type(resolve_context, scope, result)
					}
					None => TypeIndex::UNIT,
				};

				// TODO: use intern_function?
				let params_count = params.len();
				let mut items: Vec<TypeIndex> =
					Vec::with_capacity(params_count + 1);
				for ty in params.iter() {
					items.push(self.resolve_type(
						resolve_context,
						scope,
						&ty.inner.inner.ty,
					));
				}
				items.push(result_idx);
				let items: Box<[TypeIndex]> = items.into();
				self.intern_type(Type::Function {
					signature: FunctionSignature {
						params_count: params_count as u32,
						items,
					},
				})
			}
			ast::TypeExpression::Pointer { mutability, inner } => {
				let to = self.resolve_type(resolve_context, scope, inner);
				let span =
					SourceSpan::new(resolve_context.file_id, type_expr.span);
				let Ok(memory) = self.resolve_ambient_memory(span) else {
					return TypeIndex::ERROR;
				};
				self.intern_type(Type::Pointer {
					to,
					memory,
					mutable: mutability.is_some(),
				})
			}
			ast::TypeExpression::Slice { mutability, inner } => {
				let of = self.resolve_type(resolve_context, scope, inner);
				let span =
					SourceSpan::new(resolve_context.file_id, type_expr.span);
				let Ok(memory) = self.resolve_ambient_memory(span) else {
					return TypeIndex::ERROR;
				};
				self.intern_type(Type::Slice {
					of,
					memory,
					mutable: mutability.is_some(),
				})
			}
			ast::TypeExpression::Array {
				size,
				mutability,
				inner,
			} => {
				let of = self.resolve_type(resolve_context, scope, inner);
				let span =
					SourceSpan::new(resolve_context.file_id, type_expr.span);
				let Ok(memory) = self.resolve_ambient_memory(span) else {
					return TypeIndex::ERROR;
				};
				self.intern_type(Type::Array {
					of,
					size: size.inner as u32,
					memory,
					mutable: mutability.is_some(),
				})
			}
			ast::TypeExpression::Tuple { elements } => {
				if elements.is_empty() {
					return TypeIndex::UNIT;
				}
				let mut elems: Vec<TypeIndex> =
					Vec::with_capacity(elements.len());
				for e in elements.iter() {
					elems.push(self.resolve_type(resolve_context, scope, e));
				}
				self.intern_type(Type::Tuple {
					elements: elems.into(),
				})
			}
			ast::TypeExpression::MemoryTagged { memory, inner } => {
				let first = &memory[0];
				let Ok(mut memory_ty) = self.resolve_type_identifier(
					resolve_context,
					scope,
					first.ident,
				) else {
					return TypeIndex::ERROR;
				};
				// Walk remaining segments (e.g. `Self::M` has two: `Self`, then `M`).
				let mut namespace_span = first.ident.span;
				for seg in &memory[1..] {
					match self.resolve_namespace_type_member(
						resolve_context,
						memory_ty,
						namespace_span,
						seg.ident,
					) {
						Ok(ty) => {
							memory_ty = ty;
							namespace_span = seg.ident.span;
						}
						Err(()) => return TypeIndex::ERROR,
					}
				}
				match &self.tir.types[memory_ty.as_usize()] {
					Type::Memory { .. }
					| Type::TypeParam { .. }
					| Type::AssocTypeProjection { .. } => {}
					_ => {
						let span = TextSpan::new(
							memory.first().unwrap().ident.span.start,
							memory.last().unwrap().ident.span.end,
						);
						self.tir.diagnostics.push(
							Diagnostic::error()
								.with_message(format!(
									"`{}` is not a memory declaration",
									TypeFormatter::new(
										&self.tir,
										self.interner
									)
									.display_type(memory_ty)
									.unwrap()
								))
								.with_label(Label::primary(
									resolve_context.file_id,
									span,
								)),
						);
						return TypeIndex::ERROR;
					}
				};
				// Resolve the inner expression directly by AST kind so the outer
				// memory is applied without triggering ambient memory resolution
				// for untagged pointer/array/slice annotations.
				match &inner.inner {
					ast::TypeExpression::Pointer {
						mutability,
						inner: ptr_inner,
					} => {
						let to = self.resolve_type(
							resolve_context,
							scope,
							ptr_inner,
						);
						self.intern_type(Type::Pointer {
							to,
							memory: memory_ty,
							mutable: mutability.is_some(),
						})
					}
					ast::TypeExpression::Array {
						size,
						mutability,
						inner: arr_inner,
					} => {
						let of = self.resolve_type(
							resolve_context,
							scope,
							arr_inner,
						);
						self.intern_type(Type::Array {
							of,
							size: size.inner as u32,
							memory: memory_ty,
							mutable: mutability.is_some(),
						})
					}
					ast::TypeExpression::Slice {
						mutability,
						inner: sl_inner,
					} => {
						let of =
							self.resolve_type(resolve_context, scope, sl_inner);
						self.intern_type(Type::Slice {
							of,
							memory: memory_ty,
							mutable: mutability.is_some(),
						})
					}
					_ => {
						self.tir.diagnostics.push(
							Diagnostic::error()
								.with_message(
									"memory namespace can only prefix pointer, slice, or array types",
								)
								.with_label(Label::primary(
									resolve_context.file_id,
									inner.span,
								)),
						);
						TypeIndex::ERROR
					}
				}
			}
			ast::TypeExpression::GenericApplication { name, args } => {
				if let Some(SymbolKind::Pending(def_id)) = self
					.lookup_global_symbol(
						resolve_context.namespace,
						(SymbolNamespace::Type, name.inner),
					) {
					self.ensure_signature(def_id);
				}
				match self.lookup_global_symbol(
					resolve_context.namespace,
					(SymbolNamespace::Type, name.inner),
				) {
					Some(
						kind @ (SymbolKind::Struct { .. }
						| SymbolKind::TypeAlias { .. }),
					) => {
						let mut resolved_args: Vec<TypeIndex> =
							Vec::with_capacity(args.len());
						for sep in args.iter() {
							resolved_args.push(self.resolve_type(
								resolve_context,
								scope,
								&sep.inner,
							));
						}
						self.resolve_generic_type_application(
							resolve_context,
							kind,
							&resolved_args,
							name.span,
						)
					}
					_ => {
						// Not a struct — eagerly resolve args to surface type errors.
						// TODO: fix this weird ast type construction
						for sep in args.iter() {
							self.resolve_type(
								resolve_context,
								scope,
								&sep.inner,
							);
						}
						let base = Spanned {
							inner: ast::TypeExpression::Path(Box::new([
								ast::PathSegment {
									ident: *name,
									type_args: Box::new([]),
								},
							])),
							span: name.span,
						};
						self.resolve_type(resolve_context, scope, &base)
					}
				}
			}
		}
	}

	/// Resolves a `::`-separated path in type position — plain identifiers,
	/// namespaced paths (`module::Type`), and turbofish generic args
	/// (`Wrapper::<T>`, `module::Wrapper::<T>`). Shared by [`Self::resolve_type`]
	/// and struct-init expression resolution, so both spellings of "apply type
	/// args to a struct/alias" go through [`Self::resolve_generic_type_application`].
	fn resolve_path_type(
		&mut self,
		resolve_context: ResolveContext,
		scope: Option<GenericScope>,
		path: &[ast::PathSegment],
		span: TextSpan,
	) -> TypeIndex {
		let last = path.last().expect("path is non-empty");

		// ── single segment, no type args: plain identifier ─────────────
		if path.len() == 1 && last.type_args.is_empty() {
			return self
				.resolve_type_identifier(resolve_context, scope, last.ident)
				.unwrap_or(TypeIndex::ERROR);
		}

		// ── single segment with turbofish args: `Wrapper::<T>` ─────────
		if path.len() == 1 {
			// A name that shadows a type param in this scope resolves via
			// the type-param scope, not the global symbol table, so it can
			// never carry turbofish args. Checked without a full
			// `resolve_type_identifier` call so a bare generic struct/alias
			// reference below doesn't waste-intern a padded placeholder type.
			if scope.is_some_and(|s| {
				self.identifier_type_param_index(s, last.ident.inner)
					.is_some()
			}) {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_message("type arguments are not supported here")
						.with_label(Label::primary(
							resolve_context.file_id,
							span,
						)),
				);
				return TypeIndex::ERROR;
			}
			if let Some(SymbolKind::Pending(def_id)) = self
				.lookup_global_symbol(
					resolve_context.namespace,
					(SymbolNamespace::Type, last.ident.inner),
				) {
				self.ensure_signature(def_id);
			}
			let Some(symbol_kind) = self.lookup_global_symbol(
				resolve_context.namespace,
				(SymbolNamespace::Type, last.ident.inner),
			) else {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_message("type arguments are not supported here")
						.with_label(Label::primary(
							resolve_context.file_id,
							span,
						)),
				);
				return TypeIndex::ERROR;
			};
			let mut resolved_args: Vec<TypeIndex> =
				Vec::with_capacity(last.type_args.len());
			for arg in last.type_args.iter() {
				resolved_args.push(self.resolve_type(
					resolve_context,
					scope,
					arg,
				));
			}
			return self.resolve_generic_type_application(
				resolve_context,
				symbol_kind,
				&resolved_args,
				last.ident.span,
			);
		}

		// ── multi-segment: walk namespace chain ────────────────────────
		// TODO: for full LSP per-segment support, ExprKind needs a nested
		// namespace node so each intermediate segment carries its own span and
		// TypeIndex.  Until then each lookup registers only its own segment span.
		let first = &path[0];
		let Ok(mut namespace_ty) =
			self.resolve_type_identifier(resolve_context, scope, first.ident)
		else {
			return TypeIndex::ERROR;
		};
		let mut namespace_span = first.ident.span;

		for seg in &path[1..path.len() - 1] {
			match self.resolve_namespace_type_member(
				resolve_context,
				namespace_ty,
				namespace_span,
				seg.ident,
			) {
				Ok(ty) => {
					namespace_ty = ty;
					namespace_span = seg.ident.span;
				}
				Err(()) => return TypeIndex::ERROR,
			}
		}

		// Resolve the last segment within the current namespace.
		let Ok(last_ty) = self.resolve_namespace_type_member(
			resolve_context,
			namespace_ty,
			namespace_span,
			last.ident,
		) else {
			return TypeIndex::ERROR;
		};

		// Apply turbofish type args on the last segment if present.
		if last.type_args.is_empty() {
			return last_ty;
		}
		// Type args on a namespaced segment are only meaningful when that
		// namespace is a module — structs/aliases only live in module
		// namespaces, associated-type paths never reach turbofish (see
		// resolve_namespace_type_member).
		let Type::Namespace { namespace_idx } =
			self.tir.types[namespace_ty.as_usize()]
		else {
			self.tir.diagnostics.push(
				Diagnostic::error()
					.with_message("type arguments are not supported here")
					.with_label(Label::primary(resolve_context.file_id, span)),
			);
			return TypeIndex::ERROR;
		};
		let Some(symbol_kind) = self.tir.namespaces[namespace_idx as usize]
			.symbols
			.get(&(SymbolNamespace::Type, last.ident.inner))
			.copied()
		else {
			self.tir.diagnostics.push(
				Diagnostic::error()
					.with_message("type arguments are not supported here")
					.with_label(Label::primary(resolve_context.file_id, span)),
			);
			return TypeIndex::ERROR;
		};
		let mut resolved_args: Vec<TypeIndex> =
			Vec::with_capacity(last.type_args.len());
		for arg in last.type_args.iter() {
			resolved_args.push(self.resolve_type(resolve_context, scope, arg));
		}
		self.resolve_generic_type_application(
			resolve_context,
			symbol_kind,
			&resolved_args,
			last.ident.span,
		)
	}

	fn register_tagged_items(
		&mut self,
		def_id: ast::DefId,
		attrs: &[ItemAttribute],
	) {
		for attr in attrs {
			if let ItemAttribute::Tag(key) = attr {
				self.tir.tagged_items.insert(*key, def_id);
			}
		}
	}

	// TODO: this silently drops unrecognized attribute names/values (the
	// `_ => None` arm below) and never checks whether a resolved attribute
	// is actually valid on the item kind it was attached to (e.g.
	// `#[fixed_layout]` on a function, or `#[intrinsic]` on a struct) or
	// whether the same attribute appears more than once on one item.
	// Add validation + diagnostics for unknown attributes, attributes used
	// on the wrong item kind, and duplicates once this needs to be correct
	// rather than best-effort.
	fn resolve_attributes(
		&mut self,
		attrs: &[ast::Attribute],
	) -> Box<[ItemAttribute]> {
		attrs
			.iter()
			.filter_map(|a| {
				match (&a.value, self.interner.resolve(a.name.inner)) {
					(ast::AttributeValue::Word, Some("inline")) => {
						Some(ItemAttribute::Inline)
					}
					(ast::AttributeValue::Word, Some("intrinsic")) => {
						Some(ItemAttribute::Intrinsic)
					}
					(ast::AttributeValue::Word, Some("fixed_layout")) => {
						Some(ItemAttribute::FixedLayout)
					}
					(ast::AttributeValue::NameValue(value), Some("tag")) => {
						let raw =
							self.interner.resolve(value.inner).unwrap_or("");
						let key =
							self.interner.get_or_intern(unescape_string(raw));
						Some(ItemAttribute::Tag(key))
					}
					_ => None,
				}
			})
			.collect()
	}

	/// Returns the type params owned directly by `owner` (not including any
	/// params inherited from a parent impl block).
	fn owner_type_params(&self, owner: TypeParamOwner) -> &[TypeParamInfo] {
		match owner {
			TypeParamOwner::ImplBlock(block_idx) => {
				&self.tir.generic_impl_list[block_idx as usize].type_params
			}
			TypeParamOwner::Function(id) => {
				let func_index = self.tir.expect_function_index(id);
				&self.tir.functions[func_index as usize].type_params
			}
			TypeParamOwner::Struct(id) => {
				let struct_index = self.tir.expect_struct_index(id);
				&self.tir.structs[struct_index as usize].type_params
			}
			TypeParamOwner::Trait(trait_index) => std::slice::from_ref(
				&self.tir.traits[trait_index as usize].self_type_param,
			),
			TypeParamOwner::TypeAlias(id) => {
				let alias_index = self.tir.expect_type_alias_index(id);
				&self.tir.type_aliases[alias_index as usize].type_params
			}
		}
	}

	fn inherited_type_param_count(&self, owner: TypeParamOwner) -> usize {
		match owner {
			TypeParamOwner::Function(id) => {
				self.tir.function_index(id).map_or(0, |idx| {
					self.tir.functions[idx as usize].inherited_type_param_count
				})
			}
			_ => 0,
		}
	}

	fn type_param_bounds(&self, ty: TypeIndex) -> &[TraitBound] {
		let Type::TypeParam { owner, param_index } =
			self.tir.types[ty.as_usize()]
		else {
			return &[];
		};
		self.tir
			.type_param_info(owner, param_index as usize)
			.bounds
			.traits
			.as_ref()
	}

	fn record_type_param_access(
		&mut self,
		owner: TypeParamOwner,
		param_index: u32,
		span: SourceSpan,
	) {
		self.tir
			.type_param_info_mut(owner, param_index as usize)
			.accesses
			.push(span);
	}

	fn get_symbol_location(&self, symbol: SymbolKind) -> SourceSpan {
		match symbol {
			SymbolKind::Function { func_index } => {
				let func = &self.tir.functions[func_index as usize];
				SourceSpan::new(func.file_id, func.name.span)
			}
			SymbolKind::Global { global_index } => {
				let global = &self.tir.globals[global_index as usize];
				SourceSpan::new(global.file_id, global.name.span)
			}
			SymbolKind::Const { const_index } => {
				let const_ = &self.tir.constants[const_index as usize];
				SourceSpan::new(const_.file_id, const_.name.span)
			}
			SymbolKind::Enum { enum_index } => {
				let enum_ = &self.tir.enums[enum_index as usize];
				SourceSpan::new(enum_.file_id, enum_.name.span)
			}
			SymbolKind::Struct { struct_index } => {
				let s = &self.tir.structs[struct_index as usize];
				SourceSpan::new(s.file_id, s.name.span)
			}
			SymbolKind::Module { namespace_idx } => {
				match self.tir.namespaces[namespace_idx as usize].declaration {
					ModuleDeclarationKind::Module(decl_idx) => {
						let decl = &self.tir.module_decls[decl_idx as usize];
						SourceSpan::new(decl.declaring_file_id, decl.name.span)
					}
					ModuleDeclarationKind::Import(import_idx) => {
						let decl = &self.tir.import_decls[import_idx as usize];
						SourceSpan::new(decl.file_id, decl.external_name.span)
					}
					ModuleDeclarationKind::Crate(_, file_id) => {
						SourceSpan::new(file_id, ast::TextSpan::new(0, 0))
					}
				}
			}
			SymbolKind::Trait { trait_index } => {
				let trait_ = &self.tir.traits[trait_index as usize];
				SourceSpan::new(trait_.file_id, trait_.name.span)
			}
			SymbolKind::TypeSet { typeset_index } => {
				let ts = &self.tir.typesets[typeset_index as usize];
				SourceSpan::new(ts.file_id, ts.name.span)
			}
			SymbolKind::Memory { memory_index, .. } => {
				let memory = &self.tir.memories[memory_index as usize];
				SourceSpan::new(memory.file_id, memory.name.span)
			}
			SymbolKind::TraitAssocType { trait_index, .. } => {
				let trait_ = &self.tir.traits[trait_index as usize];
				SourceSpan::new(trait_.file_id, trait_.name.span)
			}
			SymbolKind::TypeAlias { type_alias_index } => {
				let alias = &self.tir.type_aliases[type_alias_index as usize];
				SourceSpan::new(alias.file_id, alias.name.span)
			}
			// A `Pending` entry always has a stub already pushed by
			// `pre_scan_item` (every syntactic occurrence is unconditionally
			// registered there, duplicate or not), so its declaration span
			// is available via `item_lookup` even though its fields/value
			// haven't been resolved yet.
			SymbolKind::Pending(def_id) => {
				match self.tir.item_lookup[&def_id] {
					ItemIndex::Function(idx) => {
						let f = &self.tir.functions[idx as usize];
						SourceSpan::new(f.file_id, f.name.span)
					}
					ItemIndex::Global(idx) => {
						let g = &self.tir.globals[idx as usize];
						SourceSpan::new(g.file_id, g.name.span)
					}
					ItemIndex::Memory(idx) => {
						let m = &self.tir.memories[idx as usize];
						SourceSpan::new(m.file_id, m.name.span)
					}
					ItemIndex::Struct(idx) => {
						let s = &self.tir.structs[idx as usize];
						SourceSpan::new(s.file_id, s.name.span)
					}
					ItemIndex::Const(idx) => {
						let c = &self.tir.constants[idx as usize];
						SourceSpan::new(c.file_id, c.name.span)
					}
					ItemIndex::Enum(idx) => {
						let e = &self.tir.enums[idx as usize];
						SourceSpan::new(e.file_id, e.name.span)
					}
					ItemIndex::TypeAlias(idx) => {
						let a = &self.tir.type_aliases[idx as usize];
						SourceSpan::new(a.file_id, a.name.span)
					}
					ItemIndex::TypeSet(_)
					| ItemIndex::Trait(_)
					| ItemIndex::TraitImpl(_) => unreachable!(
						"these kinds never install a Pending symbol"
					),
				}
			}
		}
	}

	/// Phase 1: registers every named item into `ast_nodes` without resolving
	/// types.
	fn pre_scan_item(
		&mut self,
		file_id: FileId,
		namespace: Option<NamespaceIndex>,
		item: &'ast ast::Item,
	) {
		match item {
			ast::Item::Function {
				id,
				signature,
				attributes,
				pub_span,
				..
			}
			| ast::Item::FunctionDeclaration {
				id,
				signature,
				attributes,
				pub_span,
				..
			} => {
				self.claim_name_binding(
					namespace,
					(SymbolNamespace::Value, signature.name.inner),
					*id,
					SourceSpan::new(file_id, signature.name.span),
				);
				let attributes = self.resolve_attributes(attributes);
				self.register_tagged_items(*id, &attributes);
				let func_index = self.tir.functions.len() as u32;
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::Function(func_index));
				self.tir.functions.push(Function {
					id: *id,
					file_id,
					namespace,
					parent: None,
					body: None,
					type_params: signature
						.type_params
						.iter()
						.map(|tp| {
							TypeParamInfo::new(tp.name.inner, tp.name.span)
						})
						.collect(),
					type_param_parent: None,
					inherited_type_param_count: 0,
					pub_span: *pub_span,
					signature_index: TypeIndex::ERROR,
					name: signature.name,
					accesses: Vec::new(),
					params: Box::new([]),
					result: None,
					attributes,
				});
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::Function { item },
				});
			}
			ast::Item::Global {
				id,
				pub_span,
				mut_span,
				name,
				..
			} => {
				self.claim_name_binding(
					namespace,
					(SymbolNamespace::Value, name.inner),
					*id,
					SourceSpan::new(file_id, name.span),
				);
				let global_index = self.tir.globals.len() as u32;
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::Global(global_index));
				self.tir.globals.push(Global {
					id: *id,
					file_id,
					namespace,
					value: None,
					name: *name,
					ty: ast::Spanned {
						inner: TypeIndex::ERROR,
						span: name.span,
					},
					pub_span: *pub_span,
					mut_span: *mut_span,
					accesses: Vec::new(),
				});
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::Global { item },
				});
			}
			ast::Item::Struct {
				id,
				pub_span,
				attributes,
				name,
				type_params,
				..
			} => {
				self.claim_name_binding(
					namespace,
					(SymbolNamespace::Type, name.inner),
					*id,
					SourceSpan::new(file_id, name.span),
				);
				let struct_index = self.tir.structs.len() as u32;
				let self_type = self.intern_type(Type::Struct {
					struct_index,
					args: Box::new([]),
				});
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::Struct(struct_index));
				let attributes = self.resolve_attributes(attributes);
				self.tir.structs.push(Struct {
					id: *id,
					file_id,
					namespace,
					pub_span: *pub_span,
					name: *name,
					type_params: type_params
						.iter()
						.map(|tp| {
							TypeParamInfo::new(tp.name.inner, tp.name.span)
						})
						.collect(),
					self_type,
					attributes,
					fields: Box::new([]),
					lookup: HashMap::new(),
					accesses: Vec::new(),
				});
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::Struct { item },
				});
			}
			ast::Item::Enum {
				id, pub_span, name, ..
			} => {
				self.claim_name_binding(
					namespace,
					(SymbolNamespace::Type, name.inner),
					*id,
					SourceSpan::new(file_id, name.span),
				);
				let enum_index = self.tir.enums.len() as u32;
				let self_type = self.intern_type(Type::Enum { enum_index });
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::Enum(enum_index));
				self.tir.enums.push(Enum {
					id: *id,
					file_id,
					namespace,
					pub_span: *pub_span,
					name: *name,
					repr_type: TypeIndex::ERROR,
					self_type,
					variants: Box::new([]),
					variant_lookup: HashMap::new(),
					accesses: Vec::new(),
				});
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::Enum { item },
				});
			}
			ast::Item::TypeAlias {
				id,
				pub_span,
				name,
				type_params,
				..
			} => {
				self.claim_name_binding(
					namespace,
					(SymbolNamespace::Type, name.inner),
					*id,
					SourceSpan::new(file_id, name.span),
				);
				let type_alias_index = self.tir.type_aliases.len() as u32;
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::TypeAlias(type_alias_index));
				self.tir.type_aliases.push(TypeAlias {
					id: *id,
					file_id,
					namespace,
					pub_span: *pub_span,
					name: *name,
					type_params: type_params
						.iter()
						.map(|tp| {
							TypeParamInfo::new(tp.name.inner, tp.name.span)
						})
						.collect(),
					template: TypeIndex::ERROR,
					accesses: Vec::new(),
				});
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::TypeAlias { item },
				});
			}
			ast::Item::Memory { id, name, .. } => {
				self.claim_name_binding(
					namespace,
					(SymbolNamespace::Type, name.inner),
					*id,
					SourceSpan::new(file_id, name.span),
				);
				self.claim_name_binding(
					namespace,
					(SymbolNamespace::Value, name.inner),
					*id,
					SourceSpan::new(file_id, name.span),
				);
				let memory_index = self.tir.memories.len() as u32;
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::Memory(memory_index));
				self.tir.memories.push(Memory {
					id: *id,
					file_id,
					name: *name,
					kind: TypeIndex::ERROR,
					min_pages: None,
					max_pages: None,
				});
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::Memory { item },
				});
			}
			ast::Item::Const {
				id, pub_span, name, ..
			} => {
				self.claim_name_binding(
					namespace,
					(SymbolNamespace::Value, name.inner),
					*id,
					SourceSpan::new(file_id, name.span),
				);
				let const_index = self.tir.constants.len() as ConstIndex;
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::Const(const_index));
				self.tir.constants.push(Constant {
					id: *id,
					file_id,
					namespace,
					parent: None,
					pub_span: *pub_span,
					name: *name,
					ty: ast::Spanned {
						inner: TypeIndex::ERROR,
						span: name.span,
					},
					value: None,
					const_value: None,
					accesses: Vec::new(),
				});
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::Const { item },
				});
			}
			ast::Item::Module {
				name,
				items,
				pub_span,
			} => {
				let namespace_index =
					self.ensure_module(file_id, namespace, *name, *pub_span);
				for child in items.iter() {
					self.pre_scan_item(
						file_id,
						Some(namespace_index),
						&child.inner.inner,
					);
				}
			}
			ast::Item::ModuleDeclaration { name, pub_span } => {
				self.ensure_module(file_id, namespace, *name, *pub_span);
			}
			ast::Item::Trait {
				id, name, items, ..
			} => {
				let trait_key = (SymbolNamespace::Type, name.inner);
				let existing_direct = if let Some(idx) = namespace {
					self.tir.namespaces[idx as usize].symbols.get(&trait_key)
				} else {
					self.symbol_lookup.get(&trait_key)
				};
				if let Some(existing) = existing_direct
					.filter(|k| !matches!(k, SymbolKind::Pending(_)))
					.cloned()
				{
					let name_str = self.interner.resolve(name.inner).unwrap();
					let first_definition = self.get_symbol_location(existing);
					self.tir.diagnostics.push(report_duplicate_definition(
						DuplicateDefinitionDiagnostic {
							name: name_str,
							namespace: SymbolNamespace::Type,
							first_definition,
							second_definition: SourceSpan::new(
								file_id, name.span,
							),
						},
					));
				}

				let trait_index = self.tir.traits.len() as u32;
				let mut member_ids: Vec<ast::DefId> =
					Vec::with_capacity(items.len());
				for trait_item in items.iter() {
					match &trait_item.inner.inner {
						ast::TraitItem::Function { id, .. } => {
							self.ast_nodes.push(AstEntry {
								def_id: *id,
								file_id,
								namespace,
								node: AstNodeRef::TraitFunction {
									trait_index,
									item: &trait_item.inner.inner,
								},
							});
							member_ids.push(*id);
						}
						ast::TraitItem::Const { id, .. } => {
							self.ast_nodes.push(AstEntry {
								def_id: *id,
								file_id,
								namespace,
								node: AstNodeRef::TraitConst {
									trait_index,
									item: &trait_item.inner.inner,
								},
							});
							member_ids.push(*id);
						}
						ast::TraitItem::AssociatedType { id, name, .. } => {
							self.insert_symbol(
								namespace,
								(SymbolNamespace::Type, name.inner),
								SymbolKind::Pending(*id),
							);
							self.ast_nodes.push(AstEntry {
								def_id: *id,
								file_id,
								namespace,
								node: AstNodeRef::TraitAssociatedType {
									trait_index,
									item: &trait_item.inner.inner,
								},
							});
							member_ids.push(*id);
						}
					}
				}

				let self_name_sym = self.interner.get_or_intern("Self");
				self.tir.traits.push(Trait {
					id: *id,
					file_id,
					namespace,
					name: *name,
					self_type_param: TypeParamInfo {
						name: self_name_sym,
						name_span: name.span,
						bounds: Bounds {
							traits: Box::new([TraitBound {
								trait_index,
								bindings: Box::new([]),
							}]),
							typeset: None,
						},
						accesses: Vec::new(),
					},
					supertraits: Vec::new(),
					members: HashMap::new(),
					assoc_types: HashMap::new(),
					member_ids,
					supertrait_bindings: HashMap::new(),
					accesses: Vec::new(),
				});
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::Trait(trait_index));
				self.insert_symbol(
					namespace,
					(SymbolNamespace::Type, name.inner),
					SymbolKind::Trait { trait_index },
				);
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::Trait { trait_index, item },
				});
			}
			ast::Item::Impl {
				id: impl_id,
				type_params,
				target,
				items,
			} => {
				if type_params.is_empty() {
					// Concrete impl — existing path.
					for impl_item in items.iter() {
						match &impl_item.inner.inner {
							ast::ImplItem::Method { id, .. } => {
								self.ast_nodes.push(AstEntry {
									def_id: *id,
									file_id,
									namespace,
									node: AstNodeRef::ImplMethod {
										impl_target: target,
										item: &impl_item.inner.inner,
									},
								});
							}
							ast::ImplItem::Const { id, .. } => {
								self.ast_nodes.push(AstEntry {
									def_id: *id,
									file_id,
									namespace,
									node: AstNodeRef::ImplConst {
										impl_target: target,
										item: &impl_item.inner.inner,
									},
								});
							}
							ast::ImplItem::AssociatedType { name, .. } => {
								self.tir.diagnostics.push(
									report_associated_type_in_inherent_impl(
										SourceSpan::new(file_id, name.span),
									),
								);
							}
						}
					}
				} else {
					// Generic impl — allocate the block with type params pre-populated,
					// register a dedicated init entry (resolves bounds + target), then
					// register each method referencing the block's AST id.
					let block_index = self.tir.generic_impl_list.len() as u32;
					self.tir.generic_impl_list.push(GenericImplBlock {
						id: *impl_id,
						file_id,
						type_params: type_params
							.iter()
							.map(|tp| {
								TypeParamInfo::new(tp.name.inner, tp.name.span)
							})
							.collect(),
						target: TypeIndex::ERROR,
						members: HashMap::new(),
					});
					self.ast_nodes.push(AstEntry {
						def_id: *impl_id,
						file_id,
						namespace,
						node: AstNodeRef::GenericImplBlock {
							impl_type_params: type_params,
							impl_target: target,
							block_index,
						},
					});
					for impl_item in items.iter() {
						match &impl_item.inner.inner {
							ast::ImplItem::Method { id, .. } => {
								self.ast_nodes.push(AstEntry {
									def_id: *id,
									file_id,
									namespace,
									node: AstNodeRef::GenericImplMethod {
										block_id: *impl_id,
										item: &impl_item.inner.inner,
										block_index,
									},
								});
							}
							ast::ImplItem::Const { .. }
							| ast::ImplItem::AssociatedType { .. } => {
								// TODO: support consts / associated types in
								// generic impls
							}
						}
					}
				}
			}
			ast::Item::Import {
				module: import_module_name,
				alias,
				entries,
			} => {
				// Imports are processed eagerly: their signatures depend only on
				// primitive types or previously-registered stdlib types.
				let import_decl_index = self.tir.import_decls.len() as u32;
				let external_name = {
					let s = self
						.interner
						.resolve(import_module_name.inner)
						.unwrap();
					let unquoted = unescape_string(s);
					Spanned {
						inner: self.interner.get_or_intern(&unquoted),
						span: import_module_name.span,
					}
				};
				if alias.is_none() {
					self.tir.diagnostics.push(report_missing_import_alias(
						SourceSpan::new(file_id, import_module_name.span),
					));
				}
				let module_sym = match alias {
					Some(a) => a.inner,
					None => external_name.inner,
				};
				let namespace_idx = self.tir.namespaces.len() as u32;
				let decl_idx = self.tir.import_decls.len() as u32;
				self.tir.namespaces.push(ModuleNamespace {
					name: module_sym,
					parent: namespace,
					declaration: ModuleDeclarationKind::Import(decl_idx),
					symbols: HashMap::new(),
					wildcard_imports: Vec::new(),
					accesses: Vec::new(),
				});
				self.insert_symbol(
					namespace,
					(SymbolNamespace::Type, module_sym),
					SymbolKind::Module { namespace_idx },
				);
				for entry in entries.iter() {
					match &entry.inner.inner.declaration {
						ast::ImportDeclaration::Function { id, .. } => {
							self.ast_nodes.push(AstEntry {
								def_id: *id,
								file_id,
								namespace,
								node: AstNodeRef::ImportedFunction {
									import_module_index: import_decl_index,
									decl: &entry.inner.inner.declaration,
								},
							});
						}
						ast::ImportDeclaration::Global { id, name, .. } => {
							self.insert_symbol(
								namespace,
								(SymbolNamespace::Value, name.inner),
								SymbolKind::Pending(*id),
							);
							self.ast_nodes.push(AstEntry {
								def_id: *id,
								file_id,
								namespace,
								node: AstNodeRef::ImportedGlobal {
									import_module_index: import_decl_index,
									decl: &entry.inner.inner.declaration,
								},
							});
						}
						ast::ImportDeclaration::Memory { id, name, .. } => {
							self.insert_symbol(
								namespace,
								(SymbolNamespace::Type, name.inner),
								SymbolKind::Pending(*id),
							);
							self.insert_symbol(
								namespace,
								(SymbolNamespace::Value, name.inner),
								SymbolKind::Pending(*id),
							);
							self.ast_nodes.push(AstEntry {
								def_id: *id,
								file_id,
								namespace,
								node: AstNodeRef::Memory { item },
							});
						}
					}
				}
				self.tir.import_decls.push(ImportDecl {
					namespace_idx,
					file_id,
					external_name,
					internal_name: *alias,
					lookup: HashMap::new(),
				});
			}
			ast::Item::Use { path, pub_span: _ } => {
				// Resolve the path to a namespace index and register it as a
				// wildcard import on the current namespace.  Symbols are looked
				// up lazily via `lookup_global_symbol` — no copying needed.
				// Each resolved segment gets an access recorded for IDE navigation.
				let mut current_ns: Option<NamespaceIndex> = None;
				let mut resolved = true;
				for segment in path.iter() {
					let key = (SymbolNamespace::Type, segment.inner);
					let kind = if let Some(idx) = current_ns {
						self.tir.namespaces[idx as usize]
							.symbols
							.get(&key)
							.copied()
					} else {
						self.symbol_lookup.get(&key).copied()
					};
					match kind {
						Some(SymbolKind::Module { namespace_idx }) => {
							self.tir.namespaces[namespace_idx as usize]
								.accesses
								.push(SourceSpan::new(file_id, segment.span));
							current_ns = Some(namespace_idx);
						}
						_ => {
							resolved = false;
							break;
						}
					}
				}
				if resolved {
					if let Some(source_ns) = current_ns {
						if let Some(ns_idx) = namespace {
							self.tir.namespaces[ns_idx as usize]
								.wildcard_imports
								.push(source_ns);
						} else {
							self.root_wildcard_imports.push(source_ns);
						}
					}
				}
			}
			ast::Item::Export { .. } => {} // handled during build pass
			ast::Item::TypeSet {
				id,
				name,
				pub_span,
				attributes,
				..
			} => {
				let resolved_attrs = self.resolve_attributes(attributes);
				self.register_tagged_items(*id, &resolved_attrs);
				let typeset_index = self.tir.typesets.len() as TypesetIndex;
				self.tir.typesets.push(TypeSet {
					id: *id,
					file_id,
					namespace,
					name: *name,
					pub_span: *pub_span,
					members: Box::new([]),
					intersection_range: IntegerRange::widest(),
					accesses: Vec::new(),
				});
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::TypeSet(typeset_index));
				self.insert_symbol(
					namespace,
					(SymbolNamespace::Type, name.inner),
					SymbolKind::TypeSet { typeset_index },
				);
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::TypeSet {
						typeset_index,
						item,
					},
				});
			}
			ast::Item::ImplTrait { id, items, .. } => {
				self.ast_nodes.push(AstEntry {
					def_id: *id,
					file_id,
					namespace,
					node: AstNodeRef::ImplTraitBlock { item },
				});
				for mi in items.iter() {
					match &mi.inner.inner {
						ast::ImplItem::Method { id: method_id, .. } => {
							self.ast_nodes.push(AstEntry {
								def_id: *method_id,
								file_id,
								namespace,
								node: AstNodeRef::ImplTraitMethod {
									parent_id: *id,
									item: &mi.inner.inner,
								},
							});
						}
						ast::ImplItem::Const { id: const_id, .. } => {
							self.ast_nodes.push(AstEntry {
								def_id: *const_id,
								file_id,
								namespace,
								node: AstNodeRef::ImplTraitConst {
									parent_id: *id,
									item: &mi.inner.inner,
								},
							});
						}
						ast::ImplItem::AssociatedType {
							id: type_id, ..
						} => {
							self.ast_nodes.push(AstEntry {
								def_id: *type_id,
								file_id,
								namespace,
								node: AstNodeRef::ImplTraitAssociatedType {
									parent_id: *id,
									item: &mi.inner.inner,
								},
							});
						}
					}
				}
			}
		}
	}

	/// Resolves the signature of `def_id`. Idempotent; detects cycles via
	/// `sig_state`.
	fn ensure_signature(&mut self, def_id: ast::DefId) {
		let node_idx = {
			let entry = self.sig_state.get_mut(&def_id).unwrap();
			match entry.state {
				ComputeState::Done | ComputeState::InProgress => return,
				ComputeState::Pending => entry.state = ComputeState::InProgress,
			}
			entry.node_idx
		};
		let AstEntry {
			file_id,
			namespace,
			node,
			..
		} = self.ast_nodes[node_idx].clone();

		let resolve_context = ResolveContext::new(file_id, namespace);

		match node {
			AstNodeRef::Struct { item } => {
				let (id, name, ast_type_params, fields) = match item {
					ast::Item::Struct {
						id,
						name,
						type_params,
						fields,
						..
					} => (id, name, type_params, fields),
					_ => unreachable!(),
				};
				let struct_index = self.tir.expect_struct_index(*id);
				// Bind the name now, before resolving fields, exactly like
				// the pre-refactor code did: this lets a self-referential
				// pointer field (e.g. `*Node`) resolve directly instead of
				// recursing into `ensure_signature` again. Only do this if
				// this occurrence still holds its own `Pending` slot — if
				// an earlier duplicate already claimed the name (or, for a
				// duplicate itself, if it never held the slot to begin
				// with), skip the bind: this struct still gets its fields
				// fully resolved below, it just never becomes referenceable.
				let key = (SymbolNamespace::Type, name.inner);
				if matches!(
					self.direct_scope_lookup(resolve_context.namespace, key),
					Some(SymbolKind::Pending(pending_id)) if pending_id == *id
				) {
					self.insert_symbol(
						resolve_context.namespace,
						key,
						SymbolKind::Struct { struct_index },
					);
				}
				// Resolve bounds now that the struct is registered and names are in TIR.
				self.resolve_type_param_bounds(
					resolve_context,
					TypeParamOwner::Struct(*id),
					None,
					ast_type_params,
				);
				let field_scope = if ast_type_params.is_empty() {
					None
				} else {
					Some(GenericScope {
						owner: TypeParamOwner::Struct(*id),
						self_type: None,
					})
				};

				// Resolve all field types. Referenced structs that haven't been
				// seen yet are pulled in demand-driven via ensure_signature.
				let field_count = fields.len();
				let mut seen_fields: HashMap<SymbolU32, ast::TextSpan> =
					HashMap::with_capacity(field_count);
				let mut tir_fields: Vec<StructField> =
					Vec::with_capacity(field_count);
				let mut field_lookup: HashMap<SymbolU32, usize> =
					HashMap::with_capacity(field_count);

				for f in fields.iter() {
					let field = &f.inner.inner;
					let sym = field.name.inner;
					if let Some(&first_span) = seen_fields.get(&sym) {
						let fname =
							self.interner.resolve(sym).unwrap().to_string();
						self.tir.diagnostics.push(
							report_duplicate_struct_field(
								&fname,
								SourceSpan::new(
									resolve_context.file_id,
									first_span,
								),
								SourceSpan::new(
									resolve_context.file_id,
									field.name.span,
								),
							),
						);
						continue;
					}
					let field_ty = self.resolve_signature_type(
						resolve_context,
						field_scope,
						&field.ty,
					);
					seen_fields.insert(sym, field.name.span);
					let idx = tir_fields.len();
					field_lookup.insert(sym, idx);
					tir_fields.push(StructField {
						name: field.name,
						ty: Spanned {
							inner: field_ty,
							span: field.ty.span,
						},
						pub_span: field.pub_span,
						accesses: Vec::new(),
					});
				}

				// Fill in the placeholder now that all field types are resolved.
				self.tir.structs[struct_index as usize].fields =
					tir_fields.into_boxed_slice();
				self.tir.structs[struct_index as usize].lookup = field_lookup;

				// Check for direct (non-pointer) self-recursion. Cycles through
				// generic struct instantiation are not caught here — see TODO in
				// mir::Builder::ensure_aggregate_for_struct.
				self.check_struct_fields_for_direct_recursion(
					struct_index,
					SourceSpan::new(resolve_context.file_id, name.span),
				);
			}
			AstNodeRef::TypeAlias { item } => {
				let (id, name, ast_type_params, ty_expr) = match item {
					ast::Item::TypeAlias {
						id,
						name,
						type_params,
						ty,
						..
					} => (id, name, type_params, ty),
					_ => unreachable!(),
				};
				let type_alias_index = self.tir.expect_type_alias_index(*id);

				// Deliberately NOT calling insert_symbol yet: the symbol table
				// still holds SymbolKind::Pending(*id) while the RHS resolves,
				// so a self-reference (`type A = A;`) hits the InProgress
				// cyclic-dependency guard in resolve_type_identifier instead of
				// resolving through a half-built alias. Aliases are transparent
				// with no indirection to break a cycle, unlike struct fields.
				self.resolve_type_param_bounds(
					resolve_context,
					TypeParamOwner::TypeAlias(*id),
					None,
					ast_type_params,
				);
				let scope = if ast_type_params.is_empty() {
					None
				} else {
					Some(GenericScope {
						owner: TypeParamOwner::TypeAlias(*id),
						self_type: None,
					})
				};
				let template = self.resolve_signature_type(
					resolve_context,
					scope,
					ty_expr,
				);
				self.tir.type_aliases[type_alias_index as usize].template =
					template;

				// Bind the name only if this occurrence still holds its own
				// `Pending` slot — see the identical comment on the Struct
				// branch.
				let key = (SymbolNamespace::Type, name.inner);
				if matches!(
					self.direct_scope_lookup(resolve_context.namespace, key),
					Some(SymbolKind::Pending(pending_id)) if pending_id == *id
				) {
					self.insert_symbol(
						resolve_context.namespace,
						key,
						SymbolKind::TypeAlias { type_alias_index },
					);
				}
			}
			AstNodeRef::Enum { item } => {
				if let ast::Item::Enum {
					id,
					name,
					repr,
					variants,
					..
				} = item
				{
					let enum_index = self.tir.expect_enum_index(*id);
					self.build_enum(
						resolve_context,
						name,
						repr.as_deref(),
						variants,
						enum_index,
					);

					// Bind the name only if this occurrence still holds its
					// own `Pending` slot — see the identical comment on the
					// Struct branch.
					let key = (SymbolNamespace::Type, name.inner);
					if matches!(
						self.direct_scope_lookup(resolve_context.namespace, key),
						Some(SymbolKind::Pending(pending_id)) if pending_id == *id
					) {
						self.insert_symbol(
							resolve_context.namespace,
							key,
							SymbolKind::Enum { enum_index },
						);
					}
				}
			}
			AstNodeRef::Function { item } => match item {
				ast::Item::Function { id, signature, .. }
				| ast::Item::FunctionDeclaration { id, signature, .. } => {
					let func_index = self.tir.expect_function_index(*id);
					self.resolve_type_param_bounds(
						resolve_context,
						TypeParamOwner::Function(*id),
						None,
						&signature.type_params,
					);
					let signature_scope = GenericScope {
						owner: TypeParamOwner::Function(*id),
						self_type: None,
					};
					let (params, result) = self.build_function_signature(
						resolve_context,
						Some(signature_scope),
						signature,
					);
					let signature_index = self.intern_function(&params, result);
					let func = &mut self.tir.functions[func_index as usize];
					func.params = params;
					func.result = result;
					func.signature_index = signature_index;

					// Bind the name only if this occurrence still holds its
					// own `Pending` slot — see the identical comment on the
					// Struct branch.
					let key = (SymbolNamespace::Value, signature.name.inner);
					if matches!(
						self.direct_scope_lookup(resolve_context.namespace, key),
						Some(SymbolKind::Pending(pending_id)) if pending_id == *id
					) {
						self.insert_symbol(
							resolve_context.namespace,
							key,
							SymbolKind::Function { func_index },
						);
					}
				}
				_ => {}
			},
			AstNodeRef::ImplConst {
				impl_target, item, ..
			} => {
				if let ast::ImplItem::Const {
					id,
					name,
					ty,
					value,
					..
				} = item
				{
					let self_type =
						self.resolve_type(resolve_context, None, impl_target);
					let self_scope = GenericScope {
						owner: TypeParamOwner::Function(*id),
						self_type: Some(self_type),
					};
					let resolved_ty = match ty {
						Some(te) => self.resolve_type(
							resolve_context,
							Some(self_scope),
							te,
						),
						None => TypeIndex::ERROR,
					};
					if let Ok(value_expr) = self.build_const_context_expression(
						resolve_context,
						value,
						resolved_ty,
					) {
						let const_value =
							match self.eval_const_expr(&value_expr) {
								Ok(v) => Some(v),
								Err(_) => {
									self.tir.diagnostics.push(
										report_not_const_evaluatable(
											SourceSpan::new(
												resolve_context.file_id,
												value.span,
											),
										),
									);
									None
								}
							};
						let const_index =
							self.tir.constants.len() as ConstIndex;
						self.tir.constants.push(Constant {
							id: *id,
							file_id: resolve_context.file_id,
							namespace: resolve_context.namespace,
							parent: Some(ItemParent::Impl(self_type)),
							pub_span: None,
							name: *name,
							ty: ast::Spanned {
								inner: resolved_ty,
								span: name.span,
							},
							value: Some(Box::new(value_expr)),
							const_value,
							accesses: Vec::new(),
						});
						self.tir
							.item_lookup
							.insert(*id, ItemIndex::Const(const_index));
						self.tir
							.impl_members
							.entry(self_type)
							.or_default()
							.insert(
								name.inner,
								ImplEntry::AssociatedConst {
									id: *id,
									ty: resolved_ty,
								},
							);
					}
				}
			}
			AstNodeRef::ImplMethod {
				impl_target, item, ..
			} => {
				let self_type =
					self.resolve_type(resolve_context, None, impl_target);
				let self_symbol = self.interner.get_or_intern("self");

				// Process this specific method.
				if let ast::ImplItem::Method {
					id,
					pub_span,
					attributes,
					signature,
					..
				} = item
				{
					let self_scope = GenericScope {
						owner: TypeParamOwner::Function(*id),
						self_type: Some(self_type),
					};
					let mut params: Vec<FunctionParam> =
						Vec::with_capacity(signature.params.len());
					for p in signature.params.iter() {
						let is_self = p.inner.inner.name.inner == self_symbol;
						let ty = match &p.inner.inner.ty {
							Some(te) => {
								let resolved = self.resolve_type(
									resolve_context,
									Some(self_scope),
									te,
								);
								if is_self
									&& !self
										.is_valid_self_type(resolved, self_type)
								{
									self.tir.diagnostics.push(
										report_invalid_self_type(
											SourceSpan::new(
												resolve_context.file_id,
												te.span,
											),
											TypeFormatter::new(
												&self.tir,
												self.interner,
											),
											resolved,
										),
									);
								}
								Spanned {
									inner: resolved,
									span: te.span,
								}
							}
							None => Spanned {
								inner: if is_self {
									self_type
								} else {
									TypeIndex::ERROR
								},
								span: p.inner.inner.name.span,
							},
						};
						params.push(FunctionParam {
							mut_span: p.inner.inner.mut_span,
							name: p.inner.inner.name,
							ty,
						});
					}
					let params: Box<[FunctionParam]> =
						params.into_boxed_slice();
					let result = signature.result.as_ref().map(|r| Spanned {
						inner: self.resolve_type(
							resolve_context,
							Some(self_scope),
							r,
						),
						span: r.span,
					});
					let signature_index = self.intern_function(&params, result);
					let func_attrs = self.resolve_attributes(attributes);
					self.register_tagged_items(*id, &func_attrs);
					let func_index = self.tir.functions.len() as u32;
					let is_method = signature
						.params
						.first()
						.map(|p| p.inner.inner.name.inner == self_symbol)
						.unwrap_or(false);
					self.tir.functions.push(Function {
						id: *id,
						file_id: resolve_context.file_id,
						namespace: resolve_context.namespace,
						parent: Some(ItemParent::Impl(self_type)),
						body: None,
						type_params: Box::new([]),
						type_param_parent: None,
						inherited_type_param_count: 0,
						pub_span: *pub_span,
						signature_index,
						name: signature.name,
						accesses: Vec::new(),
						params,
						result,
						attributes: func_attrs,
					});
					self.tir
						.item_lookup
						.insert(*id, ItemIndex::Function(func_index));
					let method_name_str = self
						.interner
						.resolve(signature.name.inner)
						.unwrap_or("?")
						.to_string();
					let existing = self
						.tir
						.impl_members
						.get(&self_type)
						.and_then(|m| m.get(&signature.name.inner))
						.cloned();
					if let Some(
						ImplEntry::Method(prev) | ImplEntry::AssociatedFn(prev),
					) = existing
					{
						let prev_func = &self.tir.functions[prev as usize];
						let first = SourceSpan::new(
							prev_func.file_id,
							prev_func.name.span,
						);
						let second = SourceSpan::new(
							resolve_context.file_id,
							signature.name.span,
						);
						self.tir.diagnostics.push(report_duplicate_definition(
							DuplicateDefinitionDiagnostic {
								name: &method_name_str,
								namespace: SymbolNamespace::Value,
								first_definition: first,
								second_definition: second,
							},
						));
					}
					self.tir.impl_members.entry(self_type).or_default().insert(
						signature.name.inner,
						if is_method {
							ImplEntry::Method(func_index)
						} else {
							ImplEntry::AssociatedFn(func_index)
						},
					);
				}
			}
			AstNodeRef::GenericImplBlock {
				impl_type_params,
				impl_target,
				block_index,
			} => {
				// Resolve bounds for the impl block's own type params.
				self.resolve_type_param_bounds(
					resolve_context,
					TypeParamOwner::ImplBlock(block_index),
					None,
					impl_type_params,
				);
				// Resolve the target type.
				let scope = GenericScope {
					owner: TypeParamOwner::ImplBlock(block_index),
					self_type: None,
				};
				let self_type = self.resolve_type(
					resolve_context,
					Some(scope),
					impl_target,
				);

				let target = match &self.tir.types[self_type.as_usize()] {
					Type::TypeParam { .. } => {
						self.tir.diagnostics.push(
                            Diagnostic::error()
                                .with_code(DiagnosticCode::TypeMistmatch.code())
                                .with_message("no nominal type found for inherent implementation")
                                .with_label(Label::primary(
                                    resolve_context.file_id,
                                    impl_target.span,
                                ))
                                .with_note(
                                    "either implement a trait on it or create a newtype to wrap it instead",
                                ),
                        );
						TypeIndex::ERROR
					}
					Type::Pointer { .. } => {
						self.tir.diagnostics.push(
							Diagnostic::error()
								.with_code(DiagnosticCode::TypeMistmatch.code())
								.with_message(
									"cannot implement methods directly on a pointer type",
								)
								.with_label(Label::primary(
									resolve_context.file_id,
									impl_target.span,
								))
								.with_note(
									"use free functions in a `module` block instead",
								),
						);
						TypeIndex::ERROR
					}
					_ => self_type,
				};
				self.tir.generic_impl_list[block_index as usize].target =
					target;
			}
			AstNodeRef::GenericImplMethod {
				block_id,
				item,
				block_index,
			} => {
				// Ensure the impl block's bounds and target are resolved first.
				self.ensure_signature(block_id);

				let ast::ImplItem::Method {
					id,
					pub_span,
					attributes,
					signature,
					..
				} = item
				else {
					return;
				};

				// The impl block already has its bounds and target resolved.
				let self_type =
					self.tir.generic_impl_list[block_index as usize].target;
				let impl_count = self.tir.generic_impl_list
					[block_index as usize]
					.type_params
					.len();

				let func_attrs = self.resolve_attributes(attributes);
				self.register_tagged_items(*id, &func_attrs);
				let func_index = self.tir.functions.len() as u32;

				// Register the function with only its own (method-level) type params.
				// Impl-level params are inherited via type_param_parent.
				self.tir.functions.push(Function {
					id: *id,
					file_id: resolve_context.file_id,
					namespace: resolve_context.namespace,
					parent: Some(ItemParent::GenericImpl(block_index)),
					body: None,
					type_params: signature
						.type_params
						.iter()
						.map(|tp| {
							TypeParamInfo::new(tp.name.inner, tp.name.span)
						})
						.collect(),
					type_param_parent: Some(TypeParamOwner::ImplBlock(
						block_index,
					)),
					inherited_type_param_count: impl_count,
					pub_span: *pub_span,
					signature_index: TypeIndex::ERROR,
					name: signature.name,
					accesses: Vec::new(),
					params: Box::new([]),
					result: None,
					attributes: func_attrs,
				});
				self.tir
					.item_lookup
					.insert(*id, ItemIndex::Function(func_index));

				// Resolve the function's own param bounds. resolve_type_identifier
				// automatically walks up to ImplBlock when a name isn't found in own params.
				self.resolve_type_param_bounds(
					resolve_context,
					TypeParamOwner::Function(*id),
					None,
					&signature.type_params,
				);

				let self_symbol = self.interner.get_or_intern("self");
				let is_method = signature
					.params
					.first()
					.map(|p| p.inner.inner.name.inner == self_symbol)
					.unwrap_or(false);
				let all_scope = GenericScope {
					owner: TypeParamOwner::Function(*id),
					self_type: Some(self_type),
				};

				let mut params: Vec<FunctionParam> =
					Vec::with_capacity(signature.params.len());
				for p in signature.params.iter() {
					let is_self = p.inner.inner.name.inner == self_symbol;
					let ty = match &p.inner.inner.ty {
						Some(te) => {
							let resolved = self.resolve_type(
								resolve_context,
								Some(all_scope),
								te,
							);
							if is_self
								&& !self.is_valid_self_type(resolved, self_type)
							{
								self.tir.diagnostics.push(
									report_invalid_self_type(
										SourceSpan::new(
											resolve_context.file_id,
											te.span,
										),
										TypeFormatter::new(
											&self.tir,
											self.interner,
										),
										resolved,
									),
								);
							}
							Spanned {
								inner: resolved,
								span: te.span,
							}
						}
						None => Spanned {
							inner: if is_self {
								self_type
							} else {
								TypeIndex::ERROR
							},
							span: p.inner.inner.name.span,
						},
					};
					params.push(FunctionParam {
						mut_span: p.inner.inner.mut_span,
						name: p.inner.inner.name,
						ty,
					});
				}
				let params: Box<[FunctionParam]> = params.into_boxed_slice();
				let result = signature.result.as_ref().map(|r| Spanned {
					inner: self.resolve_type(
						resolve_context,
						Some(all_scope),
						r,
					),
					span: r.span,
				});
				let signature_index = self.intern_function(&params, result);
				let func = &mut self.tir.functions[func_index as usize];
				func.params = params;
				func.result = result;
				func.signature_index = signature_index;

				let entry = if is_method {
					ImplEntry::Method(func_index)
				} else {
					ImplEntry::AssociatedFn(func_index)
				};
				let method_name_str = self
					.interner
					.resolve(signature.name.inner)
					.unwrap_or("?")
					.to_string();
				let existing = self.tir.generic_impl_list[block_index as usize]
					.members
					.get(&signature.name.inner)
					.cloned();
				if let Some(
					ImplEntry::Method(prev) | ImplEntry::AssociatedFn(prev),
				) = existing
				{
					let prev_func = &self.tir.functions[prev as usize];
					let first =
						SourceSpan::new(prev_func.file_id, prev_func.name.span);
					let second = SourceSpan::new(
						resolve_context.file_id,
						signature.name.span,
					);
					self.tir.diagnostics.push(report_duplicate_definition(
						DuplicateDefinitionDiagnostic {
							name: &method_name_str,
							namespace: SymbolNamespace::Value,
							first_definition: first,
							second_definition: second,
						},
					));
				}
				self.tir.generic_impl_list[block_index as usize]
					.members
					.insert(signature.name.inner, entry);

				// Populate the O(1) dispatch index.
				if let Some(kind) = GenericImplTargetKind::from_type(
					&self.tir.types[self_type.as_usize()],
				) {
					self.tir.generic_impl_dispatch.insert(
						(kind, signature.name.inner),
						block_index as usize,
					);
				}
			}
			AstNodeRef::Trait {
				trait_index, item, ..
			} => {
				let (supertraits, trait_id, attributes) = match item {
					ast::Item::Trait {
						id,
						supertraits,
						attributes,
						..
					} => (supertraits, id, attributes),
					_ => unreachable!(),
				};
				let resolved_attrs = self.resolve_attributes(attributes);
				self.register_tagged_items(*trait_id, &resolved_attrs);

				let supertrait_bounds = if let Some(spanned) = supertraits {
					self.resolve_bounds(resolve_context, None, spanned)
				} else {
					Bounds::default()
				};

				if let (Some(spanned), true) =
					(supertraits, supertrait_bounds.typeset.is_some())
				{
					self.tir.diagnostics.push(
						Diagnostic::error()
							.with_code(DiagnosticCode::ExpectedTrait.code())
							.with_message(
								"expected a trait name, not a typeset",
							)
							.with_label(Label::primary(
								resolve_context.file_id,
								spanned.span,
							)),
					);
				}

				let resolved: Vec<TraitIndex> = supertrait_bounds
					.traits
					.iter()
					.map(|tb| tb.trait_index)
					.collect();

				let mut bindings: HashMap<(TraitIndex, SymbolU32), TypeIndex> =
					HashMap::new();
				for tb in supertrait_bounds.traits.iter() {
					for &(assoc_name, val_ty) in tb.bindings.iter() {
						bindings.insert((tb.trait_index, assoc_name), val_ty);
						if let Some(spanned) = supertraits {
							self.check_assoc_type_bounds(
								resolve_context.file_id,
								tb.trait_index,
								assoc_name,
								val_ty,
								spanned.span,
							);
						}
					}
				}
				self.tir.traits[trait_index as usize].supertraits = resolved;
				self.tir.traits[trait_index as usize].supertrait_bindings =
					bindings;
			}
			AstNodeRef::TypeSet {
				typeset_index,
				item,
				..
			} => {
				let members = match item {
					ast::Item::TypeSet { members, .. } => members,
					_ => unreachable!(),
				};

				let resolved_members: Box<[TypeIndex]> = members
					.iter()
					.filter_map(|m| {
						let ty =
							self.resolve_type(resolve_context, None, &m.inner);
						if !ty.is_integer() {
							self.tir.diagnostics.push(
								Diagnostic::error()
									.with_code(
										DiagnosticCode::TypesetMemberNotInteger
											.code(),
									)
									.with_message(
										"typeset member must be an integer type",
									)
									.with_label(
										Label::primary(
											resolve_context.file_id,
											m.inner.span,
										)
										.with_message(format!(
											"`{}` is not an integer type",
											TypeFormatter::new(
												&self.tir,
												self.interner
											)
											.display_type(ty)
											.unwrap_or_default()
										)),
									),
							);
							None
						} else {
							Some(ty)
						}
					})
					.collect();

				let intersection_range = resolved_members
					.iter()
					.filter_map(|&ty| IntegerRange::for_integer_type(ty))
					.fold(IntegerRange::widest(), IntegerRange::intersect);
				self.tir.typesets[typeset_index as usize].members =
					resolved_members;
				self.tir.typesets[typeset_index as usize].intersection_range =
					intersection_range;
			}
			AstNodeRef::TraitFunction {
				trait_index, item, ..
			} => {
				// Self is encoded as TypeParam{0} so default implementations can be
				// monomorphized: type_args[0] = concrete receiver type at the call site.
				let self_sym = self.interner.get_or_intern("self");
				if let ast::TraitItem::Function {
					id,
					attributes,
					signature,
					..
				} = item
				{
					// `Self` is owned by the trait; the function inherits it via
					// type_param_parent so type_params holds only explicit params.
					let self_type_param_idx =
						self.intern_type(Type::TypeParam {
							owner: TypeParamOwner::Trait(trait_index),
							param_index: 0,
						});
					let attributes = self.resolve_attributes(attributes);
					self.register_tagged_items(*id, &attributes);

					let func_index = self.tir.functions.len() as u32;
					self.tir.functions.push(Function {
						id: *id,
						file_id: resolve_context.file_id,
						namespace: resolve_context.namespace,
						parent: Some(ItemParent::Trait(trait_index)),
						body: None,
						pub_span: None,
						type_params: signature
							.type_params
							.iter()
							.map(|tp| {
								TypeParamInfo::new(tp.name.inner, tp.name.span)
							})
							.collect(),
						type_param_parent: Some(TypeParamOwner::Trait(
							trait_index,
						)),
						inherited_type_param_count: 1,
						signature_index: TypeIndex::ERROR,
						name: signature.name,
						accesses: Vec::new(),
						params: Box::new([]),
						result: None,
						attributes: attributes.clone(),
					});
					self.tir
						.item_lookup
						.insert(*id, ItemIndex::Function(func_index));
					self.resolve_type_param_bounds(
						resolve_context,
						TypeParamOwner::Function(*id),
						Some(self_type_param_idx),
						&signature.type_params,
					);
					let sig_scope = GenericScope {
						owner: TypeParamOwner::Function(*id),
						self_type: Some(self_type_param_idx),
					};
					let mut params: Vec<FunctionParam> =
						Vec::with_capacity(signature.params.len());
					for p in signature.params.iter() {
						let is_self = p.inner.inner.name.inner == self_sym;
						let ty = match &p.inner.inner.ty {
							Some(te) => Spanned {
								inner: self.resolve_type(
									resolve_context,
									Some(sig_scope),
									te,
								),
								span: te.span,
							},
							None => Spanned {
								inner: if is_self {
									self_type_param_idx
								} else {
									TypeIndex::ERROR
								},
								span: p.inner.inner.name.span,
							},
						};
						params.push(FunctionParam {
							mut_span: p.inner.inner.mut_span,
							name: p.inner.inner.name,
							ty,
						});
					}
					let result = signature.result.as_ref().map(|r| Spanned {
						inner: self.resolve_type(
							resolve_context,
							Some(sig_scope),
							r,
						),
						span: r.span,
					});
					let params: Box<[FunctionParam]> =
						params.into_boxed_slice();
					let sig_idx = self.intern_function(&params, result);
					let func = &mut self.tir.functions[func_index as usize];
					func.params = params;
					func.result = result;
					func.signature_index = sig_idx;
					self.tir.traits[trait_index as usize].members.insert(
						signature.name.inner,
						ImplEntry::Method(func_index),
					);
				}
			}
			AstNodeRef::TraitConst {
				trait_index, item, ..
			} => {
				// Self is a TypeParam owned by the trait so `Self::*mut u8` is valid.
				let self_type_param = self.intern_type(Type::TypeParam {
					owner: TypeParamOwner::Trait(trait_index),
					param_index: 0,
				});
				let self_scope = GenericScope {
					owner: TypeParamOwner::Trait(trait_index),
					self_type: Some(self_type_param),
				};
				if let ast::TraitItem::Const {
					id,
					name,
					ty,
					attributes,
				} = item
				{
					let ty_idx = self.resolve_type(
						resolve_context,
						Some(self_scope),
						ty,
					);
					let attributes = self.resolve_attributes(attributes);
					self.register_tagged_items(*id, &attributes);
					let const_index = self.tir.constants.len() as ConstIndex;
					self.tir.constants.push(Constant {
						id: *id,
						file_id: resolve_context.file_id,
						namespace: resolve_context.namespace,
						parent: Some(ItemParent::Trait(trait_index)),
						pub_span: None,
						name: *name,
						ty: Spanned {
							inner: ty_idx,
							span: ty.span,
						},
						value: None,
						const_value: None,
						accesses: Vec::new(),
					});
					self.tir
						.item_lookup
						.insert(*id, ItemIndex::Const(const_index));
					self.tir.traits[trait_index as usize].members.insert(
						name.inner,
						ImplEntry::AssociatedConst {
							id: *id,
							ty: ty_idx,
						},
					);
				}
			}
			AstNodeRef::Global { item } => {
				if let ast::Item::Global { name, ty, id, .. } = item {
					let global_index = self.tir.expect_global_index(*id);
					let (ty_idx, ty_span) = match ty {
						Some(ty) => (
							self.resolve_signature_type(
								resolve_context,
								None,
								ty,
							),
							ty.span,
						),
						None => {
							self.tir.diagnostics.push(
								report_type_annotation_required(
									SourceSpan::new(
										resolve_context.file_id,
										name.span,
									),
								),
							);
							(TypeIndex::ERROR, name.span)
						}
					};
					self.tir.globals[global_index as usize].ty = ast::Spanned {
						inner: ty_idx,
						span: ty_span,
					};

					// Bind the name only if this occurrence still holds its
					// own `Pending` slot — see the identical comment on the
					// Struct branch.
					let key = (SymbolNamespace::Value, name.inner);
					if matches!(
						self.direct_scope_lookup(resolve_context.namespace, key),
						Some(SymbolKind::Pending(pending_id)) if pending_id == *id
					) {
						self.insert_symbol(
							resolve_context.namespace,
							key,
							SymbolKind::Global { global_index },
						);
					}
				}
			}
			AstNodeRef::Memory { item } => {
				if let ast::Item::Memory {
					name,
					kind,
					id,
					config,
				} = item
				{
					let kind_bounds =
						self.resolve_bounds(resolve_context, None, kind);
					let trait_index = match (
						kind_bounds.traits.as_ref(),
						kind_bounds.typeset,
					) {
						([tb], None) => tb.trait_index,
						_ => {
							self.tir.diagnostics.push(
								report_invalid_memory_kind(SourceSpan::new(
									resolve_context.file_id,
									kind.span,
								)),
							);
							self.register_placeholder_memory(
								resolve_context,
								*id,
								name,
							);
							self.sig_state.get_mut(&def_id).unwrap().state =
								ComputeState::Done;
							return;
						}
					};

					let mut bindings: HashMap<SymbolU32, TypeIndex> =
						HashMap::new();
					if let ast::BoundExpression::WithBindings {
						bindings: where_bindings,
						..
					} = &kind.inner
					{
						for binding in where_bindings.iter() {
							let val_ty = self.resolve_type(
								resolve_context,
								None,
								&binding.ty,
							);
							bindings.insert(binding.name.inner, val_ty);
							if let Some(at) = self.tir.traits
								[trait_index as usize]
								.assoc_types
								.get_mut(&binding.name.inner)
							{
								at.accesses.push(SourceSpan::new(
									resolve_context.file_id,
									binding.name.span,
								));
							}
							self.check_assoc_type_bounds(
								resolve_context.file_id,
								trait_index,
								binding.name.inner,
								val_ty,
								binding.ty.span,
							);
						}
					}

					let size_symbol = self.interner.get_or_intern("Size");
					let memory_kind = match bindings.get(&size_symbol).copied()
					{
						Some(ty)
							if ty == TypeIndex::U32 || ty == TypeIndex::U64 =>
						{
							ty
						}
						_ => {
							self.tir.diagnostics.push(
								report_invalid_memory_kind(SourceSpan::new(
									resolve_context.file_id,
									kind.span,
								)),
							);
							self.register_placeholder_memory(
								resolve_context,
								*id,
								name,
							);
							self.sig_state.get_mut(&def_id).unwrap().state =
								ComputeState::Done;
							return;
						}
					};

					let trait_fn_ids = self.tir.traits[trait_index as usize]
						.member_ids
						.clone();
					for tid in trait_fn_ids {
						self.ensure_signature(tid);
					}
					let memory_index = self.tir.expect_memory_index(*id);
					self.tir.memories[memory_index as usize] = Memory {
						id: *id,
						file_id: resolve_context.file_id,
						kind: memory_kind,
						name: *name,
						min_pages: config.as_ref().and_then(|c| {
							c.min_pages.as_ref().map(|s| s.inner)
						}),
						max_pages: config.as_ref().and_then(|c| {
							c.max_pages.as_ref().map(|s| s.inner)
						}),
					};
					let memory_type = self.intern_type(Type::Memory {
						kind: memory_kind,
						id: *id,
					});
					self.seed_memory_trait_impl_with(
						trait_index,
						memory_type,
						&bindings,
					);

					// Register the memory type as implementing its declared trait
					// so that check_assoc_type_bounds can verify `type M: Memory`
					// bindings on concrete impls (e.g. `impl Allocator for T { type M = heap; }`).
					// Populate members from impl_members so conformance checking passes.
					let impl_member_snapshot: HashMap<SymbolU32, ImplEntry> =
						self.tir
							.impl_members
							.get(&memory_type)
							.map(|m| {
								m.iter().map(|(&k, v)| (k, v.clone())).collect()
							})
							.unwrap_or_default();
					let trait_impl_index =
						self.tir.trait_impls.len() as TraitImplIndex;
					let synthetic_def_id = self.id_generator.generate();
					self.tir.trait_impls.push(TraitImpl {
						id: synthetic_def_id,
						trait_index,
						target: memory_type,
						members: impl_member_snapshot,
						span: name.span,
						file_id: resolve_context.file_id,
					});
					self.tir
						.trait_impl_lookup
						.insert((memory_type, trait_index), trait_impl_index);
					self.tir
						.type_trait_impls
						.entry(memory_type)
						.or_default()
						.push(trait_impl_index);
					self.tir.item_lookup.insert(
						synthetic_def_id,
						ItemIndex::TraitImpl(trait_impl_index),
					);

					// Bind each namespace only if this occurrence still holds
					// its own `Pending` slot there — see the identical
					// comment on the Struct branch. Type and Value are
					// independent claims (mirroring the two separate
					// `claim_name_binding` calls in `pre_scan_item`).
					let type_key = (SymbolNamespace::Type, name.inner);
					if matches!(
						self.direct_scope_lookup(resolve_context.namespace, type_key),
						Some(SymbolKind::Pending(pending_id)) if pending_id == *id
					) {
						self.insert_symbol(
							resolve_context.namespace,
							type_key,
							SymbolKind::Memory {
								memory_index,
								kind: memory_kind,
							},
						);
					}
					let value_key = (SymbolNamespace::Value, name.inner);
					if matches!(
						self.direct_scope_lookup(resolve_context.namespace, value_key),
						Some(SymbolKind::Pending(pending_id)) if pending_id == *id
					) {
						self.insert_symbol(
							resolve_context.namespace,
							value_key,
							SymbolKind::Memory {
								memory_index,
								kind: memory_kind,
							},
						);
					}
				}
			}
			AstNodeRef::Const { item } => {
				if let ast::Item::Const {
					id,
					name,
					ty,
					value,
					..
				} = item
				{
					let const_index = self.tir.expect_const_index(*id);
					let (ty_idx, ty_span) = match ty {
						Some(ty) => (
							self.resolve_type(resolve_context, None, ty),
							ty.span,
						),
						None => {
							self.tir.diagnostics.push(
								report_type_annotation_required(
									SourceSpan::new(
										resolve_context.file_id,
										name.span,
									),
								),
							);
							(TypeIndex::ERROR, name.span)
						}
					};
					self.tir.constants[const_index as usize].ty =
						ast::Spanned {
							inner: ty_idx,
							span: ty_span,
						};
					// A const whose value fails to build never claims its
					// name, matching current behavior — the stub still
					// exists (for `item_lookup`/duplicate-span purposes)
					// with `value: None`, but stays permanently `Pending`.
					if let Ok(value_expr) = self.build_const_context_expression(
						resolve_context,
						value,
						ty_idx,
					) {
						let const_value =
							match self.eval_const_expr(&value_expr) {
								Ok(v) => Some(v),
								Err(_) => {
									self.tir.diagnostics.push(
										report_not_const_evaluatable(
											SourceSpan::new(
												resolve_context.file_id,
												value.span,
											),
										),
									);
									None
								}
							};
						self.tir.constants[const_index as usize].value =
							Some(Box::new(value_expr));
						self.tir.constants[const_index as usize].const_value =
							const_value;

						let key = (SymbolNamespace::Value, name.inner);
						if matches!(
							self.direct_scope_lookup(resolve_context.namespace, key),
							Some(SymbolKind::Pending(pending_id)) if pending_id == *id
						) {
							self.insert_symbol(
								resolve_context.namespace,
								key,
								SymbolKind::Const { const_index },
							);
						}
					}
				}
			}
			AstNodeRef::ImportedFunction {
				import_module_index,
				decl,
				..
			} => {
				if let ast::ImportDeclaration::Function { id, signature } = decl
				{
					let (params, result) = self.build_function_signature(
						resolve_context,
						None,
						signature,
					);
					let signature_index = self.intern_function(&params, result);
					let func_index = self.tir.functions.len() as u32;
					let import_ns_idx = self.tir.import_decls
						[import_module_index as usize]
						.namespace_idx;
					self.tir.functions.push(Function {
						id: *id,
						file_id: resolve_context.file_id,
						namespace: Some(import_ns_idx),
						parent: None,
						signature_index,
						body: None,
						type_params: Box::new([]),
						type_param_parent: None,
						inherited_type_param_count: 0,
						pub_span: None,
						name: signature.name,
						accesses: Vec::new(),
						params,
						result,
						attributes: Box::new([]),
					});
					self.tir
						.item_lookup
						.insert(*id, ItemIndex::Function(func_index));
					let import_decl = &mut self.tir.import_decls
						[import_module_index as usize];
					import_decl.lookup.insert(
						signature.name.inner,
						ImportValue::Function { id: *id },
					);
					let namespace_idx = import_decl.namespace_idx;
					self.tir.namespaces[namespace_idx as usize].symbols.insert(
						(SymbolNamespace::Value, signature.name.inner),
						SymbolKind::Function { func_index },
					);
				}
			}
			AstNodeRef::ImportedGlobal {
				import_module_index,
				decl,
				..
			} => {
				if let ast::ImportDeclaration::Global {
					id,
					name,
					ty,
					mut_span,
				} = decl
				{
					let resolved_ty =
						self.resolve_type(resolve_context, None, ty);
					let global_index = self.tir.globals.len() as u32;
					let import_ns_idx = self.tir.import_decls
						[import_module_index as usize]
						.namespace_idx;
					self.tir.globals.push(Global {
						id: *id,
						file_id: resolve_context.file_id,
						namespace: Some(import_ns_idx),
						value: None,
						name: *name,
						ty: ast::Spanned {
							inner: resolved_ty,
							span: ty.span,
						},
						pub_span: None,
						mut_span: *mut_span,
						accesses: Vec::new(),
					});
					self.tir
						.item_lookup
						.insert(*id, ItemIndex::Global(global_index));
					let import_decl = &mut self.tir.import_decls
						[import_module_index as usize];
					import_decl
						.lookup
						.insert(name.inner, ImportValue::Global { id: *id });
					let namespace_idx = import_decl.namespace_idx;
					self.tir.namespaces[namespace_idx as usize].symbols.insert(
						(SymbolNamespace::Value, name.inner),
						SymbolKind::Global { global_index },
					);
				}
			}
			AstNodeRef::ImplTraitBlock { item } => {
				let (block_id, trait_name, target) = match item {
					ast::Item::ImplTrait {
						id,
						trait_name,
						target,
						..
					} => (id, trait_name, target),
					_ => unreachable!(),
				};

				let trait_name_span = TextSpan::new(
					trait_name.first().unwrap().ident.span.start,
					trait_name.last().unwrap().ident.span.end,
				);
				let trait_index = match self.resolve_path_segments_as_bound(
					resolve_context,
					trait_name,
					trait_name_span,
				) {
					Ok(BoundKind::Trait(tb)) => tb.trait_index,
					Ok(BoundKind::TypeSet(_)) => {
						self.tir.diagnostics.push(
							Diagnostic::error()
								.with_code(DiagnosticCode::ExpectedTrait.code())
								.with_message("expected a trait name")
								.with_label(Label::primary(
									resolve_context.file_id,
									trait_name_span,
								)),
						);
						return;
					}
					Err(()) => return,
				};

				let target_type =
					self.resolve_type(resolve_context, None, target);
				let trait_impl_index =
					self.tir.trait_impls.len() as TraitImplIndex;

				self.tir.trait_impls.push(TraitImpl {
					id: *block_id,
					trait_index,
					target: target_type,
					members: HashMap::new(),
					span: trait_name_span,
					file_id: resolve_context.file_id,
				});
				self.tir
					.trait_impl_lookup
					.insert((target_type, trait_index), trait_impl_index);
				self.tir
					.type_trait_impls
					.entry(target_type)
					.or_default()
					.push(trait_impl_index);
				self.tir
					.item_lookup
					.insert(*block_id, ItemIndex::TraitImpl(trait_impl_index));

				// Seed default (bodied) trait methods into impl_members so
				// `self.method()` dispatch finds them without explicit overrides.
				let trait_fn_ids =
					self.tir.traits[trait_index as usize].member_ids.clone();
				for &tid in &trait_fn_ids {
					self.ensure_signature(tid);
				}
				let self_sym = self.interner.get_or_intern("self");
				let mut default_members: Vec<(SymbolU32, ImplEntry)> =
					Vec::new();
				for &tid in &trait_fn_ids {
					let fi = match self.tir.function_index(tid) {
						Some(fi) => fi,
						None => continue,
					};
					let node = match self.sig_state.get(&tid) {
						Some(e) => &self.ast_nodes[e.node_idx].node,
						None => continue,
					};
					if let AstNodeRef::TraitFunction { item, .. } = node
						&& let ast::TraitItem::Function {
							signature,
							body: Some(_),
							..
						} = item
					{
						let is_method = signature
							.params
							.first()
							.map(|p| p.inner.inner.name.inner == self_sym)
							.unwrap_or(false);
						let entry = if is_method {
							ImplEntry::Method(fi)
						} else {
							ImplEntry::AssociatedFn(fi)
						};
						default_members.push((signature.name.inner, entry));
					}
				}
				// `or_insert`: block DefId < child method DefIds (source order),
				// so defaults land first; `ImplTraitMethod` overwrites with plain
				// `insert` later in Phase 2, giving explicit overrides priority.
				let slot =
					self.tir.impl_members.entry(target_type).or_default();
				for (sym, entry) in default_members {
					slot.entry(sym).or_insert(entry);
				}
			}
			AstNodeRef::ImplTraitMethod {
				parent_id, item, ..
			} => {
				self.ensure_signature(parent_id);
				let trait_impl_index =
					match self.tir.trait_impl_index(parent_id) {
						Some(idx) => idx,
						None => return,
					};
				let self_type =
					self.tir.trait_impls[trait_impl_index as usize].target;
				let self_symbol = self.interner.get_or_intern("self");

				if let ast::ImplItem::Method {
					id,
					pub_span,
					attributes,
					signature,
					..
				} = item
				{
					let self_scope = GenericScope {
						owner: TypeParamOwner::Function(*id),
						self_type: Some(self_type),
					};
					let mut params: Vec<FunctionParam> =
						Vec::with_capacity(signature.params.len());
					for p in signature.params.iter() {
						let is_self = p.inner.inner.name.inner == self_symbol;
						let ty = match &p.inner.inner.ty {
							Some(te) => Spanned {
								inner: self.resolve_type(
									resolve_context,
									Some(self_scope),
									te,
								),
								span: te.span,
							},
							None => Spanned {
								inner: if is_self {
									self_type
								} else {
									TypeIndex::ERROR
								},
								span: p.inner.inner.name.span,
							},
						};
						params.push(FunctionParam {
							mut_span: p.inner.inner.mut_span,
							name: p.inner.inner.name,
							ty,
						});
					}
					let params: Box<[FunctionParam]> =
						params.into_boxed_slice();
					let result = signature.result.as_ref().map(|r| Spanned {
						inner: self.resolve_type(
							resolve_context,
							Some(self_scope),
							r,
						),
						span: r.span,
					});
					let signature_index = self.intern_function(&params, result);
					let func_index = self.tir.functions.len() as u32;
					let is_method = signature
						.params
						.first()
						.map(|p| p.inner.inner.name.inner == self_symbol)
						.unwrap_or(false);
					let entry = if is_method {
						ImplEntry::Method(func_index)
					} else {
						ImplEntry::AssociatedFn(func_index)
					};
					let func_attrs = self.resolve_attributes(attributes);
					self.register_tagged_items(*id, &func_attrs);
					self.tir.functions.push(Function {
						id: *id,
						file_id: resolve_context.file_id,
						namespace: resolve_context.namespace,
						parent: Some(ItemParent::Impl(self_type)),
						body: None,
						type_params: Box::new([]),
						type_param_parent: None,
						inherited_type_param_count: 0,
						pub_span: *pub_span,
						signature_index,
						name: signature.name,
						accesses: Vec::new(),
						params,
						result,
						attributes: func_attrs,
					});
					self.tir
						.item_lookup
						.insert(*id, ItemIndex::Function(func_index));
					self.tir
						.impl_members
						.entry(self_type)
						.or_default()
						.insert(signature.name.inner, entry.clone());
					self.tir.trait_impls[trait_impl_index as usize]
						.members
						.insert(signature.name.inner, entry);
				}
			}
			AstNodeRef::ImplTraitConst {
				parent_id, item, ..
			} => {
				self.ensure_signature(parent_id);
				let trait_impl_index =
					match self.tir.trait_impl_index(parent_id) {
						Some(idx) => idx,
						None => return,
					};
				let self_type =
					self.tir.trait_impls[trait_impl_index as usize].target;

				if let ast::ImplItem::Const {
					id,
					name,
					ty,
					value,
					..
				} = item
				{
					let self_scope = GenericScope {
						owner: TypeParamOwner::Function(*id),
						self_type: Some(self_type),
					};
					let resolved_ty = match ty {
						Some(te) => self.resolve_type(
							resolve_context,
							Some(self_scope),
							te,
						),
						None => TypeIndex::ERROR,
					};
					if let Ok(value_expr) = self.build_const_context_expression(
						resolve_context,
						value,
						resolved_ty,
					) {
						let const_value =
							match self.eval_const_expr(&value_expr) {
								Ok(v) => Some(v),
								Err(_) => {
									self.tir.diagnostics.push(
										report_not_const_evaluatable(
											SourceSpan::new(
												resolve_context.file_id,
												value.span,
											),
										),
									);
									None
								}
							};
						let const_index =
							self.tir.constants.len() as ConstIndex;
						self.tir.constants.push(Constant {
							id: *id,
							file_id: resolve_context.file_id,
							namespace: resolve_context.namespace,
							parent: Some(ItemParent::Impl(self_type)),
							pub_span: None,
							name: *name,
							ty: ast::Spanned {
								inner: resolved_ty,
								span: name.span,
							},
							value: Some(Box::new(value_expr)),
							const_value,
							accesses: Vec::new(),
						});
						self.tir
							.item_lookup
							.insert(*id, ItemIndex::Const(const_index));
						let entry = ImplEntry::AssociatedConst {
							id: *id,
							ty: resolved_ty,
						};
						self.tir
							.impl_members
							.entry(self_type)
							.or_default()
							.insert(name.inner, entry.clone());
						self.tir.trait_impls[trait_impl_index as usize]
							.members
							.insert(name.inner, entry);
					}
				}
			}
			AstNodeRef::TraitAssociatedType {
				trait_index, item, ..
			} => {
				if let ast::TraitItem::AssociatedType {
					id,
					name,
					bounds,
					attributes,
				} = item
				{
					let bounds = bounds
						.as_ref()
						.map(|bound| {
							self.resolve_bounds(resolve_context, None, bound)
						})
						.unwrap_or_default();
					let attributes = self.resolve_attributes(attributes);
					self.register_tagged_items(*id, &attributes);

					let placeholder = self.intern_type(Type::AssociatedType {
						assoc_name: name.inner,
						trait_index,
					});

					self.tir.traits[trait_index as usize].assoc_types.insert(
						name.inner,
						TraitAssocType {
							id: *id,
							name_span: name.span,
							bounds,
							accesses: Vec::new(),
						},
					);
					self.tir.traits[trait_index as usize].members.insert(
						name.inner,
						ImplEntry::AssociatedType { ty: placeholder },
					);

					// Replace Pending with TraitAssocType only if it's still our
					// own Pending — never clobber a same-named resolved symbol.
					if matches!(
						self.lookup_global_symbol(resolve_context.namespace, (SymbolNamespace::Type, name.inner)),
						Some(SymbolKind::Pending(d)) if d == *id
					) {
						self.insert_symbol(
							resolve_context.namespace,
							(SymbolNamespace::Type, name.inner),
							SymbolKind::TraitAssocType {
								trait_index,
								assoc_name: name.inner,
							},
						);
					}
				}
			}
			AstNodeRef::ImplTraitAssociatedType {
				parent_id, item, ..
			} => {
				self.ensure_signature(parent_id);
				let trait_impl_index =
					match self.tir.trait_impl_index(parent_id) {
						Some(idx) => idx,
						None => return,
					};
				let trait_index =
					self.tir.trait_impls[trait_impl_index as usize].trait_index;
				let self_type =
					self.tir.trait_impls[trait_impl_index as usize].target;

				if let ast::ImplItem::AssociatedType { name, ty, .. } = item {
					let self_scope = GenericScope {
						owner: TypeParamOwner::Trait(trait_index),
						self_type: Some(self_type),
					};
					let concrete_ty = self.resolve_type(
						resolve_context,
						Some(self_scope),
						ty,
					);
					let entry = ImplEntry::AssociatedType { ty: concrete_ty };
					self.tir
						.impl_members
						.entry(self_type)
						.or_default()
						.insert(name.inner, entry.clone());
					self.tir.trait_impls[trait_impl_index as usize]
						.members
						.insert(name.inner, entry);

					// Bounds are resolved lazily — ensure the trait's signature is
					// ready before reading assoc_types.
					let trait_def_id = self.tir.traits[trait_index as usize]
                        .member_ids
                        .iter()
                        .copied()
                        .find(|&did| {
                            matches!(
                                self.sig_state.get(&did).map(|e| &self.ast_nodes[e.node_idx].node),
                                Some(AstNodeRef::TraitAssociatedType { item, .. })
                                    if matches!(item, ast::TraitItem::AssociatedType { name: n, .. } if n.inner == name.inner)
                            )
                        });
					if let Some(did) = trait_def_id {
						self.ensure_signature(did);
					}
					if let Some(at) = self.tir.traits[trait_index as usize]
						.assoc_types
						.get_mut(&name.inner)
					{
						at.accesses.push(SourceSpan::new(
							resolve_context.file_id,
							name.span,
						));
					}
					self.check_assoc_type_bounds(
						resolve_context.file_id,
						trait_index,
						name.inner,
						concrete_ty,
						name.span,
					);
				}
			}
		}

		self.sig_state.get_mut(&def_id).unwrap().state = ComputeState::Done;
	}

	/// Resolves the body of `def_id`. Not idempotent — calling twice
	/// double-counts accesses.
	fn ensure_body(&mut self, def_id: ast::DefId) {
		self.ensure_signature(def_id);

		let node_idx = self.sig_state.get(&def_id).unwrap().node_idx;
		let AstEntry {
			file_id,
			namespace,
			node,
			..
		} = self.ast_nodes[node_idx].clone();

		let (sig, body_expr, func_index, self_type) = match node {
			AstNodeRef::Function { item } => match item {
				ast::Item::Function {
					id,
					signature,
					block,
					..
				} => {
					let func_index = self.tir.expect_function_index(*id);
					(signature, block.as_ref(), func_index, None)
				}
				ast::Item::FunctionDeclaration { id, signature, .. } => {
					let func_index = self.tir.expect_function_index(*id);
					if self.tir.functions[func_index as usize]
						.attributes
						.contains(&ItemAttribute::Intrinsic)
					{
						/* allow missing body for intrinsics */
					} else {
						self.tir.diagnostics.push(
							report_missing_function_body(SourceSpan::new(
								file_id,
								signature.name.span,
							)),
						);
					}
					return;
				}
				_ => unreachable!(),
			},
			AstNodeRef::ImplMethod {
				item, impl_target, ..
			} => {
				let ast::ImplItem::Method {
					id,
					signature,
					block,
					..
				} = item
				else {
					return;
				};
				let Some(fi) = self.tir.function_index(*id) else {
					return;
				};
				let self_type = Some(self.resolve_type(
					ResolveContext::new(file_id, namespace),
					None,
					impl_target,
				));
				(signature, block.as_ref(), fi, self_type)
			}
			AstNodeRef::ImplTraitMethod {
				item, parent_id, ..
			} => {
				let ast::ImplItem::Method {
					id,
					signature,
					block,
					..
				} = item
				else {
					return;
				};
				let Some(fi) = self.tir.function_index(*id) else {
					return;
				};
				let self_type = self
					.tir
					.trait_impl_index(parent_id)
					.map(|idx| self.tir.trait_impls[idx as usize].target);
				(signature, block.as_ref(), fi, self_type)
			}
			AstNodeRef::GenericImplMethod {
				item, block_index, ..
			} => {
				let ast::ImplItem::Method {
					id,
					signature,
					block,
					..
				} = item
				else {
					return;
				};
				let Some(fi) = self.tir.function_index(*id) else {
					return;
				};
				let self_type = Some(
					self.tir.generic_impl_list[block_index as usize].target,
				);
				(signature, block.as_ref(), fi, self_type)
			}
			AstNodeRef::TraitFunction { item, .. } => {
				let ast::TraitItem::Function {
					id,
					signature,
					body: Some(body),
					..
				} = item
				else {
					return;
				};
				let Some(fi) = self.tir.function_index(*id) else {
					return;
				};
				let self_type = Some(self.intern_type(Type::TypeParam {
					owner: TypeParamOwner::Function(
						self.tir.functions[fi as usize].id,
					),
					param_index: 0,
				}));
				(signature, body.as_ref(), fi, self_type)
			}
			AstNodeRef::Global { item } => {
				let ast::Item::Global { id, value, .. } = item else {
					unreachable!();
				};

				let global_index = self.tir.expect_global_index(*id);
				let global_ty =
					self.tir.globals[global_index as usize].ty.inner;

				let root_scope = BlockScope {
					parent: None,
					label: None,
					kind: BlockKind::Block,
					span: value.span,
					locals: Vec::new(),
					inferred_type: TypeIndex::INFER,
					expected_type: global_ty,
				};
				let mut func_ctx = ExprContext {
					stack: StackFrame {
						scopes: vec![root_scope],
						labels: Vec::new(),
					},
					scope_index: 0 as ScopeIndex,
					lookup: HashMap::new(),
					resolve_context: ResolveContext::new(file_id, namespace),
					// Globals can't be generic and have no `Self` — no honest
					// `GenericScope` to give them.
					scope: None,
				};
				let mut value_expr = match self.build_expression(
					&mut func_ctx,
					AccessContext {
						expected_type: global_ty,
						access_kind: AccessKind::Read,
					},
					value,
				) {
					Ok(expr) => expr,
					Err(_) => return,
				};

				if value_expr.ty.is_comptime_number()
					&& global_ty != TypeIndex::INFER
				{
					_ = self.coerce_untyped_expr(
						&mut func_ctx,
						&mut value_expr,
						global_ty,
					);
				}

				if value_expr.ty.is_comptime_number() {
					self.tir.diagnostics.push(report_type_annotation_required(
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							value.span,
						),
					));
				} else if !self.coercible_to(value_expr.ty, global_ty) {
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type: global_ty,
							actual_type: value_expr.ty,
							span: SourceSpan::new(
								func_ctx.resolve_context.file_id,
								value.span,
							),
						},
					));
				} else if self.tir.globals[global_index as usize]
					.mut_span
					.is_none() && !matches!(
					value_expr.kind,
					ExprKind::Int { .. } | ExprKind::Float { .. }
				) {
					self.tir.diagnostics.push(
						report_non_constant_global_initializer(
							SourceSpan::new(
								func_ctx.resolve_context.file_id,
								value.span,
							),
						),
					);
				}

				self.report_stack_warnings(
					func_ctx.resolve_context.file_id,
					&func_ctx.stack,
				);

				self.tir.globals[global_index as usize].value =
					Some(FunctionBody {
						block: Box::new(value_expr),
						stack: func_ctx.stack,
					});

				return;
			}
			_ => return,
		};

		// Self is TypeParam{0} in trait default methods (see ensure_signature).
		let resolve_context = ResolveContext::new(file_id, namespace);
		let scope = GenericScope {
			owner: TypeParamOwner::Function(
				self.tir.functions[func_index as usize].id,
			),
			self_type,
		};

		if let Ok(body) = self.build_function_body(
			resolve_context,
			&scope,
			sig,
			body_expr,
			func_index,
		) {
			self.tir.functions[func_index as usize].body = Some(body);
		}
	}

	/// Resolves a single bound name (identifier or `module::name`) directly to a [`BoundKind`]
	/// without going through the type pool.
	fn resolve_identifier_as_bound(
		&mut self,
		resolve_context: ResolveContext,
		identifier: Spanned<SymbolU32>,
		span: TextSpan,
	) -> Result<BoundKind, ()> {
		let file_id = resolve_context.file_id;
		let kind = self.resolve_pending_global_symbol(
			resolve_context.namespace,
			(SymbolNamespace::Type, identifier.inner),
			file_id,
			identifier.span,
		)?;
		match kind {
			Some(SymbolKind::Trait { trait_index }) => {
				self.tir.traits[trait_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, identifier.span));
				Ok(BoundKind::Trait(TraitBound {
					trait_index,
					bindings: Box::new([]),
				}))
			}
			Some(SymbolKind::TypeSet { typeset_index }) => {
				self.tir.typesets[typeset_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, identifier.span));
				Ok(BoundKind::TypeSet(typeset_index))
			}
			None => {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::UndeclaredType.code())
						.with_message(format!(
							"cannot find trait or typeset `{}` in this scope",
							self.interner
								.resolve(identifier.inner)
								.unwrap_or("<unknown>")
						))
						.with_label(Label::primary(file_id, span)),
				);
				Err(())
			}
			_ => {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::ExpectedTrait.code())
						.with_message(
							"expected a trait or typeset name as a bound",
						)
						.with_label(Label::primary(file_id, span)),
				);
				Err(())
			}
		}
	}

	/// Resolves a path (possibly `module::Trait`) to a [`BoundKind`] without touching the
	/// type pool. Intermediate segments are walked as type namespaces; only the final
	/// segment is converted to a bound.
	fn resolve_path_segments_as_bound(
		&mut self,
		resolve_context: ResolveContext,
		segs: &[ast::PathSegment],
		span: TextSpan,
	) -> Result<BoundKind, ()> {
		let file_id = resolve_context.file_id;
		if segs.len() == 1 {
			return self.resolve_identifier_as_bound(
				resolve_context,
				segs[0].ident,
				span,
			);
		}
		// Walk all but the last segment as type namespaces (modules).
		let first = &segs[0];
		let Ok(mut namespace_ty) =
			self.resolve_type_identifier(resolve_context, None, first.ident)
		else {
			return Err(());
		};
		let mut namespace_span = first.ident.span;
		for seg in &segs[1..segs.len() - 1] {
			match self.resolve_namespace_type_member(
				resolve_context,
				namespace_ty,
				namespace_span,
				seg.ident,
			) {
				Ok(ty) => {
					namespace_ty = ty;
					namespace_span = seg.ident.span;
				}
				Err(()) => return Err(()),
			}
		}
		// Final segment: look up the symbol in the final namespace and convert to BoundKind.
		let last = segs.last().unwrap();
		let Type::Namespace { namespace_idx } =
			self.tir.types[namespace_ty.as_usize()].clone()
		else {
			self.tir.diagnostics.push(
				Diagnostic::error()
					.with_message(
						"expected a module namespace before a bound name",
					)
					.with_label(Label::primary(file_id, namespace_span)),
			);
			return Err(());
		};
		let kind = self.resolve_pending_namespace_symbol(
			namespace_idx,
			(SymbolNamespace::Type, last.ident.inner),
			SourceSpan::new(file_id, last.ident.span),
		)?;
		match kind {
			Some(SymbolKind::Trait { trait_index }) => {
				self.tir.traits[trait_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, last.ident.span));
				Ok(BoundKind::Trait(TraitBound {
					trait_index,
					bindings: Box::new([]),
				}))
			}
			Some(SymbolKind::TypeSet { typeset_index }) => {
				self.tir.typesets[typeset_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, last.ident.span));
				Ok(BoundKind::TypeSet(typeset_index))
			}
			_ => {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::ExpectedTrait.code())
						.with_message(
							"expected a trait or typeset name as a bound",
						)
						.with_label(Label::primary(file_id, last.ident.span)),
				);
				Err(())
			}
		}
	}

	/// Resolves a bound expression into a [`Bounds`], handling `BoundList` (flattening into
	/// multiple trait/typeset entries), `WithBindings` (resolving associated-type bindings),
	/// and plain `Path` bounds. At most one typeset bound is allowed; a second one is an error.
	fn resolve_bounds(
		&mut self,
		resolve_context: ResolveContext,
		scope: Option<GenericScope>,
		bound: &ast::Spanned<ast::BoundExpression>,
	) -> Bounds {
		match &bound.inner {
			ast::BoundExpression::Path(segs) => {
				match self.resolve_path_segments_as_bound(
					resolve_context,
					segs,
					bound.span,
				) {
					Ok(BoundKind::Trait(tb)) => Bounds {
						traits: Box::new([tb]),
						typeset: None,
					},
					Ok(BoundKind::TypeSet(ts)) => Bounds {
						traits: Box::new([]),
						typeset: Some(ts),
					},
					Err(()) => Bounds::default(),
				}
			}
			ast::BoundExpression::WithBindings {
				path,
				bindings: where_bindings,
			} => {
				let segs = match path.as_ref() {
					ast::BoundExpression::Path(segs) => segs,
					_ => {
						self.tir.diagnostics.push(
							Diagnostic::error()
								.with_message(
									"expected a single trait bound here",
								)
								.with_label(Label::primary(
									resolve_context.file_id,
									bound.span,
								)),
						);
						return Bounds::default();
					}
				};
				let trait_index = match self.resolve_path_segments_as_bound(
					resolve_context,
					segs,
					bound.span,
				) {
					Ok(BoundKind::Trait(tb)) => tb.trait_index,
					Ok(BoundKind::TypeSet(_)) => {
						self.tir.diagnostics.push(
							Diagnostic::error()
								.with_message(
									"typesets cannot have associated type bindings",
								)
								.with_label(Label::primary(
									resolve_context.file_id,
									bound.span,
								)),
						);
						return Bounds::default();
					}
					Err(()) => return Bounds::default(),
				};
				let mut bindings: Vec<(SymbolU32, TypeIndex)> = Vec::new();
				for binding in where_bindings.iter() {
					if let Some(at) = self.tir.traits[trait_index as usize]
						.assoc_types
						.get_mut(&binding.name.inner)
					{
						at.accesses.push(SourceSpan::new(
							resolve_context.file_id,
							binding.name.span,
						));
					}
					let rhs_ty =
						self.resolve_type(resolve_context, scope, &binding.ty);
					bindings.push((binding.name.inner, rhs_ty));
				}
				bindings.sort_unstable_by_key(|(name, _)| *name);
				Bounds {
					traits: Box::new([TraitBound {
						trait_index,
						bindings: bindings.into_boxed_slice(),
					}]),
					typeset: None,
				}
			}
			ast::BoundExpression::BoundList(items) => {
				let mut traits: Vec<TraitBound> = Vec::new();
				let mut typeset: Option<TypesetIndex> = None;
				for item in items.iter() {
					let resolved =
						self.resolve_bounds(resolve_context, scope, item);
					traits.extend_from_slice(&resolved.traits);
					if let Some(ts) = resolved.typeset {
						if typeset.is_some() {
							self.tir.diagnostics.push(
								Diagnostic::error()
									.with_code(
										DiagnosticCode::MultipleTypesetBounds
											.code(),
									)
									.with_message(
										"at most one typeset bound is allowed",
									)
									.with_label(Label::primary(
										resolve_context.file_id,
										item.span,
									)),
							);
						} else {
							typeset = Some(ts);
						}
					}
				}
				Bounds {
					traits: traits.into_boxed_slice(),
					typeset,
				}
			}
		}
	}

	/// Resolves and writes bounds for `ast_params` into the type params already
	/// registered in TIR under `owner`. Must be called after the item is pushed
	/// and its index-lookup entry is inserted.
	///
	/// `self_type` makes `Self` resolvable inside bound expressions for impl
	/// block methods (where `Self` is a concrete type alias, not a type param).
	/// For trait methods `Self` is found via the parent-chain lookup instead.
	///
	/// The offset — how many inherited params precede the first AST param in the
	/// absolute-index space — is read directly from the owner's registered
	/// `inherited_type_param_count` rather than computed by subtraction.
	fn resolve_type_param_bounds(
		&mut self,
		resolve_context: ResolveContext,
		owner: TypeParamOwner,
		self_type: Option<TypeIndex>,
		ast_params: &[ast::TypeParam],
	) {
		if ast_params.is_empty() {
			return;
		}
		let offset = self.inherited_type_param_count(owner);
		for (i, tp) in ast_params.iter().enumerate() {
			let resolved = tp
				.bounds
				.as_ref()
				.map(|b| {
					self.resolve_bounds(
						resolve_context,
						Some(GenericScope { owner, self_type }),
						b,
					)
				})
				.unwrap_or_default();
			self.tir.type_param_info_mut(owner, offset + i).bounds = resolved;
		}
	}

	fn build_function_signature(
		&mut self,
		resolve_context: ResolveContext,
		scope: Option<GenericScope>,
		signature: &ast::FunctionSignature,
	) -> (Box<[FunctionParam]>, Option<Spanned<TypeIndex>>) {
		let mut seen_params: HashMap<SymbolU32, ast::TextSpan> = HashMap::new();
		let mut params: Vec<FunctionParam> =
			Vec::with_capacity(signature.params.len());
		for param in signature.params.iter() {
			let name = &param.inner.inner.name;
			if let Some(&first_span) = seen_params.get(&name.inner) {
				let name_str = self.interner.resolve(name.inner).unwrap();
				self.tir.diagnostics.push(report_duplicate_parameter(
					name_str,
					SourceSpan::new(resolve_context.file_id, first_span),
					SourceSpan::new(resolve_context.file_id, name.span),
				));
			} else {
				seen_params.insert(name.inner, name.span);
			}
			let ty = match &param.inner.inner.ty {
				Some(ty) => Spanned {
					inner: self.resolve_signature_type(
						resolve_context,
						scope,
						ty,
					),
					span: ty.span,
				},
				None => Spanned {
					inner: TypeIndex::ERROR,
					span: param.inner.inner.name.span,
				},
			};
			params.push(FunctionParam {
				mut_span: param.inner.inner.mut_span,
				name: *name,
				ty,
			});
		}
		let result = signature.result.as_ref().map(|result| Spanned {
			inner: self.resolve_signature_type(resolve_context, scope, result),
			span: result.span,
		});

		(params.into_boxed_slice(), result)
	}

	/// Finishes resolving `Alias::<T, U>` / `Alias<T, U>` once the caller has
	/// already resolved the type arguments: checks the count against the
	/// alias's own type params, then substitutes them into the alias's
	/// (possibly `TypeParam`-laden) template via `substitute_type`. Aliases
	/// are transparent: the result is always the substituted target type,
	/// never anything alias-shaped.
	/// Applies type arguments to a generic struct or type alias, used by every
	/// turbofish / `GenericApplication` / bare-reference call site. Providing
	/// fewer arguments than declared is allowed — missing trailing slots are
	/// padded with `TypeIndex::INFER` for later inference; providing more is
	/// an error.
	fn resolve_generic_type_application(
		&mut self,
		resolve_context: ResolveContext,
		symbol_kind: SymbolKind,
		resolved_args: &[TypeIndex],
		span: TextSpan,
	) -> TypeIndex {
		let (expected, name_sym) = match symbol_kind {
			SymbolKind::Struct { struct_index } => {
				let s = &self.tir.structs[struct_index as usize];
				(s.type_params.len(), s.name.inner)
			}
			SymbolKind::TypeAlias { type_alias_index } => {
				let a = &self.tir.type_aliases[type_alias_index as usize];
				(a.type_params.len(), a.name.inner)
			}
			_ => {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_message("type arguments are not supported here")
						.with_label(Label::primary(
							resolve_context.file_id,
							span,
						)),
				);
				return TypeIndex::ERROR;
			}
		};
		if resolved_args.len() > expected {
			let name =
				self.interner.resolve(name_sym).unwrap_or("?").to_string();
			self.tir.diagnostics.push(
				Diagnostic::error()
					.with_code(DiagnosticCode::TypeArgCountMismatch.code())
					.with_message(format!(
						"`{}` expects at most {} type argument{}, found {}",
						name,
						expected,
						if expected == 1 { "" } else { "s" },
						resolved_args.len(),
					))
					.with_label(Label::primary(resolve_context.file_id, span)),
			);
			return TypeIndex::ERROR;
		}
		let mut args = resolved_args.to_vec();
		args.resize(expected, TypeIndex::INFER);

		match symbol_kind {
			SymbolKind::Struct { struct_index } => {
				self.tir.structs[struct_index as usize]
					.accesses
					.push(SourceSpan::new(resolve_context.file_id, span));
				self.intern_type(Type::Struct {
					struct_index,
					args: args.into_boxed_slice(),
				})
			}
			SymbolKind::TypeAlias { type_alias_index } => {
				self.tir.type_aliases[type_alias_index as usize]
					.accesses
					.push(SourceSpan::new(resolve_context.file_id, span));
				let template =
					self.tir.type_aliases[type_alias_index as usize].template;
				self.substitute_type(template, &args)
			}
			_ => unreachable!("filtered above"),
		}
	}

	fn substitute_type(
		&mut self,
		ty: TypeIndex,
		type_args: &[TypeIndex],
	) -> TypeIndex {
		match &self.tir.types[ty.as_usize()] {
			// Types that can never contain TypeParams — return immediately.
			Type::Unit
			| Type::Bool
			| Type::Error
			| Type::Infer
			| Type::Never
			| Type::Integer
			| Type::Float
			| Type::I8
			| Type::I16
			| Type::I32
			| Type::I64
			| Type::U8
			| Type::U16
			| Type::U32
			| Type::U64
			| Type::F32
			| Type::F64
			| Type::Char
			| Type::Enum { .. }
			| Type::Namespace { .. }
			| Type::Memory { .. }
			| Type::AssociatedType { .. } => ty,
			Type::TypeParam { param_index, .. } => type_args
				.get(*param_index as usize)
				.copied()
				.filter(|&t| t != TypeIndex::ERROR)
				.unwrap_or(ty),
			Type::AssocTypeProjection {
				base,
				assoc_name,
				trait_index,
			} => {
				let (base, assoc_name, trait_index) =
					(*base, *assoc_name, *trait_index);
				let substituted = self.substitute_type(base, type_args);
				match &self.tir.types[substituted.as_usize()] {
					Type::TypeParam { .. }
					| Type::AssocTypeProjection { .. } => {
						if substituted == base {
							ty
						} else {
							self.intern_type(Type::AssocTypeProjection {
								trait_index,
								assoc_name,
								base: substituted,
							})
						}
					}
					_ => self
						.tir
						.impl_members
						.get(&substituted)
						.and_then(|m| m.get(&assoc_name))
						.and_then(|e| {
							if let ImplEntry::AssociatedType { ty: concrete } =
								e
							{
								Some(*concrete)
							} else {
								None
							}
						})
						.unwrap_or(ty),
				}
			}
			Type::Pointer {
				to,
				memory,
				mutable,
			} => {
				let (to, memory, mutable) = (*to, *memory, *mutable);
				let next_to = self.substitute_type(to, type_args);
				let next_memory = self.substitute_type(memory, type_args);
				if next_to == to && next_memory == memory {
					ty
				} else {
					self.intern_type(Type::Pointer {
						to: next_to,
						memory: next_memory,
						mutable,
					})
				}
			}
			Type::Array {
				of,
				size,
				memory,
				mutable,
			} => {
				let (of, size, memory, mutable) =
					(*of, *size, *memory, *mutable);
				let next_of = self.substitute_type(of, type_args);
				let next_memory = self.substitute_type(memory, type_args);
				if next_of == of && next_memory == memory {
					ty
				} else {
					self.intern_type(Type::Array {
						of: next_of,
						size,
						memory: next_memory,
						mutable,
					})
				}
			}
			Type::Slice {
				of,
				memory,
				mutable,
			} => {
				let (of, memory, mutable) = (*of, *memory, *mutable);
				let next_of = self.substitute_type(of, type_args);
				let next_memory = self.substitute_type(memory, type_args);
				if next_of == of && next_memory == memory {
					ty
				} else {
					self.intern_type(Type::Slice {
						of: next_of,
						memory: next_memory,
						mutable,
					})
				}
			}
			Type::Tuple { elements } => {
				let mut changed = false;
				let substituted: Box<[TypeIndex]> = elements
					.clone()
					.iter()
					.copied()
					.map(|element| {
						let next = self.substitute_type(element, type_args);
						changed |= next != element;
						next
					})
					.collect();
				if changed {
					self.intern_type(Type::Tuple {
						elements: substituted,
					})
				} else {
					ty
				}
			}
			Type::Function { signature } => {
				let signature = signature.clone();
				let mut changed = false;
				let items: Box<[TypeIndex]> = signature
					.items
					.iter()
					.copied()
					.map(|item| {
						let next = self.substitute_type(item, type_args);
						changed |= next != item;
						next
					})
					.collect();
				if changed {
					self.intern_type(Type::Function {
						signature: FunctionSignature {
							items,
							params_count: signature.params_count,
						},
					})
				} else {
					ty
				}
			}
			Type::Struct {
				struct_index,
				args: struct_args,
			} => {
				if struct_args.is_empty() {
					return ty;
				}
				let mut changed = false;
				let struct_index = *struct_index;
				let substituted: Box<[TypeIndex]> = struct_args
					.clone()
					.iter()
					.copied()
					.map(|a| {
						let next = self.substitute_type(a, type_args);
						changed |= next != a;
						next
					})
					.collect();
				if changed {
					self.intern_type(Type::Struct {
						struct_index,
						args: substituted,
					})
				} else {
					ty
				}
			}
			Type::FunctionItem {
				id,
				type_args: item_args,
			} => {
				if item_args.is_empty() {
					return ty;
				}
				let mut changed = false;
				let id = *id;
				let substituted: Box<[TypeIndex]> = item_args
					.clone()
					.iter()
					.copied()
					.map(|item_arg| {
						let next = self.substitute_type(item_arg, type_args);
						changed |= next != item_arg;
						next
					})
					.collect();
				if changed {
					self.intern_type(Type::FunctionItem {
						id,
						type_args: substituted,
					})
				} else {
					ty
				}
			}
		}
	}

	/// Returns the concrete expected type for an argument position, or `None`
	/// if inference hasn't resolved it to a usable type yet. `None` tells the
	/// caller to emit a "type annotation required" diagnostic rather than
	/// attempt coercion against an unknown target.
	fn substitute_expected_type(
		&mut self,
		ty: TypeIndex,
		type_args: &[TypeIndex],
	) -> TypeIndex {
		let result = self.substitute_type(ty, type_args);
		match &self.tir.types[result.as_usize()] {
			Type::TypeParam { .. }
			| Type::Integer
			| Type::Float
			| Type::Error => TypeIndex::INFER,
			_ => result,
		}
	}

	fn infer_type_args(
		&self,
		type_args: &mut [TypeIndex],
		pattern_ty: TypeIndex,
		actual_ty: TypeIndex,
	) {
		// Unresolved comptime literals have no concrete type yet; inferring T = INTEGER would give the wrong answer once the literal is coerced.
		// Skip ERROR and INFER actuals too — they must not fill a still-unresolved slot.
		if actual_ty == TypeIndex::INTEGER
			|| actual_ty == TypeIndex::FLOAT
			|| actual_ty == TypeIndex::ERROR
			|| actual_ty == TypeIndex::INFER
		{
			return;
		}

		match (
			&self.tir.types[pattern_ty.as_usize()],
			&self.tir.types[actual_ty.as_usize()],
		) {
			(Type::TypeParam { param_index, .. }, _) => {
				// First binding wins — don't overwrite a slot already set by turbofish or an earlier argument.
				match type_args.get_mut(*param_index as usize) {
					Some(slot) if *slot == TypeIndex::INFER => {
						*slot = actual_ty
					}
					_ => {}
				}
			}
			(
				Type::AssocTypeProjection {
					assoc_name,
					trait_index,
					base,
					..
				},
				Type::AssocTypeProjection {
					assoc_name: actual_assoc,
					trait_index: actual_trait,
					base: actual_base,
					..
				},
			) if assoc_name == actual_assoc && trait_index == actual_trait => {
				self.infer_type_args(type_args, *base, *actual_base);
			}
			(
				Type::Tuple { elements: pattern },
				Type::Tuple { elements: actual },
			) if pattern.len() == actual.len() => {
				for (pattern, actual) in
					pattern.iter().copied().zip(actual.iter().copied())
				{
					self.infer_type_args(type_args, pattern, actual);
				}
			}
			(
				Type::Function {
					signature: pattern_sig,
				},
				Type::Function {
					signature: actual_sig,
				},
			) if pattern_sig.params_count == actual_sig.params_count
				&& pattern_sig.items.len() == actual_sig.items.len() =>
			{
				for (pattern, actual) in pattern_sig
					.items
					.iter()
					.copied()
					.zip(actual_sig.items.iter().copied())
				{
					self.infer_type_args(type_args, pattern, actual);
				}
			}
			(
				Type::Pointer {
					to: pattern_to,
					memory: pattern_memory,
					..
				},
				Type::Pointer {
					to: actual_to,
					memory: actual_memory,
					..
				},
			) => {
				self.infer_type_args(type_args, *pattern_to, *actual_to);
				self.infer_type_args(
					type_args,
					*pattern_memory,
					*actual_memory,
				);
			}
			(
				Type::Array {
					of: pattern_of,
					size: pattern_size,
					memory: pattern_memory,
					..
				},
				Type::Array {
					of: actual_of,
					size: actual_size,
					memory: actual_memory,
					..
				},
			) if pattern_size == actual_size => {
				self.infer_type_args(type_args, *pattern_of, *actual_of);
				self.infer_type_args(
					type_args,
					*pattern_memory,
					*actual_memory,
				);
			}
			(
				Type::Slice {
					of: pattern_of,
					memory: pattern_memory,
					..
				},
				Type::Slice {
					of: actual_of,
					memory: actual_memory,
					..
				},
			) => {
				self.infer_type_args(type_args, *pattern_of, *actual_of);
				self.infer_type_args(
					type_args,
					*pattern_memory,
					*actual_memory,
				);
			}
			(
				Type::Struct {
					struct_index: pattern_struct,
					args: pattern_args,
				},
				Type::Struct {
					struct_index: actual_struct,
					args: actual_args,
				},
			) if pattern_struct == actual_struct
				&& pattern_args.len() == actual_args.len() =>
			{
				for (pattern, actual) in pattern_args
					.iter()
					.copied()
					.zip(actual_args.iter().copied())
				{
					self.infer_type_args(type_args, pattern, actual);
				}
			}
			_ => {}
		}
	}

	/// Structural compatibility check for type annotations that contain `_` holes.
	/// `expected` is the annotation type (may contain `TypeIndex::INFER`); `actual`
	/// is the inferred type.  INFER positions in `expected` match any type in `actual`.
	/// Used only when `self.contains_infer(expected)` is true.
	fn type_satisfies_annotation(
		&self,
		actual: TypeIndex,
		expected: TypeIndex,
	) -> bool {
		if expected == TypeIndex::INFER || actual == expected {
			return true;
		}
		match (
			&self.tir.types[actual.as_usize()],
			&self.tir.types[expected.as_usize()],
		) {
			(
				Type::Struct {
					struct_index: ai,
					args: aa,
				},
				Type::Struct {
					struct_index: bi,
					args: ba,
				},
			) if ai == bi && aa.len() == ba.len() => aa
				.iter()
				.copied()
				.zip(ba.iter().copied())
				.all(|(a, b)| self.type_satisfies_annotation(a, b)),
			(
				Type::Pointer {
					to: at,
					memory: amem,
					mutable: amut,
				},
				Type::Pointer {
					to: bt,
					memory: bmem,
					mutable: bmut,
				},
			) => {
				// *mut T satisfies *T (dropping mut is safe); the reverse is not.
				(*amut || !*bmut)
					&& self.type_satisfies_annotation(*at, *bt)
					&& self.type_satisfies_annotation(*amem, *bmem)
			}
			(Type::Tuple { elements: ae }, Type::Tuple { elements: be })
				if ae.len() == be.len() =>
			{
				ae.iter()
					.copied()
					.zip(be.iter().copied())
					.all(|(a, b)| self.type_satisfies_annotation(a, b))
			}
			_ => false,
		}
	}

	/// Returns `true` if `TypeIndex::INFER` appears anywhere in `ty`'s structure.
	/// Used to detect when a generic type parameter was not resolved during inference
	/// and has propagated into the call's result type.
	fn contains_infer(&self, ty: TypeIndex) -> bool {
		if ty == TypeIndex::INFER {
			return true;
		}
		match &self.tir.types[ty.as_usize()] {
			Type::Struct { args, .. } => {
				args.iter().any(|&a| self.contains_infer(a))
			}
			Type::Pointer { to, memory, .. } => {
				self.contains_infer(*to) || self.contains_infer(*memory)
			}
			Type::Array { of, memory, .. } => {
				self.contains_infer(*of) || self.contains_infer(*memory)
			}
			Type::Slice { of, memory, .. } => {
				self.contains_infer(*of) || self.contains_infer(*memory)
			}
			Type::Tuple { elements } => {
				elements.iter().any(|&e| self.contains_infer(e))
			}
			Type::Function { signature } => {
				signature.items.iter().any(|&t| self.contains_infer(t))
			}
			_ => false,
		}
	}

	fn seed_memory_trait_impl_with(
		&mut self,
		trait_index: u32,
		memory_type: TypeIndex,
		// Assoc-type overrides from the parent supertrait declaration.
		// E.g. Memory32's `Memory<Size=u32>` provides {"Size" → u32}.
		bindings: &HashMap<SymbolU32, TypeIndex>,
	) {
		// Ensure all member signatures in this trait are resolved.
		for did in self.tir.traits[trait_index as usize].member_ids.clone() {
			self.ensure_signature(did);
		}
		let self_symbol = self.interner.get_or_intern("self");
		let raw_members: Vec<(SymbolU32, ImplEntry)> = self.tir.traits
			[trait_index as usize]
			.members
			.iter()
			.map(|(&sym, entry)| (sym, entry.clone()))
			.collect();
		let mut members: Vec<(SymbolU32, ImplEntry)> =
			Vec::with_capacity(raw_members.len());
		for (sym, entry) in raw_members {
			let processed = match entry {
				ImplEntry::Method(fi) => {
					let func = &self.tir.functions[fi as usize];
					if func
						.params
						.first()
						.map(|p| p.name.inner == self_symbol)
						.unwrap_or(false)
					{
						ImplEntry::Method(fi)
					} else {
						ImplEntry::AssociatedFn(fi)
					}
				}
				ImplEntry::AssociatedType { ty } => {
					let concrete = bindings.get(&sym).copied().unwrap_or(ty);
					ImplEntry::AssociatedType { ty: concrete }
				}
				ImplEntry::AssociatedConst { id, ty } => {
					// Substitute Self (TypeParam at param_index 0) with the concrete memory type.
					let concrete_ty = self.substitute_type(ty, &[memory_type]);
					ImplEntry::AssociatedConst {
						id,
						ty: concrete_ty,
					}
				}
				other => other,
			};
			members.push((sym, processed));
		}
		let slot = self.tir.impl_members.entry(memory_type).or_default();
		for (sym, entry) in members {
			slot.insert(sym, entry);
		}
	}

	fn build_exports(
		&mut self,
		file_id: FileId,
		entries: &[Separated<Spanned<ast::ExportEntry>>],
	) {
		for entry in entries.iter() {
			let internal_name = &entry.inner.inner.name;

			let global_value = match self
				.symbol_lookup
				.get(&(SymbolNamespace::Value, internal_name.inner))
			{
				Some(value) => *value,
				None => {
					// Not a value, but it might still name a real item that
					// simply isn't exportable (an enum, struct, trait, ...) —
					// report the more precise diagnostic instead of treating
					// it as an unresolved name. Still record the access so
					// the LSP can resolve hover/go-to-definition on it.
					if let Some(type_value) = self
						.symbol_lookup
						.get(&(SymbolNamespace::Type, internal_name.inner))
						.copied()
					{
						self.record_type_kind_access(
							file_id,
							type_value,
							internal_name.span,
						);
						self.tir.diagnostics.push(report_cannot_export_item(
							self.interner.resolve(internal_name.inner).unwrap(),
							SourceSpan::new(file_id, internal_name.span),
						));
					} else {
						self.tir.diagnostics.push(
							report_undeclared_identifier(SourceSpan::new(
								file_id,
								internal_name.span,
							)),
						);
					}
					continue;
				}
			};

			let external_name =
				entry.inner.inner.alias.as_ref().map(|alias_span| {
					let escaped_text =
						self.interner.resolve(alias_span.inner).unwrap();
					let unescaped = unescape_string(escaped_text);
					let symbol = self.interner.get_or_intern(&unescaped);
					ast::Spanned {
						inner: symbol,
						span: alias_span.span,
					}
				});

			let export_item = match global_value {
				SymbolKind::Function { func_index } => {
					if self.tir.functions[func_index as usize]
						.total_type_param_count()
						> 0
					{
						self.tir.functions[func_index as usize]
							.accesses
							.push(SourceSpan::new(file_id, internal_name.span));
						self.tir.diagnostics.push(
							report_cannot_export_generic_function(
								self.interner
									.resolve(internal_name.inner)
									.unwrap(),
								SourceSpan::new(file_id, internal_name.span),
							),
						);
						continue;
					}

					self.tir.functions[func_index as usize]
						.accesses
						.push(SourceSpan::new(file_id, internal_name.span));

					ExportItem::Function {
						id: self.tir.functions[func_index as usize].id,
						internal_name: *internal_name,
						external_name,
					}
				}
				SymbolKind::Global { global_index } => {
					self.tir.globals[global_index as usize]
						.accesses
						.push(SourceSpan::new(file_id, internal_name.span));

					ExportItem::Global {
						id: self.tir.globals[global_index as usize].id,
						internal_name: *internal_name,
						external_name,
					}
				}
				SymbolKind::Memory { memory_index, .. } => ExportItem::Memory {
					id: self.tir.memories[memory_index as usize].id,
					internal_name: *internal_name,
					external_name,
				},
				_ => {
					self.record_type_kind_access(
						file_id,
						global_value,
						internal_name.span,
					);
					self.tir.diagnostics.push(report_cannot_export_item(
						self.interner.resolve(internal_name.inner).unwrap(),
						SourceSpan::new(file_id, internal_name.span),
					));
					continue;
				}
			};

			let (export_symbol, export_span) = match &export_item {
				ExportItem::Function {
					internal_name,
					external_name,
					..
				}
				| ExportItem::Global {
					internal_name,
					external_name,
					..
				}
				| ExportItem::Memory {
					internal_name,
					external_name,
					..
				} => {
					if let Some(ext) = external_name {
						(ext.inner, ext.span)
					} else {
						(internal_name.inner, internal_name.span)
					}
				}
			};

			match self.tir.exports.get(&export_symbol) {
				Some(existing_export) => {
					let name = self.interner.resolve(export_symbol).unwrap();
					let first_export_span = match existing_export {
						ExportItem::Function {
							internal_name,
							external_name,
							..
						}
						| ExportItem::Global {
							internal_name,
							external_name,
							..
						}
						| ExportItem::Memory {
							internal_name,
							external_name,
							..
						} => {
							if let Some(ext) = external_name {
								ext.span
							} else {
								internal_name.span
							}
						}
					};

					self.tir.diagnostics.push(report_duplicate_export(
						name,
						SourceSpan::new(file_id, first_export_span),
						SourceSpan::new(file_id, export_span),
					));
				}
				None => {
					self.tir.exports.insert(export_symbol, export_item);
				}
			}
		}
	}

	/// Resolves an enum's repr type, folds every variant's value (explicit or
	/// auto-incremented) to a `ConstValue`, range-checks it against the repr, and
	/// reports one grouped diagnostic per set of variants that collide on the same
	/// value. Writes `ty`/`variants`/`lookup` directly onto `self.tir.enums[enum_index]`.
	fn build_enum(
		&mut self,
		resolve_context: ResolveContext,
		name: &ast::Spanned<SymbolU32>,
		repr_type: Option<&ast::Spanned<ast::TypeExpression>>,
		ast_variants: &[ast::Separated<ast::Spanned<ast::EnumVariant>>],
		enum_index: EnumIndex,
	) {
		let repr_type = match repr_type {
			Some(repr_type) => {
				let resolved =
					self.resolve_type(resolve_context, None, repr_type);
				if resolved != TypeIndex::ERROR && !resolved.is_integer() {
					self.tir.diagnostics.push(report_enum_repr_not_integer(
						TypeFormatter::new(&self.tir, self.interner),
						resolved,
						SourceSpan::new(
							resolve_context.file_id,
							repr_type.span,
						),
					));
					TypeIndex::ERROR
				} else {
					resolved
				}
			}
			None => {
				self.tir.diagnostics.push(report_missing_enum_repr(
					SourceSpan::new(resolve_context.file_id, name.span),
				));
				TypeIndex::ERROR
			}
		};
		let ty_range = IntegerRange::for_integer_type(repr_type);

		let mut variants: Vec<EnumVariant> =
			Vec::with_capacity(ast_variants.len());
		let mut variant_lookup: HashMap<SymbolU32, EnumVariantIndex> =
			HashMap::with_capacity(variants.len());
		let mut next_auto_value: i64 = 0;

		for ast_variant in ast_variants.iter().map(|v| &v.inner.inner) {
			if let Some(first_index) =
				variant_lookup.get(&ast_variant.name.inner).copied()
			{
				let first_span = variants[first_index as usize].name.span;
				let vname =
					self.interner.resolve(ast_variant.name.inner).unwrap();
				self.tir.diagnostics.push(report_duplicate_definition(
					DuplicateDefinitionDiagnostic {
						name: vname,
						namespace: SymbolNamespace::Value,
						first_definition: SourceSpan::new(
							resolve_context.file_id,
							first_span,
						),
						second_definition: SourceSpan::new(
							resolve_context.file_id,
							ast_variant.name.span,
						),
					},
				));
				continue;
			}

			let (value, const_value) = match &ast_variant.value {
				// `repr_type` is `ERROR` only because it already failed to
				// resolve (missing or non-integer repr) — that's reported
				// once on the enum itself, so don't also try to type-check
				// every variant's value against it and cascade into
				// "unable to coerce"/"type annotation required" per variant.
				Some(_) if repr_type == TypeIndex::ERROR => (None, None),
				Some(value_expr) => {
					match self.build_const_context_expression(
						resolve_context,
						value_expr,
						repr_type,
					) {
						Ok(expr) => {
							let value = match self.eval_const_expr(&expr) {
								Ok(ConstValue::Int(v)) => Some(v),
								Ok(_) => None,
								Err(_) => {
									self.tir.diagnostics.push(
										report_not_const_evaluatable(
											SourceSpan::new(
												resolve_context.file_id,
												value_expr.span,
											),
										),
									);
									None
								}
							};
							(Some(Box::new(expr)), value)
						}
						Err(_) => (None, None),
					}
				}
				None => {
					if let Some(ref range) = ty_range {
						if !range.contains(next_auto_value) {
							self.tir.diagnostics.push(
								report_integer_literal_out_of_range(
									TypeFormatter::new(
										&self.tir,
										self.interner,
									),
									IntegerLiteralOutOfRangeDiagnostic {
										ty: repr_type,
										value: next_auto_value,
										span: SourceSpan::new(
											resolve_context.file_id,
											ast_variant.name.span,
										),
									},
								),
							);
						}
					}
					(None, Some(next_auto_value))
				}
			};
			next_auto_value = match const_value {
				Some(value) => value.wrapping_add(1),
				None => next_auto_value.wrapping_add(1),
			};

			let variant_index = variants.len() as EnumVariantIndex;
			variant_lookup.insert(ast_variant.name.inner, variant_index);
			variants.push(EnumVariant {
				name: ast_variant.name,
				value,
				const_value: const_value.map(ConstValue::Int),
				accesses: Vec::new(),
			});
		}

		self.report_enum_duplicate_values(
			resolve_context,
			name.span,
			&variants,
		);

		let enumeration = &mut self.tir.enums[enum_index as usize];
		enumeration.repr_type = repr_type;
		enumeration.variants = variants.into_boxed_slice();
		enumeration.variant_lookup = variant_lookup;
	}

	/// Reports one grouped diagnostic per set of variants that share the same
	/// discriminant value (rustc's `E0081`-style grouping, primary label on
	/// the enum name, one secondary label per colliding variant).
	///
	/// Runs as a single pass over the already-built `tir_variants` rather than
	/// accumulating a `HashMap<i64, Vec<SourceSpan>>` while folding: that would
	/// allocate a `Vec` for every *unique* value, even though the overwhelming
	/// majority of enums have no collisions at all and every such `Vec` would
	/// just be thrown away. Sorting by value once and scanning for runs keeps
	/// the common (no-duplicates) case to a single flat allocation.
	fn report_enum_duplicate_values(
		&mut self,
		resolve_context: ResolveContext,
		enum_name_span: ast::TextSpan,
		tir_variants: &[EnumVariant],
	) {
		let mut by_value: Vec<(i64, &EnumVariant)> = tir_variants
			.iter()
			.filter_map(|variant| match variant.const_value {
				Some(ConstValue::Int(value)) => Some((value, variant)),
				_ => None,
			})
			.collect();
		by_value.sort_unstable_by_key(|(value, _)| *value);

		let mut duplicate_groups: Vec<(i64, Vec<SourceSpan>)> = Vec::new();
		let mut i = 0;
		while i < by_value.len() {
			let mut j = i + 1;
			while j < by_value.len() && by_value[j].0 == by_value[i].0 {
				j += 1;
			}
			if j - i > 1 {
				let spans = by_value[i..j]
					.iter()
					.map(|(_, variant)| {
						// Auto-incremented variants have no explicit
						// expression (see `build_enum`) — point at the
						// variant's name instead.
						let span = variant
							.value
							.as_deref()
							.map_or(variant.name.span, |expr| expr.span);
						SourceSpan::new(resolve_context.file_id, span)
					})
					.collect();
				duplicate_groups.push((by_value[i].0, spans));
			}
			i = j;
		}

		// Grouping above is ordered by value, not by where it appears in
		// source — re-sort just the (typically few) colliding groups by their
		// earliest span so diagnostics come out in source order.
		duplicate_groups
			.sort_by_key(|(_, spans)| spans.iter().map(|s| s.span.start).min());
		for (value, spans) in &duplicate_groups {
			self.tir.diagnostics.push(report_enum_duplicate_value(
				SourceSpan::new(resolve_context.file_id, enum_name_span),
				*value,
				spans,
			));
		}
	}

	/// Evaluates the compile-time value of an already-built `Expression`, for the
	/// small subset of shapes that are actually constant. `Err(())` means "does not
	/// fold" — for the const-initializer/enum-variant callers, that doubles as "not a
	/// constant expression," since `build_expression` itself imposes no shape
	/// restriction. Recursive calls propagate `Err(())` via `?` without pushing a
	/// diagnostic; only the top-level call site reports one.
	///
	/// `Const`/`NamespaceAccess` references read the referenced constant's own
	/// already-cached `const_value` rather than re-walking its expression tree, so a
	/// chain of const-on-const arithmetic stays linear instead of blowing up.
	fn eval_const_expr(&self, expr: &Expression) -> Result<ConstValue, ()> {
		match &expr.kind {
			ExprKind::Int { value } => Ok(ConstValue::Int(*value)),
			ExprKind::Float { value } => Ok(ConstValue::Float(*value)),
			ExprKind::Bool { value } => Ok(ConstValue::Bool(*value)),
			ExprKind::Char { value } => Ok(ConstValue::Char(*value)),
			ExprKind::Unary { operator, operand } => {
				let ConstValue::Int(value) = self.eval_const_expr(operand)?
				else {
					return Err(());
				};
				match operator.inner {
					ast::UnaryOp::InvertSign => Ok(ConstValue::Int(-value)),
					ast::UnaryOp::BitNot => Ok(ConstValue::Int(!value)),
					ast::UnaryOp::Not => Err(()),
				}
			}
			ExprKind::Binary {
				operator,
				left,
				right,
			} => {
				let ConstValue::Int(left) = self.eval_const_expr(left)? else {
					return Err(());
				};
				let ConstValue::Int(right) = self.eval_const_expr(right)?
				else {
					return Err(());
				};
				match operator.inner {
					ast::BinaryOp::Add => {
						Ok(ConstValue::Int(left.wrapping_add(right)))
					}
					ast::BinaryOp::Sub => {
						Ok(ConstValue::Int(left.wrapping_sub(right)))
					}
					ast::BinaryOp::Mul => {
						Ok(ConstValue::Int(left.wrapping_mul(right)))
					}
					ast::BinaryOp::Div => {
						if right == 0 {
							Err(())
						} else {
							Ok(ConstValue::Int(left.wrapping_div(right)))
						}
					}
					ast::BinaryOp::Rem => {
						if right == 0 {
							Err(())
						} else {
							Ok(ConstValue::Int(left.wrapping_rem(right)))
						}
					}
					_ => Err(()),
				}
			}
			ExprKind::Const { id } => {
				let const_index = self.tir.expect_const_index(*id);
				self.tir.constants[const_index as usize]
					.const_value
					.ok_or(())
			}
			ExprKind::NamespaceAccess { member, .. } => {
				self.eval_const_expr(member)
			}
			_ => Err(()),
		}
	}

	/// Builds a constant-context expression (enum variant value, const initializer)
	/// via the general expression builder, using a throwaway single-scope
	/// `BodyContext` (mirrors the `Global` initializer path). `scope: None` since
	/// const expressions can never be generic and have no `Self`. Coerces untyped
	/// int/float literals to `ty` and reports a type mismatch if the result doesn't
	/// match — same idiom `Global` initializers already use. Does *not* check
	/// constant-ness; callers that need that call `eval_const_expr` on the result.
	fn build_const_context_expression(
		&mut self,
		resolve_context: ResolveContext,
		expr: &ast::Spanned<ast::Expression>,
		ty: TypeIndex,
	) -> Result<Expression, ()> {
		let root_scope = BlockScope {
			parent: None,
			label: None,
			kind: BlockKind::Block,
			span: expr.span,
			locals: Vec::new(),
			inferred_type: TypeIndex::INFER,
			expected_type: ty,
		};
		let mut func_ctx = ExprContext {
			stack: StackFrame {
				scopes: vec![root_scope],
				labels: Vec::new(),
			},
			scope_index: 0 as ScopeIndex,
			lookup: HashMap::new(),
			resolve_context,
			scope: None,
		};
		let mut value_expr = self.build_expression(
			&mut func_ctx,
			AccessContext {
				expected_type: ty,
				access_kind: AccessKind::Read,
			},
			expr,
		)?;

		if value_expr.ty.is_comptime_number() && ty != TypeIndex::INFER {
			_ = self.coerce_untyped_expr(&mut func_ctx, &mut value_expr, ty);
		}

		if value_expr.ty.is_comptime_number() {
			self.tir.diagnostics.push(report_type_annotation_required(
				SourceSpan::new(resolve_context.file_id, expr.span),
			));
			return Err(());
		}
		if ty != TypeIndex::ERROR && !self.coercible_to(value_expr.ty, ty) {
			self.tir.diagnostics.push(report_type_mistmatch(
				TypeFormatter::new(&self.tir, self.interner),
				TypeMistmatchDiagnostic {
					expected_type: ty,
					actual_type: value_expr.ty,
					span: SourceSpan::new(resolve_context.file_id, expr.span),
				},
			));
			return Err(());
		}
		Ok(value_expr)
	}

	fn build_function_body(
		&mut self,
		resolve_context: ResolveContext,
		scope: &GenericScope,
		signature: &ast::FunctionSignature,
		block: &Spanned<ast::Expression>,
		func_index: FunctionIndex,
	) -> Result<FunctionBody, ()> {
		let lookup = signature
			.params
			.iter()
			.enumerate()
			.map(|(index, param)| {
				(
					(0 as ScopeIndex, param.inner.inner.name.inner),
					index as LocalIndex,
				)
			})
			.collect();

		let root_scope = BlockScope {
			parent: None,
			label: None,
			kind: BlockKind::Block,
			span: block.span,
			locals: self.tir.functions[func_index as usize]
				.params
				.iter()
				.map(|param| Local {
					name: param.name,
					accesses: Vec::new(),
					mut_span: param.mut_span,
					ty: param.ty.inner,
				})
				.collect(),
			inferred_type: TypeIndex::INFER,
			expected_type: self.tir.functions[func_index as usize]
				.result
				.map(|ty| ty.inner)
				.unwrap_or(TypeIndex::UNIT),
		};

		let mut ctx = ExprContext {
			stack: StackFrame {
				scopes: vec![root_scope],
				labels: Vec::new(),
			},
			scope_index: 0 as ScopeIndex,
			lookup,
			resolve_context,
			scope: Some(GenericScope {
				owner: scope.owner,
				self_type: scope.self_type,
			}),
		};
		let result = self.build_block_expression(&mut ctx, block)?;
		self.report_stack_warnings(ctx.resolve_context.file_id, &ctx.stack);
		Ok(FunctionBody {
			block: Box::new(result),
			stack: ctx.stack,
		})
	}

	fn build_block_expression(
		&mut self,
		ctx: &mut ExprContext,
		block: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let statements = match &block.inner {
			ast::Expression::Block { statements } => statements,
			_ => unreachable!(),
		};

		let (statements, result) = match statements.split_last() {
			Some((last, rest)) if last.separator.is_none() => match &last
				.inner
				.inner
			{
				ast::Statement::Expression(expr) => (rest, Some(expr.as_ref())),
				_ => (statements.as_ref(), None),
			},
			_ => (statements.as_ref(), None),
		};

		let expressions = match self.build_block_statements(ctx, statements) {
			BlockState::Exhaustive(expressions) => {
				let unreachable_start = statements
					.get(expressions.len())
					.map(|s| s.inner.span.start)
					.or_else(|| result.as_ref().map(|r| r.span.start));

				let unreachable_end = result
					.map(|r| r.span.end)
					.or_else(|| statements.last().map(|s| s.inner.span.end));

				if let (Some(start), Some(end)) =
					(unreachable_start, unreachable_end)
				{
					self.tir.diagnostics.push(report_unreachable_code(
						SourceSpan::new(
							ctx.resolve_context.file_id,
							TextSpan::new(start, end),
						),
					));
				}

				let scope = &mut ctx.stack.scopes[ctx.scope_index as usize];
				let inferred_type =
					scope.inferred_type.infer_or(TypeIndex::NEVER);
				scope.inferred_type = inferred_type;

				return Ok(Expression {
					kind: ExprKind::Block {
						scope_index: ctx.scope_index,
						expressions,
						result: None,
					},
					ty: inferred_type,
					span: block.span,
				});
			}
			BlockState::Incomplete(expressions) => expressions,
		};

		match ctx.stack.scopes[ctx.scope_index as usize].kind {
			BlockKind::Loop => {
				let result = match result {
					Some(result) => Some(self.build_expression(
						ctx,
						AccessContext {
							expected_type: TypeIndex::UNIT,
							access_kind: AccessKind::Read,
						},
						result,
					)?),
					None => None,
				};

				let scope = &ctx.stack.scopes[ctx.scope_index as usize];
				let inferred_type =
					scope.inferred_type.infer_or(TypeIndex::NEVER);
				Ok(Expression {
					kind: ExprKind::Block {
						scope_index: ctx.scope_index,
						expressions,
						result: result.map(Box::new),
					},
					ty: inferred_type,
					span: block.span,
				})
			}
			BlockKind::Block => {
				let result = self.build_block_result(ctx, result)?;

				let scope = &ctx.stack.scopes[ctx.scope_index as usize];
				let inferred_type = scope.inferred_type;
				let expected_type = scope.expected_type;
				let block_ty = if expected_type != TypeIndex::INFER
					&& !self.coercible_to(inferred_type, expected_type)
				{
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type,
							actual_type: inferred_type,
							span: SourceSpan::new(
								ctx.resolve_context.file_id,
								block.span,
							),
						},
					));
					TypeIndex::ERROR
				} else {
					inferred_type
				};

				Ok(Expression {
					kind: ExprKind::Block {
						scope_index: ctx.scope_index,
						expressions,
						result: result.map(Box::new),
					},
					ty: block_ty,
					span: block.span,
				})
			}
		}
	}

	fn report_stack_warnings(&mut self, file_id: FileId, stack: &StackFrame) {
		for label in stack.labels.iter() {
			if label.accesses.is_empty() {
				self.tir.diagnostics.push(
					Diagnostic::warning()
						.with_code(DiagnosticCode::UnusedLabel.code())
						.with_message("unused label")
						.with_label(
							SourceSpan::new(file_id, label.name.span)
								.primary_label(),
						),
				);
			}
		}

		for scope in stack.scopes.iter() {
			for local in scope.locals.iter() {
				if local.accesses.is_empty() && local.ty != TypeIndex::ERROR {
					self.tir.diagnostics.push(report_unused_variable(
						SourceSpan::new(file_id, local.name.span),
					));
				}

				match local.mut_span {
					Some(mut_span)
						if !local.accesses.iter().any(|access| {
							access.kind == AccessKind::Write
								|| access.kind == AccessKind::ReadWrite
						}) =>
					{
						self.tir.diagnostics.push(
							report_unnecessary_mutability(SourceSpan::new(
								file_id, mut_span,
							)),
						);
					}
					_ => {}
				}
			}
		}
	}

	/// Emits a diagnostic for each bound on `assoc_name` that `concrete_ty`
	/// does not satisfy. `error_span` is where the type was written.
	fn check_assoc_type_bounds(
		&mut self,
		file_id: FileId,
		trait_index: TraitIndex,
		assoc_name: SymbolU32,
		concrete_ty: TypeIndex,
		error_span: TextSpan,
	) {
		let Some(at) = self.tir.traits[trait_index as usize]
			.assoc_types
			.get(&assoc_name)
		else {
			return;
		};

		let assoc_name_str =
			self.interner.resolve(assoc_name).unwrap_or("?").to_string();

		for bound in &at.bounds.traits {
			if !self
				.tir
				.trait_impl_lookup
				.contains_key(&(concrete_ty, bound.trait_index))
			{
				let bound_name = self
					.interner
					.resolve(
						self.tir.traits[bound.trait_index as usize].name.inner,
					)
					.unwrap()
					.to_string();
				let type_name = TypeFormatter::new(&self.tir, self.interner)
					.display_type(concrete_ty)
					.unwrap();
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::TypeMistmatch.code())
						.with_message(format!(
							"associated type `{assoc_name_str}` must implement `{bound_name}`",
						))
						.with_label(Label::primary(file_id, error_span))
						.with_note(format!(
							"`{type_name}` does not implement `{bound_name}`"
						)),
				);
			}
		}

		if let Some(typeset_index) = at.bounds.typeset {
			if !self.concrete_type_in_typeset(concrete_ty, typeset_index) {
				let ts_name = self
					.interner
					.resolve(
						self.tir.typesets[typeset_index as usize].name.inner,
					)
					.unwrap_or("?")
					.to_string();
				let type_name = TypeFormatter::new(&self.tir, self.interner)
					.display_type(concrete_ty)
					.unwrap();
				self.tir.diagnostics.push(
                    Diagnostic::error()
                        .with_code(DiagnosticCode::TypesetBoundViolation.code())
                        .with_message(format!(
                            "associated type `{assoc_name_str}` must be a member of typeset `{ts_name}`",
                        ))
                        .with_label(Label::primary(file_id, error_span))
                        .with_note(format!("`{type_name}` is not a member of typeset `{ts_name}`")),
                );
			}
		}
	}

	fn check_trait_conformance(&mut self) {
		enum Violation {
			MissingItem {
				file_id: FileId,
				span: TextSpan,
				item_sym: SymbolU32,
				trait_sym: SymbolU32,
				kind: &'static str,
			},
			MissingSupertrait {
				file_id: FileId,
				span: TextSpan,
				trait_sym: SymbolU32,
				supertrait_sym: SymbolU32,
			},
		}

		let mut violations: Vec<Violation> = Vec::new();

		for ti in &self.tir.trait_impls {
			let trait_ = &self.tir.traits[ti.trait_index as usize];

			// `ti.members` = only what the impl block explicitly provided.
			// `impl_members[target]` = full dispatch table including seeded
			// defaults. We use `ti.members` intentionally: a default method
			// must not satisfy an abstract (no-body) requirement.
			for (&sym, entry) in &trait_.members {
				// `body.is_none()` distinguishes abstract from default methods.
				// Requires Phase 3 to have populated bodies before this runs.
				let (required, kind) = match entry {
					ImplEntry::Method(fi) => {
						(self.tir.functions[*fi as usize].body.is_none(), "fn")
					}
					ImplEntry::AssociatedConst { .. } => (true, "const"),
					ImplEntry::AssociatedType { .. } => (true, "type"),
					_ => continue,
				};
				if required && !ti.members.contains_key(&sym) {
					violations.push(Violation::MissingItem {
						file_id: ti.file_id,
						span: ti.span,
						item_sym: sym,
						trait_sym: trait_.name.inner,
						kind,
					});
				}
			}

			for &supertrait_index in &trait_.supertraits {
				if !self
					.tir
					.trait_impl_lookup
					.contains_key(&(ti.target, supertrait_index))
				{
					let supertrait_sym =
						self.tir.traits[supertrait_index as usize].name.inner;
					violations.push(Violation::MissingSupertrait {
						file_id: ti.file_id,
						span: ti.span,
						trait_sym: trait_.name.inner,
						supertrait_sym,
					});
				}
			}
		}

		for v in violations {
			match v {
				Violation::MissingItem {
					file_id,
					span,
					item_sym,
					trait_sym,
					kind,
				} => {
					let item_name = self.interner.resolve(item_sym).unwrap();
					let trait_name = self.interner.resolve(trait_sym).unwrap();
					self.tir.diagnostics.push(
						Diagnostic::error()
							.with_code(
								DiagnosticCode::MissingTraitImplItem.code(),
							)
							.with_message(format!(
								"missing {} `{}` required by `{}`",
								kind, item_name, trait_name
							))
							.with_label(Label::primary(file_id, span)),
					);
				}
				Violation::MissingSupertrait {
					file_id,
					span,
					trait_sym,
					supertrait_sym,
				} => {
					let trait_name =
						self.interner.resolve(trait_sym).unwrap_or("?");
					let supertrait_name =
						self.interner.resolve(supertrait_sym).unwrap_or("?");
					self.tir.diagnostics.push(
						Diagnostic::error()
							.with_code(
								DiagnosticCode::MissingSupertraitImpl.code(),
							)
							.with_message(format!(
								"cannot implement `{}` without implementing supertrait `{}`",
								trait_name, supertrait_name
							))
							.with_label(Label::primary(file_id, span)),
					);
				}
			}
		}
	}

	fn report_unused_items(&mut self) {
		let code = DiagnosticCode::UnusedItem.code();
		let type_param_code = DiagnosticCode::UnusedTypeParam.code();

		for function in self.tir.functions.iter() {
			let is_intrinsic =
				function.attributes.contains(&ItemAttribute::Intrinsic);
			let is_imported = function
				.namespace
				.map(|ns| {
					matches!(
						self.tir.namespaces[ns as usize].declaration,
						ModuleDeclarationKind::Import(_)
					)
				})
				.unwrap_or(false);
			if is_intrinsic || is_imported {
				continue;
			}
			if function.accesses.is_empty()
				&& function.pub_span.is_none()
				&& !matches!(
					function.type_param_parent,
					Some(TypeParamOwner::Trait(_))
				) {
				let name = self.interner.resolve(function.name.inner).unwrap();
				self.tir.diagnostics.push(
					Diagnostic::warning()
						.with_code(code)
						.with_message(format!(
							"function `{}` is never used",
							name
						))
						.with_label(
							SourceSpan::new(
								function.file_id,
								function.name.span,
							)
							.primary_label(),
						),
				);
			}

			for param in function.type_params.iter() {
				if param.accesses.is_empty() {
					let name = self.interner.resolve(param.name).unwrap();
					self.tir.diagnostics.push(
						Diagnostic::warning()
							.with_code(type_param_code)
							.with_message(format!(
								"type parameter `{name}` is never used"
							))
							.with_label(
								SourceSpan::new(
									function.file_id,
									param.name_span,
								)
								.primary_label(),
							)
							.with_note(
								"consider removing this type parameter or using it in signature",
							),
					);
				}
			}
		}

		for global in self.tir.globals.iter() {
			let is_imported = global
				.namespace
				.map(|ns| {
					matches!(
						self.tir.namespaces[ns as usize].declaration,
						ModuleDeclarationKind::Import(_)
					)
				})
				.unwrap_or(false);
			if !is_imported && global.accesses.is_empty() {
				let name = self.interner.resolve(global.name.inner).unwrap();
				self.tir.diagnostics.push(
					Diagnostic::warning()
						.with_code(code)
						.with_message(format!(
							"global variable `{}` is never used",
							name
						))
						.with_label(
							SourceSpan::new(global.file_id, global.name.span)
								.primary_label(),
						),
				);
			}
		}

		for constant in self.tir.constants.iter() {
			if constant.pub_span.is_none()
				&& constant.accesses.is_empty()
				&& constant.value.is_some()
			{
				let name = self.interner.resolve(constant.name.inner).unwrap();
				self.tir.diagnostics.push(
					Diagnostic::warning()
						.with_code(code)
						.with_message(format!("const `{}` is never used", name))
						.with_label(
							SourceSpan::new(
								constant.file_id,
								constant.name.span,
							)
							.primary_label(),
						),
				);
			}
		}

		let field_code = DiagnosticCode::UnusedStructField.code();

		for struct_ in self.tir.structs.iter() {
			if struct_.pub_span.is_none() && struct_.accesses.is_empty() {
				let name = self.interner.resolve(struct_.name.inner).unwrap();
				self.tir.diagnostics.push(
					Diagnostic::warning()
						.with_code(code)
						.with_message(format!(
							"struct `{}` is never used",
							name
						))
						.with_label(
							SourceSpan::new(struct_.file_id, struct_.name.span)
								.primary_label(),
						),
				);
			} else {
				// Struct is live — warn about fields that are initialized but never read.
				for field in struct_.fields.iter() {
					if field.pub_span.is_some() {
						continue;
					}
					let has_read = field
						.accesses
						.iter()
						.any(|a| matches!(a.kind, FieldAccessKind::Read));
					let has_init = field
						.accesses
						.iter()
						.any(|a| matches!(a.kind, FieldAccessKind::Init));
					if has_init && !has_read {
						let name =
							self.interner.resolve(field.name.inner).unwrap();
						self.tir.diagnostics.push(
							Diagnostic::warning()
								.with_code(field_code)
								.with_message(format!(
									"field `{name}` is never read"
								))
								.with_label(
									SourceSpan::new(
										struct_.file_id,
										field.name.span,
									)
									.primary_label(),
								),
						);
					}
				}
			}
		}

		for enum_index in 0..self.tir.enums.len() as EnumIndex {
			let enum_ = &self.tir.enums[enum_index as usize];
			if enum_.pub_span.is_none() && enum_.accesses.is_empty() {
				let name = self.interner.resolve(enum_.name.inner).unwrap();
				self.tir.diagnostics.push(
					Diagnostic::warning()
						.with_code(code)
						.with_message(format!("enum `{}` is never used", name))
						.with_label(
							SourceSpan::new(enum_.file_id, enum_.name.span)
								.primary_label(),
						),
				);
				continue;
			}

			let unused_variants: Box<_> = enum_
				.variants
				.iter()
				.filter(|v| v.accesses.is_empty())
				.map(|v| v.name)
				.collect();
			if unused_variants.is_empty() {
				continue;
			}
			let diagnostic = report_unused_enum_variants(
				self.interner,
				enum_.file_id,
				&unused_variants,
			);
			self.tir.diagnostics.push(diagnostic);
		}
	}

	fn build_block_result(
		&mut self,
		ctx: &mut ExprContext,
		result: Option<&Spanned<ast::Expression>>,
	) -> Result<Option<Expression>, ()> {
		match result {
			Some(result) => {
				let mut result = self.build_expression(
					ctx,
					AccessContext {
						expected_type: ctx.stack.scopes
							[ctx.scope_index as usize]
							.expected_type,
						access_kind: AccessKind::Read,
					},
					result,
				)?;

				let scope = &mut ctx.stack.scopes[ctx.scope_index as usize];
				let inferred_type = self.infer_block_type(
					ctx.resolve_context.file_id,
					scope,
					&result,
				)?;
				scope.inferred_type = inferred_type;
				if result.ty.is_comptime_number()
					&& !inferred_type.is_comptime_number()
				{
					_ = self.coerce_untyped_expr(
						ctx,
						&mut result,
						inferred_type,
					);
				}

				Ok(Some(result))
			}
			None => {
				let scope = &mut ctx.stack.scopes[ctx.scope_index as usize];
				let inferred_type =
					scope.inferred_type.infer_or(TypeIndex::UNIT);
				scope.inferred_type = inferred_type;

				Ok(None)
			}
		}
	}

	fn build_block_statements(
		&mut self,
		ctx: &mut ExprContext,
		statements: &[Separated<Spanned<ast::Statement>>],
	) -> BlockState {
		let mut expressions = Vec::with_capacity(statements.len());
		for stmt in statements.iter() {
			let result = match &stmt.inner.inner {
				ast::Statement::Expression(_) => {
					self.build_expression_statement(ctx, &stmt.inner.inner)
				}
				ast::Statement::LocalDefinition { .. } => {
					self.build_local_definition_statement(ctx, stmt)
				}
			};
			let expr = match result {
				Ok(expr) => expr,
				Err(_) => continue,
			};

			match expr.ty {
				_ if expr.ty == TypeIndex::NEVER => {
					expressions.push(expr);
					return BlockState::Exhaustive(
						expressions.into_boxed_slice(),
					);
				}
				_ => {
					// Expression statement with unused value (already reported as warning)
					// Treat it as a Unit statement
					expressions.push(expr);
				}
			}
		}

		BlockState::Incomplete(expressions.into_boxed_slice())
	}

	fn infer_block_type(
		&mut self,
		file_id: FileId,
		scope: &BlockScope,
		value: &Expression,
	) -> Result<TypeIndex, ()> {
		if value.ty.is_comptime_number() {
			let coerce_to = scope.inferred_type.infer_or(scope.expected_type);
			if coerce_to != TypeIndex::INFER {
				return Ok(coerce_to);
			} else {
				// No type context — let the comptime type bubble up. The caller
				// (e.g. build_if_else_expression) may resolve it via the other branch.
				return Ok(value.ty);
			}
		}
		let result_type = value.ty;
		if scope.inferred_type != TypeIndex::INFER {
			let inferred_type = scope.inferred_type;
			if !self.coercible_to(result_type, inferred_type) {
				self.tir.diagnostics.push(report_type_mistmatch(
					TypeFormatter::new(&self.tir, self.interner),
					TypeMistmatchDiagnostic {
						expected_type: inferred_type,
						actual_type: result_type,
						span: SourceSpan::new(file_id, value.span),
					},
				));
			}
			Ok(inferred_type)
		} else if scope.expected_type != TypeIndex::INFER {
			let expected_type = scope.expected_type;
			if !self.coercible_to(result_type, expected_type) {
				self.tir.diagnostics.push(report_type_mistmatch(
					TypeFormatter::new(&self.tir, self.interner),
					TypeMistmatchDiagnostic {
						expected_type,
						actual_type: result_type,
						span: SourceSpan::new(file_id, value.span),
					},
				));
				return Err(());
			}
			Ok(result_type)
		} else {
			Ok(result_type)
		}
	}

	fn build_expression_statement(
		&mut self,
		ctx: &mut ExprContext,
		stmt: &ast::Statement,
	) -> Result<Expression, ()> {
		let value = match &stmt {
			ast::Statement::Expression(value) => value,
			_ => unreachable!(),
		};

		let value = self.build_expression(
			ctx,
			AccessContext {
				access_kind: AccessKind::Read,
				expected_type: TypeIndex::INFER,
			},
			value,
		)?;
		if value.ty == TypeIndex::UNIT {
			return Ok(value);
		} else if value.ty == TypeIndex::ERROR {
			// Skip reporting unused value for error types, as the error has already been
			// reported
			return Ok(value);
		} else if value.ty == TypeIndex::NEVER {
			let scope =
				ctx.stack.scopes.get_mut(ctx.scope_index as usize).unwrap();
			if scope.inferred_type == TypeIndex::INFER {
				scope.inferred_type = TypeIndex::NEVER;
			}
			return Ok(value);
		} else if value.ty.is_comptime_number() {
			self.tir.diagnostics.push(report_type_annotation_required(
				SourceSpan::new(ctx.resolve_context.file_id, value.span),
			));
			return Err(());
		}
		self.tir
			.diagnostics
			.push(report_unused_value(SourceSpan::new(
				ctx.resolve_context.file_id,
				value.span,
			)));
		Ok(value)
	}

	fn build_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		match &expr.inner {
			ast::Expression::Int { value } => Ok(Expression {
				kind: ExprKind::Int { value: *value },
				ty: TypeIndex::INTEGER,
				span: expr.span,
			}),
			ast::Expression::Float { value } => Ok(Expression {
				kind: ExprKind::Float { value: *value },
				ty: TypeIndex::FLOAT,
				span: expr.span,
			}),
			ast::Expression::Unreachable => Ok(Expression {
				kind: ExprKind::Unreachable,
				ty: TypeIndex::NEVER,
				span: expr.span,
			}),
			ast::Expression::True => Ok(Expression {
				kind: ExprKind::Bool { value: true },
				ty: TypeIndex::BOOL,
				span: expr.span,
			}),
			ast::Expression::False => Ok(Expression {
				kind: ExprKind::Bool { value: false },
				ty: TypeIndex::BOOL,
				span: expr.span,
			}),
			ast::Expression::Placeholder => Ok(Expression {
				kind: ExprKind::Placeholder,
				ty: access_ctx.expected_type.infer_or(TypeIndex::ERROR),
				span: expr.span,
			}),
			ast::Expression::Error => Ok(Expression {
				kind: ExprKind::Error,
				ty: TypeIndex::ERROR,
				span: expr.span,
			}),
			ast::Expression::String => {
				let source = &self
					.files
					.get(func_ctx.resolve_context.file_id)
					.unwrap()
					.source;
				let raw = expr.span.extract_str(source);
				let unescaped = unescape_string(raw);
				let symbol = self.interner.get_or_intern(&unescaped);
				let memory_ty =
					self.resolve_ambient_memory(SourceSpan::new(
						func_ctx.resolve_context.file_id,
						expr.span,
					))?;
				Ok(Expression {
					kind: ExprKind::String { symbol },
					ty: self.intern_type(Type::Slice {
						of: TypeIndex::U8,
						memory: memory_ty,
						mutable: false,
					}),
					span: expr.span,
				})
			}
			ast::Expression::Char => {
				let source = &self
					.files
					.get(func_ctx.resolve_context.file_id)
					.unwrap()
					.source;
				let raw = expr.span.extract_str(source);
				match parse_char_literal(raw) {
					Ok(value) => Ok(Expression {
						kind: ExprKind::Char { value },
						ty: TypeIndex::CHAR,
						span: expr.span,
					}),
					Err(CharLiteralError::Empty) => {
						self.tir.diagnostics.push(report_empty_char_literal(
							SourceSpan::new(
								func_ctx.resolve_context.file_id,
								expr.span,
							),
						));
						Err(())
					}
					Err(CharLiteralError::TooLong) => {
						self.tir.diagnostics.push(
							report_char_literal_too_long(SourceSpan::new(
								func_ctx.resolve_context.file_id,
								expr.span,
							)),
						);
						Err(())
					}
				}
			}
			ast::Expression::Path(path) => self
				.build_path_expression(func_ctx, access_ctx, path, expr.span),
			ast::Expression::Binary { .. } => {
				self.build_binary_expression(func_ctx, access_ctx, expr)
			}
			ast::Expression::Grouping { value } => {
				self.build_expression(func_ctx, access_ctx, value)
			}
			ast::Expression::Unary { .. } => {
				self.build_unary_expression(func_ctx, access_ctx, expr)
			}
			ast::Expression::Call { .. } => {
				self.build_call_expression(func_ctx, access_ctx, expr)
			}
			ast::Expression::MethodCall(_) => {
				self.build_method_call_expression(func_ctx, access_ctx, expr)
			}
			ast::Expression::ObjectAccess { object, member } => self
				.build_object_access_expression(
					func_ctx, access_ctx, object, *member, expr.span,
				),
			ast::Expression::Deref { pointer } => self.build_deref_expression(
				func_ctx, access_ctx, expr.span, pointer,
			),
			ast::Expression::Return { .. } => {
				self.build_return_expression(func_ctx, expr)
			}
			ast::Expression::Block { .. } => func_ctx.enter_block(
				BlockScope {
					label: None,
					kind: BlockKind::Block,
					parent: Some(func_ctx.scope_index),
					span: expr.span,
					locals: Vec::new(),
					inferred_type: TypeIndex::INFER,
					expected_type: access_ctx.expected_type,
				},
				|ctx| self.build_block_expression(ctx, expr),
			),
			ast::Expression::IfElse { .. } => {
				self.build_if_else_expression(func_ctx, access_ctx, expr, None)
			}
			ast::Expression::Loop { .. } => {
				self.build_loop_expression(func_ctx, access_ctx, expr, None)
			}
			ast::Expression::Cast { .. } => {
				self.build_cast_expression(func_ctx, access_ctx, expr)
			}
			ast::Expression::Break { .. } => {
				self.build_break_expression(func_ctx, expr)
			}
			ast::Expression::Continue { .. } => {
				self.build_continue_expression(func_ctx, expr)
			}
			ast::Expression::Label { .. } => {
				self.build_label_expression(func_ctx, access_ctx, expr)
			}
			ast::Expression::StructInit { path, fields } => self
				.build_struct_init_expression(
					func_ctx, access_ctx, expr.span, path, fields,
				),
			ast::Expression::Tuple { elements } => self.build_tuple_expression(
				func_ctx, expr.span, elements, access_ctx,
			),
			ast::Expression::TypeApplication { callee, args } => self
				.build_type_application_expression(
					func_ctx, callee, args, expr.span,
				),
			ast::Expression::ArrayList { elements } => self
				.build_array_literal_expression(
					func_ctx, access_ctx, expr.span, elements,
				),
			ast::Expression::ArrayRepeat { value, count } => self
				.build_array_repeat_expression(
					func_ctx, access_ctx, expr.span, value, count,
				),
			ast::Expression::Index { object, index } => self
				.build_index_expression(
					func_ctx, access_ctx, expr.span, object, index,
				),
			ast::Expression::SliceRange { object, start, end } => self
				.build_slice_range_expression(
					func_ctx, expr.span, object, start, end,
				),
			ast::Expression::AddressOf { value, mut_span } => {
				let mutable = mut_span.is_some();
				let operand = self.build_expression(
					func_ctx,
					AccessContext {
						expected_type: TypeIndex::INFER,
						access_kind: AccessKind::Read,
					},
					value,
				)?;
				match operand.kind {
					ExprKind::Load { place } => {
						if mutable && !place.mutable {
							self.tir.diagnostics.push(
								report_cannot_take_mutable_address_of_immutable(
									SourceSpan::new(
										func_ctx.resolve_context.file_id,
										expr.span,
									),
								),
							);
						}
						let pointer_ty = self.intern_type(Type::Pointer {
							to: place.ty,
							memory: place.memory,
							mutable,
						});
						Ok(Expression {
							kind: ExprKind::AddressOf { place, mutable },
							ty: pointer_ty,
							span: expr.span,
						})
					}
					_ => {
						self.tir.diagnostics.push(
							report_cannot_take_address_of_value(
								SourceSpan::new(
									func_ctx.resolve_context.file_id,
									operand.span,
								),
							),
						);
						Err(())
					}
				}
			}
		}
	}

	/// O(1) lookup in the `generic_impl_dispatch` index, then type-arg
	/// extraction via the existing `infer_type_args_from_types`.  Returns the
	/// block index and the concrete substitution for each type param.
	fn find_generic_impl(
		&mut self,
		receiver_ty: TypeIndex,
		member_name: SymbolU32,
	) -> Option<(usize, Box<[TypeIndex]>)> {
		let outer_kind = GenericImplTargetKind::from_type(
			&self.tir.types[receiver_ty.as_usize()],
		)?;
		let &block_idx = self
			.tir
			.generic_impl_dispatch
			.get(&(outer_kind, member_name))?;
		let (block_target, type_params_len) = {
			let block = &self.tir.generic_impl_list[block_idx];
			(block.target, block.type_params.len())
		};
		let mut type_args: Vec<TypeIndex> =
			vec![TypeIndex::INFER; type_params_len];
		self.infer_type_args(&mut type_args, block_target, receiver_ty);
		// Slots that couldn't be resolved from the receiver stay INFER so the
		// call site can still fill them in from explicit type arguments or args.
		Some((block_idx, type_args.into_boxed_slice()))
	}

	fn build_object_access_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		object: &Spanned<ast::Expression>,
		member: Spanned<SymbolU32>,
		expr_span: TextSpan,
	) -> Result<Expression, ()> {
		let object = self.build_expression(func_ctx, access_ctx, object)?;
		let entry = match &self.tir.types[object.ty.as_usize()] {
			Type::TypeParam { owner, param_index } => self
				.tir
				.type_param_info(*owner, *param_index as usize)
				.bounds
				.traits
				.iter()
				.find_map(|b| {
					self.tir.traits[b.trait_index as usize]
						.members
						.get(&member.inner)
						.cloned()
				}),
			_ => self
				.tir
				.impl_members
				.get(&object.ty)
				.and_then(|m| m.get(&member.inner))
				.cloned(),
		};
		match entry {
            Some(ImplEntry::AssociatedFn(_)) => {
                // TODO: emit "this is an associated function, use Type::name()
                // syntax" diagnostic
            }
            Some(ImplEntry::AssociatedConst { ty, .. }) => {
                return Ok(Expression {
                    kind: ExprKind::ObjectAccess {
                        object: Box::new(object),
                        member,
                    },
                    ty,
                    span: expr_span,
                });
            }
            // Methods are only callable via `obj.method()` — not accessible as values via
            // dot syntax. Fall through to the struct-field lookup below.
            // TODO: improve the error for `obj.method` (no parens) to say "did you mean to
            // call this method with `()`?"
            Some(ImplEntry::Method(_))
            // Associated types are type-level constructs, not value members.
            | Some(ImplEntry::AssociatedType { .. })
            | None => {}
        }

		// Check struct fields
		if let Type::Struct { struct_index, args } =
			self.tir.types[object.ty.as_usize()].clone()
		{
			if let Some(&field_index) = self.tir.structs[struct_index as usize]
				.lookup
				.get(&member.inner)
			{
				let raw_field_ty = self.tir.structs[struct_index as usize]
					.fields[field_index]
					.ty
					.inner;
				let field_ty = if args.is_empty() {
					raw_field_ty
				} else {
					self.substitute_type(raw_field_ty, &args)
				};
				self.tir.structs[struct_index as usize].fields[field_index]
					.accesses
					.push(FieldAccess {
						kind: FieldAccessKind::Read,
						file_id: func_ctx.resolve_context.file_id,
						span: member.span,
					});
				// Reuse object_ty / object_span so we can move object.kind in the match.
				let object_ty = object.ty;
				let object_span = object.span;
				return match object.kind {
					// Object lives in linear memory — chain into a Field place.
					ExprKind::Load { place } => {
						let memory = place.memory;
						let mutable = place.mutable;
						Ok(Expression {
							kind: ExprKind::Load {
								place: Box::new(Place {
									kind: PlaceKind::Field {
										object: place,
										member,
									},
									ty: field_ty,
									memory,
									mutable,
									span: expr_span,
								}),
							},
							ty: field_ty,
							span: expr_span,
						})
					}
					// Object is a stack value — keep the existing ObjectAccess path.
					object_kind => Ok(Expression {
						kind: ExprKind::ObjectAccess {
							object: Box::new(Expression {
								kind: object_kind,
								ty: object_ty,
								span: object_span,
							}),
							member,
						},
						ty: field_ty,
						span: expr_span,
					}),
				};
			}
		}

		self.tir.diagnostics.push(report_undeclared_identifier(
			SourceSpan::new(func_ctx.resolve_context.file_id, member.span),
		));
		Err(())
	}

	/// Build a TIR expression from a parsed `Path`.
	///
	/// - Single segment, no type args  → identifier / local / global lookup
	/// - Single segment, with type args → generic function reference
	/// - Multiple segments              → resolve each leading segment as a
	///   namespace `TypeIndex`, then dispatch via
	///   `build_namespace_member_expression`
	fn build_path_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		path: &[ast::PathSegment],
		expr_span: TextSpan,
	) -> Result<Expression, ()> {
		let last = path.last().expect("path is non-empty");

		// ── single-segment, no type args: plain identifier / local / global ───
		if path.len() == 1 && last.type_args.is_empty() {
			let symbol = last.ident.inner;
			return match self.resolve_symbol(func_ctx, symbol) {
				Some(resolved) => self.resolved_symbol_to_expression(
					func_ctx, access_ctx, resolved, expr_span,
				),
				None => {
					self.tir.diagnostics.push(report_undeclared_identifier(
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							expr_span,
						),
					));
					Ok(Expression {
						kind: ExprKind::Error,
						ty: access_ctx.expected_type.infer_or(TypeIndex::ERROR),
						span: expr_span,
					})
				}
			};
		}

		// ── single-segment with type args: generic function reference ──────────
		if path.len() == 1 {
			let seg = &path[0];
			let func_index = match self
				.lookup_global_symbol(
					func_ctx.resolve_context.namespace,
					(SymbolNamespace::Value, seg.ident.inner),
				)
				.filter(|k| !matches!(k, SymbolKind::Pending(_)))
			{
				Some(SymbolKind::Function { func_index }) => func_index,
				_ => {
					self.tir.diagnostics.push(report_undeclared_identifier(
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							expr_span,
						),
					));
					return Ok(Expression {
						kind: ExprKind::Error,
						ty: TypeIndex::ERROR,
						span: expr_span,
					});
				}
			};

			let type_params_len =
				self.tir.functions[func_index as usize].type_params.len();
			if type_params_len == 0 {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::TypeArgCountMismatch.code())
						.with_message("function is not generic")
						.with_label(
							SourceSpan::new(
								func_ctx.resolve_context.file_id,
								expr_span,
							)
							.primary_label()
							.with_message(
								"type arguments provided but this function has no type parameters",
							),
						),
				);
				return Ok(Expression {
					kind: ExprKind::Error,
					ty: TypeIndex::ERROR,
					span: expr_span,
				});
			}
			if seg.type_args.len() > type_params_len {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::TypeArgCountMismatch.code())
						.with_message(format!(
							"expected {} type argument{}, found {}",
							type_params_len,
							if type_params_len == 1 { "" } else { "s" },
							seg.type_args.len()
						))
						.with_label(
							SourceSpan::new(
								func_ctx.resolve_context.file_id,
								expr_span,
							)
							.primary_label()
							.with_message("wrong number of type arguments"),
						),
				);
			}

			let type_params =
				self.tir.functions[func_index as usize].type_params.len();
			let resolved_args: Box<[TypeIndex]> = seg
				.type_args
				.iter()
				.map(|arg| {
					self.resolve_type(
						func_ctx.resolve_context,
						func_ctx.scope,
						arg,
					)
				})
				.chain(std::iter::repeat(TypeIndex::INFER))
				.take(type_params)
				.collect();
			let func = &mut self.tir.functions[func_index as usize];
			func.accesses.push(SourceSpan::new(
				func_ctx.resolve_context.file_id,
				seg.ident.span,
			));
			let func_id = func.id;
			let ty = self.intern_type(Type::FunctionItem {
				id: func_id,
				type_args: resolved_args,
			});
			return Ok(Expression {
				kind: ExprKind::Function { id: func_id },
				ty,
				span: expr_span,
			});
		}

		// ── multi-segment: resolve namespace chain then dispatch on last member ─
		// Walk segments[0..n-1] left-to-right: each resolves to a namespace TypeIndex.
		let first = &path[0];
		let mut namespace_ty = self.resolve_type_identifier(
			func_ctx.resolve_context,
			func_ctx.scope,
			first.ident,
		)?;
		let mut namespace_span = first.ident.span;

		// Apply turbofish type args on the first segment when present, e.g.
		// `Wrapper::<u32>::new(...)` → instantiate to `Wrapper<u32>`.
		if !first.type_args.is_empty() {
			let resolve_context = func_ctx.resolve_context;
			let struct_index = match &self.tir.types[namespace_ty.as_usize()] {
				Type::Struct { struct_index, .. } => *struct_index,
				_ => {
					self.tir.diagnostics.push(
						Diagnostic::error()
							.with_message(
								"type arguments are not supported here",
							)
							.with_label(Label::primary(
								resolve_context.file_id,
								first.ident.span,
							)),
					);
					return Err(());
				}
			};
			let resolved_args: Box<[TypeIndex]> = first
				.type_args
				.iter()
				.map(|arg| self.resolve_type(resolve_context, None, arg))
				.collect();
			namespace_ty = self.intern_type(Type::Struct {
				struct_index,
				args: resolved_args,
			});
		}

		for seg in &path[1..path.len() - 1] {
			// namespace_span grows to cover all qualifier segments so far.
			// TODO: per-segment span requires a nested namespace expression node.
			namespace_ty = self.resolve_namespace_type_member(
				func_ctx.resolve_context,
				namespace_ty,
				namespace_span,
				seg.ident,
			)?;
			namespace_span =
				TextSpan::new(namespace_span.start, seg.ident.span.end);
		}

		self.build_namespace_member_expression(
			func_ctx,
			ast::Spanned {
				inner: namespace_ty,
				span: namespace_span,
			},
			last,
			expr_span,
		)
	}

	/// Resolve a type-namespace member (used when walking intermediate path
	/// segments): given a resolved namespace `TypeIndex`, look up `member_sym`
	/// as a nested namespace and return its `TypeIndex`.
	fn resolve_namespace_type_member(
		&mut self,
		resolve_context: ResolveContext,
		namespace_ty: TypeIndex,
		namespace_span: TextSpan,
		member: Spanned<SymbolU32>,
	) -> Result<TypeIndex, ()> {
		match &self.tir.types[namespace_ty.as_usize()] {
			Type::Namespace { namespace_idx } => {
				if let Type::Namespace { namespace_idx } =
					&self.tir.types[namespace_ty.as_usize()]
				{
					self.tir.namespaces[*namespace_idx as usize].accesses.push(
						SourceSpan::new(
							resolve_context.file_id,
							namespace_span,
						),
					)
				};
				let namespace_idx = *namespace_idx;
				let kind = self.resolve_pending_namespace_symbol(
					namespace_idx,
					(SymbolNamespace::Type, member.inner),
					SourceSpan::new(resolve_context.file_id, member.span),
				)?;
				match kind {
					Some(
						kind @ (SymbolKind::Struct { .. }
						| SymbolKind::TypeAlias { .. }),
					) => {
						let ty = self.resolve_generic_type_application(
							resolve_context,
							kind,
							&[],
							member.span,
						);
						if ty == TypeIndex::ERROR {
							Err(())
						} else {
							Ok(ty)
						}
					}
					Some(kind) => {
						self.record_type_kind_access(
							resolve_context.file_id,
							kind,
							member.span,
						);
						self.symbol_kind_to_type(kind).ok_or_else(|| {
							self.tir.diagnostics.push(report_undeclared_type(
								SourceSpan::new(
									resolve_context.file_id,
									member.span,
								),
							));
						})
					}
					None => {
						self.tir.diagnostics.push(report_undeclared_type(
							SourceSpan::new(
								resolve_context.file_id,
								member.span,
							),
						));
						Err(())
					}
				}
			}
			Type::TypeParam { .. } => {
				let bounds = self.type_param_bounds(namespace_ty).to_owned();
				for bound in &bounds {
					let trait_index = bound.trait_index;
					let def_ids = self.tir.traits[trait_index as usize]
						.member_ids
						.clone();
					for def_id in def_ids {
						self.ensure_signature(def_id);
					}
					if let Some(ImplEntry::AssociatedType { .. }) =
						self.tir.traits[trait_index as usize]
							.members
							.get(&member.inner)
					{
						if let Some(at) = self.tir.traits[trait_index as usize]
							.assoc_types
							.get_mut(&member.inner)
						{
							at.accesses.push(SourceSpan::new(
								resolve_context.file_id,
								member.span,
							));
						}
						return Ok(self.intern_type(
							Type::AssocTypeProjection {
								trait_index,
								assoc_name: member.inner,
								base: namespace_ty,
							},
						));
					}
				}
				self.tir.diagnostics.push(report_undeclared_type(
					SourceSpan::new(resolve_context.file_id, member.span),
				));
				Err(())
			}
			Type::AssocTypeProjection {
				trait_index,
				assoc_name,
				..
			} => {
				// Nested projection: e.g. `A::M::Size` where namespace_ty = `A::M`.
				// Look up the bound declared on the assoc type (`type M: Memory`)
				// and search those bound traits for the requested member.
				let (trait_index, assoc_name) = (*trait_index, *assoc_name);
				let bounds = self.tir.traits[trait_index as usize]
					.assoc_types
					.get(&assoc_name)
					.map(|at| at.bounds.traits.clone())
					.unwrap_or_default();
				for bound in bounds.iter() {
					let bound_trait = bound.trait_index;
					let def_ids = self.tir.traits[bound_trait as usize]
						.member_ids
						.clone();
					for def_id in def_ids {
						self.ensure_signature(def_id);
					}
					if let Some(ImplEntry::AssociatedType { .. }) =
						self.tir.traits[bound_trait as usize]
							.members
							.get(&member.inner)
					{
						if let Some(at) = self.tir.traits[bound_trait as usize]
							.assoc_types
							.get_mut(&member.inner)
						{
							at.accesses.push(SourceSpan::new(
								resolve_context.file_id,
								member.span,
							));
						}
						return Ok(self.intern_type(
							Type::AssocTypeProjection {
								trait_index: bound_trait,
								assoc_name: member.inner,
								base: namespace_ty,
							},
						));
					}
				}
				let member_name =
					self.interner.resolve(member.inner).unwrap_or("?");
				let type_name = TypeFormatter::new(&self.tir, self.interner)
					.display_type(namespace_ty)
					.unwrap_or_default();
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::UndeclaredType.code())
						.with_message(format!(
							"no type named `{member_name}` found for type `{type_name}`",
						))
						.with_label(Label::primary(
							resolve_context.file_id,
							member.span,
						)),
				);
				Err(())
			}
			_ => {
				match self
					.tir
					.impl_members
					.get(&namespace_ty)
					.and_then(|m| m.get(&member.inner))
					.cloned()
				{
					Some(ImplEntry::AssociatedType { ty }) => Ok(ty),
					_ => {
						let member_name =
							self.interner.resolve(member.inner).unwrap_or("?");
						let type_name =
							TypeFormatter::new(&self.tir, self.interner)
								.display_type(namespace_ty)
								.unwrap();
						self.tir.diagnostics.push(
							Diagnostic::error()
								.with_code(
									DiagnosticCode::UndeclaredType.code(),
								)
								.with_message(format!(
									"no type named `{member_name}` found for type `{type_name}`",
								))
								.with_label(Label::primary(
									resolve_context.file_id,
									member.span,
								)),
						);
						Err(())
					}
				}
			}
		}
	}

	fn record_type_kind_access(
		&mut self,
		file_id: FileId,
		kind: SymbolKind,
		span: TextSpan,
	) {
		match kind {
			SymbolKind::Struct { struct_index } => {
				self.tir.structs[struct_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, span));
			}
			SymbolKind::Enum { enum_index } => {
				self.tir.enums[enum_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, span));
			}
			SymbolKind::Trait { trait_index } => {
				self.tir.traits[trait_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, span));
			}
			SymbolKind::TypeSet { typeset_index } => {
				self.tir.typesets[typeset_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, span));
			}
			SymbolKind::TypeAlias { type_alias_index } => {
				self.tir.type_aliases[type_alias_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, span));
			}
			SymbolKind::Const { const_index } => {
				self.tir.constants[const_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, span));
			}
			_ => {}
		}
	}

	/// Resolve `member_sym` within `namespace_ty`, emitting a diagnostic and
	/// returning `Err(())` when resolution fails. On success returns
	/// `Ok(ResolvedMember)` — the caller decides whether the specific kind is
	/// valid in its context (e.g. callability).
	fn resolve_namespace_member(
		&mut self,
		file_id: FileId,
		namespace: Spanned<TypeIndex>,
		member: Spanned<SymbolU32>,
	) -> Result<ResolvedMember, ()> {
		// impl_members take priority (associated consts and methods on any type).
		match self
			.tir
			.impl_members
			.get(&namespace.inner)
			.and_then(|m| m.get(&member.inner))
			.cloned()
		{
			Some(ImplEntry::AssociatedConst { id, ty }) => {
				return Ok(ResolvedMember::Const { id, ty });
			}
			Some(
				ImplEntry::Method(func_index)
				| ImplEntry::AssociatedFn(func_index),
			) => {
				return Ok(ResolvedMember::Function {
					func_index,
					type_args: Box::new([]),
				});
			}
			Some(ImplEntry::AssociatedType { .. }) | None => {}
		}

		match &self.tir.types[namespace.inner.as_usize()].clone() {
			Type::Memory { .. } => {
				self.tir.diagnostics.push(report_undeclared_identifier(
					SourceSpan::new(file_id, member.span),
				));
				Err(())
			}
			Type::Enum { enum_index } => {
				let enum_idx = *enum_index;
				match self.tir.enums[enum_idx as usize]
					.variant_lookup
					.get(&member.inner)
					.copied()
				{
					Some(variant_index) => Ok(ResolvedMember::EnumVariant {
						enum_index: enum_idx,
						variant_index,
					}),
					None => {
						self.tir.diagnostics.push(
							report_undeclared_identifier(SourceSpan::new(
								file_id,
								member.span,
							)),
						);
						Err(())
					}
				}
			}
			Type::Namespace { namespace_idx } => {
				let ns_idx = *namespace_idx;
				match self.tir.namespaces[ns_idx as usize]
					.symbols
					.get(&(SymbolNamespace::Value, member.inner))
					.cloned()
				{
					Some(SymbolKind::Function { func_index }) => {
						Ok(ResolvedMember::Function {
							func_index,
							type_args: Box::new([]),
						})
					}
					Some(SymbolKind::Global { global_index }) => {
						Ok(ResolvedMember::Global { global_index })
					}
					_ => {
						self.tir.diagnostics.push(
							report_undeclared_identifier(SourceSpan::new(
								file_id,
								member.span,
							)),
						);
						Err(())
					}
				}
			}
			_ => {
				// Check generic impl blocks before giving up.
				if let Some((block_idx, type_args)) =
					self.find_generic_impl(namespace.inner, member.inner)
				{
					if let Some(
						ImplEntry::Method(func_index)
						| ImplEntry::AssociatedFn(func_index),
					) = self.tir.generic_impl_list[block_idx]
						.members
						.get(&member.inner)
						.cloned()
					{
						return Ok(ResolvedMember::Function {
							func_index,
							type_args,
						});
					}
				}
				let member_name =
					self.interner.resolve(member.inner).unwrap_or("?");
				let type_name = TypeFormatter::new(&self.tir, self.interner)
					.display_type(namespace.inner)
					.unwrap();
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::UndeclaredIdentifier.code())
						.with_message(format!(
							"no associated item named `{member_name}` found for type `{type_name}`",
						))
						.with_label(
							SourceSpan::new(file_id, member.span)
								.primary_label(),
						),
				);
				Err(())
			}
		}
	}

	/// Core namespace-member dispatch: look up `member` inside a type whose
	/// `TypeIndex` has already been resolved.
	fn build_namespace_member_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		namespace: Spanned<TypeIndex>,
		segment: &ast::PathSegment,
		expr_span: TextSpan,
	) -> Result<Expression, ()> {
		let file_id = func_ctx.resolve_context.file_id;

		if let Type::Namespace { namespace_idx } =
			&self.tir.types[namespace.inner.as_usize()]
		{
			self.tir.namespaces[*namespace_idx as usize]
				.accesses
				.push(SourceSpan::new(file_id, namespace.span))
		};

		let resolved =
			self.resolve_namespace_member(file_id, namespace, segment.ident)?;

		let member_span = segment.ident.span;
		match resolved {
			ResolvedMember::Function {
				func_index,
				type_args: impl_args,
			} => {
				let func_id = self.tir.functions[func_index as usize].id;
				let fn_params_len =
					self.tir.functions[func_index as usize].type_params.len();
				let type_params_len = self.tir.functions[func_index as usize]
					.total_type_param_count();

				if !segment.type_args.is_empty()
					&& segment.type_args.len() != fn_params_len
				{
					self.tir.diagnostics.push(
						Diagnostic::error()
							.with_code(
								DiagnosticCode::TypeArgCountMismatch.code(),
							)
							.with_message(format!(
								"expected {} type argument{}, found {}",
								fn_params_len,
								if fn_params_len == 1 { "" } else { "s" },
								segment.type_args.len()
							))
							.with_label(
								SourceSpan::new(file_id, expr_span)
									.primary_label()
									.with_message(
										"wrong number of type arguments",
									),
							),
					);
				}

				self.tir.functions[func_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, member_span));

				let mut combined = vec![TypeIndex::INFER; type_params_len];
				for (slot, &arg) in combined.iter_mut().zip(impl_args.iter()) {
					*slot = arg;
				}
				for (slot, ast_arg) in combined[impl_args.len()..]
					.iter_mut()
					.zip(segment.type_args.iter())
				{
					*slot = self.resolve_type(
						func_ctx.resolve_context,
						func_ctx.scope,
						ast_arg,
					);
				}

				let func_ty = self.intern_type(Type::FunctionItem {
					id: func_id,
					type_args: combined.into_boxed_slice(),
				});
				Ok(Expression {
					kind: ExprKind::NamespaceAccess {
						namespace,
						member: Box::new(Expression {
							kind: ExprKind::Function { id: func_id },
							ty: func_ty,
							span: member_span,
						}),
					},
					ty: func_ty,
					span: expr_span,
				})
			}
			ResolvedMember::Const { id, ty } => {
				if let Some(ci) = self.tir.const_index(id) {
					self.tir.constants[ci as usize]
						.accesses
						.push(SourceSpan::new(file_id, member_span));
				}
				Ok(Expression {
					kind: ExprKind::NamespaceAccess {
						namespace,
						member: Box::new(Expression {
							kind: ExprKind::Const { id },
							ty,
							span: member_span,
						}),
					},
					ty,
					span: expr_span,
				})
			}
			ResolvedMember::Global { global_index } => {
				let global = &mut self.tir.globals[global_index as usize];
				global.accesses.push(SourceSpan::new(file_id, member_span));
				let global_id = global.id;
				let ty = global.ty.inner;
				Ok(Expression {
					kind: ExprKind::NamespaceAccess {
						namespace,
						member: Box::new(Expression {
							kind: ExprKind::Global { id: global_id },
							ty,
							span: member_span,
						}),
					},
					ty,
					span: expr_span,
				})
			}
			ResolvedMember::EnumVariant {
				enum_index,
				variant_index,
			} => {
				self.tir.enums[enum_index as usize].variants
					[variant_index as usize]
					.accesses
					.push(SourceSpan::new(file_id, member_span));
				Ok(Expression {
					kind: ExprKind::NamespaceAccess {
						namespace,
						member: Box::new(Expression {
							kind: ExprKind::EnumVariant {
								enum_index,
								variant_index,
							},
							ty: namespace.inner,
							span: member_span,
						}),
					},
					ty: namespace.inner,
					span: expr_span,
				})
			}
		}
	}

	fn build_label_expression(
		&mut self,
		ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (label, block) = match &expr.inner {
			ast::Expression::Label { label, block } => (*label, block),
			_ => unreachable!(),
		};
		let label = ctx.stack.push_label(label);

		match block.inner {
			ast::Expression::Block { .. } => ctx.enter_block(
				BlockScope {
					label: Some(label),
					kind: BlockKind::Block,
					parent: Some(ctx.scope_index),
					span: block.span,
					locals: Vec::new(),
					inferred_type: TypeIndex::INFER,
					expected_type: access_ctx.expected_type,
				},
				|ctx| self.build_block_expression(ctx, block),
			),
			ast::Expression::IfElse { .. } => self.build_if_else_expression(
				ctx,
				AccessContext {
					expected_type: access_ctx.expected_type,
					access_kind: AccessKind::Read,
				},
				block,
				Some(label),
			),
			ast::Expression::Loop { .. } => self.build_loop_expression(
				ctx,
				AccessContext {
					expected_type: access_ctx.expected_type,
					access_kind: AccessKind::Read,
				},
				block,
				Some(label),
			),
			_ => unreachable!(),
		}
	}

	fn build_loop_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
		label: Option<LabelIndex>,
	) -> Result<Expression, ()> {
		let block = match &expr.inner {
			ast::Expression::Loop { block } => block,
			_ => unreachable!(),
		};

		let file_id = func_ctx.resolve_context.file_id;
		func_ctx.enter_block(
			BlockScope {
				label,
				kind: BlockKind::Loop,
				parent: Some(func_ctx.scope_index),
				span: expr.span,
				locals: Vec::new(),
				inferred_type: TypeIndex::INFER,
				expected_type: access_ctx.expected_type,
			},
			|ctx| {
				let block = self.build_block_expression(ctx, block)?;

				let scope = &ctx.stack.scopes[ctx.scope_index as usize];
				let (expected_type, inferred_type) =
					(scope.expected_type, scope.inferred_type);
				if expected_type != TypeIndex::INFER
					&& inferred_type != TypeIndex::INFER
					&& !self.coercible_to(inferred_type, expected_type)
				{
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type,
							actual_type: inferred_type,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
					return Err(());
				}

				// No `break` was encountered → loop is infinite → type is Never.
				let ty = inferred_type.infer_or(TypeIndex::NEVER);
				Ok(Expression {
					kind: ExprKind::Loop {
						scope_index: ctx.scope_index,
						block: Box::new(block),
					},
					ty,
					span: expr.span,
				})
			},
		)
	}

	fn build_continue_expression(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let label = match &expr.inner {
			ast::Expression::Continue { label } => *label,
			_ => unreachable!(),
		};

		let scope_index = match label {
			Some(label) => match ctx.resolve_label(label.inner) {
				Some((scope_index, label_index)) => {
					ctx.stack.labels[label_index as usize]
						.accesses
						.push(label.span);
					scope_index
				}
				None => {
					self.tir.diagnostics.push(report_undeclared_label(
						self.interner.resolve(label.inner).unwrap(),
						SourceSpan::new(
							ctx.resolve_context.file_id,
							label.span,
						),
					));
					return Ok(Expression {
						kind: ExprKind::Error,
						ty: TypeIndex::NEVER,
						span: expr.span,
					});
				}
			},
			None => match ctx.get_closest_loop_block() {
				Some(scope_index) => scope_index,
				None => {
					self.tir.diagnostics.push(
						Diagnostic::error()
							.with_code(
								DiagnosticCode::ContinueOutsideOfLoop.code(),
							)
							.with_message("`continue` outside of a loop")
							.with_label(
								SourceSpan::new(
									ctx.resolve_context.file_id,
									expr.span,
								)
								.primary_label()
								.with_message(
									"cannot `continue` outside of a loop",
								),
							),
					);
					return Ok(Expression {
						kind: ExprKind::Error,
						ty: TypeIndex::NEVER,
						span: expr.span,
					});
				}
			},
		};

		Ok(Expression {
			kind: ExprKind::Continue { scope_index },
			ty: TypeIndex::NEVER,
			span: expr.span,
		})
	}

	fn build_if_else_expression(
		&mut self,
		ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
		label: Option<LabelIndex>,
	) -> Result<Expression, ()> {
		let (condition, then_block, maybe_else_block) = match &expr.inner {
			ast::Expression::IfElse {
				condition,
				then_block,
				else_block,
			} => (condition, then_block, else_block),
			_ => unreachable!(),
		};

		let condition = self.build_expression(
			ctx,
			AccessContext {
				expected_type: TypeIndex::BOOL,
				access_kind: AccessKind::Read,
			},
			condition,
		)?;

		let mut then_block = match then_block.inner {
			ast::Expression::Block { .. } => ctx.enter_block(
				BlockScope {
					label,
					kind: BlockKind::Block,
					parent: Some(ctx.scope_index),
					span: then_block.span,
					locals: Vec::new(),
					inferred_type: TypeIndex::INFER,
					expected_type: match maybe_else_block {
						Some(_) => access_ctx.expected_type,
						None => TypeIndex::INFER,
					},
				},
				|ctx| self.build_block_expression(ctx, then_block),
			)?,
			_ => unreachable!(),
		};
		let (else_block, ty) = match maybe_else_block {
			Some(ast_else_block) => {
				let mut else_block = match ast_else_block.inner {
					ast::Expression::Block { .. } => ctx.enter_block(
						BlockScope {
							label,
							kind: BlockKind::Block,
							parent: Some(ctx.scope_index),
							span: ast_else_block.span,
							locals: Vec::new(),
							inferred_type: TypeIndex::INFER,
							expected_type: access_ctx.expected_type,
						},
						|ctx| self.build_block_expression(ctx, ast_else_block),
					)?,
					_ => unreachable!(),
				};

				// Cross-branch comptime coercion: coerce the comptime branch to match the
				// concrete sibling. Break values inside a comptime branch still need a type
				// annotation — they're resolved at build time before we see the sibling type.
				if then_block.ty.is_comptime_number()
					&& !else_block.ty.is_comptime_number()
				{
					self.coerce_untyped_expr(
						ctx,
						&mut then_block,
						else_block.ty,
					)?;
				} else if else_block.ty.is_comptime_number()
					&& !then_block.ty.is_comptime_number()
				{
					self.coerce_untyped_expr(
						ctx,
						&mut else_block,
						then_block.ty,
					)?;
				}

				match self.unify(then_block.ty, else_block.ty) {
					Ok(ty) => (Some(else_block), ty),
					Err(_) => {
						self.tir.diagnostics.push(report_type_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							TypeMistmatchDiagnostic {
								expected_type: then_block.ty,
								actual_type: else_block.ty,
								span: SourceSpan::new(
									ctx.resolve_context.file_id,
									ast_else_block.span,
								),
							},
						));
						return Err(());
					}
				}
			}
			None => {
				if then_block.ty == TypeIndex::UNIT
					|| then_block.ty == TypeIndex::NEVER
				{
					(None, TypeIndex::UNIT)
				} else {
					self.tir.diagnostics.push(report_missing_else_block(
						TypeFormatter::new(&self.tir, self.interner),
						then_block.ty,
						SourceSpan::new(
							ctx.resolve_context.file_id,
							then_block.span,
						),
					));
					return Err(());
				}
			}
		};

		Ok(Expression {
			kind: ExprKind::IfElse {
				condition: Box::new(condition),
				then_block: Box::new(then_block),
				else_block: else_block.map(Box::new),
			},
			ty,
			span: expr.span,
		})
	}

	fn build_cast_expression(
		&mut self,
		ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (value, cast_type) = match &expr.inner {
			ast::Expression::Cast { value, ty } => (value, ty),
			_ => unreachable!(),
		};

		let cast_type =
			self.resolve_type(ctx.resolve_context, ctx.scope, cast_type);
		if cast_type == TypeIndex::ERROR {
			return self.build_expression(ctx, access_ctx, value);
		}
		let cast_type = if cast_type == TypeIndex::INFER {
			let expected = access_ctx.expected_type;
			if expected == TypeIndex::INFER {
				self.tir.diagnostics.push(report_type_annotation_required(
					SourceSpan::new(ctx.resolve_context.file_id, expr.span),
				));
				return self.build_expression(ctx, access_ctx, value);
			}
			expected
		} else {
			cast_type
		};
		let mut value = self.build_expression(
			ctx,
			AccessContext {
				expected_type: cast_type,
				access_kind: access_ctx.access_kind,
			},
			value,
		)?;
		if value.ty.is_comptime_number() {
			self.coerce_untyped_expr(ctx, &mut value, cast_type)?;
		} else if self.are_scalar_compatible(value.ty, cast_type) {
			// TODO: add checks for unsafe/lossy casts like i32 to u8, or u32 to char
			value.ty = cast_type;
		} else {
			self.tir.diagnostics.push(report_invalid_cast(
				TypeFormatter::new(&self.tir, self.interner),
				value.ty,
				cast_type,
				SourceSpan::new(ctx.resolve_context.file_id, expr.span),
			));
		}

		Ok(value)
	}

	fn are_scalar_compatible(&self, a: TypeIndex, b: TypeIndex) -> bool {
		if a == b {
			return true;
		}
		match (&self.tir.types[a.as_usize()], &self.tir.types[b.as_usize()]) {
			(
				Type::Pointer { memory: a_mem, .. },
				Type::Pointer { memory: b_mem, .. },
			) => a_mem == b_mem,
			(
				Type::Array { memory: a_mem, .. },
				Type::Array { memory: b_mem, .. },
			) => a_mem == b_mem,
			(
				Type::Slice { memory: a_mem, .. },
				Type::Slice { memory: b_mem, .. },
			) => a_mem == b_mem,
			// Allow M::Size ↔ M::*T (both directions): same memory base, assoc type is "Size"
			(
				Type::AssocTypeProjection {
					base: a_base,
					assoc_name,
					..
				},
				Type::Pointer { memory: b_mem, .. },
			) => {
				a_base == b_mem
					&& self.interner.resolve(*assoc_name) == Some("Size")
			}
			(
				Type::Pointer { memory: a_mem, .. },
				Type::AssocTypeProjection {
					base: b_base,
					assoc_name,
					..
				},
			) => {
				a_mem == b_base
					&& self.interner.resolve(*assoc_name) == Some("Size")
			}
			_ => matches!(
				(self.type_scalar(a), self.type_scalar(b)),
				(Some(x), Some(y)) if x == y
			),
		}
	}

	fn type_scalar(&self, ty: TypeIndex) -> Option<WasmScalar> {
		match &self.tir.types[ty.as_usize()] {
			Type::Bool
			| Type::U8
			| Type::I8
			| Type::U16
			| Type::I16
			| Type::I32
			| Type::U32
			| Type::Char
			| Type::Function { .. } => Some(WasmScalar::I32),
			Type::Enum { enum_index } => {
				let repr_type = self.tir.enums[*enum_index as usize].repr_type;
				self.type_scalar(repr_type)
			}
			Type::U64 | Type::I64 => Some(WasmScalar::I64),
			Type::F32 => Some(WasmScalar::F32),
			Type::F64 => Some(WasmScalar::F64),
			Type::Pointer { memory, .. } => {
				match &self.tir.types[memory.as_usize()] {
					Type::Memory { id, .. } => {
						let kind = self.tir.memories
							[self.tir.expect_memory_index(*id) as usize]
							.kind;
						self.type_scalar(kind)
					}
					_ => None,
				}
			}
			Type::Tuple { .. }
			| Type::Array { .. }
			| Type::AssociatedType { .. }
			| Type::AssocTypeProjection { .. }
			| Type::FunctionItem { .. }
			| Type::Struct { .. }
			| Type::Slice { .. }
			| Type::Namespace { .. }
			| Type::Memory { .. }
			| Type::TypeParam { .. }
			| Type::Error
			| Type::Infer
			| Type::Never
			| Type::Unit
			| Type::Integer
			| Type::Float => None,
		}
	}

	fn build_break_expression(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (label, value) = match &expr.inner {
			ast::Expression::Break { label, value } => (*label, value),
			_ => unreachable!(),
		};

		let scope_index = match label {
			Some(label) => match ctx.resolve_label(label.inner) {
				Some((scope_index, label_index)) => {
					ctx.stack.labels[label_index as usize]
						.accesses
						.push(label.span);
					scope_index
				}
				None => {
					self.tir.diagnostics.push(report_undeclared_label(
						self.interner.resolve(label.inner).unwrap(),
						SourceSpan::new(
							ctx.resolve_context.file_id,
							label.span,
						),
					));

					// TODO: how to handle this better? we don't parse the value if the label is
					// undeclared
					return Ok(Expression {
						kind: ExprKind::Error,
						ty: TypeIndex::NEVER,
						span: expr.span,
					});
				}
			},
			None => match ctx.get_closest_loop_block() {
				Some(scope_index) => scope_index,
				None => {
					self.tir.diagnostics.push(report_break_outside_of_loop(
						SourceSpan::new(ctx.resolve_context.file_id, expr.span),
					));

					// TODO: same as above, we don't parse the value if the break is outside of a
					// loop
					return Ok(Expression {
						kind: ExprKind::Error,
						ty: TypeIndex::NEVER,
						span: expr.span,
					});
				}
			},
		};

		match value {
			Some(value) => {
				let expected_type = ctx
					.stack
					.scopes
					.get(scope_index as usize)
					.unwrap()
					.expected_type;
				let mut built = match self.build_expression(
					ctx,
					AccessContext {
						expected_type,
						access_kind: AccessKind::Read,
					},
					value,
				) {
					Ok(v) => v,
					Err(()) => {
						return Ok(Expression {
							kind: ExprKind::Error,
							ty: TypeIndex::NEVER,
							span: expr.span,
						});
					}
				};

				let inferred_type = {
					let scope =
						ctx.stack.scopes.get_mut(scope_index as usize).unwrap();
					let inferred_type = self.infer_block_type(
						ctx.resolve_context.file_id,
						scope,
						&built,
					)?;
					scope.inferred_type = inferred_type;
					inferred_type
				};

				if built.ty.is_comptime_number() {
					if inferred_type.is_comptime_number() {
						self.tir.diagnostics.push(
							report_type_annotation_required(SourceSpan::new(
								ctx.resolve_context.file_id,
								built.span,
							)),
						);
						return Err(());
					}
					self.coerce_untyped_expr(ctx, &mut built, inferred_type)?;
				}

				Ok(Expression {
					kind: ExprKind::Break {
						scope_index,
						value: Some(Box::new(built)),
					},
					ty: TypeIndex::NEVER,
					span: expr.span,
				})
			}
			None => {
				let scope =
					ctx.stack.scopes.get_mut(scope_index as usize).unwrap();
				if scope.inferred_type != TypeIndex::INFER {
					let inferred = scope.inferred_type;
					if !self.coercible_to(TypeIndex::UNIT, inferred) {
						let formatter =
							TypeFormatter::new(&self.tir, self.interner);
						self.tir.diagnostics.push(report_type_mistmatch(
							formatter,
							TypeMistmatchDiagnostic {
								expected_type: inferred,
								actual_type: TypeIndex::UNIT,
								span: SourceSpan::new(
									ctx.resolve_context.file_id,
									expr.span,
								),
							},
						));
					}
				} else {
					scope.inferred_type = TypeIndex::UNIT;
				}

				Ok(Expression {
					kind: ExprKind::Break {
						scope_index,
						value: None,
					},
					ty: TypeIndex::NEVER,
					span: expr.span,
				})
			}
		}
	}

	fn build_type_application_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		callee: &Spanned<ast::Expression>,
		_args: &[Spanned<ast::TypeExpression>],
		expr_span: ast::TextSpan,
	) -> Result<Expression, ()> {
		// TypeApplication on a non-path callee, e.g. a bare `obj.field::<T>`
		// without a following call.  Method turbofish calls (`obj.m::<T>(args)`)
		// are handled by MethodCall.  Identifier-started turbofish (`f::<T>()`)
		// is fully handled by build_path_expression and never reaches here.
		// Type args are not semantically resolved for these forms; just build
		// the callee and carry its type through.
		let mut result = self.build_expression(
			func_ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Read,
			},
			callee,
		)?;
		result.span = expr_span;
		Ok(result)
	}

	fn build_binary_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let operator = match &expr.inner {
			ast::Expression::Binary { operator, .. } => operator.inner,
			_ => unreachable!(),
		};

		match operator {
			ast::BinaryOp::Add
			| ast::BinaryOp::Sub
			| ast::BinaryOp::Mul
			| ast::BinaryOp::Div
			| ast::BinaryOp::Rem => {
				self.build_arithmetic_expr(func_ctx, expr, access_ctx)
			}
			ast::BinaryOp::Assign => self.build_assignment_expr(func_ctx, expr),
			ast::BinaryOp::AddAssign
			| ast::BinaryOp::SubAssign
			| ast::BinaryOp::MulAssign
			| ast::BinaryOp::DivAssign
			| ast::BinaryOp::RemAssign => {
				self.build_arithmetic_assignment_expr(func_ctx, expr)
			}
			ast::BinaryOp::Eq
			| ast::BinaryOp::NotEq
			| ast::BinaryOp::Less
			| ast::BinaryOp::LessEq
			| ast::BinaryOp::Greater
			| ast::BinaryOp::GreaterEq => {
				self.build_comparison_binary_expr(func_ctx, expr)
			}
			ast::BinaryOp::And | ast::BinaryOp::Or => {
				self.build_logical_binary_expr(func_ctx, expr)
			}
			ast::BinaryOp::BitAnd
			| ast::BinaryOp::BitOr
			| ast::BinaryOp::BitXor
			| ast::BinaryOp::LeftShift
			| ast::BinaryOp::RightShift => {
				self.build_bitwise_binary_expr(func_ctx, expr, access_ctx)
			}
		}
	}

	fn build_unary_expression(
		&mut self,
		ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (operator, ast_operand) = match &expr.inner {
			ast::Expression::Unary { operator, operand } => {
				(*operator, operand)
			}
			_ => unreachable!(),
		};
		let mut operand = self.build_expression(
			ctx,
			AccessContext {
				expected_type: access_ctx.expected_type,
				access_kind: AccessKind::Read,
			},
			ast_operand,
		)?;

		match operator.inner {
			ast::UnaryOp::InvertSign | ast::UnaryOp::BitNot => {
				if operand.ty.is_primitive() || operand.ty.is_comptime_number()
				{
					let ty = operand.ty;
					Ok(Expression {
						kind: ExprKind::Unary {
							operator,
							operand: Box::new(operand),
						},
						ty,
						span: expr.span,
					})
				} else {
					panic!("can't apply unary operator to this type")
				}
			}
			ast::UnaryOp::Not => {
				if operand.ty == TypeIndex::BOOL {
					Ok(Expression {
						kind: ExprKind::Unary {
							operator,
							operand: Box::new(operand),
						},
						ty: TypeIndex::BOOL,
						span: expr.span,
					})
				} else if operand.ty.is_comptime_number() {
					_ = self.coerce_untyped_expr(
						ctx,
						&mut operand,
						TypeIndex::BOOL,
					);
					Ok(Expression {
						kind: ExprKind::Unary {
							operator,
							operand: Box::new(operand),
						},
						ty: TypeIndex::BOOL,
						span: expr.span,
					})
				} else {
					let formatter =
						TypeFormatter::new(&self.tir, self.interner);
					let diagnostic = Diagnostic::error()
						.with_code(
							DiagnosticCode::UnaryOperatorCannotBeApplied.code(),
						)
						.with_message(format!(
							"operator `{}` cannot be applied to type `{}`",
							operator.inner,
							formatter.display_type(operand.ty).unwrap()
						))
						.with_label(Label::primary(
							ctx.resolve_context.file_id,
							operand.span,
						))
						.with_label(Label::secondary(
							ctx.resolve_context.file_id,
							operator.span,
						));

					self.tir.diagnostics.push(diagnostic);
					Ok(Expression {
						kind: ExprKind::Unary {
							operator,
							operand: Box::new(operand),
						},
						ty: TypeIndex::BOOL,
						span: expr.span,
					})
				}
			}
		}
	}

	fn build_logical_binary_expr(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (left, right, operator) = match &expr.inner {
			ast::Expression::Binary {
				left,
				right,
				operator,
				..
			} => (left, right, *operator),
			_ => unreachable!(),
		};

		let left = self.build_expression(
			ctx,
			AccessContext {
				expected_type: TypeIndex::BOOL,
				access_kind: AccessKind::Read,
			},
			left,
		)?;
		if left.ty == TypeIndex::ERROR {
			// Error already reported
		} else if left.ty.is_comptime_number() {
			self.tir.diagnostics.push(report_type_annotation_required(
				SourceSpan::new(ctx.resolve_context.file_id, left.span),
			));
		} else if left.ty != TypeIndex::BOOL {
			self.tir.diagnostics.push(report_type_mistmatch(
				TypeFormatter::new(&self.tir, self.interner),
				TypeMistmatchDiagnostic {
					expected_type: TypeIndex::BOOL,
					actual_type: left.ty,
					span: SourceSpan::new(
						ctx.resolve_context.file_id,
						left.span,
					),
				},
			));
		}
		let right = self.build_expression(
			ctx,
			AccessContext {
				expected_type: TypeIndex::BOOL,
				access_kind: AccessKind::Read,
			},
			right,
		)?;
		if right.ty == TypeIndex::ERROR {
			// Error already reported
		} else if right.ty.is_comptime_number() {
			self.tir.diagnostics.push(report_type_annotation_required(
				SourceSpan::new(ctx.resolve_context.file_id, right.span),
			));
		} else if right.ty != TypeIndex::BOOL {
			self.tir.diagnostics.push(report_type_mistmatch(
				TypeFormatter::new(&self.tir, self.interner),
				TypeMistmatchDiagnostic {
					expected_type: TypeIndex::BOOL,
					actual_type: right.ty,
					span: SourceSpan::new(
						ctx.resolve_context.file_id,
						right.span,
					),
				},
			));
		}

		Ok(Expression {
			kind: ExprKind::Binary {
				operator,
				left: Box::new(left),
				right: Box::new(right),
			},
			ty: TypeIndex::BOOL,
			span: expr.span,
		})
	}

	fn build_bitwise_binary_expr(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
		access_ctx: AccessContext,
	) -> Result<Expression, ()> {
		let (left, right, operator) = match &expr.inner {
			ast::Expression::Binary {
				left,
				right,
				operator,
			} => (left, right, *operator),
			_ => unreachable!(),
		};

		let mut left = self.build_expression(ctx, access_ctx.clone(), left)?;
		let mut right = self.build_expression(
			ctx,
			AccessContext {
				expected_type: match &self.tir.types[left.ty.as_usize()] {
					Type::Integer
					| Type::Float
					| Type::Error
					| Type::Never
					| Type::Unit => access_ctx.expected_type,
					_ => left.ty,
				},
				access_kind: access_ctx.access_kind,
			},
			right,
		)?;

		match (left.ty, right.ty) {
			// Allow operations with Error type (error already reported elsewhere)
			(l, r) if l == TypeIndex::ERROR || r == TypeIndex::ERROR => {
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: access_ctx.expected_type.infer_or(TypeIndex::ERROR),
					span: expr.span,
				})
			}
			(l, r) if l.is_comptime_number() && r.is_comptime_number() => {
				if access_ctx.expected_type != TypeIndex::INFER {
					let expected_type = access_ctx.expected_type;
					self.coerce_untyped_expr(ctx, &mut left, expected_type)?;
					self.coerce_untyped_expr(ctx, &mut right, expected_type)?;

					if !expected_type.is_integer()
						&& expected_type != TypeIndex::BOOL
					{
						self.tir.diagnostics.push(
							report_binary_operator_cannot_be_applied(
								TypeFormatter::new(&self.tir, self.interner),
								BinaryOperatorCannotBeAppliedDiagnostic {
									file_id: ctx.resolve_context.file_id,
									operator,
									operand: Spanned {
										inner: expected_type,
										span: left.span,
									},
								},
							),
						);
					}

					Ok(Expression {
						kind: ExprKind::Binary {
							operator,
							left: Box::new(left),
							right: Box::new(right),
						},
						ty: expected_type,
						span: expr.span,
					})
				} else {
					self.tir.diagnostics.push(report_type_annotation_required(
						SourceSpan::new(ctx.resolve_context.file_id, expr.span),
					));
					Err(())
				}
			}
			(l, right_type) if l.is_comptime_number() => {
				if !right_type.is_integer() && right_type != TypeIndex::BOOL {
					self.tir.diagnostics.push(
						report_binary_operator_cannot_be_applied(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryOperatorCannotBeAppliedDiagnostic {
								file_id: ctx.resolve_context.file_id,
								operator,
								operand: Spanned {
									inner: right_type,
									span: right.span,
								},
							},
						),
					);
				}
				self.coerce_untyped_expr(ctx, &mut left, right_type)?;

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: right_type,
					span: expr.span,
				})
			}
			(left_type, r) if r.is_comptime_number() => {
				if !left_type.is_integer() && left_type != TypeIndex::BOOL {
					self.tir.diagnostics.push(
						report_binary_operator_cannot_be_applied(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryOperatorCannotBeAppliedDiagnostic {
								file_id: ctx.resolve_context.file_id,
								operator,
								operand: Spanned {
									inner: left_type,
									span: left.span,
								},
							},
						),
					);
				}
				self.coerce_untyped_expr(ctx, &mut right, left_type)?;

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: left_type,
					span: expr.span,
				})
			}
			(left_type, right_type)
				if left_type == right_type
					&& (left_type.is_integer()
						|| left_type == TypeIndex::BOOL) =>
			{
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: left_type,
					span: expr.span,
				})
			}
			(left_type, right_type) => {
				self.tir
					.diagnostics
					.push(report_binary_expression_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						BinaryExpressionMistmatchDiagnostic {
							file_id: ctx.resolve_context.file_id,
							left_type: Spanned {
								inner: left_type,
								span: left.span,
							},
							operator,
							right_type: Spanned {
								inner: right_type,
								span: right.span,
							},
						},
					));

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: access_ctx.expected_type.infer_or(TypeIndex::ERROR),
					span: expr.span,
				})
			}
		}
	}

	fn build_comparison_binary_expr(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (left, right, operator) = match &expr.inner {
			ast::Expression::Binary {
				left,
				right,
				operator,
				..
			} => (left, right, *operator),
			_ => unreachable!(),
		};

		let mut left = self.build_expression(
			ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Read,
			},
			left,
		)?;
		let mut right = self.build_expression(
			ctx,
			AccessContext {
				expected_type: left.ty,
				access_kind: AccessKind::Read,
			},
			right,
		)?;

		match (left.ty, right.ty) {
			// Allow operations with Error type (error already reported elsewhere)
			(l, r) if l == TypeIndex::ERROR || r == TypeIndex::ERROR => {
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
			(l, r) if l.is_comptime_number() && r.is_comptime_number() => {
				self.tir.diagnostics.push(
					report_comparison_type_annotation_required(
						SourceSpan::new(ctx.resolve_context.file_id, left.span),
						SourceSpan::new(
							ctx.resolve_context.file_id,
							right.span,
						),
					),
				);

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
			(l, ty) if l.is_comptime_number() => {
				self.coerce_untyped_expr(ctx, &mut left, ty)?;

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
			(ty, r) if r.is_comptime_number() => {
				self.coerce_untyped_expr(ctx, &mut right, ty)?;

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
			(l, r) if l == TypeIndex::BOOL && r == TypeIndex::BOOL => {
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
			(left_type, right_type)
				if left_type == right_type
					&& self.is_arithmetic_type(left_type) =>
			{
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
			(left_type, right_type)
				if left_type == right_type
					&& matches!(
						self.tir.types[left_type.as_usize()],
						Type::Enum { .. }
					) =>
			{
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
			(left_type, right_type)
				if matches!(
					operator.inner,
					ast::BinaryOp::Eq | ast::BinaryOp::NotEq
				) && matches!(
					(
						&self.tir.types[left_type.as_usize()],
						&self.tir.types[right_type.as_usize()],
					),
					(
						Type::Pointer { to: lt, memory: lm, .. },
						Type::Pointer { to: rt, memory: rm, .. },
					) if lt == rt && lm == rm
				) =>
			{
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
			(left_type, right_type) => {
				self.tir
					.diagnostics
					.push(report_binary_expression_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						BinaryExpressionMistmatchDiagnostic {
							file_id: ctx.resolve_context.file_id,
							left_type: Spanned {
								inner: left_type,
								span: left.span,
							},
							operator,
							right_type: Spanned {
								inner: right_type,
								span: right.span,
							},
						},
					));

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: TypeIndex::BOOL,
					span: expr.span,
				})
			}
		}
	}

	fn build_assignment_expr(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (left, right, operator) = match &expr.inner {
			ast::Expression::Binary {
				left,
				right,
				operator,
			} => (left, right, *operator),
			_ => unreachable!(),
		};

		let left = self.build_expression(
			ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Write,
			},
			left,
		)?;
		match left.kind {
			ExprKind::Local {
				scope_index,
				local_index,
			} => {
				let local_type =
					ctx.stack.get_local(scope_index, local_index).ty;
				let mut right = self.build_expression(
					ctx,
					AccessContext {
						expected_type: local_type,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right.ty.is_comptime_number() {
					self.coerce_untyped_expr(ctx, &mut right, local_type)?;
				} else if !self.coercible_to(right.ty, local_type) {
					self.tir.diagnostics.push(
						report_binary_expression_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryExpressionMistmatchDiagnostic {
								file_id: ctx.resolve_context.file_id,
								left_type: Spanned {
									inner: local_type,
									span: left.span,
								},
								operator,
								right_type: Spanned {
									inner: right.ty,
									span: right.span,
								},
							},
						),
					);
				}

				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(left),
						operator,
						right: Box::new(right),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			ExprKind::Global { id } => {
				let global_index = self.tir.expect_global_index(id);
				let global = &self.tir.globals[global_index as usize];
				let global_type = global.ty.inner;
				let mut right = self.build_expression(
					ctx,
					AccessContext {
						expected_type: global_type,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right.ty.is_comptime_number() {
					self.coerce_untyped_expr(ctx, &mut right, global_type)?;
				} else if !self.coercible_to(right.ty, global_type) {
					self.tir.diagnostics.push(
						report_binary_expression_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryExpressionMistmatchDiagnostic {
								file_id: ctx.resolve_context.file_id,
								left_type: Spanned {
									inner: global_type,
									span: left.span,
								},
								operator,
								right_type: Spanned {
									inner: right.ty,
									span: right.span,
								},
							},
						),
					);
				}

				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(left),
						operator,
						right: Box::new(right),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			ExprKind::Placeholder => {
				let right = self.build_expression(
					ctx,
					AccessContext {
						expected_type: TypeIndex::INFER,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right.ty.is_comptime_number() {
					self.tir.diagnostics.push(report_type_annotation_required(
						SourceSpan::new(
							ctx.resolve_context.file_id,
							right.span,
						),
					));
					return Err(());
				}
				let right_type = right.ty;

				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(Expression {
							kind: ExprKind::Placeholder,
							ty: right_type,
							span: left.span,
						}),
						operator,
						right: Box::new(right),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			ExprKind::Load { place } => {
				let inner_ty = place.ty;
				let left_span = left.span;
				let mut right_expr = self.build_expression(
					ctx,
					AccessContext {
						expected_type: inner_ty,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right_expr.ty.is_comptime_number() {
					self.coerce_untyped_expr(ctx, &mut right_expr, inner_ty)?;
				} else if !self.coercible_to(right_expr.ty, inner_ty) {
					self.tir.diagnostics.push(
						report_binary_expression_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryExpressionMistmatchDiagnostic {
								file_id: ctx.resolve_context.file_id,
								left_type: Spanned {
									inner: inner_ty,
									span: left_span,
								},
								operator,
								right_type: Spanned {
									inner: right_expr.ty,
									span: right_expr.span,
								},
							},
						),
					);
				}
				Ok(Expression {
					kind: ExprKind::Store {
						target: place,
						value: Box::new(right_expr),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			ExprKind::ObjectAccess { ref object, .. } => {
				if !matches!(
					object.kind,
					ExprKind::Local { .. } | ExprKind::Global { .. }
				) {
					self.tir.diagnostics.push(
						report_invalid_assignment_target(SourceSpan::new(
							ctx.resolve_context.file_id,
							left.span,
						)),
					);
					return Ok(Expression {
						kind: ExprKind::Error,
						ty: TypeIndex::UNIT,
						span: expr.span,
					});
				}
				let field_ty = left.ty;
				let left_span = left.span;
				let mut right_expr = self.build_expression(
					ctx,
					AccessContext {
						expected_type: field_ty,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right_expr.ty.is_comptime_number() {
					self.coerce_untyped_expr(ctx, &mut right_expr, field_ty)?;
				} else if !self.coercible_to(right_expr.ty, field_ty) {
					self.tir.diagnostics.push(
						report_binary_expression_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryExpressionMistmatchDiagnostic {
								file_id: ctx.resolve_context.file_id,
								left_type: Spanned {
									inner: field_ty,
									span: left_span,
								},
								operator,
								right_type: Spanned {
									inner: right_expr.ty,
									span: right_expr.span,
								},
							},
						),
					);
				}
				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(left),
						operator,
						right: Box::new(right_expr),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			ExprKind::Error => {
				let right_expr = self
					.build_expression(
						ctx,
						AccessContext {
							expected_type: TypeIndex::ERROR,
							access_kind: AccessKind::Read,
						},
						right,
					)
					.unwrap_or(Expression {
						kind: ExprKind::Error,
						ty: TypeIndex::ERROR,
						span: right.span,
					});
				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(left),
						operator,
						right: Box::new(right_expr),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			_ => {
				self.tir.diagnostics.push(report_invalid_assignment_target(
					SourceSpan::new(ctx.resolve_context.file_id, left.span),
				));

				Ok(Expression {
					kind: ExprKind::Error,
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
		}
	}

	fn build_arithmetic_assignment_expr(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (left, right, operator) = match &expr.inner {
			ast::Expression::Binary {
				left,
				right,
				operator,
			} => (left, right, *operator),
			_ => unreachable!(),
		};

		let left = self.build_expression(
			ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::ReadWrite,
			},
			left,
		)?;
		match left.kind {
			ExprKind::Local {
				scope_index,
				local_index,
			} => {
				let local_type =
					ctx.stack.get_local(scope_index, local_index).ty;
				if !local_type.is_primitive() {
					self.tir.diagnostics.push(
						report_binary_operator_cannot_be_applied(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryOperatorCannotBeAppliedDiagnostic {
								file_id: ctx.resolve_context.file_id,
								operator,
								operand: Spanned {
									inner: local_type,
									span: left.span,
								},
							},
						),
					);

					return Err(());
				}
				let mut right = self.build_expression(
					ctx,
					AccessContext {
						expected_type: local_type,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right.ty.is_comptime_number() {
					self.coerce_untyped_expr(ctx, &mut right, local_type)?;
				} else if !self.coercible_to(right.ty, local_type) {
					self.tir.diagnostics.push(
						report_binary_expression_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryExpressionMistmatchDiagnostic {
								file_id: ctx.resolve_context.file_id,
								left_type: Spanned {
									inner: local_type,
									span: left.span,
								},
								operator,
								right_type: Spanned {
									inner: right.ty,
									span: right.span,
								},
							},
						),
					);
				}

				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(left),
						operator,
						right: Box::new(right),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			ExprKind::Global { id } => {
				let global_index = self.tir.expect_global_index(id);
				let global =
					self.tir.globals.get(global_index as usize).unwrap();

				if !global.ty.inner.is_primitive() {
					self.tir.diagnostics.push(
						report_binary_operator_cannot_be_applied(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryOperatorCannotBeAppliedDiagnostic {
								file_id: ctx.resolve_context.file_id,
								operator,
								operand: Spanned {
									inner: global.ty.inner,
									span: left.span,
								},
							},
						),
					);

					return Err(());
				}

				let global_type = global.ty.inner;
				let mut right = self.build_expression(
					ctx,
					AccessContext {
						expected_type: global_type,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right.ty.is_comptime_number() {
					self.coerce_untyped_expr(ctx, &mut right, global_type)?;
				} else if !self.coercible_to(right.ty, global_type) {
					self.tir.diagnostics.push(
						report_binary_expression_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryExpressionMistmatchDiagnostic {
								file_id: ctx.resolve_context.file_id,
								left_type: Spanned {
									inner: global_type,
									span: left.span,
								},
								operator,
								right_type: Spanned {
									inner: right.ty,
									span: right.span,
								},
							},
						),
					);
				}

				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(left),
						operator,
						right: Box::new(right),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			ExprKind::Load { place } => {
				let inner_ty = place.ty;
				if !inner_ty.is_primitive() {
					self.tir.diagnostics.push(
						report_binary_operator_cannot_be_applied(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryOperatorCannotBeAppliedDiagnostic {
								file_id: ctx.resolve_context.file_id,
								operator,
								operand: Spanned {
									inner: inner_ty,
									span: left.span,
								},
							},
						),
					);
					return Err(());
				}
				let mut right_expr = self.build_expression(
					ctx,
					AccessContext {
						expected_type: inner_ty,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right_expr.ty.is_comptime_number() {
					self.coerce_untyped_expr(ctx, &mut right_expr, inner_ty)?;
				} else if !self.coercible_to(right_expr.ty, inner_ty) {
					self.tir.diagnostics.push(
						report_binary_expression_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryExpressionMistmatchDiagnostic {
								file_id: ctx.resolve_context.file_id,
								left_type: Spanned {
									inner: inner_ty,
									span: left.span,
								},
								operator,
								right_type: Spanned {
									inner: right_expr.ty,
									span: right_expr.span,
								},
							},
						),
					);
				}
				let left_span = left.span;
				let left_ty = left.ty;
				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(Expression {
							kind: ExprKind::Load { place },
							ty: left_ty,
							span: left_span,
						}),
						operator,
						right: Box::new(right_expr),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			ExprKind::ObjectAccess { ref object, .. } => {
				if !matches!(
					object.kind,
					ExprKind::Local { .. } | ExprKind::Global { .. }
				) {
					self.tir.diagnostics.push(
						report_invalid_assignment_target(SourceSpan::new(
							ctx.resolve_context.file_id,
							left.span,
						)),
					);
					return Ok(Expression {
						kind: ExprKind::Error,
						ty: TypeIndex::UNIT,
						span: expr.span,
					});
				}
				let field_ty = left.ty;
				if !field_ty.is_primitive() {
					self.tir.diagnostics.push(
						report_binary_operator_cannot_be_applied(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryOperatorCannotBeAppliedDiagnostic {
								file_id: ctx.resolve_context.file_id,
								operator,
								operand: Spanned {
									inner: field_ty,
									span: left.span,
								},
							},
						),
					);
					return Err(());
				}
				let left_span = left.span;
				let mut right_expr = self.build_expression(
					ctx,
					AccessContext {
						expected_type: field_ty,
						access_kind: AccessKind::Read,
					},
					right,
				)?;
				if right_expr.ty.is_comptime_number() {
					self.coerce_untyped_expr(ctx, &mut right_expr, field_ty)?;
				} else if !self.coercible_to(right_expr.ty, field_ty) {
					self.tir.diagnostics.push(
						report_binary_expression_mistmatch(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryExpressionMistmatchDiagnostic {
								file_id: ctx.resolve_context.file_id,
								left_type: Spanned {
									inner: field_ty,
									span: left_span,
								},
								operator,
								right_type: Spanned {
									inner: right_expr.ty,
									span: right_expr.span,
								},
							},
						),
					);
				}
				Ok(Expression {
					kind: ExprKind::Binary {
						left: Box::new(left),
						operator,
						right: Box::new(right_expr),
					},
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
			_ => {
				self.tir.diagnostics.push(report_invalid_assignment_target(
					SourceSpan::new(ctx.resolve_context.file_id, left.span),
				));

				Ok(Expression {
					kind: ExprKind::Error,
					ty: TypeIndex::UNIT,
					span: expr.span,
				})
			}
		}
	}

	fn build_return_expression(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let value = match &expr.inner {
			ast::Expression::Return { value } => value,
			_ => unreachable!(),
		};

		match value {
			Some(value) => {
				let expected_type =
					ctx.stack.scopes.first().unwrap().expected_type;
				let mut built = match self.build_expression(
					ctx,
					AccessContext {
						expected_type,
						access_kind: AccessKind::Read,
					},
					value,
				) {
					Ok(v) => v,
					Err(()) => {
						return Ok(Expression {
							kind: ExprKind::Unreachable,
							ty: TypeIndex::NEVER,
							span: expr.span,
						});
					}
				};

				let inferred_type = {
					let scope = ctx.stack.scopes.get_mut(0).unwrap();
					let inferred_type = self.infer_block_type(
						ctx.resolve_context.file_id,
						scope,
						&built,
					)?;
					scope.inferred_type = inferred_type;
					inferred_type
				};

				if built.ty.is_comptime_number() {
					if inferred_type.is_comptime_number() {
						self.tir.diagnostics.push(
							report_type_annotation_required(SourceSpan::new(
								ctx.resolve_context.file_id,
								built.span,
							)),
						);
						return Err(());
					}
					self.coerce_untyped_expr(ctx, &mut built, inferred_type)?;
				}

				let expected_type =
					ctx.stack.scopes.first().unwrap().expected_type;
				if expected_type != TypeIndex::INFER
					&& !self.coercible_to(inferred_type, expected_type)
				{
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type,
							actual_type: inferred_type,
							span: SourceSpan::new(
								ctx.resolve_context.file_id,
								built.span,
							),
						},
					));
					return Err(());
				}

				Ok(Expression {
					kind: ExprKind::Return {
						value: Some(Box::new(built)),
					},
					ty: TypeIndex::NEVER,
					span: expr.span,
				})
			}
			None => {
				let scope =
					ctx.stack.scopes.get_mut(ctx.scope_index as usize).unwrap();

				let inferred_type =
					scope.inferred_type.infer_or(TypeIndex::UNIT);
				scope.inferred_type = inferred_type;

				let expected_type = scope.expected_type;
				if expected_type != TypeIndex::INFER
					&& self.coercible_to(inferred_type, expected_type)
				{
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type,
							actual_type: inferred_type,
							span: SourceSpan::new(
								ctx.resolve_context.file_id,
								expr.span,
							),
						},
					));
					return Err(());
				}

				Ok(Expression {
					kind: ExprKind::Return { value: None },
					ty: TypeIndex::NEVER,
					span: expr.span,
				})
			}
		}
	}

	fn build_arithmetic_expr(
		&mut self,
		ctx: &mut ExprContext,
		expr: &Spanned<ast::Expression>,
		access_ctx: AccessContext,
	) -> Result<Expression, ()> {
		let (left, right, operator) = match &expr.inner {
			ast::Expression::Binary {
				left,
				right,
				operator,
			} => (left, right, *operator),
			_ => unreachable!(),
		};

		let mut left = self.build_expression(
			ctx,
			AccessContext {
				expected_type: access_ctx.expected_type,
				access_kind: AccessKind::Read,
			},
			left,
		)?;
		let mut right = self.build_expression(
			ctx,
			AccessContext {
				expected_type: match &self.tir.types[left.ty.as_usize()] {
					Type::Integer
					| Type::Float
					| Type::Error
					| Type::Never
					| Type::Unit => access_ctx.expected_type,
					_ => left.ty,
				},
				access_kind: AccessKind::Read,
			},
			right,
		)?;

		match (left.ty, right.ty) {
			(l, r) if l.is_comptime_number() && r.is_comptime_number() => {
				if l != r {
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type: l,
							actual_type: r,
							span: SourceSpan::new(
								ctx.resolve_context.file_id,
								right.span,
							),
						},
					));
					return Ok(Expression {
						kind: ExprKind::Binary {
							operator,
							left: Box::new(left),
							right: Box::new(right),
						},
						ty: TypeIndex::ERROR,
						span: expr.span,
					});
				}
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: l,
					span: expr.span,
				})
			}
			(l, ty) if l.is_comptime_number() => {
				if !ty.is_primitive() {
					self.tir.diagnostics.push(
						report_binary_operator_cannot_be_applied(
							TypeFormatter::new(&self.tir, self.interner),
							BinaryOperatorCannotBeAppliedDiagnostic {
								file_id: ctx.resolve_context.file_id,
								operator,
								operand: Spanned {
									inner: ty,
									span: right.span,
								},
							},
						),
					);

					return Ok(Expression {
						kind: ExprKind::Binary {
							operator,
							left: Box::new(left),
							right: Box::new(right),
						},
						ty: TypeIndex::ERROR,
						span: expr.span,
					});
				}
				self.coerce_untyped_expr(ctx, &mut left, ty)?;

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty,
					span: expr.span,
				})
			}
			(ty, r) if r.is_comptime_number() => {
				// TODO: check if primitive
				self.coerce_untyped_expr(ctx, &mut right, ty)?;

				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty,
					span: expr.span,
				})
			}
			(l, _) if l == TypeIndex::NEVER => {
				self.tir.diagnostics.push(report_unreachable_code(
					SourceSpan::new(ctx.resolve_context.file_id, right.span),
				));

				Ok(left)
			}
			(_, r) if r == TypeIndex::NEVER => {
				self.tir.diagnostics.push(report_unreachable_code(
					SourceSpan::new(ctx.resolve_context.file_id, operator.span),
				));

				Ok(right)
			}
			(left_type, right_type)
				if left_type == right_type
					&& self.is_arithmetic_type(left_type) =>
			{
				Ok(Expression {
					kind: ExprKind::Binary {
						operator,
						left: Box::new(left),
						right: Box::new(right),
					},
					ty: left_type,
					span: expr.span,
				})
			}
			(left_type, right_type) => {
				self.tir
					.diagnostics
					.push(report_binary_expression_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						BinaryExpressionMistmatchDiagnostic {
							file_id: ctx.resolve_context.file_id,
							left_type: Spanned {
								inner: left_type,
								span: left.span,
							},
							operator,
							right_type: Spanned {
								inner: right_type,
								span: right.span,
							},
						},
					));

				if access_ctx.expected_type != TypeIndex::INFER {
					Ok(Expression {
						kind: ExprKind::Binary {
							operator,
							left: Box::new(left),
							right: Box::new(right),
						},
						ty: access_ctx.expected_type,
						span: expr.span,
					})
				} else {
					Err(())
				}
			}
		}
	}

	/// True when `ty` can be used as an operand in arithmetic or comparison
	/// expressions.  Extends `is_primitive()` to cover `AssocTypeProjection`
	/// types bounded by a typeset (e.g. `M::Size` where `type Size: PointerSize`).
	/// Currently all typesets consist entirely of integer primitives, so any
	/// typeset-bounded projection is unconditionally accepted here.
	/// TODO: re-check each typeset member when non-numeric typesets are added.
	fn is_arithmetic_type(&self, ty: TypeIndex) -> bool {
		if ty.is_primitive() {
			return true;
		}
		let Type::AssocTypeProjection {
			trait_index,
			assoc_name,
			..
		} = &self.tir.types[ty.as_usize()]
		else {
			return false;
		};
		self.tir.traits[*trait_index as usize]
			.assoc_types
			.get(assoc_name)
			.is_some_and(|a| a.bounds.typeset.is_some())
	}

	fn type_param_typeset_bound(&self, ty: TypeIndex) -> Option<TypesetIndex> {
		let Type::TypeParam {
			ref owner,
			param_index,
		} = self.tir.types[ty.as_usize()]
		else {
			return None;
		};
		self.tir
			.type_param_info(*owner, param_index as usize)
			.bounds
			.typeset
	}

	/// Returns the typeset bound for any type that can carry one:
	/// `TypeParam` (via its `typeset_bound` field) or `AssocTypeProjection`
	/// (via the trait's associated-type `typeset_bound`).
	fn typeset_bound_for(&self, ty: TypeIndex) -> Option<TypesetIndex> {
		match &self.tir.types[ty.as_usize()] {
			Type::TypeParam { .. } => self.type_param_typeset_bound(ty),
			Type::AssocTypeProjection {
				trait_index,
				assoc_name,
				..
			} => self.tir.traits[*trait_index as usize]
				.assoc_types
				.get(assoc_name)
				.and_then(|at| at.bounds.typeset),
			_ => None,
		}
	}

	/// True when concrete `ty` is a member of the given typeset.
	fn concrete_type_in_typeset(
		&self,
		ty: TypeIndex,
		typeset_index: TypesetIndex,
	) -> bool {
		self.tir.typesets[typeset_index as usize]
			.members
			.contains(&ty)
	}

	/// After type_args are finalized for a generic call, check that each
	/// type arg satisfies the typeset bounds of its type parameter.
	/// If the type arg is itself a TypeParam (nested generic context), check
	/// that it has the required typeset bound rather than requiring membership.
	fn check_typeset_bounds_on_type_args(
		&mut self,
		func_index: FunctionIndex,
		type_args: &[TypeIndex],
		file_id: FileId,
		call_span: TextSpan,
	) {
		let type_params: Vec<(SymbolU32, Option<TypesetIndex>)> = self
			.tir
			.function_type_params_iter(func_index)
			.map(|tp| (tp.name, tp.bounds.typeset))
			.collect();
		for (i, (param_name, typeset_bound)) in
			type_params.iter().copied().enumerate()
		{
			let Some(ts_index) = typeset_bound else {
				continue;
			};
			let Some(&arg_ty) = type_args.get(i) else {
				continue;
			};
			if arg_ty == TypeIndex::ERROR {
				continue;
			}
			let satisfied = match &self.tir.types[arg_ty.as_usize()] {
				// Nested generic: the caller's TypeParam forwards here — check its typeset bound.
				Type::TypeParam { .. } => {
					self.type_param_typeset_bound(arg_ty) == Some(ts_index)
				}
				// Concrete type: must be a member of the typeset.
				_ => self.concrete_type_in_typeset(arg_ty, ts_index),
			};
			if !satisfied {
				let type_name = TypeFormatter::new(&self.tir, self.interner)
					.display_type(arg_ty)
					.unwrap_or_default();
				let set_name = self
					.interner
					.resolve(self.tir.typesets[ts_index as usize].name.inner)
					.unwrap_or("?")
					.to_owned();
				let param_name_str =
					self.interner.resolve(param_name).unwrap_or("?").to_owned();
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_code(DiagnosticCode::TypesetBoundViolation.code())
						.with_message(format!(
							"type `{type_name}` is not a member of typeset `{set_name}`"
						))
						.with_label(
							Label::primary(file_id, call_span).with_message(
								format!(
									"`{param_name_str}` requires a type from `{set_name}`"
								),
							),
						),
				);
			}
		}
	}

	fn build_generic_call_arguments(
		&mut self,
		ctx: &mut ExprContext,
		func_index: FunctionIndex,
		arguments: &mut [Expression],
		mut type_args: Box<[TypeIndex]>,
		expected_result: TypeIndex,
		call_span: TextSpan,
	) -> Result<Box<[TypeIndex]>, ()> {
		if expected_result != TypeIndex::INFER {
			let result_type = self.tir.functions[func_index as usize]
				.result
				.as_ref()
				.map(|r| r.inner)
				.unwrap_or(TypeIndex::UNIT);
			self.infer_type_args(&mut type_args, result_type, expected_result);
		}
		for (index, arg) in arguments.iter().enumerate() {
			let param_type =
				match self.tir.functions[func_index as usize].params.get(index)
				{
					Some(p) => p.ty.inner,
					None => break,
				};
			self.infer_type_args(&mut type_args, param_type, arg.ty);
		}

		// Detect unresolvable type parameters by substituting the current type_args
		// into the function's result type and checking whether INFER survives.
		//
		// substitute_type propagates INFER through TypeParam positions but leaves
		// AssocTypeProjection positions unchanged (because those are resolved
		// structurally at the call site rather than requiring a concrete type arg).
		// This means `contains_infer` on the substituted result is false for
		// params that appear only via `C::Item` style projections, and true for
		// params that appear directly (e.g. M in `Layout<M>`).
		let result_type = self.tir.functions[func_index as usize]
			.result
			.as_ref()
			.map(|r| r.inner)
			.unwrap_or(TypeIndex::UNIT);
		let substituted_result = self.substitute_type(result_type, &type_args);
		let mut had_unresolved = false;
		if self.contains_infer(substituted_result) {
			for (i, &slot) in type_args.iter().enumerate() {
				if slot == TypeIndex::INFER {
					let param_name_sym = self
						.tir
						.function_type_params_iter(func_index)
						.nth(i)
						.expect(
							"type_args length must equal total_type_param_count",
						)
						.name;
					let param_name = self
						.interner
						.resolve(param_name_sym)
						.unwrap_or("?")
						.to_string();
					self.tir.diagnostics.push(
						Diagnostic::error()
							.with_code(
								DiagnosticCode::TypeAnnotationRequired.code(),
							)
							.with_message(format!(
								"cannot infer type for type parameter `{param_name}`"
							))
							.with_label(
								Label::primary(
									ctx.resolve_context.file_id,
									call_span,
								)
								.with_message("type annotation required"),
							),
					);
					had_unresolved = true;
				}
			}
		}
		if had_unresolved {
			return Err(());
		}

		let mut had_error = false;
		for (index, arg) in arguments.iter_mut().enumerate() {
			let param_type =
				match self.tir.functions[func_index as usize].params.get(index)
				{
					Some(p) => p.ty.inner,
					None => break,
				};

			let expected_type =
				self.substitute_expected_type(param_type, &type_args);

			if expected_type != TypeIndex::INFER {
				if arg.ty.is_comptime_number() {
					if self
						.coerce_untyped_expr(ctx, arg, expected_type)
						.is_err()
					{
						had_error = true;
					}
				} else if !self.coercible_to(arg.ty, expected_type) {
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type,
							actual_type: arg.ty,
							span: SourceSpan::new(
								ctx.resolve_context.file_id,
								arg.span,
							),
						},
					));
					had_error = true;
				}
			} else if arg.ty.is_comptime_number() {
				self.tir.diagnostics.push(report_type_annotation_required(
					SourceSpan::new(ctx.resolve_context.file_id, arg.span),
				));
				had_error = true;
			}
		}

		// Any slot still INFER after return-type and argument inference is a
		// phantom param — one that appears nowhere in the function's signature
		// and can never be constrained.  Skip if coercion already failed to
		// avoid double-reporting on top of a TypeMistmatch.
		if !had_error {
			for (i, &slot) in type_args.iter().enumerate() {
				if slot == TypeIndex::INFER {
					let param_name_sym =
						self.tir.functions[func_index as usize].type_params[i]
							.name;
					let param_name = self
						.interner
						.resolve(param_name_sym)
						.unwrap_or("?")
						.to_string();
					self.tir.diagnostics.push(
						Diagnostic::error()
							.with_code(
								DiagnosticCode::TypeAnnotationRequired.code(),
							)
							.with_message(format!(
								"cannot infer type for type parameter `{param_name}`"
							))
							.with_label(
								Label::primary(
									ctx.resolve_context.file_id,
									call_span,
								)
								.with_message("type annotation required"),
							),
					);
					had_error = true;
				}
			}
		}

		if had_error { Err(()) } else { Ok(type_args) }
	}

	fn build_call_arguments(
		&mut self,
		ctx: &mut ExprContext,
		arguments: &[Separated<Spanned<ast::Expression>>],
		params: &[TypeIndex],
		type_args: &[TypeIndex],
	) -> Box<[Expression]> {
		let mut result: Vec<Expression> = Vec::with_capacity(arguments.len());
		for (index, argument) in arguments.iter().enumerate() {
			let expected_type = params
				.get(index)
				.copied()
				.map(|param_type| {
					self.substitute_expected_type(param_type, type_args)
				})
				.unwrap_or(TypeIndex::INFER);

			let mut argument = match self.build_expression(
				ctx,
				AccessContext {
					expected_type,
					access_kind: AccessKind::Read,
				},
				&argument.inner,
			) {
				Ok(expr) => expr,
				Err(_) => {
					result.push(Expression {
						kind: ExprKind::Error,
						span: argument.inner.span,
						ty: TypeIndex::ERROR,
					});
					continue;
				}
			};

			if expected_type != TypeIndex::INFER {
				if argument.ty.is_comptime_number() {
					_ = self.coerce_untyped_expr(
						ctx,
						&mut argument,
						expected_type,
					);
				} else if !self.coercible_to(argument.ty, expected_type) {
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type,
							actual_type: argument.ty,
							span: SourceSpan::new(
								ctx.resolve_context.file_id,
								argument.span,
							),
						},
					));
				}
			} else if argument.ty.is_comptime_number() {
				self.tir.diagnostics.push(report_type_annotation_required(
					SourceSpan::new(ctx.resolve_context.file_id, argument.span),
				));
			}

			result.push(argument);
		}

		result.into_boxed_slice()
	}

	fn build_call_expression(
		&mut self,
		ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let (ast_callee, arguments) = match &expr.inner {
			ast::Expression::Call { callee, arguments } => (callee, arguments),
			_ => unreachable!(),
		};

		let callee = self.build_expression(
			ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Read,
			},
			ast_callee,
		)?;
		let signature = match &self.tir.types[callee.ty.as_usize()] {
			Type::Function { signature } => signature.clone(),
			Type::FunctionItem { id, .. } => {
				let signature_index = self.tir.functions
					[self.tir.expect_function_index(*id) as usize]
					.signature_index;
				match &self.tir.types[signature_index.as_usize()] {
					Type::Function { signature } => signature.clone(),
					_ => unreachable!(),
				}
			}
			_ => {
				// still trying to check arguments, even though we don't have information about the parameters
				let arguments: Box<_> = arguments
					.iter()
					.map(|arg| {
						match self.build_expression(
							ctx,
							AccessContext {
								expected_type: TypeIndex::INFER,
								access_kind: AccessKind::Read,
							},
							&arg.inner,
						) {
							Ok(expr) => expr,
							Err(_) => Expression {
								kind: ExprKind::Error,
								ty: TypeIndex::ERROR,
								span: arg.inner.span,
							},
						}
					})
					.collect();

				if callee.ty != TypeIndex::ERROR {
					let formatter =
						TypeFormatter::new(&self.tir, self.interner);
					let mut diagnostic = Diagnostic::error()
						.with_code(DiagnosticCode::CannotCallExpression.code())
						.with_message("call expression requires function")
						.with_label(
							SourceSpan::new(
								ctx.resolve_context.file_id,
								ast_callee.span,
							)
							.primary_label()
							.with_message(format!(
								"expected function, found `{}`",
								formatter.display_type(callee.ty).unwrap()
							)),
						);
					if ast_callee.inner.is_block_like() {
						diagnostic = diagnostic.with_note(
							"consider using a semicolon here to finish the statement: `;`",
						);
					}
					self.tir.diagnostics.push(diagnostic);
				}

				return Ok(Expression {
					kind: ExprKind::Call {
						callee: Box::new(callee),
						arguments,
					},
					ty: TypeIndex::ERROR,
					span: expr.span,
				});
			}
		};
		if arguments.len() != signature.params().len() {
			self.tir.diagnostics.push(report_argument_count_mismatch(
				TypeFormatter::new(&self.tir, self.interner),
				ArgumentCountMismatchDiagnostic {
					actual_count: arguments.len(),
					params: signature.params(),
					call_span: SourceSpan::new(
						ctx.resolve_context.file_id,
						callee.span,
					),
					is_method: false,
				},
			));
		}

		let direct_id = match &callee.kind {
			ExprKind::Function { id } => Some(*id),
			ExprKind::NamespaceAccess { member, .. } => {
				if let ExprKind::Function { id } = &member.kind {
					Some(*id)
				} else {
					None
				}
			}
			_ => None,
		};
		if let Some(callee_id) = direct_id {
			let func_index = self.tir.expect_function_index(callee_id);
			let type_params_len = self.tir.functions[func_index as usize]
				.total_type_param_count();
			if type_params_len > 0 {
				let mut built_args = Vec::with_capacity(arguments.len());
				for arg in arguments.iter() {
					built_args.push(self.build_expression(
						ctx,
						AccessContext {
							expected_type: TypeIndex::INFER,
							access_kind: AccessKind::Read,
						},
						&arg.inner,
					)?);
				}

				// FunctionItem.type_args is always padded to type_params_len (with
				// impl-level args pre-filled and remaining slots as INFER) by the time
				// we get here — build_namespace_member_expression enforces this invariant.
				let type_args: Box<[TypeIndex]> = match &self.tir.types
					[callee.ty.as_usize()]
				{
					Type::FunctionItem { type_args, .. } => type_args.clone(),
					_ => vec![TypeIndex::INFER; type_params_len]
						.into_boxed_slice(),
				};

				let type_args = self.build_generic_call_arguments(
					ctx,
					func_index,
					&mut built_args,
					type_args,
					access_ctx.expected_type,
					expr.span,
				)?;
				self.check_typeset_bounds_on_type_args(
					func_index,
					&type_args,
					ctx.resolve_context.file_id,
					expr.span,
				);
				let return_ty =
					self.substitute_type(signature.result(), &type_args);

				return Ok(Expression {
					kind: ExprKind::GenericCall {
						id: callee_id,
						type_args,
						arguments: built_args.into_boxed_slice(),
					},
					ty: return_ty,
					span: expr.span,
				});
			}
		}

		let arguments =
			self.build_call_arguments(ctx, arguments, signature.params(), &[]);
		Ok(Expression {
			kind: ExprKind::Call {
				callee: Box::new(callee),
				arguments,
			},
			ty: signature.result(),
			span: expr.span,
		})
	}

	/// Looks up `method_name` on `ty` in direct impl members or trait bounds (for type params).
	fn try_method_entry(
		&self,
		ty: TypeIndex,
		method_name: SymbolU32,
	) -> Option<ImplEntry> {
		match &self.tir.types[ty.as_usize()] {
			Type::TypeParam { owner, param_index } => self
				.tir
				.type_param_info(*owner, *param_index as usize)
				.bounds
				.traits
				.iter()
				.find_map(|b| {
					self.tir.traits[b.trait_index as usize]
						.members
						.get(&method_name)
						.cloned()
				}),
			_ => self
				.tir
				.impl_members
				.get(&ty)
				.and_then(|m| m.get(&method_name))
				.cloned(),
		}
	}

	/// Resolves a method call on `ty`, including one level of pointer auto-deref.
	/// Returns `(func_index, type_args)` on success. `type_args` is empty for non-generic
	/// methods, filled with `INFER` for generic methods whose args must be inferred from
	/// the call, and pre-concrete for generic impl methods (inferred from the receiver type).
	/// Reports a diagnostic and returns `Err` when no method is found or the entry is not
	/// callable as a method.
	fn resolve_method_call(
		&mut self,
		file_id: FileId,
		ty: TypeIndex,
		method: Spanned<SymbolU32>,
	) -> Result<(FunctionIndex, Box<[TypeIndex]>), ()> {
		// Pointer types cannot have impl blocks, so look up methods on the inner type directly.
		let lookup_ty = match &self.tir.types[ty.as_usize()] {
			Type::Pointer { to, .. } => *to,
			_ => ty,
		};

		match self.try_method_entry(lookup_ty, method.inner) {
			Some(ImplEntry::Method(func_index)) => {
				let n = self.tir.functions[func_index as usize]
					.total_type_param_count();
				return Ok((
					func_index,
					vec![TypeIndex::INFER; n].into_boxed_slice(),
				));
			}
			Some(_) => {
				self.tir.diagnostics.push(report_not_a_method(
					SourceSpan::new(file_id, method.span),
					TypeFormatter::new(&self.tir, self.interner),
					method.inner,
					lookup_ty,
				));
				return Err(());
			}
			None => {}
		}

		if let Some((block_idx, type_args)) =
			self.find_generic_impl(lookup_ty, method.inner)
		{
			match self.tir.generic_impl_list[block_idx]
				.members
				.get(&method.inner)
				.cloned()
			{
				Some(ImplEntry::Method(func_index)) => {
					return Ok((func_index, type_args));
				}
				Some(_) => {
					self.tir.diagnostics.push(report_not_a_method(
						SourceSpan::new(file_id, method.span),
						TypeFormatter::new(&self.tir, self.interner),
						method.inner,
						lookup_ty,
					));
					return Err(());
				}
				None => {}
			}
		}

		self.tir.diagnostics.push(report_method_not_found(
			SourceSpan::new(file_id, method.span),
			TypeFormatter::new(&self.tir, self.interner),
			method.inner,
			ty,
		));
		Err(())
	}

	fn build_method_call_expression(
		&mut self,
		ctx: &mut ExprContext,
		access_ctx: AccessContext,
		expr: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let MethodCallExpr {
			arguments,
			method,
			object,
			..
		} = match &expr.inner {
			ast::Expression::MethodCall(method_call) => method_call.as_ref(),
			_ => unreachable!(),
		};

		let object = self.build_expression(
			ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Read,
			},
			object,
		)?;

		let file_id = ctx.resolve_context.file_id;
		let (func_index, type_args) =
			self.resolve_method_call(file_id, object.ty, *method)?;

		self.tir.functions[func_index as usize]
			.accesses
			.push(SourceSpan::new(file_id, method.span));
		let id = self.tir.functions[func_index as usize].id;
		let signature_index =
			self.tir.functions[func_index as usize].signature_index;
		let signature = match &self.tir.types[signature_index.as_usize()] {
			Type::Function { signature } => signature.clone(),
			_ => unreachable!(),
		};
		let non_self_params = &signature.params()[1..];
		if arguments.len() != non_self_params.len() {
			self.tir.diagnostics.push(report_argument_count_mismatch(
				TypeFormatter::new(&self.tir, self.interner),
				ArgumentCountMismatchDiagnostic {
					actual_count: arguments.len(),
					params: non_self_params,
					call_span: SourceSpan::new(file_id, object.span),
					is_method: true,
				},
			));
		}

		if type_args.is_empty() {
			if let Some(&self_param_ty) = signature.params().first() {
				if !self.coercible_to(object.ty, self_param_ty) {
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type: self_param_ty,
							actual_type: object.ty,
							span: SourceSpan::new(file_id, object.span),
						},
					));
				}
			}
			let args =
				self.build_call_arguments(ctx, arguments, non_self_params, &[]);
			return Ok(Expression {
				kind: ExprKind::MethodCall {
					arguments: std::iter::once(object).chain(args).collect(),
					id,
				},
				ty: signature.result(),
				span: expr.span,
			});
		}

		let mut built_arguments = std::iter::once(Ok(object))
			.chain(arguments.iter().map(|arg| {
				self.build_expression(
					ctx,
					AccessContext {
						expected_type: TypeIndex::INFER,
						access_kind: AccessKind::Read,
					},
					&arg.inner,
				)
			}))
			.collect::<Result<Box<_>, _>>()?;
		let inferred_type_args = self.build_generic_call_arguments(
			ctx,
			func_index,
			&mut built_arguments,
			type_args,
			access_ctx.expected_type,
			expr.span,
		)?;
		self.check_typeset_bounds_on_type_args(
			func_index,
			&inferred_type_args,
			file_id,
			expr.span,
		);
		let return_ty =
			self.substitute_type(signature.result(), &inferred_type_args);
		Ok(Expression {
			kind: ExprKind::GenericMethodCall {
				id,
				type_args: inferred_type_args,
				arguments: built_arguments,
			},
			ty: return_ty,
			span: expr.span,
		})
	}

	fn build_local_definition_statement(
		&mut self,
		ctx: &mut ExprContext,
		stmt: &Separated<Spanned<ast::Statement>>,
	) -> Result<Expression, ()> {
		let (mut_span, name, ty, value) = match &stmt.inner.inner {
			ast::Statement::LocalDefinition { pattern, ty, value } => {
				match &pattern.inner {
					ast::Pattern::Binding { mut_span, name } => {
						(*mut_span, *name, ty, value)
					}
					_ => {
						self.tir.diagnostics.push(
							codespan_reporting::diagnostic::Diagnostic::error()
								.with_message(
									"pattern destructuring in locals is not yet supported",
								)
								.with_label(Label::primary(
									ctx.resolve_context.file_id,
									pattern.span,
								)),
						);
						return Err(());
					}
				}
			}
			_ => unreachable!(),
		};

		let expected_type = match ty {
			Some(ty) => self.resolve_type(ctx.resolve_context, ctx.scope, ty),
			None => TypeIndex::INFER,
		};
		let value_result = self.build_expression(
			ctx,
			AccessContext {
				expected_type,
				access_kind: AccessKind::Read,
			},
			value,
		);

		let (ty, value) = match value_result {
			Err(()) => {
				// Expression failed; register the local with the declared type so
				// subsequent references don't produce cascading errors.
				let ty = expected_type.infer_or(TypeIndex::ERROR);
				let error_expr = Expression {
					kind: ExprKind::Error,
					ty: TypeIndex::ERROR,
					span: value.span,
				};
				(ty, error_expr)
			}
			Ok(mut value) => {
				let ty = self.resolve_local_type(
					ctx,
					name.span,
					&mut value,
					expected_type,
				)?;
				(ty, value)
			}
		};

		let local_index = ctx.push_local(Local {
			name,
			ty,
			mut_span,
			accesses: Vec::new(),
		});

		Ok(Expression {
			kind: ExprKind::LocalDeclaration {
				name,
				scope_index: ctx.scope_index,
				local_index,
				value: Box::new(value),
			},
			ty: if ty == TypeIndex::NEVER {
				TypeIndex::NEVER
			} else {
				TypeIndex::UNIT
			},
			span: stmt.inner.span,
		})
	}

	fn resolve_local_type(
		&mut self,
		ctx: &mut ExprContext,
		name_span: TextSpan,
		value: &mut Expression,
		expected_type: TypeIndex,
	) -> Result<TypeIndex, ()> {
		let file_id = ctx.resolve_context.file_id;
		if expected_type == TypeIndex::INFER {
			// TODO: impove diagnostic for case where value.ty contains infer
			if value.ty.is_comptime_number() || self.contains_infer(value.ty) {
				self.tir.diagnostics.push(report_type_annotation_required(
					SourceSpan::new(file_id, name_span),
				));
				return Ok(TypeIndex::ERROR);
			}
			return Ok(value.ty);
		}

		if value.ty == TypeIndex::ERROR {
			return Ok(expected_type);
		}

		if value.ty.is_comptime_number() {
			if self.coerce_untyped_expr(ctx, value, expected_type).is_err() {
				return Ok(TypeIndex::ERROR);
			}
			return Ok(expected_type);
		}

		if self.contains_infer(expected_type) {
			if self.type_satisfies_annotation(value.ty, expected_type) {
				return Ok(value.ty);
			}
			self.tir.diagnostics.push(report_type_mistmatch(
				TypeFormatter::new(&self.tir, self.interner),
				TypeMistmatchDiagnostic {
					expected_type,
					actual_type: value.ty,
					span: SourceSpan::new(file_id, value.span),
				},
			));
			return Ok(expected_type);
		}

		if self.coercible_to(value.ty, expected_type) {
			return Ok(expected_type);
		}

		self.tir.diagnostics.push(report_type_mistmatch(
			TypeFormatter::new(&self.tir, self.interner),
			TypeMistmatchDiagnostic {
				expected_type,
				actual_type: value.ty,
				span: SourceSpan::new(file_id, value.span),
			},
		));
		Ok(expected_type)
	}

	fn coerce_untyped_expr(
		&mut self,
		ctx: &mut ExprContext,
		expr: &mut Expression,
		target_type: TypeIndex,
	) -> Result<(), ()> {
		let file_id = ctx.resolve_context.file_id;
		// if target_type == TypeIndex::INFER {
		//     self.tir
		//         .diagnostics
		//         .push(report_type_annotation_required(SourceSpan::new(
		//             file_id, expr.span,
		//         )));
		//     return Err(());
		// }
		match expr.kind {
			ExprKind::Int { .. } => {
				self.coerce_untyped_int_expr(file_id, expr, target_type)
			}
			ExprKind::Float { .. } => {
				self.coerce_untyped_float_expr(file_id, expr, target_type)
			}
			ExprKind::Unary { .. } => {
				self.coerce_untyped_unary_expr(ctx, expr, target_type)
			}
			ExprKind::Binary { .. } => {
				self.coerce_untyped_binary_expression(ctx, expr, target_type)
			}
			ExprKind::Block {
				scope_index,
				result: Some(ref mut result),
				..
			} => {
				self.coerce_untyped_expr(ctx, result, target_type)?;
				expr.ty = target_type;
				ctx.stack.scopes[scope_index as usize].inferred_type =
					target_type;
				Ok(())
			}
			// Any other expression kind that ends up here already had an error
			// reported; propagate failure without emitting a second diagnostic.
			_ => Err(()),
		}
	}

	fn coerce_untyped_int_expr(
		&mut self,
		file_id: FileId,
		expr: &mut Expression,
		target_idx: TypeIndex,
	) -> Result<(), ()> {
		let value = match expr.kind {
			ExprKind::Int { value } => value,
			_ => unreachable!(),
		};
		let formatter = TypeFormatter::new(&self.tir, self.interner);

		if target_idx == TypeIndex::I32 {
			if value > i32::MAX as i64 || value < i32::MIN as i64 {
				self.tir
					.diagnostics
					.push(report_integer_literal_out_of_range(
						formatter,
						IntegerLiteralOutOfRangeDiagnostic {
							ty: TypeIndex::I32,
							value,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
			}
			expr.ty = TypeIndex::I32;
			Ok(())
		} else if target_idx == TypeIndex::I64 {
			// `value` is already an `i64`, so it can never fall outside
			// `i64::MIN..=i64::MAX` — literals too large to parse as `i64`
			// are rejected earlier, in `parse_integer_literal`.
			expr.ty = TypeIndex::I64;
			Ok(())
		} else if target_idx == TypeIndex::U32 {
			if value > u32::MAX as i64 || value < 0 {
				self.tir
					.diagnostics
					.push(report_integer_literal_out_of_range(
						formatter,
						IntegerLiteralOutOfRangeDiagnostic {
							ty: TypeIndex::U32,
							value,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
			}
			expr.ty = TypeIndex::U32;
			Ok(())
		} else if target_idx == TypeIndex::U64 {
			// i64 is at most i64::MAX which always fits in u64; only negative values are
			// invalid
			if value < 0 {
				self.tir
					.diagnostics
					.push(report_integer_literal_out_of_range(
						formatter,
						IntegerLiteralOutOfRangeDiagnostic {
							ty: TypeIndex::U64,
							value,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
			}
			expr.ty = TypeIndex::U64;
			Ok(())
		} else if target_idx == TypeIndex::U8 {
			if value < 0 || value > u8::MAX as i64 {
				self.tir
					.diagnostics
					.push(report_integer_literal_out_of_range(
						formatter,
						IntegerLiteralOutOfRangeDiagnostic {
							ty: TypeIndex::U8,
							value,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
			}
			expr.ty = TypeIndex::U8;
			Ok(())
		} else if target_idx == TypeIndex::I8 {
			if value < i8::MIN as i64 || value > i8::MAX as i64 {
				self.tir
					.diagnostics
					.push(report_integer_literal_out_of_range(
						formatter,
						IntegerLiteralOutOfRangeDiagnostic {
							ty: TypeIndex::I8,
							value,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
			}
			expr.ty = TypeIndex::I8;
			Ok(())
		} else if target_idx == TypeIndex::U16 {
			if value < 0 || value > u16::MAX as i64 {
				self.tir
					.diagnostics
					.push(report_integer_literal_out_of_range(
						formatter,
						IntegerLiteralOutOfRangeDiagnostic {
							ty: TypeIndex::U16,
							value,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
			}
			expr.ty = TypeIndex::U16;
			Ok(())
		} else if target_idx == TypeIndex::I16 {
			if value < i16::MIN as i64 || value > i16::MAX as i64 {
				self.tir
					.diagnostics
					.push(report_integer_literal_out_of_range(
						formatter,
						IntegerLiteralOutOfRangeDiagnostic {
							ty: TypeIndex::I16,
							value,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
			}
			expr.ty = TypeIndex::I16;
			Ok(())
		} else if target_idx == TypeIndex::CHAR {
			if value < 0 || value > u32::MAX as i64 {
				self.tir
					.diagnostics
					.push(report_integer_literal_out_of_range(
						formatter,
						IntegerLiteralOutOfRangeDiagnostic {
							ty: TypeIndex::CHAR,
							value,
							span: SourceSpan::new(file_id, expr.span),
						},
					));
			}
			expr.ty = TypeIndex::CHAR;
			Ok(())
		} else if target_idx == TypeIndex::F32 || target_idx == TypeIndex::F64 {
			self.tir
				.diagnostics
				.push(report_integer_literal_for_float_type(SourceSpan::new(
					file_id, expr.span,
				)));
			Err(())
		} else if matches!(
			self.tir.types[target_idx.as_usize()],
			Type::Pointer { .. }
		) {
			match self.type_scalar(target_idx) {
				Some(WasmScalar::I32) => {
					if value < 0 || value > u32::MAX as i64 {
						self.tir.diagnostics.push(
							report_integer_literal_out_of_range(
								formatter,
								IntegerLiteralOutOfRangeDiagnostic {
									ty: TypeIndex::U32,
									value,
									span: SourceSpan::new(file_id, expr.span),
								},
							),
						);
					}
				}
				Some(WasmScalar::I64) => {
					if value < 0 {
						self.tir.diagnostics.push(
							report_integer_literal_out_of_range(
								formatter,
								IntegerLiteralOutOfRangeDiagnostic {
									ty: TypeIndex::U64,
									value,
									span: SourceSpan::new(file_id, expr.span),
								},
							),
						);
					}
				}
				_ => {
					// Generic pointer (TypeParam memory) — validate against the
					// `#[tag = "pointer_size"]` typeset (`PointerSize` in std.wx).
					if let Some(ts) = self
						.interner
						.get("pointer_size")
						.and_then(|key| self.tir.tagged_items.get(&key))
						.and_then(|tagged_id| {
							self.tir.typeset_index(*tagged_id)
						})
						.map(|idx| &self.tir.typesets[idx as usize])
					{
						if !ts.intersection_range.contains(value) {
							let ts_name = self
								.interner
								.resolve(ts.name.inner)
								.unwrap_or("PointerSize")
								.to_string();
							self.tir.diagnostics.push(
								report_integer_literal_out_of_typeset_range(
									value,
									&ts_name,
									&ts.intersection_range,
									SourceSpan::new(file_id, expr.span),
								),
							);
							return Err(());
						}
					}
				}
			}
			expr.ty = target_idx;
			Ok(())
		} else if let Some(typeset_index) = self.typeset_bound_for(target_idx) {
			let ts = &self.tir.typesets[typeset_index as usize];
			let range = &ts.intersection_range;
			let ts_name = self
				.interner
				.resolve(ts.name.inner)
				.unwrap_or("?")
				.to_string();
			if !range.contains(value) {
				self.tir.diagnostics.push(
					report_integer_literal_out_of_typeset_range(
						value,
						&ts_name,
						range,
						SourceSpan::new(file_id, expr.span),
					),
				);
				return Err(());
			}
			expr.ty = target_idx;
			Ok(())
		} else {
			self.tir.diagnostics.push(report_unable_to_coerce(
				formatter,
				target_idx,
				SourceSpan::new(file_id, expr.span),
			));
			Err(())
		}
	}

	fn coerce_untyped_float_expr(
		&mut self,
		file_id: FileId,
		expr: &mut Expression,
		target_idx: TypeIndex,
	) -> Result<(), ()> {
		if target_idx == TypeIndex::F32 {
			// TODO: add a diagnostic if the literal is out of range
			expr.ty = TypeIndex::F32;
			Ok(())
		} else if target_idx == TypeIndex::F64 {
			// TODO: add a diagnostic if the literal is out of range
			expr.ty = TypeIndex::F64;
			Ok(())
		} else {
			self.tir.diagnostics.push(report_unable_to_coerce(
				TypeFormatter::new(&self.tir, self.interner),
				target_idx,
				SourceSpan::new(file_id, expr.span),
			));
			Err(())
		}
	}

	fn coerce_untyped_unary_expr(
		&mut self,
		ctx: &mut ExprContext,
		expr: &mut Expression,
		target_idx: TypeIndex,
	) -> Result<(), ()> {
		let file_id = ctx.resolve_context.file_id;
		let (operand, operator) = match &mut expr.kind {
			ExprKind::Unary { operand, operator } => (operand, operator.inner),
			_ => unreachable!(),
		};

		match operator {
			ast::UnaryOp::BitNot | ast::UnaryOp::InvertSign => {
				let is_valid = target_idx == TypeIndex::I32
					|| target_idx == TypeIndex::I64;
				if !is_valid {
					self.tir.diagnostics.push(report_unable_to_coerce(
						TypeFormatter::new(&self.tir, self.interner),
						target_idx,
						SourceSpan::new(file_id, expr.span),
					));
					return Err(());
				}
			}
			_ => unreachable!(),
		}

		self.coerce_untyped_expr(ctx, operand, target_idx).map(|_| {
			let _: () = expr.ty = target_idx;
		})
	}

	fn coerce_untyped_binary_expression(
		&mut self,
		ctx: &mut ExprContext,
		expr: &mut Expression,
		target_idx: TypeIndex,
	) -> Result<(), ()> {
		let file_id = ctx.resolve_context.file_id;
		let (left, right, operator) = match &mut expr.kind {
			ExprKind::Binary {
				operator,
				left,
				right,
			} => (left, right, operator.inner),
			_ => unreachable!(),
		};

		match operator {
			operator if operator.is_arithmetic() => {
				if !target_idx.is_primitive() {
					self.tir.diagnostics.push(report_unable_to_coerce(
						TypeFormatter::new(&self.tir, self.interner),
						target_idx,
						SourceSpan::new(file_id, expr.span),
					));
					return Err(());
				}
			}
			operator if operator.is_bitwise() => {
				let is_integer = target_idx == TypeIndex::I32
					|| target_idx == TypeIndex::I64
					|| target_idx == TypeIndex::U32
					|| target_idx == TypeIndex::U64;
				if !is_integer {
					self.tir.diagnostics.push(report_unable_to_coerce(
						TypeFormatter::new(&self.tir, self.interner),
						target_idx,
						SourceSpan::new(file_id, expr.span),
					));
					return Err(());
				}
			}
			_ => unreachable!(),
		};

		let left_result = self.coerce_untyped_expr(ctx, left, target_idx);
		let right_result = self.coerce_untyped_expr(ctx, right, target_idx);
		match (left_result, right_result) {
			(Ok(_), Ok(_)) => {
				expr.ty = target_idx;
				Ok(())
			}
			_ => Err(()),
		}
	}

	fn build_struct_init_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		init_span: ast::TextSpan,
		path: &[ast::PathSegment],
		fields: &[ast::Separated<ast::Spanned<ast::StructInitField>>],
	) -> Result<Expression, ()> {
		let struct_seg = path.last().expect("path has at least one segment");
		let file_id = func_ctx.resolve_context.file_id;

		// Shared with type-position resolution: handles namespace walking,
		// turbofish (with alias support and INFER-padding for short arg
		// lists), and plain identifiers uniformly.
		let struct_ty = self.resolve_path_type(
			func_ctx.resolve_context,
			func_ctx.scope,
			path,
			init_span,
		);
		if struct_ty == TypeIndex::ERROR {
			return Err(());
		}

		let struct_index = match &self.tir.types[struct_ty.as_usize()] {
			Type::Struct { struct_index, .. } => *struct_index,
			_ => {
				let name = self
					.interner
					.resolve(struct_seg.ident.inner)
					.unwrap()
					.to_string();
				self.tir.diagnostics.push(report_not_a_struct_type(
					file_id,
					name,
					struct_seg.ident.span,
				));
				return Err(());
			}
		};

		// Priority: explicit turbofish (already resolved, alias-substituted,
		// and INFER-padded into struct_ty's args by resolve_path_type) >
		// args concretely embedded in path type (e.g. `Self` inside
		// `impl<M,T> Vec<M,T>` — must be more than just INFER placeholders,
		// or a bare generic reference padded by resolve_path_type would be
		// mistaken for a real instantiation) > infer from expected type >
		// empty (inferred per-field below).
		let type_params_len =
			self.tir.structs[struct_index as usize].type_params.len();
		let resolved_args: Box<[TypeIndex]> =
			if !struct_seg.type_args.is_empty() {
				match &self.tir.types[struct_ty.as_usize()] {
					Type::Struct { args, .. } => args.clone(),
					_ => Box::new([]),
				}
			} else if type_params_len == 0 {
				Box::new([])
			} else {
				match &self.tir.types[struct_ty.as_usize()] {
					Type::Struct { args, .. }
						if args.len() == type_params_len
							&& args.iter().any(|a| *a != TypeIndex::INFER) =>
					{
						args.clone()
					}
					_ => match &self.tir.types
						[access_ctx.expected_type.as_usize()]
					{
						Type::Struct {
							struct_index: esi,
							args,
						} if *esi == struct_index
							&& args.len() == type_params_len =>
						{
							args.clone()
						}
						_ => Box::new([]),
					},
				}
			};

		let struct_name = self
			.interner
			.resolve(self.tir.structs[struct_index as usize].name.inner)
			.unwrap()
			.to_string();
		let field_count = self.tir.structs[struct_index as usize].fields.len();
		// Tracks the field name span of the first mention of each field (regardless of
		// whether the value built successfully). Used for duplicate detection and to
		// distinguish genuinely-missing fields from errored ones.
		let mut first_mention: Vec<Option<ast::TextSpan>> =
			(0..field_count).map(|_| None).collect();
		let mut field_slots: Vec<Option<Expression>> =
			(0..field_count).map(|_| None).collect();

		for field in fields.iter() {
			let field = &field.inner.inner;
			let field_name = self.interner.resolve(field.name.inner).unwrap();

			let field_index = match self.tir.structs[struct_index as usize]
				.lookup
				.get(&field.name.inner)
				.copied()
			{
				Some(idx) => idx,
				None => {
					self.tir.diagnostics.push(report_unknown_struct_field(
						UnknownStructFieldDiagnostic {
							file_id: func_ctx.resolve_context.file_id,
							struct_name: &struct_name,
							field_name,
							field_span: field.name.span,
						},
					));
					continue;
				}
			};

			if let Some(first_span) = first_mention[field_index] {
				self.tir
					.diagnostics
					.push(report_duplicate_struct_field_init(
						field_name,
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							first_span,
						),
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							field.name.span,
						),
					));
				continue;
			}
			// Mark this field as mentioned (by its name span) before building the value,
			// so that build errors don't cause it to appear in the "missing fields" list.
			first_mention[field_index] = Some(field.name.span);
			self.tir.structs[struct_index as usize].fields[field_index]
				.accesses
				.push(FieldAccess {
					kind: FieldAccessKind::Init,
					file_id: func_ctx.resolve_context.file_id,
					span: field.name.span,
				});

			let raw_expected_ty = self.tir.structs[struct_index as usize]
				.fields[field_index]
				.ty
				.inner;
			let expected_ty = if resolved_args.is_empty() {
				raw_expected_ty
			} else {
				self.substitute_type(raw_expected_ty, &resolved_args)
			};
			let field_value = match &field.value {
				Some(expr) => expr.as_ref(),
				None => {
					// Shorthand: treat `{ a }` as `{ a: a }` by synthesising a single-segment path
					&ast::Spanned {
						inner: ast::Expression::Path(Box::new([
							ast::PathSegment {
								ident: field.name,
								type_args: Box::new([]),
							},
						])),
						span: field.name.span,
					}
				}
			};
			let mut field_expr = match self.build_expression(
				func_ctx,
				AccessContext {
					expected_type: expected_ty,
					access_kind: AccessKind::Read,
				},
				field_value,
			) {
				Ok(e) => e,
				Err(_) => continue,
			};

			if field_expr.ty.is_comptime_number() {
				match self.coerce_untyped_expr(
					func_ctx,
					&mut field_expr,
					expected_ty,
				) {
					Ok(_) => {}
					Err(_) => continue,
				}
			} else if !self.coercible_to(field_expr.ty, expected_ty) {
				self.tir.diagnostics.push(report_type_mistmatch(
					TypeFormatter::new(&self.tir, self.interner),
					TypeMistmatchDiagnostic {
						expected_type: expected_ty,
						actual_type: field_expr.ty,
						span: SourceSpan::new(
							func_ctx.resolve_context.file_id,
							field_expr.span,
						),
					},
				));
				continue;
			}

			field_slots[field_index] = Some(field_expr);
		}

		let missing: Box<[&str]> = first_mention
			.iter()
			.enumerate()
			.filter(|(_, m)| m.is_none())
			.map(|(i, _)| {
				self.interner
					.resolve(
						self.tir.structs[struct_index as usize].fields[i]
							.name
							.inner,
					)
					.unwrap()
			})
			.collect();
		if !missing.is_empty() {
			self.tir.diagnostics.push(report_missing_struct_fields(
				MissingStructFieldsDiagnostic {
					file_id: func_ctx.resolve_context.file_id,
					struct_name: &struct_name,
					missing_fields: missing,
					init_span,
				},
			));
		}

		let ty = self.intern_type(Type::Struct {
			struct_index,
			args: resolved_args,
		});

		// If any field was mentioned but failed to build (type error, coercion error,
		// …), its slot is still None even though first_mention is Some. Return
		// an error expression so we don't panic on unwrap, and the error has
		// already been reported above.
		let has_field_errors = field_slots.iter().any(|s| s.is_none());
		if has_field_errors {
			return Ok(Expression {
				kind: ExprKind::StructInit {
					struct_index,
					fields: Box::new([]),
				},
				ty,
				span: init_span,
			});
		}

		let fields: Box<[Expression]> =
			field_slots.into_iter().map(|e| e.unwrap()).collect();
		Ok(Expression {
			kind: ExprKind::StructInit {
				struct_index,
				fields,
			},
			ty,
			span: init_span,
		})
	}

	fn build_tuple_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		span: ast::TextSpan,
		ast_elements: &[ast::Spanned<ast::Expression>],
		access_ctx: AccessContext,
	) -> Result<Expression, ()> {
		if ast_elements.is_empty() {
			return Ok(Expression {
				kind: ExprKind::TupleInit {
					elements: Box::new([]),
				},
				ty: TypeIndex::UNIT,
				span,
			});
		}

		// If the expected type is a tuple, use its element types as hints.
		let expected_elems: Option<Box<[TypeIndex]>> =
			match &self.tir.types[access_ctx.expected_type.as_usize()] {
				Type::Tuple { elements }
					if elements.len() == ast_elements.len() =>
				{
					Some(elements.clone())
				}
				_ => None,
			};

		let mut built = Vec::with_capacity(ast_elements.len());
		let mut had_error = false;
		for (i, elem_expr) in ast_elements.iter().enumerate() {
			let expected = expected_elems
				.as_ref()
				.map(|e| e[i])
				.unwrap_or(TypeIndex::INFER);
			match self.build_expression(
				func_ctx,
				AccessContext {
					expected_type: expected,
					access_kind: AccessKind::Read,
				},
				elem_expr,
			) {
				Ok(mut e) => {
					if e.ty.is_comptime_number() && expected != TypeIndex::INFER
					{
						let _ = self
							.coerce_untyped_expr(func_ctx, &mut e, expected);
					}
					built.push(e);
				}
				Err(()) => {
					had_error = true;
				}
			}
		}

		let elem_types: Box<[TypeIndex]> = built.iter().map(|e| e.ty).collect();
		let ty = self.intern_type(Type::Tuple {
			elements: elem_types,
		});

		if had_error {
			return Ok(Expression {
				kind: ExprKind::TupleInit {
					elements: Box::new([]),
				},
				ty,
				span,
			});
		}

		Ok(Expression {
			kind: ExprKind::TupleInit {
				elements: built.into_boxed_slice(),
			},
			ty,
			span,
		})
	}

	fn build_deref_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		span: ast::TextSpan,
		pointer: &Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		// Always build the pointer expression with Read — we only need to read
		// the pointer value itself. Write-through is governed by the pointer type.
		let pointer = self.build_expression(
			func_ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Read,
			},
			pointer,
		)?;

		let (inner_ty, memory, mutable) =
			match &self.tir.types[pointer.ty.as_usize()] {
				Type::Pointer {
					to,
					memory,
					mutable,
				} => (*to, *memory, *mutable),
				_ => {
					self.tir.diagnostics.push(report_cannot_deref_non_pointer(
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							pointer.span,
						),
						TypeFormatter::new(&self.tir, self.interner)
							.display_type(pointer.ty)
							.unwrap(),
					));
					return Err(());
				}
			};

		if matches!(
			access_ctx.access_kind,
			AccessKind::Write | AccessKind::ReadWrite
		) && !mutable
		{
			self.tir.diagnostics.push(
				report_cannot_store_through_immutable_pointer(SourceSpan::new(
					func_ctx.resolve_context.file_id,
					span,
				)),
			);
		}

		Ok(Expression {
			kind: ExprKind::Load {
				place: Box::new(Place {
					kind: PlaceKind::Deref {
						pointer: Box::new(pointer),
					},
					ty: inner_ty,
					memory,
					mutable,
					span,
				}),
			},
			ty: inner_ty,
			span,
		})
	}

	fn resolve_ambient_memory(
		&mut self,
		span: SourceSpan,
	) -> Result<TypeIndex, ()> {
		match self.tir.memories.len() {
			0 => {
				self.tir
					.diagnostics
					.push(report_no_memory_for_pointer(span));
				Err(())
			}
			1 => {
				let mem = &self.tir.memories[0];
				let id = mem.id;
				let kind = mem.kind;
				Ok(self.intern_type(Type::Memory { id, kind }))
			}
			_ => {
				self.tir
					.diagnostics
					.push(report_ambiguous_pointer_memory(span));
				Err(())
			}
		}
	}

	fn pointer_type_for_memory(&mut self, memory: TypeIndex) -> TypeIndex {
		match &self.tir.types[memory.as_usize()].clone() {
			Type::Memory { id, .. } => {
				let idx = self.tir.expect_memory_index(*id);
				self.tir.memories[idx as usize].kind
			}
			Type::TypeParam { owner, param_index } => {
				// Generic `M: Memory` — the index type is `M::Size`.
				// Find the first bound trait that declares `Size` as an assoc type.
				let size_sym = self.interner.get_or_intern("Size");
				let param_index = *param_index;
				let bounds = self
					.tir
					.type_param_info(*owner, param_index as usize)
					.bounds
					.traits
					.to_owned();
				let trait_index = bounds
					.iter()
					.find(|b| {
						self.tir.traits[b.trait_index as usize]
							.assoc_types
							.contains_key(&size_sym)
					})
					.map(|b| b.trait_index);
				match trait_index {
					Some(trait_index) => {
						self.intern_type(Type::AssocTypeProjection {
							trait_index,
							assoc_name: size_sym,
							base: memory,
						})
					}
					// No bound with Size — fall back to untyped; will be caught
					// by type checking if the user provides a typed index.
					None => TypeIndex::INTEGER,
				}
			}
			_ => TypeIndex::INTEGER,
		}
	}

	fn build_array_literal_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		span: ast::TextSpan,
		elements: &[ast::Spanned<ast::Expression>],
	) -> Result<Expression, ()> {
		let source_span =
			SourceSpan::new(func_ctx.resolve_context.file_id, span);

		let (expected_of, expected_memory, expected_size, expected_mutable) =
			match self.tir.types[access_ctx.expected_type.as_usize()].clone() {
				Type::Array {
					of,
					memory,
					size,
					mutable,
				} => (of, Some(memory), Some(size), mutable),
				_ => (TypeIndex::INFER, None, None, false),
			};

		if let Some(expected_size) = expected_size {
			if elements.len() as u32 != expected_size {
				self.tir.diagnostics.push(report_array_size_mismatch(
					source_span,
					expected_size,
					elements.len(),
				));
				return Err(());
			}
		}

		let mut built = Vec::with_capacity(elements.len());
		for element in elements {
			let mut elem = self.build_expression(
				func_ctx,
				AccessContext {
					expected_type: expected_of,
					access_kind: AccessKind::Read,
				},
				element,
			)?;
			if elem.ty.is_comptime_number() {
				if expected_of != TypeIndex::INFER {
					self.coerce_untyped_expr(func_ctx, &mut elem, expected_of)?;
				} else {
					self.tir.diagnostics.push(report_type_annotation_required(
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							elem.span,
						),
					));
					return Err(());
				}
			}
			if !elem.ty.is_numeric() {
				self.tir.diagnostics.push(
					Diagnostic::error()
						.with_message(
							"array element type must be a numeric type",
						)
						.with_label(Label::primary(
							func_ctx.resolve_context.file_id,
							elem.span,
						)),
				);
				return Err(());
			}
			if !matches!(
				elem.kind,
				ExprKind::Int { .. } | ExprKind::Float { .. }
			) {
				self.tir.diagnostics.push(report_array_element_not_const(
					SourceSpan::new(
						func_ctx.resolve_context.file_id,
						elem.span,
					),
				));
				return Err(());
			}
			built.push(elem);
		}

		let elem_type = if let Some(first) = built.first() {
			let ty = first.ty;
			for elem in &built[1..] {
				if elem.ty != ty {
					self.tir.diagnostics.push(report_type_mistmatch(
						TypeFormatter::new(&self.tir, self.interner),
						TypeMistmatchDiagnostic {
							expected_type: ty,
							actual_type: elem.ty,
							span: SourceSpan::new(
								func_ctx.resolve_context.file_id,
								elem.span,
							),
						},
					));
					return Err(());
				}
			}
			ty
		} else if expected_of != TypeIndex::INFER {
			expected_of
		} else {
			self.tir
				.diagnostics
				.push(report_type_annotation_required(source_span));
			return Err(());
		};

		let memory = match expected_memory {
			Some(m) => m,
			None => self.resolve_ambient_memory(source_span)?,
		};
		let array_ty = self.intern_type(Type::Array {
			of: elem_type,
			size: elements.len() as u32,
			memory,
			mutable: expected_mutable,
		});

		Ok(Expression {
			kind: ExprKind::ArrayLiteral {
				elements: built.into_boxed_slice(),
				memory,
			},
			ty: array_ty,
			span,
		})
	}

	fn build_array_repeat_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		span: ast::TextSpan,
		value_expr: &ast::Spanned<ast::Expression>,
		count_expr: &ast::Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		let source_span =
			SourceSpan::new(func_ctx.resolve_context.file_id, span);

		let (expected_of, expected_memory, expected_mutable) =
			match self.tir.types[access_ctx.expected_type.as_usize()].clone() {
				Type::Array {
					of,
					memory,
					mutable,
					..
				} => (of, Some(memory), mutable),
				_ => (TypeIndex::INFER, None, false),
			};

		let count_built = self.build_expression(
			func_ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Read,
			},
			count_expr,
		)?;
		let count =
			match count_built.kind {
				ExprKind::Int { value } if value >= 0 => value as u32,
				_ => {
					self.tir.diagnostics.push(
						report_array_repeat_count_not_const(SourceSpan::new(
							func_ctx.resolve_context.file_id,
							count_expr.span,
						)),
					);
					return Err(());
				}
			};

		if let Type::Array { size, .. } =
			self.tir.types[access_ctx.expected_type.as_usize()].clone()
		{
			if count != size {
				self.tir.diagnostics.push(report_array_size_mismatch(
					source_span,
					size,
					count as usize,
				));
				return Err(());
			}
		}

		let mut value = self.build_expression(
			func_ctx,
			AccessContext {
				expected_type: expected_of,
				access_kind: AccessKind::Read,
			},
			value_expr,
		)?;
		if value.ty.is_comptime_number() {
			if expected_of != TypeIndex::INFER {
				self.coerce_untyped_expr(func_ctx, &mut value, expected_of)?;
			} else {
				self.tir.diagnostics.push(report_type_annotation_required(
					SourceSpan::new(
						func_ctx.resolve_context.file_id,
						value.span,
					),
				));
				return Err(());
			}
		}
		if !value.ty.is_numeric() {
			self.tir.diagnostics.push(
				Diagnostic::error()
					.with_message("array element type must be a numeric type")
					.with_label(Label::primary(
						func_ctx.resolve_context.file_id,
						value.span,
					)),
			);
			return Err(());
		}
		if !matches!(value.kind, ExprKind::Int { .. } | ExprKind::Float { .. })
		{
			self.tir.diagnostics.push(report_array_element_not_const(
				SourceSpan::new(func_ctx.resolve_context.file_id, value.span),
			));
			return Err(());
		}

		let memory = match expected_memory {
			Some(m) => m,
			None => self.resolve_ambient_memory(source_span)?,
		};
		let array_ty = self.intern_type(Type::Array {
			of: value.ty,
			size: count,
			memory,
			mutable: expected_mutable,
		});

		Ok(Expression {
			kind: ExprKind::ArrayRepeat {
				value: Box::new(value),
				count,
				memory,
			},
			ty: array_ty,
			span,
		})
	}

	fn build_index_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		access_ctx: AccessContext,
		span: ast::TextSpan,
		object_expr: &ast::Spanned<ast::Expression>,
		index_expr: &ast::Spanned<ast::Expression>,
	) -> Result<Expression, ()> {
		// Always build the indexed object with Read — write-through is governed
		// by the array/slice type's mutable flag, not the binding.
		let object = self.build_expression(
			func_ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Read,
			},
			object_expr,
		)?;

		let (elem_type, memory, mutable) =
			match self.tir.types[object.ty.as_usize()].clone() {
				Type::Array {
					of,
					memory,
					mutable,
					..
				} => (of, memory, mutable),
				Type::Slice {
					of,
					memory,
					mutable,
				} => (of, memory, mutable),
				_ => {
					self.tir.diagnostics.push(report_index_on_non_indexable(
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							object.span,
						),
						TypeFormatter::new(&self.tir, self.interner)
							.display_type(object.ty)
							.unwrap(),
					));
					return Err(());
				}
			};

		if matches!(
			access_ctx.access_kind,
			AccessKind::Write | AccessKind::ReadWrite
		) && !mutable
		{
			self.tir.diagnostics.push(
				report_cannot_store_through_immutable_pointer(SourceSpan::new(
					func_ctx.resolve_context.file_id,
					span,
				)),
			);
		}

		let index_type = self.pointer_type_for_memory(memory);

		let mut index = self.build_expression(
			func_ctx,
			AccessContext {
				expected_type: index_type,
				access_kind: AccessKind::Read,
			},
			index_expr,
		)?;
		if index.ty.is_comptime_number() {
			self.coerce_untyped_expr(func_ctx, &mut index, index_type)?;
		} else if index.ty != index_type {
			self.tir.diagnostics.push(report_type_mistmatch(
				TypeFormatter::new(&self.tir, self.interner),
				TypeMistmatchDiagnostic {
					expected_type: index_type,
					actual_type: index.ty,
					span: SourceSpan::new(
						func_ctx.resolve_context.file_id,
						index.span,
					),
				},
			));
		}

		Ok(Expression {
			kind: ExprKind::Load {
				place: Box::new(Place {
					kind: PlaceKind::Index {
						object: Box::new(object),
						index: Box::new(index),
					},
					ty: elem_type,
					memory,
					mutable,
					span,
				}),
			},
			ty: elem_type,
			span,
		})
	}

	fn build_slice_range_expression(
		&mut self,
		func_ctx: &mut ExprContext,
		span: ast::TextSpan,
		object_expr: &ast::Spanned<ast::Expression>,
		start_expr: &Option<Box<ast::Spanned<ast::Expression>>>,
		end_expr: &Option<Box<ast::Spanned<ast::Expression>>>,
	) -> Result<Expression, ()> {
		let object = self.build_expression(
			func_ctx,
			AccessContext {
				expected_type: TypeIndex::INFER,
				access_kind: AccessKind::Read,
			},
			object_expr,
		)?;

		let (elem_type, memory, mutable) =
			match self.tir.types[object.ty.as_usize()].clone() {
				Type::Array {
					of,
					memory,
					mutable,
					..
				} => (of, memory, mutable),
				Type::Slice {
					of,
					memory,
					mutable,
				} => (of, memory, mutable),
				_ => {
					self.tir.diagnostics.push(report_index_on_non_indexable(
						SourceSpan::new(
							func_ctx.resolve_context.file_id,
							object.span,
						),
						TypeFormatter::new(&self.tir, self.interner)
							.display_type(object.ty)
							.unwrap(),
					));
					return Err(());
				}
			};

		let index_type = self.pointer_type_for_memory(memory);

		let mut build_bound = |builder: &mut Self,
		                       ast_expr: &ast::Spanned<ast::Expression>|
		 -> Result<Expression, ()> {
			let mut bound = builder.build_expression(
				func_ctx,
				AccessContext {
					expected_type: index_type,
					access_kind: AccessKind::Read,
				},
				ast_expr,
			)?;
			if bound.ty.is_comptime_number() {
				builder
					.coerce_untyped_expr(func_ctx, &mut bound, index_type)?;
			} else if bound.ty != index_type {
				builder.tir.diagnostics.push(report_type_mistmatch(
					TypeFormatter::new(&builder.tir, builder.interner),
					TypeMistmatchDiagnostic {
						expected_type: index_type,
						actual_type: bound.ty,
						span: SourceSpan::new(
							func_ctx.resolve_context.file_id,
							ast_expr.span,
						),
					},
				));
				return Err(());
			}
			Ok(bound)
		};

		let start = start_expr
			.as_ref()
			.map(|e| build_bound(self, e).map(Box::new))
			.transpose()?;
		let end = end_expr
			.as_ref()
			.map(|e| build_bound(self, e).map(Box::new))
			.transpose()?;

		let result_ty = self.intern_type(Type::Slice {
			of: elem_type,
			memory,
			mutable,
		});
		Ok(Expression {
			kind: ExprKind::SliceRange {
				object: Box::new(object),
				start,
				end,
			},
			ty: result_ty,
			span,
		})
	}
}
