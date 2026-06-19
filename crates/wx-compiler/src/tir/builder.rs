use std::collections::HashMap;

use crate::{ast::MethodCallExpr, tir::*};

struct FunctionContext {
    lookup: HashMap<(ScopeIndex, SymbolU32), LocalIndex>,
    func_index: FunctionIndex,
    scope_index: ScopeIndex,
    stack: StackFrame,
    resolve_context: ResolveContext,
}

impl FunctionContext {
    fn push_local(&mut self, local: Local) -> LocalIndex {
        let name_symbol = local.name.inner;
        let index = self.stack.push_local(self.scope_index, local);
        self.lookup.insert((self.scope_index, name_symbol), index);
        index
    }

    fn resolve_local(&self, symbol: SymbolU32) -> Option<(ScopeIndex, LocalIndex)> {
        let mut scope_index = self.scope_index;

        loop {
            if let Some(&value) = self.lookup.get(&(scope_index, symbol)) {
                return Some((scope_index, value));
            }

            scope_index = self.stack.scopes[scope_index as usize].parent?;
        }
    }

    fn enter_block<T>(&mut self, block: BlockScope, handler: impl FnOnce(&mut Self) -> T) -> T {
        let parent_scope_index = self.scope_index;
        self.scope_index = self.stack.scopes.len() as u32;
        self.stack.scopes.push(block);

        let result = handler(self);

        self.scope_index = parent_scope_index;
        result
    }

    fn resolve_label(&self, symbol: SymbolU32) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.stack.scopes[scope_index as usize];
            if scope.label.as_ref().is_some_and(|l| l.name == symbol) {
                return Some(scope_index);
            }

            scope_index = match scope.parent {
                Some(parent) => parent,
                None => return None,
            };
        }
    }

    fn get_closest_loop_block(&self) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.stack.scopes[scope_index as usize];
            match scope.kind {
                BlockKind::Loop => return Some(scope_index),
                _ => {}
            }

            scope_index = match scope.parent {
                Some(parent) => parent,
                None => return None,
            }
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

struct Builder<'ast, 'interner> {
    interner: &'interner mut ast::StringInterner,
    symbol_lookup: HashMap<(SymbolNamespace, SymbolU32), SymbolKind>,
    type_index_lookup: HashMap<Type, TypeIndex>,
    tir: TIR,
    id_generator: ast::DefIdGenerator,

    // ── demand-driven resolution ──────────────────────────────────────────────
    /// Populated in Phase 1, in parse order. Index matches `sig_state` entries.
    ast_nodes: Vec<AstEntry<'ast>>,
    /// Maps DefId → SigEntry; populated after Phase 1 with exact capacity.
    sig_state: HashMap<ast::DefId, SigEntry>,
    /// Maps ImplTraitBlock DefId → TraitImplIndex; populated when the block
    /// resolves.
    trait_impl_block_lookup: HashMap<ast::DefId, TraitImplIndex>,
}

enum BlockState<T> {
    Exhaustive(T),
    Incomplete(T),
}

#[derive(Clone, Copy, PartialEq)]
enum ComputeState {
    Pending,
    InProgress,
    Done,
}

#[derive(Clone)]
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

#[derive(Clone)]
struct TypeParamScope {
    owner: TypeParamOwner,
    params: Box<[TypeParamInfo]>,
}

#[derive(Clone)]
struct ResolveContext {
    file_id: FileId,
    namespace: Option<NamespaceIndex>,
    self_type: Option<TypeIndex>,
    type_param_scope: Option<TypeParamScope>,
}

impl ResolveContext {
    fn new(file_id: FileId, namespace: Option<NamespaceIndex>) -> Self {
        Self {
            file_id,
            namespace,
            self_type: None,
            type_param_scope: None,
        }
    }

    fn with_type_param_scope(&self, scope: TypeParamScope) -> Self {
        Self {
            file_id: self.file_id,
            namespace: self.namespace,
            self_type: self.self_type,
            type_param_scope: Some(scope),
        }
    }

    fn with_self_type(&self, self_type: Option<TypeIndex>) -> Self {
        Self {
            file_id: self.file_id,
            namespace: self.namespace,
            self_type,
            type_param_scope: self.type_param_scope.clone(),
        }
    }
}

/// AST node reference for demand-driven resolution.
/// Resolution context (`file_id`, `namespace`) lives in `AstEntry`.
#[derive(Clone)]
enum AstNodeRef<'ast> {
    /// Top-level `fn` or `#[intrinsic]` declaration.
    Function {
        item: &'ast ast::Item,
    },
    ImplMethod {
        impl_target: &'ast ast::Spanned<ast::TypeExpression>,
        item: &'ast ast::ImplItem,
    },
    GenericImplMethod {
        impl_type_params: &'ast [ast::TypeParam],
        impl_target: &'ast ast::Spanned<ast::TypeExpression>,
        item: &'ast ast::ImplItem,
        /// Index into `tir.generic_impl_list` for this impl block.
        block_index: usize,
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
    IntrinsicFunction {
        item: &'ast ast::Item,
    },
}

fn report_missing_enum_repr(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::MissingEnumRepr.code())
        .with_message("enum requires a repr type")
        .with_label(span.primary_label().with_message("add `: <type>` here"))
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
        .with_note("expected `Memory<Size = u32>` or `Memory<Size = u64>`")
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
        .with_label(
            diagnostic
                .first_definition
                .primary_label()
                .with_message(format!(
                    "previous definition of the {} `{}` here",
                    diagnostic.name, namespace
                )),
        )
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

fn report_non_constant_global_initializer(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::NonConstantGlobalInitializer.code())
        .with_message("global variable initializers can only contain constant expressions")
        .with_label(span.primary_label())
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
        .with_note("if you meant to write a string literal, use double quotes: `\"`, `\"`")
}

struct TypeMistmatchDiagnostic {
    expected_type: TypeIndex,
    actual_type: TypeIndex,
    span: SourceSpan,
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
        .with_note("if you don't need the value, consider dropping it with assignment to `_`")
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

fn report_integer_literal_for_float_type(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::LiteralTypeMismatch.code())
        .with_message("cannot use an integer literal for a float type")
        .with_label(span.primary_label())
        .with_note("consider adding a decimal point, e.g. `1.0` instead of `1`")
}

fn report_float_literal_for_integer_type(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::LiteralTypeMismatch.code())
        .with_message("cannot use a float literal for an integer type")
        .with_label(span.primary_label())
        .with_note("remove the decimal point, e.g. `1` instead of `1.0`")
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
    let right_type_name = fmt.display_type(diagnostic.right_type.inner).unwrap();

    let message = match diagnostic.operator.inner {
        ast::BinaryOp::Add => format!("cannot add `{}` to `{}`", left_type_name, right_type_name),
        ast::BinaryOp::Sub => format!(
            "cannot subtract `{}` from `{}`",
            left_type_name, right_type_name
        ),
        ast::BinaryOp::Assign => format!(
            "cannot assign `{}` to `{}`",
            left_type_name, right_type_name
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

fn report_undeclared_label(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::UndeclaredLabel.code())
        .with_message("undeclared label")
        .with_label(span.primary_label())
}

fn report_break_outside_of_loop(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::BreakOutsideOfLoop.code())
        .with_message("`break` outside of loop")
        .with_label(span.primary_label())
        .with_note("`break` is only allowed inside loops or labeled blocks")
}

fn report_cannot_mutate_immutable(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::CannotMutateImmutable.code())
        .with_message("cannot mutate immutable variable")
        .with_label(span.primary_label())
}

fn report_cannot_deref_non_pointer(span: SourceSpan, ty_display: String) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::CannotDerefNonPointer.code())
        .with_message("dereference of non-pointer type")
        .with_label(
            span.primary_label()
                .with_message(format!("type `{}` is not a pointer", ty_display)),
        )
}

fn report_no_memory_for_pointer(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::NoMemoryForPointer.code())
        .with_message("pointer dereference requires a linear memory")
        .with_label(span.primary_label())
        .with_note("declare a memory in this module: `memory <name>: Memory<Size = u32>;`")
}

fn report_ambiguous_pointer_memory(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::AmbiguousPointerMemory.code())
        .with_message("pointer dereference is ambiguous: multiple memories defined")
        .with_label(span.primary_label())
        .with_note("specify which memory with `<memory_name>::*T` syntax")
}

fn report_index_on_non_indexable(span: SourceSpan, type_name: String) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::IndexOnNonIndexable.code())
        .with_message(format!("cannot index into a value of type `{type_name}`"))
        .with_label(span.primary_label())
        .with_note("indexing is only supported on array `[N]T` and slice `[]T` types")
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
        .with_message("array repeat count must be a compile-time integer constant")
        .with_label(span.primary_label())
}

fn report_array_element_not_const(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::ArrayElementNotConst.code())
        .with_message("array literal elements must be compile-time constants")
        .with_label(span.primary_label())
        .with_note("only integer and float literals are allowed in array literals")
}

fn report_cannot_mutate_immutable_array_element(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::CannotMutateImmutable.code())
        .with_message("cannot assign to element of immutable array")
        .with_label(span.primary_label())
        .with_note("consider using `[N]mut T` instead of `[N]T`")
}

fn report_cannot_store_through_immutable_pointer(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::CannotMutateImmutable.code())
        .with_message("cannot assign through immutable pointer")
        .with_label(span.primary_label())
        .with_note("consider using `*mut T` instead of `*T`")
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

fn report_cannot_export_item(name: &str, span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::CannotExportItem.code())
        .with_message(format!("cannot export `{}`", name))
        .with_label(span.primary_label())
        .with_note("only functions and global variables can be exported")
}

fn report_expected_trait(span: SourceSpan, kind: SymbolKind, name: String) -> Diagnostic<FileId> {
    let found = match kind {
        SymbolKind::Struct { .. } => "struct",
        SymbolKind::Enum { .. } => "enum",
        SymbolKind::Memory { .. } => "memory",
        SymbolKind::Module { .. } => "module",
        SymbolKind::Function { .. } => "function",
        SymbolKind::Global { .. } => "global",
        SymbolKind::Const { .. } => "constant",
        SymbolKind::Pending(_) | SymbolKind::TraitAssocType { .. } => unreachable!(),
        _ => "value",
    };
    Diagnostic::error()
        .with_code(DiagnosticCode::ExpectedTrait.code())
        .with_message(format!("expected trait, found {} `{}`", found, name))
        .with_label(span.primary_label())
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
        .with_note("insert some indirection (e.g. a pointer) to break the cycle")
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
            diagnostic =
                diagnostic.with_note(format!("unexpected argument #{}", details.actual_count));
        } else {
            diagnostic = diagnostic.with_note(format!("{} unexpected arguments", extra_count));
        }
    }

    diagnostic
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

fn report_unknown_struct_field(details: UnknownStructFieldDiagnostic<'_>) -> Diagnostic<FileId> {
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
        .with_message(format!("field `{}` specified more than once", field_name))
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

fn report_missing_struct_fields(details: MissingStructFieldsDiagnostic<'_>) -> Diagnostic<FileId> {
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

fn report_associated_type_in_inherent_impl(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::AssociatedTypeInInherentImpl.code())
        .with_message("associated types are not allowed in inherent impl blocks")
        .with_label(span.primary_label())
        .with_note("associated types can only be defined in `impl Trait for Type` blocks")
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
        type_pool: vec![
            // Order MUST match the IDX constants defined at the top of this file.
            Type::Error,
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
        function_index_lookup: HashMap::new(),
        global_index_lookup: HashMap::new(),
        memory_index_lookup: HashMap::new(),
        struct_index_lookup: HashMap::new(),
        constants: Vec::new(),
        const_index_lookup: HashMap::new(),
        lang_items: HashMap::new(),
        typesets: Vec::new(),
        typeset_index_lookup: HashMap::new(),
    };
    let type_index_lookup = HashMap::from_iter(
        tir.type_pool
            .iter()
            .enumerate()
            .map(|(idx, ty)| (ty.clone(), TypeIndex(idx as u32))),
    );
    let mut symbol_lookup = HashMap::new();
    symbol_lookup.insert(
        (SymbolNamespace::Value, graph.interner.get_or_intern("_")),
        SymbolKind::Placeholder,
    );
    symbol_lookup.insert(
        (SymbolNamespace::Value, graph.interner.get_or_intern("true")),
        SymbolKind::True,
    );
    symbol_lookup.insert(
        (
            SymbolNamespace::Value,
            graph.interner.get_or_intern("false"),
        ),
        SymbolKind::False,
    );
    symbol_lookup.insert(
        (
            SymbolNamespace::Value,
            graph.interner.get_or_intern("unreachable"),
        ),
        SymbolKind::Unreachable,
    );
    let mut builder = Builder {
        symbol_lookup,
        interner: &mut graph.interner,
        tir,
        trait_impl_block_lookup: HashMap::new(),
        type_index_lookup,
        sig_state: HashMap::new(),
        ast_nodes: Vec::new(),
        id_generator: graph.id_generator,
    };

    // Phase 1: register all top-level items into ast_nodes / pending
    for (crate_graph, source_module) in source_modules.iter().copied() {
        let module_path = crate_graph.module_symbol_path(source_module.id);
        let namespace = builder.ensure_module_path(source_module.file_id, &module_path);
        for item in source_module.ast.items.iter() {
            builder.pre_scan_item(source_module.file_id, namespace, &item.inner.inner);
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

    builder.tir
}

impl<'ast> Builder<'ast, '_> {
    fn record_type_param_access(
        &mut self,
        owner: TypeParamOwner,
        param_index: u32,
        span: SourceSpan,
    ) {
        match owner {
            TypeParamOwner::Function(def_id) => {
                if let Some(&fi) = self.tir.function_index_lookup.get(&def_id) {
                    self.tir.functions[fi as usize].type_params[param_index as usize]
                        .accesses
                        .push(span);
                }
            }
            TypeParamOwner::Struct(def_id) => {
                if let Some(&si) = self.tir.struct_index_lookup.get(&def_id) {
                    self.tir.structs[si as usize].type_params[param_index as usize]
                        .accesses
                        .push(span);
                }
            }
            TypeParamOwner::Trait(_) => {}
        }
    }

    fn intern_type(&mut self, ty: Type) -> TypeIndex {
        if let Some(&idx) = self.type_index_lookup.get(&ty) {
            idx
        } else {
            let idx = TypeIndex(self.tir.type_pool.len() as u32);
            self.tir.type_pool.push(ty.clone());
            self.type_index_lookup.insert(ty, idx);
            idx
        }
    }

    pub fn coercible_to(&mut self, a: TypeIndex, b: TypeIndex) -> bool {
        if a == b || a == TypeIndex::NEVER || a == TypeIndex::ERROR || b == TypeIndex::ERROR {
            return true;
        }
        // *mut T coerces implicitly to *T when memory and pointee match.
        if let (
            &Type::Pointer {
                to: a_to,
                mutable: true,
                memory: a_mem,
            },
            &Type::Pointer {
                to: b_to,
                mutable: false,
                memory: b_mem,
            },
        ) = (
            &self.tir.type_pool[a.as_usize()],
            &self.tir.type_pool[b.as_usize()],
        ) {
            if a_to == b_to && a_mem == b_mem {
                return true;
            }
        }
        // FunctionItem coerces implicitly to its matching Function type.
        let (id, type_args) = match &self.tir.type_pool[a.as_usize()] {
            Type::FunctionItem { id, type_args } => (*id, type_args.clone()),
            _ => return false,
        };
        if !matches!(self.tir.type_pool[b.as_usize()], Type::Function { .. }) {
            return false;
        }
        let fi = self.tir.function_index_lookup[&id] as usize;
        let generic_sig = self.tir.functions[fi].signature_index;
        self.substitute_type(generic_sig, &type_args) == b
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
        ) = (
            &self.tir.type_pool[a.as_usize()],
            &self.tir.type_pool[b.as_usize()],
        ) {
            let a_args = a_args.clone();
            let b_args = b_args.clone();
            let a_sig =
                self.tir.functions[self.tir.function_index_lookup[&a_id] as usize].signature_index;
            let b_sig =
                self.tir.functions[self.tir.function_index_lookup[&b_id] as usize].signature_index;
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
                params_count: params.len(),
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
            .lookup_symbol(namespace, (SymbolNamespace::Type, symbol))
            .cloned()
        {
            let decl = &mut self.tir.module_decls[namespace_idx as usize];
            if decl.pub_span.is_none() {
                decl.pub_span = pub_span;
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
        });
        self.tir.module_decls.push(ModuleDecl {
            namespace_idx,
            declaring_file_id: file_id,
            own_file_id: None,
            name,
            pub_span,
            accesses: Vec::new(),
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
        path: &[SymbolU32],
    ) -> Option<NamespaceIndex> {
        let mut namespace: Option<NamespaceIndex> = None;

        for (i, &segment) in path.iter().enumerate() {
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
                self.tir.module_decls[namespace_idx as usize].own_file_id = Some(file_id);
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
        match &self.tir.type_pool[ty.as_usize()] {
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
                        self.find_direct_struct_recursion(field_type, root_struct_index, visited)
                    });
                visited.pop();
                found
            }
            Type::Tuple { elements } => elements.iter().copied().any(|element| {
                self.find_direct_struct_recursion(element, root_struct_index, visited)
            }),
            // Pointer and slice are indirection — stop here.
            Type::Pointer { .. } | Type::Slice { .. } | Type::Array { .. } => false,
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
        for (field_ty, field_span) in
            self.tir.structs[struct_index as usize]
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
                })
        {
            if self.find_direct_struct_recursion(field_ty, struct_index, &mut visited) {
                let name = self
                    .interner
                    .resolve(self.tir.structs[struct_index as usize].name.inner)
                    .unwrap();
                self.tir
                    .diagnostics
                    .push(report_recursive_type(name, struct_span, field_span));
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

    fn lookup_symbol(
        &self,
        namespace: Option<NamespaceIndex>,
        key: (SymbolNamespace, SymbolU32),
    ) -> Option<&SymbolKind> {
        let mut current = namespace;
        while let Some(idx) = current {
            if let Some(kind) = self.tir.namespaces[idx as usize].symbols.get(&key) {
                return Some(kind);
            }
            current = self.tir.namespaces[idx as usize].parent;
        }
        self.symbol_lookup.get(&key)
    }

    fn symbol_kind_to_type(&mut self, kind: SymbolKind) -> Option<TypeIndex> {
        match kind {
            SymbolKind::Memory { kind, memory_index } => {
                let id = self.tir.memories[memory_index as usize].id;
                Some(self.intern_type(Type::Memory { kind, id }))
            }
            SymbolKind::Trait { trait_index } => {
                Some(self.intern_type(Type::Trait { trait_index }))
            }
            SymbolKind::TypeSet { typeset_index } => {
                Some(self.intern_type(Type::TypeSet { typeset_index }))
            }
            SymbolKind::Module { namespace_idx } => {
                Some(self.intern_type(Type::Module { namespace_idx }))
            }
            SymbolKind::Enum { enum_index } => Some(self.intern_type(Type::Enum { enum_index })),
            SymbolKind::Struct { struct_index } => Some(self.intern_type(Type::Struct {
                struct_index,
                args: Box::new([]),
            })),
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
            SymbolKind::TraitAssocType { .. } | SymbolKind::Pending(_) => None,
            SymbolKind::False
            | SymbolKind::True
            | SymbolKind::Unreachable
            | SymbolKind::Placeholder => unreachable!(),
        }
    }

    /// Resolve a bare identifier symbol to a `TypeIndex`.
    /// Extracted from the `TypeExpression::Identifier` arm so it can be called
    /// directly from path-walking code without constructing AST nodes.
    fn resolve_type_identifier(
        &mut self,
        resolve_context: &ResolveContext,
        symbol: SymbolU32,
        span: TextSpan,
    ) -> TypeIndex {
        // `Self` — the concrete type of the enclosing impl or trait block.
        if self.interner.resolve(symbol) == Some("Self") {
            return match resolve_context.self_type {
                Some(ty) => {
                    if let Type::TypeParam { owner, param_index } =
                        self.tir.type_pool[ty.as_usize()].clone()
                    {
                        self.record_type_param_access(
                            owner,
                            param_index,
                            SourceSpan::new(resolve_context.file_id, span),
                        );
                    }
                    ty
                }
                None => {
                    self.tir.diagnostics.push(
                        Diagnostic::error()
                            .with_code(DiagnosticCode::UndeclaredType.code())
                            .with_message("`Self` is only valid inside an impl or trait block")
                            .with_label(Label::primary(resolve_context.file_id, span)),
                    );
                    TypeIndex::ERROR
                }
            };
        }
        if let Ok(ty) = Type::try_from(self.interner.resolve(symbol).unwrap()) {
            return self.intern_type(ty);
        }
        if let Some(scope) = &resolve_context.type_param_scope {
            for (param_index, type_param) in scope.params.iter().enumerate() {
                if type_param.name == symbol {
                    let owner = scope.owner.clone();
                    let param_index = param_index as u32;
                    self.record_type_param_access(
                        owner.clone(),
                        param_index,
                        SourceSpan::new(resolve_context.file_id, span),
                    );
                    return self.intern_type(Type::TypeParam { owner, param_index });
                }
            }
        }
        match self
            .lookup_symbol(resolve_context.namespace, (SymbolNamespace::Type, symbol))
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
                        .push(report_cyclic_type_dependency(SourceSpan::new(
                            resolve_context.file_id,
                            span,
                        )));
                    return TypeIndex::ERROR;
                }
                self.ensure_signature(def_id);
                match self
                    .lookup_symbol(resolve_context.namespace, (SymbolNamespace::Type, symbol))
                    .cloned()
                {
                    Some(SymbolKind::TraitAssocType { assoc_name, .. }) => {
                        let name = self.interner.resolve(assoc_name).unwrap();
                        self.tir.diagnostics.push(report_bare_assoc_type(
                            SourceSpan::new(resolve_context.file_id, span),
                            name,
                        ));
                        TypeIndex::ERROR
                    }
                    Some(kind) => {
                        if let Some(ty) = self.symbol_kind_to_type(kind) {
                            match self.tir.type_pool[ty.as_usize()] {
                                Type::Struct { struct_index, .. } => {
                                    self.tir.structs[struct_index as usize]
                                        .accesses
                                        .push(SourceSpan::new(resolve_context.file_id, span));
                                }
                                Type::Trait { trait_index } => {
                                    self.tir.traits[trait_index as usize]
                                        .accesses
                                        .push(SourceSpan::new(resolve_context.file_id, span));
                                }
                                _ => {}
                            }
                            return ty;
                        }
                        TypeIndex::ERROR
                    }
                    None => TypeIndex::ERROR,
                }
            }
            Some(SymbolKind::TraitAssocType { assoc_name, .. }) => {
                let name = self.interner.resolve(assoc_name).unwrap();
                self.tir.diagnostics.push(report_bare_assoc_type(
                    SourceSpan::new(resolve_context.file_id, span),
                    name,
                ));
                TypeIndex::ERROR
            }
            Some(kind) => {
                if let Some(ty) = self.symbol_kind_to_type(kind) {
                    match self.tir.type_pool[ty.as_usize()] {
                        Type::Struct { struct_index, .. } => {
                            self.tir.structs[struct_index as usize]
                                .accesses
                                .push(SourceSpan::new(resolve_context.file_id, span));
                        }
                        Type::Trait { trait_index } => {
                            self.tir.traits[trait_index as usize]
                                .accesses
                                .push(SourceSpan::new(resolve_context.file_id, span));
                        }
                        _ => {}
                    }
                    return ty;
                }
                self.tir
                    .diagnostics
                    .push(report_undeclared_type(SourceSpan::new(
                        resolve_context.file_id,
                        span,
                    )));
                TypeIndex::ERROR
            }
            None => {
                self.tir
                    .diagnostics
                    .push(report_undeclared_type(SourceSpan::new(
                        resolve_context.file_id,
                        span,
                    )));
                TypeIndex::ERROR
            }
        }
    }

    pub fn resolve_type(
        &mut self,
        resolve_context: &ResolveContext,
        type_expr: &Spanned<ast::TypeExpression>,
    ) -> TypeIndex {
        match &type_expr.inner {
            ast::TypeExpression::Path(path) => {
                let last = path.segments.last().expect("path is non-empty");

                // ── single segment, no type args: plain identifier ─────────────
                if path.segments.len() == 1 && last.type_args.is_empty() {
                    return self.resolve_type_identifier(
                        resolve_context,
                        last.ident.inner,
                        last.ident.span,
                    );
                }

                // ── single segment with turbofish args: `Wrapper::<T>` ─────────
                if path.segments.len() == 1 {
                    let base_ty = self.resolve_type_identifier(
                        resolve_context,
                        last.ident.inner,
                        last.ident.span,
                    );
                    if base_ty == TypeIndex::ERROR {
                        return TypeIndex::ERROR;
                    }
                    let struct_index = match &self.tir.type_pool[base_ty.as_usize()] {
                        Type::Struct { struct_index, .. } => *struct_index,
                        _ => {
                            self.tir.diagnostics.push(
                                Diagnostic::error()
                                    .with_message("type arguments are not supported here")
                                    .with_label(Label::primary(
                                        resolve_context.file_id,
                                        type_expr.span,
                                    )),
                            );
                            return TypeIndex::ERROR;
                        }
                    };
                    let type_params_len = self.tir.structs[struct_index as usize].type_params.len();
                    let resolved_args: Box<[TypeIndex]> = last
                        .type_args
                        .iter()
                        .map(|arg| self.resolve_type(resolve_context, arg))
                        .collect();
                    if resolved_args.len() != type_params_len {
                        let struct_name = self
                            .interner
                            .resolve(last.ident.inner)
                            .unwrap_or("?")
                            .to_string();
                        self.tir.diagnostics.push(
                            Diagnostic::error()
                                .with_code(DiagnosticCode::TypeArgCountMismatch.code())
                                .with_message(format!(
                                    "`{}` expects {} type argument{}, found {}",
                                    struct_name,
                                    type_params_len,
                                    if type_params_len == 1 { "" } else { "s" },
                                    resolved_args.len(),
                                ))
                                .with_label(Label::primary(
                                    resolve_context.file_id,
                                    type_expr.span,
                                )),
                        );
                        return TypeIndex::ERROR;
                    }
                    return self.intern_type(Type::Struct {
                        struct_index,
                        args: resolved_args,
                    });
                }

                // ── multi-segment: walk namespace chain ────────────────────────
                let first = &path.segments[0];
                let mut namespace_ty = self.resolve_type_identifier(
                    resolve_context,
                    first.ident.inner,
                    first.ident.span,
                );
                if namespace_ty == TypeIndex::ERROR {
                    return TypeIndex::ERROR;
                }

                for seg in &path.segments[1..path.segments.len() - 1] {
                    namespace_ty = self.resolve_namespace_type_member(
                        resolve_context,
                        namespace_ty,
                        seg.ident.inner,
                        seg.ident.span,
                    );
                    if namespace_ty == TypeIndex::ERROR {
                        return TypeIndex::ERROR;
                    }
                }

                // Resolve the last segment within the current namespace.
                let last_ty = self.resolve_namespace_type_member(
                    resolve_context,
                    namespace_ty,
                    last.ident.inner,
                    last.ident.span,
                );
                if last_ty == TypeIndex::ERROR {
                    return TypeIndex::ERROR;
                }

                // Apply turbofish type args on the last segment if present.
                if last.type_args.is_empty() {
                    return last_ty;
                }
                let struct_index = match &self.tir.type_pool[last_ty.as_usize()] {
                    Type::Struct { struct_index, .. } => *struct_index,
                    _ => {
                        self.tir.diagnostics.push(
                            Diagnostic::error()
                                .with_message("type arguments are not supported here")
                                .with_label(Label::primary(
                                    resolve_context.file_id,
                                    type_expr.span,
                                )),
                        );
                        return TypeIndex::ERROR;
                    }
                };
                let type_params_len = self.tir.structs[struct_index as usize].type_params.len();
                let resolved_args: Box<[TypeIndex]> = last
                    .type_args
                    .iter()
                    .map(|arg| self.resolve_type(resolve_context, arg))
                    .collect();
                if resolved_args.len() != type_params_len {
                    let struct_name = self
                        .interner
                        .resolve(last.ident.inner)
                        .unwrap_or("?")
                        .to_string();
                    self.tir.diagnostics.push(
                        Diagnostic::error()
                            .with_code(DiagnosticCode::TypeArgCountMismatch.code())
                            .with_message(format!(
                                "`{}` expects {} type argument{}, found {}",
                                struct_name,
                                type_params_len,
                                if type_params_len == 1 { "" } else { "s" },
                                resolved_args.len(),
                            ))
                            .with_label(Label::primary(resolve_context.file_id, type_expr.span)),
                    );
                    return TypeIndex::ERROR;
                }
                self.intern_type(Type::Struct {
                    struct_index,
                    args: resolved_args,
                })
            }
            ast::TypeExpression::Function { params, result } => {
                let result_idx = match result {
                    Some(result) => self.resolve_type(resolve_context, result),
                    None => TypeIndex::UNIT,
                };

                // TODO: use intern_function?
                let params_count = params.len();
                let items: Box<[TypeIndex]> = params
                    .iter()
                    .map(|ty| self.resolve_type(resolve_context, &ty.inner.inner.ty))
                    .chain(Some(result_idx))
                    .collect();
                self.intern_type(Type::Function {
                    signature: FunctionSignature {
                        params_count,
                        items,
                    },
                })
            }
            ast::TypeExpression::Pointer { mutability, inner } => {
                let to = self.resolve_type(resolve_context, inner);
                let span = SourceSpan::new(resolve_context.file_id, type_expr.span);
                let Ok(memory) = self.resolve_ambient_memory(span) else {
                    return TypeIndex::ERROR;
                };
                self.intern_type(Type::Pointer {
                    to,
                    mutable: mutability.is_some(),
                    memory,
                })
            }
            ast::TypeExpression::Slice { mutability, inner } => {
                let of = self.resolve_type(resolve_context, inner);
                let span = SourceSpan::new(resolve_context.file_id, type_expr.span);
                let Ok(memory) = self.resolve_ambient_memory(span) else {
                    return TypeIndex::ERROR;
                };
                self.intern_type(Type::Slice {
                    of,
                    mutable: mutability.is_some(),
                    memory,
                })
            }
            ast::TypeExpression::Array {
                size,
                mutability,
                inner,
            } => {
                let of = self.resolve_type(resolve_context, inner);
                let span = SourceSpan::new(resolve_context.file_id, type_expr.span);
                let Ok(memory) = self.resolve_ambient_memory(span) else {
                    return TypeIndex::ERROR;
                };
                self.intern_type(Type::Array {
                    of,
                    size: size.inner as u32,
                    mutable: mutability.is_some(),
                    memory,
                })
            }
            ast::TypeExpression::Tuple { elements } => {
                if elements.is_empty() {
                    return TypeIndex::UNIT;
                }
                let elems: Box<[TypeIndex]> = elements
                    .iter()
                    .map(|e| self.resolve_type(resolve_context, e))
                    .collect();
                self.intern_type(Type::Tuple { elements: elems })
            }
            ast::TypeExpression::MemoryTagged { memory, inner } => {
                let memory_ty = self.resolve_type_identifier(
                    resolve_context,
                    memory.segments[0].ident.inner,
                    memory.span,
                );
                if memory_ty == TypeIndex::ERROR {
                    return TypeIndex::ERROR;
                }
                match &self.tir.type_pool[memory_ty.as_usize()] {
                    Type::Memory { .. }
                    | Type::TypeParam { .. }
                    | Type::AssocTypeProjection { .. } => {}
                    _ => {
                        self.tir.diagnostics.push(
                            Diagnostic::error()
                                .with_message(format!(
                                    "`{}` is not a memory declaration",
                                    TypeFormatter::new(&self.tir, &self.interner)
                                        .display_type(memory_ty)
                                        .unwrap()
                                ))
                                .with_label(Label::primary(resolve_context.file_id, memory.span)),
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
                        let to = self.resolve_type(resolve_context, ptr_inner);
                        self.intern_type(Type::Pointer {
                            to,
                            mutable: mutability.is_some(),
                            memory: memory_ty,
                        })
                    }
                    ast::TypeExpression::Array {
                        size,
                        mutability,
                        inner: arr_inner,
                    } => {
                        let of = self.resolve_type(resolve_context, arr_inner);
                        self.intern_type(Type::Array {
                            of,
                            size: size.inner as u32,
                            mutable: mutability.is_some(),
                            memory: memory_ty,
                        })
                    }
                    ast::TypeExpression::Slice {
                        mutability,
                        inner: sl_inner,
                    } => {
                        let of = self.resolve_type(resolve_context, sl_inner);
                        self.intern_type(Type::Slice {
                            of,
                            mutable: mutability.is_some(),
                            memory: memory_ty,
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
                match self
                    .lookup_symbol(
                        resolve_context.namespace,
                        (SymbolNamespace::Type, name.inner),
                    )
                    .copied()
                {
                    Some(SymbolKind::Pending(def_id)) => {
                        self.ensure_signature(def_id);
                    }
                    _ => {}
                }
                match self
                    .lookup_symbol(
                        resolve_context.namespace,
                        (SymbolNamespace::Type, name.inner),
                    )
                    .copied()
                {
                    Some(SymbolKind::Struct { struct_index }) => {
                        let type_params_len =
                            self.tir.structs[struct_index as usize].type_params.len();
                        let positional_args: Box<[TypeIndex]> = args
                            .iter()
                            .filter_map(|sep| match &sep.inner.inner {
                                ast::GenericArg::Type(ty) => {
                                    Some(self.resolve_type(resolve_context, ty))
                                }
                                ast::GenericArg::Binding { ty, .. } => {
                                    // Binding args are trait-specific; report error for structs.
                                    self.tir.diagnostics.push(
                                        Diagnostic::error()
                                            .with_code(DiagnosticCode::TypeArgCountMismatch.code())
                                            .with_message(
                                                "associated-type bindings are not valid in struct type arguments",
                                            )
                                            .with_label(Label::primary(
                                                resolve_context.file_id,
                                                ty.span,
                                            )),
                                    );
                                    None
                                }
                            })
                            .collect();
                        if positional_args.len() != type_params_len {
                            let struct_name =
                                self.interner.resolve(name.inner).unwrap_or("?").to_string();
                            self.tir.diagnostics.push(
                                Diagnostic::error()
                                    .with_code(DiagnosticCode::TypeArgCountMismatch.code())
                                    .with_message(format!(
                                        "`{}` expects {} type argument{}, found {}",
                                        struct_name,
                                        type_params_len,
                                        if type_params_len == 1 { "" } else { "s" },
                                        positional_args.len(),
                                    ))
                                    .with_label(Label::primary(
                                        resolve_context.file_id,
                                        type_expr.span,
                                    )),
                            );
                            return TypeIndex::ERROR;
                        }
                        self.tir.structs[struct_index as usize]
                            .accesses
                            .push(SourceSpan::new(resolve_context.file_id, name.span));
                        self.intern_type(Type::Struct {
                            struct_index,
                            args: positional_args,
                        })
                    }
                    _ => {
                        // Not a struct — resolve as bare identifier and surface any errors.
                        // Eagerly resolve arg types to surface undeclared-type errors.
                        for sep in args.iter() {
                            match &sep.inner.inner {
                                ast::GenericArg::Type(ty) => {
                                    self.resolve_type(resolve_context, ty);
                                }
                                ast::GenericArg::Binding { ty, .. } => {
                                    self.resolve_type(resolve_context, ty);
                                }
                            }
                        }
                        let base = Spanned {
                            inner: ast::TypeExpression::Path(ast::Path {
                                segments: Box::new([ast::PathSegment {
                                    ident: name.clone(),
                                    type_args: Box::new([]),
                                }]),
                                span: name.span,
                            }),
                            span: name.span,
                        };
                        self.resolve_type(resolve_context, &base)
                    }
                }
            }
        }
    }

    fn register_lang_items(&mut self, def_id: ast::DefId, attrs: &[ItemAttribute]) {
        for attr in attrs {
            if let ItemAttribute::Lang(key) = attr {
                self.tir.lang_items.insert(*key, def_id);
            }
        }
    }

    fn resolve_attributes(&mut self, attrs: &[ast::Attribute]) -> Box<[ItemAttribute]> {
        attrs
            .iter()
            .filter_map(|a| match (&a.value, self.interner.resolve(a.name.inner)) {
                (ast::AttributeValue::Word, Some("inline")) => Some(ItemAttribute::Inline),
                (ast::AttributeValue::NameValue(value), Some("lang")) => {
                    let raw = self.interner.resolve(value.inner).unwrap_or("");
                    let key = self.interner.get_or_intern(&unescape_string(raw));
                    Some(ItemAttribute::Lang(key))
                }
                _ => None,
            })
            .collect()
    }

    fn type_param_bounds_by_owner<'a>(
        &'a self,
        owner: TypeParamOwner,
        param_index: u32,
    ) -> &'a [TraitIndex] {
        match owner {
            TypeParamOwner::Function(def_id) => self
                .tir
                .function_index_lookup
                .get(&def_id)
                .and_then(|&func_index| {
                    self.tir.functions[func_index as usize]
                        .type_params
                        .get(param_index as usize)
                })
                .map(|tp| tp.bounds.as_ref())
                .unwrap_or(&[]),
            TypeParamOwner::Struct(def_id) => self
                .tir
                .struct_index_lookup
                .get(&def_id)
                .and_then(|&struct_index| {
                    self.tir.structs[struct_index as usize]
                        .type_params
                        .get(param_index as usize)
                })
                .map(|tp| tp.bounds.as_ref())
                .unwrap_or(&[]),
            // Self in a trait item is bounded by the trait itself.
            TypeParamOwner::Trait(_) => &[],
        }
    }

    fn type_param_bounds_in_context<'a>(
        &'a self,
        resolve_context: &'a ResolveContext,
        ty: TypeIndex,
    ) -> &'a [TraitIndex] {
        match &self.tir.type_pool[ty.as_usize()] {
            Type::TypeParam { owner, param_index } => {
                match &resolve_context.type_param_scope {
                    Some(scope) if scope.owner == *owner => {
                        return scope.params[*param_index as usize].bounds.as_ref();
                    }
                    // TODO: check if this is correct
                    _ => self.type_param_bounds_by_owner(*owner, *param_index),
                }
            }
            _ => unreachable!(),
        }
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
                let decl = &self.tir.module_decls[namespace_idx as usize];
                SourceSpan::new(decl.declaring_file_id, decl.name.span)
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
            // these are keywords and will be handled at the parser level
            SymbolKind::False
            | SymbolKind::True
            | SymbolKind::Unreachable
            | SymbolKind::Placeholder
            | SymbolKind::Pending(_) => unreachable!(),
        }
    }

    // ── pre-scan ──────────────────────────────────────────────────────────────

    /// Phase 1: registers every named item into `ast_nodes` without resolving
    /// types.
    fn pre_scan_item(
        &mut self,
        file_id: FileId,
        namespace: Option<NamespaceIndex>,
        item: &'ast ast::Item,
    ) {
        match item {
            ast::Item::IntrinsicFunction { id, signature } => {
                self.insert_symbol(
                    namespace,
                    (SymbolNamespace::Value, signature.name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.push(AstEntry {
                    def_id: *id,
                    file_id,
                    namespace,
                    node: AstNodeRef::IntrinsicFunction { item },
                });
            }
            ast::Item::Function { id, signature, .. }
            | ast::Item::FunctionDeclaration { id, signature, .. } => {
                self.insert_symbol(
                    namespace,
                    (SymbolNamespace::Value, signature.name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.push(AstEntry {
                    def_id: *id,
                    file_id,
                    namespace,
                    node: AstNodeRef::Function { item },
                });
            }
            ast::Item::Global { id, name, .. } => {
                self.insert_symbol(
                    namespace,
                    (SymbolNamespace::Value, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.push(AstEntry {
                    def_id: *id,
                    file_id,
                    namespace,
                    node: AstNodeRef::Global { item },
                });
            }
            ast::Item::Struct { id, name, .. } => {
                self.insert_symbol(
                    namespace,
                    (SymbolNamespace::Type, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.push(AstEntry {
                    def_id: *id,
                    file_id,
                    namespace,
                    node: AstNodeRef::Struct { item },
                });
            }
            ast::Item::Enum { id, name, .. } => {
                self.insert_symbol(
                    namespace,
                    (SymbolNamespace::Type, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.push(AstEntry {
                    def_id: *id,
                    file_id,
                    namespace,
                    node: AstNodeRef::Enum { item },
                });
            }
            ast::Item::Memory { id, name, .. } => {
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
            ast::Item::Const { id, name, .. } => {
                self.insert_symbol(
                    namespace,
                    (SymbolNamespace::Value, name.inner),
                    SymbolKind::Pending(*id),
                );
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
                    self.ensure_module(file_id, namespace, name.clone(), *pub_span);
                for child in items.iter() {
                    self.pre_scan_item(file_id, Some(namespace_index), &child.inner.inner);
                }
            }
            ast::Item::ModuleDeclaration { name, pub_span } => {
                self.ensure_module(file_id, namespace, name.clone(), *pub_span);
            }
            ast::Item::Trait {
                id, name, items, ..
            } => {
                // Traits are structural containers; allocate the slot and register
                // each item's DefId so ensure_signature can fill it in on demand.
                let trait_index = self.tir.traits.len() as u32;
                self.tir.traits.push(Trait {
                    file_id,
                    namespace,
                    name: name.clone(),
                    supertraits: Vec::new(),
                    members: HashMap::new(),
                    assoc_types: HashMap::new(),
                    member_def_ids: Vec::new(),
                    supertrait_bindings: HashMap::new(),
                    accesses: Vec::new(),
                });
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
                let mut ids: Vec<ast::DefId> = Vec::new();
                for ti in items.iter() {
                    match &ti.inner.inner {
                        ast::TraitItem::Function { id, signature, .. } => {
                            self.insert_symbol(
                                namespace,
                                (SymbolNamespace::Value, signature.name.inner),
                                SymbolKind::Pending(*id),
                            );
                            self.ast_nodes.push(AstEntry {
                                def_id: *id,
                                file_id,
                                namespace,
                                node: AstNodeRef::TraitFunction {
                                    trait_index,
                                    item: &ti.inner.inner,
                                },
                            });
                            ids.push(*id);
                        }
                        ast::TraitItem::Const { id, name, .. } => {
                            self.insert_symbol(
                                namespace,
                                (SymbolNamespace::Value, name.inner),
                                SymbolKind::Pending(*id),
                            );
                            self.ast_nodes.push(AstEntry {
                                def_id: *id,
                                file_id,
                                namespace,
                                node: AstNodeRef::TraitConst {
                                    trait_index,
                                    item: &ti.inner.inner,
                                },
                            });
                            ids.push(*id);
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
                                    item: &ti.inner.inner,
                                },
                            });
                            ids.push(*id);
                        }
                    }
                }
                self.tir.traits[trait_index as usize].member_def_ids = ids;
            }
            ast::Item::Impl {
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
                                self.tir
                                    .diagnostics
                                    .push(report_associated_type_in_inherent_impl(
                                        SourceSpan::new(file_id, name.span),
                                    ));
                            }
                        }
                    }
                } else {
                    // Generic impl — allocate a placeholder block, then register each method.
                    let block_index = self.tir.generic_impl_list.len();
                    self.tir.generic_impl_list.push(GenericImplBlock {
                        type_params: Box::new([]),
                        target: TypeIndex::ERROR,
                        members: HashMap::new(),
                    });
                    for impl_item in items.iter() {
                        match &impl_item.inner.inner {
                            ast::ImplItem::Method { id, .. } => {
                                self.ast_nodes.push(AstEntry {
                                    def_id: *id,
                                    file_id,
                                    namespace,
                                    node: AstNodeRef::GenericImplMethod {
                                        impl_type_params: type_params,
                                        impl_target: target,
                                        item: &impl_item.inner.inner,
                                        block_index,
                                    },
                                });
                            }
                            ast::ImplItem::Const { .. } | ast::ImplItem::AssociatedType { .. } => {
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
                    let s = self.interner.resolve(import_module_name.inner).unwrap();
                    let unquoted = unescape_string(s);
                    Spanned {
                        inner: self.interner.get_or_intern(&unquoted),
                        span: import_module_name.span,
                    }
                };
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
                    internal_name: alias.clone(),
                    lookup: HashMap::new(),
                    accesses: Vec::new(),
                });
            }
            ast::Item::Export { .. } => {} // handled during build pass
            ast::Item::TypeSet {
                id, name, pub_span, ..
            } => {
                let typeset_index = self.tir.typesets.len() as TypesetIndex;
                self.tir.typesets.push(TypeSet {
                    id: *id,
                    file_id,
                    name: name.clone(),
                    pub_span: *pub_span,
                    members: Box::new([]),
                    intersection_range: IntegerRange::widest(),
                });
                self.tir.typeset_index_lookup.insert(*id, typeset_index);
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
                        ast::ImplItem::AssociatedType { id: type_id, .. } => {
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

    // ── query: ensure_signature ───────────────────────────────────────────────

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
            // ── struct ────────────────────────────────────────────────────────
            AstNodeRef::Struct { item } => {
                let (id, pub_span, name, ast_type_params, fields) = match item {
                    ast::Item::Struct {
                        id,
                        pub_span,
                        name,
                        type_params,
                        fields,
                    } => (id, pub_span, name, type_params, fields),
                    _ => unreachable!(),
                };
                // Duplicate check.
                if let Some(existing) = self
                    .lookup_symbol(
                        resolve_context.namespace,
                        (SymbolNamespace::Type, name.inner),
                    )
                    .filter(|k| !matches!(k, SymbolKind::Pending(_)))
                    .cloned()
                {
                    let first_definition = match existing {
                        SymbolKind::Struct { struct_index } => {
                            let struct_ = &self.tir.structs[struct_index as usize];
                            SourceSpan::new(struct_.file_id, struct_.name.span)
                        }
                        _ => SourceSpan::new(resolve_context.file_id, name.span),
                    };
                    let name_str = self.interner.resolve(name.inner).unwrap();
                    self.tir.diagnostics.push(report_duplicate_definition(
                        DuplicateDefinitionDiagnostic {
                            name: name_str,
                            namespace: SymbolNamespace::Type,
                            first_definition,
                            second_definition: SourceSpan::new(resolve_context.file_id, name.span),
                        },
                    ));
                    self.sig_state.get_mut(&def_id).unwrap().state = ComputeState::Done;
                    return;
                }

                // Resolve type params, then build a resolve context that lets field
                // types reference those params by name.
                let type_params = self.resolve_ast_type_params(&resolve_context, &ast_type_params);
                let field_resolve_context = if type_params.is_empty() {
                    resolve_context.clone()
                } else {
                    resolve_context.with_type_param_scope(TypeParamScope {
                        owner: TypeParamOwner::Struct(*id),
                        params: type_params.clone(),
                    })
                };

                // Pre-register the struct with a placeholder before resolving its
                // fields. This allows field types to reference the struct itself
                // through indirection (e.g. `*Node`) without triggering a false
                // cycle error. Direct self-reference is caught by
                // `check_struct_fields_for_direct_recursion` below.
                let struct_index = self.tir.structs.len() as u32;
                self.tir.struct_index_lookup.insert(*id, struct_index);
                self.tir.structs.push(Struct {
                    id: *id,
                    file_id: resolve_context.file_id,
                    namespace: resolve_context.namespace,
                    pub_span: *pub_span,
                    name: name.clone(),
                    type_params,
                    fields: Box::new([]),
                    lookup: HashMap::new(),
                    accesses: Vec::new(),
                });
                self.insert_symbol(
                    resolve_context.namespace,
                    (SymbolNamespace::Type, name.inner),
                    SymbolKind::Struct { struct_index },
                );

                // Resolve all field types. Referenced structs that haven't been
                // seen yet are pulled in demand-driven via ensure_signature.
                let field_count = fields.len();
                let mut seen_fields: HashMap<SymbolU32, ast::TextSpan> =
                    HashMap::with_capacity(field_count);
                let mut tir_fields: Vec<StructField> = Vec::with_capacity(field_count);
                let mut field_lookup: HashMap<SymbolU32, usize> =
                    HashMap::with_capacity(field_count);

                for f in fields.iter() {
                    let field = &f.inner.inner;
                    let sym = field.name.inner;
                    if let Some(&first_span) = seen_fields.get(&sym) {
                        let fname = self.interner.resolve(sym).unwrap().to_string();
                        self.tir.diagnostics.push(report_duplicate_struct_field(
                            &fname,
                            SourceSpan::new(resolve_context.file_id, first_span),
                            SourceSpan::new(resolve_context.file_id, field.name.span),
                        ));
                        continue;
                    }
                    let field_ty = self.resolve_type(&field_resolve_context, &field.ty);
                    seen_fields.insert(sym, field.name.span);
                    let idx = tir_fields.len();
                    field_lookup.insert(sym, idx);
                    tir_fields.push(StructField {
                        name: field.name.clone(),
                        ty: Spanned {
                            inner: field_ty,
                            span: field.ty.span,
                        },
                    });
                }

                // Fill in the placeholder now that all field types are resolved.
                self.tir.structs[struct_index as usize].fields = tir_fields.into_boxed_slice();
                self.tir.structs[struct_index as usize].lookup = field_lookup;

                // Check for direct (non-pointer) self-recursion. Cycles through
                // generic struct instantiation are not caught here — see TODO in
                // mir::Builder::ensure_aggregate_for_struct.
                self.check_struct_fields_for_direct_recursion(
                    struct_index,
                    SourceSpan::new(resolve_context.file_id, name.span),
                );

                let _ = pub_span;
            }

            // ── enum ──────────────────────────────────────────────────────────
            AstNodeRef::Enum { item } => {
                if let ast::Item::Enum {
                    id,
                    pub_span,
                    name,
                    repr,
                    variants,
                } = item
                {
                    if !matches!(
                        self.lookup_symbol(resolve_context.namespace, (SymbolNamespace::Type, name.inner)),
                        Some(k) if !matches!(k, SymbolKind::Pending(_))
                    ) {
                        let enum_index = self.tir.enums.len() as u32;
                        let ty = match repr {
                            Some(r) => self.resolve_type(&resolve_context, &**r),
                            None => {
                                self.tir.diagnostics.push(report_missing_enum_repr(
                                    SourceSpan::new(resolve_context.file_id, name.span),
                                ));
                                TypeIndex::ERROR
                            }
                        };

                        let mut tir_variants: Vec<EnumVariant> = Vec::with_capacity(variants.len());
                        let mut variant_lookup: HashMap<SymbolU32, EnumVariantIndex> =
                            HashMap::with_capacity(variants.len());
                        let mut seen_variants: HashMap<SymbolU32, ast::TextSpan> = HashMap::new();
                        let mut next_auto_value: i64 = 0;

                        for ast_variant in variants.iter() {
                            let v = &ast_variant.inner.inner;
                            let v_sym = v.name.inner;

                            if let Some(&first_span) = seen_variants.get(&v_sym) {
                                let vname = self.interner.resolve(v_sym).unwrap().to_string();
                                self.tir.diagnostics.push(report_duplicate_definition(
                                    DuplicateDefinitionDiagnostic {
                                        name: &vname,
                                        namespace: SymbolNamespace::Value,
                                        first_definition: SourceSpan::new(
                                            resolve_context.file_id,
                                            first_span,
                                        ),
                                        second_definition: SourceSpan::new(
                                            resolve_context.file_id,
                                            v.name.span,
                                        ),
                                    },
                                ));
                                continue;
                            }
                            seen_variants.insert(v_sym, v.name.span);

                            let value_expr = match &v.value {
                                Some(explicit_expr) => {
                                    if ty == TypeIndex::ERROR {
                                        next_auto_value = next_auto_value.wrapping_add(1);
                                        Expression {
                                            kind: ExprKind::Error,
                                            ty: TypeIndex::ERROR,
                                            span: explicit_expr.span,
                                        }
                                    } else {
                                        match self.build_const_expression(
                                            &resolve_context,
                                            explicit_expr,
                                            ty,
                                        ) {
                                            Ok(expr) => {
                                                if let ExprKind::Int { value } = expr.kind {
                                                    next_auto_value = value.wrapping_add(1);
                                                } else {
                                                    next_auto_value =
                                                        next_auto_value.wrapping_add(1);
                                                }
                                                expr
                                            }
                                            Err(_) => {
                                                next_auto_value = next_auto_value.wrapping_add(1);
                                                Expression {
                                                    kind: ExprKind::Error,
                                                    ty: TypeIndex::ERROR,
                                                    span: explicit_expr.span,
                                                }
                                            }
                                        }
                                    }
                                }
                                None => {
                                    let auto_val = next_auto_value;
                                    next_auto_value = next_auto_value.wrapping_add(1);
                                    Expression {
                                        kind: ExprKind::Int { value: auto_val },
                                        ty,
                                        span: v.name.span,
                                    }
                                }
                            };

                            let variant_index = tir_variants.len() as EnumVariantIndex;
                            variant_lookup.insert(v_sym, variant_index);
                            tir_variants.push(EnumVariant {
                                name: v.name.clone(),
                                value: Box::new(value_expr),
                                accesses: Vec::new(),
                            });
                        }

                        self.tir.enums.push(Enum {
                            file_id: resolve_context.file_id,
                            namespace: resolve_context.namespace,
                            pub_span: *pub_span,
                            name: name.clone(),
                            ty,
                            variants: tir_variants.into_boxed_slice(),
                            lookup: variant_lookup,
                        });
                        self.insert_symbol(
                            resolve_context.namespace,
                            (SymbolNamespace::Type, name.inner),
                            SymbolKind::Enum { enum_index },
                        );
                        let _ = id;
                    }
                }
            }

            // ── free function / function declaration ──────────────────────────
            AstNodeRef::Function { item } => match item {
                ast::Item::Function {
                    id,
                    signature,
                    attributes,
                    pub_span,
                    ..
                } => {
                    let existing_span = self
                        .lookup_symbol(
                            resolve_context.namespace,
                            (SymbolNamespace::Value, signature.name.inner),
                        )
                        .filter(|k| !matches!(k, SymbolKind::Pending(_)))
                        .cloned()
                        .map(|k| self.get_symbol_location(k));
                    let type_params =
                        self.resolve_ast_type_params(&resolve_context, &signature.type_params);
                    let func_attrs = self.resolve_attributes(attributes);
                    self.register_lang_items(*id, &func_attrs);
                    let func_index = self.tir.functions.len() as u32;
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: resolve_context.file_id,
                        namespace: resolve_context.namespace,
                        body: None,
                        kind: FunctionKind::Free,
                        type_params,
                        pub_span: *pub_span,
                        signature_index: TypeIndex::ERROR,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params: Box::new([]),
                        result: None,
                        attributes: func_attrs,
                    });
                    self.tir.function_index_lookup.insert(*id, func_index);
                    let signature_context = resolve_context.with_type_param_scope(TypeParamScope {
                        owner: TypeParamOwner::Function(*id),
                        params: self.tir.functions[func_index as usize].type_params.clone(),
                    });
                    let (params, result) =
                        self.build_function_signature(&signature_context, signature);
                    let signature_index = self.intern_function(&params, result.clone());
                    let func = &mut self.tir.functions[func_index as usize];
                    func.params = params;
                    func.result = result;
                    func.signature_index = signature_index;
                    match existing_span {
                        Some(first_definition) => {
                            let name = self.interner.resolve(signature.name.inner).unwrap();
                            self.tir.diagnostics.push(report_duplicate_definition(
                                DuplicateDefinitionDiagnostic {
                                    name,
                                    namespace: SymbolNamespace::Value,
                                    first_definition,
                                    second_definition: SourceSpan::new(
                                        resolve_context.file_id,
                                        signature.name.span,
                                    ),
                                },
                            ));
                        }
                        None => {
                            self.insert_symbol(
                                resolve_context.namespace,
                                (SymbolNamespace::Value, signature.name.inner),
                                SymbolKind::Function { func_index },
                            );
                        }
                    }
                }
                ast::Item::FunctionDeclaration { signature, .. } => {
                    self.tir
                        .diagnostics
                        .push(report_missing_function_body(SourceSpan::new(
                            resolve_context.file_id,
                            signature.name.span,
                        )));
                }
                _ => {}
            },

            // ── impl method ───────────────────────────────────────────────────
            // ── impl const ────────────────────────────────────────────────────
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
                    let self_type = self.resolve_type(&resolve_context, impl_target);
                    let resolve_context = resolve_context.with_self_type(Some(self_type));
                    let resolved_ty = match ty {
                        Some(te) => self.resolve_type(&resolve_context, te),
                        None => TypeIndex::ERROR,
                    };
                    if let Ok(value_expr) =
                        self.build_const_expression(&resolve_context, value, resolved_ty)
                    {
                        let const_index = self.tir.constants.len() as ConstIndex;
                        self.tir.constants.push(Constant {
                            id: *id,
                            file_id: resolve_context.file_id,
                            namespace: resolve_context.namespace,
                            pub_span: None,
                            name: name.clone(),
                            ty: ast::Spanned {
                                inner: resolved_ty,
                                span: name.span,
                            },
                            value: Some(Box::new(value_expr)),
                            accesses: Vec::new(),
                        });
                        self.tir.const_index_lookup.insert(*id, const_index);
                        self.tir.impl_members.entry(self_type).or_default().insert(
                            name.inner,
                            ImplEntry::AssociatedConst {
                                id: *id,
                                ty: resolved_ty,
                            },
                        );
                    }
                }
            }

            // ── impl method ───────────────────────────────────────────────────
            AstNodeRef::ImplMethod {
                impl_target, item, ..
            } => {
                let self_type = self.resolve_type(&resolve_context, impl_target);
                let resolve_context = resolve_context.with_self_type(Some(self_type));
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
                    let params: Box<_> = signature
                        .params
                        .iter()
                        .map(|p| {
                            let is_self = p.inner.inner.name.inner == self_symbol;
                            FunctionParam {
                                mut_span: p.inner.inner.mut_span,
                                name: p.inner.inner.name.clone(),
                                ty: match &p.inner.inner.ty {
                                    Some(te) => Spanned {
                                        inner: self.resolve_type(&resolve_context, te),
                                        span: te.span,
                                    },
                                    None => Spanned {
                                        inner: if is_self { self_type } else { TypeIndex::ERROR },
                                        span: p.inner.inner.name.span,
                                    },
                                },
                            }
                        })
                        .collect();
                    let result = signature.result.as_ref().map(|r| Spanned {
                        inner: self.resolve_type(&resolve_context, r),
                        span: r.span,
                    });
                    let signature_index = self.intern_function(&params, result.clone());
                    let func_attrs = self.resolve_attributes(attributes);
                    self.register_lang_items(*id, &func_attrs);
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
                        body: None,
                        type_params: Box::new([]),
                        pub_span: *pub_span,
                        kind: FunctionKind::Impl,
                        signature_index,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params,
                        result,
                        attributes: func_attrs,
                    });
                    self.tir.function_index_lookup.insert(*id, func_index);
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

            // ── generic impl method ───────────────────────────────────────────
            AstNodeRef::GenericImplMethod {
                impl_type_params,
                impl_target,
                item,
                block_index,
            } => {
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

                // Each method in the impl block owns its own copy of the type
                // params because TypeParam { owner: Function(id) } is keyed by
                // the function's DefId.  The resolved names/bounds are the same
                // for every method; only the owner differs.
                let resolved_type_params =
                    self.resolve_ast_type_params(&resolve_context, impl_type_params);
                let type_param_scope = TypeParamScope {
                    owner: TypeParamOwner::Function(*id),
                    params: resolved_type_params.clone(),
                };
                let resolve_context = resolve_context.with_type_param_scope(type_param_scope);

                let self_type = self.resolve_type(&resolve_context, impl_target);

                // Reject `impl<T> T { ... }` — the outer type constructor must
                // be concrete even if its inner types reference type params.
                if matches!(
                    self.tir.type_pool[self_type.as_usize()],
                    Type::TypeParam { .. }
                ) {
                    self.tir.diagnostics.push(
                        Diagnostic::error()
                            .with_code(DiagnosticCode::TypeMistmatch.code())
                            .with_message("no nominal type found for inherent implementation")
                            .with_label(Label::primary(resolve_context.file_id, impl_target.span))
                            .with_note(
                                "either implement a trait on it or create a newtype to wrap it instead",
                            ),
                    );
                    return;
                }

                let resolve_context = resolve_context.with_self_type(Some(self_type));
                let self_symbol = self.interner.get_or_intern("self");

                let params: Box<_> = signature
                    .params
                    .iter()
                    .map(|p| {
                        let is_self = p.inner.inner.name.inner == self_symbol;
                        FunctionParam {
                            mut_span: p.inner.inner.mut_span,
                            name: p.inner.inner.name.clone(),
                            ty: match &p.inner.inner.ty {
                                Some(te) => Spanned {
                                    inner: self.resolve_type(&resolve_context, te),
                                    span: te.span,
                                },
                                None => Spanned {
                                    inner: if is_self { self_type } else { TypeIndex::ERROR },
                                    span: p.inner.inner.name.span,
                                },
                            },
                        }
                    })
                    .collect();
                let result = signature.result.as_ref().map(|r| Spanned {
                    inner: self.resolve_type(&resolve_context, r),
                    span: r.span,
                });
                let signature_index = self.intern_function(&params, result.clone());
                let func_attrs = self.resolve_attributes(attributes);
                self.register_lang_items(*id, &func_attrs);
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
                    body: None,
                    type_params: resolved_type_params.clone(),
                    pub_span: *pub_span,
                    kind: FunctionKind::Impl,
                    signature_index,
                    name: signature.name.clone(),
                    accesses: Vec::new(),
                    params,
                    result,
                    attributes: func_attrs,
                });
                self.tir.function_index_lookup.insert(*id, func_index);

                let entry = if is_method {
                    ImplEntry::Method(func_index)
                } else {
                    ImplEntry::AssociatedFn(func_index)
                };
                let block = &mut self.tir.generic_impl_list[block_index];
                if block.target == TypeIndex::ERROR {
                    block.target = self_type;
                    block.type_params = resolved_type_params;
                }
                block.members.insert(signature.name.inner, entry);

                // Populate the O(1) dispatch index.
                if let Some(kind) =
                    GenericImplTargetKind::from_type(&self.tir.type_pool[self_type.as_usize()])
                {
                    self.tir
                        .generic_impl_dispatch
                        .insert((kind, signature.name.inner), block_index);
                }
            }

            // ── trait block (supertrait resolution) ───────────────────────────
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
                self.register_lang_items(*trait_id, &resolved_attrs);

                let resolved: Vec<TraitIndex> = supertraits
                    .iter()
                    .filter_map(|bound| {
                        let type_idx = self.resolve_type(&resolve_context, bound);
                        match &self.tir.type_pool[type_idx.as_usize()] {
                            Type::Trait { trait_index } => Some(*trait_index),
                            _ if type_idx == TypeIndex::ERROR => None,
                            _ => {
                                self.tir.diagnostics.push(
                                    Diagnostic::error()
                                        .with_code(DiagnosticCode::ExpectedTrait.code())
                                        .with_message("expected a trait name")
                                        .with_label(Label::primary(
                                            resolve_context.file_id,
                                            bound.span,
                                        )),
                                );
                                None
                            }
                        }
                    })
                    .collect();

                let mut bindings: HashMap<(TraitIndex, SymbolU32), TypeIndex> = HashMap::new();
                for (bound, &trait_idx) in supertraits.iter().zip(resolved.iter()) {
                    let args = match &bound.inner {
                        ast::TypeExpression::GenericApplication { args, .. }
                            if !args.is_empty() =>
                        {
                            args
                        }
                        _ => continue,
                    };

                    for arg in args.iter() {
                        let (key, value) = match &arg.inner.inner {
                            ast::GenericArg::Binding { name, ty } => (name, ty),
                            _ => continue,
                        };
                        let val_ty = self.resolve_type(&resolve_context, value);
                        bindings.insert((trait_idx, key.inner), val_ty);
                        if let Some(at) = self.tir.traits[trait_idx as usize]
                            .assoc_types
                            .get_mut(&key.inner)
                        {
                            at.accesses
                                .push(SourceSpan::new(resolve_context.file_id, key.span));
                        }
                        self.check_assoc_type_bounds(
                            resolve_context.file_id,
                            trait_idx,
                            key.inner,
                            val_ty,
                            value.span,
                        );
                    }
                }
                self.tir.traits[trait_index as usize].supertraits = resolved;
                self.tir.traits[trait_index as usize].supertrait_bindings = bindings;
            }

            // ── typeset ───────────────────────────────────────────────────────
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
                        let ty = self.resolve_type(&resolve_context, &m.inner);
                        if !ty.is_integer() {
                            self.tir.diagnostics.push(
                                Diagnostic::error()
                                    .with_code(DiagnosticCode::TypesetMemberNotInteger.code())
                                    .with_message("typeset member must be an integer type")
                                    .with_label(
                                        Label::primary(resolve_context.file_id, m.inner.span)
                                            .with_message(format!(
                                                "`{}` is not an integer type",
                                                TypeFormatter::new(&self.tir, &self.interner)
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
                self.tir.typesets[typeset_index as usize].members = resolved_members;
                self.tir.typesets[typeset_index as usize].intersection_range = intersection_range;
            }

            // ── trait function ────────────────────────────────────────────────
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
                    let self_type_param_idx = self.intern_type(Type::TypeParam {
                        owner: TypeParamOwner::Function(*id),
                        param_index: 0,
                    });
                    let resolve_context = resolve_context.with_self_type(Some(self_type_param_idx));
                    let attributes = self.resolve_attributes(attributes);
                    self.register_lang_items(*id, &attributes);

                    // Self occupies slot 0; any explicit generics on the method start at 1.
                    let self_name_sym = self.interner.get_or_intern("Self");
                    let self_type_param = TypeParamInfo {
                        name: self_name_sym,
                        name_span: self.tir.traits[trait_index as usize].name.span,
                        bounds: Box::new([trait_index]),
                        typeset_bound: None,
                        accesses: Vec::new(),
                    };
                    let explicit_type_params =
                        self.resolve_ast_type_params(&resolve_context, &signature.type_params);
                    let type_params: Box<[TypeParamInfo]> = std::iter::once(self_type_param)
                        .chain(explicit_type_params)
                        .collect();
                    let func_index = self.tir.functions.len() as u32;
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: resolve_context.file_id,
                        namespace: resolve_context.namespace,
                        body: None,
                        pub_span: None,
                        kind: FunctionKind::Trait,
                        type_params,
                        signature_index: TypeIndex::ERROR,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params: Box::new([]),
                        result: None,
                        attributes: attributes.clone(),
                    });
                    self.tir.function_index_lookup.insert(*id, func_index);
                    let resolve_context = resolve_context.with_type_param_scope(TypeParamScope {
                        owner: TypeParamOwner::Function(*id),
                        params: self.tir.functions[func_index as usize].type_params.clone(),
                    });
                    let params: Box<[FunctionParam]> = signature
                        .params
                        .iter()
                        .map(|p| {
                            let is_self = p.inner.inner.name.inner == self_sym;
                            FunctionParam {
                                mut_span: p.inner.inner.mut_span,
                                name: p.inner.inner.name.clone(),
                                ty: match &p.inner.inner.ty {
                                    Some(te) => Spanned {
                                        inner: self.resolve_type(&resolve_context, te),
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
                                },
                            }
                        })
                        .collect();
                    let result = signature.result.as_ref().map(|r| Spanned {
                        inner: self.resolve_type(&resolve_context, r),
                        span: r.span,
                    });
                    let sig_idx = self.intern_function(&params, result.clone());
                    let func = &mut self.tir.functions[func_index as usize];
                    func.params = params;
                    func.result = result;
                    func.signature_index = sig_idx;
                    self.tir.traits[trait_index as usize]
                        .members
                        .insert(signature.name.inner, ImplEntry::Method(func_index));
                }
            }

            // ── trait const ───────────────────────────────────────────────────
            AstNodeRef::TraitConst {
                trait_index, item, ..
            } => {
                // Self is a TypeParam owned by the trait so `Self::*mut u8` is valid.
                let self_type_param = self.intern_type(Type::TypeParam {
                    owner: TypeParamOwner::Trait(trait_index),
                    param_index: 0,
                });
                let resolve_context = resolve_context.with_self_type(Some(self_type_param));
                if let ast::TraitItem::Const { id, name, ty } = item {
                    let ty_idx = self.resolve_type(&resolve_context, ty);
                    let const_index = self.tir.constants.len() as ConstIndex;
                    self.tir.constants.push(Constant {
                        id: *id,
                        file_id: resolve_context.file_id,
                        namespace: resolve_context.namespace,
                        pub_span: None,
                        name: name.clone(),
                        ty: Spanned {
                            inner: ty_idx,
                            span: ty.span,
                        },
                        value: None,
                        accesses: Vec::new(),
                    });
                    self.tir.const_index_lookup.insert(*id, const_index);
                    self.tir.traits[trait_index as usize].members.insert(
                        name.inner,
                        ImplEntry::AssociatedConst {
                            id: *id,
                            ty: ty_idx,
                        },
                    );
                }
            }

            // ── global ────────────────────────────────────────────────────────
            AstNodeRef::Global { item } => {
                if let ast::Item::Global {
                    pub_span,
                    mut_span,
                    name,
                    ty,
                    id,
                    ..
                } = item
                {
                    if let Some(first_def) = self
                        .lookup_symbol(
                            resolve_context.namespace,
                            (SymbolNamespace::Value, name.inner),
                        )
                        .filter(|k| !matches!(k, SymbolKind::Pending(_)))
                        .cloned()
                    {
                        let name_str = self.interner.resolve(name.inner).unwrap();
                        let first_definition = self.get_symbol_location(first_def);
                        self.tir.diagnostics.push(report_duplicate_definition(
                            DuplicateDefinitionDiagnostic {
                                name: name_str,
                                namespace: SymbolNamespace::Value,
                                first_definition,
                                second_definition: SourceSpan::new(
                                    resolve_context.file_id,
                                    name.span,
                                ),
                            },
                        ));
                    } else {
                        let (ty, ty_span) = match ty {
                            Some(ty) => (self.resolve_type(&resolve_context, ty), ty.span),
                            None => {
                                self.tir.diagnostics.push(report_type_annotation_required(
                                    SourceSpan::new(resolve_context.file_id, name.span),
                                ));
                                (TypeIndex::ERROR, name.span)
                            }
                        };
                        let global_index = self.tir.globals.len() as u32;
                        self.insert_symbol(
                            resolve_context.namespace,
                            (SymbolNamespace::Value, name.inner),
                            SymbolKind::Global { global_index },
                        );
                        self.tir.globals.push(Global {
                            id: *id,
                            file_id: resolve_context.file_id,
                            namespace: resolve_context.namespace,
                            value: None,
                            name: name.clone(),
                            ty: ast::Spanned {
                                inner: ty,
                                span: ty_span,
                            },
                            pub_span: *pub_span,
                            mut_span: mut_span.clone(),
                            accesses: Vec::new(),
                        });
                        self.tir.global_index_lookup.insert(*id, global_index);
                    }
                }
            }

            // ── memory ────────────────────────────────────────────────────────
            AstNodeRef::Memory { item } => {
                if let ast::Item::Memory {
                    name,
                    kind,
                    id,
                    config,
                } = item
                {
                    let type_idx = self.resolve_type(&resolve_context, kind);
                    let trait_index =
                        match self.tir.type_pool[type_idx.as_usize()] {
                            Type::Trait { trait_index } => trait_index,
                            _ => {
                                self.tir.diagnostics.push(report_invalid_memory_kind(
                                    SourceSpan::new(resolve_context.file_id, kind.span),
                                ));
                                return;
                            }
                        };

                    let mut bindings: HashMap<SymbolU32, TypeIndex> = HashMap::new();
                    match &kind.inner {
                        ast::TypeExpression::GenericApplication { args, .. }
                            if !args.is_empty() =>
                        {
                            for arg in args.iter() {
                                let (key, value) = match &arg.inner.inner {
                                    ast::GenericArg::Binding { name, ty } => (name, ty),
                                    _ => continue,
                                };
                                let val_ty = self.resolve_type(&resolve_context, value);
                                bindings.insert(key.inner, val_ty);
                                if let Some(at) = self.tir.traits[trait_index as usize]
                                    .assoc_types
                                    .get_mut(&key.inner)
                                {
                                    at.accesses
                                        .push(SourceSpan::new(resolve_context.file_id, key.span));
                                }
                                self.check_assoc_type_bounds(
                                    resolve_context.file_id,
                                    trait_index,
                                    key.inner,
                                    val_ty,
                                    value.span,
                                );
                            }
                        }
                        _ => {}
                    };

                    let size_symbol = self.interner.get_or_intern("Size");
                    let memory_kind =
                        match bindings.get(&size_symbol).copied() {
                            Some(ty) if ty == TypeIndex::U32 => MemoryKind::Memory32,
                            Some(ty) if ty == TypeIndex::U64 => MemoryKind::Memory64,
                            _ => {
                                self.tir.diagnostics.push(report_invalid_memory_kind(
                                    SourceSpan::new(resolve_context.file_id, kind.span),
                                ));
                                return;
                            }
                        };

                    let trait_fn_ids = self.tir.traits[trait_index as usize].member_def_ids.clone();
                    for tid in trait_fn_ids {
                        self.ensure_signature(tid);
                    }
                    let memory_index = self.tir.memories.len() as u32;
                    self.tir.memories.push(Memory {
                        id: *id,
                        file_id: resolve_context.file_id,
                        kind: memory_kind,
                        name: name.clone(),
                        min_pages: config
                            .as_ref()
                            .and_then(|c| c.min.as_ref().map(|s| s.inner)),
                        max_pages: config
                            .as_ref()
                            .and_then(|c| c.max.as_ref().map(|s| s.inner)),
                    });
                    self.tir.memory_index_lookup.insert(*id, memory_index);
                    let memory_type = self.intern_type(Type::Memory {
                        kind: memory_kind,
                        id: *id,
                    });
                    self.seed_memory_trait_impl_with(trait_index, memory_type, &bindings);
                    self.insert_symbol(
                        resolve_context.namespace,
                        (SymbolNamespace::Type, name.inner),
                        SymbolKind::Memory {
                            memory_index,
                            kind: memory_kind,
                        },
                    );
                    self.insert_symbol(
                        resolve_context.namespace,
                        (SymbolNamespace::Value, name.inner),
                        SymbolKind::Memory {
                            memory_index,
                            kind: memory_kind,
                        },
                    );
                }
            }

            // ── const ─────────────────────────────────────────────────────────
            AstNodeRef::Const { item } => {
                if let ast::Item::Const {
                    id,
                    pub_span,
                    name,
                    ty,
                    value,
                } = item
                {
                    if let Some(first_def) = self
                        .lookup_symbol(
                            resolve_context.namespace,
                            (SymbolNamespace::Value, name.inner),
                        )
                        .filter(|k| !matches!(k, SymbolKind::Pending(_)))
                        .cloned()
                    {
                        let name_str = self.interner.resolve(name.inner).unwrap();
                        let first_definition = self.get_symbol_location(first_def);
                        self.tir.diagnostics.push(report_duplicate_definition(
                            DuplicateDefinitionDiagnostic {
                                name: name_str,
                                namespace: SymbolNamespace::Value,
                                first_definition,
                                second_definition: SourceSpan::new(
                                    resolve_context.file_id,
                                    name.span,
                                ),
                            },
                        ));
                    } else {
                        let (ty_idx, ty_span) = match ty {
                            Some(ty) => (self.resolve_type(&resolve_context, ty), ty.span),
                            None => {
                                self.tir.diagnostics.push(report_type_annotation_required(
                                    SourceSpan::new(resolve_context.file_id, name.span),
                                ));
                                (TypeIndex::ERROR, name.span)
                            }
                        };
                        if let Ok(value_expr) =
                            self.build_const_expression(&resolve_context, value, ty_idx)
                        {
                            let const_index = self.tir.constants.len() as ConstIndex;
                            self.tir.constants.push(Constant {
                                id: *id,
                                file_id: resolve_context.file_id,
                                namespace: resolve_context.namespace,
                                pub_span: *pub_span,
                                name: name.clone(),
                                ty: ast::Spanned {
                                    inner: ty_idx,
                                    span: ty_span,
                                },
                                value: Some(Box::new(value_expr)),
                                accesses: Vec::new(),
                            });
                            self.tir.const_index_lookup.insert(*id, const_index);
                            self.insert_symbol(
                                resolve_context.namespace,
                                (SymbolNamespace::Value, name.inner),
                                SymbolKind::Const { const_index },
                            );
                        }
                    }
                }
            }

            // ── imported function ─────────────────────────────────────────────
            AstNodeRef::IntrinsicFunction { item } => {
                if let ast::Item::IntrinsicFunction { id, signature } = item {
                    let type_params =
                        self.resolve_ast_type_params(&resolve_context, &signature.type_params);
                    let func_index = self.tir.functions.len() as u32;
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: resolve_context.file_id,
                        namespace: resolve_context.namespace,
                        body: None,
                        kind: FunctionKind::Intrinsic,
                        type_params,
                        pub_span: None,
                        signature_index: TypeIndex::ERROR,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params: Box::new([]),
                        result: None,
                        attributes: Box::new([]),
                    });
                    self.tir.function_index_lookup.insert(*id, func_index);
                    let signature_context = resolve_context.with_type_param_scope(TypeParamScope {
                        owner: TypeParamOwner::Function(*id),
                        params: self.tir.functions[func_index as usize].type_params.clone(),
                    });
                    let (params, result) =
                        self.build_function_signature(&signature_context, signature);
                    let signature_index = self.intern_function(&params, result.clone());
                    let func = &mut self.tir.functions[func_index as usize];
                    func.params = params;
                    func.result = result;
                    func.signature_index = signature_index;
                    self.insert_symbol(
                        resolve_context.namespace,
                        (SymbolNamespace::Value, signature.name.inner),
                        SymbolKind::Function { func_index },
                    );
                }
            }

            AstNodeRef::ImportedFunction {
                import_module_index,
                decl,
                ..
            } => {
                if let ast::ImportDeclaration::Function { id, signature } = decl {
                    let (params, result) =
                        self.build_function_signature(&resolve_context, signature);
                    let signature_index = self.intern_function(&params, result.clone());
                    let func_index = self.tir.functions.len() as u32;
                    let import_ns_idx =
                        self.tir.import_decls[import_module_index as usize].namespace_idx;
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: resolve_context.file_id,
                        namespace: Some(import_ns_idx),
                        kind: FunctionKind::Free,
                        signature_index,
                        body: None,
                        type_params: Box::new([]),
                        pub_span: None,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params,
                        result,
                        attributes: Box::new([]),
                    });
                    self.tir.function_index_lookup.insert(*id, func_index);
                    let import_decl = &mut self.tir.import_decls[import_module_index as usize];
                    import_decl
                        .lookup
                        .insert(signature.name.inner, ImportValue::Function { id: *id });
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
                    let resolved_ty = self.resolve_type(&resolve_context, ty);
                    let global_index = self.tir.globals.len() as u32;
                    let import_ns_idx =
                        self.tir.import_decls[import_module_index as usize].namespace_idx;
                    self.tir.globals.push(Global {
                        id: *id,
                        file_id: resolve_context.file_id,
                        namespace: Some(import_ns_idx),
                        value: None,
                        name: name.clone(),
                        ty: ast::Spanned {
                            inner: resolved_ty,
                            span: ty.span,
                        },
                        pub_span: None,
                        mut_span: mut_span.clone(),
                        accesses: Vec::new(),
                    });
                    self.tir.global_index_lookup.insert(*id, global_index);
                    let import_decl = &mut self.tir.import_decls[import_module_index as usize];
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

            // ── impl trait block ─────────────────────────────────────────────
            // Resolves the trait and target types, creates the `TraitImpl`
            // entry, and populates the lookup tables. Methods and consts
            // demand-drive this arm before inserting into `TraitImpl.members`.
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

                let trait_type = self.resolve_type(&resolve_context, trait_name);
                let trait_index = match &self.tir.type_pool[trait_type.as_usize()] {
                    Type::Trait { trait_index } => *trait_index,
                    _ if trait_type == TypeIndex::ERROR => return,
                    _ => {
                        self.tir.diagnostics.push(
                            Diagnostic::error()
                                .with_code(DiagnosticCode::ExpectedTrait.code())
                                .with_message("expected a trait name")
                                .with_label(Label::primary(
                                    resolve_context.file_id,
                                    trait_name.span,
                                )),
                        );
                        return;
                    }
                };

                let target_type = self.resolve_type(&resolve_context, target);
                let trait_impl_index = self.tir.trait_impls.len() as TraitImplIndex;

                self.tir.trait_impls.push(TraitImpl {
                    trait_index,
                    target: target_type,
                    members: HashMap::new(),
                    span: trait_name.span,
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
                self.trait_impl_block_lookup
                    .insert(*block_id, trait_impl_index);

                // Seed default (bodied) trait methods into impl_members so
                // `self.method()` dispatch finds them without explicit overrides.
                let trait_fn_ids = self.tir.traits[trait_index as usize].member_def_ids.clone();
                for &tid in &trait_fn_ids {
                    self.ensure_signature(tid);
                }
                let self_sym = self.interner.get_or_intern("self");
                let mut default_members: Vec<(SymbolU32, ImplEntry)> = Vec::new();
                for &tid in &trait_fn_ids {
                    let fi = match self.tir.function_index_lookup.get(&tid).copied() {
                        Some(fi) => fi,
                        None => continue,
                    };
                    let node = match self.sig_state.get(&tid) {
                        Some(e) => &self.ast_nodes[e.node_idx].node,
                        None => continue,
                    };
                    if let AstNodeRef::TraitFunction { item, .. } = node {
                        if let ast::TraitItem::Function {
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
                }
                // `or_insert`: block DefId < child method DefIds (source order),
                // so defaults land first; `ImplTraitMethod` overwrites with plain
                // `insert` later in Phase 2, giving explicit overrides priority.
                let slot = self.tir.impl_members.entry(target_type).or_default();
                for (sym, entry) in default_members {
                    slot.entry(sym).or_insert(entry);
                }
            }

            // ── impl trait method ─────────────────────────────────────────────
            AstNodeRef::ImplTraitMethod {
                parent_id, item, ..
            } => {
                self.ensure_signature(parent_id);
                let trait_impl_index = match self.trait_impl_block_lookup.get(&parent_id) {
                    Some(&idx) => idx,
                    None => return,
                };
                let self_type = self.tir.trait_impls[trait_impl_index as usize].target;
                let resolve_context = resolve_context.with_self_type(Some(self_type));
                let self_symbol = self.interner.get_or_intern("self");

                if let ast::ImplItem::Method {
                    id,
                    pub_span,
                    attributes,
                    signature,
                    ..
                } = item
                {
                    let params: Box<_> = signature
                        .params
                        .iter()
                        .map(|p| {
                            let is_self = p.inner.inner.name.inner == self_symbol;
                            FunctionParam {
                                mut_span: p.inner.inner.mut_span,
                                name: p.inner.inner.name.clone(),
                                ty: match &p.inner.inner.ty {
                                    Some(te) => Spanned {
                                        inner: self.resolve_type(&resolve_context, te),
                                        span: te.span,
                                    },
                                    None => Spanned {
                                        inner: if is_self { self_type } else { TypeIndex::ERROR },
                                        span: p.inner.inner.name.span,
                                    },
                                },
                            }
                        })
                        .collect();
                    let result = signature.result.as_ref().map(|r| Spanned {
                        inner: self.resolve_type(&resolve_context, r),
                        span: r.span,
                    });
                    let signature_index = self.intern_function(&params, result.clone());
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
                    self.register_lang_items(*id, &func_attrs);
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: resolve_context.file_id,
                        namespace: resolve_context.namespace,
                        body: None,
                        type_params: Box::new([]),
                        pub_span: *pub_span,
                        kind: FunctionKind::TraitImpl { trait_impl_index },
                        signature_index,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params,
                        result,
                        attributes: func_attrs,
                    });
                    self.tir.function_index_lookup.insert(*id, func_index);
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

            // ── impl trait const ──────────────────────────────────────────────
            AstNodeRef::ImplTraitConst {
                parent_id, item, ..
            } => {
                self.ensure_signature(parent_id);
                let trait_impl_index = match self.trait_impl_block_lookup.get(&parent_id) {
                    Some(&idx) => idx,
                    None => return,
                };
                let self_type = self.tir.trait_impls[trait_impl_index as usize].target;
                let resolve_context = resolve_context.with_self_type(Some(self_type));

                if let ast::ImplItem::Const {
                    id,
                    name,
                    ty,
                    value,
                    ..
                } = item
                {
                    let resolved_ty = match ty {
                        Some(te) => self.resolve_type(&resolve_context, te),
                        None => TypeIndex::ERROR,
                    };
                    if let Ok(value_expr) =
                        self.build_const_expression(&resolve_context, value, resolved_ty)
                    {
                        let const_index = self.tir.constants.len() as ConstIndex;
                        self.tir.constants.push(Constant {
                            id: *id,
                            file_id: resolve_context.file_id,
                            namespace: resolve_context.namespace,
                            pub_span: None,
                            name: name.clone(),
                            ty: ast::Spanned {
                                inner: resolved_ty,
                                span: name.span,
                            },
                            value: Some(Box::new(value_expr)),
                            accesses: Vec::new(),
                        });
                        self.tir.const_index_lookup.insert(*id, const_index);
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

            // ── trait associated type ─────────────────────────────────────────
            AstNodeRef::TraitAssociatedType {
                trait_index, item, ..
            } => {
                let resolve_context = resolve_context
                    .with_self_type(Some(self.intern_type(Type::Trait { trait_index })));
                if let ast::TraitItem::AssociatedType {
                    id, name, bounds, ..
                } = item
                {
                    // Resolve each bound TypeExpression to either a TraitIndex or TypesetIndex.
                    let mut trait_bounds: Vec<TraitIndex> = Vec::new();
                    let mut typeset_bound: Option<TypesetIndex> = None;
                    for b in bounds.iter() {
                        let ty = self.resolve_type(&resolve_context, b);
                        match self.tir.type_pool[ty.as_usize()].clone() {
                            Type::Trait { trait_index } => trait_bounds.push(trait_index),
                            Type::TypeSet { typeset_index } => {
                                if typeset_bound.is_some() {
                                    self.tir.diagnostics.push(
                                        Diagnostic::error()
                                            .with_code(DiagnosticCode::MultipleTypesetBounds.code())
                                            .with_message(
                                                "associated type may have at most one typeset bound",
                                            )
                                            .with_label(Label::primary(
                                                resolve_context.file_id,
                                                b.span,
                                            )),
                                    );
                                } else {
                                    typeset_bound = Some(typeset_index);
                                }
                            }
                            _ => {
                                self.tir.diagnostics.push(
                                    Diagnostic::error()
                                        .with_code(DiagnosticCode::ExpectedTrait.code())
                                        .with_message(
                                            "associated type bound must be a trait or typeset",
                                        )
                                        .with_label(Label::primary(
                                            resolve_context.file_id,
                                            b.span,
                                        )),
                                );
                            }
                        }
                    }

                    let placeholder = self.intern_type(Type::AssociatedType {
                        assoc_name: name.inner,
                        trait_index,
                    });

                    self.tir.traits[trait_index as usize].assoc_types.insert(
                        name.inner,
                        TraitAssocType {
                            id: *id,
                            name_span: name.span,
                            bounds: trait_bounds.into_boxed_slice(),
                            typeset_bound,
                            accesses: Vec::new(),
                        },
                    );
                    self.tir.traits[trait_index as usize]
                        .members
                        .insert(name.inner, ImplEntry::AssociatedType { ty: placeholder });

                    // Replace Pending with TraitAssocType only if it's still our
                    // own Pending — never clobber a same-named resolved symbol.
                    if matches!(
                        self.lookup_symbol(resolve_context.namespace, (SymbolNamespace::Type, name.inner)),
                        Some(SymbolKind::Pending(d)) if *d == *id
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

            // ── impl trait associated type ────────────────────────────────────
            AstNodeRef::ImplTraitAssociatedType {
                parent_id, item, ..
            } => {
                self.ensure_signature(parent_id);
                let trait_impl_index = match self.trait_impl_block_lookup.get(&parent_id) {
                    Some(&idx) => idx,
                    None => return,
                };
                let trait_index = self.tir.trait_impls[trait_impl_index as usize].trait_index;
                let self_type = self.tir.trait_impls[trait_impl_index as usize].target;
                let resolve_context = resolve_context.with_self_type(Some(self_type));

                if let ast::ImplItem::AssociatedType { name, ty, .. } = item {
                    let concrete_ty = self.resolve_type(&resolve_context, ty);
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
                        .member_def_ids
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
                        at.accesses
                            .push(SourceSpan::new(resolve_context.file_id, name.span));
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

    // ── query: ensure_body ────────────────────────────────────────────────────

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

        // Extract (sig, body_expr, func_index) — only function-like nodes have bodies.
        let (sig, body_expr, func_index) = match node {
            AstNodeRef::Function { item } => match item {
                ast::Item::Function {
                    id,
                    signature,
                    block,
                    ..
                } => {
                    let fi = match self.tir.function_index_lookup.get(id) {
                        Some(&fi) => fi,
                        None => return,
                    };
                    (signature, block.as_ref(), fi)
                }
                _ => return,
            },
            AstNodeRef::ImplMethod { item, .. }
            | AstNodeRef::ImplTraitMethod { item, .. }
            | AstNodeRef::GenericImplMethod { item, .. } => match item {
                ast::ImplItem::Method {
                    id,
                    signature,
                    block,
                    ..
                } => {
                    let fi = match self.tir.function_index_lookup.get(id) {
                        Some(&fi) => fi,
                        None => return,
                    };
                    (signature, block.as_ref(), fi)
                }
                _ => return,
            },
            AstNodeRef::TraitFunction { item, .. } => match item {
                ast::TraitItem::Function {
                    id,
                    signature,
                    body: Some(body),
                    ..
                } => {
                    let fi = match self.tir.function_index_lookup.get(id) {
                        Some(&fi) => fi,
                        None => return,
                    };
                    (signature, body.as_ref(), fi)
                }
                _ => return,
            },
            AstNodeRef::Global { item } => {
                let resolve_context = ResolveContext::new(file_id, namespace);

                let ast::Item::Global { id, value, .. } = item else {
                    unreachable!();
                };

                let global_index = self.tir.global_index_lookup[id];
                let global_ty = self.tir.globals[global_index as usize].ty.inner;
                let value_expr =
                    match self.build_const_expression(&resolve_context, value, global_ty) {
                        Ok(value_expr) => value_expr,
                        Err(_) => return,
                    };

                match value_expr.ty {
                    _ if value_expr.ty.is_comptime_number() => {
                        self.tir.diagnostics.push(report_type_annotation_required(
                            SourceSpan::new(resolve_context.file_id, value.span),
                        ));
                    }
                    ty if !self.coercible_to(ty, global_ty) => {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            TypeMistmatchDiagnostic {
                                expected_type: global_ty,
                                actual_type: ty,
                                span: SourceSpan::new(resolve_context.file_id, value.span),
                            },
                        ));
                    }
                    _ => {
                        self.tir.globals[global_index as usize].value =
                            Some(Box::new(ast::Spanned {
                                inner: value_expr,
                                span: value.span,
                            }));
                    }
                }

                return;
            }
            _ => return,
        };

        let resolve_context = ResolveContext::new(file_id, namespace);
        let self_type = match &node {
            AstNodeRef::ImplMethod { impl_target, .. } => {
                Some(self.resolve_type(&resolve_context, impl_target))
            }
            AstNodeRef::ImplTraitMethod { parent_id, .. } => self
                .trait_impl_block_lookup
                .get(parent_id)
                .map(|&idx| self.tir.trait_impls[idx as usize].target),
            AstNodeRef::TraitFunction { .. } => {
                // Self is TypeParam{0} in trait default methods (see ensure_signature).
                Some(self.intern_type(Type::TypeParam {
                    owner: TypeParamOwner::Function(self.tir.functions[func_index as usize].id),
                    param_index: 0,
                }))
            }
            _ => None,
        };

        let resolve_context = ResolveContext {
            file_id: resolve_context.file_id,
            namespace: resolve_context.namespace,
            self_type,
            type_param_scope: Some(TypeParamScope {
                owner: TypeParamOwner::Function(self.tir.functions[func_index as usize].id),
                params: self.tir.functions[func_index as usize].type_params.clone(),
            }),
        };

        match self.build_function_body(resolve_context, sig, body_expr, func_index) {
            Ok(body) => {
                self.tir.functions[func_index as usize].body = Some(body);
            }
            Err(_) => {}
        }
    }

    fn resolve_ast_type_params(
        &mut self,
        resolve_context: &ResolveContext,
        ast_params: &[ast::TypeParam],
    ) -> Box<[TypeParamInfo]> {
        ast_params
            .iter()
            .map(|tp| {
                let mut bounds: Vec<TraitIndex> = Vec::new();
                let mut typeset_bound: Option<TypesetIndex> = None;

                for bound in tp.bounds.iter() {
                    let ty = self.resolve_type(resolve_context, bound);
                    match &self.tir.type_pool[ty.as_usize()] {
                        Type::Trait { trait_index } => bounds.push(*trait_index),
                        Type::TypeSet { typeset_index } => {
                            if typeset_bound.is_some() {
                                self.tir.diagnostics.push(
                                    Diagnostic::error()
                                        .with_code(DiagnosticCode::MultipleTypesetBounds.code())
                                        .with_message(
                                            "a type parameter can have at most one typeset bound",
                                        )
                                        .with_label(Label::primary(
                                            resolve_context.file_id,
                                            bound.span,
                                        )),
                                );
                            } else {
                                typeset_bound = Some(*typeset_index);
                            }
                        }
                        _ => {
                            self.tir.diagnostics.push(
                                Diagnostic::error()
                                    .with_code(DiagnosticCode::ExpectedTrait.code())
                                    .with_message("expected a trait or typeset name as a bound")
                                    .with_label(Label::primary(
                                        resolve_context.file_id,
                                        bound.span,
                                    )),
                            );
                        }
                    }
                }

                TypeParamInfo {
                    name: tp.name.inner,
                    name_span: tp.name.span,
                    bounds: bounds.into_boxed_slice(),
                    typeset_bound,
                    accesses: Vec::new(),
                }
            })
            .collect()
    }

    fn build_function_signature(
        &mut self,
        resolve_context: &ResolveContext,
        signature: &ast::FunctionSignature,
    ) -> (Box<[FunctionParam]>, Option<Spanned<TypeIndex>>) {
        let mut seen_params: HashMap<SymbolU32, ast::TextSpan> = HashMap::new();
        let params: Box<[FunctionParam]> = signature
            .params
            .iter()
            .map(|param| {
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
                // TODO: report unnecessary mutability for imported function signatures or move
                // this to the resolver itself match param.inner.inner.mut_span
                // {     Some(mut_span) => {}
                //     None => {}
                // }
                FunctionParam {
                    mut_span: param.inner.inner.mut_span,
                    name: name.clone(),
                    ty: match &param.inner.inner.ty {
                        Some(ty) => Spanned {
                            inner: self.resolve_type(&resolve_context, &ty),
                            span: ty.span,
                        },
                        None => Spanned {
                            inner: TypeIndex::ERROR,
                            span: param.inner.inner.name.span,
                        },
                    },
                }
            })
            .collect();
        let result = signature.result.as_ref().map(|result| Spanned {
            inner: self.resolve_type(&resolve_context, result),
            span: result.span,
        });

        (params, result)
    }

    fn substitute_type(&mut self, ty: TypeIndex, type_args: &[TypeIndex]) -> TypeIndex {
        match &self.tir.type_pool[ty.as_usize()] {
            // Types that can never contain TypeParams — return immediately.
            Type::Unit
            | Type::Bool
            | Type::Error
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
            | Type::Module { .. }
            | Type::Memory { .. }
            | Type::Trait { .. }
            | Type::AssociatedType { .. }
            | Type::TypeSet { .. } => ty,
            Type::TypeParam { param_index, .. } => type_args
                .get(*param_index as usize)
                .copied()
                .filter(|&t| t != TypeIndex::ERROR)
                .unwrap_or(ty),
            Type::AssocTypeProjection {
                param_index,
                assoc_name,
                ..
            } => {
                let receiver = type_args.get(*param_index as usize).copied().unwrap_or(ty);
                match &self.tir.type_pool[receiver.as_usize()] {
                    Type::TypeParam { .. } => ty,
                    _ => self
                        .tir
                        .impl_members
                        .get(&receiver)
                        .and_then(|m| m.get(&assoc_name))
                        .and_then(|e| {
                            if let ImplEntry::AssociatedType { ty: concrete } = e {
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
                mutable,
                memory,
            } => {
                let (to, mutable, memory) = (*to, *mutable, *memory);
                let next_to = self.substitute_type(to, type_args);
                let next_memory = self.substitute_type(memory, type_args);
                if next_to == to && next_memory == memory {
                    ty
                } else {
                    self.intern_type(Type::Pointer {
                        to: next_to,
                        mutable,
                        memory: next_memory,
                    })
                }
            }
            Type::Array {
                of,
                size,
                mutable,
                memory,
            } => {
                let (of, size, mutable, memory) = (*of, *size, *mutable, *memory);
                let next_of = self.substitute_type(of, type_args);
                let next_memory = self.substitute_type(memory, type_args);
                if next_of == of && next_memory == memory {
                    ty
                } else {
                    self.intern_type(Type::Array {
                        of: next_of,
                        size,
                        mutable,
                        memory: next_memory,
                    })
                }
            }
            Type::Slice {
                of,
                mutable,
                memory,
            } => {
                let (of, mutable, memory) = (*of, *mutable, *memory);
                let next_of = self.substitute_type(of, type_args);
                let next_memory = self.substitute_type(memory, type_args);
                if next_of == of && next_memory == memory {
                    ty
                } else {
                    self.intern_type(Type::Slice {
                        of: next_of,
                        mutable,
                        memory: next_memory,
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
    ) -> Option<TypeIndex> {
        let result = self.substitute_type(ty, type_args);
        match &self.tir.type_pool[result.as_usize()] {
            Type::TypeParam { .. } | Type::Integer | Type::Float | Type::Error => None,
            _ => Some(result),
        }
    }

    fn infer_type_args(
        &self,
        type_args: &mut [TypeIndex],
        pattern_ty: TypeIndex,
        actual_ty: TypeIndex,
    ) {
        // Unresolved comptime literals have no concrete type yet; inferring T = INTEGER would give the wrong answer once the literal is coerced.
        if actual_ty == TypeIndex::INTEGER
            || actual_ty == TypeIndex::FLOAT
            || actual_ty == TypeIndex::ERROR
        {
            return;
        }

        match (
            &self.tir.type_pool[pattern_ty.as_usize()],
            &self.tir.type_pool[actual_ty.as_usize()],
        ) {
            (Type::TypeParam { param_index, .. }, _) => {
                // First binding wins — don't overwrite a slot already set by turbofish or an earlier argument.
                match type_args.get_mut(*param_index as usize) {
                    Some(slot) if *slot == TypeIndex::ERROR => *slot = actual_ty,
                    _ => {}
                }
            }
            (
                Type::AssocTypeProjection {
                    assoc_name,
                    trait_index,
                    param_index,
                    ..
                },
                Type::AssocTypeProjection {
                    assoc_name: actual_assoc,
                    trait_index: actual_trait,
                    param_index: actual_param,
                    ..
                },
            ) if assoc_name == actual_assoc
                && trait_index == actual_trait
                && param_index == actual_param => {}
            (Type::Tuple { elements: pattern }, Type::Tuple { elements: actual })
                if pattern.len() == actual.len() =>
            {
                for (pattern, actual) in pattern.iter().copied().zip(actual.iter().copied()) {
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
                    mutable: pattern_mutable,
                    memory: pattern_memory,
                },
                Type::Pointer {
                    to: actual_to,
                    mutable: actual_mutable,
                    memory: actual_memory,
                },
            ) if pattern_mutable == actual_mutable => {
                self.infer_type_args(type_args, *pattern_to, *actual_to);
                self.infer_type_args(type_args, *pattern_memory, *actual_memory);
            }
            (
                Type::Array {
                    of: pattern_of,
                    size: pattern_size,
                    mutable: pattern_mutable,
                    memory: pattern_memory,
                },
                Type::Array {
                    of: actual_of,
                    size: actual_size,
                    mutable: actual_mutable,
                    memory: actual_memory,
                },
            ) if pattern_size == actual_size && pattern_mutable == actual_mutable => {
                self.infer_type_args(type_args, *pattern_of, *actual_of);
                self.infer_type_args(type_args, *pattern_memory, *actual_memory);
            }
            (
                Type::Slice {
                    of: pattern_of,
                    mutable: pattern_mutable,
                    memory: pattern_memory,
                },
                Type::Slice {
                    of: actual_of,
                    mutable: actual_mutable,
                    memory: actual_memory,
                },
            ) if pattern_mutable == actual_mutable => {
                self.infer_type_args(type_args, *pattern_of, *actual_of);
                self.infer_type_args(type_args, *pattern_memory, *actual_memory);
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
            ) if pattern_struct == actual_struct && pattern_args.len() == actual_args.len() => {
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

    fn seed_memory_trait_impl_with(
        &mut self,
        trait_index: u32,
        memory_type: TypeIndex,
        // Assoc-type overrides from the parent supertrait declaration.
        // E.g. Memory32's `Memory<Size=u32>` provides {"Size" → u32}.
        bindings: &HashMap<SymbolU32, TypeIndex>,
    ) {
        // Ensure all member signatures in this trait are resolved.
        for did in self.tir.traits[trait_index as usize].member_def_ids.clone() {
            self.ensure_signature(did);
        }
        let self_symbol = self.interner.get_or_intern("self");
        let raw_members: Vec<(SymbolU32, ImplEntry)> = self.tir.traits[trait_index as usize]
            .members
            .iter()
            .map(|(&sym, entry)| (sym, entry.clone()))
            .collect();
        let mut members: Vec<(SymbolU32, ImplEntry)> = Vec::with_capacity(raw_members.len());
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

    fn build_exports(&mut self, file_id: FileId, entries: &[Separated<Spanned<ast::ExportEntry>>]) {
        for entry in entries.iter() {
            let internal_name = &entry.inner.inner.name;

            let global_value = match self
                .symbol_lookup
                .get(&(SymbolNamespace::Value, internal_name.inner))
            {
                Some(value) => value.clone(),
                None => {
                    self.tir
                        .diagnostics
                        .push(report_undeclared_identifier(SourceSpan::new(
                            file_id,
                            internal_name.span,
                        )));
                    continue;
                }
            };

            let external_name = entry.inner.inner.alias.as_ref().map(|alias_span| {
                let escaped_text = self.interner.resolve(alias_span.inner).unwrap();
                let unescaped = unescape_string(escaped_text);
                let symbol = self.interner.get_or_intern(&unescaped);
                ast::Spanned {
                    inner: symbol,
                    span: alias_span.span,
                }
            });

            let export_item = match global_value {
                SymbolKind::Function { func_index } => {
                    self.tir.functions[func_index as usize]
                        .accesses
                        .push(FunctionAccess {
                            caller: None,
                            kind: FunctionAccessKind::Reference,
                            file_id,
                            span: internal_name.span,
                        });

                    ExportItem::Function {
                        id: self.tir.functions[func_index as usize].id,
                        internal_name: internal_name.clone(),
                        external_name,
                    }
                }
                SymbolKind::Global { global_index } => {
                    self.tir.globals[global_index as usize]
                        .accesses
                        .push(SourceSpan::new(file_id, internal_name.span));

                    ExportItem::Global {
                        id: self.tir.globals[global_index as usize].id,
                        internal_name: internal_name.clone(),
                        external_name,
                    }
                }
                SymbolKind::Memory { memory_index, .. } => ExportItem::Memory {
                    id: self.tir.memories[memory_index as usize].id,
                    internal_name: internal_name.clone(),
                    external_name,
                },
                _ => {
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

    fn build_const_expression(
        &mut self,
        resolve_context: &ResolveContext,
        expr: &ast::Spanned<ast::Expression>,
        expected_type: TypeIndex,
    ) -> Result<Expression, ()> {
        match &expr.inner {
            ast::Expression::Int { value } => Ok(Expression {
                kind: ExprKind::Int { value: *value },
                ty: expected_type,
                span: expr.span,
            }),
            ast::Expression::Float { value } => {
                let is_float_type = expected_type == TypeIndex::F32
                    || expected_type == TypeIndex::F64
                    || expected_type == TypeIndex::ERROR;
                if !is_float_type {
                    self.tir
                        .diagnostics
                        .push(report_float_literal_for_integer_type(SourceSpan::new(
                            resolve_context.file_id,
                            expr.span,
                        )));
                    return Err(());
                }
                Ok(Expression {
                    kind: ExprKind::Float { value: *value },
                    ty: expected_type,
                    span: expr.span,
                })
            }
            ast::Expression::Path(path) if path.segments.len() == 1 => {
                // Single-segment path: bool literal, named const, or error.
                let seg = path.segments.last().expect("path non-empty");
                let symbol = seg.ident.inner;
                match self
                    .lookup_symbol(resolve_context.namespace, (SymbolNamespace::Value, symbol))
                    .cloned()
                {
                    Some(SymbolKind::True) => Ok(Expression {
                        kind: ExprKind::Bool { value: true },
                        ty: TypeIndex::BOOL,
                        span: expr.span,
                    }),
                    Some(SymbolKind::False) => Ok(Expression {
                        kind: ExprKind::Bool { value: false },
                        ty: TypeIndex::BOOL,
                        span: expr.span,
                    }),
                    Some(SymbolKind::Const { const_index }) => {
                        let constant = &mut self.tir.constants[const_index as usize];
                        constant
                            .accesses
                            .push(SourceSpan::new(resolve_context.file_id, expr.span));
                        let id = constant.id;
                        let ty = constant.ty.inner;
                        Ok(Expression {
                            kind: ExprKind::Const { id },
                            ty,
                            span: expr.span,
                        })
                    }
                    _ => {
                        self.tir
                            .diagnostics
                            .push(report_non_constant_global_initializer(SourceSpan::new(
                                resolve_context.file_id,
                                expr.span,
                            )));
                        Err(())
                    }
                }
            }
            ast::Expression::Path(path) if path.segments.len() == 2 => {
                // Two-segment path: `Namespace::ASSOCIATED_CONST` in const context.
                // Only associated consts are valid here (methods, types, etc. are not).
                let first = &path.segments[0];
                let last = &path.segments[1];
                let file_id = resolve_context.file_id;
                let namespace_span = first.ident.span;
                let member_span = last.ident.span;

                let namespace_ty = self.resolve_type_identifier(
                    resolve_context,
                    first.ident.inner,
                    first.ident.span,
                );
                if namespace_ty == TypeIndex::ERROR {
                    return Err(());
                }

                let member_sym = last.ident.inner;
                let entry = self
                    .tir
                    .impl_members
                    .get(&namespace_ty)
                    .and_then(|m| m.get(&member_sym))
                    .cloned();
                match entry {
                    Some(ImplEntry::AssociatedConst { id, ty }) => {
                        if let Some(ci) = self.tir.const_index_lookup.get(&id).copied() {
                            self.tir.constants[ci as usize]
                                .accesses
                                .push(SourceSpan::new(file_id, member_span));
                        }
                        Ok(Expression {
                            kind: ExprKind::NamespaceAccess {
                                namespace: ast::Spanned {
                                    inner: namespace_ty,
                                    span: namespace_span,
                                },
                                member: Box::new(Expression {
                                    kind: ExprKind::Const { id },
                                    ty,
                                    span: member_span,
                                }),
                            },
                            ty,
                            span: expr.span,
                        })
                    }
                    _ => {
                        self.tir
                            .diagnostics
                            .push(report_non_constant_global_initializer(SourceSpan::new(
                                file_id, expr.span,
                            )));
                        Err(())
                    }
                }
            }
            ast::Expression::Unary { operator, operand } => {
                let operand_expr =
                    self.build_const_expression(resolve_context, operand, expected_type)?;

                Ok(Expression {
                    kind: ExprKind::Unary {
                        operator: operator.clone(),
                        operand: Box::new(operand_expr),
                    },
                    ty: expected_type,
                    span: expr.span,
                })
            }
            ast::Expression::Binary {
                left,
                right,
                operator,
            } => {
                let left_expr =
                    self.build_const_expression(resolve_context, left, expected_type)?;
                let right_expr =
                    self.build_const_expression(resolve_context, right, expected_type)?;

                let result_ty = match operator.inner {
                    operator if operator.is_comparison() || operator.is_logical() => {
                        TypeIndex::BOOL
                    }
                    _ => left_expr.ty,
                };

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator: operator.clone(),
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    ty: result_ty,
                    span: expr.span,
                })
            }
            ast::Expression::Grouping { value } => {
                self.build_const_expression(resolve_context, value, expected_type)
            }
            ast::Expression::Cast { value, ty } => {
                let cast_type = self.resolve_type(&resolve_context, &ty);
                let value_expr = self.build_const_expression(resolve_context, value, cast_type)?;

                Ok(Expression {
                    kind: value_expr.kind,
                    ty: cast_type,
                    span: expr.span,
                })
            }
            _ => {
                self.tir
                    .diagnostics
                    .push(report_non_constant_global_initializer(SourceSpan::new(
                        resolve_context.file_id,
                        expr.span,
                    )));
                Err(())
            }
        }
    }

    fn build_function_body(
        &mut self,
        resolve_context: ResolveContext,
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
            locals: self.tir.functions[func_index as usize]
                .params
                .iter()
                .map(|param| Local {
                    name: param.name.clone(),
                    accesses: Vec::new(),
                    mut_span: param.mut_span,
                    ty: param.ty.inner,
                })
                .collect(),
            inferred_type: None,
            expected_type: Some(
                self.tir.functions[func_index as usize]
                    .result
                    .clone()
                    .map(|ty| ty.inner)
                    .unwrap_or(TypeIndex::UNIT),
            ),
        };

        let mut ctx = FunctionContext {
            func_index,
            stack: StackFrame {
                scopes: vec![root_scope],
            },
            scope_index: 0 as ScopeIndex,
            lookup,
            resolve_context,
        };
        let block = self.build_block_expression(&mut ctx, &block)?;

        Ok(FunctionBody {
            block: Box::new(block),
            stack: ctx.stack,
        })
    }

    fn build_block_expression(
        &mut self,
        ctx: &mut FunctionContext,
        block: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let statements = match &block.inner {
            ast::Expression::Block { statements } => statements,
            _ => unreachable!(),
        };

        let (statements, result) = match statements.split_last() {
            Some((last, rest)) if last.separator.is_none() => match &last.inner.inner {
                ast::Statement::Expression(expr) => (rest, Some(expr.as_ref())),
                _ => (statements.as_ref(), None),
            },
            _ => (statements.as_ref(), None),
        };

        let expressions = match self.build_block_statements(ctx, statements) {
            BlockState::Exhaustive(expressions) => {
                self.report_local_warnings(
                    ctx.resolve_context.file_id,
                    &ctx.stack.scopes[ctx.scope_index as usize],
                );
                let unreachable_start = statements
                    .get(expressions.len())
                    .map(|s| s.inner.span.start)
                    .or_else(|| result.as_ref().map(|r| r.span.start));

                let unreachable_end = result
                    .map(|r| r.span.end)
                    .or_else(|| statements.last().map(|s| s.inner.span.end));

                if let (Some(start), Some(end)) = (unreachable_start, unreachable_end) {
                    self.tir
                        .diagnostics
                        .push(report_unreachable_code(SourceSpan::new(
                            ctx.resolve_context.file_id,
                            TextSpan::new(start, end),
                        )));
                }

                let scope = &mut ctx.stack.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.unwrap_or(TypeIndex::NEVER);
                scope.inferred_type = Some(inferred_type);

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
                            expected_type: Some(TypeIndex::UNIT),
                            access_kind: AccessKind::Read,
                        },
                        &result,
                    )?),
                    None => None,
                };

                self.report_local_warnings(
                    ctx.resolve_context.file_id,
                    &ctx.stack.scopes[ctx.scope_index as usize],
                );

                Ok(Expression {
                    kind: ExprKind::Block {
                        scope_index: ctx.scope_index,
                        expressions,
                        result: result.map(Box::new),
                    },
                    ty: ctx.stack.scopes[ctx.scope_index as usize]
                        .inferred_type
                        .unwrap_or(TypeIndex::NEVER),
                    span: block.span,
                })
            }
            BlockKind::Block => {
                let result = self.build_block_result(ctx, result.as_deref())?;

                self.report_local_warnings(
                    ctx.resolve_context.file_id,
                    &ctx.stack.scopes[ctx.scope_index as usize],
                );

                let scope = &ctx.stack.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.expect("should have inferred type");
                match scope.expected_type {
                    Some(expected_type) if !self.coercible_to(inferred_type, expected_type) => {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            TypeMistmatchDiagnostic {
                                expected_type,
                                actual_type: inferred_type,
                                span: SourceSpan::new(ctx.resolve_context.file_id, block.span),
                            },
                        ));
                        return Err(());
                    }
                    _ => {}
                }

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
        }
    }

    fn report_local_warnings(&mut self, file_id: FileId, block: &BlockScope) {
        for local in block.locals.iter() {
            if local.accesses.is_empty() && local.ty != TypeIndex::ERROR {
                self.tir
                    .diagnostics
                    .push(report_unused_variable(SourceSpan::new(
                        file_id,
                        local.name.span,
                    )));
            }

            match local.mut_span {
                Some(mut_span)
                    if !local.accesses.iter().any(|access| {
                        access.kind == AccessKind::Write || access.kind == AccessKind::ReadWrite
                    }) =>
                {
                    self.tir
                        .diagnostics
                        .push(report_unnecessary_mutability(SourceSpan::new(
                            file_id, mut_span,
                        )));
                }
                _ => {}
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
        let trait_bounds: Vec<TraitIndex> = at.bounds.to_vec();
        let typeset_bound: Option<TypesetIndex> = at.typeset_bound;

        let assoc_name_str = self.interner.resolve(assoc_name).unwrap_or("?").to_string();

        for bound_trait_index in trait_bounds {
            if !self
                .tir
                .trait_impl_lookup
                .contains_key(&(concrete_ty, bound_trait_index))
            {
                let bound_name = self
                    .interner
                    .resolve(self.tir.traits[bound_trait_index as usize].name.inner)
                    .unwrap()
                    .to_string();
                let type_name = TypeFormatter::new(&self.tir, &self.interner)
                    .display_type(concrete_ty)
                    .unwrap();
                self.tir.diagnostics.push(
                    Diagnostic::error()
                        .with_code(DiagnosticCode::TypeMistmatch.code())
                        .with_message(format!(
                            "associated type `{assoc_name_str}` must implement `{bound_name}`",
                        ))
                        .with_label(Label::primary(file_id, error_span))
                        .with_note(format!("`{type_name}` does not implement `{bound_name}`")),
                );
            }
        }

        if let Some(typeset_index) = typeset_bound {
            if !self.concrete_type_in_typeset(concrete_ty, typeset_index) {
                let ts_name = self
                    .interner
                    .resolve(self.tir.typesets[typeset_index as usize].name.inner)
                    .unwrap_or("?")
                    .to_string();
                let type_name = TypeFormatter::new(&self.tir, &self.interner)
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
                    let supertrait_sym = self.tir.traits[supertrait_index as usize].name.inner;
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
                            .with_code(DiagnosticCode::MissingTraitImplItem.code())
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
                    let trait_name = self.interner.resolve(trait_sym).unwrap_or("?");
                    let supertrait_name = self.interner.resolve(supertrait_sym).unwrap_or("?");
                    self.tir.diagnostics.push(
                        Diagnostic::error()
                            .with_code(DiagnosticCode::MissingSupertraitImpl.code())
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

        for function in self.tir.functions.iter() {
            if function.accesses.is_empty()
                && function.pub_span.is_none()
                && !self.tir.is_import_namespace(function.namespace)
                && !matches!(function.kind, FunctionKind::Trait | FunctionKind::Intrinsic)
            {
                let name = self.interner.resolve(function.name.inner).unwrap();
                self.tir.diagnostics.push(
                    Diagnostic::warning()
                        .with_code(code)
                        .with_message(format!("function `{}` is never used", name))
                        .with_label(
                            SourceSpan::new(function.file_id, function.name.span).primary_label(),
                        ),
                );
            }
        }

        for global in self.tir.globals.iter() {
            if global.accesses.is_empty() && !self.tir.is_import_namespace(global.namespace) {
                let name = self.interner.resolve(global.name.inner).unwrap();
                self.tir.diagnostics.push(
                    Diagnostic::warning()
                        .with_code(code)
                        .with_message(format!("global variable `{}` is never used", name))
                        .with_label(
                            SourceSpan::new(global.file_id, global.name.span).primary_label(),
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
                            SourceSpan::new(constant.file_id, constant.name.span).primary_label(),
                        ),
                );
            }
        }

        for struct_ in self.tir.structs.iter() {
            if struct_.pub_span.is_none() && struct_.accesses.is_empty() {
                let name = self.interner.resolve(struct_.name.inner).unwrap();
                self.tir.diagnostics.push(
                    Diagnostic::warning()
                        .with_code(code)
                        .with_message(format!("struct `{}` is never used", name))
                        .with_label(
                            SourceSpan::new(struct_.file_id, struct_.name.span).primary_label(),
                        ),
                );
            }
        }
    }

    fn build_block_result(
        &mut self,
        ctx: &mut FunctionContext,
        result: Option<&Spanned<ast::Expression>>,
    ) -> Result<Option<Expression>, ()> {
        match result {
            Some(result) => {
                let mut result = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: ctx.stack.scopes[ctx.scope_index as usize].expected_type,
                        access_kind: AccessKind::Read,
                    },
                    result,
                )?;

                let scope = &mut ctx.stack.scopes[ctx.scope_index as usize];
                let inferred_type =
                    self.infer_block_type(ctx.resolve_context.file_id, scope, &result)?;
                scope.inferred_type = Some(inferred_type);
                if result.ty.is_comptime_number() {
                    _ = self.coerce_untyped_expr(
                        ctx.resolve_context.file_id,
                        &mut result,
                        inferred_type,
                    );
                }

                Ok(Some(result))
            }
            None => {
                let scope = &mut ctx.stack.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.unwrap_or(TypeIndex::UNIT);
                scope.inferred_type = Some(inferred_type);

                Ok(None)
            }
        }
    }

    fn build_block_statements(
        &mut self,
        ctx: &mut FunctionContext,
        statements: &[Separated<Spanned<ast::Statement>>],
    ) -> BlockState<Box<[Expression]>> {
        let mut expressions = Vec::with_capacity(statements.len());
        for stmt in statements.iter() {
            let expr = match self.build_statement(ctx, &stmt) {
                Ok(expr) => expr,
                Err(_) => continue,
            };

            match expr.ty {
                _ if expr.ty == TypeIndex::NEVER => {
                    expressions.push(expr);
                    return BlockState::Exhaustive(expressions.into_boxed_slice());
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
            match scope.inferred_type.or(scope.expected_type) {
                Some(ty) => return Ok(ty),
                None => {
                    self.tir
                        .diagnostics
                        .push(report_type_annotation_required(SourceSpan::new(
                            file_id, value.span,
                        )));
                    return Err(());
                }
            }
        }
        let result_type = value.ty;
        match scope.inferred_type {
            Some(inferred_type) if !self.coercible_to(result_type, inferred_type) => {
                self.tir.diagnostics.push(report_type_mistmatch(
                    TypeFormatter::new(&self.tir, &self.interner),
                    TypeMistmatchDiagnostic {
                        expected_type: inferred_type,
                        actual_type: result_type,
                        span: SourceSpan::new(file_id, value.span),
                    },
                ));
                Ok(inferred_type)
            }
            Some(inferred) => Ok(inferred),
            None => match scope.expected_type {
                Some(expected_type) if !self.coercible_to(result_type, expected_type) => {
                    self.tir.diagnostics.push(report_type_mistmatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        TypeMistmatchDiagnostic {
                            expected_type,
                            actual_type: result_type,
                            span: SourceSpan::new(file_id, value.span),
                        },
                    ));
                    Err(())
                }
                _ => Ok(result_type),
            },
        }
    }

    fn build_statement(
        &mut self,
        ctx: &mut FunctionContext,
        statement: &Separated<Spanned<ast::Statement>>,
    ) -> Result<Expression, ()> {
        match &statement.inner.inner {
            ast::Statement::Expression(_) => {
                self.build_expression_statement(ctx, &statement.inner.inner)
            }
            ast::Statement::LocalDefinition { .. } => {
                self.build_local_definition_statement(ctx, statement)
            }
        }
    }

    fn build_expression_statement(
        &mut self,
        ctx: &mut FunctionContext,
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
                expected_type: None,
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
            let scope = ctx.stack.scopes.get_mut(ctx.scope_index as usize).unwrap();
            scope.inferred_type = scope.inferred_type.or(Some(TypeIndex::NEVER));
            return Ok(value);
        } else if value.ty.is_comptime_number() {
            self.tir
                .diagnostics
                .push(report_type_annotation_required(SourceSpan::new(
                    ctx.resolve_context.file_id,
                    value.span,
                )));
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
        func_ctx: &mut FunctionContext,
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
            ast::Expression::Error => Ok(Expression {
                kind: ExprKind::Error,
                ty: TypeIndex::ERROR,
                span: expr.span,
            }),
            ast::Expression::String { symbol } => {
                let unescaped = unescape_string(self.interner.resolve(*symbol).unwrap());
                let symbol = self.interner.get_or_intern(&unescaped);
                let memory_ty = self.resolve_ambient_memory(SourceSpan::new(
                    func_ctx.resolve_context.file_id,
                    expr.span,
                ))?;
                Ok(Expression {
                    kind: ExprKind::String { symbol },
                    ty: self.intern_type(Type::Slice {
                        of: TypeIndex::U8,
                        mutable: false,
                        memory: memory_ty,
                    }),
                    span: expr.span,
                })
            }
            ast::Expression::Char { symbol } => {
                let raw = self.interner.resolve(*symbol).unwrap();
                match parse_char_literal(raw) {
                    Ok(value) => Ok(Expression {
                        kind: ExprKind::Char { value },
                        ty: TypeIndex::CHAR,
                        span: expr.span,
                    }),
                    Err(CharLiteralError::Empty) => {
                        self.tir
                            .diagnostics
                            .push(report_empty_char_literal(SourceSpan::new(
                                func_ctx.resolve_context.file_id,
                                expr.span,
                            )));
                        Err(())
                    }
                    Err(CharLiteralError::TooLong) => {
                        self.tir
                            .diagnostics
                            .push(report_char_literal_too_long(SourceSpan::new(
                                func_ctx.resolve_context.file_id,
                                expr.span,
                            )));
                        Err(())
                    }
                }
            }
            ast::Expression::Path(path) => {
                self.build_path_expression(func_ctx, access_ctx, path, expr.span)
            }
            ast::Expression::Binary { .. } => {
                self.build_binary_expression(func_ctx, access_ctx, expr)
            }
            ast::Expression::Grouping { value } => {
                self.build_expression(func_ctx, access_ctx, value)
            }
            ast::Expression::Unary { .. } => {
                self.build_unary_expression(func_ctx, access_ctx, expr)
            }
            ast::Expression::Call { .. } => self.build_call_expression(func_ctx, access_ctx, expr),
            ast::Expression::MethodCall(_) => {
                self.build_method_call_expression(func_ctx, access_ctx, expr)
            }
            ast::Expression::ObjectAccess { object, member } => self
                .build_object_access_expression(
                    func_ctx,
                    access_ctx,
                    object,
                    member.clone(),
                    expr.span,
                ),
            ast::Expression::Deref { pointer } => {
                self.build_deref_expression(func_ctx, expr.span, pointer)
            }
            ast::Expression::Return { .. } => self.build_return_expression(func_ctx, expr),
            ast::Expression::Block { .. } => func_ctx.enter_block(
                BlockScope {
                    label: None,
                    kind: BlockKind::Block,
                    parent: Some(func_ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
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
            ast::Expression::Cast { .. } => self.build_cast_expression(func_ctx, access_ctx, expr),
            ast::Expression::Break { .. } => self.build_break_expression(func_ctx, expr),
            ast::Expression::Continue { .. } => self.build_continue_expression(func_ctx, expr),
            ast::Expression::Label { .. } => {
                self.build_label_expression(func_ctx, access_ctx, expr)
            }
            ast::Expression::StructInit { path, fields } => {
                self.build_struct_init_expression(func_ctx, access_ctx, expr.span, &path, &fields)
            }
            ast::Expression::Tuple { elements } => {
                self.build_tuple_expression(func_ctx, expr.span, elements, access_ctx)
            }
            ast::Expression::TypeApplication { callee, args } => {
                self.build_type_application_expression(func_ctx, callee, args, expr.span)
            }
            ast::Expression::IntrinsicCall { .. } => {
                self.build_intrinsic_call_expression(func_ctx, access_ctx, expr)
            }
            ast::Expression::ArrayList { elements } => {
                self.build_array_literal_expression(func_ctx, access_ctx, expr.span, elements)
            }
            ast::Expression::ArrayRepeat { value, count } => {
                self.build_array_repeat_expression(func_ctx, access_ctx, expr.span, value, count)
            }
            ast::Expression::Index { object, index } => {
                self.build_index_expression(func_ctx, expr.span, object, index)
            }
            ast::Expression::SliceRange { object, start, end } => {
                self.build_slice_range_expression(func_ctx, expr.span, object, start, end)
            }
        }
    }

    fn build_intrinsic_call_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (name, ast_type_args, arguments) = match &expr.inner {
            ast::Expression::IntrinsicCall {
                name,
                type_args,
                arguments,
            } => (name, type_args, arguments),
            _ => unreachable!(),
        };

        let func_index =
            match self
                .lookup_symbol(
                    func_ctx.resolve_context.namespace,
                    (SymbolNamespace::Value, name.inner),
                )
                .cloned()
            {
                Some(SymbolKind::Pending(def_id)) => {
                    self.ensure_signature(def_id);
                    match self
                        .lookup_symbol(
                            func_ctx.resolve_context.namespace,
                            (SymbolNamespace::Value, name.inner),
                        )
                        .cloned()
                    {
                        Some(SymbolKind::Function { func_index }) => func_index,
                        _ => {
                            self.tir.diagnostics.push(report_undeclared_identifier(
                                SourceSpan::new(func_ctx.resolve_context.file_id, name.span),
                            ));
                            return Err(());
                        }
                    }
                }
                Some(SymbolKind::Function { func_index }) => func_index,
                _ => {
                    self.tir
                        .diagnostics
                        .push(report_undeclared_identifier(SourceSpan::new(
                            func_ctx.resolve_context.file_id,
                            name.span,
                        )));
                    return Err(());
                }
            };

        let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
        self.tir.functions[func_index as usize]
            .accesses
            .push(FunctionAccess {
                caller: Some(caller_id),
                kind: FunctionAccessKind::DirectCall,
                file_id: func_ctx.resolve_context.file_id,
                span: name.span,
            });

        let mut args: Box<[Expression]> = arguments
            .iter()
            .map(|argument| {
                self.build_expression(
                    func_ctx,
                    AccessContext {
                        expected_type: None,
                        access_kind: AccessKind::Read,
                    },
                    &argument.inner,
                )
            })
            .collect::<Result<Box<_>, _>>()?;

        let explicit_type_arguments = match ast_type_args.len() {
            0 => None,
            _ => Some(
                ast_type_args
                    .iter()
                    .map(|type_arg| self.resolve_type(&func_ctx.resolve_context, type_arg))
                    .collect::<Box<_>>(),
            ),
        };

        // TODO: validate the number of specified generics and bounds

        let type_args = self.build_generic_call_arguments(
            func_ctx.resolve_context.file_id,
            func_index,
            &mut args,
            explicit_type_arguments.as_deref(),
            access_ctx.expected_type,
        )?;

        let result_type = self.tir.functions[func_index as usize]
            .result
            .as_ref()
            .map(|r| r.inner)
            .unwrap_or(TypeIndex::UNIT);
        let return_ty = self.substitute_type(result_type, &type_args);

        Ok(Expression {
            kind: ExprKind::IntrinsicCall {
                name: name.inner,
                type_args,
                arguments: args,
            },
            ty: return_ty,
            span: expr.span,
        })
    }

    /// O(1) lookup in the `generic_impl_dispatch` index, then type-arg
    /// extraction via the existing `infer_type_args_from_types`.  Returns the
    /// block index and the concrete substitution for each type param.
    fn find_generic_impl(
        &mut self,
        receiver_ty: TypeIndex,
        member_name: SymbolU32,
    ) -> Option<(usize, Box<[TypeIndex]>)> {
        let outer_kind =
            GenericImplTargetKind::from_type(&self.tir.type_pool[receiver_ty.as_usize()])?;
        let &block_idx = self
            .tir
            .generic_impl_dispatch
            .get(&(outer_kind, member_name))?;
        let (block_target, type_params_len) = {
            let block = &self.tir.generic_impl_list[block_idx];
            (block.target, block.type_params.len())
        };
        let mut type_args = vec![TypeIndex::ERROR; type_params_len];
        self.infer_type_args(&mut type_args, block_target, receiver_ty);
        Some((block_idx, type_args.into_boxed_slice()))
    }

    fn build_object_access_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        object: &Spanned<ast::Expression>,
        member: Spanned<SymbolU32>,
        expr_span: TextSpan,
    ) -> Result<Expression, ()> {
        let object = self.build_expression(func_ctx, access_ctx, object)?;
        // Trait-typed receivers appear only in default method bodies where
        // `self` has type `Type::Trait { .. }`.  impl_members holds concrete
        // dispatch entries only, so look up in Trait::members directly instead.
        // TypeParam receivers use their bounds the same way.
        let entry = match &self.tir.type_pool[object.ty.as_usize()] {
            Type::Trait { trait_index } => self.tir.traits[*trait_index as usize]
                .members
                .get(&member.inner)
                .cloned(),
            Type::TypeParam { .. } => self.type_param_bounds(object.ty).iter().find_map(|&ti| {
                self.tir.traits[ti as usize]
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
            Some(ImplEntry::Method(func_index)) => {
                let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                let func = &mut self.tir.functions[func_index as usize];
                func.accesses.push(FunctionAccess {
                    caller: Some(caller_id),
                    kind: FunctionAccessKind::Reference,
                    file_id: func_ctx.resolve_context.file_id,
                    span: member.span,
                });
                let ty = func.signature_index;
                return Ok(Expression {
                    kind: ExprKind::ObjectAccess {
                        object: Box::new(object),
                        member: member.clone(),
                    },
                    ty,
                    span: expr_span,
                });
            }
            Some(ImplEntry::AssociatedFn(_)) => {
                // TODO: emit "this is an associated function, use Type::name()
                // syntax" diagnostic
            }
            Some(ImplEntry::AssociatedConst { ty, .. }) => {
                return Ok(Expression {
                    kind: ExprKind::ObjectAccess {
                        object: Box::new(object),
                        member: member.clone(),
                    },
                    ty,
                    span: expr_span,
                });
            }
            // Associated types are type-level constructs, not value members.
            Some(ImplEntry::AssociatedType { .. }) | None => {}
        }

        // Check struct fields
        if let Type::Struct { struct_index, args } =
            self.tir.type_pool[object.ty.as_usize()].clone()
        {
            if let Some(&field_index) = self.tir.structs[struct_index as usize]
                .lookup
                .get(&member.inner)
            {
                let raw_field_ty = self.tir.structs[struct_index as usize].fields[field_index]
                    .ty
                    .inner;
                let field_ty = if args.is_empty() {
                    raw_field_ty
                } else {
                    self.substitute_type(raw_field_ty, &args)
                };
                return Ok(Expression {
                    kind: ExprKind::ObjectAccess {
                        object: Box::new(object),
                        member: member.clone(),
                    },
                    ty: field_ty,
                    span: expr_span,
                });
            }
        }

        // Fallback: check generic impl blocks.
        if let Some((block_idx, _)) = self.find_generic_impl(object.ty, member.inner) {
            if let Some(ImplEntry::Method(func_index)) = self.tir.generic_impl_list[block_idx]
                .members
                .get(&member.inner)
                .cloned()
            {
                let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                self.tir.functions[func_index as usize]
                    .accesses
                    .push(FunctionAccess {
                        caller: Some(caller_id),
                        kind: FunctionAccessKind::Reference,
                        file_id: func_ctx.resolve_context.file_id,
                        span: member.span,
                    });
                let ty = self.tir.functions[func_index as usize].signature_index;
                return Ok(Expression {
                    kind: ExprKind::ObjectAccess {
                        object: Box::new(object),
                        member: member.clone(),
                    },
                    ty,
                    span: expr_span,
                });
            }
        }

        self.tir
            .diagnostics
            .push(report_undeclared_identifier(SourceSpan::new(
                func_ctx.resolve_context.file_id,
                member.span,
            )));
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
        func_ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        path: &ast::Path,
        expr_span: TextSpan,
    ) -> Result<Expression, ()> {
        let last = path.segments.last().expect("path is non-empty");

        // ── single-segment, no type args: plain identifier / local / global ───
        if path.segments.len() == 1 && last.type_args.is_empty() {
            let symbol = last.ident.inner;

            if let Some((scope_index, local_index)) = func_ctx.resolve_local(symbol) {
                let local = func_ctx
                    .stack
                    .get_mut_local(scope_index, local_index)
                    .unwrap();
                local.accesses.push(LocalAccess {
                    kind: access_ctx.access_kind,
                    span: expr_span,
                });
                return Ok(Expression {
                    kind: ExprKind::Local {
                        local_index,
                        scope_index,
                    },
                    ty: local.ty,
                    span: expr_span,
                });
            }

            return match self
                .lookup_symbol(
                    func_ctx.resolve_context.namespace,
                    (SymbolNamespace::Value, symbol),
                )
                .filter(|k| !matches!(k, SymbolKind::Pending(_)))
                .cloned()
            {
                Some(SymbolKind::True) => Ok(Expression {
                    kind: ExprKind::Bool { value: true },
                    ty: TypeIndex::BOOL,
                    span: expr_span,
                }),
                Some(SymbolKind::False) => Ok(Expression {
                    kind: ExprKind::Bool { value: false },
                    ty: TypeIndex::BOOL,
                    span: expr_span,
                }),
                Some(SymbolKind::Placeholder) => Ok(Expression {
                    kind: ExprKind::Placeholder,
                    ty: access_ctx.expected_type.unwrap_or(TypeIndex::ERROR),
                    span: expr_span,
                }),
                Some(SymbolKind::Function { func_index }) => {
                    let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                    let func = &mut self.tir.functions[func_index as usize];
                    func.accesses.push(FunctionAccess {
                        caller: Some(caller_id),
                        kind: FunctionAccessKind::Reference,
                        file_id: func_ctx.resolve_context.file_id,
                        span: expr_span,
                    });
                    let func_id = func.id;
                    let ty = self.intern_type(Type::FunctionItem {
                        id: func_id,
                        type_args: Box::new([]),
                    });
                    Ok(Expression {
                        kind: ExprKind::Function { id: func_id },
                        ty,
                        span: expr_span,
                    })
                }
                Some(SymbolKind::Global { global_index }) => {
                    let global = &mut self.tir.globals[global_index as usize];
                    global
                        .accesses
                        .push(SourceSpan::new(func_ctx.resolve_context.file_id, expr_span));
                    Ok(Expression {
                        kind: ExprKind::Global { id: global.id },
                        ty: global.ty.inner,
                        span: expr_span,
                    })
                }
                Some(SymbolKind::Const { const_index }) => {
                    let constant = &mut self.tir.constants[const_index as usize];
                    constant
                        .accesses
                        .push(SourceSpan::new(func_ctx.resolve_context.file_id, expr_span));
                    let id = constant.id;
                    let ty = constant.ty.inner;
                    Ok(Expression {
                        kind: ExprKind::Const { id },
                        ty,
                        span: expr_span,
                    })
                }
                Some(SymbolKind::Memory { memory_index, kind }) => {
                    let id = self.tir.memories[memory_index as usize].id;
                    let ty = self.intern_type(Type::Memory { kind, id });
                    Ok(Expression {
                        kind: ExprKind::Memory { id },
                        ty,
                        span: expr_span,
                    })
                }
                Some(
                    SymbolKind::Enum { .. }
                    | SymbolKind::Module { .. }
                    | SymbolKind::Struct { .. }
                    | SymbolKind::Trait { .. }
                    | SymbolKind::TypeSet { .. },
                ) => {
                    self.tir
                        .diagnostics
                        .push(report_namespace_used_as_value(SourceSpan::new(
                            func_ctx.resolve_context.file_id,
                            expr_span,
                        )));
                    Ok(Expression {
                        kind: ExprKind::Error,
                        ty: TypeIndex::ERROR,
                        span: expr_span,
                    })
                }
                Some(
                    SymbolKind::Unreachable
                    | SymbolKind::TraitAssocType { .. }
                    | SymbolKind::Pending(_),
                ) => {
                    unreachable!()
                }
                None => {
                    self.tir
                        .diagnostics
                        .push(report_undeclared_identifier(SourceSpan::new(
                            func_ctx.resolve_context.file_id,
                            expr_span,
                        )));
                    Ok(Expression {
                        kind: ExprKind::Error,
                        ty: access_ctx.expected_type.unwrap_or(TypeIndex::ERROR),
                        span: expr_span,
                    })
                }
            };
        }

        // ── single-segment with type args: generic function reference ──────────
        if path.segments.len() == 1 {
            let seg = &path.segments[0];
            let func_index = match self
                .lookup_symbol(
                    func_ctx.resolve_context.namespace,
                    (SymbolNamespace::Value, seg.ident.inner),
                )
                .filter(|k| !matches!(k, SymbolKind::Pending(_)))
                .cloned()
            {
                Some(SymbolKind::Function { func_index }) => func_index,
                _ => {
                    self.tir
                        .diagnostics
                        .push(report_undeclared_identifier(SourceSpan::new(
                            func_ctx.resolve_context.file_id,
                            expr_span,
                        )));
                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: TypeIndex::ERROR,
                        span: expr_span,
                    });
                }
            };

            let type_params_len = self.tir.functions[func_index as usize].type_params.len();
            if type_params_len == 0 {
                self.tir.diagnostics.push(
                    Diagnostic::error()
                        .with_code(DiagnosticCode::TypeArgCountMismatch.code())
                        .with_message("function is not generic")
                        .with_label(SourceSpan::new(func_ctx.resolve_context.file_id, expr_span)
                            .primary_label()
                            .with_message("type arguments provided but this function has no type parameters")),
                );
                return Ok(Expression {
                    kind: ExprKind::Error,
                    ty: TypeIndex::ERROR,
                    span: expr_span,
                });
            }
            if seg.type_args.len() != type_params_len {
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
                            SourceSpan::new(func_ctx.resolve_context.file_id, expr_span)
                                .primary_label()
                                .with_message("wrong number of type arguments"),
                        ),
                );
                return Ok(Expression {
                    kind: ExprKind::Error,
                    ty: TypeIndex::ERROR,
                    span: expr_span,
                });
            }

            let resolve_context = func_ctx.resolve_context.clone();
            let resolved_args: Box<[TypeIndex]> = seg
                .type_args
                .iter()
                .map(|arg| self.resolve_type(&resolve_context, arg))
                .collect();

            let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
            let func = &mut self.tir.functions[func_index as usize];
            func.accesses.push(FunctionAccess {
                caller: Some(caller_id),
                kind: FunctionAccessKind::Reference,
                file_id: func_ctx.resolve_context.file_id,
                span: seg.ident.span,
            });
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
        let resolve_context = func_ctx.resolve_context.clone();
        let first = &path.segments[0];
        let mut namespace_ty =
            self.resolve_type_identifier(&resolve_context, first.ident.inner, first.ident.span);
        if namespace_ty == TypeIndex::ERROR {
            return Err(());
        }
        let mut namespace_span = first.ident.span;

        for seg in &path.segments[1..path.segments.len() - 1] {
            namespace_span = TextSpan::new(namespace_span.start, seg.ident.span.end);
            // Resolve `seg` as a member of the current namespace.
            // We reuse the namespace-member logic but we need a TypeIndex result,
            // not an expression.  For intermediate segments this is always a
            // type-namespace lookup inside the current namespace type.
            namespace_ty = self.resolve_namespace_type_member(
                &resolve_context,
                namespace_ty,
                seg.ident.inner,
                seg.ident.span,
            );
            if namespace_ty == TypeIndex::ERROR {
                return Err(());
            }
        }

        self.build_namespace_member_expression(
            func_ctx,
            namespace_ty,
            namespace_span,
            last.ident.clone(),
            expr_span,
        )
    }

    /// Resolve a type-namespace member (used when walking intermediate path
    /// segments): given a resolved namespace `TypeIndex`, look up `member_sym`
    /// as a nested namespace and return its `TypeIndex`.
    fn resolve_namespace_type_member(
        &mut self,
        resolve_context: &ResolveContext,
        namespace_ty: TypeIndex,
        member_sym: SymbolU32,
        member_span: TextSpan,
    ) -> TypeIndex {
        match &self.tir.type_pool[namespace_ty.as_usize()].clone() {
            Type::Module { namespace_idx } => {
                let namespace_idx = *namespace_idx;
                let kind = self.tir.namespaces[namespace_idx as usize]
                    .symbols
                    .get(&(SymbolNamespace::Type, member_sym))
                    .cloned();
                match kind {
                    Some(SymbolKind::Pending(def_id)) => {
                        if matches!(
                            self.sig_state.get(&def_id),
                            Some(SigEntry {
                                state: ComputeState::InProgress,
                                ..
                            })
                        ) {
                            self.tir.diagnostics.push(report_cyclic_type_dependency(
                                SourceSpan::new(resolve_context.file_id, member_span),
                            ));
                            return TypeIndex::ERROR;
                        }
                        self.ensure_signature(def_id);
                        match self.tir.namespaces[namespace_idx as usize]
                            .symbols
                            .get(&(SymbolNamespace::Type, member_sym))
                            .cloned()
                        {
                            Some(k) => self.symbol_kind_to_type(k).unwrap_or(TypeIndex::ERROR),
                            None => TypeIndex::ERROR,
                        }
                    }
                    Some(kind) => self.symbol_kind_to_type(kind).unwrap_or_else(|| {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_type(SourceSpan::new(
                                resolve_context.file_id,
                                member_span,
                            )));
                        TypeIndex::ERROR
                    }),
                    None => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_type(SourceSpan::new(
                                resolve_context.file_id,
                                member_span,
                            )));
                        TypeIndex::ERROR
                    }
                }
            }
            Type::TypeParam { param_index, .. } => {
                let param_index = *param_index;
                let bounds = self
                    .type_param_bounds_in_context(resolve_context, namespace_ty)
                    .to_owned();
                for &trait_index in &bounds {
                    let def_ids = self.tir.traits[trait_index as usize].member_def_ids.clone();
                    for def_id in def_ids {
                        self.ensure_signature(def_id);
                    }
                    if let Some(ImplEntry::AssociatedType { .. }) = self.tir.traits
                        [trait_index as usize]
                        .members
                        .get(&member_sym)
                    {
                        if let Some(at) = self.tir.traits[trait_index as usize]
                            .assoc_types
                            .get_mut(&member_sym)
                        {
                            at.accesses
                                .push(SourceSpan::new(resolve_context.file_id, member_span));
                        }
                        return self.intern_type(Type::AssocTypeProjection {
                            trait_index,
                            assoc_name: member_sym,
                            param_index,
                        });
                    }
                }
                self.tir
                    .diagnostics
                    .push(report_undeclared_type(SourceSpan::new(
                        resolve_context.file_id,
                        member_span,
                    )));
                TypeIndex::ERROR
            }
            _ => {
                match self
                    .tir
                    .impl_members
                    .get(&namespace_ty)
                    .and_then(|m| m.get(&member_sym))
                    .cloned()
                {
                    Some(ImplEntry::AssociatedType { ty }) => ty,
                    _ => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_type(SourceSpan::new(
                                resolve_context.file_id,
                                member_span,
                            )));
                        TypeIndex::ERROR
                    }
                }
            }
        }
    }

    /// Core namespace-member dispatch: look up `member` inside a type whose
    /// `TypeIndex` has already been resolved.
    fn build_namespace_member_expression(
        &mut self,
        func_ctx: &FunctionContext,
        namespace_ty: TypeIndex,
        namespace_span: TextSpan,
        member: Spanned<SymbolU32>,
        expr_span: TextSpan,
    ) -> Result<Expression, ()> {
        let namespace_spanned = ast::Spanned {
            inner: namespace_ty,
            span: namespace_span,
        };

        match self
            .tir
            .impl_members
            .get(&namespace_ty)
            .and_then(|m| m.get(&member.inner))
            .cloned()
        {
            Some(ImplEntry::AssociatedConst { id, ty }) => {
                if let Some(ci) = self.tir.const_index_lookup.get(&id).copied() {
                    self.tir.constants[ci as usize]
                        .accesses
                        .push(SourceSpan::new(
                            func_ctx.resolve_context.file_id,
                            member.span,
                        ));
                }
                return Ok(Expression {
                    kind: ExprKind::NamespaceAccess {
                        namespace: namespace_spanned,
                        member: Box::new(Expression {
                            kind: ExprKind::Const { id },
                            ty,
                            span: member.span,
                        }),
                    },
                    ty,
                    span: expr_span,
                });
            }
            Some(ImplEntry::Method(func_index) | ImplEntry::AssociatedFn(func_index)) => {
                let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                let func = &mut self.tir.functions[func_index as usize];
                func.accesses.push(FunctionAccess {
                    caller: Some(caller_id),
                    kind: FunctionAccessKind::Reference,
                    file_id: func_ctx.resolve_context.file_id,
                    span: member.span,
                });
                let func_id = func.id;
                let func_ty = func.signature_index;
                return Ok(Expression {
                    kind: ExprKind::NamespaceAccess {
                        namespace: namespace_spanned,
                        member: Box::new(Expression {
                            kind: ExprKind::Function { id: func_id },
                            ty: func_ty,
                            span: member.span,
                        }),
                    },
                    ty: func_ty,
                    span: expr_span,
                });
            }
            // Associated types resolve at the type level, not the value level.
            // `Type::member` where `member` is an associated type is handled in
            // `resolve_type` as a projection, not here.
            Some(ImplEntry::AssociatedType { .. }) | None => {}
        }

        match &self.tir.type_pool[namespace_ty.as_usize()] {
            Type::Memory { .. } => {
                // The member was not found in impl_members (checked above), so it's unknown.
                self.tir
                    .diagnostics
                    .push(report_undeclared_identifier(SourceSpan::new(
                        func_ctx.resolve_context.file_id,
                        member.span,
                    )));
                Err(())
            }
            Type::Enum { enum_index } => {
                let enum_idx = *enum_index;
                let enum_ty = namespace_ty;
                let enum_ = &self.tir.enums[enum_idx as usize];
                match enum_.lookup.get(&member.inner).copied() {
                    Some(variant_idx) => {
                        self.tir.enums[enum_idx as usize].variants[variant_idx as usize]
                            .accesses
                            .push(SourceSpan::new(
                                func_ctx.resolve_context.file_id,
                                member.span,
                            ));
                        Ok(Expression {
                            kind: ExprKind::NamespaceAccess {
                                namespace: namespace_spanned,
                                member: Box::new(Expression {
                                    kind: ExprKind::EnumVariant {
                                        enum_index: enum_idx,
                                        variant_index: variant_idx,
                                    },
                                    ty: enum_ty,
                                    span: member.span,
                                }),
                            },
                            ty: enum_ty,
                            span: expr_span,
                        })
                    }
                    None => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_identifier(SourceSpan::new(
                                func_ctx.resolve_context.file_id,
                                member.span,
                            )));
                        Err(())
                    }
                }
            }
            Type::Module { namespace_idx } => {
                let namespace_idx = *namespace_idx;
                let access_span = SourceSpan::new(func_ctx.resolve_context.file_id, namespace_span);
                match self.tir.namespaces[namespace_idx as usize].declaration {
                    ModuleDeclarationKind::Module(i) => {
                        self.tir.module_decls[i as usize].accesses.push(access_span)
                    }
                    ModuleDeclarationKind::Import(i) => {
                        self.tir.import_decls[i as usize].accesses.push(access_span)
                    }
                }
                match self.tir.namespaces[namespace_idx as usize]
                    .symbols
                    .get(&(SymbolNamespace::Value, member.inner))
                    .cloned()
                {
                    Some(SymbolKind::Function { func_index }) => {
                        let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                        let func = &mut self.tir.functions[func_index as usize];
                        func.accesses.push(FunctionAccess {
                            caller: Some(caller_id),
                            kind: FunctionAccessKind::Reference,
                            file_id: func_ctx.resolve_context.file_id,
                            span: member.span,
                        });
                        let func_id = func.id;
                        let ty = self.intern_type(Type::FunctionItem {
                            id: func_id,
                            type_args: Box::new([]),
                        });
                        Ok(Expression {
                            kind: ExprKind::NamespaceAccess {
                                namespace: namespace_spanned,
                                member: Box::new(Expression {
                                    kind: ExprKind::Function { id: func_id },
                                    ty,
                                    span: member.span,
                                }),
                            },
                            ty,
                            span: expr_span,
                        })
                    }
                    Some(SymbolKind::Global { global_index }) => {
                        let global = &mut self.tir.globals[global_index as usize];
                        global.accesses.push(SourceSpan::new(
                            func_ctx.resolve_context.file_id,
                            member.span,
                        ));
                        let global_id = global.id;
                        let ty = global.ty.inner;
                        Ok(Expression {
                            kind: ExprKind::NamespaceAccess {
                                namespace: namespace_spanned,
                                member: Box::new(Expression {
                                    kind: ExprKind::Global { id: global_id },
                                    ty,
                                    span: member.span,
                                }),
                            },
                            ty,
                            span: expr_span,
                        })
                    }
                    _ => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_identifier(SourceSpan::new(
                                func_ctx.resolve_context.file_id,
                                member.span,
                            )));
                        Err(())
                    }
                }
            }
            Type::Trait { trait_index } => {
                match self.tir.traits[*trait_index as usize]
                    .members
                    .get(&member.inner)
                    .cloned()
                {
                    Some(ImplEntry::Method(func_index)) => {
                        let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                        self.tir.functions[func_index as usize]
                            .accesses
                            .push(FunctionAccess {
                                caller: Some(caller_id),
                                kind: FunctionAccessKind::Reference,
                                file_id: func_ctx.resolve_context.file_id,
                                span: member.span,
                            });
                        let func_id = self.tir.functions[func_index as usize].id;
                        let ty = self.tir.functions[func_index as usize].signature_index;
                        Ok(Expression {
                            kind: ExprKind::NamespaceAccess {
                                namespace: namespace_spanned,
                                member: Box::new(Expression {
                                    kind: ExprKind::Function { id: func_id },
                                    ty,
                                    span: member.span,
                                }),
                            },
                            ty,
                            span: expr_span,
                        })
                    }
                    Some(ImplEntry::AssociatedConst { id, ty }) => {
                        if let Some(ci) = self.tir.const_index_lookup.get(&id).copied() {
                            self.tir.constants[ci as usize]
                                .accesses
                                .push(SourceSpan::new(
                                    func_ctx.resolve_context.file_id,
                                    member.span,
                                ));
                        }
                        Ok(Expression {
                            kind: ExprKind::NamespaceAccess {
                                namespace: namespace_spanned,
                                member: Box::new(Expression {
                                    kind: ExprKind::Const { id },
                                    ty,
                                    span: member.span,
                                }),
                            },
                            ty,
                            span: expr_span,
                        })
                    }
                    _ => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_identifier(SourceSpan::new(
                                func_ctx.resolve_context.file_id,
                                member.span,
                            )));
                        Err(())
                    }
                }
            }
            _ => {
                let diag = Diagnostic::error()
                    .with_code(DiagnosticCode::NotANamespace.code())
                    .with_message(format!(
                        "type `{}` is not a namespace",
                        TypeFormatter::new(&self.tir, &self.interner,)
                            .display_type(namespace_ty)
                            .unwrap(),
                    ))
                    .with_label(
                        SourceSpan::new(func_ctx.resolve_context.file_id, namespace_span)
                            .primary_label(),
                    );
                self.tir.diagnostics.push(diag);
                Err(())
            }
        }
    }

    fn build_label_expression(
        &mut self,
        ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (label, block) = match &expr.inner {
            ast::Expression::Label { label, block } => (label.clone(), block),
            _ => unreachable!(),
        };

        match block.inner {
            ast::Expression::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: Some(BlockLabel {
                        name: label.inner,
                        span: label.span,
                        accesses: Vec::new(),
                    }),
                    kind: BlockKind::Block,
                    parent: Some(ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
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
        func_ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        expr: &Spanned<ast::Expression>,
        label: Option<Spanned<SymbolU32>>,
    ) -> Result<Expression, ()> {
        let block = match &expr.inner {
            ast::Expression::Loop { block } => block,
            _ => unreachable!(),
        };

        let file_id = func_ctx.resolve_context.file_id;
        func_ctx.enter_block(
            BlockScope {
                label: label.map(|l| BlockLabel {
                    name: l.inner,
                    span: l.span,
                    accesses: Vec::new(),
                }),
                kind: BlockKind::Loop,
                parent: Some(func_ctx.scope_index),
                locals: Vec::new(),
                inferred_type: None,
                expected_type: access_ctx.expected_type,
            },
            |ctx| {
                let block = self.build_block_expression(ctx, block)?;

                let scope = &ctx.stack.scopes[ctx.scope_index as usize];
                match (scope.expected_type, scope.inferred_type) {
                    (Some(expected_type), Some(inferred_type))
                        if !self.coercible_to(inferred_type, expected_type) =>
                    {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            TypeMistmatchDiagnostic {
                                expected_type,
                                actual_type: inferred_type,
                                span: SourceSpan::new(file_id, expr.span),
                            },
                        ));
                        return Err(());
                    }
                    _ => {}
                }

                let ty = block.ty;
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
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let label = match &expr.inner {
            ast::Expression::Continue { label } => label.clone(),
            _ => unreachable!(),
        };

        let scope_index = match label {
            Some(label) => match ctx.resolve_label(label.inner) {
                Some(scope_index) => {
                    if let Some(block_label) = ctx.stack.scopes[scope_index as usize].label.as_mut()
                    {
                        block_label.accesses.push(label.span);
                    }
                    scope_index
                }
                None => {
                    self.tir
                        .diagnostics
                        .push(report_undeclared_label(SourceSpan::new(
                            ctx.resolve_context.file_id,
                            label.span,
                        )));
                    return Err(());
                }
            },
            None => ctx
                .get_closest_loop_block()
                .expect("continue expression must be inside a loop or a block with a label"),
        };

        Ok(Expression {
            kind: ExprKind::Continue { scope_index },
            ty: TypeIndex::NEVER,
            span: expr.span,
        })
    }

    fn build_if_else_expression(
        &mut self,
        ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        expr: &Spanned<ast::Expression>,
        label: Option<Spanned<SymbolU32>>,
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
                expected_type: Some(TypeIndex::BOOL),
                access_kind: AccessKind::Read,
            },
            condition,
        )?;

        let then_block = match then_block.inner {
            ast::Expression::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: label.clone().map(|l| BlockLabel {
                        name: l.inner,
                        span: l.span,
                        accesses: Vec::new(),
                    }),
                    kind: BlockKind::Block,
                    parent: Some(ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
                    expected_type: match maybe_else_block {
                        Some(_) => access_ctx.expected_type,
                        None => None,
                    },
                },
                |ctx| self.build_block_expression(ctx, then_block),
            )?,
            _ => unreachable!(),
        };
        let (else_block, ty) = match maybe_else_block {
            Some(ast_else_block) => {
                let else_block = match ast_else_block.inner {
                    ast::Expression::Block { .. } => ctx.enter_block(
                        BlockScope {
                            label: label.map(|l| BlockLabel {
                                name: l.inner,
                                span: l.span,
                                accesses: Vec::new(),
                            }),
                            kind: BlockKind::Block,
                            parent: Some(ctx.scope_index),
                            locals: Vec::new(),
                            inferred_type: None,
                            expected_type: access_ctx.expected_type,
                        },
                        |ctx| self.build_block_expression(ctx, ast_else_block),
                    )?,
                    _ => unreachable!(),
                };

                match self.unify(then_block.ty, else_block.ty) {
                    Ok(ty) => (Some(else_block), ty),
                    Err(_) => {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
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
                if then_block.ty == TypeIndex::UNIT || then_block.ty == TypeIndex::NEVER {
                    (None, TypeIndex::UNIT)
                } else {
                    panic!(
                        "if you want to return a value from if-else, you must provide an else block"
                    )
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
        ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (value, cast_type) = match &expr.inner {
            ast::Expression::Cast { value, ty } => (value, ty),
            _ => unreachable!(),
        };

        let cast_type = self.resolve_type(&ctx.resolve_context, &cast_type);
        if cast_type == TypeIndex::ERROR {
            return self.build_expression(ctx, access_ctx, value);
        }
        let mut value = self.build_expression(
            ctx,
            AccessContext {
                expected_type: Some(cast_type),
                access_kind: access_ctx.access_kind,
            },
            value,
        )?;
        if value.ty.is_comptime_number() {
            self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut value, cast_type)?;
        } else {
            match (
                self.tir.type_primitive(value.ty),
                self.tir.type_primitive(cast_type),
            ) {
                (Some(x), Some(y)) if x == y => {
                    // TODO: add checks for unsafe/lossy casts like i32 to u8, or u32 to char
                    value.ty = cast_type;
                }
                _ => {
                    self.tir.diagnostics.push(report_invalid_cast(
                        TypeFormatter::new(&self.tir, &self.interner),
                        value.ty,
                        cast_type,
                        SourceSpan::new(ctx.resolve_context.file_id, expr.span),
                    ));
                }
            }
        }

        Ok(value)
    }

    fn build_break_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (label, value) = match &expr.inner {
            ast::Expression::Break { label, value } => (label.clone(), value),
            _ => unreachable!(),
        };

        let scope_index = match label {
            Some(label) => match ctx.resolve_label(label.inner) {
                Some(scope_index) => {
                    if let Some(block_label) = ctx.stack.scopes[scope_index as usize].label.as_mut()
                    {
                        block_label.accesses.push(label.span);
                    }
                    scope_index
                }
                None => {
                    self.tir
                        .diagnostics
                        .push(report_undeclared_label(SourceSpan::new(
                            ctx.resolve_context.file_id,
                            label.span,
                        )));

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
                    self.tir
                        .diagnostics
                        .push(report_break_outside_of_loop(SourceSpan::new(
                            ctx.resolve_context.file_id,
                            expr.span,
                        )));

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
            Some(value) => Ok(self
                .build_expression(
                    ctx,
                    AccessContext {
                        expected_type: ctx
                            .stack
                            .scopes
                            .get(scope_index as usize)
                            .unwrap()
                            .expected_type,
                        access_kind: AccessKind::Read,
                    },
                    value,
                )
                .and_then(|mut value| {
                    let scope = ctx.stack.scopes.get_mut(scope_index as usize).unwrap();
                    let inferred_type =
                        self.infer_block_type(ctx.resolve_context.file_id, scope, &value)?;
                    if value.ty.is_comptime_number() {
                        self.coerce_untyped_expr(
                            ctx.resolve_context.file_id,
                            &mut value,
                            inferred_type,
                        )?;
                    }
                    scope.inferred_type = Some(inferred_type);

                    Ok(Expression {
                        kind: ExprKind::Break {
                            scope_index,
                            value: Some(Box::new(value)),
                        },
                        ty: TypeIndex::NEVER,
                        span: expr.span,
                    })
                })
                .unwrap_or(Expression {
                    kind: ExprKind::Unreachable,
                    ty: TypeIndex::NEVER,
                    span: expr.span,
                })),
            None => {
                let scope = ctx.stack.scopes.get_mut(scope_index as usize).unwrap();
                match scope.inferred_type {
                    Some(inferred) => {
                        if !self.coercible_to(TypeIndex::UNIT, inferred) {
                            let formatter = TypeFormatter::new(&self.tir, &self.interner);
                            self.tir.diagnostics.push(report_type_mistmatch(
                                formatter,
                                TypeMistmatchDiagnostic {
                                    expected_type: inferred,
                                    actual_type: TypeIndex::UNIT,
                                    span: SourceSpan::new(ctx.resolve_context.file_id, expr.span),
                                },
                            ));
                        }
                    }
                    None => {
                        scope.inferred_type = Some(TypeIndex::UNIT);
                    }
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
        func_ctx: &mut FunctionContext,
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
                expected_type: None,
                access_kind: AccessKind::Read,
            },
            callee,
        )?;
        result.span = expr_span;
        Ok(result)
    }

    fn build_binary_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
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
            | ast::BinaryOp::Rem => self.build_arithmetic_expr(func_ctx, expr, access_ctx),
            ast::BinaryOp::Assign => self.build_assignment_expr(func_ctx, expr),
            ast::BinaryOp::AddAssign
            | ast::BinaryOp::SubAssign
            | ast::BinaryOp::MulAssign
            | ast::BinaryOp::DivAssign
            | ast::BinaryOp::RemAssign => self.build_arithmetic_assignment_expr(func_ctx, expr),
            ast::BinaryOp::Eq
            | ast::BinaryOp::NotEq
            | ast::BinaryOp::Less
            | ast::BinaryOp::LessEq
            | ast::BinaryOp::Greater
            | ast::BinaryOp::GreaterEq => self.build_comparison_binary_expr(func_ctx, expr),
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
        ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (operator, ast_operand) = match &expr.inner {
            ast::Expression::Unary { operator, operand } => (operator.clone(), operand),
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
                if operand.ty.is_primitive() || operand.ty.is_comptime_number() {
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
                        ctx.resolve_context.file_id,
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
                    let formatter = TypeFormatter::new(&self.tir, &self.interner);
                    let diagnostic = Diagnostic::error()
                        .with_code(DiagnosticCode::UnaryOperatorCannotBeApplied.code())
                        .with_message(format!(
                            "operator `{}` cannot be applied to type `{}`",
                            operator.inner,
                            formatter.display_type(operand.ty).unwrap()
                        ))
                        .with_label(Label::primary(ctx.resolve_context.file_id, operand.span))
                        .with_label(Label::secondary(ctx.resolve_context.file_id, operator.span));

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
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.inner {
            ast::Expression::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let left = self.build_expression(
            ctx,
            AccessContext {
                expected_type: Some(TypeIndex::BOOL),
                access_kind: AccessKind::Read,
            },
            left,
        )?;
        if left.ty == TypeIndex::ERROR {
            // Error already reported
        } else if left.ty.is_comptime_number() {
            self.tir
                .diagnostics
                .push(report_type_annotation_required(SourceSpan::new(
                    ctx.resolve_context.file_id,
                    left.span,
                )));
        } else if left.ty != TypeIndex::BOOL {
            self.tir.diagnostics.push(report_type_mistmatch(
                TypeFormatter::new(&self.tir, &self.interner),
                TypeMistmatchDiagnostic {
                    expected_type: TypeIndex::BOOL,
                    actual_type: left.ty,
                    span: SourceSpan::new(ctx.resolve_context.file_id, left.span),
                },
            ));
        }
        let right = self.build_expression(
            ctx,
            AccessContext {
                expected_type: Some(TypeIndex::BOOL),
                access_kind: AccessKind::Read,
            },
            right,
        )?;
        if right.ty == TypeIndex::ERROR {
            // Error already reported
        } else if right.ty.is_comptime_number() {
            self.tir
                .diagnostics
                .push(report_type_annotation_required(SourceSpan::new(
                    ctx.resolve_context.file_id,
                    right.span,
                )));
        } else if right.ty != TypeIndex::BOOL {
            self.tir.diagnostics.push(report_type_mistmatch(
                TypeFormatter::new(&self.tir, &self.interner),
                TypeMistmatchDiagnostic {
                    expected_type: TypeIndex::BOOL,
                    actual_type: right.ty,
                    span: SourceSpan::new(ctx.resolve_context.file_id, right.span),
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
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.inner {
            ast::Expression::Binary {
                left,
                right,
                operator,
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let mut left = self.build_expression(ctx, access_ctx.clone(), left)?;
        let mut right = self.build_expression(
            ctx,
            AccessContext {
                expected_type: match &self.tir.type_pool[left.ty.as_usize()] {
                    Type::Integer | Type::Float | Type::Error | Type::Never | Type::Unit => {
                        access_ctx.expected_type
                    }
                    _ => Some(left.ty),
                },
                access_kind: access_ctx.access_kind,
            },
            right,
        )?;

        match (left.ty, right.ty) {
            // Allow operations with Error type (error already reported elsewhere)
            (l, r) if l == TypeIndex::ERROR || r == TypeIndex::ERROR => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: access_ctx.expected_type.unwrap_or(TypeIndex::ERROR),
                span: expr.span,
            }),
            (l, r) if l.is_comptime_number() && r.is_comptime_number() => {
                match access_ctx.expected_type {
                    Some(expected_type) => {
                        self.coerce_untyped_expr(
                            ctx.resolve_context.file_id,
                            &mut left,
                            expected_type,
                        )?;
                        self.coerce_untyped_expr(
                            ctx.resolve_context.file_id,
                            &mut right,
                            expected_type,
                        )?;

                        if !expected_type.is_integer() && expected_type != TypeIndex::BOOL {
                            self.tir
                                .diagnostics
                                .push(report_binary_operator_cannot_be_applied(
                                    TypeFormatter::new(&self.tir, &self.interner),
                                    BinaryOperatorCannotBeAppliedDiagnostic {
                                        file_id: ctx.resolve_context.file_id,
                                        operator: operator.clone(),
                                        operand: Spanned {
                                            inner: expected_type,
                                            span: left.span,
                                        },
                                    },
                                ));
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
                    }
                    None => {
                        self.tir.diagnostics.push(report_type_annotation_required(
                            SourceSpan::new(ctx.resolve_context.file_id, expr.span),
                        ));
                        Err(())
                    }
                }
            }
            (l, right_type) if l.is_comptime_number() => {
                if !right_type.is_integer() && right_type != TypeIndex::BOOL {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                operator: operator.clone(),
                                operand: Spanned {
                                    inner: right_type,
                                    span: right.span,
                                },
                            },
                        ));
                }
                self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut left, right_type)?;

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
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                operator: operator.clone(),
                                operand: Spanned {
                                    inner: left_type,
                                    span: left.span,
                                },
                            },
                        ));
                }
                self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut right, left_type)?;

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
                    && (left_type.is_integer() || left_type == TypeIndex::BOOL) =>
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
                        TypeFormatter::new(&self.tir, &self.interner),
                        BinaryExpressionMistmatchDiagnostic {
                            file_id: ctx.resolve_context.file_id,
                            left_type: Spanned {
                                inner: left_type,
                                span: left.span,
                            },
                            operator: operator.clone(),
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
                    ty: access_ctx.expected_type.unwrap_or(TypeIndex::ERROR),
                    span: expr.span,
                })
            }
        }
    }

    fn build_comparison_binary_expr(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.inner {
            ast::Expression::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let mut left = self.build_expression(
            ctx,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
            left,
        )?;
        let mut right = self.build_expression(
            ctx,
            AccessContext {
                expected_type: Some(left.ty),
                access_kind: AccessKind::Read,
            },
            right,
        )?;

        match (left.ty, right.ty) {
            // Allow operations with Error type (error already reported elsewhere)
            (l, r) if l == TypeIndex::ERROR || r == TypeIndex::ERROR => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: TypeIndex::BOOL,
                span: expr.span,
            }),
            (l, r) if l.is_comptime_number() && r.is_comptime_number() => {
                self.tir
                    .diagnostics
                    .push(report_comparison_type_annotation_required(
                        SourceSpan::new(ctx.resolve_context.file_id, left.span),
                        SourceSpan::new(ctx.resolve_context.file_id, right.span),
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
            (l, ty) if l.is_comptime_number() => {
                self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut left, ty)?;

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
                self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut right, ty)?;

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
            (l, r) if l == TypeIndex::BOOL && r == TypeIndex::BOOL => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: TypeIndex::BOOL,
                span: expr.span,
            }),
            (left_type, right_type) if left_type == right_type && left_type.is_primitive() => {
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
                    && matches!(self.tir.type_pool[left_type.as_usize()], Type::Enum { .. }) =>
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
                if matches!(operator.inner, ast::BinaryOp::Eq | ast::BinaryOp::NotEq)
                    && matches!(
                        (
                            &self.tir.type_pool[left_type.as_usize()],
                            &self.tir.type_pool[right_type.as_usize()],
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
                        TypeFormatter::new(&self.tir, &self.interner),
                        BinaryExpressionMistmatchDiagnostic {
                            file_id: ctx.resolve_context.file_id,
                            left_type: Spanned {
                                inner: left_type,
                                span: left.span,
                            },
                            operator: operator.clone(),
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
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.inner {
            ast::Expression::Binary {
                left,
                right,
                operator,
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let left = self.build_expression(
            ctx,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Write,
            },
            left,
        )?;
        match left.kind {
            ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let local =
                    match ctx.stack.get_mut_local(scope_index, local_index) {
                        Some(local) => local,
                        None => {
                            self.tir.diagnostics.push(report_undeclared_identifier(
                                SourceSpan::new(ctx.resolve_context.file_id, left.span),
                            ));
                            return Err(());
                        }
                    };
                match local.mut_span {
                    None => {
                        self.tir
                            .diagnostics
                            .push(report_cannot_mutate_immutable(SourceSpan::new(
                                ctx.resolve_context.file_id,
                                expr.span,
                            )));
                    }
                    _ => {}
                }

                let local_type = local.ty;

                let mut right = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: Some(local_type),
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right.ty.is_comptime_number() {
                    self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut right, local_type)?;
                } else if !self.coercible_to(right.ty, local_type) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                left_type: Spanned {
                                    inner: local_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: right.ty,
                                    span: right.span,
                                },
                            },
                        ));
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
                let global_index = self.tir.global_index_lookup[&id];
                let global = &self.tir.globals[global_index as usize];
                match global.mut_span {
                    None => {
                        self.tir
                            .diagnostics
                            .push(report_cannot_mutate_immutable(SourceSpan::new(
                                ctx.resolve_context.file_id,
                                expr.span,
                            )));
                    }
                    _ => {}
                }

                let global_type = global.ty.inner;
                let mut right = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: Some(global_type),
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right.ty.is_comptime_number() {
                    self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut right, global_type)?;
                } else if !self.coercible_to(right.ty, global_type) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                left_type: Spanned {
                                    inner: global_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: right.ty,
                                    span: right.span,
                                },
                            },
                        ));
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
                        expected_type: None,
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right.ty.is_comptime_number() {
                    self.tir
                        .diagnostics
                        .push(report_type_annotation_required(SourceSpan::new(
                            ctx.resolve_context.file_id,
                            right.span,
                        )));
                    return Err(());
                }
                let right_type = right.ty;

                return Ok(Expression {
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
                });
            }
            ExprKind::Deref { pointer, memory } => {
                let (inner_ty, mutable) = match &self.tir.type_pool[pointer.ty.as_usize()] {
                    Type::Pointer { to, mutable, .. } => (*to, *mutable),
                    _ => unreachable!("Deref ExprKind must have Pointer type"),
                };

                if !mutable {
                    self.tir
                        .diagnostics
                        .push(report_cannot_store_through_immutable_pointer(
                            SourceSpan::new(ctx.resolve_context.file_id, expr.span),
                        ));
                }

                let mut right_expr = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: Some(inner_ty),
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right_expr.ty.is_comptime_number() {
                    self.coerce_untyped_expr(
                        ctx.resolve_context.file_id,
                        &mut right_expr,
                        inner_ty,
                    )?;
                } else if !self.coercible_to(right_expr.ty, inner_ty) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                left_type: Spanned {
                                    inner: inner_ty,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: right_expr.ty,
                                    span: right_expr.span,
                                },
                            },
                        ));
                }

                let left_span = left.span;
                let left_ty = left.ty;
                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(Expression {
                            kind: ExprKind::Deref { pointer, memory },
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
            ExprKind::Index {
                object,
                index,
                memory,
            } => {
                let elem_ty = left.ty;
                let mutable = match &self.tir.type_pool[object.ty.as_usize()] {
                    Type::Array { mutable, .. } | Type::Slice { mutable, .. } => *mutable,
                    _ => unreachable!("Index ExprKind must have Array or Slice object type"),
                };
                if !mutable {
                    self.tir
                        .diagnostics
                        .push(report_cannot_mutate_immutable_array_element(
                            SourceSpan::new(ctx.resolve_context.file_id, expr.span),
                        ));
                }

                let mut right_expr = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: Some(elem_ty),
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right_expr.ty.is_comptime_number() {
                    self.coerce_untyped_expr(
                        ctx.resolve_context.file_id,
                        &mut right_expr,
                        elem_ty,
                    )?;
                } else if !self.coercible_to(right_expr.ty, elem_ty) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                left_type: Spanned {
                                    inner: elem_ty,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: right_expr.ty,
                                    span: right_expr.span,
                                },
                            },
                        ));
                }

                let left_span = left.span;
                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(Expression {
                            kind: ExprKind::Index {
                                object,
                                index,
                                memory,
                            },
                            ty: elem_ty,
                            span: left_span,
                        }),
                        operator,
                        right: Box::new(right_expr),
                    },
                    ty: TypeIndex::UNIT,
                    span: expr.span,
                })
            }
            _ => {
                self.tir
                    .diagnostics
                    .push(report_invalid_assignment_target(SourceSpan::new(
                        ctx.resolve_context.file_id,
                        left.span,
                    )));

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
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.inner {
            ast::Expression::Binary {
                left,
                right,
                operator,
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let left = self.build_expression(
            ctx,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::ReadWrite,
            },
            left,
        )?;
        match left.kind {
            ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let local =
                    match ctx.stack.get_mut_local(scope_index, local_index) {
                        Some(local) => local,
                        None => {
                            self.tir.diagnostics.push(report_undeclared_identifier(
                                SourceSpan::new(ctx.resolve_context.file_id, left.span),
                            ));
                            return Err(());
                        }
                    };
                // Allow operations with Error type (error already reported elsewhere)
                if local.ty == TypeIndex::ERROR {
                    let right = self.build_expression(
                        ctx,
                        AccessContext {
                            expected_type: Some(TypeIndex::ERROR),
                            access_kind: AccessKind::Read,
                        },
                        right,
                    )?;
                    return Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ty: TypeIndex::UNIT,
                        span: expr.span,
                    });
                }
                if !local.ty.is_primitive() {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                operator,
                                operand: Spanned {
                                    inner: local.ty,
                                    span: left.span,
                                },
                            },
                        ));

                    return Err(());
                }
                if local.mut_span == None {
                    self.tir
                        .diagnostics
                        .push(report_cannot_mutate_immutable(SourceSpan::new(
                            ctx.resolve_context.file_id,
                            expr.span,
                        )));
                }

                let local_type = local.ty;
                let mut right = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: Some(local_type),
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right.ty.is_comptime_number() {
                    self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut right, local_type)?;
                } else if !self.coercible_to(right.ty, local_type) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                left_type: Spanned {
                                    inner: local_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: right.ty,
                                    span: right.span,
                                },
                            },
                        ));
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
                let global_index = self.tir.global_index_lookup[&id];
                let global = self.tir.globals.get(global_index as usize).unwrap();

                if !global.ty.inner.is_primitive() {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                operator,
                                operand: Spanned {
                                    inner: global.ty.inner,
                                    span: left.span,
                                },
                            },
                        ));

                    return Err(());
                }

                if global.mut_span == None {
                    self.tir
                        .diagnostics
                        .push(report_cannot_mutate_immutable(SourceSpan::new(
                            ctx.resolve_context.file_id,
                            expr.span,
                        )));
                }

                let global_type = global.ty.inner;
                let mut right = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: Some(global_type),
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right.ty.is_comptime_number() {
                    self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut right, global_type)?;
                } else if !self.coercible_to(right.ty, global_type) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                left_type: Spanned {
                                    inner: global_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: right.ty,
                                    span: right.span,
                                },
                            },
                        ));
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
            ExprKind::Deref { pointer, memory } => {
                let (inner_ty, mutable) = match &self.tir.type_pool[pointer.ty.as_usize()] {
                    Type::Pointer { to, mutable, .. } => (*to, *mutable),
                    _ => unreachable!("Deref ExprKind must have Pointer type"),
                };

                if !inner_ty.is_primitive() {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                operator,
                                operand: Spanned {
                                    inner: inner_ty,
                                    span: left.span,
                                },
                            },
                        ));
                    return Err(());
                }

                if !mutable {
                    self.tir
                        .diagnostics
                        .push(report_cannot_store_through_immutable_pointer(
                            SourceSpan::new(ctx.resolve_context.file_id, expr.span),
                        ));
                }

                let mut right_expr = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: Some(inner_ty),
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right_expr.ty.is_comptime_number() {
                    self.coerce_untyped_expr(
                        ctx.resolve_context.file_id,
                        &mut right_expr,
                        inner_ty,
                    )?;
                } else if !self.coercible_to(right_expr.ty, inner_ty) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                left_type: Spanned {
                                    inner: inner_ty,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: right_expr.ty,
                                    span: right_expr.span,
                                },
                            },
                        ));
                }

                let left_span = left.span;
                let left_ty = left.ty;
                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(Expression {
                            kind: ExprKind::Deref { pointer, memory },
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
            ExprKind::Index {
                object,
                index,
                memory,
            } => {
                let elem_ty = left.ty;
                let mutable = match &self.tir.type_pool[object.ty.as_usize()] {
                    Type::Array { mutable, .. } | Type::Slice { mutable, .. } => *mutable,
                    _ => unreachable!("Index ExprKind must have Array or Slice object type"),
                };
                if !mutable {
                    self.tir
                        .diagnostics
                        .push(report_cannot_mutate_immutable_array_element(
                            SourceSpan::new(ctx.resolve_context.file_id, expr.span),
                        ));
                }
                let mut right_expr = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: Some(elem_ty),
                        access_kind: AccessKind::Read,
                    },
                    right,
                )?;
                if right_expr.ty.is_comptime_number() {
                    self.coerce_untyped_expr(
                        ctx.resolve_context.file_id,
                        &mut right_expr,
                        elem_ty,
                    )?;
                } else if !self.coercible_to(right_expr.ty, elem_ty) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                left_type: Spanned {
                                    inner: elem_ty,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: right_expr.ty,
                                    span: right_expr.span,
                                },
                            },
                        ));
                }
                let left_span = left.span;
                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(Expression {
                            kind: ExprKind::Index {
                                object,
                                index,
                                memory,
                            },
                            ty: elem_ty,
                            span: left_span,
                        }),
                        operator,
                        right: Box::new(right_expr),
                    },
                    ty: TypeIndex::UNIT,
                    span: expr.span,
                })
            }
            _ => {
                self.tir
                    .diagnostics
                    .push(report_invalid_assignment_target(SourceSpan::new(
                        ctx.resolve_context.file_id,
                        left.span,
                    )));

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
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let value = match &expr.inner {
            ast::Expression::Return { value } => value,
            _ => unreachable!(),
        };

        match value {
            Some(value) => Ok(self
                .build_expression(
                    ctx,
                    AccessContext {
                        expected_type: ctx.stack.scopes.get(0).unwrap().expected_type,
                        access_kind: AccessKind::Read,
                    },
                    value,
                )
                .and_then(|mut value| {
                    let scope = ctx.stack.scopes.get_mut(0).unwrap();
                    let inferred_type =
                        self.infer_block_type(ctx.resolve_context.file_id, scope, &value)?;
                    scope.inferred_type = Some(inferred_type);
                    if value.ty.is_comptime_number() {
                        self.coerce_untyped_expr(
                            ctx.resolve_context.file_id,
                            &mut value,
                            inferred_type,
                        )?;
                    }

                    match scope.expected_type {
                        Some(expected_type) if !self.coercible_to(inferred_type, expected_type) => {
                            self.tir.diagnostics.push(report_type_mistmatch(
                                TypeFormatter::new(&self.tir, &self.interner),
                                TypeMistmatchDiagnostic {
                                    expected_type,
                                    actual_type: inferred_type,
                                    span: SourceSpan::new(ctx.resolve_context.file_id, value.span),
                                },
                            ));
                            return Err(());
                        }
                        _ => {}
                    };

                    Ok(Expression {
                        kind: ExprKind::Return {
                            value: Some(Box::new(value)),
                        },
                        ty: TypeIndex::NEVER,
                        span: expr.span,
                    })
                })
                .unwrap_or(Expression {
                    kind: ExprKind::Unreachable,
                    ty: TypeIndex::NEVER,
                    span: expr.span,
                })),
            None => {
                let scope = ctx.stack.scopes.get_mut(ctx.scope_index as usize).unwrap();

                let inferred_type = scope.inferred_type.unwrap_or(TypeIndex::UNIT);
                scope.inferred_type = Some(inferred_type);

                match scope.expected_type {
                    Some(expected_type) if self.coercible_to(inferred_type, expected_type) => {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            TypeMistmatchDiagnostic {
                                expected_type,
                                actual_type: inferred_type,
                                span: SourceSpan::new(ctx.resolve_context.file_id, expr.span),
                            },
                        ));
                        return Err(());
                    }
                    _ => {}
                };

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
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.inner {
            ast::Expression::Binary {
                left,
                right,
                operator,
            } => (left, right, operator.clone()),
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
                expected_type: match &self.tir.type_pool[left.ty.as_usize()] {
                    Type::Integer | Type::Float | Type::Error | Type::Never | Type::Unit => {
                        access_ctx.expected_type
                    }
                    _ => Some(left.ty),
                },
                access_kind: AccessKind::Read,
            },
            right,
        )?;

        match (left.ty, right.ty) {
            (l, r) if l.is_comptime_number() && r.is_comptime_number() => {
                if l != r {
                    self.tir.diagnostics.push(report_type_mistmatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        TypeMistmatchDiagnostic {
                            expected_type: l,
                            actual_type: r,
                            span: SourceSpan::new(ctx.resolve_context.file_id, right.span),
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
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: ctx.resolve_context.file_id,
                                operator: operator.clone(),
                                operand: Spanned {
                                    inner: ty,
                                    span: right.span,
                                },
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
                self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut left, ty)?;

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
                self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut right, ty)?;

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
                self.tir
                    .diagnostics
                    .push(report_unreachable_code(SourceSpan::new(
                        ctx.resolve_context.file_id,
                        right.span,
                    )));

                Ok(left)
            }
            (_, r) if r == TypeIndex::NEVER => {
                self.tir
                    .diagnostics
                    .push(report_unreachable_code(SourceSpan::new(
                        ctx.resolve_context.file_id,
                        operator.span,
                    )));

                Ok(right)
            }
            (left_type, right_type) if left_type == right_type && left_type.is_primitive() => {
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
                        TypeFormatter::new(&self.tir, &self.interner),
                        BinaryExpressionMistmatchDiagnostic {
                            file_id: ctx.resolve_context.file_id,
                            left_type: Spanned {
                                inner: left_type,
                                span: left.span,
                            },
                            operator: operator.clone(),
                            right_type: Spanned {
                                inner: right_type,
                                span: right.span,
                            },
                        },
                    ));

                match access_ctx.expected_type {
                    Some(expected_type) => Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ty: expected_type,
                        span: expr.span,
                    }),
                    None => Err(()),
                }
            }
        }
    }

    fn type_param_bounds(&self, ty: TypeIndex) -> &[TraitIndex] {
        let Type::TypeParam {
            ref owner,
            param_index,
        } = self.tir.type_pool[ty.as_usize()]
        else {
            return &[];
        };
        self.type_param_bounds_by_owner(owner.clone(), param_index)
    }

    /// True when `arg_ty` is a `TypeParam` whose bounds include the trait that
    /// `expected_ty` represents.
    fn type_param_satisfies_bound(&self, arg_ty: TypeIndex, expected_ty: TypeIndex) -> bool {
        let &Type::Trait {
            trait_index: expected_trait,
        } = &self.tir.type_pool[expected_ty.as_usize()]
        else {
            return false;
        };
        self.type_param_bounds(arg_ty)
            .iter()
            .any(|&b| b == expected_trait)
    }

    fn type_param_typeset_bound_by_owner(
        &self,
        owner: TypeParamOwner,
        param_index: u32,
    ) -> Option<TypesetIndex> {
        match owner {
            TypeParamOwner::Function(def_id) => self
                .tir
                .function_index_lookup
                .get(&def_id)
                .and_then(|&fi| {
                    self.tir.functions[fi as usize]
                        .type_params
                        .get(param_index as usize)
                })
                .and_then(|tp| tp.typeset_bound),
            TypeParamOwner::Struct(def_id) => self
                .tir
                .struct_index_lookup
                .get(&def_id)
                .and_then(|&si| {
                    self.tir.structs[si as usize]
                        .type_params
                        .get(param_index as usize)
                })
                .and_then(|tp| tp.typeset_bound),
            TypeParamOwner::Trait(_) => None,
        }
    }

    fn type_param_typeset_bound(&self, ty: TypeIndex) -> Option<TypesetIndex> {
        let Type::TypeParam {
            ref owner,
            param_index,
        } = self.tir.type_pool[ty.as_usize()]
        else {
            return None;
        };
        self.type_param_typeset_bound_by_owner(owner.clone(), param_index)
    }

    /// Returns the typeset bound for any type that can carry one:
    /// `TypeParam` (via its `typeset_bound` field) or `AssocTypeProjection`
    /// (via the trait's associated-type `typeset_bound`).
    fn typeset_bound_for(&self, ty: TypeIndex) -> Option<TypesetIndex> {
        match &self.tir.type_pool[ty.as_usize()] {
            Type::TypeParam { .. } => self.type_param_typeset_bound(ty),
            Type::AssocTypeProjection {
                trait_index,
                assoc_name,
                ..
            } => self.tir.traits[*trait_index as usize]
                .assoc_types
                .get(assoc_name)
                .and_then(|at| at.typeset_bound),
            _ => None,
        }
    }

    /// True when concrete `ty` is a member of the given typeset.
    fn concrete_type_in_typeset(&self, ty: TypeIndex, typeset_index: TypesetIndex) -> bool {
        self.tir.typesets[typeset_index as usize]
            .members
            .iter()
            .any(|&m| m == ty)
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
        let type_params: Vec<_> = self.tir.functions[func_index as usize]
            .type_params
            .iter()
            .map(|tp| (tp.name, tp.typeset_bound))
            .collect();

        for (i, (param_name, typeset_bound)) in type_params.into_iter().enumerate() {
            let Some(ts_index) = typeset_bound else {
                continue;
            };
            let Some(&arg_ty) = type_args.get(i) else {
                continue;
            };
            if arg_ty == TypeIndex::ERROR {
                continue;
            }
            let satisfied = match &self.tir.type_pool[arg_ty.as_usize()] {
                // Nested generic: the caller's TypeParam forwards here — check its typeset bound.
                Type::TypeParam { .. } => self
                    .type_param_typeset_bound(arg_ty)
                    .map_or(false, |b| b == ts_index),
                // Concrete type: must be a member of the typeset.
                _ => self.concrete_type_in_typeset(arg_ty, ts_index),
            };
            if !satisfied {
                let type_name = TypeFormatter::new(&self.tir, &self.interner)
                    .display_type(arg_ty)
                    .unwrap_or_default();
                let set_name = self
                    .interner
                    .resolve(self.tir.typesets[ts_index as usize].name.inner)
                    .unwrap_or("?")
                    .to_owned();
                let param_name_str = self.interner.resolve(param_name).unwrap_or("?").to_owned();
                self.tir.diagnostics.push(
                    Diagnostic::error()
                        .with_code(DiagnosticCode::TypesetBoundViolation.code())
                        .with_message(format!(
                            "type `{type_name}` is not a member of typeset `{set_name}`"
                        ))
                        .with_label(Label::primary(file_id, call_span).with_message(format!(
                            "`{param_name_str}` requires a type from `{set_name}`"
                        ))),
                );
            }
        }
    }

    fn build_generic_call_arguments(
        &mut self,
        file_id: FileId,
        func_index: FunctionIndex,
        arguments: &mut [Expression],
        explicit_type_arguments: Option<&[TypeIndex]>,
        expected_result: Option<TypeIndex>,
    ) -> Result<Box<[TypeIndex]>, ()> {
        let mut type_args =
            vec![TypeIndex::ERROR; self.tir.functions[func_index as usize].type_params.len()];
        if let Some(initial) = explicit_type_arguments {
            for (slot, &ty) in type_args.iter_mut().zip(initial.iter()) {
                if ty != TypeIndex::ERROR {
                    *slot = ty;
                }
            }
        }

        if let Some(expected_result) = expected_result {
            let result_type = self.tir.functions[func_index as usize]
                .result
                .as_ref()
                .map(|r| r.inner)
                .unwrap_or(TypeIndex::UNIT);
            self.infer_type_args(&mut type_args, result_type, expected_result);
        }
        for (index, arg) in arguments.iter().enumerate() {
            let param_type = match self.tir.functions[func_index as usize].params.get(index) {
                Some(p) => p.ty.inner,
                None => break,
            };
            self.infer_type_args(&mut type_args, param_type, arg.ty);
        }

        let mut had_error = false;
        for (index, arg) in arguments.iter_mut().enumerate() {
            let param_type = match self.tir.functions[func_index as usize].params.get(index) {
                Some(p) => p.ty.inner,
                None => break,
            };

            let expected_type = self.substitute_expected_type(param_type, &type_args);

            if let Some(expected) = expected_type {
                if arg.ty.is_comptime_number() {
                    if self.coerce_untyped_expr(file_id, arg, expected).is_err() {
                        had_error = true;
                    }
                } else if !self.coercible_to(arg.ty, expected)
                    && !self.type_param_satisfies_bound(arg.ty, expected)
                {
                    self.tir.diagnostics.push(report_type_mistmatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        TypeMistmatchDiagnostic {
                            expected_type: expected,
                            actual_type: arg.ty,
                            span: SourceSpan::new(file_id, arg.span),
                        },
                    ));
                }
            } else if arg.ty.is_comptime_number() {
                self.tir
                    .diagnostics
                    .push(report_type_annotation_required(SourceSpan::new(
                        file_id, arg.span,
                    )));
                had_error = true;
            }
        }

        if had_error {
            Err(())
        } else {
            Ok(type_args.into_boxed_slice())
        }
    }

    fn build_call_arguments(
        &mut self,
        ctx: &mut FunctionContext,
        arguments: &[Separated<Spanned<ast::Expression>>],
        params: &[TypeIndex],
        type_args: &[TypeIndex],
    ) -> Result<Box<[Expression]>, ()> {
        let mut had_error = false;
        let mut built: Vec<Expression> = Vec::with_capacity(arguments.len());

        for (index, argument) in arguments.iter().enumerate() {
            let expected_type = params
                .get(index)
                .copied()
                .and_then(|param_type| self.substitute_expected_type(param_type, type_args));

            let mut argument = match self.build_expression(
                ctx,
                AccessContext {
                    expected_type,
                    access_kind: AccessKind::Read,
                },
                &argument.inner,
            ) {
                Ok(e) => e,
                Err(_) => {
                    had_error = true;
                    continue;
                }
            };

            if let Some(expected_type) = expected_type {
                if argument.ty.is_comptime_number() {
                    if self
                        .coerce_untyped_expr(
                            ctx.resolve_context.file_id,
                            &mut argument,
                            expected_type,
                        )
                        .is_err()
                    {
                        had_error = true;
                        continue;
                    }
                } else if !self.coercible_to(argument.ty, expected_type)
                    && !self.type_param_satisfies_bound(argument.ty, expected_type)
                {
                    self.tir.diagnostics.push(report_type_mistmatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        TypeMistmatchDiagnostic {
                            expected_type,
                            actual_type: argument.ty,
                            span: SourceSpan::new(ctx.resolve_context.file_id, argument.span),
                        },
                    ));
                }
            } else if argument.ty.is_comptime_number() {
                // Argument passed to a TypeParam parameter with no concrete type
                // and no inference from context — require an explicit annotation.
                self.tir
                    .diagnostics
                    .push(report_type_annotation_required(SourceSpan::new(
                        ctx.resolve_context.file_id,
                        argument.span,
                    )));
                had_error = true;
                continue;
            }

            built.push(argument);
        }

        if had_error {
            Err(())
        } else {
            Ok(built.into_boxed_slice())
        }
    }

    fn build_call_expression(
        &mut self,
        ctx: &mut FunctionContext,
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
                expected_type: None,
                access_kind: AccessKind::Read,
            },
            ast_callee,
        )?;
        let signature_index = match self.tir.type_pool[callee.ty.as_usize()] {
            Type::Function { .. } => callee.ty,
            Type::FunctionItem { id, .. } => {
                self.tir.functions[self.tir.function_index_lookup[&id] as usize].signature_index
            }
            _ => {
                let formatter = TypeFormatter::new(&self.tir, &self.interner);
                let diagnostic = Diagnostic::error()
                    .with_code(DiagnosticCode::CannotCallExpression.code())
                    .with_message("call expression requires function")
                    .with_label(
                        SourceSpan::new(ctx.resolve_context.file_id, ast_callee.span)
                            .primary_label()
                            .with_message(format!(
                                "expected function, found `{}`",
                                formatter.display_type(callee.ty).unwrap()
                            )),
                    );
                self.tir.diagnostics.push(diagnostic);

                return Ok(Expression {
                    kind: ExprKind::Call {
                        callee: Box::new(callee),
                        arguments: Box::new([]),
                    },
                    ty: TypeIndex::ERROR,
                    span: expr.span,
                });
            }
        };

        let signature = match &self.tir.type_pool[signature_index.as_usize()] {
            Type::Function { signature } => signature.clone(),
            _ => unreachable!(),
        };
        let params = signature.params();
        if arguments.len() != params.len() {
            self.tir.diagnostics.push(report_argument_count_mismatch(
                TypeFormatter::new(&self.tir, &self.interner),
                ArgumentCountMismatchDiagnostic {
                    actual_count: arguments.len(),
                    params,
                    call_span: SourceSpan::new(ctx.resolve_context.file_id, callee.span),
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
        if let Some(id) = direct_id {
            let func_index = self.tir.function_index_lookup[&id];
            if let Some(access) = self.tir.functions[func_index as usize].accesses.last_mut() {
                access.kind = FunctionAccessKind::DirectCall;
            }

            let type_params_len = self.tir.functions[func_index as usize].type_params.len();
            if type_params_len > 0 {
                // Turbofish type args are stored inside the FunctionItem type
                // by build_path_expression. Seed them before inference so
                // explicit args take priority over argument-inferred ones.
                let callee_ty = callee.ty;
                let turbofish: Vec<TypeIndex> = match &self.tir.type_pool[callee_ty.as_usize()] {
                    Type::FunctionItem { type_args, .. } if !type_args.is_empty() => {
                        type_args.to_vec()
                    }
                    _ => vec![],
                };
                let turbofish_seed: Option<&[TypeIndex]> = if turbofish.is_empty() {
                    None
                } else {
                    Some(&turbofish)
                };

                let mut built_args = Vec::with_capacity(arguments.len());
                for arg in arguments.iter() {
                    built_args.push(self.build_expression(
                        ctx,
                        AccessContext {
                            expected_type: None,
                            access_kind: AccessKind::Read,
                        },
                        &arg.inner,
                    )?);
                }

                let type_args = self.build_generic_call_arguments(
                    ctx.resolve_context.file_id,
                    func_index,
                    &mut built_args,
                    turbofish_seed,
                    access_ctx.expected_type,
                )?;
                self.check_typeset_bounds_on_type_args(
                    func_index,
                    &type_args,
                    ctx.resolve_context.file_id,
                    expr.span,
                );
                let return_ty = self.substitute_type(signature.result(), &type_args);

                return Ok(Expression {
                    kind: ExprKind::GenericCall {
                        id,
                        type_args,
                        arguments: built_args.into_boxed_slice(),
                    },
                    ty: return_ty,
                    span: expr.span,
                });
            }
        }

        // Non-generic call (direct or indirect).
        let arguments = self.build_call_arguments(ctx, arguments, params, &[])?;
        Ok(Expression {
            kind: ExprKind::Call {
                callee: Box::new(callee),
                arguments,
            },
            ty: signature.result(),
            span: expr.span,
        })
    }

    fn build_method_call_expression(
        &mut self,
        ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let MethodCallExpr {
            arguments,
            method,
            object,
            type_args,
        } = match &expr.inner {
            ast::Expression::MethodCall(method_call) => method_call.as_ref(),
            _ => unreachable!(),
        };

        let object = self.build_expression(
            ctx,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
            &object,
        )?;

        let method_entry = match &self.tir.type_pool[object.ty.as_usize()] {
            Type::Trait { trait_index } => self.tir.traits[*trait_index as usize]
                .members
                .get(&method.inner)
                .cloned(),
            Type::TypeParam { .. } => self.type_param_bounds(object.ty).iter().find_map(|&ti| {
                self.tir.traits[ti as usize]
                    .members
                    .get(&method.inner)
                    .cloned()
            }),
            _ => self
                .tir
                .impl_members
                .get(&object.ty)
                .and_then(|m| m.get(&method.inner))
                .cloned(),
        };

        match method_entry {
            Some(ImplEntry::Method(func_index)) => {
                let caller_id = self.tir.functions[ctx.func_index as usize].id;
                self.tir.functions[func_index as usize]
                    .accesses
                    .push(FunctionAccess {
                        caller: Some(caller_id),
                        kind: FunctionAccessKind::DirectCall,
                        file_id: ctx.resolve_context.file_id,
                        span: method.span,
                    });
                let id = self.tir.functions[func_index as usize].id;
                let signature_index = self.tir.functions[func_index as usize].signature_index;
                let signature = match &self.tir.type_pool[signature_index.as_usize()] {
                    Type::Function { signature } => signature.clone(),
                    _ => unreachable!(),
                };
                let non_self_params = &signature.params()[1..];
                if arguments.len() != non_self_params.len() {
                    self.tir.diagnostics.push(report_argument_count_mismatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        ArgumentCountMismatchDiagnostic {
                            actual_count: arguments.len(),
                            params: non_self_params,
                            call_span: SourceSpan::new(ctx.resolve_context.file_id, object.span),
                            is_method: true,
                        },
                    ));
                }

                let type_params_len = self.tir.functions[func_index as usize].type_params.len();
                if type_params_len > 0 {
                    let mut built_arguments = std::iter::once(Ok(object))
                        .chain(arguments.iter().map(|arg| {
                            self.build_expression(
                                ctx,
                                AccessContext {
                                    expected_type: None,
                                    access_kind: AccessKind::Read,
                                },
                                &arg.inner,
                            )
                        }))
                        .collect::<Result<Box<_>, _>>()?;
                    let type_args = self.build_generic_call_arguments(
                        ctx.resolve_context.file_id,
                        func_index,
                        &mut built_arguments,
                        None,
                        access_ctx.expected_type,
                    )?;
                    self.check_typeset_bounds_on_type_args(
                        func_index,
                        &type_args,
                        ctx.resolve_context.file_id,
                        expr.span,
                    );
                    let return_ty = self.substitute_type(signature.result(), &type_args);
                    return Ok(Expression {
                        kind: ExprKind::GenericMethodCall {
                            id,
                            type_args,
                            arguments: built_arguments,
                        },
                        ty: return_ty,
                        span: expr.span,
                    });
                }

                let args = self.build_call_arguments(ctx, &arguments, non_self_params, &[])?;
                return Ok(Expression {
                    kind: ExprKind::MethodCall {
                        arguments: std::iter::once(object).chain(args).collect(),
                        id,
                    },
                    ty: signature.result(),
                    span: expr.span,
                });
            }
            Some(_) => {
                self.tir.diagnostics.push(report_not_a_method(
                    SourceSpan::new(ctx.resolve_context.file_id, method.span),
                    TypeFormatter::new(&self.tir, &self.interner),
                    method.inner,
                    object.ty,
                ));
                return Err(());
            }
            None => { /* gonna try to look for generic impl entries */ }
        }

        if let Some((block_idx, type_args)) = self.find_generic_impl(object.ty, method.inner) {
            match self.tir.generic_impl_list[block_idx]
                .members
                .get(&method.inner)
                .cloned()
            {
                Some(ImplEntry::Method(func_index)) => {
                    let caller_id = self.tir.functions[ctx.func_index as usize].id;
                    self.tir.functions[func_index as usize]
                        .accesses
                        .push(FunctionAccess {
                            caller: Some(caller_id),
                            kind: FunctionAccessKind::DirectCall,
                            file_id: ctx.resolve_context.file_id,
                            span: method.span,
                        });
                    let id = self.tir.functions[func_index as usize].id;
                    let signature_index = self.tir.functions[func_index as usize].signature_index;
                    let signature = match &self.tir.type_pool[signature_index.as_usize()] {
                        Type::Function { signature } => signature.clone(),
                        _ => unreachable!(),
                    };
                    let params = &signature.params()[1..];
                    if arguments.len() != params.len() {
                        self.tir.diagnostics.push(report_argument_count_mismatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            ArgumentCountMismatchDiagnostic {
                                actual_count: arguments.len(),
                                params,
                                call_span: SourceSpan::new(
                                    ctx.resolve_context.file_id,
                                    object.span,
                                ),
                                is_method: true,
                            },
                        ));
                    }
                    let args = self.build_call_arguments(ctx, &arguments, params, &type_args)?;
                    let return_ty = self.substitute_type(signature.result(), &type_args);
                    return Ok(Expression {
                        kind: ExprKind::GenericMethodCall {
                            id,
                            type_args,
                            arguments: std::iter::once(object).chain(args).collect(),
                        },
                        ty: return_ty,
                        span: expr.span,
                    });
                }
                Some(_) => {
                    self.tir.diagnostics.push(report_not_a_method(
                        SourceSpan::new(ctx.resolve_context.file_id, method.span),
                        TypeFormatter::new(&self.tir, &self.interner),
                        method.inner,
                        object.ty,
                    ));
                    return Err(());
                }
                None => { /* didn't found neither direct or generic impl entry */ }
            }
        }

        self.tir.diagnostics.push(report_method_not_found(
            SourceSpan::new(ctx.resolve_context.file_id, method.span),
            TypeFormatter::new(&self.tir, &self.interner),
            method.inner,
            object.ty,
        ));
        Err(())
    }

    fn build_local_definition_statement(
        &mut self,
        ctx: &mut FunctionContext,
        stmt: &Separated<Spanned<ast::Statement>>,
    ) -> Result<Expression, ()> {
        let (mut_span, name, ty, value) = match &stmt.inner.inner {
            ast::Statement::LocalDefinition { pattern, ty, value } => match &pattern.inner {
                ast::Pattern::Binding { mut_span, name } => {
                    (mut_span.clone(), name.clone(), ty, value)
                }
                _ => {
                    self.tir.diagnostics.push(
                        codespan_reporting::diagnostic::Diagnostic::error()
                            .with_message("pattern destructuring in locals is not yet supported")
                            .with_label(Label::primary(ctx.resolve_context.file_id, pattern.span)),
                    );
                    return Err(());
                }
            },
            _ => unreachable!(),
        };

        let expected_type = match ty {
            Some(ty) => Some(self.resolve_type(&ctx.resolve_context, ty)),
            None => None,
        };
        let mut value = self.build_expression(
            ctx,
            AccessContext {
                expected_type,
                access_kind: AccessKind::Read,
            },
            value,
        )?;

        let ty: TypeIndex = match (value.ty, expected_type) {
            (v, None) if v.is_comptime_number() => {
                self.tir
                    .diagnostics
                    .push(report_type_annotation_required(SourceSpan::new(
                        ctx.resolve_context.file_id,
                        name.span,
                    )));
                // Use Error type for recovery - this allows later references to work
                // without cascading "undeclared identifier" errors
                TypeIndex::ERROR
            }
            (ty, None) => ty,
            (v, Some(expected_type)) if v.is_comptime_number() => {
                self.coerce_untyped_expr(ctx.resolve_context.file_id, &mut value, expected_type)?;
                expected_type
            }
            (actual_type, Some(expected_type)) => {
                if self.coercible_to(actual_type, expected_type) {
                    expected_type
                } else {
                    self.tir.diagnostics.push(report_type_mistmatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        TypeMistmatchDiagnostic {
                            expected_type,
                            actual_type,
                            span: SourceSpan::new(ctx.resolve_context.file_id, value.span),
                        },
                    ));
                    expected_type // Recover by using the expected type
                }
            }
        };

        let local_index = ctx.push_local(Local {
            name: name.clone(),
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

    fn coerce_untyped_expr(
        &mut self,
        file_id: FileId,
        expression: &mut Expression,
        target_idx: TypeIndex,
    ) -> Result<(), ()> {
        match expression.kind {
            ExprKind::Int { .. } => self.coerce_untyped_int_expr(file_id, expression, target_idx),
            ExprKind::Float { .. } => {
                self.coerce_untyped_float_expr(file_id, expression, target_idx)
            }
            ExprKind::Unary { .. } => {
                self.coerce_untyped_unary_expr(file_id, expression, target_idx)
            }
            ExprKind::Binary { .. } => {
                self.coerce_untyped_binary_expression(file_id, expression, target_idx)
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
        let formatter = TypeFormatter::new(&self.tir, &self.interner);

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
            if value > i64::MAX || value < i64::MIN {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: TypeIndex::I64,
                            value,
                            span: SourceSpan::new(file_id, expr.span),
                        },
                    ));
            }
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
        } else if let Some(typeset_index) = self.typeset_bound_for(target_idx) {
            let ts = &self.tir.typesets[typeset_index as usize];
            let range = &ts.intersection_range;
            let ts_name = self
                .interner
                .resolve(ts.name.inner)
                .unwrap_or("?")
                .to_string();
            if !range.contains(value) {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_typeset_range(
                        value,
                        &ts_name,
                        range,
                        SourceSpan::new(file_id, expr.span),
                    ));
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
                TypeFormatter::new(&self.tir, &self.interner),
                target_idx,
                SourceSpan::new(file_id, expr.span),
            ));
            Err(())
        }
    }

    fn coerce_untyped_unary_expr(
        &mut self,
        file_id: FileId,
        expr: &mut Expression,
        target_idx: TypeIndex,
    ) -> Result<(), ()> {
        let (operand, operator) = match &mut expr.kind {
            ExprKind::Unary { operand, operator } => (operand, operator.inner),
            _ => unreachable!(),
        };

        match operator {
            ast::UnaryOp::BitNot | ast::UnaryOp::InvertSign => {
                let is_valid = target_idx == TypeIndex::I32 || target_idx == TypeIndex::I64;
                if !is_valid {
                    self.tir.diagnostics.push(report_unable_to_coerce(
                        TypeFormatter::new(&self.tir, &self.interner),
                        target_idx,
                        SourceSpan::new(file_id, expr.span),
                    ));
                    return Err(());
                }
            }
            _ => unreachable!(),
        }

        self.coerce_untyped_expr(file_id, operand, target_idx)
            .and_then(|_| Ok(expr.ty = target_idx))
    }

    fn coerce_untyped_binary_expression(
        &mut self,
        file_id: FileId,
        expr: &mut Expression,
        target_idx: TypeIndex,
    ) -> Result<(), ()> {
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
                        TypeFormatter::new(&self.tir, &self.interner),
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
                        TypeFormatter::new(&self.tir, &self.interner),
                        target_idx,
                        SourceSpan::new(file_id, expr.span),
                    ));
                    return Err(());
                }
            }
            _ => unreachable!(),
        };

        match (
            self.coerce_untyped_expr(file_id, left, target_idx),
            self.coerce_untyped_expr(file_id, right, target_idx),
        ) {
            (Ok(_), Ok(_)) => {
                expr.ty = target_idx;
                Ok(())
            }
            _ => Err(()),
        }
    }

    fn build_struct_init_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        init_span: ast::TextSpan,
        path: &ast::Path,
        fields: &[ast::Separated<ast::Spanned<ast::StructInitField>>],
    ) -> Result<Expression, ()> {
        // The last segment carries the struct name (and optional type args);
        // all preceding segments are namespace qualifiers.
        let struct_seg = path.segments.last().expect("path has at least one segment");
        let struct_name_sym = struct_seg.ident.inner;
        let struct_name_span = struct_seg.ident.span;
        let explicit_type_args = &struct_seg.type_args;

        let struct_index = if path.segments.len() == 1 {
            // Single-segment: look up bare name in current scope.
            match self
                .symbol_lookup
                .get(&(SymbolNamespace::Type, struct_name_sym))
                .cloned()
            {
                Some(SymbolKind::Struct { struct_index }) => struct_index,
                Some(_) => {
                    self.tir.diagnostics.push(report_not_a_struct_type(
                        func_ctx.resolve_context.file_id,
                        self.interner.resolve(struct_name_sym).unwrap().to_string(),
                        struct_name_span,
                    ));
                    return Err(());
                }
                None => {
                    self.tir
                        .diagnostics
                        .push(report_undeclared_identifier(SourceSpan::new(
                            func_ctx.resolve_context.file_id,
                            struct_name_span,
                        )));
                    return Err(());
                }
            }
        } else {
            // Multi-segment: walk namespace prefix, then resolve struct name in that
            // namespace.
            let resolve_context = func_ctx.resolve_context.clone();
            let first = &path.segments[0];
            let mut namespace_ty =
                self.resolve_type_identifier(&resolve_context, first.ident.inner, first.ident.span);
            if namespace_ty == TypeIndex::ERROR {
                return Err(());
            }
            let mut namespace_span = first.ident.span;
            for seg in &path.segments[1..path.segments.len() - 1] {
                namespace_span = TextSpan::new(namespace_span.start, seg.ident.span.end);
                namespace_ty = self.resolve_namespace_type_member(
                    &resolve_context,
                    namespace_ty,
                    seg.ident.inner,
                    seg.ident.span,
                );
                if namespace_ty == TypeIndex::ERROR {
                    return Err(());
                }
            }
            let struct_ty = self.resolve_namespace_type_member(
                &resolve_context,
                namespace_ty,
                struct_name_sym,
                struct_name_span,
            );
            if struct_ty == TypeIndex::ERROR {
                return Err(());
            }
            match &self.tir.type_pool[struct_ty.as_usize()] {
                Type::Struct { struct_index, .. } => *struct_index,
                _ => {
                    self.tir.diagnostics.push(report_not_a_struct_type(
                        func_ctx.resolve_context.file_id,
                        self.interner.resolve(struct_name_sym).unwrap().to_string(),
                        struct_name_span,
                    ));
                    return Err(());
                }
            }
        };

        let type_params_len = self.tir.structs[struct_index as usize].type_params.len();
        let resolved_args: Box<[TypeIndex]> = if !explicit_type_args.is_empty() {
            // Explicit turbofish args take priority.
            explicit_type_args
                .iter()
                .map(|arg| self.resolve_type(&func_ctx.resolve_context, arg))
                .collect()
        } else if type_params_len == 0 {
            Box::new([])
        } else {
            // Try to extract type args from the expected type in context.
            match access_ctx.expected_type {
                Some(expected) => match &self.tir.type_pool[expected.as_usize()] {
                    Type::Struct {
                        struct_index: expected_si,
                        args,
                    } if *expected_si == struct_index && args.len() == type_params_len => {
                        args.clone()
                    }
                    _ => Box::new([]),
                },
                None => Box::new([]),
            }
        };
        let type_params_len = self.tir.structs[struct_index as usize].type_params.len();
        if !resolved_args.is_empty() && resolved_args.len() != type_params_len {
            let struct_name = self
                .interner
                .resolve(self.tir.structs[struct_index as usize].name.inner)
                .unwrap()
                .to_string();
            self.tir.diagnostics.push(
                Diagnostic::error()
                    .with_code(DiagnosticCode::TypeArgCountMismatch.code())
                    .with_message(format!(
                        "`{}` expects {} type argument{}, found {}",
                        struct_name,
                        type_params_len,
                        if type_params_len == 1 { "" } else { "s" },
                        resolved_args.len(),
                    ))
                    .with_label(Label::primary(func_ctx.resolve_context.file_id, init_span)),
            );
            return Err(());
        }

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
        let mut field_slots: Vec<Option<Expression>> = (0..field_count).map(|_| None).collect();

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
                        &field_name,
                        SourceSpan::new(func_ctx.resolve_context.file_id, first_span),
                        SourceSpan::new(func_ctx.resolve_context.file_id, field.name.span),
                    ));
                continue;
            }
            // Mark this field as mentioned (by its name span) before building the value,
            // so that build errors don't cause it to appear in the "missing fields" list.
            first_mention[field_index] = Some(field.name.span);

            let field_value = match &field.value {
                Some(expr) => expr.as_ref(),
                None => {
                    // Shorthand: treat `{ a }` as `{ a: a }` by synthesising a single-segment path
                    &ast::Spanned {
                        inner: ast::Expression::Path(ast::Path {
                            segments: Box::new([ast::PathSegment {
                                ident: field.name.clone(),
                                type_args: Box::new([]),
                            }]),
                            span: field.name.span,
                        }),
                        span: field.name.span,
                    }
                }
            };
            let raw_expected_ty = self.tir.structs[struct_index as usize].fields[field_index]
                .ty
                .inner;
            let expected_ty = if resolved_args.is_empty() {
                raw_expected_ty
            } else {
                self.substitute_type(raw_expected_ty, &resolved_args)
            };
            let mut field_expr = match self.build_expression(
                func_ctx,
                AccessContext {
                    expected_type: Some(expected_ty),
                    access_kind: AccessKind::Read,
                },
                field_value,
            ) {
                Ok(e) => e,
                Err(_) => continue,
            };

            if field_expr.ty.is_comptime_number() {
                match self.coerce_untyped_expr(
                    func_ctx.resolve_context.file_id,
                    &mut field_expr,
                    expected_ty,
                ) {
                    Ok(_) => {}
                    Err(_) => continue,
                }
            } else if !self.coercible_to(field_expr.ty, expected_ty) {
                self.tir.diagnostics.push(report_type_mistmatch(
                    TypeFormatter::new(&self.tir, &self.interner),
                    TypeMistmatchDiagnostic {
                        expected_type: expected_ty,
                        actual_type: field_expr.ty,
                        span: SourceSpan::new(func_ctx.resolve_context.file_id, field_expr.span),
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
                    .resolve(self.tir.structs[struct_index as usize].fields[i].name.inner)
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
        self.tir.structs[struct_index as usize]
            .accesses
            .push(SourceSpan::new(
                func_ctx.resolve_context.file_id,
                struct_name_span,
            ));

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

        let fields: Box<[Expression]> = field_slots.into_iter().map(|e| e.unwrap()).collect();
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
        func_ctx: &mut FunctionContext,
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
        let expected_elems: Option<Box<[TypeIndex]>> = access_ctx.expected_type.and_then(|ty| {
            if let Type::Tuple { elements } = &self.tir.type_pool[ty.as_usize()] {
                if elements.len() == ast_elements.len() {
                    return Some(elements.clone());
                }
            }
            None
        });

        let mut built = Vec::with_capacity(ast_elements.len());
        let mut had_error = false;
        for (i, elem_expr) in ast_elements.iter().enumerate() {
            let expected = expected_elems.as_ref().map(|e| e[i]);
            match self.build_expression(
                func_ctx,
                AccessContext {
                    expected_type: expected,
                    access_kind: AccessKind::Read,
                },
                elem_expr,
            ) {
                Ok(mut e) => {
                    if e.ty.is_comptime_number() {
                        if let Some(exp_ty) = expected {
                            let _ = self.coerce_untyped_expr(
                                func_ctx.resolve_context.file_id,
                                &mut e,
                                exp_ty,
                            );
                        }
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
        func_ctx: &mut FunctionContext,
        span: ast::TextSpan,
        pointer: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let pointer = self.build_expression(
            func_ctx,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
            pointer,
        )?;

        let (inner_ty, memory) = match &self.tir.type_pool[pointer.ty.as_usize()] {
            Type::Pointer { to, memory, .. } => (*to, *memory),
            _ => {
                self.tir.diagnostics.push(report_cannot_deref_non_pointer(
                    SourceSpan::new(func_ctx.resolve_context.file_id, pointer.span),
                    TypeFormatter::new(&self.tir, &self.interner)
                        .display_type(pointer.ty)
                        .unwrap(),
                ));
                return Err(());
            }
        };

        Ok(Expression {
            kind: ExprKind::Deref {
                pointer: Box::new(pointer),
                memory,
            },
            ty: inner_ty,
            span,
        })
    }

    fn resolve_ambient_memory(&mut self, span: SourceSpan) -> Result<TypeIndex, ()> {
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
        match &self.tir.type_pool[memory.as_usize()].clone() {
            Type::Memory { id, .. } => {
                let idx = self.tir.memory_index_lookup[id];
                match self.tir.memories[idx as usize].kind {
                    MemoryKind::Memory32 => TypeIndex::U32,
                    MemoryKind::Memory64 => TypeIndex::U64,
                }
            }
            Type::TypeParam { owner, param_index } => {
                // Generic `M: Memory` — the index type is `M::Size`.
                // Find the first bound trait that declares `Size` as an assoc type.
                let size_sym = self.interner.get_or_intern("Size");
                let param_index = *param_index;
                let bounds = self
                    .type_param_bounds_by_owner(owner.clone(), param_index)
                    .to_owned();
                let trait_index = bounds.iter().copied().find(|&ti| {
                    self.tir.traits[ti as usize]
                        .assoc_types
                        .contains_key(&size_sym)
                });
                match trait_index {
                    Some(trait_index) => self.intern_type(Type::AssocTypeProjection {
                        trait_index,
                        assoc_name: size_sym,
                        param_index,
                    }),
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
        func_ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        span: ast::TextSpan,
        elements: &[ast::Spanned<ast::Expression>],
    ) -> Result<Expression, ()> {
        let source_span = SourceSpan::new(func_ctx.resolve_context.file_id, span);

        let (expected_of, expected_memory, expected_size, expected_mutable) =
            match access_ctx.expected_type {
                Some(ty) => match self.tir.type_pool[ty.as_usize()].clone() {
                    Type::Array {
                        of,
                        memory,
                        size,
                        mutable,
                    } => (Some(of), Some(memory), Some(size), mutable),
                    _ => (None, None, None, false),
                },
                None => (None, None, None, false),
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
                match expected_of {
                    Some(of) => {
                        self.coerce_untyped_expr(func_ctx.resolve_context.file_id, &mut elem, of)?
                    }
                    None => {
                        self.tir.diagnostics.push(report_type_annotation_required(
                            SourceSpan::new(func_ctx.resolve_context.file_id, elem.span),
                        ));
                        return Err(());
                    }
                }
            }
            if !elem.ty.is_numeric() {
                self.tir.diagnostics.push(
                    Diagnostic::error()
                        .with_message("array element type must be a numeric type")
                        .with_label(Label::primary(func_ctx.resolve_context.file_id, elem.span)),
                );
                return Err(());
            }
            if !matches!(elem.kind, ExprKind::Int { .. } | ExprKind::Float { .. }) {
                self.tir
                    .diagnostics
                    .push(report_array_element_not_const(SourceSpan::new(
                        func_ctx.resolve_context.file_id,
                        elem.span,
                    )));
                return Err(());
            }
            built.push(elem);
        }

        let elem_type = if let Some(first) = built.first() {
            let ty = first.ty;
            for elem in &built[1..] {
                if elem.ty != ty {
                    self.tir.diagnostics.push(report_type_mistmatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        TypeMistmatchDiagnostic {
                            expected_type: ty,
                            actual_type: elem.ty,
                            span: SourceSpan::new(func_ctx.resolve_context.file_id, elem.span),
                        },
                    ));
                    return Err(());
                }
            }
            ty
        } else {
            match expected_of {
                Some(of) => of,
                None => {
                    self.tir
                        .diagnostics
                        .push(report_type_annotation_required(source_span));
                    return Err(());
                }
            }
        };

        let memory = match expected_memory {
            Some(m) => m,
            None => self.resolve_ambient_memory(source_span)?,
        };
        let array_ty = self.intern_type(Type::Array {
            of: elem_type,
            size: elements.len() as u32,
            mutable: expected_mutable,
            memory,
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
        func_ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        span: ast::TextSpan,
        value_expr: &ast::Spanned<ast::Expression>,
        count_expr: &ast::Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let source_span = SourceSpan::new(func_ctx.resolve_context.file_id, span);

        let (expected_of, expected_memory, expected_mutable) = match access_ctx.expected_type {
            Some(ty) => match self.tir.type_pool[ty.as_usize()].clone() {
                Type::Array {
                    of,
                    memory,
                    mutable,
                    ..
                } => (Some(of), Some(memory), mutable),
                _ => (None, None, false),
            },
            None => (None, None, false),
        };

        let count_built = self.build_expression(
            func_ctx,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
            count_expr,
        )?;
        let count = match count_built.kind {
            ExprKind::Int { value } if value >= 0 => value as u32,
            _ => {
                self.tir
                    .diagnostics
                    .push(report_array_repeat_count_not_const(SourceSpan::new(
                        func_ctx.resolve_context.file_id,
                        count_expr.span,
                    )));
                return Err(());
            }
        };

        if let Some(expected_ty) = access_ctx.expected_type {
            if let Type::Array { size, .. } = self.tir.type_pool[expected_ty.as_usize()].clone() {
                if count != size {
                    self.tir.diagnostics.push(report_array_size_mismatch(
                        source_span,
                        size,
                        count as usize,
                    ));
                    return Err(());
                }
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
            match expected_of {
                Some(of) => {
                    self.coerce_untyped_expr(func_ctx.resolve_context.file_id, &mut value, of)?
                }
                None => {
                    self.tir
                        .diagnostics
                        .push(report_type_annotation_required(SourceSpan::new(
                            func_ctx.resolve_context.file_id,
                            value.span,
                        )));
                    return Err(());
                }
            }
        }
        if !value.ty.is_numeric() {
            self.tir.diagnostics.push(
                Diagnostic::error()
                    .with_message("array element type must be a numeric type")
                    .with_label(Label::primary(func_ctx.resolve_context.file_id, value.span)),
            );
            return Err(());
        }
        if !matches!(value.kind, ExprKind::Int { .. } | ExprKind::Float { .. }) {
            self.tir
                .diagnostics
                .push(report_array_element_not_const(SourceSpan::new(
                    func_ctx.resolve_context.file_id,
                    value.span,
                )));
            return Err(());
        }

        let memory = match expected_memory {
            Some(m) => m,
            None => self.resolve_ambient_memory(source_span)?,
        };
        let array_ty = self.intern_type(Type::Array {
            of: value.ty,
            size: count,
            mutable: expected_mutable,
            memory,
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
        func_ctx: &mut FunctionContext,
        span: ast::TextSpan,
        object_expr: &ast::Spanned<ast::Expression>,
        index_expr: &ast::Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let object = self.build_expression(
            func_ctx,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
            object_expr,
        )?;

        let (elem_type, memory, _mutable) = match self.tir.type_pool[object.ty.as_usize()].clone() {
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
                    SourceSpan::new(func_ctx.resolve_context.file_id, object.span),
                    TypeFormatter::new(&self.tir, &self.interner)
                        .display_type(object.ty)
                        .unwrap(),
                ));
                return Err(());
            }
        };

        let index_type = self.pointer_type_for_memory(memory);

        let mut index = self.build_expression(
            func_ctx,
            AccessContext {
                expected_type: Some(index_type),
                access_kind: AccessKind::Read,
            },
            index_expr,
        )?;
        if index.ty.is_comptime_number() {
            self.coerce_untyped_expr(func_ctx.resolve_context.file_id, &mut index, index_type)?;
        } else if index.ty != index_type {
            self.tir.diagnostics.push(report_type_mistmatch(
                TypeFormatter::new(&self.tir, &self.interner),
                TypeMistmatchDiagnostic {
                    expected_type: index_type,
                    actual_type: index.ty,
                    span: SourceSpan::new(func_ctx.resolve_context.file_id, index.span),
                },
            ));
        }

        Ok(Expression {
            kind: ExprKind::Index {
                object: Box::new(object),
                index: Box::new(index),
                memory,
            },
            ty: elem_type,
            span,
        })
    }

    fn build_slice_range_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        span: ast::TextSpan,
        object_expr: &ast::Spanned<ast::Expression>,
        start_expr: &Option<Box<ast::Spanned<ast::Expression>>>,
        end_expr: &Option<Box<ast::Spanned<ast::Expression>>>,
    ) -> Result<Expression, ()> {
        let object = self.build_expression(
            func_ctx,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
            object_expr,
        )?;

        let (elem_type, memory, mutable) = match self.tir.type_pool[object.ty.as_usize()].clone() {
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
                    SourceSpan::new(func_ctx.resolve_context.file_id, object.span),
                    TypeFormatter::new(&self.tir, &self.interner)
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
                    expected_type: Some(index_type),
                    access_kind: AccessKind::Read,
                },
                ast_expr,
            )?;
            if bound.ty.is_comptime_number() {
                builder.coerce_untyped_expr(
                    func_ctx.resolve_context.file_id,
                    &mut bound,
                    index_type,
                )?;
            } else if bound.ty != index_type {
                builder.tir.diagnostics.push(report_type_mistmatch(
                    TypeFormatter::new(&builder.tir, &builder.interner),
                    TypeMistmatchDiagnostic {
                        expected_type: index_type,
                        actual_type: bound.ty,
                        span: SourceSpan::new(func_ctx.resolve_context.file_id, ast_expr.span),
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
            mutable,
            memory,
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
