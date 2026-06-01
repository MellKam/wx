use std::collections::HashMap;

use crate::tir::*;

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
    // File id is still threaded for diagnostics outside resolver context.
    file_id: FileId,
    interner: &'interner mut ast::StringInterner,
    symbol_lookup: HashMap<(SymbolNamespace, SymbolU32), SymbolKind>,
    type_index_lookup: HashMap<Type, TypeIndex>,
    tir: TIR,
    id_generator: ast::DefIdGenerator,

    // ── demand-driven resolution ──────────────────────────────────────────────
    /// Populated in Phase 1.
    ast_nodes: HashMap<ast::DefId, AstNodeRef<'ast>>,
    /// Prevents re-entrant ensure_signature calls.
    sig_state: HashMap<ast::DefId, ComputeState>,
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
    InProgress,
    Done,
}

#[derive(Clone)]
struct TypeParamScope {
    owner: TypeParamOwner,
    params: Box<[TypeParamInfo]>,
}

#[derive(Clone)]
struct ResolveContext {
    file_id: FileId,
    module: Option<ModuleIndex>,
    self_type: Option<TypeIndex>,
    type_param_scope: Option<TypeParamScope>,
}

impl ResolveContext {
    fn root(file_id: FileId) -> Self {
        Self {
            file_id,
            module: None,
            self_type: None,
            type_param_scope: None,
        }
    }

    #[inline]
    fn is_root(&self) -> bool {
        self.module.is_none()
    }

    fn new(file_id: FileId, module: Option<ModuleIndex>) -> Self {
        Self {
            file_id,
            module,
            self_type: None,
            type_param_scope: None,
        }
    }

    fn with_type_param_scope(&self, scope: TypeParamScope) -> Self {
        let mut next = self.clone();
        next.type_param_scope = Some(scope);
        next
    }

    fn in_module(&self, module: ModuleIndex) -> Self {
        let mut next = self.clone();
        next.module = Some(module);
        next
    }

    fn with_self_type(&self, self_type: Option<TypeIndex>) -> Self {
        let mut next = self.clone();
        next.self_type = self_type;
        next
    }
}

/// AST node reference for demand-driven resolution.
#[derive(Clone)]
enum AstNodeRef<'ast> {
    /// Top-level `fn` or `#[intrinsic]` declaration.
    Function {
        file_id: FileId,
        module: Option<ModuleIndex>,
        item: &'ast ast::Item,
    },
    ImplMethod {
        file_id: FileId,
        module: Option<ModuleIndex>,
        impl_target: &'ast ast::Spanned<ast::TypeExpression>,
        item: &'ast ast::ImplItem,
    },
    /// `trait` function with or without a default body.
    TraitFunction {
        file_id: FileId,
        module: Option<ModuleIndex>,
        trait_index: u32,
        item: &'ast ast::TraitItem,
    },
    TraitConst {
        file_id: FileId,
        module: Option<ModuleIndex>,
        trait_index: u32,
        item: &'ast ast::TraitItem,
    },
    TraitAssociatedType {
        file_id: FileId,
        module: Option<ModuleIndex>,
        trait_index: u32,
        item: &'ast ast::TraitItem,
    },
    Struct {
        file_id: FileId,
        module: Option<ModuleIndex>,
        item: &'ast ast::Item,
    },
    Enum {
        file_id: FileId,
        module: Option<ModuleIndex>,
        item: &'ast ast::Item,
    },
    Global {
        file_id: FileId,
        module: Option<ModuleIndex>,
        item: &'ast ast::Item,
    },
    Memory {
        file_id: FileId,
        module: Option<ModuleIndex>,
        item: &'ast ast::Item,
    },
    Const {
        file_id: FileId,
        module: Option<ModuleIndex>,
        item: &'ast ast::Item,
    },
    ImplConst {
        file_id: FileId,
        module: Option<ModuleIndex>,
        impl_target: &'ast ast::Spanned<ast::TypeExpression>,
        item: &'ast ast::ImplItem,
    },
    /// Creates the `TraitImpl` entry when resolved.
    ImplTraitBlock {
        file_id: FileId,
        module: Option<ModuleIndex>,
        item: &'ast ast::Item,
    },
    ImplTraitMethod {
        file_id: FileId,
        module: Option<ModuleIndex>,
        /// Parent `ImplTraitBlock` must be resolved before this can insert into
        /// `TraitImpl.members`.
        parent_id: ast::DefId,
        item: &'ast ast::ImplItem,
    },
    ImplTraitConst {
        file_id: FileId,
        module: Option<ModuleIndex>,
        parent_id: ast::DefId,
        item: &'ast ast::ImplItem,
    },
    ImplTraitAssociatedType {
        file_id: FileId,
        module: Option<ModuleIndex>,
        parent_id: ast::DefId,
        item: &'ast ast::ImplItem,
    },
    Trait {
        file_id: FileId,
        module: Option<ModuleIndex>,
        trait_index: TraitIndex,
        item: &'ast ast::Item,
    },
    ImportedFunction {
        file_id: FileId,
        module: Option<ModuleIndex>,
        import_module_index: u32,
        decl: &'ast ast::ImportDeclaration,
    },
}

impl AstNodeRef<'_> {
    fn file_id(&self) -> FileId {
        match self {
            AstNodeRef::Function { file_id, .. }
            | AstNodeRef::ImplMethod { file_id, .. }
            | AstNodeRef::TraitFunction { file_id, .. }
            | AstNodeRef::TraitConst { file_id, .. }
            | AstNodeRef::TraitAssociatedType { file_id, .. }
            | AstNodeRef::Struct { file_id, .. }
            | AstNodeRef::Enum { file_id, .. }
            | AstNodeRef::Global { file_id, .. }
            | AstNodeRef::Memory { file_id, .. }
            | AstNodeRef::Const { file_id, .. }
            | AstNodeRef::ImplConst { file_id, .. }
            | AstNodeRef::ImplTraitBlock { file_id, .. }
            | AstNodeRef::ImplTraitMethod { file_id, .. }
            | AstNodeRef::ImplTraitConst { file_id, .. }
            | AstNodeRef::ImplTraitAssociatedType { file_id, .. }
            | AstNodeRef::Trait { file_id, .. }
            | AstNodeRef::ImportedFunction { file_id, .. } => *file_id,
        }
    }

    fn module(&self) -> Option<ModuleIndex> {
        match self {
            AstNodeRef::Function { module, .. }
            | AstNodeRef::ImplMethod { module, .. }
            | AstNodeRef::TraitFunction { module, .. }
            | AstNodeRef::TraitConst { module, .. }
            | AstNodeRef::TraitAssociatedType { module, .. }
            | AstNodeRef::Struct { module, .. }
            | AstNodeRef::Enum { module, .. }
            | AstNodeRef::Global { module, .. }
            | AstNodeRef::Memory { module, .. }
            | AstNodeRef::Const { module, .. }
            | AstNodeRef::ImplConst { module, .. }
            | AstNodeRef::ImplTraitBlock { module, .. }
            | AstNodeRef::ImplTraitMethod { module, .. }
            | AstNodeRef::ImplTraitConst { module, .. }
            | AstNodeRef::ImplTraitAssociatedType { module, .. }
            | AstNodeRef::Trait { module, .. }
            | AstNodeRef::ImportedFunction { module, .. } => *module,
        }
    }
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
        .with_note("expected `Memory32` or `Memory64`")
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
            fmt.display_type(diagnostic.expected_type),
            fmt.display_type(diagnostic.actual_type)
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
            fmt.display_type(diagnostic.ty)
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
            fmt.display_type(target_type)
        ))
        .with_label(span.primary_label())
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
            fmt.display_type(diagnostic.operand.inner)
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
    let left_type_name = fmt.display_type(diagnostic.left_type.inner);
    let right_type_name = fmt.display_type(diagnostic.right_type.inner);

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
        .with_note("declare a memory in this module: `memory <name>: Memory32;`")
}

fn report_ambiguous_pointer_memory(span: SourceSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::AmbiguousPointerMemory.code())
        .with_message("pointer dereference is ambiguous: multiple memories defined")
        .with_label(span.primary_label())
        .with_note("specify which memory with `<memory_name>::*T` syntax")
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
        SymbolKind::ImportModule { .. } => "import module",
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
            .map(|ty| fmt.display_type(*ty))
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

/// Returns true if casting from `from` to `to` is allowed without an explicit
/// unsafe annotation. Permitted conversions:
/// - integer ↔ integer (any combination of i8/u8/i16/u16/i32/u32/i64/u64)
/// - char → u8, u16, u32 (lossless widening; u32 is the natural repr of char)
/// - u8, u16 → char (narrow range guaranteed to be valid unicode scalar values)
/// - u32 → char is intentionally excluded (not all u32 values are valid chars)
fn is_numeric_cast(from: TypeIndex, to: TypeIndex) -> bool {
    let is_int = |idx: TypeIndex| {
        matches!(
            idx,
            Type::I8_IDX
                | Type::U8_IDX
                | Type::I16_IDX
                | Type::U16_IDX
                | Type::I32_IDX
                | Type::U32_IDX
                | Type::I64_IDX
                | Type::U64_IDX
        )
    };
    let is_char_compat = |idx: TypeIndex| matches!(idx, Type::U8_IDX | Type::U16_IDX);
    (is_int(from) && is_int(to))
        || (from == Type::CHAR_IDX && (is_char_compat(to) || to == Type::U32_IDX))
        || (is_char_compat(from) && to == Type::CHAR_IDX)
}

pub fn build(compilation: &CompilationGraph, interner: &mut ast::StringInterner) -> TIR {
    let source_modules: Vec<_> = compilation
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
        type_pool: Vec::new(),
        functions: Vec::new(),
        globals: Vec::new(),
        exports: HashMap::new(),
        import_modules: Vec::new(),
        enums: Vec::new(),
        modules: Vec::new(),
        impl_members: HashMap::new(),
        structs: Vec::new(),
        memories: Vec::new(),
        traits: Vec::new(),
        trait_impls: Vec::new(),
        trait_impl_lookup: HashMap::new(),
        type_trait_impls: HashMap::new(),
        function_index_lookup: HashMap::new(),
        global_index_lookup: HashMap::new(),
        memory_index_lookup: HashMap::new(),
        constants: Vec::new(),
        const_index_lookup: HashMap::new(),
    };

    let mut symbol_lookup = HashMap::new();
    symbol_lookup.insert(
        (SymbolNamespace::Value, interner.get_or_intern("_")),
        SymbolKind::Placeholder,
    );
    symbol_lookup.insert(
        (SymbolNamespace::Value, interner.get_or_intern("true")),
        SymbolKind::True,
    );
    symbol_lookup.insert(
        (SymbolNamespace::Value, interner.get_or_intern("false")),
        SymbolKind::False,
    );
    symbol_lookup.insert(
        (
            SymbolNamespace::Value,
            interner.get_or_intern("unreachable"),
        ),
        SymbolKind::Unreachable,
    );
    let mut builder = Builder {
        symbol_lookup,
        file_id: source_modules[0].1.file_id,
        interner,
        tir,
        trait_impl_block_lookup: HashMap::new(),
        type_index_lookup: HashMap::new(),
        sig_state: HashMap::new(),
        ast_nodes: HashMap::new(),
        id_generator: compilation.id_generator,
    };

    // Order MUST match the IDX constants defined at the top of this file.
    builder.intern_type(Type::Error);
    builder.intern_type(Type::Unit);
    builder.intern_type(Type::Never);
    builder.intern_type(Type::Unknown);
    builder.intern_type(Type::U8);
    builder.intern_type(Type::I8);
    builder.intern_type(Type::U16);
    builder.intern_type(Type::I16);
    builder.intern_type(Type::U32);
    builder.intern_type(Type::I32);
    builder.intern_type(Type::U64);
    builder.intern_type(Type::I64);
    builder.intern_type(Type::F32);
    builder.intern_type(Type::F64);
    builder.intern_type(Type::Bool);
    builder.intern_type(Type::Char);

    // Phase 1: register all top-level items into ast_nodes / pending
    for (crate_graph, source_module) in source_modules.iter().copied() {
        let module_path = crate_graph.module_symbol_path(source_module.id);
        let resolve_context = builder.ensure_module_path(source_module.file_id, &module_path);
        for item in source_module.ast.items.iter() {
            builder.pre_scan_item(
                resolve_context.clone(),
                &item.inner.inner,
                source_module.file_id,
            );
        }
    }

    // Phase 2: demand-resolve signatures for every registered def_id.
    // Sort by raw id so processing order is deterministic (parse order).
    let mut def_ids: Vec<ast::DefId> = builder.ast_nodes.keys().copied().collect();
    def_ids.sort_by_key(|id| id.as_u32());
    for def_id in &def_ids {
        builder.ensure_signature(*def_id);
    }

    // Phase 3: demand-resolve bodies for every registered def_id
    for def_id in &def_ids {
        builder.ensure_body(*def_id);
    }

    // Phase 3.5: verify every trait impl provides all required items
    builder.check_trait_conformance();

    // Phase 4: process exports (must run after all signatures are resolved)
    for (_, source_module) in source_modules.iter().copied() {
        builder.file_id = source_module.file_id;
        for item in source_module.ast.items.iter() {
            if let ast::Item::Export { entries } = &item.inner.inner {
                builder.build_exports(entries);
            }
        }
    }

    builder.report_unused_items();

    builder.tir
}

impl<'ast> Builder<'ast, '_> {
    fn intern_type(&mut self, ty: Type) -> TypeIndex {
        if let Some(&idx) = self.type_index_lookup.get(&ty) {
            idx
        } else {
            let idx = self.tir.type_pool.len() as u32;
            self.tir.type_pool.push(ty.clone());
            self.type_index_lookup.insert(ty, idx);
            idx
        }
    }

    pub fn coercible_to(&self, a: TypeIndex, b: TypeIndex) -> bool {
        a == b || a == Type::NEVER_IDX || a == Type::ERROR_IDX || b == Type::ERROR_IDX
    }

    fn unify(&self, a: TypeIndex, b: TypeIndex) -> Result<TypeIndex, ()> {
        if a == b {
            return Ok(a);
        }
        if a == Type::NEVER_IDX {
            return Ok(b);
        }
        if b == Type::NEVER_IDX {
            return Ok(a);
        }
        if a == Type::ERROR_IDX || b == Type::ERROR_IDX {
            return Ok(Type::ERROR_IDX);
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
                        None => Type::UNIT_IDX,
                    }))
                    .collect(),
                params_count: params.len(),
            },
        })
    }

    fn ensure_module(
        &mut self,
        resolve_context: &ResolveContext,
        file_id: FileId,
        name: ast::Spanned<SymbolU32>,
        pub_span: Option<ast::TextSpan>,
    ) -> ModuleIndex {
        let symbol = name.inner;
        if let Some(SymbolKind::Module { module_index }) = self
            .lookup_symbol(resolve_context, SymbolNamespace::Type, symbol)
            .cloned()
        {
            let module = &mut self.tir.modules[module_index as usize];
            if module.pub_span.is_none() {
                module.pub_span = pub_span;
            }
            return module_index;
        }

        let module_index = self.tir.modules.len() as u32;
        self.tir.modules.push(Module {
            file_id,
            name,
            pub_span,
            parent: resolve_context.module,
            symbol_lookup: HashMap::new(),
        });
        self.insert_symbol(
            resolve_context,
            (SymbolNamespace::Type, symbol),
            SymbolKind::Module { module_index },
        );
        module_index
    }

    fn ensure_module_path(&mut self, file_id: FileId, path: &[SymbolU32]) -> ResolveContext {
        let mut resolve_context = ResolveContext::root(file_id);

        for &segment in path {
            let module_index = self.ensure_module(
                &resolve_context,
                file_id,
                ast::Spanned {
                    inner: segment,
                    span: ast::TextSpan::new(0, 0),
                },
                None,
            );
            resolve_context = resolve_context.in_module(module_index);
        }

        resolve_context
    }

    fn insert_symbol(
        &mut self,
        resolve_context: &ResolveContext,
        key: (SymbolNamespace, SymbolU32),
        kind: SymbolKind,
    ) {
        if let Some(idx) = resolve_context.module {
            self.tir.modules[idx as usize]
                .symbol_lookup
                .insert(key, kind);
        } else {
            self.symbol_lookup.insert(key, kind);
        }
    }

    fn lookup_symbol(
        &self,
        resolve_context: &ResolveContext,
        ns: SymbolNamespace,
        sym: SymbolU32,
    ) -> Option<&SymbolKind> {
        let mut current = resolve_context.module;
        while let Some(idx) = current {
            if let Some(kind) = self.tir.modules[idx as usize].symbol_lookup.get(&(ns, sym)) {
                return Some(kind);
            }
            current = self.tir.modules[idx as usize].parent;
        }
        self.symbol_lookup.get(&(ns, sym))
    }

    fn symbol_kind_to_type(&mut self, kind: SymbolKind) -> Option<TypeIndex> {
        match kind {
            SymbolKind::ImportModule { module_index } => {
                Some(self.intern_type(Type::ImportModule { module_index }))
            }
            SymbolKind::Memory { kind, memory_index } => {
                let id = self.tir.memories[memory_index as usize].id;
                Some(self.intern_type(Type::Memory { kind, id }))
            }
            SymbolKind::Trait { trait_index } => {
                Some(self.intern_type(Type::Trait { trait_index }))
            }
            SymbolKind::Module { module_index } => {
                Some(self.intern_type(Type::Module { module_index }))
            }
            SymbolKind::Enum { enum_index } => Some(self.intern_type(Type::Enum { enum_index })),
            SymbolKind::Struct { struct_index } => {
                Some(self.intern_type(Type::Struct { struct_index }))
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
            SymbolKind::TraitAssocType { .. } | SymbolKind::Pending(_) => None,
            SymbolKind::False
            | SymbolKind::True
            | SymbolKind::Unreachable
            | SymbolKind::Placeholder => unreachable!(),
        }
    }

    pub fn resolve_type(
        &mut self,
        resolve_context: &ResolveContext,
        type_expr: &Spanned<ast::TypeExpression>,
    ) -> TypeIndex {
        match &type_expr.inner {
            ast::TypeExpression::Identifier { symbol } => {
                let symbol = *symbol;
                if let Ok(ty) = Type::try_from(self.interner.resolve(symbol).unwrap()) {
                    return self.intern_type(ty);
                }
                if let Some(scope) = &resolve_context.type_param_scope {
                    for (param_index, type_param) in scope.params.iter().enumerate() {
                        if type_param.name == symbol {
                            return self.intern_type(Type::TypeParam {
                                owner: scope.owner.clone(),
                                param_index: param_index as u32,
                            });
                        }
                    }
                }
                match self
                    .lookup_symbol(resolve_context, SymbolNamespace::Type, symbol)
                    .copied()
                {
                    Some(SymbolKind::Pending(def_id)) => {
                        if self.sig_state.get(&def_id) == Some(&ComputeState::InProgress) {
                            self.tir.diagnostics.push(report_cyclic_type_dependency(
                                SourceSpan::new(resolve_context.file_id, type_expr.span),
                            ));
                            return Type::ERROR_IDX;
                        }
                        self.ensure_signature(def_id);
                        match self
                            .lookup_symbol(resolve_context, SymbolNamespace::Type, symbol)
                            .cloned()
                        {
                            Some(SymbolKind::TraitAssocType { assoc_name, .. }) => {
                                let name = self.interner.resolve(assoc_name).unwrap_or("?");
                                self.tir.diagnostics.push(report_bare_assoc_type(
                                    SourceSpan::new(resolve_context.file_id, type_expr.span),
                                    name,
                                ));
                                return Type::ERROR_IDX;
                            }
                            Some(kind) => {
                                if let Some(ty) = self.symbol_kind_to_type(kind) {
                                    if let Type::Struct { struct_index } =
                                        self.tir.type_pool[ty as usize]
                                    {
                                        self.tir.structs[struct_index as usize].accesses.push(
                                            SourceSpan::new(
                                                resolve_context.file_id,
                                                type_expr.span,
                                            ),
                                        );
                                    }
                                    return ty;
                                }
                            }
                            None => {}
                        }
                        return Type::ERROR_IDX;
                    }
                    Some(SymbolKind::TraitAssocType { assoc_name, .. }) => {
                        let name = self.interner.resolve(assoc_name).unwrap_or("?");
                        self.tir.diagnostics.push(report_bare_assoc_type(
                            SourceSpan::new(resolve_context.file_id, type_expr.span),
                            name,
                        ));
                        return Type::ERROR_IDX;
                    }
                    Some(kind) => {
                        if let Some(ty) = self.symbol_kind_to_type(kind) {
                            if let Type::Struct { struct_index } = self.tir.type_pool[ty as usize] {
                                self.tir.structs[struct_index as usize]
                                    .accesses
                                    .push(SourceSpan::new(resolve_context.file_id, type_expr.span));
                            }
                            return ty;
                        }
                    }
                    None => {}
                }

                self.tir
                    .diagnostics
                    .push(report_undeclared_type(SourceSpan::new(
                        resolve_context.file_id,
                        type_expr.span,
                    )));
                Type::ERROR_IDX
            }
            ast::TypeExpression::Function { params, result } => {
                let result_idx = match result {
                    Some(result) => self.resolve_type(resolve_context, result),
                    None => Type::UNIT_IDX,
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
                self.intern_type(Type::Pointer {
                    to,
                    mutable: mutability.is_some(),
                    memory: None,
                })
            }
            ast::TypeExpression::Slice { mutability, inner } => {
                let of = self.resolve_type(resolve_context, inner);
                self.intern_type(Type::Slice {
                    of,
                    mutable: mutability.is_some(),
                    memory: None,
                })
            }
            ast::TypeExpression::Array {
                size,
                mutability,
                inner,
            } => {
                let of = self.resolve_type(resolve_context, inner);
                self.intern_type(Type::Array {
                    of,
                    size: size.inner as u32,
                    mutable: mutability.is_some(),
                    memory: None,
                })
            }
            ast::TypeExpression::Tuple { elements } => {
                let elems: Box<[TypeIndex]> = elements
                    .iter()
                    .map(|e| self.resolve_type(resolve_context, e))
                    .collect();
                self.intern_type(Type::Tuple { elements: elems })
            }
            ast::TypeExpression::SelfType => match resolve_context.self_type {
                Some(ty) => ty,
                None => {
                    self.tir.diagnostics.push(
                        Diagnostic::error()
                            .with_code(DiagnosticCode::UndeclaredType.code())
                            .with_message("`Self` is only valid inside an impl or trait block")
                            .with_label(Label::primary(resolve_context.file_id, type_expr.span)),
                    );
                    Type::ERROR_IDX
                }
            },
            ast::TypeExpression::NamespaceAccess { namespace, member } => {
                let namespace_ty = self.resolve_type(resolve_context, namespace);
                if namespace_ty == Type::ERROR_IDX {
                    return Type::ERROR_IDX;
                }

                match &self.tir.type_pool[namespace_ty as usize].clone() {
                    Type::TypeParam { param_index, .. } => {
                        let member_name = match &member.inner {
                            ast::TypeExpression::Identifier { symbol } => *symbol,
                            _ => {
                                self.tir.diagnostics.push(
                                    Diagnostic::error()
                                        .with_message(
                                            "memory-scoped pointer types are not yet supported",
                                        )
                                        .with_label(Label::primary(
                                            resolve_context.file_id,
                                            member.span,
                                        )),
                                );
                                return Type::ERROR_IDX;
                            }
                        };

                        let bounds = self
                            .type_param_bounds_in_context(resolve_context, namespace_ty)
                            .to_owned();

                        for &trait_index in &bounds {
                            // Ensure all trait member signatures (including assoc types) are
                            // resolved.
                            let def_ids =
                                self.tir.traits[trait_index as usize].member_def_ids.clone();
                            for def_id in def_ids {
                                self.ensure_signature(def_id);
                            }
                            if let Some(ImplEntry::AssociatedType { .. }) = self.tir.traits
                                [trait_index as usize]
                                .members
                                .get(&member_name)
                            {
                                return self.intern_type(Type::AssocTypeProjection {
                                    trait_index,
                                    assoc_name: member_name,
                                    param_index: *param_index,
                                });
                            }
                        }

                        self.tir
                            .diagnostics
                            .push(report_undeclared_type(SourceSpan::new(
                                resolve_context.file_id,
                                member.span,
                            )));
                        Type::ERROR_IDX
                    }
                    Type::Module { module_index } => {
                        // `module::Type`
                        let member_name = match &member.inner {
                            ast::TypeExpression::Identifier { symbol } => *symbol,
                            _ => {
                                self.tir.diagnostics.push(
                                    Diagnostic::error()
                                        .with_message("expected a type name after `::`")
                                        .with_label(Label::primary(
                                            resolve_context.file_id,
                                            member.span,
                                        )),
                                );
                                return Type::ERROR_IDX;
                            }
                        };

                        let kind = self.tir.modules.get(*module_index as usize).and_then(|m| {
                            m.symbol_lookup
                                .get(&(SymbolNamespace::Type, member_name))
                                .cloned()
                        });

                        match kind {
                            Some(SymbolKind::Pending(def_id)) => {
                                self.ensure_signature(def_id);
                                let kind = self.tir.modules[*module_index as usize]
                                    .symbol_lookup
                                    .get(&(SymbolNamespace::Type, member_name))
                                    .cloned();
                                match kind {
                                    Some(k) => {
                                        self.symbol_kind_to_type(k).unwrap_or(Type::ERROR_IDX)
                                    }
                                    None => Type::ERROR_IDX,
                                }
                            }
                            Some(k) => self.symbol_kind_to_type(k).unwrap_or(Type::ERROR_IDX),
                            None => {
                                self.tir
                                    .diagnostics
                                    .push(report_undeclared_type(SourceSpan::new(
                                        resolve_context.file_id,
                                        member.span,
                                    )));
                                Type::ERROR_IDX
                            }
                        }
                    }
                    Type::Memory { id, .. } => {
                        // `memory::*T`, `memory::[]T`, `memory::[N]T`
                        let member_ty = self.resolve_type(resolve_context, member);
                        if member_ty == Type::ERROR_IDX {
                            return Type::ERROR_IDX;
                        }
                        match self.tir.type_pool[member_ty as usize].clone() {
                            Type::Pointer { to, mutable, .. } => self.intern_type(Type::Pointer {
                                to,
                                mutable,
                                memory: Some(*id),
                            }),
                            Type::Slice { of, mutable, .. } => self.intern_type(Type::Slice {
                                of,
                                mutable,
                                memory: Some(*id),
                            }),
                            Type::Array {
                                of, size, mutable, ..
                            } => self.intern_type(Type::Array {
                                of,
                                size,
                                mutable,
                                memory: Some(*id),
                            }),
                            _ => {
                                self.tir.diagnostics.push(
                                    Diagnostic::error()
                                        .with_message(
                                            "memory namespace can only prefix pointer, slice, or array types",
                                        )
                                        .with_label(Label::primary(resolve_context.file_id, member.span)),
                                );
                                Type::ERROR_IDX
                            }
                        }
                    }
                    _ => {
                        self.tir.diagnostics.push(
                            Diagnostic::error()
                                .with_message(format!(
                                    "`{}` is not a valid namespace for type access",
                                    TypeFormatter {
                                        interner: &self.interner,
                                        tir: &self.tir,
                                    }
                                    .display_type(namespace_ty)
                                ))
                                .with_label(Label::primary(
                                    resolve_context.file_id,
                                    namespace.span,
                                )),
                        );
                        Type::ERROR_IDX
                    }
                }
            }
            ast::TypeExpression::ImplTrait { name } => {
                // Full APIT support requires monomorphisation; for now we record the
                // trait type so callers can at least see the constraint.
                if let Some(SymbolKind::Pending(def_id)) = self
                    .lookup_symbol(resolve_context, SymbolNamespace::Type, name.inner)
                    .cloned()
                {
                    self.ensure_signature(def_id);
                }
                match self
                    .lookup_symbol(resolve_context, SymbolNamespace::Type, name.inner)
                    .cloned()
                {
                    Some(SymbolKind::Trait { trait_index }) => {
                        self.intern_type(Type::Trait { trait_index })
                    }
                    Some(SymbolKind::Pending(_)) => Type::ERROR_IDX,
                    Some(kind) => {
                        let name_str = self.interner.resolve(name.inner).unwrap().to_string();
                        self.tir.diagnostics.push(report_expected_trait(
                            SourceSpan::new(resolve_context.file_id, name.span),
                            kind,
                            name_str,
                        ));
                        Type::ERROR_IDX
                    }
                    None => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_type(SourceSpan::new(
                                resolve_context.file_id,
                                name.span,
                            )));
                        Type::ERROR_IDX
                    }
                }
            }
            ast::TypeExpression::GenericApplication { name, args } => {
                // TODO: resolve as the correct generic type (struct or trait) and
                // validate that binding args are only used with traits.
                let base = Spanned {
                    inner: ast::TypeExpression::Identifier { symbol: name.inner },
                    span: name.span,
                };
                let base_ty = self.resolve_type(resolve_context, &base);
                // Eagerly resolve all arg types to surface undeclared-type errors.
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
                base_ty
            }
        }
    }

    fn resolve_function_attributes(&self, attrs: &[ast::Attribute]) -> Box<[FunctionAttribute]> {
        attrs
            .iter()
            .filter_map(|a| match self.interner.resolve(a.name.inner) {
                Some("inline") => Some(FunctionAttribute::Inline),
                Some("intrinsic") => Some(FunctionAttribute::Intrinsic),
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
            TypeParamOwner::Struct(_) => unreachable!(),
        }
    }

    fn type_param_bounds_in_context<'a>(
        &'a self,
        resolve_context: &'a ResolveContext,
        ty: TypeIndex,
    ) -> &'a [TraitIndex] {
        match &self.tir.type_pool[ty as usize] {
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
            SymbolKind::ImportModule { module_index } => {
                let module = &self.tir.import_modules[module_index as usize];
                SourceSpan::new(
                    module.file_id,
                    match &module.internal_name {
                        Some(internal_name) => internal_name.span,
                        None => module.external_name.span,
                    },
                )
            }
            SymbolKind::Enum { enum_index } => {
                let enum_ = &self.tir.enums[enum_index as usize];
                SourceSpan::new(enum_.file_id, enum_.name.span)
            }
            SymbolKind::Struct { struct_index } => {
                let s = &self.tir.structs[struct_index as usize];
                SourceSpan::new(s.file_id, s.name.span)
            }
            SymbolKind::Module { module_index } => {
                let module = &self.tir.modules[module_index as usize];
                SourceSpan::new(module.file_id, module.name.span)
            }
            SymbolKind::Trait { trait_index } => {
                let trait_ = &self.tir.traits[trait_index as usize];
                SourceSpan::new(trait_.file_id, trait_.name.span)
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
        resolve_context: ResolveContext,
        item: &'ast ast::Item,
        file_id: FileId,
    ) {
        let module = resolve_context.module;

        match item {
            ast::Item::Function { id, signature, .. }
            | ast::Item::FunctionDeclaration { id, signature, .. } => {
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Value, signature.name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.insert(
                    *id,
                    AstNodeRef::Function {
                        file_id,
                        module,
                        item,
                    },
                );
            }
            ast::Item::Global { id, name, .. } => {
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Value, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.insert(
                    *id,
                    AstNodeRef::Global {
                        file_id,
                        module,
                        item,
                    },
                );
            }
            ast::Item::Struct { id, name, .. } => {
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Type, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.insert(
                    *id,
                    AstNodeRef::Struct {
                        file_id,
                        module,
                        item,
                    },
                );
            }
            ast::Item::Enum { id, name, .. } => {
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Type, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.insert(
                    *id,
                    AstNodeRef::Enum {
                        file_id,
                        module,
                        item,
                    },
                );
            }
            ast::Item::Memory { id, name, .. } => {
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Type, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Value, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.insert(
                    *id,
                    AstNodeRef::Memory {
                        file_id,
                        module,
                        item,
                    },
                );
            }
            ast::Item::Const { id, name, .. } => {
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Value, name.inner),
                    SymbolKind::Pending(*id),
                );
                self.ast_nodes.insert(
                    *id,
                    AstNodeRef::Const {
                        file_id,
                        module,
                        item,
                    },
                );
            }
            ast::Item::Module {
                name,
                items,
                pub_span,
            } => {
                // Modules are structural — register eagerly and recurse.
                let module_index =
                    self.ensure_module(&resolve_context, file_id, name.clone(), *pub_span);
                let child_context = resolve_context.in_module(module_index);
                for child in items.iter() {
                    self.pre_scan_item(child_context.clone(), &child.inner.inner, file_id);
                }
            }
            ast::Item::ModuleDeclaration { name, pub_span } => {
                self.ensure_module(&resolve_context, file_id, name.clone(), *pub_span);
            }
            ast::Item::Trait {
                id, name, items, ..
            } => {
                // Traits are structural containers; allocate the slot and register
                // each item's DefId so ensure_signature can fill it in on demand.
                let trait_index = self.tir.traits.len() as u32;
                self.tir.traits.push(Trait {
                    file_id,
                    name: name.clone(),
                    supertraits: Vec::new(),
                    members: HashMap::new(),
                    assoc_type_bounds: HashMap::new(),
                    member_def_ids: Vec::new(),
                    supertrait_bindings: HashMap::new(),
                });
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Type, name.inner),
                    SymbolKind::Trait { trait_index },
                );
                self.ast_nodes.insert(
                    *id,
                    AstNodeRef::Trait {
                        file_id,
                        module,
                        trait_index,
                        item,
                    },
                );
                let mut ids: Vec<ast::DefId> = Vec::new();
                for ti in items.iter() {
                    match &ti.inner.inner {
                        ast::TraitItem::Function { id, signature, .. } => {
                            self.insert_symbol(
                                &resolve_context,
                                (SymbolNamespace::Value, signature.name.inner),
                                SymbolKind::Pending(*id),
                            );
                            self.ast_nodes.insert(
                                *id,
                                AstNodeRef::TraitFunction {
                                    file_id,
                                    module,
                                    trait_index,
                                    item: &ti.inner.inner,
                                },
                            );
                            ids.push(*id);
                        }
                        ast::TraitItem::Const { id, name, .. } => {
                            self.insert_symbol(
                                &resolve_context,
                                (SymbolNamespace::Value, name.inner),
                                SymbolKind::Pending(*id),
                            );
                            self.ast_nodes.insert(
                                *id,
                                AstNodeRef::TraitConst {
                                    file_id,
                                    module,
                                    trait_index,
                                    item: &ti.inner.inner,
                                },
                            );
                            ids.push(*id);
                        }
                        ast::TraitItem::AssociatedType { id, name, .. } => {
                            self.insert_symbol(
                                &resolve_context,
                                (SymbolNamespace::Type, name.inner),
                                SymbolKind::Pending(*id),
                            );
                            self.ast_nodes.insert(
                                *id,
                                AstNodeRef::TraitAssociatedType {
                                    file_id,
                                    module,
                                    trait_index,
                                    item: &ti.inner.inner,
                                },
                            );
                            ids.push(*id);
                        }
                    }
                }
                self.tir.traits[trait_index as usize].member_def_ids = ids;
            }
            ast::Item::Impl { target, items } => {
                for impl_item in items.iter() {
                    match &impl_item.inner.inner {
                        ast::ImplItem::Method { id, .. } => {
                            self.ast_nodes.insert(
                                *id,
                                AstNodeRef::ImplMethod {
                                    file_id,
                                    module,
                                    impl_target: target,
                                    item: &impl_item.inner.inner,
                                },
                            );
                        }
                        ast::ImplItem::Const { id, .. } => {
                            self.ast_nodes.insert(
                                *id,
                                AstNodeRef::ImplConst {
                                    file_id,
                                    module,
                                    impl_target: target,
                                    item: &impl_item.inner.inner,
                                },
                            );
                        }
                        ast::ImplItem::AssociatedType { name, .. } => {
                            self.tir
                                .diagnostics
                                .push(report_associated_type_in_inherent_impl(SourceSpan::new(
                                    file_id, name.span,
                                )));
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
                let module_index = self.tir.import_modules.len() as u32;
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
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Type, module_sym),
                    SymbolKind::ImportModule { module_index },
                );
                for entry in entries.iter() {
                    match &entry.inner.inner.declaration {
                        ast::ImportDeclaration::Function { id, .. } => {
                            self.ast_nodes.insert(
                                *id,
                                AstNodeRef::ImportedFunction {
                                    file_id,
                                    module,
                                    import_module_index: module_index,
                                    decl: &entry.inner.inner.declaration,
                                },
                            );
                        }
                        ast::ImportDeclaration::Global { id, name, .. } => {
                            self.insert_symbol(
                                &resolve_context,
                                (SymbolNamespace::Value, name.inner),
                                SymbolKind::Pending(*id),
                            );
                            self.ast_nodes.insert(
                                *id,
                                AstNodeRef::Global {
                                    file_id,
                                    module,
                                    item,
                                },
                            );
                        }
                        ast::ImportDeclaration::Memory { id, name, .. } => {
                            self.insert_symbol(
                                &resolve_context,
                                (SymbolNamespace::Type, name.inner),
                                SymbolKind::Pending(*id),
                            );
                            self.insert_symbol(
                                &resolve_context,
                                (SymbolNamespace::Value, name.inner),
                                SymbolKind::Pending(*id),
                            );
                            self.ast_nodes.insert(
                                *id,
                                AstNodeRef::Memory {
                                    file_id,
                                    module,
                                    item,
                                },
                            );
                        }
                    }
                }
                self.tir.import_modules.push(ImportModule {
                    file_id,
                    external_name,
                    internal_name: alias.clone(),
                    lookup: HashMap::new(),
                });
            }
            ast::Item::Export { .. } => {} // handled during build pass
            ast::Item::ImplTrait { id, items, .. } => {
                self.ast_nodes.insert(
                    *id,
                    AstNodeRef::ImplTraitBlock {
                        file_id,
                        module,
                        item,
                    },
                );
                for mi in items.iter() {
                    match &mi.inner.inner {
                        ast::ImplItem::Method { id: method_id, .. } => {
                            self.ast_nodes.insert(
                                *method_id,
                                AstNodeRef::ImplTraitMethod {
                                    file_id,
                                    module,
                                    parent_id: *id,
                                    item: &mi.inner.inner,
                                },
                            );
                        }
                        ast::ImplItem::Const { id: const_id, .. } => {
                            self.ast_nodes.insert(
                                *const_id,
                                AstNodeRef::ImplTraitConst {
                                    file_id,
                                    module,
                                    parent_id: *id,
                                    item: &mi.inner.inner,
                                },
                            );
                        }
                        ast::ImplItem::AssociatedType { id: type_id, .. } => {
                            self.ast_nodes.insert(
                                *type_id,
                                AstNodeRef::ImplTraitAssociatedType {
                                    file_id,
                                    module,
                                    parent_id: *id,
                                    item: &mi.inner.inner,
                                },
                            );
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
        match self.sig_state.get(&def_id) {
            Some(ComputeState::Done) => return,
            Some(ComputeState::InProgress) => return, // cycle already reported by resolve_type
            None => {}
        }
        self.sig_state.insert(def_id, ComputeState::InProgress);

        let node = match self.ast_nodes.get(&def_id).cloned() {
            Some(n) => n,
            None => {
                self.sig_state.insert(def_id, ComputeState::Done);
                return;
            }
        };

        let resolve_context = ResolveContext::new(node.file_id(), node.module());

        // Set up context for this node.
        let saved_file_id = self.file_id;
        self.file_id = node.file_id();

        match node.clone() {
            // ── struct ────────────────────────────────────────────────────────
            AstNodeRef::Struct { item, .. } => {
                let (id, pub_span, name, fields) = match item {
                    ast::Item::Struct {
                        id,
                        pub_span,
                        name,
                        fields,
                        ..
                    } => (id, pub_span, name, fields),
                    _ => unreachable!(),
                };
                // Duplicate check.
                if let Some(existing) = self
                    .lookup_symbol(&resolve_context, SymbolNamespace::Type, name.inner)
                    .filter(|k| !matches!(k, SymbolKind::Pending(_)))
                    .cloned()
                {
                    let first_definition = match existing {
                        SymbolKind::Struct { struct_index } => {
                            let struct_ = &self.tir.structs[struct_index as usize];
                            SourceSpan::new(struct_.file_id, struct_.name.span)
                        }
                        _ => SourceSpan::new(self.file_id, name.span),
                    };
                    let name_str = self.interner.resolve(name.inner).unwrap();
                    self.tir.diagnostics.push(report_duplicate_definition(
                        DuplicateDefinitionDiagnostic {
                            name: name_str,
                            namespace: SymbolNamespace::Type,
                            first_definition,
                            second_definition: SourceSpan::new(self.file_id, name.span),
                        },
                    ));
                    self.file_id = saved_file_id;
                    self.sig_state.insert(*id, ComputeState::Done);
                    return;
                }

                // Resolve all field types (may recursively call ensure_signature for
                // referenced structs; cycles are detected via InProgress state).
                let mut seen_fields: HashMap<SymbolU32, ast::TextSpan> = HashMap::new();
                let mut tir_fields: Vec<StructField> = Vec::new();
                let mut field_lookup: HashMap<SymbolU32, usize> = HashMap::new();

                for f in fields.iter() {
                    let field = &f.inner.inner;
                    let sym = field.name.inner;
                    if let Some(&first_span) = seen_fields.get(&sym) {
                        let fname = self.interner.resolve(sym).unwrap().to_string();
                        self.tir.diagnostics.push(report_duplicate_struct_field(
                            &fname,
                            SourceSpan::new(self.file_id, first_span),
                            SourceSpan::new(self.file_id, field.name.span),
                        ));
                        continue;
                    }
                    let field_ty = self.resolve_type(&resolve_context, &field.ty);
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

                let struct_index = self.tir.structs.len() as u32;
                self.tir.structs.push(Struct {
                    file_id: self.file_id,
                    pub_span: *pub_span,
                    name: name.clone(),
                    fields: tir_fields.into_boxed_slice(),
                    lookup: field_lookup,
                    accesses: Vec::new(),
                });
                self.insert_symbol(
                    &resolve_context,
                    (SymbolNamespace::Type, name.inner),
                    SymbolKind::Struct { struct_index },
                );
                let _ = pub_span;
            }

            // ── enum ──────────────────────────────────────────────────────────
            AstNodeRef::Enum { item, .. } => {
                // TODO: full enum lowering; for now just register the name so
                // resolve_type can find it.
                if let ast::Item::Enum {
                    id,
                    pub_span,
                    name,
                    repr,
                    ..
                } = item
                {
                    if !matches!(
                        self.lookup_symbol(&resolve_context, SymbolNamespace::Type, name.inner),
                        Some(k) if !matches!(k, SymbolKind::Pending(_))
                    ) {
                        let enum_index = self.tir.enums.len() as u32;
                        let ty = match repr {
                            Some(r) => self.resolve_type(&resolve_context, &**r),
                            None => {
                                self.tir.diagnostics.push(report_missing_enum_repr(
                                    SourceSpan::new(self.file_id, name.span),
                                ));
                                Type::ERROR_IDX
                            }
                        };
                        self.tir.enums.push(Enum {
                            file_id: self.file_id,
                            pub_span: *pub_span,
                            name: name.clone(),
                            ty,
                            variants: Box::new([]),
                            lookup: HashMap::new(),
                        });
                        self.insert_symbol(
                            &resolve_context,
                            (SymbolNamespace::Type, name.inner),
                            SymbolKind::Enum { enum_index },
                        );
                        let _ = id;
                    }
                }
            }

            // ── free function / function declaration ──────────────────────────
            AstNodeRef::Function { item, .. } => {
                match item {
                    ast::Item::Function {
                        id,
                        signature,
                        attributes,
                        pub_span,
                        ..
                    } => {
                        let existing_span = self
                            .lookup_symbol(
                                &resolve_context,
                                SymbolNamespace::Value,
                                signature.name.inner,
                            )
                            .filter(|k| !matches!(k, SymbolKind::Pending(_)))
                            .cloned()
                            .map(|k| self.get_symbol_location(k));
                        let type_params =
                            self.resolve_ast_type_params(&resolve_context, &signature.type_params);
                        let signature_context =
                            resolve_context.with_type_param_scope(TypeParamScope {
                                owner: TypeParamOwner::Function(*id),
                                params: type_params.clone(),
                            });
                        let (params, result) =
                            self.build_function_signature(&signature_context, signature);
                        let signature_index = self.intern_function(&params, result.clone());
                        let func_index = self.tir.functions.len() as u32;
                        let origin = if resolve_context.is_root() {
                            FunctionOrigin::Free
                        } else {
                            FunctionOrigin::Module
                        };
                        self.tir.functions.push(Function {
                            id: *id,
                            file_id: self.file_id,
                            body: None,
                            origin,
                            type_params,
                            pub_span: *pub_span,
                            source: ItemSource::Internal,
                            signature_index,
                            name: signature.name.clone(),
                            accesses: Vec::new(),
                            params,
                            result,
                            attributes: self.resolve_function_attributes(attributes),
                        });
                        self.tir.function_index_lookup.insert(*id, func_index);
                        match existing_span {
                            Some(first_definition) => {
                                let name = self.interner.resolve(signature.name.inner).unwrap();
                                self.tir.diagnostics.push(report_duplicate_definition(
                                    DuplicateDefinitionDiagnostic {
                                        name,
                                        namespace: SymbolNamespace::Value,
                                        first_definition,
                                        second_definition: SourceSpan::new(
                                            self.file_id,
                                            signature.name.span,
                                        ),
                                    },
                                ));
                            }
                            None => {
                                self.insert_symbol(
                                    &resolve_context,
                                    (SymbolNamespace::Value, signature.name.inner),
                                    SymbolKind::Function { func_index },
                                );
                            }
                        }
                    }
                    ast::Item::FunctionDeclaration {
                        id,
                        attributes,
                        signature,
                        pub_span,
                        ..
                    } => {
                        let attributes = self.resolve_function_attributes(attributes);
                        if !attributes
                            .iter()
                            .any(|&a| a == FunctionAttribute::Intrinsic)
                        {
                            self.tir.diagnostics.push(report_missing_function_body(
                                SourceSpan::new(self.file_id, signature.name.span),
                            ));
                        } else {
                            let type_params = self
                                .resolve_ast_type_params(&resolve_context, &signature.type_params);
                            let signature_context =
                                resolve_context.with_type_param_scope(TypeParamScope {
                                    owner: TypeParamOwner::Function(*id),
                                    params: type_params.clone(),
                                });
                            let (params, result) =
                                self.build_function_signature(&signature_context, signature);
                            let signature_index = self.intern_function(&params, result.clone());
                            let func_index = self.tir.functions.len() as u32;
                            let origin = if resolve_context.is_root() {
                                FunctionOrigin::Free
                            } else {
                                FunctionOrigin::Module
                            };
                            self.tir.functions.push(Function {
                                id: *id,
                                file_id: self.file_id,
                                body: None,
                                origin,
                                type_params,
                                pub_span: *pub_span,
                                source: ItemSource::Internal,
                                signature_index,
                                name: signature.name.clone(),
                                accesses: Vec::new(),
                                params,
                                result,
                                attributes,
                            });
                            self.tir.function_index_lookup.insert(*id, func_index);
                            self.insert_symbol(
                                &resolve_context,
                                (SymbolNamespace::Value, signature.name.inner),
                                SymbolKind::Function { func_index },
                            );
                        }
                    }
                    _ => {}
                }
            }

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
                        None => Type::UNKNOWN_IDX,
                    };
                    if let Ok(value_expr) =
                        self.build_const_expression(&resolve_context, value, resolved_ty)
                    {
                        let const_index = self.tir.constants.len() as ConstIndex;
                        self.tir.constants.push(Constant {
                            id: *id,
                            file_id: self.file_id,
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
                                        inner: if is_self { self_type } else { Type::ERROR_IDX },
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
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: self.file_id,
                        body: None,
                        type_params: Box::new([]),
                        pub_span: *pub_span,
                        origin: FunctionOrigin::Impl,
                        source: ItemSource::Internal,
                        signature_index,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params,
                        result,
                        attributes: self.resolve_function_attributes(attributes),
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

            // ── trait block (supertrait resolution) ───────────────────────────
            AstNodeRef::Trait {
                trait_index, item, ..
            } => {
                let supertraits = match item {
                    ast::Item::Trait { supertraits, .. } => supertraits,
                    _ => unreachable!(),
                };

                let resolved: Vec<TraitIndex> = supertraits
                    .iter()
                    .filter_map(|bound| {
                        let type_idx = self.resolve_type(&resolve_context, bound);
                        match &self.tir.type_pool[type_idx as usize] {
                            Type::Trait { trait_index } => Some(*trait_index),
                            _ if type_idx == Type::ERROR_IDX => None,
                            _ => {
                                self.tir.diagnostics.push(
                                    Diagnostic::error()
                                        .with_code(DiagnosticCode::ExpectedTrait.code())
                                        .with_message("expected a trait name")
                                        .with_label(Label::primary(self.file_id, bound.span)),
                                );
                                None
                            }
                        }
                    })
                    .collect();

                // Extract assoc-type bindings from GenericApplication supertraits.
                let mut bindings: HashMap<(TraitIndex, SymbolU32), TypeIndex> = HashMap::new();
                for (bound, &st) in supertraits.iter().zip(resolved.iter()) {
                    if let ast::TypeExpression::GenericApplication { args, .. } = &bound.inner {
                        for sep in args.iter() {
                            if let ast::GenericArg::Binding {
                                name: key,
                                ty: val_te,
                            } = &sep.inner.inner
                            {
                                let val_ty = self.resolve_type(&resolve_context, val_te);
                                bindings.insert((st, key.inner), val_ty);
                            }
                        }
                    }
                }
                self.tir.traits[trait_index as usize].supertraits = resolved;
                self.tir.traits[trait_index as usize].supertrait_bindings = bindings;
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
                    let attributes = self.resolve_function_attributes(attributes);

                    // Self occupies slot 0; any explicit generics on the method start at 1.
                    let self_name_sym = self.interner.get_or_intern("Self");
                    let self_type_param = TypeParamInfo {
                        name: self_name_sym,
                        bounds: Box::new([trait_index]),
                    };
                    let explicit_type_params =
                        self.resolve_ast_type_params(&resolve_context, &signature.type_params);
                    let type_params: Box<[TypeParamInfo]> = std::iter::once(self_type_param)
                        .chain(explicit_type_params)
                        .collect();
                    let resolve_context = resolve_context.with_type_param_scope(TypeParamScope {
                        owner: TypeParamOwner::Function(*id),
                        params: type_params.clone(),
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
                                            Type::ERROR_IDX
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
                    let func_index = self.tir.functions.len() as u32;
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: self.file_id,
                        body: None,
                        pub_span: None,
                        origin: FunctionOrigin::Trait,
                        type_params,
                        source: ItemSource::Internal,
                        signature_index: sig_idx,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params: params.clone(),
                        result: result.clone(),
                        attributes: attributes.clone(),
                    });
                    self.tir.function_index_lookup.insert(*id, func_index);
                    self.tir.traits[trait_index as usize]
                        .members
                        .insert(signature.name.inner, ImplEntry::Method(func_index));
                }
            }

            // ── trait const ───────────────────────────────────────────────────
            AstNodeRef::TraitConst {
                trait_index, item, ..
            } => {
                let resolve_context = resolve_context
                    .with_self_type(Some(self.intern_type(Type::Trait { trait_index })));
                if let ast::TraitItem::Const { id, name, ty } = item {
                    let ty_idx = self.resolve_type(&resolve_context, ty);
                    let const_index = self.tir.constants.len() as ConstIndex;
                    self.tir.constants.push(Constant {
                        id: *id,
                        file_id: self.file_id,
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
            AstNodeRef::Global { item, .. } => {
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
                        .lookup_symbol(&resolve_context, SymbolNamespace::Value, name.inner)
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
                                second_definition: SourceSpan::new(self.file_id, name.span),
                            },
                        ));
                    } else {
                        let (ty, ty_span) = match ty {
                            Some(ty) => (self.resolve_type(&resolve_context, ty), ty.span),
                            None => {
                                self.tir.diagnostics.push(report_type_annotation_required(
                                    SourceSpan::new(self.file_id, name.span),
                                ));
                                (Type::ERROR_IDX, name.span)
                            }
                        };
                        let global_index = self.tir.globals.len() as u32;
                        self.insert_symbol(
                            &resolve_context,
                            (SymbolNamespace::Value, name.inner),
                            SymbolKind::Global { global_index },
                        );
                        self.tir.globals.push(Global {
                            id: *id,
                            file_id: self.file_id,
                            value: None,
                            source: ItemSource::Internal,
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
            AstNodeRef::Memory { item, .. } => {
                if let ast::Item::Memory {
                    name,
                    kind,
                    id,
                    config,
                } = item
                {
                    // Resolve the trait type and ensure all its functions are
                    // registered before seed_memory_trait_impl reads them.
                    let type_idx = self.resolve_type(&resolve_context, kind);
                    let trait_index =
                        match self.tir.type_pool[type_idx as usize] {
                            Type::Trait { trait_index } => trait_index,
                            _ => {
                                self.tir.diagnostics.push(report_invalid_memory_kind(
                                    SourceSpan::new(self.file_id, kind.span),
                                ));
                                return;
                            }
                        };
                    let trait_fn_ids = self.tir.traits[trait_index as usize].member_def_ids.clone();
                    for tid in trait_fn_ids {
                        self.ensure_signature(tid);
                    }
                    let memory_kind =
                        match self
                            .interner
                            .resolve(self.tir.traits[trait_index as usize].name.inner)
                            .unwrap()
                        {
                            "Memory32" => MemoryKind::Memory32,
                            "Memory64" => MemoryKind::Memory64,
                            _ => {
                                self.tir.diagnostics.push(report_invalid_memory_kind(
                                    SourceSpan::new(self.file_id, kind.span),
                                ));
                                return;
                            }
                        };
                    let memory_index = self.tir.memories.len() as u32;
                    self.tir.memories.push(Memory {
                        id: *id,
                        file_id: self.file_id,
                        kind: memory_kind,
                        name: name.clone(),
                        source: ItemSource::Internal,
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
                    self.seed_memory_trait_impl(trait_index, memory_type);
                    self.insert_symbol(
                        &resolve_context,
                        (SymbolNamespace::Type, name.inner),
                        SymbolKind::Memory {
                            memory_index,
                            kind: memory_kind,
                        },
                    );
                    self.insert_symbol(
                        &resolve_context,
                        (SymbolNamespace::Value, name.inner),
                        SymbolKind::Memory {
                            memory_index,
                            kind: memory_kind,
                        },
                    );
                }
            }

            // ── const ─────────────────────────────────────────────────────────
            AstNodeRef::Const { item, .. } => {
                if let ast::Item::Const {
                    id,
                    pub_span,
                    name,
                    ty,
                    value,
                } = item
                {
                    if let Some(first_def) = self
                        .lookup_symbol(&resolve_context, SymbolNamespace::Value, name.inner)
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
                                second_definition: SourceSpan::new(self.file_id, name.span),
                            },
                        ));
                    } else {
                        let (ty_idx, ty_span) = match ty {
                            Some(ty) => (self.resolve_type(&resolve_context, ty), ty.span),
                            None => {
                                self.tir.diagnostics.push(report_type_annotation_required(
                                    SourceSpan::new(self.file_id, name.span),
                                ));
                                (Type::ERROR_IDX, name.span)
                            }
                        };
                        if let Ok(value_expr) =
                            self.build_const_expression(&resolve_context, value, ty_idx)
                        {
                            let const_index = self.tir.constants.len() as ConstIndex;
                            self.tir.constants.push(Constant {
                                id: *id,
                                file_id: self.file_id,
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
                                &resolve_context,
                                (SymbolNamespace::Value, name.inner),
                                SymbolKind::Const { const_index },
                            );
                        }
                    }
                }
            }

            // ── imported function ─────────────────────────────────────────────
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
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: self.file_id,
                        source: ItemSource::External,
                        origin: FunctionOrigin::Free,
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
                    self.tir.import_modules[import_module_index as usize]
                        .lookup
                        .insert(signature.name.inner, ImportValue::Function { id: *id });
                }
            }

            // ── impl trait block ─────────────────────────────────────────────
            // Resolves the trait and target types, creates the `TraitImpl`
            // entry, and populates the lookup tables. Methods and consts
            // demand-drive this arm before inserting into `TraitImpl.members`.
            AstNodeRef::ImplTraitBlock { item, .. } => {
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
                let trait_index = match &self.tir.type_pool[trait_type as usize] {
                    Type::Trait { trait_index } => *trait_index,
                    _ if trait_type == Type::ERROR_IDX => return,
                    _ => {
                        self.tir.diagnostics.push(
                            Diagnostic::error()
                                .with_code(DiagnosticCode::ExpectedTrait.code())
                                .with_message("expected a trait name")
                                .with_label(Label::primary(self.file_id, trait_name.span)),
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
                    file_id: self.file_id,
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
                    let node = match self.ast_nodes.get(&tid) {
                        Some(n) => n,
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
                                        inner: if is_self { self_type } else { Type::ERROR_IDX },
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
                    self.tir.functions.push(Function {
                        id: *id,
                        file_id: self.file_id,
                        body: None,
                        type_params: Box::new([]),
                        pub_span: *pub_span,
                        origin: FunctionOrigin::TraitImpl { trait_impl_index },
                        source: ItemSource::Internal,
                        signature_index,
                        name: signature.name.clone(),
                        accesses: Vec::new(),
                        params,
                        result,
                        attributes: self.resolve_function_attributes(attributes),
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
                        None => Type::UNKNOWN_IDX,
                    };
                    if let Ok(value_expr) =
                        self.build_const_expression(&resolve_context, value, resolved_ty)
                    {
                        let const_index = self.tir.constants.len() as ConstIndex;
                        self.tir.constants.push(Constant {
                            id: *id,
                            file_id: self.file_id,
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
                    // Resolve each bound TypeExpression to a TraitIndex.
                    let resolved_bounds: Box<[TraitIndex]> = bounds
                        .iter()
                        .filter_map(|b| {
                            let ty = self.resolve_type(&resolve_context, b);
                            match self.tir.type_pool[ty as usize] {
                                Type::Trait { trait_index } => Some(trait_index),
                                _ => {
                                    // Non-trait bound — emit error and skip.
                                    self.tir.diagnostics.push(
                                        Diagnostic::error()
                                            .with_code(DiagnosticCode::ExpectedTrait.code())
                                            .with_message("associated type bound must be a trait")
                                            .with_label(Label::primary(self.file_id, b.span)),
                                    );
                                    None
                                }
                            }
                        })
                        .collect();

                    let placeholder = self.intern_type(Type::AssociatedType {
                        assoc_name: name.inner,
                        trait_index,
                    });

                    self.tir.traits[trait_index as usize]
                        .assoc_type_bounds
                        .insert(name.inner, resolved_bounds);
                    self.tir.traits[trait_index as usize]
                        .members
                        .insert(name.inner, ImplEntry::AssociatedType { ty: placeholder });

                    // Replace Pending with TraitAssocType only if it's still our
                    // own Pending — never clobber a same-named resolved symbol.
                    if matches!(
                        self.lookup_symbol(&resolve_context, SymbolNamespace::Type, name.inner),
                        Some(SymbolKind::Pending(d)) if *d == *id
                    ) {
                        self.insert_symbol(
                            &resolve_context,
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

                    // Check that the concrete type satisfies each declared bound.
                    // Bounds are resolved lazily — ensure the trait's signature is
                    // ready before reading assoc_type_bounds.
                    let trait_def_id = self.tir.traits[trait_index as usize]
                        .member_def_ids
                        .iter()
                        .copied()
                        .find(|&did| {
                            matches!(
                                self.ast_nodes.get(&did),
                                Some(AstNodeRef::TraitAssociatedType { item, .. })
                                    if matches!(item, ast::TraitItem::AssociatedType { name: n, .. } if n.inner == name.inner)
                            )
                        });
                    if let Some(did) = trait_def_id {
                        self.ensure_signature(did);
                    }
                    let bounds = self.tir.traits[trait_index as usize]
                        .assoc_type_bounds
                        .get(&name.inner)
                        .cloned()
                        .unwrap_or_default();
                    for bound_trait_index in bounds.iter().copied() {
                        if !self
                            .tir
                            .trait_impl_lookup
                            .contains_key(&(concrete_ty, bound_trait_index))
                        {
                            let bound_name = self
                                .interner
                                .resolve(self.tir.traits[bound_trait_index as usize].name.inner)
                                .unwrap();
                            let type_name = TypeFormatter {
                                interner: &self.interner,
                                tir: &self.tir,
                            }
                            .display_type(concrete_ty);
                            self.tir.diagnostics.push(
                                Diagnostic::error()
                                    .with_code(DiagnosticCode::TypeMistmatch.code())
                                    .with_message(format!(
                                        "associated type `{}` must implement `{}`",
                                        self.interner.resolve(name.inner).unwrap_or("?"),
                                        bound_name,
                                    ))
                                    .with_label(Label::primary(self.file_id, name.span))
                                    .with_note(format!(
                                        "`{}` does not implement `{}`",
                                        type_name, bound_name
                                    )),
                            );
                        }
                    }
                }
            }
        }

        self.file_id = saved_file_id;
        self.sig_state.insert(def_id, ComputeState::Done);
    }

    // ── query: ensure_body ────────────────────────────────────────────────────

    /// Resolves the body of `def_id`. Not idempotent — calling twice
    /// double-counts accesses.
    fn ensure_body(&mut self, def_id: ast::DefId) {
        self.ensure_signature(def_id);

        let node = match self.ast_nodes.get(&def_id).cloned() {
            Some(n) => n,
            None => return,
        };

        // Extract (sig, body_expr, func_index) — only function-like nodes have bodies.
        let (sig, body_expr, func_index) = match &node {
            AstNodeRef::Function { item, .. } => match item {
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
            AstNodeRef::ImplMethod { item, .. } | AstNodeRef::ImplTraitMethod { item, .. } => {
                match item {
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
                }
            }
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
            AstNodeRef::Global { item, .. } => {
                let saved_file_id = self.file_id;
                let resolve_context = ResolveContext::new(node.file_id(), node.module());
                self.file_id = node.file_id();

                let ast::Item::Global { id, value, .. } = item else {
                    unreachable!();
                };

                let global_index = self.tir.global_index_lookup[id];
                let global_ty = self.tir.globals[global_index as usize].ty.inner;
                let value_expr =
                    match self.build_const_expression(&resolve_context, value, global_ty) {
                        Ok(value_expr) => value_expr,
                        Err(_) => {
                            self.file_id = saved_file_id;
                            return;
                        }
                    };

                match value_expr.ty {
                    _ if value_expr.ty == Type::UNKNOWN_IDX => {
                        self.tir.diagnostics.push(report_type_annotation_required(
                            SourceSpan::new(self.file_id, value.span),
                        ));
                    }
                    ty if !self.coercible_to(ty, global_ty) => {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            TypeMistmatchDiagnostic {
                                expected_type: global_ty,
                                actual_type: ty,
                                span: SourceSpan::new(self.file_id, value.span),
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

                self.file_id = saved_file_id;
                return;
            }
            _ => return,
        };

        let resolve_context = ResolveContext::new(node.file_id(), node.module());
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

        let saved_file_id = self.file_id;
        self.file_id = node.file_id();

        let resolve_context = ResolveContext {
            file_id: resolve_context.file_id,
            module: resolve_context.module,
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

        self.file_id = saved_file_id;
    }

    fn resolve_ast_type_params(
        &mut self,
        resolve_context: &ResolveContext,
        ast_params: &[ast::TypeParam],
    ) -> Box<[TypeParamInfo]> {
        ast_params
            .iter()
            .map(|tp| {
                let bounds: Box<[TraitIndex]> = tp
                    .bounds
                    .iter()
                    .filter_map(|bound| {
                        let ty = self.resolve_type(&resolve_context, bound);
                        match &self.tir.type_pool[ty as usize] {
                            Type::Trait { trait_index } => Some(*trait_index),
                            _ => {
                                self.tir.diagnostics.push(
                                    Diagnostic::error()
                                        .with_code(DiagnosticCode::ExpectedTrait.code())
                                        .with_message("expected a trait name as a bound")
                                        .with_label(Label::primary(self.file_id, bound.span)),
                                );
                                None
                            }
                        }
                    })
                    .collect();
                TypeParamInfo {
                    name: tp.name.inner,
                    bounds,
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
                        SourceSpan::new(self.file_id, first_span),
                        SourceSpan::new(self.file_id, name.span),
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
                            inner: Type::ERROR_IDX,
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
        match self.tir.type_pool[ty as usize].clone() {
            Type::TypeParam { param_index, .. } => type_args
                .get(param_index as usize)
                .copied()
                .filter(|&t| t != Type::ERROR_IDX)
                .unwrap_or(ty),
            Type::AssociatedType { .. } => ty,
            Type::AssocTypeProjection {
                param_index,
                assoc_name,
                ..
            } => {
                let receiver = type_args.get(param_index as usize).copied().unwrap_or(ty);
                if matches!(
                    self.tir.type_pool[receiver as usize],
                    Type::TypeParam { .. }
                ) {
                    return ty;
                }
                self.tir
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
                    .unwrap_or(ty)
            }
            Type::Tuple { elements } => {
                let mut changed = false;
                let substituted: Box<[TypeIndex]> = elements
                    .iter()
                    .map(|&element| {
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
                let mut changed = false;
                let items: Box<[TypeIndex]> = signature
                    .items
                    .iter()
                    .map(|&item| {
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
            Type::FunctionItem {
                id,
                type_args: item_args,
            } => {
                let mut changed = false;
                let substituted: Box<[TypeIndex]> = item_args
                    .iter()
                    .map(|&item_arg| {
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
            Type::Pointer {
                to,
                mutable,
                memory,
            } => {
                let next = self.substitute_type(to, type_args);
                if next == to {
                    ty
                } else {
                    self.intern_type(Type::Pointer {
                        to: next,
                        mutable,
                        memory,
                    })
                }
            }
            Type::Array {
                of,
                size,
                mutable,
                memory,
            } => {
                let next = self.substitute_type(of, type_args);
                if next == of {
                    ty
                } else {
                    self.intern_type(Type::Array {
                        of: next,
                        size,
                        mutable,
                        memory,
                    })
                }
            }
            Type::Slice {
                of,
                mutable,
                memory,
            } => {
                let next = self.substitute_type(of, type_args);
                if next == of {
                    ty
                } else {
                    self.intern_type(Type::Slice {
                        of: next,
                        mutable,
                        memory,
                    })
                }
            }
            _ => ty,
        }
    }

    fn substitute_expected_type(
        &mut self,
        ty: TypeIndex,
        type_args: &[TypeIndex],
    ) -> Option<TypeIndex> {
        match &self.tir.type_pool[ty as usize] {
            Type::TypeParam { param_index, .. } => {
                match type_args.get(*param_index as usize).copied() {
                    Some(t) if t != Type::UNKNOWN_IDX && t != Type::ERROR_IDX => Some(t),
                    _ => None,
                }
            }
            _ => Some(self.substitute_type(ty, type_args)),
        }
    }

    fn seed_type_arg_slot(&mut self, type_args: &mut [TypeIndex], param_index: u32, ty: TypeIndex) {
        let Some(slot) = type_args.get_mut(param_index as usize) else {
            return;
        };
        if *slot == Type::UNKNOWN_IDX {
            *slot = ty;
        }
    }

    fn infer_type_args_from_types(
        &mut self,
        pattern_ty: TypeIndex,
        actual_ty: TypeIndex,
        type_args: &mut [TypeIndex],
    ) {
        if actual_ty == Type::UNKNOWN_IDX || actual_ty == Type::ERROR_IDX {
            return;
        }

        match (
            self.tir.type_pool[pattern_ty as usize].clone(),
            self.tir.type_pool[actual_ty as usize].clone(),
        ) {
            (Type::TypeParam { param_index, .. }, _) => {
                self.seed_type_arg_slot(type_args, param_index, actual_ty);
            }
            (
                Type::AssocTypeProjection {
                    assoc_name,
                    trait_index,
                    param_index,
                },
                Type::AssocTypeProjection {
                    assoc_name: actual_assoc,
                    trait_index: actual_trait,
                    param_index: actual_param,
                },
            ) if assoc_name == actual_assoc
                && trait_index == actual_trait
                && param_index == actual_param => {}
            (Type::Tuple { elements: pattern }, Type::Tuple { elements: actual })
                if pattern.len() == actual.len() =>
            {
                for (&pattern, &actual) in pattern.iter().zip(actual.iter()) {
                    self.infer_type_args_from_types(pattern, actual, type_args);
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
                for (&pattern, &actual) in pattern_sig.items.iter().zip(actual_sig.items.iter()) {
                    self.infer_type_args_from_types(pattern, actual, type_args);
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
            ) if pattern_mutable == actual_mutable && pattern_memory == actual_memory => {
                self.infer_type_args_from_types(pattern_to, actual_to, type_args);
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
            ) if pattern_size == actual_size
                && pattern_mutable == actual_mutable
                && pattern_memory == actual_memory =>
            {
                self.infer_type_args_from_types(pattern_of, actual_of, type_args);
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
            ) if pattern_mutable == actual_mutable && pattern_memory == actual_memory => {
                self.infer_type_args_from_types(pattern_of, actual_of, type_args);
            }
            _ => {}
        }
    }

    fn seed_memory_trait_impl(&mut self, trait_index: u32, memory_type: TypeIndex) {
        self.seed_memory_trait_impl_with(trait_index, memory_type, &HashMap::new());
    }

    fn seed_memory_trait_impl_with(
        &mut self,
        trait_index: u32,
        memory_type: TypeIndex,
        // Assoc-type overrides from the parent supertrait declaration.
        // E.g. Memory32's `Memory<Size=u32>` provides {"Size" → u32}.
        bindings: &HashMap<SymbolU32, TypeIndex>,
    ) {
        // Seed supertrait members first (lower priority); own members override them.
        let st_list = self.tir.traits[trait_index as usize].supertraits.clone();
        for &st in &st_list {
            // Pass bindings declared at *this* trait's supertrait use site down to st.
            let st_bindings: HashMap<SymbolU32, TypeIndex> = self.tir.traits[trait_index as usize]
                .supertrait_bindings
                .iter()
                .filter_map(|(&(ti, sym), &ty)| if ti == st { Some((sym, ty)) } else { None })
                .collect();
            self.seed_memory_trait_impl_with(st, memory_type, &st_bindings);
        }
        // Ensure all member signatures in this trait are resolved.
        for did in self.tir.traits[trait_index as usize].member_def_ids.clone() {
            self.ensure_signature(did);
        }
        let self_symbol = self.interner.get_or_intern("self");
        let members: Vec<(SymbolU32, ImplEntry)> = self.tir.traits[trait_index as usize]
            .members
            .iter()
            .map(|(&sym, entry)| match entry {
                ImplEntry::Method(fi) => {
                    let func = &self.tir.functions[*fi as usize];
                    let entry = if func
                        .params
                        .first()
                        .map(|p| p.name.inner == self_symbol)
                        .unwrap_or(false)
                    {
                        ImplEntry::Method(*fi)
                    } else {
                        ImplEntry::AssociatedFn(*fi)
                    };
                    (sym, entry)
                }
                ImplEntry::AssociatedType { ty } => {
                    // Substitute placeholder with the concrete binding if provided.
                    let concrete = bindings.get(&sym).copied().unwrap_or(*ty);
                    (sym, ImplEntry::AssociatedType { ty: concrete })
                }
                other => (sym, other.clone()),
            })
            .collect();
        let slot = self.tir.impl_members.entry(memory_type).or_default();
        for (sym, entry) in members {
            slot.insert(sym, entry);
        }
    }

    fn build_exports(&mut self, entries: &[Separated<Spanned<ast::ExportEntry>>]) {
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
                            self.file_id,
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
                            file_id: self.file_id,
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
                        .push(SourceSpan::new(self.file_id, internal_name.span));

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
                        SourceSpan::new(self.file_id, internal_name.span),
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
                        SourceSpan::new(self.file_id, first_export_span),
                        SourceSpan::new(self.file_id, export_span),
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
                let is_float_type = expected_type == Type::F32_IDX
                    || expected_type == Type::F64_IDX
                    || expected_type == Type::ERROR_IDX;
                if !is_float_type {
                    self.tir
                        .diagnostics
                        .push(report_float_literal_for_integer_type(SourceSpan::new(
                            self.file_id,
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
            ast::Expression::Identifier { symbol } => {
                // Allow references to global constants like true/false
                match self
                    .lookup_symbol(&resolve_context, SymbolNamespace::Value, *symbol)
                    .cloned()
                {
                    Some(SymbolKind::True) => Ok(Expression {
                        kind: ExprKind::Bool { value: true },
                        ty: Type::BOOL_IDX,
                        span: expr.span,
                    }),
                    Some(SymbolKind::False) => Ok(Expression {
                        kind: ExprKind::Bool { value: false },
                        ty: Type::BOOL_IDX,
                        span: expr.span,
                    }),
                    Some(SymbolKind::Const { const_index }) => {
                        let constant = &mut self.tir.constants[const_index as usize];
                        constant
                            .accesses
                            .push(SourceSpan::new(self.file_id, expr.span));
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
                                self.file_id,
                                expr.span,
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
                    operator if operator.is_comparison() || operator.is_logical() => Type::BOOL_IDX,
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
                        self.file_id,
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
                    .unwrap_or(Type::UNIT_IDX),
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
                self.report_local_warnings(&ctx.stack.scopes[ctx.scope_index as usize]);
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
                            self.file_id,
                            TextSpan::new(start, end),
                        )));
                }

                let scope = &mut ctx.stack.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.unwrap_or(Type::NEVER_IDX);
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
                            expected_type: Some(Type::UNIT_IDX),
                            access_kind: AccessKind::Read,
                        },
                        &result,
                    )?),
                    None => None,
                };

                self.report_local_warnings(&ctx.stack.scopes[ctx.scope_index as usize]);

                Ok(Expression {
                    kind: ExprKind::Block {
                        scope_index: ctx.scope_index,
                        expressions,
                        result: result.map(Box::new),
                    },
                    ty: ctx.stack.scopes[ctx.scope_index as usize]
                        .inferred_type
                        .unwrap_or(Type::NEVER_IDX),
                    span: block.span,
                })
            }
            BlockKind::Block => {
                let result = self.build_block_result(ctx, result.as_deref())?;

                self.report_local_warnings(&ctx.stack.scopes[ctx.scope_index as usize]);

                let scope = &ctx.stack.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.expect("should have inferred type");
                match scope.expected_type {
                    Some(expected_type) if !self.coercible_to(inferred_type, expected_type) => {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            TypeMistmatchDiagnostic {
                                expected_type,
                                actual_type: inferred_type,
                                span: SourceSpan::new(self.file_id, block.span),
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

    fn report_local_warnings(&mut self, block: &BlockScope) {
        for local in block.locals.iter() {
            if local.accesses.is_empty() && local.ty != Type::ERROR_IDX {
                self.tir
                    .diagnostics
                    .push(report_unused_variable(SourceSpan::new(
                        self.file_id,
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
                            self.file_id,
                            mut_span,
                        )));
                }
                _ => {}
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
                && function.source == ItemSource::Internal
                && !matches!(
                    function.origin,
                    FunctionOrigin::Trait | FunctionOrigin::Module
                )
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
            if global.source == ItemSource::Internal && global.accesses.is_empty() {
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
                        .with_message(format!("struct `{}` is never constructed", name))
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
                let inferred_type = self.infer_block_type(scope, &result)?;
                scope.inferred_type = Some(inferred_type);
                if result.ty == Type::UNKNOWN_IDX {
                    _ = self.coerce_untyped_expr(&mut result, inferred_type);
                }

                Ok(Some(result))
            }
            None => {
                let scope = &mut ctx.stack.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.unwrap_or(Type::UNIT_IDX);
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
                _ if expr.ty == Type::NEVER_IDX => {
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
        scope: &BlockScope,
        value: &Expression,
    ) -> Result<TypeIndex, ()> {
        if value.ty == Type::UNKNOWN_IDX {
            match scope.inferred_type.or(scope.expected_type) {
                Some(ty) => return Ok(ty),
                None => {
                    self.tir
                        .diagnostics
                        .push(report_type_annotation_required(SourceSpan::new(
                            self.file_id,
                            value.span,
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
                        span: SourceSpan::new(self.file_id, value.span),
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
                            span: SourceSpan::new(self.file_id, value.span),
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
        if value.ty == Type::UNIT_IDX {
            return Ok(value);
        } else if value.ty == Type::ERROR_IDX {
            // Skip reporting unused value for error types, as the error has already been
            // reported
            return Ok(value);
        } else if value.ty == Type::NEVER_IDX {
            let scope = ctx.stack.scopes.get_mut(ctx.scope_index as usize).unwrap();
            scope.inferred_type = scope.inferred_type.or(Some(Type::NEVER_IDX));
            return Ok(value);
        } else if value.ty == Type::UNKNOWN_IDX {
            self.tir
                .diagnostics
                .push(report_type_annotation_required(SourceSpan::new(
                    self.file_id,
                    value.span,
                )));
            return Err(());
        }
        self.tir
            .diagnostics
            .push(report_unused_value(SourceSpan::new(
                self.file_id,
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
                ty: Type::UNKNOWN_IDX,
                span: expr.span,
            }),
            ast::Expression::Float { value } => Ok(Expression {
                kind: ExprKind::Float { value: *value },
                ty: Type::UNKNOWN_IDX,
                span: expr.span,
            }),
            ast::Expression::Unreachable => Ok(Expression {
                kind: ExprKind::Unreachable,
                ty: Type::NEVER_IDX,
                span: expr.span,
            }),
            ast::Expression::SelfType => {
                // `Self` used as a standalone value (not `Self::X`) — always an error.
                self.tir.diagnostics.push(
                    Diagnostic::error()
                        .with_code(DiagnosticCode::NamespaceUsedAsValue.code())
                        .with_message("`Self` is a type, not a value; use `Self::member` to access associated items")
                        .with_label(Label::primary(self.file_id, expr.span)),
                );
                Err(())
            }
            ast::Expression::Error => Ok(Expression {
                kind: ExprKind::Error,
                ty: Type::ERROR_IDX,
                span: expr.span,
            }),
            ast::Expression::String { symbol } => {
                let unescaped = unescape_string(self.interner.resolve(*symbol).unwrap());
                let symbol = self.interner.get_or_intern(&unescaped);
                let string_symbol = self.interner.get_or_intern("string");
                Ok(Expression {
                    kind: ExprKind::String { symbol },
                    ty: match self.lookup_symbol(
                        &func_ctx.resolve_context,
                        SymbolNamespace::Type,
                        string_symbol,
                    ) {
                        Some(SymbolKind::Struct { struct_index }) => {
                            self.intern_type(Type::Struct {
                                struct_index: *struct_index,
                            })
                        }
                        _ => panic!("built-in string struct should be defined"),
                    },
                    span: expr.span,
                })
            }
            ast::Expression::Char { symbol } => {
                let raw = self.interner.resolve(*symbol).unwrap();
                match parse_char_literal(raw) {
                    Ok(value) => Ok(Expression {
                        kind: ExprKind::Char { value },
                        ty: Type::CHAR_IDX,
                        span: expr.span,
                    }),
                    Err(CharLiteralError::Empty) => {
                        self.tir
                            .diagnostics
                            .push(report_empty_char_literal(SourceSpan::new(
                                self.file_id,
                                expr.span,
                            )));
                        Err(())
                    }
                    Err(CharLiteralError::TooLong) => {
                        self.tir
                            .diagnostics
                            .push(report_char_literal_too_long(SourceSpan::new(
                                self.file_id,
                                expr.span,
                            )));
                        Err(())
                    }
                }
            }
            ast::Expression::Identifier { .. } => {
                self.build_identifier_expression(func_ctx, access_ctx, expr)
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
            ast::Expression::NamespaceAccess { namespace, member } => self
                .build_namespace_access_expression(func_ctx, namespace, member.clone(), expr.span),
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
            ast::Expression::StructInit { name, fields } => {
                // Extract the struct symbol and optional explicit type args from
                // the name expression: Identifier (non-generic) or TypeApplication
                // (generic: `Point::<T>::{ ... }`).
                let (struct_name, explicit_type_args): (
                    ast::Spanned<SymbolU32>,
                    &[ast::Spanned<ast::TypeExpression>],
                ) = match &name.inner {
                    ast::Expression::Identifier { symbol } => (
                        ast::Spanned {
                            inner: *symbol,
                            span: name.span,
                        },
                        &[],
                    ),
                    ast::Expression::TypeApplication { callee, args } => match &callee.inner {
                        ast::Expression::Identifier { symbol } => (
                            ast::Spanned {
                                inner: *symbol,
                                span: callee.span,
                            },
                            args.as_ref(),
                        ),
                        _ => {
                            self.tir.diagnostics.push(report_not_a_struct_type(
                                self.file_id,
                                "expression".to_string(),
                                name.span,
                            ));
                            return Ok(Expression {
                                kind: ExprKind::Error,
                                ty: Type::ERROR_IDX,
                                span: expr.span,
                            });
                        }
                    },
                    _ => {
                        self.tir.diagnostics.push(report_not_a_struct_type(
                            self.file_id,
                            "expression".to_string(),
                            name.span,
                        ));
                        return Ok(Expression {
                            kind: ExprKind::Error,
                            ty: Type::ERROR_IDX,
                            span: expr.span,
                        });
                    }
                };
                let _ = explicit_type_args; // TODO: use for generic struct init
                self.build_struct_init_expression(func_ctx, expr.span, struct_name, &fields)
            }
            ast::Expression::Tuple { elements } => {
                self.build_tuple_expression(func_ctx, expr.span, elements, access_ctx)
            }
            // Type args are stored for future monomorphization; TIR resolves the
            // inner callee and treats the call as non-generic until mono pass.
            ast::Expression::TypeApplication { callee, .. } => {
                self.build_expression(func_ctx, access_ctx, callee)
            }
        }
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
        let entry = match &self.tir.type_pool[object.ty as usize] {
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
                .ensure_impl_members(object.ty)
                .get(&member.inner)
                .cloned(),
        };
        match entry {
            Some(ImplEntry::Method(func_index)) => {
                let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                let func = &mut self.tir.functions[func_index as usize];
                func.accesses.push(FunctionAccess {
                    caller: Some(caller_id),
                    kind: FunctionAccessKind::Reference,
                    file_id: self.file_id,
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
        if let Type::Struct { struct_index } = &self.tir.type_pool[object.ty as usize] {
            if let Some(&field_index) = self.tir.structs[*struct_index as usize]
                .lookup
                .get(&member.inner)
            {
                let field_ty = self.tir.structs[*struct_index as usize].fields[field_index]
                    .ty
                    .inner;
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

        self.tir
            .diagnostics
            .push(report_undeclared_identifier(SourceSpan::new(
                self.file_id,
                member.span,
            )));
        Err(())
    }

    /// Ensures `SIZE` and `ALIGN` are present for `ty` and returns its member
    /// map. Always use this for impl-member lookups — never access
    /// `impl_members` directly.
    fn ensure_impl_members(&mut self, ty: TypeIndex) -> &HashMap<SymbolU32, ImplEntry> {
        let size_sym = self.interner.get_or_intern("SIZE");
        let already_seeded = self
            .tir
            .impl_members
            .get(&ty)
            .map_or(false, |m| m.contains_key(&size_sym));
        if !already_seeded {
            let is_sized = !matches!(
                self.tir.type_pool[ty as usize],
                Type::Error
                    | Type::Unknown
                    | Type::ImportModule { .. }
                    | Type::Module { .. }
                    | Type::Enum { .. }
                    | Type::Function { .. }
                    | Type::Memory { .. }
                    | Type::Trait { .. }
            );
            if is_sized {
                let align_sym = self.interner.get_or_intern("ALIGN");
                let size_id = self.id_generator.generate();
                let align_id = self.id_generator.generate();
                // TODO: replace dummy_span with the actual span of the Sized trait SIZE/ALIGN
                // constants once the Sized lang-item trait is defined in the stdlib.
                let dummy_span = ast::TextSpan { start: 0, end: 0 };
                let size_index = self.tir.constants.len() as ConstIndex;
                self.tir.constants.push(Constant {
                    id: size_id,
                    file_id: self.file_id,
                    pub_span: None,
                    name: ast::Spanned {
                        inner: size_sym,
                        span: dummy_span,
                    },
                    ty: ast::Spanned {
                        inner: Type::U32_IDX,
                        span: dummy_span,
                    },
                    value: None,
                    accesses: Vec::new(),
                });
                self.tir.const_index_lookup.insert(size_id, size_index);
                let align_index = self.tir.constants.len() as ConstIndex;
                self.tir.constants.push(Constant {
                    id: align_id,
                    file_id: self.file_id,
                    pub_span: None,
                    name: ast::Spanned {
                        inner: align_sym,
                        span: dummy_span,
                    },
                    ty: ast::Spanned {
                        inner: Type::U32_IDX,
                        span: dummy_span,
                    },
                    value: None,
                    accesses: Vec::new(),
                });
                self.tir.const_index_lookup.insert(align_id, align_index);
                let members = self.tir.impl_members.entry(ty).or_default();
                members.insert(
                    size_sym,
                    ImplEntry::AssociatedConst {
                        id: size_id,
                        ty: Type::U32_IDX,
                    },
                );
                members.insert(
                    align_sym,
                    ImplEntry::AssociatedConst {
                        id: align_id,
                        ty: Type::U32_IDX,
                    },
                );
            }
        }

        self.tir.impl_members.entry(ty).or_default()
    }

    fn build_namespace_access_expression(
        &mut self,
        func_ctx: &FunctionContext,
        namespace: &Spanned<ast::TypeExpression>,
        member: Spanned<SymbolU32>,
        expr_span: TextSpan,
    ) -> Result<Expression, ()> {
        let namespace_ty = self.resolve_type(&func_ctx.resolve_context, &namespace);
        if namespace_ty == Type::ERROR_IDX {
            return Err(());
        }
        let namespace_spanned = ast::Spanned {
            inner: namespace_ty,
            span: namespace.span,
        };

        match self
            .ensure_impl_members(namespace_ty)
            .get(&member.inner)
            .cloned()
        {
            Some(ImplEntry::AssociatedConst { id, ty }) => {
                if let Some(ci) = self.tir.const_index_lookup.get(&id).copied() {
                    self.tir.constants[ci as usize]
                        .accesses
                        .push(SourceSpan::new(self.file_id, member.span));
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
                    file_id: self.file_id,
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

        match &self.tir.type_pool[namespace_ty as usize] {
            Type::Memory { .. } => {
                // The member was not found in impl_members (checked above), so it's unknown.
                self.tir
                    .diagnostics
                    .push(report_undeclared_identifier(SourceSpan::new(
                        self.file_id,
                        member.span,
                    )));
                Err(())
            }
            Type::Enum { enum_index } => {
                let enum_idx = *enum_index;
                let enum_ = &self.tir.enums[enum_idx as usize];
                match enum_.lookup.get(&member.inner).copied() {
                    Some(variant_idx) => Ok(Expression {
                        kind: ExprKind::NamespaceAccess {
                            namespace: namespace_spanned,
                            member: Box::new(Expression {
                                kind: ExprKind::EnumVariant {
                                    enum_index: enum_idx,
                                    variant_index: variant_idx,
                                },
                                ty: enum_.ty,
                                span: member.span,
                            }),
                        },
                        ty: enum_.ty,
                        span: expr_span,
                    }),
                    None => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_identifier(SourceSpan::new(
                                self.file_id,
                                member.span,
                            )));
                        Err(())
                    }
                }
            }
            Type::ImportModule { module_index } => {
                let module = &self.tir.import_modules[*module_index as usize];
                match module.lookup.get(&member.inner).copied() {
                    Some(ImportValue::Function { id }) => {
                        let func_index = self.tir.function_index_lookup[&id];
                        let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                        self.tir.functions[func_index as usize]
                            .accesses
                            .push(FunctionAccess {
                                caller: Some(caller_id),
                                kind: FunctionAccessKind::Reference,
                                file_id: self.file_id,
                                span: member.span,
                            });
                        let signature_index =
                            self.tir.functions[func_index as usize].signature_index;
                        Ok(Expression {
                            kind: ExprKind::NamespaceAccess {
                                namespace: namespace_spanned,
                                member: Box::new(Expression {
                                    kind: ExprKind::Function { id },
                                    ty: signature_index,
                                    span: member.span,
                                }),
                            },
                            ty: signature_index,
                            span: expr_span,
                        })
                    }
                    Some(ImportValue::Global { id }) => {
                        let global_index = self.tir.global_index_lookup[&id];
                        let global = &mut self.tir.globals[global_index as usize];
                        global
                            .accesses
                            .push(SourceSpan::new(self.file_id, member.span));
                        let ty = global.ty.inner;
                        Ok(Expression {
                            kind: ExprKind::NamespaceAccess {
                                namespace: namespace_spanned,
                                member: Box::new(Expression {
                                    kind: ExprKind::Global { id },
                                    ty,
                                    span: member.span,
                                }),
                            },
                            ty,
                            span: expr_span,
                        })
                    }
                    Some(ImportValue::Memory { id }) => {
                        let memory_index = self.tir.memory_index_lookup[&id];
                        let kind = self.tir.memories[memory_index as usize].kind;
                        let ty = self.intern_type(Type::Memory { kind, id });
                        Ok(Expression {
                            kind: ExprKind::Memory { id },
                            ty,
                            span: expr_span,
                        })
                    }
                    None => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_identifier(SourceSpan::new(
                                self.file_id,
                                member.span,
                            )));
                        Err(())
                    }
                }
            }
            Type::Module { module_index } => {
                match self.tir.modules[*module_index as usize]
                    .symbol_lookup
                    .get(&(SymbolNamespace::Value, member.inner))
                    .cloned()
                {
                    Some(SymbolKind::Function { func_index }) => {
                        let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                        let func = &mut self.tir.functions[func_index as usize];
                        func.accesses.push(FunctionAccess {
                            caller: Some(caller_id),
                            kind: FunctionAccessKind::Reference,
                            file_id: self.file_id,
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
                    _ => {
                        self.tir
                            .diagnostics
                            .push(report_undeclared_identifier(SourceSpan::new(
                                self.file_id,
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
                                file_id: self.file_id,
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
                                .push(SourceSpan::new(self.file_id, member.span));
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
                                self.file_id,
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
                        TypeFormatter::new(&self.tir, &self.interner,).display_type(namespace_ty),
                    ))
                    .with_label(SourceSpan::new(self.file_id, namespace.span).primary_label());
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
                                span: SourceSpan::new(self.file_id, expr.span),
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
                            self.file_id,
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
            ty: Type::NEVER_IDX,
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
                expected_type: Some(Type::BOOL_IDX),
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
                                span: SourceSpan::new(self.file_id, ast_else_block.span),
                            },
                        ));
                        return Err(());
                    }
                }
            }
            None => {
                if then_block.ty == Type::UNIT_IDX || then_block.ty == Type::NEVER_IDX {
                    (None, Type::UNIT_IDX)
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

        let cast_type_idx = self.resolve_type(&ctx.resolve_context, &cast_type);
        if cast_type_idx == Type::ERROR_IDX {
            return self.build_expression(ctx, access_ctx, value);
        }
        let cast_type = cast_type_idx;
        let mut value = self.build_expression(
            ctx,
            AccessContext {
                expected_type: Some(cast_type),
                access_kind: access_ctx.access_kind,
            },
            value,
        )?;
        if value.ty == Type::UNKNOWN_IDX {
            self.coerce_untyped_expr(&mut value, cast_type)?;
        } else if value.ty == cast_type {
            // TODO: report redundant cast
        } else if value.ty == Type::BOOL_IDX && self.tir.type_pool[cast_type as usize].is_integer()
        {
            value.ty = cast_type;
        } else if is_numeric_cast(value.ty, cast_type) {
            value.ty = cast_type;
        } else if value.ty != cast_type {
            self.tir.diagnostics.push(report_type_mistmatch(
                TypeFormatter::new(&self.tir, &self.interner),
                TypeMistmatchDiagnostic {
                    expected_type: cast_type,
                    actual_type: value.ty,
                    span: SourceSpan::new(self.file_id, value.span),
                },
            ));
            // Set the type to the cast target to avoid cascading errors
            value.ty = cast_type;
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
                            self.file_id,
                            label.span,
                        )));

                    // TODO: how to handle this better? we don't parse the value if the label is
                    // undeclared
                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: Type::NEVER_IDX,
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
                            self.file_id,
                            expr.span,
                        )));

                    // TODO: same as above, we don't parse the value if the break is outside of a
                    // loop
                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: Type::NEVER_IDX,
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
                    let inferred_type = self.infer_block_type(scope, &value)?;
                    if value.ty == Type::UNKNOWN_IDX {
                        self.coerce_untyped_expr(&mut value, inferred_type)?;
                    }
                    scope.inferred_type = Some(inferred_type);

                    Ok(Expression {
                        kind: ExprKind::Break {
                            scope_index,
                            value: Some(Box::new(value)),
                        },
                        ty: Type::NEVER_IDX,
                        span: expr.span,
                    })
                })
                .unwrap_or(Expression {
                    kind: ExprKind::Unreachable,
                    ty: Type::NEVER_IDX,
                    span: expr.span,
                })),
            None => {
                let scope = ctx.stack.scopes.get_mut(scope_index as usize).unwrap();
                match scope.inferred_type {
                    Some(inferred) => {
                        if !self.coercible_to(Type::UNIT_IDX, inferred) {
                            let formatter = TypeFormatter::new(&self.tir, &self.interner);
                            self.tir.diagnostics.push(report_type_mistmatch(
                                formatter,
                                TypeMistmatchDiagnostic {
                                    expected_type: inferred,
                                    actual_type: Type::UNIT_IDX,
                                    span: SourceSpan::new(self.file_id, expr.span),
                                },
                            ));
                        }
                    }
                    None => {
                        scope.inferred_type = Some(Type::UNIT_IDX);
                    }
                }

                Ok(Expression {
                    kind: ExprKind::Break {
                        scope_index,
                        value: None,
                    },
                    ty: Type::NEVER_IDX,
                    span: expr.span,
                })
            }
        }
    }

    fn build_identifier_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        access_ctx: AccessContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let symbol = match expr.inner {
            ast::Expression::Identifier { symbol } => symbol.clone(),
            _ => unreachable!(),
        };
        match func_ctx.resolve_local(symbol) {
            Some((scope_index, local_index)) => {
                let local = func_ctx
                    .stack
                    .get_mut_local(scope_index, local_index)
                    .unwrap();

                local.accesses.push(LocalAccess {
                    kind: access_ctx.access_kind,
                    span: expr.span,
                });

                return Ok(Expression {
                    kind: ExprKind::Local {
                        local_index,
                        scope_index,
                    },
                    ty: local.ty,
                    span: expr.span,
                });
            }
            None => {}
        }

        match self
            .lookup_symbol(&func_ctx.resolve_context, SymbolNamespace::Value, symbol)
            .filter(|k| !matches!(k, SymbolKind::Pending(_)))
            .cloned()
        {
            Some(global) => match global {
                SymbolKind::True => Ok(Expression {
                    kind: ExprKind::Bool { value: true },
                    ty: Type::BOOL_IDX,
                    span: expr.span,
                }),
                SymbolKind::False => Ok(Expression {
                    kind: ExprKind::Bool { value: false },
                    ty: Type::BOOL_IDX,
                    span: expr.span,
                }),
                SymbolKind::Placeholder => Ok(Expression {
                    kind: ExprKind::Placeholder,
                    ty: access_ctx.expected_type.unwrap_or(Type::ERROR_IDX),
                    span: expr.span,
                }),
                SymbolKind::Function { func_index } => {
                    let caller_id = self.tir.functions[func_ctx.func_index as usize].id;
                    let func = &mut self.tir.functions[func_index as usize];
                    func.accesses.push(FunctionAccess {
                        caller: Some(caller_id),
                        kind: FunctionAccessKind::Reference,
                        file_id: self.file_id,
                        span: expr.span,
                    });
                    let func_id = func.id;
                    let ty = self.intern_type(Type::FunctionItem {
                        id: func_id,
                        type_args: Box::new([]),
                    });
                    Ok(Expression {
                        kind: ExprKind::Function { id: func_id },
                        ty,
                        span: expr.span,
                    })
                }
                SymbolKind::Global { global_index } => {
                    let global = &mut self.tir.globals[global_index as usize];
                    global
                        .accesses
                        .push(SourceSpan::new(self.file_id, expr.span));

                    Ok(Expression {
                        kind: ExprKind::Global { id: global.id },
                        ty: global.ty.inner,
                        span: expr.span,
                    })
                }
                SymbolKind::Const { const_index } => {
                    let constant = &mut self.tir.constants[const_index as usize];
                    constant
                        .accesses
                        .push(SourceSpan::new(self.file_id, expr.span));
                    let id = constant.id;
                    let ty = constant.ty.inner;
                    Ok(Expression {
                        kind: ExprKind::Const { id },
                        ty,
                        span: expr.span,
                    })
                }
                SymbolKind::Memory { memory_index, kind } => {
                    let id = self.tir.memories[memory_index as usize].id;
                    let ty = self.intern_type(Type::Memory { kind, id });
                    return Ok(Expression {
                        kind: ExprKind::Memory { id },
                        ty,
                        span: expr.span,
                    });
                }
                // these are namespace-only; a bare identifier resolving to one is an error
                SymbolKind::ImportModule { .. }
                | SymbolKind::Enum { .. }
                | SymbolKind::Module { .. }
                | SymbolKind::Trait { .. } => {
                    self.tir
                        .diagnostics
                        .push(report_namespace_used_as_value(SourceSpan::new(
                            self.file_id,
                            expr.span,
                        )));
                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: Type::ERROR_IDX,
                        span: expr.span,
                    });
                }
                SymbolKind::Unreachable
                | SymbolKind::TraitAssocType { .. }
                | SymbolKind::Pending(_) => unreachable!(),
                // Struct names are type-namespace values, not usable as expressions
                SymbolKind::Struct { .. } => {
                    self.tir
                        .diagnostics
                        .push(report_undeclared_identifier(SourceSpan::new(
                            self.file_id,
                            expr.span,
                        )));
                    Ok(Expression {
                        kind: ExprKind::Error,
                        ty: access_ctx.expected_type.unwrap_or(Type::ERROR_IDX),
                        span: expr.span,
                    })
                }
            },
            None => {
                self.tir
                    .diagnostics
                    .push(report_undeclared_identifier(SourceSpan::new(
                        self.file_id,
                        expr.span,
                    )));

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: access_ctx.expected_type.unwrap_or(Type::ERROR_IDX),
                    span: expr.span,
                })
            }
        }
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
                if self.tir.type_pool[operand.ty as usize].is_primitive()
                    || operand.ty == Type::UNKNOWN_IDX
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
                if operand.ty == Type::BOOL_IDX {
                    Ok(Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: Type::BOOL_IDX,
                        span: expr.span,
                    })
                } else if operand.ty == Type::UNKNOWN_IDX {
                    _ = self.coerce_untyped_expr(&mut operand, Type::BOOL_IDX);
                    Ok(Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: Type::BOOL_IDX,
                        span: expr.span,
                    })
                } else {
                    let formatter = TypeFormatter::new(&self.tir, &self.interner);
                    let diagnostic = Diagnostic::error()
                        .with_code(DiagnosticCode::UnaryOperatorCannotBeApplied.code())
                        .with_message(format!(
                            "operator `{}` cannot be applied to type `{}`",
                            operator.inner,
                            formatter.display_type(operand.ty)
                        ))
                        .with_label(Label::primary(self.file_id, operand.span))
                        .with_label(Label::secondary(self.file_id, operator.span));

                    self.tir.diagnostics.push(diagnostic);
                    Ok(Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: Type::BOOL_IDX,
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
                expected_type: Some(Type::BOOL_IDX),
                access_kind: AccessKind::Read,
            },
            left,
        )?;
        if left.ty == Type::ERROR_IDX {
            // Error already reported
        } else if left.ty == Type::UNKNOWN_IDX {
            self.tir
                .diagnostics
                .push(report_type_annotation_required(SourceSpan::new(
                    self.file_id,
                    left.span,
                )));
        } else if left.ty != Type::BOOL_IDX {
            self.tir.diagnostics.push(report_type_mistmatch(
                TypeFormatter::new(&self.tir, &self.interner),
                TypeMistmatchDiagnostic {
                    expected_type: Type::BOOL_IDX,
                    actual_type: left.ty,
                    span: SourceSpan::new(self.file_id, left.span),
                },
            ));
        }
        let right = self.build_expression(
            ctx,
            AccessContext {
                expected_type: Some(Type::BOOL_IDX),
                access_kind: AccessKind::Read,
            },
            right,
        )?;
        if right.ty == Type::ERROR_IDX {
            // Error already reported
        } else if right.ty == Type::UNKNOWN_IDX {
            self.tir
                .diagnostics
                .push(report_type_annotation_required(SourceSpan::new(
                    self.file_id,
                    right.span,
                )));
        } else if right.ty != Type::BOOL_IDX {
            self.tir.diagnostics.push(report_type_mistmatch(
                TypeFormatter::new(&self.tir, &self.interner),
                TypeMistmatchDiagnostic {
                    expected_type: Type::BOOL_IDX,
                    actual_type: right.ty,
                    span: SourceSpan::new(self.file_id, right.span),
                },
            ));
        }

        Ok(Expression {
            kind: ExprKind::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            },
            ty: Type::BOOL_IDX,
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
                expected_type: match &self.tir.type_pool[left.ty as usize] {
                    Type::Unknown | Type::Error | Type::Never | Type::Unit => {
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
            (l, r) if l == Type::ERROR_IDX || r == Type::ERROR_IDX => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: access_ctx.expected_type.unwrap_or(Type::ERROR_IDX),
                span: expr.span,
            }),
            (l, r) if l == Type::UNKNOWN_IDX && r == Type::UNKNOWN_IDX => {
                match access_ctx.expected_type {
                    Some(expected_type) => {
                        self.coerce_untyped_expr(&mut left, expected_type)?;
                        self.coerce_untyped_expr(&mut right, expected_type)?;

                        let ty = &self.tir.type_pool[expected_type as usize];
                        if !ty.is_integer() && *ty != Type::Bool {
                            self.tir
                                .diagnostics
                                .push(report_binary_operator_cannot_be_applied(
                                    TypeFormatter::new(&self.tir, &self.interner),
                                    BinaryOperatorCannotBeAppliedDiagnostic {
                                        file_id: self.file_id,
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
                            SourceSpan::new(self.file_id, expr.span),
                        ));
                        Err(())
                    }
                }
            }
            (l, right_type) if l == Type::UNKNOWN_IDX => {
                let ty = &self.tir.type_pool[right_type as usize];
                if !ty.is_integer() && *ty != Type::Bool {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.file_id,
                                operator: operator.clone(),
                                operand: Spanned {
                                    inner: right_type,
                                    span: right.span,
                                },
                            },
                        ));
                }
                self.coerce_untyped_expr(&mut left, right_type)?;

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
            (left_type, r) if r == Type::UNKNOWN_IDX => {
                if !self.tir.type_pool[left_type as usize].is_integer()
                    && left_type != Type::BOOL_IDX
                {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.file_id,
                                operator: operator.clone(),
                                operand: Spanned {
                                    inner: left_type,
                                    span: left.span,
                                },
                            },
                        ));
                }
                self.coerce_untyped_expr(&mut right, left_type)?;

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
                    && (self.tir.type_pool[left_type as usize].is_integer()
                        || left_type == Type::BOOL_IDX) =>
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
                            file_id: self.file_id,
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
                    ty: access_ctx.expected_type.unwrap_or(Type::UNKNOWN_IDX),
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
            (l, r) if l == Type::ERROR_IDX || r == Type::ERROR_IDX => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: Type::BOOL_IDX,
                span: expr.span,
            }),
            (l, r) if l == Type::UNKNOWN_IDX && r == Type::UNKNOWN_IDX => {
                self.tir
                    .diagnostics
                    .push(report_comparison_type_annotation_required(
                        SourceSpan::new(self.file_id, left.span),
                        SourceSpan::new(self.file_id, right.span),
                    ));

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::BOOL_IDX,
                    span: expr.span,
                })
            }
            (l, ty) if l == Type::UNKNOWN_IDX => {
                self.coerce_untyped_expr(&mut left, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::BOOL_IDX,
                    span: expr.span,
                })
            }
            (ty, r) if r == Type::UNKNOWN_IDX => {
                self.coerce_untyped_expr(&mut right, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::BOOL_IDX,
                    span: expr.span,
                })
            }
            (l, r) if l == Type::BOOL_IDX && r == Type::BOOL_IDX => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: Type::BOOL_IDX,
                span: expr.span,
            }),
            (left_type, right_type)
                if left_type == right_type
                    && self.tir.type_pool[left_type as usize].is_primitive() =>
            {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::BOOL_IDX,
                    span: expr.span,
                })
            }
            // TODO: compare enum variants
            // (Type::Enum { enum_index: e1 }, Type::Enum { enum_index: e2 }) if e1 == e2 =>
            // {
            //     Ok(Expression {
            //         kind: ExprKind::Binary {
            //             operator,
            //             left: Box::new(left),
            //             right: Box::new(right),
            //         },
            //         ty: Type::Bool,
            //         span: expr.span,
            //     })
            // }
            (left_type, right_type) => {
                self.tir
                    .diagnostics
                    .push(report_binary_expression_mistmatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        BinaryExpressionMistmatchDiagnostic {
                            file_id: self.file_id,
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
                    ty: Type::BOOL_IDX,
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
                                SourceSpan::new(self.file_id, left.span),
                            ));
                            return Err(());
                        }
                    };
                match local.mut_span {
                    None => {
                        self.tir
                            .diagnostics
                            .push(report_cannot_mutate_immutable(SourceSpan::new(
                                self.file_id,
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
                if right.ty == Type::UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right, local_type)?;
                } else if !self.coercible_to(right.ty, local_type) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.file_id,
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
                    ty: Type::UNIT_IDX,
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
                                self.file_id,
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
                if right.ty == Type::UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right, global_type)?;
                } else if !self.coercible_to(right.ty, global_type) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.file_id,
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
                    ty: Type::UNIT_IDX,
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
                if right.ty == Type::UNKNOWN_IDX {
                    self.tir
                        .diagnostics
                        .push(report_type_annotation_required(SourceSpan::new(
                            self.file_id,
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
                    ty: Type::UNIT_IDX,
                    span: expr.span,
                });
            }
            ExprKind::Deref { pointer, memory_id } => {
                let (inner_ty, mutable) = match &self.tir.type_pool[pointer.ty as usize] {
                    Type::Pointer { to, mutable, .. } => (*to, *mutable),
                    _ => unreachable!("Deref ExprKind must have Pointer type"),
                };

                if !mutable {
                    self.tir
                        .diagnostics
                        .push(report_cannot_store_through_immutable_pointer(
                            SourceSpan::new(self.file_id, expr.span),
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
                if right_expr.ty == Type::UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right_expr, inner_ty)?;
                } else if !self.coercible_to(right_expr.ty, inner_ty) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.file_id,
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
                            kind: ExprKind::Deref { pointer, memory_id },
                            ty: left_ty,
                            span: left_span,
                        }),
                        operator,
                        right: Box::new(right_expr),
                    },
                    ty: Type::UNIT_IDX,
                    span: expr.span,
                })
            }
            _ => {
                self.tir
                    .diagnostics
                    .push(report_invalid_assignment_target(SourceSpan::new(
                        self.file_id,
                        left.span,
                    )));

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: Type::UNIT_IDX,
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
                                SourceSpan::new(self.file_id, left.span),
                            ));
                            return Err(());
                        }
                    };
                // Allow operations with Error type (error already reported elsewhere)
                if local.ty == Type::ERROR_IDX {
                    let right = self.build_expression(
                        ctx,
                        AccessContext {
                            expected_type: Some(Type::ERROR_IDX),
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
                        ty: Type::UNIT_IDX,
                        span: expr.span,
                    });
                }
                if !self.tir.type_pool[local.ty as usize].is_primitive() {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.file_id,
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
                            self.file_id,
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
                if right.ty == Type::UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right, local_type)?;
                } else if !self.coercible_to(right.ty, local_type) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.file_id,
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
                    ty: Type::UNIT_IDX,
                    span: expr.span,
                })
            }
            ExprKind::Global { id } => {
                let global_index = self.tir.global_index_lookup[&id];
                let global = self.tir.globals.get(global_index as usize).unwrap();

                if !self.tir.type_pool[global.ty.inner as usize].is_primitive() {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.file_id,
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
                            self.file_id,
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
                if right.ty == Type::UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right, global_type)?;
                } else if !self.coercible_to(right.ty, global_type) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.file_id,
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
                    ty: Type::UNIT_IDX,
                    span: expr.span,
                })
            }
            ExprKind::Deref { pointer, memory_id } => {
                let (inner_ty, mutable) = match &self.tir.type_pool[pointer.ty as usize] {
                    Type::Pointer { to, mutable, .. } => (*to, *mutable),
                    _ => unreachable!("Deref ExprKind must have Pointer type"),
                };

                if !self.tir.type_pool[inner_ty as usize].is_primitive() {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.file_id,
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
                            SourceSpan::new(self.file_id, expr.span),
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
                if right_expr.ty == Type::UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right_expr, inner_ty)?;
                } else if !self.coercible_to(right_expr.ty, inner_ty) {
                    self.tir
                        .diagnostics
                        .push(report_binary_expression_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.file_id,
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
                            kind: ExprKind::Deref { pointer, memory_id },
                            ty: left_ty,
                            span: left_span,
                        }),
                        operator,
                        right: Box::new(right_expr),
                    },
                    ty: Type::UNIT_IDX,
                    span: expr.span,
                })
            }
            _ => {
                self.tir
                    .diagnostics
                    .push(report_invalid_assignment_target(SourceSpan::new(
                        self.file_id,
                        left.span,
                    )));

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: Type::UNIT_IDX,
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
                    let inferred_type = self.infer_block_type(scope, &value)?;
                    scope.inferred_type = Some(inferred_type);
                    if value.ty == Type::UNKNOWN_IDX {
                        self.coerce_untyped_expr(&mut value, inferred_type)?;
                    }

                    match scope.expected_type {
                        Some(expected_type) if !self.coercible_to(inferred_type, expected_type) => {
                            self.tir.diagnostics.push(report_type_mistmatch(
                                TypeFormatter::new(&self.tir, &self.interner),
                                TypeMistmatchDiagnostic {
                                    expected_type,
                                    actual_type: inferred_type,
                                    span: SourceSpan::new(self.file_id, value.span),
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
                        ty: Type::NEVER_IDX,
                        span: expr.span,
                    })
                })
                .unwrap_or(Expression {
                    kind: ExprKind::Unreachable,
                    ty: Type::NEVER_IDX,
                    span: expr.span,
                })),
            None => {
                let scope = ctx.stack.scopes.get_mut(ctx.scope_index as usize).unwrap();

                let inferred_type = scope.inferred_type.unwrap_or(Type::UNIT_IDX);
                scope.inferred_type = Some(inferred_type);

                match scope.expected_type {
                    Some(expected_type) if self.coercible_to(inferred_type, expected_type) => {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            TypeMistmatchDiagnostic {
                                expected_type,
                                actual_type: inferred_type,
                                span: SourceSpan::new(self.file_id, expr.span),
                            },
                        ));
                        return Err(());
                    }
                    _ => {}
                };

                Ok(Expression {
                    kind: ExprKind::Return { value: None },
                    ty: Type::NEVER_IDX,
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
                expected_type: match &self.tir.type_pool[left.ty as usize] {
                    Type::Unknown | Type::Error | Type::Never | Type::Unit => {
                        access_ctx.expected_type
                    }
                    _ => Some(left.ty),
                },
                access_kind: AccessKind::Read,
            },
            right,
        )?;

        match (left.ty, right.ty) {
            (l, r) if l == Type::UNKNOWN_IDX && r == Type::UNKNOWN_IDX => {
                match access_ctx.expected_type {
                    Some(_) => Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ty: Type::UNKNOWN_IDX,
                        span: expr.span,
                    }),
                    None => {
                        self.tir.diagnostics.push(report_type_annotation_required(
                            SourceSpan::new(self.file_id, expr.span),
                        ));
                        Err(())
                    }
                }
            }
            (l, ty) if l == Type::UNKNOWN_IDX => {
                if !self.tir.type_pool[ty as usize].is_primitive() {
                    self.tir
                        .diagnostics
                        .push(report_binary_operator_cannot_be_applied(
                            TypeFormatter::new(&self.tir, &self.interner),
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.file_id,
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
                        ty: access_ctx.expected_type.unwrap_or(Type::UNKNOWN_IDX),
                        span: expr.span,
                    });
                }
                self.coerce_untyped_expr(&mut left, ty)?;

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
            (ty, r) if r == Type::UNKNOWN_IDX => {
                // TODO: check if primitive
                self.coerce_untyped_expr(&mut right, ty)?;

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
            (l, _) if l == Type::NEVER_IDX => {
                self.tir
                    .diagnostics
                    .push(report_unreachable_code(SourceSpan::new(
                        self.file_id,
                        right.span,
                    )));

                Ok(left)
            }
            (_, r) if r == Type::NEVER_IDX => {
                self.tir
                    .diagnostics
                    .push(report_unreachable_code(SourceSpan::new(
                        self.file_id,
                        operator.span,
                    )));

                Ok(right)
            }
            (left_type, right_type)
                if left_type == right_type
                    && self.tir.type_pool[left_type as usize].is_primitive() =>
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
                            file_id: self.file_id,
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
        } = self.tir.type_pool[ty as usize]
        else {
            return &[];
        };
        self.type_param_bounds_by_owner(owner.clone(), param_index)
    }

    /// True when a non-generic `FunctionItem` can coerce to the given
    /// `Function` type. Generic functions (empty `type_args`) are rejected
    /// — they need explicit instantiation first.
    fn coercible_fn_item_to_fn(&self, arg_ty: TypeIndex, expected_ty: TypeIndex) -> bool {
        let &Type::FunctionItem { id, ref type_args } = &self.tir.type_pool[arg_ty as usize] else {
            return false;
        };
        if !type_args.is_empty() {
            return false;
        }
        if !matches!(
            self.tir.type_pool[expected_ty as usize],
            Type::Function { .. }
        ) {
            return false;
        }
        let fi = self.tir.function_index_lookup[&id] as usize;
        self.tir.functions[fi].signature_index == expected_ty
    }

    /// True when `arg_ty` is a `TypeParam` whose bounds include the trait that
    /// `expected_ty` represents.
    fn type_param_satisfies_bound(&self, arg_ty: TypeIndex, expected_ty: TypeIndex) -> bool {
        let &Type::Trait {
            trait_index: expected_trait,
        } = &self.tir.type_pool[expected_ty as usize]
        else {
            return false;
        };
        self.type_param_bounds(arg_ty)
            .iter()
            .any(|&b| b == expected_trait)
    }

    /// Builds, infers type args, and checks arguments for a generic call.
    /// `self_ty` always wins for slot 0; return-type expectation seeds type
    /// args first.
    fn build_generic_call_arguments(
        &mut self,
        ctx: &mut FunctionContext,
        arguments: &[Separated<Spanned<ast::Expression>>],
        params: &[TypeIndex],
        result_type: TypeIndex,
        type_params_len: usize,
        expected_result: Option<TypeIndex>,
        self_ty: Option<TypeIndex>,
    ) -> Result<(Box<[Expression]>, Vec<TypeIndex>), ()> {
        // Phase 1: build with no hint so all argument types are known before
        // any slot is filled — avoids left-to-right ordering issues.
        let mut args: Box<[Expression]> = arguments
            .iter()
            .map(|argument| {
                self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type: None,
                        access_kind: AccessKind::Read,
                    },
                    &argument.inner,
                )
            })
            .collect::<Result<Box<_>, _>>()?;

        // Phase 2: infer type_args.
        let mut type_args = vec![Type::UNKNOWN_IDX; type_params_len];

        if let Some(expected) = expected_result {
            self.infer_type_args_from_types(result_type, expected, &mut type_args);
        }

        for (arg, &param_ty) in args.iter().zip(params.iter()) {
            self.infer_type_args_from_types(param_ty, arg.ty, &mut type_args);
        }

        // Self is always authoritative; set it last so it can't be overridden
        // by return-type seeding or argument inference.
        if let Some(self_ty) = self_ty {
            type_args[0] = self_ty;
        }

        // Phase 3: check / coerce against the now-complete type_args.
        for (index, arg) in args.iter_mut().enumerate() {
            let param_type = match params.get(index).copied() {
                Some(p) => p,
                None => continue,
            };

            let expected_type = self.substitute_expected_type(param_type, &type_args);

            if let Some(expected) = expected_type {
                if arg.ty == Type::UNKNOWN_IDX {
                    self.coerce_untyped_expr(arg, expected)?;
                } else if !self.coercible_to(arg.ty, expected)
                    && !self.type_param_satisfies_bound(arg.ty, expected)
                    && !self.coercible_fn_item_to_fn(arg.ty, expected)
                {
                    self.tir.diagnostics.push(report_type_mistmatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        TypeMistmatchDiagnostic {
                            expected_type: expected,
                            actual_type: arg.ty,
                            span: SourceSpan::new(self.file_id, arg.span),
                        },
                    ));
                }
            } else if arg.ty == Type::UNKNOWN_IDX {
                self.tir
                    .diagnostics
                    .push(report_type_annotation_required(SourceSpan::new(
                        self.file_id,
                        arg.span,
                    )));
                return Err(());
            }
        }

        Ok((args, type_args))
    }

    fn build_call_arguments(
        &mut self,
        ctx: &mut FunctionContext,
        arguments: &[Separated<Spanned<ast::Expression>>],
        params: &[TypeIndex],
        type_args: &[TypeIndex],
    ) -> Result<Box<[Expression]>, ()> {
        arguments
            .iter()
            .enumerate()
            .map(|(index, argument)| {
                let expected_type = params
                    .get(index)
                    .copied()
                    .and_then(|param_type| self.substitute_expected_type(param_type, type_args));

                let mut argument = self.build_expression(
                    ctx,
                    AccessContext {
                        expected_type,
                        access_kind: AccessKind::Read,
                    },
                    &argument.inner,
                )?;

                if let Some(expected_type) = expected_type {
                    if argument.ty == Type::UNKNOWN_IDX {
                        self.coerce_untyped_expr(&mut argument, expected_type)?;
                    } else if !self.coercible_to(argument.ty, expected_type)
                        && !self.type_param_satisfies_bound(argument.ty, expected_type)
                        && !self.coercible_fn_item_to_fn(argument.ty, expected_type)
                    {
                        self.tir.diagnostics.push(report_type_mistmatch(
                            TypeFormatter::new(&self.tir, &self.interner),
                            TypeMistmatchDiagnostic {
                                expected_type,
                                actual_type: argument.ty,
                                span: SourceSpan::new(self.file_id, argument.span),
                            },
                        ));
                    }
                } else if argument.ty == Type::UNKNOWN_IDX {
                    // Argument passed to a TypeParam parameter with no concrete type
                    // and no inference from context — require an explicit annotation.
                    self.tir
                        .diagnostics
                        .push(report_type_annotation_required(SourceSpan::new(
                            self.file_id,
                            argument.span,
                        )));
                    return Err(());
                }

                Ok(argument)
            })
            .collect::<Result<Box<_>, _>>()
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
        let signature_index = match self.tir.type_pool[callee.ty as usize] {
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
                        SourceSpan::new(self.file_id, ast_callee.span)
                            .primary_label()
                            .with_message(format!(
                                "expected function, found `{}`",
                                formatter.display_type(callee.ty)
                            )),
                    );
                self.tir.diagnostics.push(diagnostic);

                return Ok(Expression {
                    kind: ExprKind::Call {
                        callee: Box::new(callee),
                        arguments: Box::new([]),
                    },
                    ty: Type::ERROR_IDX,
                    span: expr.span,
                });
            }
        };

        let signature = match &self.tir.type_pool[signature_index as usize] {
            Type::Function { signature } => signature.clone(),
            _ => unreachable!(),
        };
        match callee.kind {
            ExprKind::ObjectAccess { member, object } => {
                let method_entry = match &self.tir.type_pool[object.ty as usize] {
                    Type::Trait { trait_index } => self.tir.traits[*trait_index as usize]
                        .members
                        .get(&member.inner)
                        .cloned(),
                    Type::TypeParam { .. } => {
                        self.type_param_bounds(object.ty).iter().find_map(|&ti| {
                            self.tir.traits[ti as usize]
                                .members
                                .get(&member.inner)
                                .cloned()
                        })
                    }
                    _ => self
                        .tir
                        .impl_members
                        .get(&object.ty)
                        .and_then(|m| m.get(&member.inner))
                        .cloned(),
                };
                match method_entry {
                    Some(ImplEntry::Method(func_index)) => {
                        if let Some(access) =
                            self.tir.functions[func_index as usize].accesses.last_mut()
                        {
                            access.kind = FunctionAccessKind::DirectCall;
                        }
                        let id = self.tir.functions[func_index as usize].id;
                        let params = &signature.params()[1..];
                        if arguments.len() != params.len() {
                            self.tir.diagnostics.push(report_argument_count_mismatch(
                                TypeFormatter::new(&self.tir, &self.interner),
                                ArgumentCountMismatchDiagnostic {
                                    actual_count: arguments.len(),
                                    params,
                                    call_span: SourceSpan::new(self.file_id, callee.span),
                                    is_method: true,
                                },
                            ));
                        }

                        let type_params_len =
                            self.tir.functions[func_index as usize].type_params.len();
                        if type_params_len > 0 {
                            let (arguments, type_args) = self.build_generic_call_arguments(
                                ctx,
                                arguments,
                                params,
                                signature.result(),
                                type_params_len,
                                access_ctx.expected_type,
                                Some(object.ty),
                            )?;
                            let return_ty = self.substitute_type(signature.result(), &type_args);

                            return Ok(Expression {
                                kind: ExprKind::GenericMethodCall {
                                    id,
                                    type_args: type_args.into_boxed_slice(),
                                    object,
                                    arguments,
                                },
                                ty: return_ty,
                                span: expr.span,
                            });
                        }

                        let arguments = self.build_call_arguments(ctx, arguments, params, &[])?;

                        Ok(Expression {
                            kind: ExprKind::MethodCall {
                                object,
                                arguments,
                                id,
                            },
                            ty: signature.result(),
                            span: expr.span,
                        })
                    }
                    _ => todo!(
                        "report error for calling unknown member or associated function as method"
                    ),
                }
            }
            _ => {
                let params = signature.params();
                if arguments.len() != params.len() {
                    self.tir.diagnostics.push(report_argument_count_mismatch(
                        TypeFormatter::new(&self.tir, &self.interner),
                        ArgumentCountMismatchDiagnostic {
                            actual_count: arguments.len(),
                            params,
                            call_span: SourceSpan::new(self.file_id, callee.span),
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
                    if let Some(access) =
                        self.tir.functions[func_index as usize].accesses.last_mut()
                    {
                        access.kind = FunctionAccessKind::DirectCall;
                    }

                    let type_params_len = self.tir.functions[func_index as usize].type_params.len();
                    if type_params_len > 0 {
                        let (arguments, type_args) = self.build_generic_call_arguments(
                            ctx,
                            arguments,
                            params,
                            signature.result(),
                            type_params_len,
                            access_ctx.expected_type,
                            None,
                        )?;
                        let return_ty = self.substitute_type(signature.result(), &type_args);

                        return Ok(Expression {
                            kind: ExprKind::GenericCall {
                                id,
                                type_args: type_args.into_boxed_slice(),
                                arguments,
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
        }
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
                            .with_label(Label::primary(self.file_id, pattern.span)),
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
            (v, None) if v == Type::UNKNOWN_IDX => {
                self.tir
                    .diagnostics
                    .push(report_type_annotation_required(SourceSpan::new(
                        self.file_id,
                        name.span,
                    )));
                // Use Error type for recovery - this allows later references to work
                // without cascading "undeclared identifier" errors
                Type::ERROR_IDX
            }
            (ty, None) => ty,
            (v, Some(expected_type)) if v == Type::UNKNOWN_IDX => {
                self.coerce_untyped_expr(&mut value, expected_type)?;
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
                            span: SourceSpan::new(self.file_id, value.span),
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
            ty: if ty == Type::NEVER_IDX {
                Type::NEVER_IDX
            } else {
                Type::UNIT_IDX
            },
            span: stmt.inner.span,
        })
    }

    fn coerce_untyped_expr(
        &mut self,
        expression: &mut Expression,
        target_idx: TypeIndex,
    ) -> Result<(), ()> {
        match expression.kind {
            ExprKind::Int { .. } => self.coerce_untyped_int_expr(expression, target_idx),
            ExprKind::Float { .. } => self.coerce_untyped_float_expr(expression, target_idx),
            ExprKind::Unary { .. } => self.coerce_untyped_unary_expr(expression, target_idx),
            ExprKind::Binary { .. } => {
                self.coerce_untyped_binary_expression(expression, target_idx)
            }
            // Any other expression kind that ends up here already had an error
            // reported; propagate failure without emitting a second diagnostic.
            _ => Err(()),
        }
    }

    fn coerce_untyped_int_expr(
        &mut self,
        expr: &mut Expression,
        target_idx: TypeIndex,
    ) -> Result<(), ()> {
        let value = match expr.kind {
            ExprKind::Int { value } => value,
            _ => unreachable!(),
        };
        let formatter = TypeFormatter::new(&self.tir, &self.interner);

        if target_idx == Type::I32_IDX {
            if value > i32::MAX as i64 || value < i32::MIN as i64 {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::I32_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::I32_IDX;
            Ok(())
        } else if target_idx == Type::I64_IDX {
            if value > i64::MAX || value < i64::MIN {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::I64_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::I64_IDX;
            Ok(())
        } else if target_idx == Type::U32_IDX {
            if value > u32::MAX as i64 || value < 0 {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::U32_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::U32_IDX;
            Ok(())
        } else if target_idx == Type::U64_IDX {
            // i64 is at most i64::MAX which always fits in u64; only negative values are
            // invalid
            if value < 0 {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::U64_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::U64_IDX;
            Ok(())
        } else if target_idx == Type::U8_IDX {
            if value < 0 || value > u8::MAX as i64 {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::U8_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::U8_IDX;
            Ok(())
        } else if target_idx == Type::I8_IDX {
            if value < i8::MIN as i64 || value > i8::MAX as i64 {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::I8_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::I8_IDX;
            Ok(())
        } else if target_idx == Type::U16_IDX {
            if value < 0 || value > u16::MAX as i64 {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::U16_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::U16_IDX;
            Ok(())
        } else if target_idx == Type::I16_IDX {
            if value < i16::MIN as i64 || value > i16::MAX as i64 {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::I16_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::I16_IDX;
            Ok(())
        } else if target_idx == Type::CHAR_IDX {
            if value < 0 || value > u32::MAX as i64 {
                self.tir
                    .diagnostics
                    .push(report_integer_literal_out_of_range(
                        formatter,
                        IntegerLiteralOutOfRangeDiagnostic {
                            ty: Type::CHAR_IDX,
                            value,
                            span: SourceSpan::new(self.file_id, expr.span),
                        },
                    ));
            }
            expr.ty = Type::CHAR_IDX;
            Ok(())
        } else if target_idx == Type::F32_IDX || target_idx == Type::F64_IDX {
            self.tir
                .diagnostics
                .push(report_integer_literal_for_float_type(SourceSpan::new(
                    self.file_id,
                    expr.span,
                )));
            Err(())
        } else {
            self.tir.diagnostics.push(report_unable_to_coerce(
                formatter,
                target_idx,
                SourceSpan::new(self.file_id, expr.span),
            ));
            Err(())
        }
    }

    fn coerce_untyped_float_expr(
        &mut self,
        expr: &mut Expression,
        target_idx: TypeIndex,
    ) -> Result<(), ()> {
        if target_idx == Type::F32_IDX {
            // TODO: add a diagnostic if the literal is out of range
            expr.ty = Type::F32_IDX;
            Ok(())
        } else if target_idx == Type::F64_IDX {
            // TODO: add a diagnostic if the literal is out of range
            expr.ty = Type::F64_IDX;
            Ok(())
        } else {
            self.tir.diagnostics.push(report_unable_to_coerce(
                TypeFormatter::new(&self.tir, &self.interner),
                target_idx,
                SourceSpan::new(self.file_id, expr.span),
            ));
            Err(())
        }
    }

    fn coerce_untyped_unary_expr(
        &mut self,
        expr: &mut Expression,
        target_idx: TypeIndex,
    ) -> Result<(), ()> {
        let (operand, operator) = match &mut expr.kind {
            ExprKind::Unary { operand, operator } => (operand, operator.inner),
            _ => unreachable!(),
        };

        match operator {
            ast::UnaryOp::BitNot | ast::UnaryOp::InvertSign => {
                let is_valid = target_idx == Type::I32_IDX || target_idx == Type::I64_IDX;
                if !is_valid {
                    self.tir.diagnostics.push(report_unable_to_coerce(
                        TypeFormatter::new(&self.tir, &self.interner),
                        target_idx,
                        SourceSpan::new(self.file_id, expr.span),
                    ));
                    return Err(());
                }
            }
            _ => unreachable!(),
        }

        self.coerce_untyped_expr(operand, target_idx)
            .and_then(|_| Ok(expr.ty = target_idx))
    }

    fn coerce_untyped_binary_expression(
        &mut self,
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
                if !self.tir.type_pool[target_idx as usize].is_primitive() {
                    self.tir.diagnostics.push(report_unable_to_coerce(
                        TypeFormatter::new(&self.tir, &self.interner),
                        target_idx,
                        SourceSpan::new(self.file_id, expr.span),
                    ));
                    return Err(());
                }
            }
            operator if operator.is_bitwise() => {
                let is_integer = target_idx == Type::I32_IDX
                    || target_idx == Type::I64_IDX
                    || target_idx == Type::U32_IDX
                    || target_idx == Type::U64_IDX;
                if !is_integer {
                    self.tir.diagnostics.push(report_unable_to_coerce(
                        TypeFormatter::new(&self.tir, &self.interner),
                        target_idx,
                        SourceSpan::new(self.file_id, expr.span),
                    ));
                    return Err(());
                }
            }
            _ => unreachable!(),
        };

        match (
            self.coerce_untyped_expr(left, target_idx),
            self.coerce_untyped_expr(right, target_idx),
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
        init_span: ast::TextSpan,
        name: ast::Spanned<SymbolU32>,
        fields: &[ast::Separated<ast::Spanned<ast::StructInitField>>],
    ) -> Result<Expression, ()> {
        let struct_index = match self
            .symbol_lookup
            .get(&(SymbolNamespace::Type, name.inner))
            .cloned()
        {
            Some(SymbolKind::Struct { struct_index }) => struct_index,
            Some(_) => {
                self.tir.diagnostics.push(report_not_a_struct_type(
                    self.file_id,
                    self.interner.resolve(name.inner).unwrap().to_string(),
                    name.span,
                ));
                return Err(());
            }
            None => {
                self.tir
                    .diagnostics
                    .push(report_undeclared_identifier(SourceSpan::new(
                        self.file_id,
                        name.span,
                    )));
                return Err(());
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
                            file_id: self.file_id,
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
                        SourceSpan::new(self.file_id, first_span),
                        SourceSpan::new(self.file_id, field.name.span),
                    ));
                continue;
            }
            // Mark this field as mentioned (by its name span) before building the value,
            // so that build errors don't cause it to appear in the "missing fields" list.
            first_mention[field_index] = Some(field.name.span);

            let field_value = match &field.value {
                Some(expr) => expr.as_ref(),
                None => {
                    // Shorthand: treat `{ a }` as `{ a: a }` by synthesising an identifier expr
                    &ast::Spanned {
                        inner: ast::Expression::Identifier {
                            symbol: field.name.inner,
                        },
                        span: field.name.span,
                    }
                }
            };
            let expected_ty = self.tir.structs[struct_index as usize].fields[field_index]
                .ty
                .inner;
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

            if field_expr.ty == Type::UNKNOWN_IDX {
                match self.coerce_untyped_expr(&mut field_expr, expected_ty) {
                    Ok(_) => {}
                    Err(_) => continue,
                }
            } else if !self.coercible_to(field_expr.ty, expected_ty) {
                self.tir.diagnostics.push(report_type_mistmatch(
                    TypeFormatter::new(&self.tir, &self.interner),
                    TypeMistmatchDiagnostic {
                        expected_type: expected_ty,
                        actual_type: field_expr.ty,
                        span: SourceSpan::new(self.file_id, field_expr.span),
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
                    file_id: self.file_id,
                    struct_name: &struct_name,
                    missing_fields: missing,
                    init_span,
                },
            ));
        }

        let ty = self.intern_type(Type::Struct { struct_index });
        self.tir.structs[struct_index as usize]
            .accesses
            .push(SourceSpan::new(self.file_id, name.span));

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
        // If the expected type is a tuple, use its element types as hints.
        let expected_elems: Option<Box<[TypeIndex]>> = access_ctx.expected_type.and_then(|ty| {
            if let Type::Tuple { elements } = &self.tir.type_pool[ty as usize] {
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
                    if e.ty == Type::UNKNOWN_IDX {
                        if let Some(exp_ty) = expected {
                            let _ = self.coerce_untyped_expr(&mut e, exp_ty);
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

        let (inner_ty, memory_id) = match &self.tir.type_pool[pointer.ty as usize] {
            Type::Pointer { to, memory, .. } => {
                let inner_ty = *to;
                let memory_id = match memory {
                    Some(id) => *id,
                    None => match self.tir.memories.len() {
                        0 => {
                            self.tir.diagnostics.push(report_no_memory_for_pointer(
                                SourceSpan::new(self.file_id, span),
                            ));
                            return Err(());
                        }
                        1 => self.tir.memories[0].id,
                        _ => {
                            self.tir.diagnostics.push(report_ambiguous_pointer_memory(
                                SourceSpan::new(self.file_id, span),
                            ));
                            return Err(());
                        }
                    },
                };
                (inner_ty, memory_id)
            }
            _ => {
                self.tir.diagnostics.push(report_cannot_deref_non_pointer(
                    SourceSpan::new(self.file_id, pointer.span),
                    TypeFormatter::new(&self.tir, &self.interner).display_type(pointer.ty),
                ));
                return Err(());
            }
        };

        Ok(Expression {
            kind: ExprKind::Deref {
                pointer: Box::new(pointer),
                memory_id,
            },
            ty: inner_ty,
            span,
        })
    }
}
