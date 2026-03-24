use std::collections::HashMap;
use std::hash::Hash;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use string_interner::symbol::SymbolU32;

use crate::ast::{self, Annotated, FileId, Grouped, Separated, Spanned, StringInterner, TextSpan};

/// Unescape a string literal, removing quotes and handling escape sequences.
/// Supports basic Rust-like escape sequences: \n, \r, \t, \\, \", \'
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

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    U32,
    U64,
    Unit,
    Never,
    Unknown,
    Error,
    String,
    Bool,
    Function { signature_index: u32 },
    Namespace { namespace_index: u32 },
}

impl Clone for ast::Spanned<Type> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner,
            span: self.span,
        }
    }
}

impl Type {
    pub fn is_primitive(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::F32 | Type::F64 => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 => true,
            _ => false,
        }
    }

    pub fn coercible_to(self, other: Type) -> bool {
        match (self, other) {
            (a, b) if a == b => true,
            (Type::Never, _) => true,
            (Type::Error, _) => true,
            (Type::Unknown, _) => false,
            _ => false,
        }
    }

    pub fn unify(a: Type, b: Type) -> Result<Type, ()> {
        match (a, b) {
            (a, b) if a == b => Ok(a),
            (_, Type::Never) | (Type::Never, _) => Ok(b),
            // ??? do we need to propagate error type like this?
            (Type::Error, _) | (_, Type::Error) => Ok(Type::Error),
            _ => Err(()),
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, <Type as TryFrom<&str>>::Error> {
        match value {
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "bool" => Ok(Type::Bool),
            "unit" => Ok(Type::Unit),
            "never" => Ok(Type::Never),
            "string" => Ok(Type::String),
            _ => Err(()),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSignature {
    pub items: Box<[Type]>,
    pub params_count: usize,
}

impl FunctionSignature {
    pub fn params(&self) -> &[Type] {
        &self.items[..self.params_count]
    }

    pub fn result(&self) -> Type {
        self.items[self.params_count]
    }
}

impl std::hash::Hash for FunctionSignature {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for item in self.items.iter().copied() {
            item.hash(state);
        }
        self.params_count.hash(state);
    }
}

pub type LocalIndex = u32;
pub type ScopeIndex = u32;
pub type FunctionIndex = u32;
pub type NamespaceIndex = u32;
pub type GlobalIndex = u32;
pub type SignatureIndex = u32;
pub type EnumVariantIndex = u32;

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ExprKind {
    Error,
    Placeholder,
    Unreachable,
    Int {
        value: i64,
    },
    Float {
        value: f64,
    },
    Bool {
        value: bool,
    },
    Global {
        global_index: GlobalIndex,
    },
    LocalDeclaration {
        name: ast::Spanned<SymbolU32>,
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    Local {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    Function {
        func_index: FunctionIndex,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    EnumVariant {
        namespace_index: NamespaceIndex,
        variant_index: EnumVariantIndex,
    },
    Unary {
        operator: ast::Spanned<ast::UnaryOp>,
        operand: Box<Expression>,
    },
    Binary {
        operator: ast::Spanned<ast::BinaryOp>,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    Block {
        scope_index: ScopeIndex,
        expressions: Box<[Expression]>,
        result: Option<Box<Expression>>,
    },
    IfElse {
        condition: Box<Expression>,
        then_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
    Break {
        scope_index: ScopeIndex,
        value: Option<Box<Expression>>,
    },
    Continue {
        scope_index: ScopeIndex,
    },
    Loop {
        scope_index: ScopeIndex,
        block: Box<Expression>,
    },
    NamespaceAccess {
        namespace_index: NamespaceIndex,
        namespace_span: ast::TextSpan,
        member: Box<Expression>,
    },
    String {
        symbol: SymbolU32,
    },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
    pub span: ast::TextSpan,
}

#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum AccessKind {
    Read,
    Write,
    ReadWrite,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
struct AccessContext {
    expected_type: Option<Type>,
    access_kind: AccessKind,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct LocalAccess {
    pub span: ast::TextSpan,
    pub kind: AccessKind,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Local {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: Type,
    pub mut_span: Option<ast::TextSpan>,
    pub accesses: Vec<LocalAccess>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq)]
pub enum BlockKind {
    Block,
    /// Loop blocks have an implicit `continue` at the end.
    /// Their type is inferred from `break` expressions, not the final
    /// expression.
    Loop,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct BlockScope {
    pub kind: BlockKind,
    pub label: Option<SymbolU32>,
    pub parent: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub inferred_type: Option<Type>,
    pub expected_type: Option<Type>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StackFrame {
    pub scopes: Vec<BlockScope>,
}

impl StackFrame {
    pub fn push_local(&mut self, scope_index: u32, local: Local) -> LocalIndex {
        let scope = &mut self.scopes[scope_index as usize];
        let local_index = scope.locals.len() as LocalIndex;
        scope.locals.push(local);
        local_index
    }

    pub fn get_local(&self, scope_index: ScopeIndex, local_index: LocalIndex) -> Option<&Local> {
        self.scopes
            .get(scope_index as usize)?
            .locals
            .get(local_index as usize)
    }

    pub fn get_mut_local(
        &mut self,
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    ) -> Option<&mut Local> {
        self.scopes
            .get_mut(scope_index as usize)?
            .locals
            .get_mut(local_index as usize)
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionParam {
    pub mut_span: Option<ast::TextSpan>,
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<Type>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
    pub name: ast::Spanned<SymbolU32>,
    pub params: Box<[FunctionParam]>,
    pub result: ast::Spanned<Type>,
    pub signature_index: SignatureIndex,
    pub stack: StackFrame,
    pub block: Box<Expression>,
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
#[derive(Clone)]
pub enum ExportItem {
    Function {
        internal_name: Spanned<SymbolU32>,
        external_name: Option<Spanned<SymbolU32>>,
        func_index: u32,
    },
    Global {
        internal_name: Spanned<SymbolU32>,
        external_name: Option<Spanned<SymbolU32>>,
        global_index: u32,
    },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Enum {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: Type,
    pub variants: Box<[EnumVariant]>,
    pub lookup: HashMap<SymbolU32, EnumVariantIndex>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct EnumVariant {
    pub name: ast::Spanned<SymbolU32>,
    pub value: Box<Expression>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct DeclaredGlobal {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<Type>,
    pub mut_span: Option<ast::TextSpan>,
    pub accesses: Vec<ast::TextSpan>,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GlobalValue {
    Global { global_index: GlobalIndex },
    Function { func_index: FunctionIndex },
    Namespace { namespace_index: u32 },
    True,
    False,
    Unreachable,
    Placeholder,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImportValue {
    Function { func_index: FunctionIndex },
    Global { global_index: GlobalIndex },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportedFunction {
    pub external_name: ast::Spanned<SymbolU32>,
    pub internal_name: ast::Spanned<SymbolU32>,
    pub signature_index: SignatureIndex,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportedGlobal {
    pub external_name: ast::Spanned<SymbolU32>,
    pub internal_name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<Type>,
    pub mut_span: Option<ast::TextSpan>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportModule {
    pub external_name: ast::Spanned<SymbolU32>,
    pub internal_name: Option<ast::Spanned<SymbolU32>>,
    pub lookup: HashMap<SymbolU32, ImportValue>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolNamespace {
    Type,
    Value,
}

#[derive(Clone)]
#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub struct DeclaredFunction {
    pub signature_index: SignatureIndex,
    pub name: ast::Spanned<SymbolU32>,
    pub accesses: Vec<ast::TextSpan>,
}

pub struct FunctionContext {
    pub lookup: HashMap<(ScopeIndex, SymbolU32), LocalIndex>,
    pub func_index: FunctionIndex,
    pub scope_index: ScopeIndex,
    pub frame: StackFrame,
}

impl FunctionContext {
    pub fn push_local(&mut self, local: Local) -> LocalIndex {
        let name_symbol = local.name.inner;
        let index = self.frame.push_local(self.scope_index, local);
        self.lookup.insert((self.scope_index, name_symbol), index);
        index
    }

    pub fn resolve_local(&self, symbol: SymbolU32) -> Option<(ScopeIndex, LocalIndex)> {
        let mut scope_index = self.scope_index;

        loop {
            if let Some(&value) = self.lookup.get(&(scope_index, symbol)) {
                return Some((scope_index, value));
            }

            scope_index = self.frame.scopes[scope_index as usize].parent?;
        }
    }

    pub fn enter_block<T>(&mut self, block: BlockScope, handler: impl FnOnce(&mut Self) -> T) -> T {
        let parent_scope_index = self.scope_index;
        self.scope_index = self.frame.scopes.len() as u32;
        self.frame.scopes.push(block);

        let result = handler(self);

        self.scope_index = parent_scope_index;
        result
    }

    pub fn resolve_label(&self, symbol: SymbolU32) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.frame.scopes[scope_index as usize];
            if scope.label == Some(symbol) {
                return Some(scope_index);
            }

            scope_index = match scope.parent {
                Some(parent) => parent,
                None => return None,
            };
        }
    }

    pub fn get_closest_loop_block(&self) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.frame.scopes[scope_index as usize];
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

macro_rules! define_diagnostic_codes {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $(
                $variant:ident => $code:literal,
            )*
        }
    ) => {
        $(#[$meta])*
        $vis enum $name {
            $($variant,)*
        }

        impl $name {
            pub const fn code(&self) -> &'static str {
                match self {
                    $(Self::$variant => $code,)*
                }
            }
        }

        impl std::str::FromStr for $name {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($code => Ok(Self::$variant),)*
                    _ => Err(()),
                }
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(self.code())
            }
        }
    };
}

define_diagnostic_codes! {
    pub enum DiagnosticCode {
        DuplicateDefinition => "E1000",
        TypeMistmatch => "E1001",
        TypeAnnotationRequired => "E1002",
        UnusedValue => "E1003",
        IntegerLiteralOutOfRange => "E1004",
        UnableToCoerce => "E1005",
        IntegerLiteralForFloatType => "E1006",
        UndeclaredIdentifier => "E1007",
        BinaryOperatorCannotBeApplied => "E1008",
        CannotCallExpression => "E1009",
        UnaryOperatorCannotBeApplied => "E1010",
        UndeclaredLabel => "E1011",
        BreakOutsideOfLoop => "E1012",
        InvalidAssignmentTarget => "E1013",
        ComparisonTypeAnnotationRequired => "E1014",
        NonConstantGlobalInitializer => "E1015",
        ArgumentCountMismatch => "E1016",
        DuplicateExport => "E1018",
        CannotExportItem => "E1019",
        CannotMutateImmutable => "W1000",
        UnusedVariable => "W1001",
        UnnecessaryMutability => "W1002",
        UnreachableCode => "W1003",
        UnusedItem => "W1004",
    }
}

struct DuplicateDefinitionDiagnostic<'interner> {
    file_id: FileId,
    name: &'interner str,
    namespace: SymbolNamespace,
    first_definition: TextSpan,
    second_definition: TextSpan,
}

impl DuplicateDefinitionDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        let namespace = match self.namespace {
            SymbolNamespace::Type => "type",
            SymbolNamespace::Value => "value",
        };
        Diagnostic::error()
            .with_code(DiagnosticCode::DuplicateDefinition.code())
            .with_message(format!(
                "the name `{}` is defined multiple times",
                self.name
            ))
            .with_label(Label::primary(self.file_id, self.second_definition))
            .with_label(
                Label::primary(self.file_id, self.first_definition).with_message(format!(
                    "previous definition of the {} `{}` here",
                    self.name, namespace
                )),
            )
            .with_note(format!(
                "`{}` must be defined only once in the {} namespace of this module",
                self.name, namespace
            ))
    }
}

struct NonConstantGlobalInitializerDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl NonConstantGlobalInitializerDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::NonConstantGlobalInitializer.code())
            .with_message("global variable initializers can only contain constant expressions")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct TypeMistmatchDiagnostic {
    file_id: FileId,
    expected_type: Type,
    actual_type: Type,
    span: TextSpan,
}

impl TypeMistmatchDiagnostic {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::TypeMistmatch.code())
            .with_message("type mismatch")
            .with_label(
                Label::primary(self.file_id, self.span).with_message(format!(
                    "expected `{}`, found `{}`",
                    builder.display_type(self.expected_type),
                    builder.display_type(self.actual_type)
                )),
            )
    }
}

struct TypeAnnotationRequiredDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl TypeAnnotationRequiredDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::TypeAnnotationRequired.code())
            .with_message("type annotation required")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct UnusedVariableDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl UnusedVariableDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(DiagnosticCode::UnusedVariable.code())
            .with_message("unused variable")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct UnusedFunctionDiagnostic {
    file_id: FileId,
    name: String,
    span: TextSpan,
}

impl UnusedFunctionDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(DiagnosticCode::UnusedItem.code())
            .with_message(format!("function `{}` is never used", self.name))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct UnusedGlobalDiagnostic {
    file_id: FileId,
    name: String,
    span: TextSpan,
}

impl UnusedGlobalDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(DiagnosticCode::UnusedItem.code())
            .with_message(format!("global variable `{}` is never used", self.name))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct UnnecessaryMutabilityDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl UnnecessaryMutabilityDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(DiagnosticCode::UnnecessaryMutability.code())
            .with_message("unnecessary mutability")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct UnreachableCodeDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl UnreachableCodeDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(DiagnosticCode::UnreachableCode.code())
            .with_message("unreachable code")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("this code will never be executed"),
            )
    }
}

struct UnusedValueDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl UnusedValueDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UnusedValue.code())
            .with_message("value must be used")
            .with_label(Label::primary(self.file_id, self.span).with_message("value never used"))
            .with_note("if you don't need the value, consider dropping it with assignment to `_`")
    }
}

struct IntegerLiteralOutOfRangeDiagnostic {
    file_id: FileId,
    ty: Type,
    value: i64,
    span: TextSpan,
}

impl IntegerLiteralOutOfRangeDiagnostic {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        // TODO: add the actual value and the type in the message, e.g.
        // the literal `2_583` does not fit into the type `i8` whose range is
        // `-128..=127` consider using the type `i16` instead
        Diagnostic::error()
            .with_code(DiagnosticCode::IntegerLiteralOutOfRange.code())
            .with_message(format!(
                "literal `{}` out of range for `{}`",
                self.value,
                builder.display_type(self.ty)
            ))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct UnableToCoerceDiagnostic {
    file_id: FileId,
    target_type: Type,
    span: TextSpan,
}

impl UnableToCoerceDiagnostic {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UnableToCoerce.code())
            .with_message(format!(
                "unable to coerce to type `{}`",
                builder.display_type(self.target_type)
            ))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct IntegerLiteralForFloatTypeDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl IntegerLiteralForFloatTypeDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::IntegerLiteralForFloatType.code())
            .with_message("cannot use an integer literal for a float type")
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("consider adding a decimal point, e.g. `1.0` instead of `1`")
    }
}

struct UndeclaredIdentifierDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl UndeclaredIdentifierDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UndeclaredIdentifier.code())
            .with_message("undeclared identifier")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct BinaryOperatorCannotBeAppliedDiagnostic {
    file_id: FileId,
    operator: Spanned<ast::BinaryOp>,
    operand: Spanned<Type>,
}

impl BinaryOperatorCannotBeAppliedDiagnostic {
    pub fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::BinaryOperatorCannotBeApplied.code())
            .with_message(format!(
                "operator `{}` cannot be applied to type `{}`",
                self.operator.inner,
                builder.display_type(self.operand.inner)
            ))
            .with_label(Label::primary(self.file_id, self.operand.span))
            .with_label(Label::secondary(self.file_id, self.operator.span))
    }
}

struct BinaryExpressionMistmatchDiagnostic {
    file_id: FileId,
    left_type: Spanned<Type>,
    operator: Spanned<ast::BinaryOp>,
    right_type: Spanned<Type>,
}

impl BinaryExpressionMistmatchDiagnostic {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        let left_type = builder.display_type(self.left_type.inner);
        let right_type = builder.display_type(self.right_type.inner);

        let message = match self.operator.inner {
            ast::BinaryOp::Add => format!("cannot add `{}` to `{}`", left_type, right_type),
            ast::BinaryOp::Sub => format!("cannot subtract `{}` from `{}`", left_type, right_type),
            ast::BinaryOp::Assign => format!("cannot assign `{}` to `{}`", left_type, right_type),
            ast::BinaryOp::Mul => format!("cannot multiply `{}` by `{}`", left_type, right_type),
            ast::BinaryOp::Div => format!("cannot divide `{}` by `{}`", left_type, right_type),
            ast::BinaryOp::Rem => format!(
                "cannot calculate the remainder of `{}` by `{}`",
                left_type, right_type
            ),
            ast::BinaryOp::Eq
            | ast::BinaryOp::NotEq
            | ast::BinaryOp::Less
            | ast::BinaryOp::LessEq
            | ast::BinaryOp::Greater
            | ast::BinaryOp::GreaterEq => {
                format!("cannot compare `{}` to `{}`", left_type, right_type)
            }
            ast::BinaryOp::MulAssign => {
                format!("cannot multiply-assign `{}` to `{}`", right_type, left_type)
            }
            ast::BinaryOp::DivAssign => {
                format!("cannot divide-assign `{}` by `{}`", right_type, left_type)
            }
            ast::BinaryOp::RemAssign => {
                format!(
                    "cannot remainder-assign `{}` by `{}`",
                    right_type, left_type
                )
            }
            ast::BinaryOp::AddAssign => {
                format!("cannot add-assign `{}` to `{}`", right_type, left_type)
            }
            ast::BinaryOp::SubAssign => {
                format!(
                    "cannot subtract-assign `{}` from `{}`",
                    right_type, left_type
                )
            }
            _ => {
                format!(
                    "cannot perform operation on `{}` and `{}`",
                    left_type, right_type
                )
            }
        };

        Diagnostic::error()
            .with_message(message)
            .with_label(
                Label::secondary(self.file_id, self.left_type.span)
                    .with_message(format!("`{}`", left_type)),
            )
            .with_label(
                Label::primary(self.file_id, self.right_type.span)
                    .with_message(format!("`{}`", right_type)),
            )
    }
}

struct CannotCallExpressionDiagnostic {
    file_id: FileId,
    ty: Type,
    span: TextSpan,
}

impl CannotCallExpressionDiagnostic {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::CannotCallExpression.code())
            .with_message("call expression requires function")
            .with_label(
                Label::primary(self.file_id, self.span).with_message(format!(
                    "expected function, found `{}`",
                    builder.display_type(self.ty)
                )),
            )
    }
}

struct UnaryOperatorCannotBeAppliedDiagnostic {
    file_id: FileId,
    operator: Spanned<ast::UnaryOp>,
    operand: Spanned<Type>,
}

impl UnaryOperatorCannotBeAppliedDiagnostic {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UnaryOperatorCannotBeApplied.code())
            .with_message(format!(
                "operator `{}` cannot be applied to type `{}`",
                self.operator.inner,
                builder.display_type(self.operand.inner)
            ))
            .with_label(Label::primary(self.file_id, self.operand.span))
            .with_label(Label::secondary(self.file_id, self.operator.span))
    }
}

struct UndeclaredLabelDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl UndeclaredLabelDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UndeclaredLabel.code())
            .with_message("undeclared label")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct BreakOutsideOfLoopDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl BreakOutsideOfLoopDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        // TODO: we can break from if else expressions as well, so the message should be
        // more general, e.g. "`break` outside of loop or labeled block"
        Diagnostic::error()
            .with_code(DiagnosticCode::BreakOutsideOfLoop.code())
            .with_message("`break` outside of loop")
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("`break` is only allowed inside loops or labeled blocks")
    }
}

struct CannotMutateImmutableDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl CannotMutateImmutableDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::CannotMutateImmutable.code())
            .with_message("cannot mutate immutable variable")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct InvalidAssignmentTargetDiagnostic {
    file_id: FileId,
    span: TextSpan,
}

impl InvalidAssignmentTargetDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::InvalidAssignmentTarget.code())
            .with_message("invalid assignment target")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("cannot assign to this expression"),
            )
            .with_note("assignment only allowed to a variable or `_`")
    }
}

struct DuplicateExportDiagnostic<'interner> {
    file_id: FileId,
    name: &'interner str,
    first_export: TextSpan,
    second_export: TextSpan,
}

impl DuplicateExportDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::DuplicateExport.code())
            .with_message(format!(
                "the name `{}` is exported multiple times",
                self.name
            ))
            .with_label(Label::primary(self.file_id, self.second_export))
            .with_label(
                Label::secondary(self.file_id, self.first_export)
                    .with_message(format!("previous export of `{}` here", self.name)),
            )
            .with_note(format!(
                "`{}` can only be exported once from this module",
                self.name
            ))
    }
}

struct ComparisonTypeAnnotationRequiredDiagnostic {
    file_id: FileId,
    left: TextSpan,
    right: TextSpan,
}

impl ComparisonTypeAnnotationRequiredDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::ComparisonTypeAnnotationRequired.code())
            .with_message("type annotation required")
            .with_label(Label::primary(self.file_id, self.left))
            .with_label(Label::primary(self.file_id, self.right))
            .with_note("at least one side of the comparison must have a known type")
    }
}

struct CannotExportItemDiagnostic<'interner> {
    file_id: FileId,
    name: &'interner str,
    span: TextSpan,
}

impl CannotExportItemDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::CannotExportItem.code())
            .with_message(format!("cannot export `{}`", self.name))
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("only functions and global variables can be exported")
    }
}

struct ArgumentCountMismatchDiagnostic<'a> {
    file_id: FileId,
    expected_count: usize,
    actual_count: usize,
    param_types: &'a [Type],
    call_span: TextSpan,
}

impl ArgumentCountMismatchDiagnostic<'_> {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        let mut diagnostic = Diagnostic::error()
            .with_code(DiagnosticCode::ArgumentCountMismatch.code())
            .with_message(format!(
                "this function takes {} {} but {} {} supplied",
                self.expected_count,
                if self.expected_count == 1 {
                    "argument"
                } else {
                    "arguments"
                },
                self.actual_count,
                if self.actual_count == 1 {
                    "argument was"
                } else {
                    "arguments were"
                },
            ))
            .with_label(Label::primary(self.file_id, self.call_span));

        if self.actual_count < self.expected_count {
            // Missing arguments
            let missing_count = self.expected_count - self.actual_count;
            let missing_types: Vec<String> = self.param_types[self.actual_count..]
                .iter()
                .map(|ty| builder.display_type(*ty))
                .collect();

            if missing_count == 1 {
                diagnostic = diagnostic.with_note(format!(
                    "argument #{} of type `{}` is missing",
                    self.actual_count + 1,
                    missing_types[0]
                ));
            } else {
                let types_str = missing_types.join("`, `");
                diagnostic = diagnostic.with_note(format!(
                    "{} arguments of type `{}` are missing",
                    missing_count.to_string(),
                    types_str
                ));
            }
        } else {
            // Extra arguments
            let extra_count = self.actual_count - self.expected_count;
            if extra_count == 1 {
                diagnostic =
                    diagnostic.with_note(format!("unexpected argument #{}", self.actual_count));
            } else {
                diagnostic = diagnostic.with_note(format!("{} unexpected arguments", extra_count));
            }
        }

        diagnostic
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<Type>,
    pub mut_span: Option<ast::TextSpan>,
    pub value: Box<ast::Spanned<Expression>>,
}

struct Builder<'ast, 'interner> {
    ast: &'ast ast::AST,
    interner: &'interner mut ast::StringInterner,

    defined_functions: HashMap<FunctionIndex, Function>,
    defined_globals: HashMap<GlobalIndex, Global>,
    exports: HashMap<SymbolU32, ExportItem>,
    diagnostics: Vec<Diagnostic<FileId>>,
    namespaces: Vec<Namespace>,
    declared_globals: Vec<DeclaredGlobal>,
    declared_functions: Vec<DeclaredFunction>,
    signatures: Vec<FunctionSignature>,
    signature_index_lookup: HashMap<FunctionSignature, SignatureIndex>,
    symbol_lookup: HashMap<(SymbolNamespace, SymbolU32), GlobalValue>,
}

enum BlockState<T> {
    Exhaustive(T),
    Incomplete(T),
}

impl Builder<'_, '_> {
    pub fn resolve_type(&mut self, type_expr: &ast::TypeExpression) -> Result<Type, ()> {
        match &type_expr {
            ast::TypeExpression::Error => Err(()),
            ast::TypeExpression::Identifier { symbol } => {
                let symbol = *symbol;
                let text = self.interner.resolve(symbol).unwrap();
                if let Ok(ty) = Type::try_from(text) {
                    return Ok(ty);
                }
                match self.symbol_lookup.get(&(SymbolNamespace::Type, symbol)) {
                    Some(GlobalValue::Namespace { namespace_index }) => Ok(Type::Namespace {
                        namespace_index: *namespace_index,
                    }),
                    _ => Err(()),
                }
            }
            ast::TypeExpression::Function { params, result } => {
                let result = self.resolve_type(&result.inner.inner).unwrap();
                let items = params
                    .inner
                    .iter()
                    .map(|ty| self.resolve_type(&ty.inner.inner).unwrap())
                    .chain(Some(result))
                    .collect::<Box<_>>();
                let signature_index = self.ensure_signature_index(&FunctionSignature {
                    items,
                    params_count: params.inner.len(),
                });
                Ok(Type::Function { signature_index })
            }
        }
    }

    pub fn build_signature(
        &mut self,
        params: &Grouped<Box<[Separated<Spanned<ast::FunctionParam>>]>>,
        result: &Annotated<Box<Spanned<ast::TypeExpression>>>,
    ) -> FunctionSignature {
        let result = self.resolve_type(&result.inner.inner).unwrap();
        let params_count = params.inner.len();
        let mut items = Vec::with_capacity(params_count + 1);
        for param in params.inner.iter() {
            items.push(
                self.resolve_type(&param.inner.inner.type_annotation.inner.inner)
                    .unwrap(),
            );
        }
        items.push(result);
        FunctionSignature {
            items: items.into_boxed_slice(),
            params_count,
        }
    }

    pub fn ensure_signature_index(&mut self, signature: &FunctionSignature) -> SignatureIndex {
        match self.signature_index_lookup.get(signature).cloned() {
            Some(type_index) => type_index,
            None => {
                let signature_index = self.signatures.len() as u32;
                self.signatures.push(signature.clone());
                self.signature_index_lookup
                    .insert(signature.clone(), signature_index);
                signature_index
            }
        }
    }

    pub fn resolve_value(&mut self, symbol: SymbolU32) -> Option<GlobalValue> {
        match self
            .symbol_lookup
            .get(&(SymbolNamespace::Value, symbol))
            .cloned()
        {
            Some(value) => Some(value),
            None => None,
        }
    }

    pub fn resolve_func(&self, symbol: SymbolU32) -> Option<FunctionIndex> {
        match self
            .symbol_lookup
            .get(&(SymbolNamespace::Value, symbol))
            .cloned()
        {
            Some(GlobalValue::Function { func_index }) => Some(func_index),
            _ => None,
        }
    }

    pub fn display_type(&self, ty: Type) -> String {
        match ty {
            Type::Unknown => "unknown".to_string(),
            Type::Error => "error".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Never => "never".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::String => "string".to_string(),
            Type::Namespace { namespace_index } => {
                let ns = &self.namespaces[namespace_index as usize];
                match ns {
                    Namespace::ImportModule(module) => {
                        let name = module
                            .internal_name
                            .as_ref()
                            .map(|x| x.inner)
                            .unwrap_or(module.external_name.inner);
                        self.interner.resolve(name).unwrap().to_string()
                    }
                    Namespace::Enum(enum_) => {
                        let name = enum_.name.inner;
                        self.interner.resolve(name).unwrap().to_string()
                    }
                }
            }
            Type::Function { signature_index } => {
                let func = &self.signatures[signature_index as usize];
                let params = func
                    .params()
                    .iter()
                    .map(|param| self.display_type(*param))
                    .collect::<Box<[_]>>()
                    .join(", ");

                format!("fn({}) -> {}", params, self.display_type(func.result()))
            }
        }
    }

    fn define_item(&mut self, item: &ast::Item) -> Result<(), ()> {
        match item {
            ast::Item::FunctionDefinition { signature, .. } => {
                match self
                    .symbol_lookup
                    .get(&(SymbolNamespace::Value, signature.name.inner))
                    .cloned()
                {
                    Some(first_definition) => {
                        let name = self.interner.resolve(signature.name.inner).unwrap();
                        let first_definition_span = match first_definition {
                            GlobalValue::Function { func_index } => {
                                let func = &self.declared_functions[func_index as usize];
                                func.name.span
                            }
                            GlobalValue::Global { global_index } => {
                                let global = &self.declared_globals[global_index as usize];
                                global.name.span
                            }
                            _ => unreachable!(),
                        };

                        self.diagnostics.push(
                            DuplicateDefinitionDiagnostic {
                                file_id: self.ast.file_id,
                                name,
                                namespace: SymbolNamespace::Value,
                                first_definition: first_definition_span,
                                second_definition: signature.name.span,
                            }
                            .report(),
                        );

                        // TODO: we should probably continue processing the function to find more
                        // errors
                        return Err(());
                    }
                    None => {}
                };

                let name_symbol = signature.name.inner;
                let ty = self.build_signature(&signature.params, &signature.result);
                let type_index = self.ensure_signature_index(&ty);

                let func_index = self.declared_functions.len() as u32;
                self.symbol_lookup.insert(
                    (SymbolNamespace::Value, name_symbol),
                    GlobalValue::Function { func_index },
                );
                self.declared_functions.push(DeclaredFunction {
                    signature_index: type_index,
                    name: signature.name.clone(),
                    accesses: Vec::new(),
                });
                Ok(())
            }
            ast::Item::GlobalDefinition {
                mut_span,
                name,
                type_annotation,
                ..
            } => {
                match self
                    .symbol_lookup
                    .get(&(SymbolNamespace::Value, name.inner))
                    .cloned()
                {
                    Some(first_definition) => {
                        let name_str = self.interner.resolve(name.inner).unwrap();
                        let first_definition_span = match first_definition {
                            GlobalValue::Function { func_index } => {
                                let func = &self.declared_functions[func_index as usize];
                                func.name.span
                            }
                            GlobalValue::Global { global_index } => {
                                let global = &self.declared_globals[global_index as usize];
                                global.name.span
                            }
                            _ => todo!(),
                        };

                        self.diagnostics.push(
                            DuplicateDefinitionDiagnostic {
                                file_id: self.ast.file_id,
                                name: name_str,
                                namespace: SymbolNamespace::Value,
                                first_definition: first_definition_span,
                                second_definition: name.span,
                            }
                            .report(),
                        );

                        return Err(());
                    }
                    None => {}
                };

                // Resolve the type
                let (ty, ty_span) = match type_annotation {
                    Some(type_ann) => match self.resolve_type(&type_ann.inner.inner) {
                        Ok(ty) => (ty, type_ann.inner.span),
                        Err(_) => {
                            // Type resolution error - still register the global with Error type
                            (Type::Error, type_ann.inner.span)
                        }
                    },
                    None => {
                        self.diagnostics.push(
                            TypeAnnotationRequiredDiagnostic {
                                file_id: self.ast.file_id,
                                span: name.span,
                            }
                            .report(),
                        );
                        // Use Error type but still register the global
                        (Type::Error, name.span)
                    }
                };

                let global_index = self.declared_globals.len() as u32;
                self.symbol_lookup.insert(
                    (SymbolNamespace::Value, name.inner),
                    GlobalValue::Global { global_index },
                );

                self.declared_globals.push(DeclaredGlobal {
                    name: name.clone(),
                    ty: ast::Spanned {
                        inner: ty,
                        span: ty_span,
                    },
                    mut_span: mut_span.clone(),
                    accesses: Vec::new(),
                });

                Ok(())
            }
            ast::Item::Export { .. } => Ok(()),
            ast::Item::Import {
                module,
                alias,
                entries,
            } => {
                let ns_index = self.namespaces.len() as u32;

                let external_name = {
                    let module_str = self.interner.resolve(module.inner).unwrap();
                    let unquoted = unescape_string(module_str);
                    Spanned {
                        inner: self.interner.get_or_intern(&unquoted),
                        span: module.span,
                    }
                };

                let module_symbol = match alias {
                    Some(alias) => alias.inner,
                    None => external_name.inner,
                };

                if let Some(existing) = self
                    .symbol_lookup
                    .get(&(SymbolNamespace::Type, module_symbol))
                {
                    let name_str = self.interner.resolve(module_symbol).unwrap();
                    let first_definition_span = match existing {
                        GlobalValue::Function { func_index } => {
                            self.declared_functions[*func_index as usize].name.span
                        }
                        GlobalValue::Global { global_index } => {
                            self.declared_globals[*global_index as usize].name.span
                        }
                        GlobalValue::Namespace { namespace_index } => {
                            match &self.namespaces[*namespace_index as usize] {
                                Namespace::ImportModule(module) => module
                                    .internal_name
                                    .as_ref()
                                    .map(|x| x.span)
                                    .unwrap_or(module.external_name.span),
                                Namespace::Enum(enum_) => enum_.name.span,
                            }
                        }
                        _ => alias.as_ref().unwrap_or(module).span,
                    };

                    self.diagnostics.push(
                        DuplicateDefinitionDiagnostic {
                            file_id: self.ast.file_id,
                            name: name_str,
                            namespace: SymbolNamespace::Value,
                            first_definition: first_definition_span,
                            second_definition: alias.as_ref().unwrap_or(module).span,
                        }
                        .report(),
                    );
                    return Err(());
                }

                self.symbol_lookup.insert(
                    (SymbolNamespace::Type, module_symbol),
                    GlobalValue::Namespace {
                        namespace_index: ns_index,
                    },
                );

                let module = self.define_import_module(external_name, alias.clone(), entries)?;
                self.namespaces.push(Namespace::ImportModule(module));

                Ok(())
            }
        }
    }

    fn define_import_module(
        &mut self,
        external_name: ast::Spanned<SymbolU32>,
        internal_name: Option<ast::Spanned<SymbolU32>>,
        entries: &Grouped<Box<[Separated<Spanned<ast::ImportEntry>>]>>,
    ) -> Result<ImportModule, ()> {
        let mut module = ImportModule {
            lookup: HashMap::new(),
            external_name,
            internal_name,
        };

        for entry in &entries.inner {
            let entry_name_symbol = match &entry.inner.inner.declaration {
                ast::ImportDeclaration::Function { signature } => signature.name.inner,
                ast::ImportDeclaration::Global { name, .. } => name.inner,
            };

            match module.lookup.get(&entry_name_symbol) {
                Some(existing_value) => {
                    let existing_span = match existing_value {
                        ImportValue::Function { func_index } => {
                            self.declared_functions[*func_index as usize].name.span
                        }
                        ImportValue::Global { global_index } => {
                            self.declared_globals[*global_index as usize].name.span
                        }
                    };

                    let name_str = self.interner.resolve(entry_name_symbol).unwrap();
                    self.diagnostics.push(
                        DuplicateDefinitionDiagnostic {
                            file_id: self.ast.file_id,
                            name: name_str,
                            namespace: SymbolNamespace::Value,
                            first_definition: existing_span,
                            second_definition: entry.inner.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
                None => {}
            };

            let import_value = match &entry.inner.inner.declaration {
                ast::ImportDeclaration::Function { signature } => {
                    let func_signature = self.build_signature(&signature.params, &signature.result);
                    let signature_index = self.ensure_signature_index(&func_signature);

                    let func_index = self.declared_functions.len() as u32;
                    self.declared_functions.push(DeclaredFunction {
                        name: signature.name.clone(),
                        signature_index,
                        accesses: Vec::new(),
                    });

                    ImportValue::Function { func_index }
                }
                ast::ImportDeclaration::Global {
                    name,
                    mut_span,
                    type_annotation,
                } => {
                    let ty = match self.resolve_type(&type_annotation.inner.inner) {
                        Ok(ty) => ty,
                        Err(_) => Type::Error,
                    };

                    let global_index = self.declared_globals.len() as u32;
                    self.declared_globals.push(DeclaredGlobal {
                        name: name.clone(),
                        ty: ast::Spanned {
                            inner: ty,
                            span: type_annotation.inner.span,
                        },
                        mut_span: *mut_span,
                        accesses: Vec::new(),
                    });

                    ImportValue::Global { global_index }
                }
            };

            module.lookup.insert(entry_name_symbol, import_value);
        }

        Ok(module)
    }

    fn build_item(&mut self, item: &ast::Item) -> Result<(), ()> {
        match item {
            ast::Item::FunctionDefinition { signature, block } => {
                self.build_function_definition(signature, block)
            }
            ast::Item::GlobalDefinition {
                name,
                value,
                mut_span,
                ..
            } => {
                let global_index = match self.resolve_value(name.inner) {
                    Some(GlobalValue::Global { global_index }) => global_index,
                    _ => return Err(()),
                };

                let global_type = self.declared_globals[global_index as usize].ty.clone();
                let value_expr = self.build_const_expression(value, global_type.inner)?;

                match value_expr.ty {
                    Type::Unknown => {
                        self.diagnostics.push(
                            TypeAnnotationRequiredDiagnostic {
                                file_id: self.ast.file_id,
                                span: value.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                    ty if !ty.coercible_to(global_type.inner) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected_type: global_type.inner,
                                actual_type: ty,
                                span: value.span,
                            }
                            .report(&self),
                        );
                        return Err(());
                    }
                    _ => {}
                }

                self.defined_globals.insert(
                    global_index,
                    Global {
                        name: name.clone(),
                        ty: global_type,
                        mut_span: *mut_span,
                        value: Box::new(ast::Spanned {
                            inner: value_expr,
                            span: value.span,
                        }),
                    },
                );

                Ok(())
            }
            ast::Item::Export { entries } => {
                // Process exports - look up already-defined items and add them to exports
                for entry in entries.inner.iter() {
                    let internal_name = &entry.inner.inner.name;

                    // Look up the item to export
                    let global_value = match self
                        .symbol_lookup
                        .get(&(SymbolNamespace::Value, internal_name.inner))
                    {
                        Some(value) => value.clone(),
                        None => {
                            self.diagnostics.push(
                                UndeclaredIdentifierDiagnostic {
                                    file_id: self.ast.file_id,
                                    span: internal_name.span,
                                }
                                .report(),
                            );
                            continue;
                        }
                    };

                    // Determine the export alias (or use the item name)
                    let external_name = entry.inner.inner.alias.as_ref().map(|alias_span| {
                        // Get the original escaped text from the interner (includes quotes)
                        let escaped_text = self.interner.resolve(alias_span.inner).unwrap();
                        let unescaped = unescape_string(escaped_text);
                        let symbol = self.interner.get_or_intern(&unescaped);
                        ast::Spanned {
                            inner: symbol,
                            span: alias_span.span,
                        }
                    });

                    // Create the export item
                    let export_item = match global_value {
                        GlobalValue::Function { func_index } => {
                            self.declared_functions[func_index as usize]
                                .accesses
                                .push(internal_name.span);

                            ExportItem::Function {
                                func_index,
                                internal_name: internal_name.clone(),
                                external_name,
                            }
                        }
                        GlobalValue::Global { global_index } => {
                            self.declared_globals[global_index as usize]
                                .accesses
                                .push(internal_name.span);

                            ExportItem::Global {
                                global_index,
                                internal_name: internal_name.clone(),
                                external_name,
                            }
                        }
                        _ => {
                            self.diagnostics.push(
                                CannotExportItemDiagnostic {
                                    file_id: self.ast.file_id,
                                    name: self.interner.resolve(internal_name.inner).unwrap(),
                                    span: internal_name.span,
                                }
                                .report(),
                            );
                            continue;
                        }
                    };

                    // Check for duplicate exports based on external export name
                    // The external name is either the alias or the internal name if no alias
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
                        } => {
                            if let Some(ext) = external_name {
                                (ext.inner, ext.span)
                            } else {
                                (internal_name.inner, internal_name.span)
                            }
                        }
                    };

                    match self.exports.get(&export_symbol) {
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
                                } => {
                                    if let Some(ext) = external_name {
                                        ext.span
                                    } else {
                                        internal_name.span
                                    }
                                }
                            };

                            self.diagnostics.push(
                                DuplicateExportDiagnostic {
                                    file_id: self.ast.file_id,
                                    name,
                                    first_export: first_export_span,
                                    second_export: export_span,
                                }
                                .report(),
                            );
                            continue;
                        }
                        None => {
                            self.exports.insert(export_symbol, export_item);
                        }
                    }
                }

                Ok(())
            }
            ast::Item::Import { .. } => {
                // Imports are defined in define_item to handle duplicate definition errors, so
                // nothing to do here
                Ok(())
            }
        }
    }

    fn build_const_expression(
        &mut self,
        expr: &ast::Spanned<ast::Expression>,
        expected_type: Type,
    ) -> Result<Expression, ()> {
        match &expr.inner {
            ast::Expression::Int { value } => Ok(Expression {
                kind: ExprKind::Int { value: *value },
                ty: expected_type,
                span: expr.span,
            }),
            ast::Expression::Float { value } => Ok(Expression {
                kind: ExprKind::Float { value: *value },
                ty: expected_type,
                span: expr.span,
            }),
            ast::Expression::Identifier { symbol } => {
                // Allow references to global constants like true/false
                match self.resolve_value(*symbol) {
                    Some(GlobalValue::True) => Ok(Expression {
                        kind: ExprKind::Bool { value: true },
                        ty: Type::Bool,
                        span: expr.span,
                    }),
                    Some(GlobalValue::False) => Ok(Expression {
                        kind: ExprKind::Bool { value: false },
                        ty: Type::Bool,
                        span: expr.span,
                    }),
                    _ => {
                        self.diagnostics.push(
                            NonConstantGlobalInitializerDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                        Err(())
                    }
                }
            }
            ast::Expression::Unary { operator, operand } => {
                let operand_expr = self.build_const_expression(operand, expected_type)?;

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
                let left_expr = self.build_const_expression(left, expected_type)?;
                let right_expr = self.build_const_expression(right, expected_type)?;

                let result_ty = match operator.inner {
                    operator if operator.is_comparison() || operator.is_logical() => Type::Bool,
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
                self.build_const_expression(&value.inner, expected_type)
            }
            ast::Expression::Cast { value, ty } => {
                let cast_type = self.resolve_type(&ty.inner)?;
                let value_expr = self.build_const_expression(value, cast_type)?;

                Ok(Expression {
                    kind: value_expr.kind,
                    ty: cast_type,
                    span: expr.span,
                })
            }
            _ => {
                self.diagnostics.push(
                    NonConstantGlobalInitializerDiagnostic {
                        file_id: self.ast.file_id,
                        span: expr.span,
                    }
                    .report(),
                );
                Err(())
            }
        }
    }

    fn build_function_definition(
        &mut self,
        signature: &ast::FunctionSignature,
        block: &Spanned<ast::Expression>,
    ) -> Result<(), ()> {
        let params = signature
            .params
            .inner
            .iter()
            .map(|param| FunctionParam {
                name: param.inner.inner.name.clone(),
                ty: Spanned {
                    inner: self
                        .resolve_type(&param.inner.inner.type_annotation.inner.inner)
                        .unwrap(),
                    span: param.inner.inner.type_annotation.inner.span,
                },
                mut_span: param.inner.inner.mut_span,
            })
            .collect::<Box<[_]>>();

        let lookup = params
            .iter()
            .enumerate()
            .map(|(index, param)| ((0 as ScopeIndex, param.name.inner), index as LocalIndex))
            .collect();

        let func_index = self.resolve_func(signature.name.inner).unwrap();
        let func_meta = self
            .declared_functions
            .get(func_index as usize)
            .cloned()
            .unwrap();
        let typed_signature = self
            .signatures
            .get(func_meta.signature_index as usize)
            .unwrap()
            .clone();

        let root_scope = BlockScope {
            parent: None,
            label: None,
            kind: BlockKind::Block,
            locals: params
                .iter()
                .map(|param| Local {
                    name: param.name.clone(),
                    mut_span: param.mut_span,
                    accesses: Vec::new(),
                    ty: param.ty.inner,
                })
                .collect(),
            inferred_type: None,
            expected_type: Some(typed_signature.result()),
        };

        let mut ctx = FunctionContext {
            func_index,
            frame: StackFrame {
                scopes: vec![root_scope],
            },
            scope_index: 0 as ScopeIndex,
            lookup,
        };
        let block = self.build_block_expression(&mut ctx, &block)?;

        self.defined_functions.insert(
            func_index,
            Function {
                params,
                result: Spanned {
                    inner: typed_signature.result(),
                    span: signature.result.inner.span,
                },
                signature_index: func_meta.signature_index,
                name: signature.name.clone(),
                stack: ctx.frame,
                block: Box::new(block),
            },
        );
        Ok(())
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

        let (statements, result) = match statements.inner.split_last() {
            Some((last, rest)) if last.separator.is_none() => match &last.inner.inner {
                ast::Statement::Expression(expr) => (rest, Some(expr.as_ref())),
                _ => (statements.inner.as_ref(), None),
            },
            _ => (statements.inner.as_ref(), None),
        };

        let expressions = match self.build_block_statements(ctx, statements) {
            BlockState::Exhaustive(expressions) => {
                self.report_local_warnings(&ctx.frame.scopes[ctx.scope_index as usize]);
                if result.is_some() || expressions.len() < statements.len() {
                    self.diagnostics.push(
                        UnreachableCodeDiagnostic {
                            file_id: self.ast.file_id,
                            span: TextSpan::merge(
                                match statements.get(expressions.len()) {
                                    Some(stmt) => stmt.inner.span,
                                    None => result.unwrap().span,
                                },
                                match result {
                                    Some(result) => result.span,
                                    None => statements.last().unwrap().inner.span,
                                },
                            ),
                        }
                        .report(),
                    );
                }

                let scope = &mut ctx.frame.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.unwrap_or(Type::Never);
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

        match ctx.frame.scopes[ctx.scope_index as usize].kind {
            BlockKind::Loop => {
                let result = match result {
                    Some(result) => Some(self.build_expression(
                        ctx,
                        &result,
                        AccessContext {
                            expected_type: Some(Type::Unit),
                            access_kind: AccessKind::Read,
                        },
                    )?),
                    None => None,
                };

                self.report_local_warnings(&ctx.frame.scopes[ctx.scope_index as usize]);

                Ok(Expression {
                    kind: ExprKind::Block {
                        scope_index: ctx.scope_index,
                        expressions,
                        result: result.map(Box::new),
                    },
                    ty: ctx.frame.scopes[ctx.scope_index as usize]
                        .inferred_type
                        .unwrap_or(Type::Never),
                    span: block.span,
                })
            }
            BlockKind::Block => {
                let result = self.build_block_result(ctx, result.as_deref())?;

                self.report_local_warnings(&ctx.frame.scopes[ctx.scope_index as usize]);

                let scope = &ctx.frame.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.expect("should have inferred type");
                match scope.expected_type {
                    Some(expected_type) if !inferred_type.coercible_to(expected_type) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected_type,
                                actual_type: inferred_type,
                                span: block.span,
                            }
                            .report(&self),
                        );
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
            if local.accesses.is_empty() && local.ty != Type::Error {
                self.diagnostics.push(
                    UnusedVariableDiagnostic {
                        file_id: self.ast.file_id,
                        span: local.name.span,
                    }
                    .report(),
                );
            }

            match local.mut_span {
                Some(mut_span)
                    if !local.accesses.iter().any(|access| {
                        access.kind == AccessKind::Write || access.kind == AccessKind::ReadWrite
                    }) =>
                {
                    self.diagnostics.push(
                        UnnecessaryMutabilityDiagnostic {
                            file_id: self.ast.file_id,
                            span: mut_span,
                        }
                        .report(),
                    );
                }
                _ => {}
            }
        }
    }

    fn report_unused_items(&mut self) {
        for (func_index, func) in self.defined_functions.iter() {
            if self.declared_functions[*func_index as usize]
                .accesses
                .is_empty()
            {
                let name = self.interner.resolve(func.name.inner).unwrap().to_string();
                self.diagnostics.push(
                    UnusedFunctionDiagnostic {
                        file_id: self.ast.file_id,
                        name,
                        span: func.name.span,
                    }
                    .report(),
                );
            }
        }

        for (global_index, global) in self.defined_globals.iter() {
            if self.declared_globals[*global_index as usize]
                .accesses
                .is_empty()
            {
                let name = self
                    .interner
                    .resolve(global.name.inner)
                    .unwrap()
                    .to_string();
                self.diagnostics.push(
                    UnusedGlobalDiagnostic {
                        file_id: self.ast.file_id,
                        name,
                        span: global.name.span,
                    }
                    .report(),
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
                    result,
                    AccessContext {
                        expected_type: ctx.frame.scopes[ctx.scope_index as usize].expected_type,
                        access_kind: AccessKind::Read,
                    },
                )?;

                let scope = &mut ctx.frame.scopes[ctx.scope_index as usize];
                let inferred_type = self.infer_block_type(scope, &result)?;
                scope.inferred_type = Some(inferred_type);
                if result.ty == Type::Unknown {
                    _ = self.coerce_untyped_expr(&mut result, inferred_type);
                }

                Ok(Some(result))
            }
            None => {
                let scope = &mut ctx.frame.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.unwrap_or(Type::Unit);
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
                Type::Never => {
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

    fn infer_block_type(&mut self, scope: &BlockScope, value: &Expression) -> Result<Type, ()> {
        match value.ty {
            Type::Unknown => match scope.inferred_type.or(scope.expected_type) {
                Some(ty) => Ok(ty),
                None => {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.ast.file_id,
                            span: value.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
            },
            result_type => match scope.inferred_type {
                Some(inferred_type) if !result_type.coercible_to(inferred_type) => {
                    self.diagnostics.push(
                        TypeMistmatchDiagnostic {
                            file_id: self.ast.file_id,
                            expected_type: inferred_type,
                            actual_type: result_type,
                            span: value.span,
                        }
                        .report(&self),
                    );
                    Ok(inferred_type)
                }
                Some(inferred) => Ok(inferred),
                None => match scope.expected_type {
                    Some(expected_type) if !result_type.coercible_to(expected_type) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected_type,
                                actual_type: result_type,
                                span: value.span,
                            }
                            .report(&self),
                        );
                        Err(())
                    }
                    _ => Ok(result_type),
                },
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
            value,
            AccessContext {
                access_kind: AccessKind::Read,
                expected_type: None,
            },
        )?;
        match value.ty {
            Type::Unit => Ok(value),
            Type::Error => {
                // Skip reporting unused value for error types, as the error has already been
                // reported
                Ok(value)
            }
            Type::Never => {
                let scope = ctx.frame.scopes.get_mut(ctx.scope_index as usize).unwrap();
                scope.inferred_type = scope.inferred_type.or(Some(Type::Never));

                Ok(value)
            }
            Type::Unknown => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: value.span,
                    }
                    .report(),
                );

                Err(())
            }
            _ => {
                self.diagnostics.push(
                    UnusedValueDiagnostic {
                        file_id: self.ast.file_id,
                        span: value.span,
                    }
                    .report(),
                );

                Ok(value)
            }
        }
    }

    fn build_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        match &expr.inner {
            ast::Expression::Int { value } => Ok(Expression {
                kind: ExprKind::Int { value: *value },
                ty: Type::Unknown,
                span: expr.span,
            }),
            ast::Expression::Float { value } => Ok(Expression {
                kind: ExprKind::Float { value: *value },
                ty: Type::Unknown,
                span: expr.span,
            }),
            ast::Expression::Unreachable => Ok(Expression {
                kind: ExprKind::Unreachable,
                ty: Type::Never,
                span: expr.span,
            }),
            ast::Expression::Error => Ok(Expression {
                kind: ExprKind::Error,
                ty: Type::Error,
                span: expr.span,
            }),
            ast::Expression::String { symbol } => Ok(Expression {
                kind: ExprKind::String { symbol: *symbol },
                ty: Type::String,
                span: expr.span,
            }),
            ast::Expression::Identifier { .. } => {
                self.build_identifier_expression(func_ctx, expr, access_ctx)
            }
            ast::Expression::Binary { .. } => {
                self.build_binary_expression(func_ctx, expr, access_ctx)
            }
            ast::Expression::Grouping { value } => {
                self.build_expression(func_ctx, &value.inner, access_ctx)
            }
            ast::Expression::Unary { .. } => {
                self.build_unary_expression(func_ctx, expr, access_ctx)
            }
            ast::Expression::Call { .. } => self.build_call_expression(func_ctx, expr),
            ast::Expression::NamespaceAccess { namespace, member } => {
                self.build_namespace_access_expression(namespace, member.clone())
            }
            ast::Expression::ObjectAccess { object, member } => {
                self.build_object_access_expression(func_ctx, object, member.clone(), access_ctx)
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
                self.build_if_else_expression(func_ctx, expr, None, access_ctx)
            }
            ast::Expression::Loop { .. } => {
                self.build_loop_expression(func_ctx, expr, None, access_ctx)
            }
            ast::Expression::Cast { .. } => self.build_cast_expression(func_ctx, expr, access_ctx),
            ast::Expression::Break { .. } => self.build_break_expression(func_ctx, expr),
            ast::Expression::Continue { .. } => self.build_continue_expression(func_ctx, expr),
            ast::Expression::Label { .. } => {
                self.build_label_expression(func_ctx, expr, access_ctx)
            }
        }
    }

    fn build_object_access_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        object_expr: &Spanned<ast::Expression>,
        member: Spanned<SymbolU32>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let object = self.build_expression(func_ctx, object_expr, access_ctx)?;
        match object.ty {
            Type::String => {
                todo!("string member access is not implemented yet")
            }
            _ => todo!(),
        }
    }

    fn build_namespace_access_expression(
        &mut self,
        namespace_expr: &Spanned<ast::TypeExpression>,
        member: Spanned<SymbolU32>,
    ) -> Result<Expression, ()> {
        let namespace_ty = self.resolve_type(&namespace_expr.inner)?;
        let namespace_index = match namespace_ty {
            Type::Namespace { namespace_index } => namespace_index,
            ty => {
                // TODO: Better error message that includes the actual type
                todo!("type `{}` is not a namespace", self.display_type(ty))
            }
        };

        let namespace = &self.namespaces[namespace_index as usize];
        match namespace {
            Namespace::Enum(enum_) => match enum_.lookup.get(&member.inner) {
                Some(variant_index) => Ok(Expression {
                    kind: ExprKind::NamespaceAccess {
                        namespace_index,
                        namespace_span: namespace_expr.span,
                        member: Box::new(Expression {
                            kind: ExprKind::EnumVariant {
                                namespace_index,
                                variant_index: *variant_index,
                            },
                            ty: enum_.ty,
                            span: member.span,
                        }),
                    },
                    ty: enum_.ty,
                    span: ast::TextSpan::merge(namespace_expr.span, member.span),
                }),
                _ => {
                    self.diagnostics.push(
                        UndeclaredIdentifierDiagnostic {
                            file_id: self.ast.file_id,
                            span: member.span,
                        }
                        .report(),
                    );
                    Err(())
                }
            },
            Namespace::ImportModule(module) => match module.lookup.get(&member.inner) {
                Some(ImportValue::Function { func_index }) => {
                    let func = &mut self.declared_functions[*func_index as usize];
                    func.accesses.push(member.span);
                    let ty = Type::Function {
                        signature_index: func.signature_index,
                    };
                    Ok(Expression {
                        kind: ExprKind::NamespaceAccess {
                            namespace_index,
                            namespace_span: namespace_expr.span,
                            member: Box::new(Expression {
                                kind: ExprKind::Function {
                                    func_index: *func_index,
                                },
                                ty,
                                span: member.span,
                            }),
                        },
                        ty,
                        span: ast::TextSpan::merge(namespace_expr.span, member.span),
                    })
                }
                Some(ImportValue::Global { global_index }) => {
                    let global = &mut self.declared_globals[*global_index as usize];
                    global.accesses.push(member.span);

                    Ok(Expression {
                        kind: ExprKind::NamespaceAccess {
                            namespace_index,
                            namespace_span: namespace_expr.span,
                            member: Box::new(Expression {
                                kind: ExprKind::Global {
                                    global_index: *global_index,
                                },
                                ty: global.ty.inner,
                                span: member.span,
                            }),
                        },
                        ty: global.ty.inner,
                        span: ast::TextSpan::merge(namespace_expr.span, member.span),
                    })
                }
                None => {
                    self.diagnostics.push(
                        UndeclaredIdentifierDiagnostic {
                            file_id: self.ast.file_id,
                            span: member.span,
                        }
                        .report(),
                    );
                    Err(())
                }
            },
        }
    }

    fn build_label_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let (label, block) = match &expr.inner {
            ast::Expression::Label { label, block } => (label.clone(), block),
            _ => unreachable!(),
        };

        match block.inner {
            ast::Expression::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: Some(label.inner),
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
                block,
                Some(label),
                AccessContext {
                    expected_type: access_ctx.expected_type,
                    access_kind: AccessKind::Read,
                },
            ),
            ast::Expression::Loop { .. } => self.build_loop_expression(
                ctx,
                block,
                Some(label),
                AccessContext {
                    expected_type: access_ctx.expected_type,
                    access_kind: AccessKind::Read,
                },
            ),
            _ => unreachable!(),
        }
    }

    fn build_loop_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
        label: Option<Spanned<SymbolU32>>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let block = match &expr.inner {
            ast::Expression::Loop { block } => block,
            _ => unreachable!(),
        };

        func_ctx.enter_block(
            BlockScope {
                label: label.map(|l| l.inner),
                kind: BlockKind::Loop,
                parent: Some(func_ctx.scope_index),
                locals: Vec::new(),
                inferred_type: None,
                expected_type: access_ctx.expected_type,
            },
            |ctx| {
                let block = self.build_block_expression(ctx, block)?;

                let scope = &ctx.frame.scopes[ctx.scope_index as usize];
                match (scope.expected_type, scope.inferred_type) {
                    (Some(expected_type), Some(inferred_type))
                        if !inferred_type.coercible_to(expected_type) =>
                    {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected_type,
                                actual_type: inferred_type,
                                span: expr.span,
                            }
                            .report(&self),
                        );
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
                Some(scope_index) => scope_index,
                None => {
                    self.diagnostics.push(
                        UndeclaredLabelDiagnostic {
                            file_id: self.ast.file_id,
                            span: label.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
            },
            None => ctx
                .get_closest_loop_block()
                .expect("continue expression must be inside a loop or a block with a label"),
        };

        Ok(Expression {
            kind: ExprKind::Continue { scope_index },
            ty: Type::Never,
            span: expr.span,
        })
    }

    fn build_if_else_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
        label: Option<Spanned<SymbolU32>>,
        access_ctx: AccessContext,
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
            condition,
            AccessContext {
                expected_type: Some(Type::Bool),
                access_kind: AccessKind::Read,
            },
        )?;

        let then_block = match then_block.inner {
            ast::Expression::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: label.clone().map(|l| l.inner),
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
                            label: label.map(|l| l.inner),
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

                match Type::unify(then_block.ty, else_block.ty) {
                    Ok(ty) => (Some(else_block), ty),
                    Err(_) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected_type: then_block.ty,
                                actual_type: else_block.ty,
                                span: ast_else_block.span,
                            }
                            .report(&self),
                        );
                        return Err(());
                    }
                }
            }
            None => match then_block.ty {
                Type::Unit | Type::Never => (None, Type::Unit),
                _ => panic!(
                    "if you want to return a value from if-else, you must provide an else block"
                ),
            },
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
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let (value, cast_type) = match &expr.inner {
            ast::Expression::Cast { value, ty } => (value, ty),
            _ => unreachable!(),
        };

        match self.resolve_type(&cast_type.inner) {
            Ok(cast_type) => {
                let mut value = self.build_expression(
                    ctx,
                    value,
                    AccessContext {
                        expected_type: Some(cast_type),
                        access_kind: access_ctx.access_kind,
                    },
                )?;
                match value.ty {
                    Type::Unknown => self.coerce_untyped_expr(&mut value, cast_type)?,
                    ty if ty == cast_type => {
                        // TODO: report redundant cast
                    }
                    Type::Bool if cast_type.is_integer() => value.ty = cast_type,
                    ty if ty != cast_type => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected_type: cast_type,
                                actual_type: ty,
                                span: value.span,
                            }
                            .report(&self),
                        );
                        // Set the type to the cast target to avoid cascading errors
                        value.ty = cast_type;
                    }
                    _ => {}
                };

                Ok(value)
            }
            Err(_) => self.build_expression(ctx, value, access_ctx),
        }
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
                Some(scope_index) => scope_index,
                None => {
                    self.diagnostics.push(
                        UndeclaredLabelDiagnostic {
                            file_id: self.ast.file_id,
                            span: label.span,
                        }
                        .report(),
                    );

                    // TODO: how to handle this better? we don't parse the value if the label is
                    // undeclared
                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: Type::Never,
                        span: expr.span,
                    });
                }
            },
            None => match ctx.get_closest_loop_block() {
                Some(scope_index) => scope_index,
                None => {
                    self.diagnostics.push(
                        BreakOutsideOfLoopDiagnostic {
                            file_id: self.ast.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );

                    // TODO: same as above, we don't parse the value if the break is outside of a
                    // loop
                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: Type::Never,
                        span: expr.span,
                    });
                }
            },
        };

        match value {
            Some(value) => Ok(self
                .build_expression(
                    ctx,
                    value,
                    AccessContext {
                        expected_type: ctx
                            .frame
                            .scopes
                            .get(scope_index as usize)
                            .unwrap()
                            .expected_type,
                        access_kind: AccessKind::Read,
                    },
                )
                .and_then(|mut value| {
                    let scope = ctx.frame.scopes.get_mut(scope_index as usize).unwrap();
                    let inferred_type = self.infer_block_type(scope, &value)?;
                    match value.ty {
                        Type::Unknown => {
                            self.coerce_untyped_expr(&mut value, inferred_type)?;
                        }
                        _ => {}
                    }
                    scope.inferred_type = Some(inferred_type);

                    Ok(Expression {
                        kind: ExprKind::Break {
                            scope_index,
                            value: Some(Box::new(value)),
                        },
                        ty: Type::Never,
                        span: expr.span,
                    })
                })
                .unwrap_or(Expression {
                    kind: ExprKind::Unreachable,
                    ty: Type::Never,
                    span: expr.span,
                })),
            None => {
                let scope = ctx.frame.scopes.get_mut(scope_index as usize).unwrap();
                match scope.inferred_type {
                    Some(inferred) => match Type::Unit.coercible_to(inferred) {
                        true => {}
                        false => {
                            self.diagnostics.push(
                                TypeMistmatchDiagnostic {
                                    file_id: self.ast.file_id,
                                    expected_type: inferred,
                                    actual_type: Type::Unit,
                                    span: expr.span,
                                }
                                .report(&self),
                            );
                        }
                    },
                    None => {
                        scope.inferred_type = Some(Type::Unit);
                    }
                }

                Ok(Expression {
                    kind: ExprKind::Break {
                        scope_index,
                        value: None,
                    },
                    ty: Type::Never,
                    span: expr.span,
                })
            }
        }
    }

    fn build_identifier_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let symbol = match expr.inner {
            ast::Expression::Identifier { symbol } => symbol.clone(),
            _ => unreachable!(),
        };
        match func_ctx.resolve_local(symbol) {
            Some((scope_index, local_index)) => {
                let local = func_ctx
                    .frame
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

        match self.resolve_value(symbol) {
            Some(global) => match global {
                GlobalValue::True => Ok(Expression {
                    kind: ExprKind::Bool { value: true },
                    ty: Type::Bool,
                    span: expr.span,
                }),
                GlobalValue::False => Ok(Expression {
                    kind: ExprKind::Bool { value: false },
                    ty: Type::Bool,
                    span: expr.span,
                }),
                GlobalValue::Placeholder => Ok(Expression {
                    kind: ExprKind::Placeholder,
                    ty: access_ctx.expected_type.unwrap_or(Type::Error),
                    span: expr.span,
                }),
                GlobalValue::Function { func_index } => {
                    let func = &mut self.declared_functions[func_index as usize];
                    func.accesses.push(expr.span);
                    Ok(Expression {
                        kind: ExprKind::Function { func_index },
                        ty: Type::Function {
                            signature_index: func.signature_index,
                        },
                        span: expr.span,
                    })
                }
                GlobalValue::Global { global_index } => {
                    let global = &mut self.declared_globals[global_index as usize];
                    global.accesses.push(expr.span);

                    Ok(Expression {
                        kind: ExprKind::Global { global_index },
                        ty: global.ty.inner,
                        span: expr.span,
                    })
                }
                // this must be handled in the namespace access expression
                GlobalValue::Namespace { .. } => todo!(),
                GlobalValue::Unreachable => unreachable!(),
            },
            None => {
                self.diagnostics.push(
                    UndeclaredIdentifierDiagnostic {
                        file_id: self.ast.file_id,
                        span: expr.span,
                    }
                    .report(),
                );

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: access_ctx.expected_type.unwrap_or(Type::Error),
                    span: expr.span,
                })
            }
        }
    }

    fn build_binary_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
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
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let (operator, ast_operand) = match &expr.inner {
            ast::Expression::Unary { operator, operand } => (operator.clone(), operand),
            _ => unreachable!(),
        };
        let mut operand = self.build_expression(
            ctx,
            ast_operand,
            AccessContext {
                expected_type: access_ctx.expected_type,
                access_kind: AccessKind::Read,
            },
        )?;

        match operator.inner {
            ast::UnaryOp::InvertSign | ast::UnaryOp::BitNot => match operand.ty {
                Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Unknown => {
                    let ty = operand.ty;
                    Ok(Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty,
                        span: expr.span,
                    })
                }
                _ => panic!("can't apply unary operator to this type"),
            },
            ast::UnaryOp::Not => match operand.ty {
                Type::Bool => Ok(Expression {
                    kind: ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty: Type::Bool,
                    span: expr.span,
                }),
                Type::Unknown => {
                    _ = self.coerce_untyped_expr(&mut operand, Type::Bool);

                    Ok(Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: Type::Bool,
                        span: expr.span,
                    })
                }
                ty => {
                    self.diagnostics.push(
                        UnaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.ast.file_id,
                            operator: operator.clone(),
                            operand: Spanned {
                                inner: ty,
                                span: operand.span,
                            },
                        }
                        .report(&self),
                    );

                    Ok(Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: Type::Bool,
                        span: expr.span,
                    })
                }
            },
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
            left,
            AccessContext {
                expected_type: Some(Type::Bool),
                access_kind: AccessKind::Read,
            },
        )?;
        match left.ty {
            Type::Error => {
                // Error already reported, allow operation to continue
            }
            Type::Unknown => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: left.span,
                    }
                    .report(),
                );
            }
            actual_type if actual_type != Type::Bool => {
                self.diagnostics.push(
                    TypeMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        expected_type: Type::Bool,
                        actual_type,
                        span: left.span,
                    }
                    .report(&self),
                );
            }
            _ => {}
        }
        let right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: Some(Type::Bool),
                access_kind: AccessKind::Read,
            },
        )?;
        match right.ty {
            Type::Error => {
                // Error already reported, allow operation to continue
            }
            Type::Unknown => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: right.span,
                    }
                    .report(),
                );
            }
            actual_type if actual_type != Type::Bool => {
                self.diagnostics.push(
                    TypeMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        expected_type: Type::Bool,
                        actual_type,
                        span: right.span,
                    }
                    .report(&self),
                );
            }
            _ => {}
        }

        Ok(Expression {
            kind: ExprKind::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            },
            ty: Type::Bool,
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

        let mut left = self.build_expression(ctx, left, access_ctx.clone())?;
        let mut right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: match left.ty {
                    Type::Unknown | Type::Error | Type::Never | Type::Unit => {
                        access_ctx.expected_type
                    }
                    ty => Some(ty),
                },
                access_kind: access_ctx.access_kind,
            },
        )?;

        match (left.ty, right.ty) {
            // Allow operations with Error type (error already reported elsewhere)
            (Type::Error, _) | (_, Type::Error) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: access_ctx.expected_type.unwrap_or(Type::Error),
                span: expr.span,
            }),
            (Type::Unknown, Type::Unknown) => match access_ctx.expected_type {
                Some(expected_type) => {
                    self.coerce_untyped_expr(&mut left, expected_type)?;
                    self.coerce_untyped_expr(&mut right, expected_type)?;

                    if !expected_type.is_integer() {
                        self.diagnostics.push(
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.ast.file_id,
                                operator: operator.clone(),
                                operand: Spanned {
                                    inner: expected_type,
                                    span: left.span,
                                },
                            }
                            .report(&self),
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
                }
                None => {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.ast.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                    Err(())
                }
            },
            (Type::Unknown, right_type) => {
                if !right_type.is_integer() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.ast.file_id,
                            operator: operator.clone(),
                            operand: Spanned {
                                inner: right_type,
                                span: right.span,
                            },
                        }
                        .report(&self),
                    );
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
            (left_type, Type::Unknown) => {
                if !left_type.is_integer() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.ast.file_id,
                            operator: operator.clone(),
                            operand: Spanned {
                                inner: left_type,
                                span: left.span,
                            },
                        }
                        .report(&self),
                    );
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
            (left_type, right_type) if left_type == right_type && left_type.is_integer() => {
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
                self.diagnostics.push(
                    BinaryExpressionMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        left_type: Spanned {
                            inner: left_type,
                            span: left.span,
                        },
                        operator: operator.clone(),
                        right_type: Spanned {
                            inner: right_type,
                            span: right.span,
                        },
                    }
                    .report(&self),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: access_ctx.expected_type.unwrap_or(Type::Unknown),
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
            left,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
        )?;
        let mut right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: Some(left.ty),
                access_kind: AccessKind::Read,
            },
        )?;

        match (left.ty, right.ty) {
            // Allow operations with Error type (error already reported elsewhere)
            (Type::Error, _) | (_, Type::Error) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: Type::Bool,
                span: expr.span,
            }),
            (Type::Unknown, Type::Unknown) => {
                self.diagnostics.push(
                    ComparisonTypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        left: left.span,
                        right: right.span,
                    }
                    .report(),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::Bool,
                    span: expr.span,
                })
            }
            (Type::Unknown, ty) => {
                self.coerce_untyped_expr(&mut left, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::Bool,
                    span: expr.span,
                })
            }
            (ty, Type::Unknown) => {
                self.coerce_untyped_expr(&mut right, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::Bool,
                    span: expr.span,
                })
            }
            (Type::Bool, Type::Bool) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: Type::Bool,
                span: expr.span,
            }),
            (left_type, right_type) if left_type == right_type && left_type.is_primitive() => {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::Bool,
                    span: expr.span,
                })
            }
            // TODO: compare namespace variants
            // (Type::Namespace { namespace_index }, Type::Namespace { namespace_index })
            //     if enum_index_1 == enum_index_2 =>
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
                self.diagnostics.push(
                    BinaryExpressionMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        left_type: Spanned {
                            inner: left_type,
                            span: left.span,
                        },
                        operator: operator.clone(),
                        right_type: Spanned {
                            inner: right_type,
                            span: right.span,
                        },
                    }
                    .report(&self),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::Bool,
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
            left,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Write,
            },
        )?;
        match left.kind {
            ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let local = match ctx.frame.get_mut_local(scope_index, local_index) {
                    Some(local) => local,
                    None => {
                        self.diagnostics.push(
                            UndeclaredIdentifierDiagnostic {
                                file_id: self.ast.file_id,
                                span: left.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                };
                match local.mut_span {
                    None => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    _ => {}
                }

                let local_type = local.ty;

                let mut right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: Some(local_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match right.ty {
                    Type::Unknown => {
                        self.coerce_untyped_expr(&mut right, local_type)?;
                    }
                    ty if !ty.coercible_to(local_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left_type: Spanned {
                                    inner: local_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: ty,
                                    span: right.span,
                                },
                            }
                            .report(&self),
                        );
                    }
                    _ => {}
                }

                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Type::Unit,
                    span: expr.span,
                })
            }
            ExprKind::Global { global_index } => {
                let global = &self.declared_globals[global_index as usize];
                match global.mut_span {
                    None => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    _ => {}
                }

                let global_type = global.ty.inner;
                let mut right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: Some(global_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match right.ty {
                    Type::Unknown => {
                        self.coerce_untyped_expr(&mut right, global_type)?;
                    }
                    ty if !ty.coercible_to(global_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left_type: Spanned {
                                    inner: global_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: ty,
                                    span: right.span,
                                },
                            }
                            .report(&self),
                        );
                    }
                    _ => {}
                }

                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Type::Unit,
                    span: expr.span,
                })
            }
            ExprKind::Placeholder => {
                let right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: None,
                        access_kind: AccessKind::Read,
                    },
                )?;
                let right_type = match right.ty {
                    Type::Unknown => {
                        self.diagnostics.push(
                            TypeAnnotationRequiredDiagnostic {
                                file_id: self.ast.file_id,
                                span: right.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                    ty => ty,
                };

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
                    ty: Type::Unit,
                    span: expr.span,
                });
            }
            _ => {
                self.diagnostics.push(
                    InvalidAssignmentTargetDiagnostic {
                        file_id: self.ast.file_id,
                        span: left.span,
                    }
                    .report(),
                );

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: Type::Unit,
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
            left,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::ReadWrite,
            },
        )?;
        match left.kind {
            ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let local = match ctx.frame.get_mut_local(scope_index, local_index) {
                    Some(local) => local,
                    None => {
                        self.diagnostics.push(
                            UndeclaredIdentifierDiagnostic {
                                file_id: self.ast.file_id,
                                span: left.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                };
                // Allow operations with Error type (error already reported elsewhere)
                if local.ty == Type::Error {
                    let right = self.build_expression(
                        ctx,
                        right,
                        AccessContext {
                            expected_type: Some(Type::Error),
                            access_kind: AccessKind::Read,
                        },
                    )?;
                    return Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ty: Type::Unit,
                        span: expr.span,
                    });
                }
                if !local.ty.is_primitive() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.ast.file_id,
                            operator,
                            operand: Spanned {
                                inner: local.ty,
                                span: left.span,
                            },
                        }
                        .report(&self),
                    );

                    return Err(());
                }
                if local.mut_span == None {
                    self.diagnostics.push(
                        CannotMutateImmutableDiagnostic {
                            file_id: self.ast.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                }

                let local_type = local.ty;
                let mut right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: Some(local_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match right.ty {
                    Type::Unknown => {
                        self.coerce_untyped_expr(&mut right, local_type)?;
                    }
                    ty if !ty.coercible_to(local_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left_type: Spanned {
                                    inner: local_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: ty,
                                    span: right.span,
                                },
                            }
                            .report(&self),
                        );
                    }
                    _ => {}
                }

                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Type::Unit,
                    span: expr.span,
                })
            }
            ExprKind::Global { global_index } => {
                let global = self
                    .declared_globals
                    .get_mut(global_index as usize)
                    .unwrap();

                if !global.ty.inner.is_primitive() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.ast.file_id,
                            operator,
                            operand: Spanned {
                                inner: global.ty.inner,
                                span: left.span,
                            },
                        }
                        .report(&self),
                    );

                    return Err(());
                }

                if global.mut_span == None {
                    self.diagnostics.push(
                        CannotMutateImmutableDiagnostic {
                            file_id: self.ast.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                }

                let global_type = global.ty.inner;
                let mut right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: Some(global_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match right.ty {
                    Type::Unknown => {
                        self.coerce_untyped_expr(&mut right, global_type)?;
                    }
                    ty if !ty.coercible_to(global_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left_type: Spanned {
                                    inner: global_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right_type: Spanned {
                                    inner: ty,
                                    span: right.span,
                                },
                            }
                            .report(&self),
                        );
                    }
                    _ => {}
                }

                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Type::Unit,
                    span: expr.span,
                })
            }
            _ => {
                self.diagnostics.push(
                    InvalidAssignmentTargetDiagnostic {
                        file_id: self.ast.file_id,
                        span: left.span,
                    }
                    .report(),
                );

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: Type::Unit,
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
                    value,
                    AccessContext {
                        expected_type: ctx.frame.scopes.get(0).unwrap().expected_type,
                        access_kind: AccessKind::Read,
                    },
                )
                .and_then(|mut value| {
                    let scope = ctx.frame.scopes.get_mut(0).unwrap();
                    let inferred_type = self.infer_block_type(scope, &value)?;
                    scope.inferred_type = Some(inferred_type);
                    match value.ty {
                        Type::Unknown => {
                            self.coerce_untyped_expr(&mut value, inferred_type)?;
                        }
                        _ => {}
                    }

                    match scope.expected_type {
                        Some(expected_type) if !inferred_type.coercible_to(expected_type) => {
                            self.diagnostics.push(
                                TypeMistmatchDiagnostic {
                                    file_id: self.ast.file_id,
                                    expected_type,
                                    actual_type: inferred_type,
                                    span: value.span,
                                }
                                .report(&self),
                            );
                            return Err(());
                        }
                        _ => {}
                    };

                    Ok(Expression {
                        kind: ExprKind::Return {
                            value: Some(Box::new(value)),
                        },
                        ty: Type::Never,
                        span: expr.span,
                    })
                })
                .unwrap_or(Expression {
                    kind: ExprKind::Unreachable,
                    ty: Type::Never,
                    span: expr.span,
                })),
            None => {
                let scope = ctx.frame.scopes.get_mut(ctx.scope_index as usize).unwrap();

                let inferred_type = scope.inferred_type.unwrap_or(Type::Unit);
                scope.inferred_type = Some(inferred_type);

                match scope.expected_type {
                    Some(expected_type) if inferred_type.coercible_to(expected_type) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected_type,
                                actual_type: inferred_type,
                                span: expr.span,
                            }
                            .report(&self),
                        );
                        return Err(());
                    }
                    _ => {}
                };

                Ok(Expression {
                    kind: ExprKind::Return { value: None },
                    ty: Type::Never,
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
            left,
            AccessContext {
                expected_type: access_ctx.expected_type,
                access_kind: AccessKind::Read,
            },
        )?;
        let mut right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: match left.ty {
                    Type::Unknown | Type::Error | Type::Never | Type::Unit => {
                        access_ctx.expected_type
                    }
                    ty => Some(ty),
                },
                access_kind: AccessKind::Read,
            },
        )?;

        match (left.ty, right.ty) {
            (Type::Unknown, Type::Unknown) => match access_ctx.expected_type {
                Some(_) => Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Type::Unknown,
                    span: expr.span,
                }),
                None => {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.ast.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                    Err(())
                }
            },
            (Type::Unknown, ty) => {
                if !ty.is_primitive() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.ast.file_id,
                            operator: operator.clone(),
                            operand: Spanned {
                                inner: ty,
                                span: right.span,
                            },
                        }
                        .report(&self),
                    );

                    return Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ty: access_ctx.expected_type.unwrap_or(Type::Unknown),
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
            (ty, Type::Unknown) => {
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
            (Type::Never, _) => {
                self.diagnostics.push(
                    UnreachableCodeDiagnostic {
                        file_id: self.ast.file_id,
                        span: right.span,
                    }
                    .report(),
                );

                Ok(left)
            }
            (_, Type::Never) => {
                self.diagnostics.push(
                    UnreachableCodeDiagnostic {
                        file_id: self.ast.file_id,
                        span: operator.span,
                    }
                    .report(),
                );

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
                self.diagnostics.push(
                    BinaryExpressionMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        left_type: Spanned {
                            inner: left_type,
                            span: left.span,
                        },
                        operator: operator.clone(),
                        right_type: Spanned {
                            inner: right_type,
                            span: right.span,
                        },
                    }
                    .report(&self),
                );

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

    fn build_call_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (ast_callee, ast_arguments) = match &expr.inner {
            ast::Expression::Call { callee, arguments } => (callee, arguments),
            _ => unreachable!(),
        };

        let callee = self.build_expression(
            ctx,
            ast_callee,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
        )?;
        let signature_index = match callee.ty {
            Type::Function { signature_index } => signature_index,
            Type::Unknown => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: ast_callee.span,
                    }
                    .report(),
                );

                return Ok(Expression {
                    kind: ExprKind::Call {
                        callee: Box::new(callee),
                        arguments: Box::new([]),
                    },
                    ty: Type::Unknown,
                    span: expr.span,
                });
            }
            ty => {
                self.diagnostics.push(
                    CannotCallExpressionDiagnostic {
                        file_id: self.ast.file_id,
                        span: ast_callee.span,
                        ty,
                    }
                    .report(&self),
                );

                return Ok(Expression {
                    kind: ExprKind::Call {
                        callee: Box::new(callee),
                        arguments: Box::new([]),
                    },
                    ty: Type::Unknown,
                    span: expr.span,
                });
            }
        };

        let func_type = self
            .signatures
            .get(signature_index as usize)
            .unwrap()
            .clone();

        let expected_count = func_type.params().len();
        let actual_count = ast_arguments.inner.len();

        let arguments: Box<_> = ast_arguments
            .inner
            .iter()
            .enumerate()
            .map(|(index, argument)| {
                let expected_type = func_type.params().get(index).copied();

                let mut argument = self.build_expression(
                    ctx,
                    &argument.inner,
                    AccessContext {
                        expected_type,
                        access_kind: AccessKind::Read,
                    },
                )?;

                // Only do type checking if we have a valid parameter at this index
                if let Some(expected_type) = expected_type {
                    match argument.ty {
                        Type::Unknown => {
                            self.coerce_untyped_expr(&mut argument, expected_type)?;
                        }
                        ty if !ty.coercible_to(expected_type) => {
                            self.diagnostics.push(
                                TypeMistmatchDiagnostic {
                                    file_id: self.ast.file_id,
                                    expected_type,
                                    actual_type: ty,
                                    span: argument.span,
                                }
                                .report(&self),
                            );
                        }
                        _ => {}
                    }
                }

                Ok(argument)
            })
            .collect::<Result<_, _>>()?;

        // Check argument count mismatch
        if actual_count != expected_count {
            self.diagnostics.push(
                ArgumentCountMismatchDiagnostic {
                    file_id: self.ast.file_id,
                    expected_count,
                    actual_count,
                    param_types: func_type.params(),
                    call_span: callee.span,
                }
                .report(&self),
            );
        }

        Ok(Expression {
            kind: ExprKind::Call {
                callee: Box::new(callee),
                arguments,
            },
            ty: self
                .signatures
                .get(signature_index as usize)
                .unwrap()
                .result(),
            span: expr.span,
        })
    }

    fn build_local_definition_statement(
        &mut self,
        ctx: &mut FunctionContext,
        stmt: &Separated<Spanned<ast::Statement>>,
    ) -> Result<Expression, ()> {
        let (mut_span, name, annotation, value) = match &stmt.inner.inner {
            ast::Statement::LocalDefinition {
                mut_span,
                name,
                type_annotation,
                value,
                ..
            } => (mut_span.clone(), name.clone(), type_annotation, value),
            _ => unreachable!(),
        };

        let expected_type = match annotation {
            Some(annotation) => self.resolve_type(&annotation.inner.inner).ok(),
            None => None,
        };
        let mut value = self.build_expression(
            ctx,
            value,
            AccessContext {
                expected_type,
                access_kind: AccessKind::Read,
            },
        )?;

        let ty = match (value.ty, expected_type) {
            (Type::Unknown, None) => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: name.span,
                    }
                    .report(),
                );
                // Use Error type for recovery - this allows later references to work
                // without cascading "undeclared identifier" errors
                Type::Error
            }
            (ty, None) => ty,
            (Type::Unknown, Some(expected_type)) => {
                self.coerce_untyped_expr(&mut value, expected_type)?;
                expected_type
            }
            (actual_type, Some(expected_type)) => {
                if actual_type.coercible_to(expected_type) {
                    expected_type
                } else {
                    self.diagnostics.push(
                        TypeMistmatchDiagnostic {
                            file_id: self.ast.file_id,
                            expected_type,
                            actual_type,
                            span: value.span,
                        }
                        .report(&self),
                    );
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
            ty: match ty {
                Type::Never => Type::Never,
                _ => Type::Unit,
            },
            span: stmt.inner.span,
        })
    }

    fn coerce_untyped_expr(
        &mut self,
        expression: &mut Expression,
        target_type: Type,
    ) -> Result<(), ()> {
        match expression.kind {
            ExprKind::Int { .. } => self.coerce_untyped_int_expr(expression, target_type),
            ExprKind::Float { .. } => self.coerce_untyped_float_expr(expression, target_type),
            ExprKind::Unary { .. } => self.coerce_untyped_unary_expr(expression, target_type),
            ExprKind::Binary { .. } => {
                self.coerce_untyped_binary_expression(expression, target_type)
            }
            _ => unimplemented!(),
        }
    }

    fn coerce_untyped_int_expr(
        &mut self,
        expr: &mut Expression,
        target_type: Type,
    ) -> Result<(), ()> {
        match target_type {
            Type::I32 => {
                let value = match expr.kind {
                    ExprKind::Int { value } => value,
                    _ => unreachable!(),
                };

                if value > i32::MAX as i64 || value < i32::MIN as i64 {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            ty: Type::I32,
                            value,
                            span: expr.span,
                        }
                        .report(&self),
                    );
                }

                expr.ty = Type::I32;
                Ok(())
            }
            Type::I64 => {
                let value = match expr.kind {
                    ExprKind::Int { value } => value,
                    _ => unreachable!(),
                };
                if value > i64::MAX || value < i64::MIN {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            ty: Type::I64,
                            value,
                            span: expr.span,
                        }
                        .report(&self),
                    );
                }

                expr.ty = Type::I64;
                Ok(())
            }
            Type::U32 => {
                let value = match expr.kind {
                    ExprKind::Int { value } => value,
                    _ => unreachable!(),
                };

                if value > u32::MAX as i64 || value < 0 {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            ty: Type::U32,
                            value,
                            span: expr.span,
                        }
                        .report(&self),
                    );
                }

                expr.ty = Type::U32;
                Ok(())
            }
            Type::U64 => {
                let value = match expr.kind {
                    ExprKind::Int { value } => value,
                    _ => unreachable!(),
                };

                if value > u64::MAX as i64 || value < 0 {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            ty: Type::U64,
                            value,
                            span: expr.span,
                        }
                        .report(&self),
                    );
                }

                expr.ty = Type::U64;
                Ok(())
            }
            Type::F32 | Type::F64 => {
                self.diagnostics.push(
                    IntegerLiteralForFloatTypeDiagnostic {
                        file_id: self.ast.file_id,
                        span: expr.span,
                    }
                    .report(),
                );

                Err(())
            }
            target_type => {
                self.diagnostics.push(
                    UnableToCoerceDiagnostic {
                        file_id: self.ast.file_id,
                        target_type,
                        span: expr.span,
                    }
                    .report(&self),
                );

                Err(())
            }
        }
    }

    fn coerce_untyped_float_expr(
        &mut self,
        expr: &mut Expression,
        target_type: Type,
    ) -> Result<(), ()> {
        match target_type {
            Type::F32 => {
                // TODO: add a diagnostic if the literal is out of range
                expr.ty = Type::F32;
                Ok(())
            }
            Type::F64 => {
                // TODO: add a diagnostic if the literal is out of range
                expr.ty = Type::F64;
                Ok(())
            }
            target_type => {
                self.diagnostics.push(
                    UnableToCoerceDiagnostic {
                        file_id: self.ast.file_id,
                        target_type,
                        span: expr.span,
                    }
                    .report(&self),
                );

                Err(())
            }
        }
    }

    fn coerce_untyped_unary_expr(
        &mut self,
        expr: &mut Expression,
        target_type: Type,
    ) -> Result<(), ()> {
        let (operand, operator) = match &mut expr.kind {
            ExprKind::Unary { operand, operator } => (operand, operator.inner),
            _ => unreachable!(),
        };

        match operator {
            ast::UnaryOp::BitNot | ast::UnaryOp::InvertSign => match target_type {
                Type::I32 | Type::I64 => {}
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        self.coerce_untyped_expr(operand, target_type)
            .and_then(|_| Ok(expr.ty = target_type))
    }

    fn coerce_untyped_binary_expression(
        &mut self,
        expr: &mut Expression,
        target_type: Type,
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
            operator if operator.is_arithmetic() => match target_type {
                target_type if target_type.is_primitive() => {}
                target_type => {
                    self.diagnostics.push(
                        UnableToCoerceDiagnostic {
                            file_id: self.ast.file_id,
                            target_type,
                            span: expr.span,
                        }
                        .report(&self),
                    );
                    return Err(());
                }
            },
            operator if operator.is_bitwise() => match target_type {
                Type::I32 | Type::I64 | Type::U32 | Type::U64 => {}
                Type::F32 | Type::F64 => {
                    self.diagnostics.push(
                        UnableToCoerceDiagnostic {
                            file_id: self.ast.file_id,
                            target_type,
                            span: expr.span,
                        }
                        .report(&self),
                    );
                    return Err(());
                }
                target_type => {
                    self.diagnostics.push(
                        UnableToCoerceDiagnostic {
                            file_id: self.ast.file_id,
                            target_type,
                            span: expr.span,
                        }
                        .report(&self),
                    );
                    return Err(());
                }
            },
            _ => unreachable!(),
        };

        match (
            self.coerce_untyped_expr(left, target_type),
            self.coerce_untyped_expr(right, target_type),
        ) {
            (Ok(_), Ok(_)) => {
                expr.ty = target_type;
                Ok(())
            }
            _ => Err(()),
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TIR {
    pub file_id: ast::FileId,
    pub signatures: Vec<FunctionSignature>,
    pub defined_functions: HashMap<FunctionIndex, Function>,
    pub defined_globals: HashMap<GlobalIndex, Global>,
    pub namespaces: Vec<Namespace>,
    pub exports: Vec<ExportItem>,
    pub declared_functions: Vec<DeclaredFunction>,
    pub declared_globals: Vec<DeclaredGlobal>,
    pub diagnostics: Vec<Diagnostic<FileId>>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Namespace {
    Enum(Enum),
    ImportModule(ImportModule),
}

impl TIR {
    pub fn build(ast: &ast::AST, interner: &mut StringInterner) -> TIR {
        let mut symbol_lookup = HashMap::new();
        symbol_lookup.insert(
            (SymbolNamespace::Value, interner.get_or_intern("_")),
            GlobalValue::Placeholder,
        );
        symbol_lookup.insert(
            (SymbolNamespace::Value, interner.get_or_intern("true")),
            GlobalValue::True,
        );
        symbol_lookup.insert(
            (SymbolNamespace::Value, interner.get_or_intern("false")),
            GlobalValue::False,
        );
        symbol_lookup.insert(
            (
                SymbolNamespace::Value,
                interner.get_or_intern("unreachable"),
            ),
            GlobalValue::Unreachable,
        );

        let mut builder = Builder {
            ast,
            interner,
            defined_globals: HashMap::new(),
            exports: HashMap::new(),
            defined_functions: HashMap::new(),
            diagnostics: Vec::new(),
            declared_functions: Vec::new(),
            declared_globals: Vec::new(),
            namespaces: Vec::new(),
            signature_index_lookup: HashMap::new(),
            signatures: Vec::new(),
            symbol_lookup,
        };

        for item in ast.items.iter() {
            match builder.define_item(&item.inner) {
                Ok(_) => {}
                Err(_) => continue,
            }
        }

        for item in ast.items.iter() {
            match builder.build_item(&item.inner) {
                Ok(()) => {}
                Err(_) => continue,
            }
        }

        builder.report_unused_items();

        TIR {
            file_id: ast.file_id,
            defined_functions: builder.defined_functions,
            defined_globals: builder.defined_globals,
            namespaces: builder.namespaces,
            exports: builder.exports.into_values().collect(),
            signatures: builder.signatures,
            declared_functions: builder.declared_functions,
            declared_globals: builder.declared_globals,
            diagnostics: builder.diagnostics,
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::ast::{AST, Files};

    #[allow(unused)]
    struct TestCase {
        interner: StringInterner,
        files: Files,
        ast: AST,
        tir: TIR,
    }

    impl<'case> TestCase {
        fn new(source: &str) -> Self {
            let mut interner = StringInterner::new();
            let mut files = Files::new();
            let file_id = files
                .add("main.wx".to_string(), source.to_string())
                .unwrap();
            let ast =
                ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

            let tir = TIR::build(&ast, &mut interner);

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
            import \"console\" {
                fn log(ptr: i32, len: i32) -> unit;
            }

            fn main() -> unit {
                local x: i32 = 5;
                console::log(x, 4);
            }

            export { main }
        "});
        insta::assert_yaml_snapshot!(case.tir);
    }
}
