use std::collections::HashMap;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use string_interner::symbol::SymbolU32;

use crate::ast::{self, Annotated, FileId, Grouped, Separated, Spanned, StringInterner, TextSpan};

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
    Bool,
    Function { signature_index: u32 },
    Enum { enum_index: u32 },
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
            _ => Err(()),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSignature {
    items: Box<[Type]>,
    params_count: u32,
}

impl FunctionSignature {
    pub fn params(&self) -> &[Type] {
        &self.items[..self.params_count as usize]
    }

    pub fn result(&self) -> Type {
        self.items[self.params_count as usize]
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

type LocalIndex = u32;
type ScopeIndex = u32;
type FunctionIndex = u32;
type EnumIndex = u32;
type GlobalIndex = u32;
type SignatureIndex = u32;
type EnumVariantIndex = u32;

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
        enum_index: EnumIndex,
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
pub struct VariableAccess {
    pub span: ast::TextSpan,
    pub kind: AccessKind,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Local {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: Type,
    pub mut_span: Option<ast::TextSpan>,
    pub accesses: Vec<VariableAccess>,
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
    pub stack: StackFrame,
    pub block: Box<Expression>,
    pub accesses: Vec<ast::TextSpan>,
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
#[derive(Clone, Copy)]
pub enum ExportItem {
    Function { func_index: u32 },
    Global { global_index: u32 },
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
pub struct Global {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<Type>,
    pub mut_span: Option<ast::TextSpan>,
    pub value: Box<Expression>,
    pub accesses: Vec<VariableAccess>,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GlobalValue {
    Global {
        global_index: GlobalIndex,
    },
    Function {
        func_index: FunctionIndex,
    },
    Enum {
        enum_index: EnumIndex,
    },
    EnumVariant {
        enum_index: EnumIndex,
        variant_index: EnumVariantIndex,
    },
    True,
    False,
    Unreachable,
    Placeholder,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolNamespace {
    Type,
    Value,
}

#[derive(Clone)]
pub struct FunctionMeta {
    signature_index: SignatureIndex,
    name: ast::Spanned<SymbolU32>,
}

pub struct GlobalContext<'interner> {
    pub interner: &'interner ast::StringInterner,
    pub exports: Vec<ExportItem>,
    pub globals: Vec<Global>,
    pub functions: Vec<FunctionMeta>,
    pub function_types: Vec<FunctionSignature>,
    pub function_lookup: HashMap<FunctionSignature, SignatureIndex>,
    pub enums: Vec<Enum>,
    pub symbol_lookup: HashMap<(SymbolNamespace, SymbolU32), GlobalValue>,
}

impl<'interner> GlobalContext<'interner> {
    pub fn new(interner: &'interner mut ast::StringInterner) -> Self {
        let mut lookup = HashMap::new();
        lookup.insert(
            (SymbolNamespace::Value, interner.get_or_intern("_")),
            GlobalValue::Placeholder,
        );
        lookup.insert(
            (SymbolNamespace::Value, interner.get_or_intern("true")),
            GlobalValue::True,
        );
        lookup.insert(
            (SymbolNamespace::Value, interner.get_or_intern("false")),
            GlobalValue::False,
        );
        lookup.insert(
            (
                SymbolNamespace::Value,
                interner.get_or_intern("unreachable"),
            ),
            GlobalValue::Unreachable,
        );

        GlobalContext {
            exports: Vec::new(),
            function_types: Vec::new(),
            enums: Vec::new(),
            interner,
            globals: Vec::new(),
            functions: Vec::new(),
            function_lookup: HashMap::new(),
            symbol_lookup: lookup,
        }
    }

    pub fn resolve_type(&mut self, type_expr: &ast::TypeExpression) -> Result<Type, ()> {
        match &type_expr {
            ast::TypeExpression::Error => Err(()),
            ast::TypeExpression::Identifier { symbol } => {
                let symbol = *symbol;
                let text = self.interner.resolve(symbol).unwrap();
                match Type::try_from(text) {
                    Ok(ty) => return Ok(ty),
                    Err(_) => {}
                }
                match self.symbol_lookup.get(&(SymbolNamespace::Type, symbol)) {
                    Some(GlobalValue::Enum { enum_index }) => Ok(Type::Enum {
                        enum_index: *enum_index,
                    }),
                    Some(_) => Err(()),
                    None => Err(()),
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
                    params_count: params.inner.len() as u32,
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
            params_count: params_count as u32,
        }
    }

    pub fn ensure_signature_index(&mut self, func_type: &FunctionSignature) -> SignatureIndex {
        match self.function_lookup.get(func_type).cloned() {
            Some(type_index) => type_index,
            None => {
                let signature_index = self.function_types.len() as u32;
                self.function_types.push(func_type.clone());
                self.function_lookup
                    .insert(func_type.clone(), signature_index);
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
            Type::Enum { enum_index } => self
                .interner
                .resolve(self.enums[enum_index as usize].name.inner)
                .unwrap()
                .to_string(),
            Type::Function { signature_index } => {
                let func = &self.function_types[signature_index as usize];
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

    pub fn enter_block<T>(
        &mut self,
        block: BlockScope,
        handler: fn(&mut FunctionContext) -> T,
    ) -> T {
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

pub enum DiagnosticCode {
    DuplicateDefinition,
    TypeMistmatch,
    TypeAnnotationRequired,
    UnusedVariable,
    UnnecessaryMutability,
    UnreachableCode,
    UnusedValue,
    IntegerLiteralOutOfRange,
    UnableToCoerce,
}

impl DiagnosticCode {
    const fn code(self) -> &'static str {
        match self {
            DiagnosticCode::DuplicateDefinition => "E1000",
            DiagnosticCode::TypeMistmatch => "E1001",
            DiagnosticCode::TypeAnnotationRequired => "E1002",
            DiagnosticCode::UnusedVariable => "W1000",
            DiagnosticCode::UnnecessaryMutability => "W1001",
            DiagnosticCode::UnreachableCode => "W1002", // Is this a warning or an error?
            DiagnosticCode::UnusedValue => "E1003",
            DiagnosticCode::IntegerLiteralOutOfRange => "E1004",
            DiagnosticCode::UnableToCoerce => "E1005",
        }
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
        Diagnostic::error()
            .with_code(DiagnosticCode::DuplicateDefinition.code())
            .with_label(
                Label::primary(self.file_id, self.second_definition).with_message(format!(
                    "the name `{}` is defined multiple times",
                    self.name
                )),
            )
            .with_label(
                Label::secondary(self.file_id, self.first_definition).with_message(format!(
                    "previous definition of the type `{}` here",
                    self.name
                )),
            )
            .with_note(match self.namespace {
                SymbolNamespace::Type => format!(
                    "`{}` must be defined only once in the type namespace of this module",
                    self.name
                ),
                SymbolNamespace::Value => format!(
                    "`{}` must be defined only once in the value namespace of this module",
                    self.name
                ),
            })
    }
}

struct TypeMistmatchDiagnostic {
    file_id: FileId,
    expected_type: Type,
    actual_type: Type,
    span: TextSpan,
}

impl TypeMistmatchDiagnostic {
    fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::TypeMistmatch.code())
            .with_message("type mismatch")
            .with_label(
                Label::primary(self.file_id, self.span).with_message(format!(
                    "expected `{}`, found `{}`",
                    global.display_type(self.expected_type),
                    global.display_type(self.actual_type)
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
    fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::IntegerLiteralOutOfRange.code())
            .with_message(format!(
                "literal `{}` out of range for `{}`",
                self.value,
                global.display_type(self.ty)
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
    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UnableToCoerce.code())
            .with_message(format!(
                "unable to coerce to type `{}`",
                global.display_type(self.target_type)
            ))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct TIRBuiler<'ast, 'interner> {
    ast: &'ast ast::Ast,
    global: GlobalContext<'interner>,
    diagnostics: Vec<Diagnostic<FileId>>,
}

enum BlockState<T> {
    Exhaustive(T),
    Incomplete(T),
}

impl TIRBuiler<'_, '_> {
    fn define_item(&mut self, item: &ast::Item) -> Result<GlobalValue, ()> {
        match item {
            ast::Item::FunctionDefinition { signature, .. } => {
                match self
                    .global
                    .symbol_lookup
                    .get(&(SymbolNamespace::Value, signature.name.inner))
                    .cloned()
                {
                    Some(first_definition) => {
                        let name = self.global.interner.resolve(signature.name.inner).unwrap();
                        let first_definition_span = match first_definition {
                            GlobalValue::Function { func_index } => {
                                let func = &self.global.functions[func_index as usize];
                                func.name.span
                            }
                            GlobalValue::Global { global_index } => {
                                let global = &self.global.globals[global_index as usize];
                                global.name.span
                            }
                            _ => unimplemented!(),
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

                        // TODO: we should probably continue processing the function to find more errors
                        return Err(());
                    }
                    None => {}
                };

                let name_symbol = signature.name.inner;
                let ty = self
                    .global
                    .build_signature(&signature.params, &signature.result);
                let type_index = self.global.ensure_signature_index(&ty);

                let func_index = self.global.functions.len() as u32;
                self.global.symbol_lookup.insert(
                    (SymbolNamespace::Value, name_symbol),
                    GlobalValue::Function { func_index },
                );
                self.global.functions.push(FunctionMeta {
                    signature_index: type_index,
                    name: signature.name.clone(),
                });
                Ok(GlobalValue::Function { func_index })
            }
            ast::Item::GlobalDefinition {
                mut_span,
                name,
                type_annotation,
                value,
            } => unimplemented!(),
            ast::Item::ExportModifier { alias, item } => unimplemented!(),
        }
    }

    fn build_item(&mut self, item: &ast::Item) -> Result<(), ()> {
        match item {
            ast::Item::FunctionDefinition { signature, block } => {
                let func = self.build_function_definition(item)?;
                // self.functions.push(func);

                Ok(())
            }
            ast::Item::GlobalDefinition {
                mut_span,
                name,
                type_annotation,
                value,
            } => unimplemented!(),
            ast::Item::ExportModifier { alias, item } => unimplemented!(),
        }
    }

    fn build_function_definition(&mut self, item: &ast::Item) -> Result<Function, ()> {
        let (signature, block) = match &item {
            ast::Item::FunctionDefinition { signature, block } => (signature, block),
            _ => unreachable!(),
        };

        let params = signature
            .params
            .inner
            .iter()
            .map(|param| FunctionParam {
                name: param.inner.inner.name.clone(),
                ty: Spanned {
                    inner: self
                        .global
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

        let func_index = self.global.resolve_func(signature.name.inner).unwrap();
        let func_meta = self
            .global
            .functions
            .get(func_index as usize)
            .cloned()
            .unwrap();
        let func_type = self
            .global
            .function_types
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
            expected_type: Some(func_type.result()),
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

        Ok(Function {
            params,
            result: Spanned {
                inner: func_type.result(),
                span: signature.result.inner.span,
            },
            name: signature.name.clone(),
            stack: ctx.frame,
            accesses: Vec::new(),
            block: Box::new(block),
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
                            .report(&self.global),
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
            if local.accesses.is_empty() {
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
                            span: TextSpan::merge(mut_span, local.name.span),
                        }
                        .report(),
                    );
                }
                _ => {}
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
                Type::Unit => expressions.push(expr),
                Type::Never => {
                    expressions.push(expr);
                    return BlockState::Exhaustive(expressions.into_boxed_slice());
                }
                _ => unreachable!(),
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
                        .report(&self.global),
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
                            .report(&self.global),
                        );
                        Err(())
                    }
                    _ => Ok(result_type),
                },
            },
        }
    }

    fn build_statement(
        &self,
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

    fn build_local_definition_statement(
        &mut self,
        ctx: &mut FunctionContext,
        stmt: &ast::Separated<ast::Statement>,
    ) -> Result<Expression, ()> {
        let (mut_span, name, annotation, value) = match &stmt.inner {
            ast::Statement::LocalDefinition {
                mut_span,
                name,
                type_annotation,
                value,
                ..
            } => (
                mut_span.clone(),
                name.clone(),
                type_annotation.clone(),
                value,
            ),
            _ => unreachable!(),
        };

        let expected_type = match annotation {
            Some(annotation) => self.global.resolve_type(&annotation.inner.inner).ok(),
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
                        .report(&self.global),
                    );
                    expected_type // Recover by using the expected type
                }
            }
            (Type::Unknown, None) => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: name.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        let local_index = ctx.push_local(Local {
            name: name.clone(),
            ty,
            mut_span,
            accesses: Vec::new(),
        });

        let span = TextSpan::new(stmt.span.start().0, value.span.end().0);
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
            span,
        })
    }

    fn build_expression(
        &self,
        ctx: &mut FunctionContext,
        expression: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        todo!()
    }

    fn coerce_untyped_expr(
        &self,
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
            _ => unreachable!(),
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
                        .report(&self.global),
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
                        .report(&self.global),
                    );
                }

                expr.ty = Type::I64;
                Ok(())
            }
            Type::F32 => {
                let value = match expr.kind {
                    ExprKind::Int { value } => value,
                    _ => unreachable!(),
                };
                if (value as f32) > f32::MAX || (value as f32) < f32::MIN {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            ty: Type::F32,
                            value,
                            span: expr.span,
                        }
                        .report(&self.global),
                    );
                }

                expr.ty = Type::F32;
                Ok(())
            }
            Type::F64 => {
                let value = match expr.kind {
                    ExprKind::Int { value } => value,
                    _ => unreachable!(),
                };
                if (value as f64) > f64::MAX || (value as f64) < f64::MIN {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            ty: Type::F64,
                            value,
                            span: expr.span,
                        }
                        .report(&self.global),
                    );
                }

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
                    .report(&self.global),
                );

                Err(())
            }
        }
    }
}

pub struct TIR {
    pub file_id: ast::FileId,
    pub functions: Vec<FunctionMeta>,
    pub enums: Vec<Enum>,
    pub globals: Vec<Global>,
    pub exports: Vec<ExportItem>,
    pub diagnostics: Vec<Diagnostic<FileId>>,
}

impl TIR {
    pub fn build(ast: &ast::Ast, interner: &mut StringInterner) -> TIR {
        let mut builder = TIRBuiler {
            ast,
            global: GlobalContext::new(interner),
            diagnostics: Vec::new(),
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

        // TODO

        let TIRBuiler {
            global,
            diagnostics,
            ..
        } = builder;

        TIR {
            file_id: ast.file_id,
            functions: global.functions,
            enums: global.enums,
            globals: global.globals,
            exports: global.exports,
            diagnostics,
        }
    }
}
