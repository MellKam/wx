use std::collections::HashMap;
use std::hash::Hash;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use string_interner::symbol::SymbolU32;

use crate::ast::{self, FileId, Separated, Spanned, TextSpan};

/// Serializes a HashMap with keys sorted for deterministic snapshot output.
#[cfg(test)]
fn serialize_sorted_map<K, V, S>(map: &HashMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
where
    K: Ord + serde::Serialize,
    V: serde::Serialize,
    S: serde::Serializer,
{
    use serde::ser::SerializeMap;
    let mut pairs: Vec<_> = map.iter().collect();
    pairs.sort_by_key(|(k, _)| *k);
    let mut ser = serializer.serialize_map(Some(pairs.len()))?;
    for (k, v) in pairs {
        ser.serialize_entry(k, v)?;
    }
    ser.end()
}

/// Serializes a `HashMap<K, HashMap<K2, V>>` with both levels of keys sorted.
#[cfg(test)]
fn serialize_sorted_nested_map<K, K2, V, S>(
    map: &HashMap<K, HashMap<K2, V>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    K: Ord + serde::Serialize,
    K2: Ord + serde::Serialize,
    V: serde::Serialize,
    S: serde::Serializer,
{
    use serde::ser::SerializeMap;
    let mut outer: Vec<_> = map.iter().collect();
    outer.sort_by_key(|(k, _)| *k);
    let mut ser = serializer.serialize_map(Some(outer.len()))?;
    for (k, inner_map) in outer {
        let mut inner: Vec<_> = inner_map.iter().collect();
        inner.sort_by_key(|(k2, _)| *k2);
        ser.serialize_entry(k, &SortedMapRef(&inner))?;
    }
    ser.end()
}

#[cfg(test)]
struct SortedMapRef<'a, K, V>(&'a Vec<(&'a K, &'a V)>);

#[cfg(test)]
impl<K: serde::Serialize, V: serde::Serialize> serde::Serialize for SortedMapRef<'_, K, V> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let mut ser = serializer.serialize_map(Some(self.0.len()))?;
        for (k, v) in self.0 {
            ser.serialize_entry(k, v)?;
        }
        ser.end()
    }
}

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

/// Parses a char literal (e.g. `'a'`, `'\n'`) and returns the single character
/// it represents, or `None` if it contains more than one logical character.
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
            Some(c) => c,
        },
        Some(c) => c,
    };

    if chars.next().is_some() {
        return Err(CharLiteralError::TooLong);
    }

    Ok(value)
}

pub type TypeIndex = u32;

// Pre-interned indices — values must match the intern order in
// `TypePool::new()`.
pub const ERROR_IDX: TypeIndex = 0;
pub const UNIT_IDX: TypeIndex = 1;
pub const NEVER_IDX: TypeIndex = 2;
pub const UNKNOWN_IDX: TypeIndex = 3;
pub const I32_IDX: TypeIndex = 4;
pub const I64_IDX: TypeIndex = 5;
pub const F32_IDX: TypeIndex = 6;
pub const F64_IDX: TypeIndex = 7;
pub const U32_IDX: TypeIndex = 8;
pub const U64_IDX: TypeIndex = 9;
pub const BOOL_IDX: TypeIndex = 10;
/// TypeIndex for the built-in `char` primitive type.
pub const CHAR_IDX: TypeIndex = 11;
/// TypeIndex for the built-in `string` sealed struct.
pub const STRING_IDX: TypeIndex = 12;
pub const U8_IDX: TypeIndex = 13;
pub const I8_IDX: TypeIndex = 14;
pub const U16_IDX: TypeIndex = 15;
pub const I16_IDX: TypeIndex = 16;

/// Struct pool index of the built-in `string` sealed struct.
pub const BUILTIN_STRING_STRUCT: u32 = 0;

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Error,
    Unit,
    Never,
    Unknown,
    I32,
    I64,
    F32,
    F64,
    U32,
    U64,
    Bool,
    Char,
    U8,
    I8,
    U16,
    I16,
    Pointer {
        to: TypeIndex,
        mutable: bool,
    },
    Array {
        of: TypeIndex,
        size: u32,
        mutable: bool,
    },
    Slice {
        of: TypeIndex,
        mutable: bool,
    },
    Tuple {
        elements: Box<[TypeIndex]>,
    },
    Struct {
        struct_index: u32,
    },
    Function {
        items: Box<[TypeIndex]>,
        params_count: usize,
    },
    Namespace {
        namespace_index: u32,
    },
}

impl Type {
    pub fn is_primitive(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::F32 | Type::F64
            | Type::Char | Type::U8 | Type::I8 | Type::U16 | Type::I16 => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64
            | Type::U8 | Type::I8 | Type::U16 | Type::I16 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 => true,
            _ => false,
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, ()> {
        match value {
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "bool" => Ok(Type::Bool),
            "char" => Ok(Type::Char),
            "u8" => Ok(Type::U8),
            "i8" => Ok(Type::I8),
            "u16" => Ok(Type::U16),
            "i16" => Ok(Type::I16),
            "unit" => Ok(Type::Unit),
            "never" => Ok(Type::Never),
            "string" => Ok(Type::Struct {
                struct_index: BUILTIN_STRING_STRUCT,
            }),
            _ => Err(()),
        }
    }
}

/// Returns true if casting from `from` to `to` is allowed without an explicit
/// unsafe annotation. Permitted conversions:
/// - integer ↔ integer (any combination of i8/u8/i16/u16/i32/u32/i64/u64)
/// - char ↔ u8 or u16 (ASCII / Latin-1 / BMP range fits naturally)
pub fn is_numeric_cast(from: TypeIndex, to: TypeIndex) -> bool {
    use Type::*;
    let is_int = |idx: TypeIndex| matches!(idx, I8_IDX | U8_IDX | I16_IDX | U16_IDX | I32_IDX | U32_IDX | I64_IDX | U64_IDX);
    let is_char_compat = |idx: TypeIndex| matches!(idx, U8_IDX | U16_IDX);
    (is_int(from) && is_int(to))
        || (from == CHAR_IDX && is_char_compat(to))
        || (is_char_compat(from) && to == CHAR_IDX)
}

pub type LocalIndex = u32;
pub type ScopeIndex = u32;
pub type FunctionIndex = u32;
pub type NamespaceIndex = u32;
pub type GlobalIndex = u32;
pub type ConstIndex = u32;
/// `SignatureIndex` is now an alias for `TypeIndex` — the function type lives
/// in the type pool.
pub type SignatureIndex = TypeIndex;
pub type EnumVariantIndex = u32;

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum ConstValue {
    Int(i64),
    Float(f64),
}

pub struct DeclaredConst {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<TypeIndex>,
}

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
    MethodCall {
        object: Box<Expression>,
        arguments: Box<[Expression]>,
        func_index: FunctionIndex,
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
        ty: ast::Spanned<TypeIndex>,
        member: ast::Spanned<SymbolU32>,
    },
    String {
        symbol: SymbolU32,
    },
    Char {
        value: char,
    },
    ObjectAccess {
        object: Box<Expression>,
        member: ast::Spanned<SymbolU32>,
    },
    StructInit {
        struct_index: u32,
        fields: Box<[Expression]>,
    },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: TypeIndex,
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
    expected_type: Option<TypeIndex>,
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
    pub ty: TypeIndex,
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
    pub inferred_type: Option<TypeIndex>,
    pub expected_type: Option<TypeIndex>,
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
pub struct DeclaredFunctionParam {
    pub name: Option<ast::Spanned<SymbolU32>>,
    pub ty: ast::Spanned<TypeIndex>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionParam {
    pub mut_span: Option<ast::TextSpan>,
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<TypeIndex>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
    pub name: ast::Spanned<SymbolU32>,
    pub params: Box<[FunctionParam]>,
    pub result_span: Option<ast::TextSpan>,
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
        func_index: FunctionIndex,
    },
    Global {
        internal_name: Spanned<SymbolU32>,
        external_name: Option<Spanned<SymbolU32>>,
        global_index: GlobalIndex,
    },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Enum {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: TypeIndex,
    pub variants: Box<[EnumVariant]>,
    #[cfg_attr(test, serde(serialize_with = "serialize_sorted_map"))]
    pub lookup: HashMap<SymbolU32, EnumVariantIndex>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct EnumVariant {
    pub name: ast::Spanned<SymbolU32>,
    pub value: Box<Expression>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct DeclaredGlobal {
    pub source: ItemSource,
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<TypeIndex>,
    pub mut_span: Option<ast::TextSpan>,
    pub accesses: Vec<ast::TextSpan>,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GlobalValue {
    Global { global_index: GlobalIndex },
    Function { func_index: FunctionIndex },
    Namespace { namespace_index: NamespaceIndex },
    Struct { struct_index: u32 },
    Const { const_index: ConstIndex },
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

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportModule {
    pub external_name: ast::Spanned<SymbolU32>,
    pub internal_name: Option<ast::Spanned<SymbolU32>>,
    #[cfg_attr(test, serde(serialize_with = "serialize_sorted_map"))]
    pub lookup: HashMap<SymbolU32, ImportValue>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolNamespace {
    Type,
    Value,
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub enum ItemSource {
    Defined,
    Imported,
}

/// Classifies a source file for diagnostic purposes.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FileKind {
    /// A library file (e.g. stdlib). Unused-item warnings are suppressed.
    Library,
    /// A user module. Unused-item warnings are emitted normally.
    Module,
}

#[derive(Clone, Copy)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImplEntry {
    Method(FunctionIndex),
    AssociatedFn(FunctionIndex),
    AssociatedConst { ty: TypeIndex, value: i64 },
}

impl PartialOrd for ItemSource {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ItemSource {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (ItemSource::Imported, ItemSource::Defined) => std::cmp::Ordering::Less,
            (ItemSource::Defined, ItemSource::Imported) => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        }
    }
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub struct DeclaredFunction {
    pub source: ItemSource,
    pub signature_index: SignatureIndex,
    pub name: ast::Spanned<SymbolU32>,
    pub params: Box<[DeclaredFunctionParam]>,
    pub result: Option<Spanned<TypeIndex>>,
    pub accesses: Vec<ast::TextSpan>,
    /// Set when the function was declared `pub fn`; suppresses the unused-item warning.
    pub pub_span: Option<ast::TextSpan>,
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
        InvalidLiteral => "E1017",
        DuplicateExport => "E1018",
        CannotExportItem => "E1019",
        NotANamespace => "E1020",
        UndeclaredType => "E1021",
        DuplicateStructField => "E1022",
        RecursiveStructType => "E1023",
        UnknownStructField => "E1025",
        DuplicateStructFieldInit => "E1026",
        MissingStructFields => "E1027",
        CannotMutateImmutable => "W1000",
        UnusedVariable => "W1001",
        UnnecessaryMutability => "W1002",
        UnreachableCode => "W1003",
        UnusedItem => "W1004",
        MissingImportParamName => "W1005",
    }
}

struct DuplicateDefinitionDiagnostic<'interner> {
    file_id: FileId,
    name: &'interner str,
    namespace: SymbolNamespace,
    first_definition: ast::TextSpan,
    second_definition: ast::TextSpan,
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
    }
}

struct DuplicateParameterDiagnostic<'interner> {
    file_id: FileId,
    name: &'interner str,
    first_definition: ast::TextSpan,
    second_definition: ast::TextSpan,
}

impl DuplicateParameterDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::DuplicateDefinition.code())
            .with_message(format!(
                "identifier `{}` is bound more than once in this parameter list",
                self.name
            ))
            .with_label(Label::primary(self.file_id, self.second_definition))
            .with_label(
                Label::secondary(self.file_id, self.first_definition)
                    .with_message(format!("first use of `{}` as parameter", self.name)),
            )
    }
}

struct MissingImportParamNameDiagnostic {
    file_id: FileId,
    span: ast::TextSpan,
}

impl MissingImportParamNameDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(DiagnosticCode::MissingImportParamName.code())
            .with_message("imported function parameter has no name")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("give this parameter a name: `name: type`"),
            )
    }
}

struct NonConstantGlobalInitializerDiagnostic {
    file_id: FileId,
    span: ast::TextSpan,
}

impl NonConstantGlobalInitializerDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::NonConstantGlobalInitializer.code())
            .with_message("global variable initializers can only contain constant expressions")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct EmptyCharLiteralDiagnostic {
    file_id: FileId,
    span: ast::TextSpan,
}

impl EmptyCharLiteralDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::InvalidLiteral.code())
            .with_message("empty character literal")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct CharLiteralTooLongDiagnostic {
    file_id: FileId,
    span: ast::TextSpan,
}

impl CharLiteralTooLongDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::InvalidLiteral.code())
            .with_message("character literal may only contain one codepoint")
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("if you meant to write a string literal, use double quotes: `\"`, `\"`")
    }
}

struct TypeMistmatchDiagnostic {
    file_id: FileId,
    expected_type: TypeIndex,
    actual_type: TypeIndex,
    span: ast::TextSpan,
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
    span: ast::TextSpan,
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
    span: ast::TextSpan,
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
    span: ast::TextSpan,
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
    span: ast::TextSpan,
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
    span: ast::TextSpan,
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
    span: ast::TextSpan,
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
    span: ast::TextSpan,
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
    ty: TypeIndex,
    value: i64,
    span: ast::TextSpan,
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
    target_type: TypeIndex,
    span: ast::TextSpan,
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
    span: ast::TextSpan,
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
    span: ast::TextSpan,
}

impl UndeclaredIdentifierDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UndeclaredIdentifier.code())
            .with_message("undeclared identifier")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct UndeclaredTypeDiagnostic {
    file_id: FileId,
    span: ast::TextSpan,
}

impl UndeclaredTypeDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UndeclaredType.code())
            .with_message("undeclared type")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct BinaryOperatorCannotBeAppliedDiagnostic {
    file_id: FileId,
    operator: Spanned<ast::BinaryOp>,
    operand: Spanned<TypeIndex>,
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
    left_type: Spanned<TypeIndex>,
    operator: Spanned<ast::BinaryOp>,
    right_type: Spanned<TypeIndex>,
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
    ty: TypeIndex,
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
    operand: Spanned<TypeIndex>,
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

struct NotANamespaceDiagnostic {
    file_id: FileId,
    span: TextSpan,
    ty: TypeIndex,
}

impl NotANamespaceDiagnostic {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::NotANamespace.code())
            .with_message(format!(
                "type `{}` is not a namespace",
                builder.display_type(self.ty)
            ))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct ArgumentCountMismatchDiagnostic<'a> {
    file_id: FileId,
    actual_count: usize,
    params: &'a [TypeIndex],
    call_span: TextSpan,
    is_method: bool,
}

impl ArgumentCountMismatchDiagnostic<'_> {
    fn report(self, builder: &Builder) -> Diagnostic<FileId> {
        let mut diagnostic = Diagnostic::error()
            .with_code(DiagnosticCode::ArgumentCountMismatch.code())
            .with_message(format!(
                "this {} takes {} {} but {} {} supplied",
                if self.is_method { "method" } else { "function" },
                self.params.len(),
                if self.params.len() == 1 {
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

        if self.actual_count < self.params.len() {
            // Missing arguments
            let missing_count = self.params.len() - self.actual_count;
            let missing_types: Vec<String> = self.params[self.actual_count..]
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
            let extra_count = self.actual_count - self.params.len();
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
    pub ty: ast::Spanned<TypeIndex>,
    pub mut_span: Option<ast::TextSpan>,
    pub value: Box<ast::Spanned<Expression>>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructField {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<TypeIndex>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Struct {
    pub name: ast::Spanned<SymbolU32>,
    pub fields: Box<[StructField]>,
    #[cfg_attr(test, serde(serialize_with = "serialize_sorted_map"))]
    pub lookup: HashMap<SymbolU32, usize>,
}

struct DuplicateStructFieldDiagnostic<'a> {
    file_id: FileId,
    name: &'a str,
    first_span: ast::TextSpan,
    second_span: ast::TextSpan,
}

impl DuplicateStructFieldDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::DuplicateStructField.code())
            .with_message(format!("field `{}` is already declared", self.name))
            .with_label(Label::primary(self.file_id, self.second_span))
            .with_label(
                Label::secondary(self.file_id, self.first_span)
                    .with_message(format!("`{}` first declared here", self.name)),
            )
    }
}

struct RecursiveStructTypeDiagnostic<'a> {
    file_id: FileId,
    struct_name: &'a str,
    field_span: ast::TextSpan,
}

impl RecursiveStructTypeDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::RecursiveStructType.code())
            .with_message(format!(
                "recursive type `{}` has infinite size",
                self.struct_name
            ))
            .with_label(
                Label::primary(self.file_id, self.field_span).with_message("recursive field here"),
            )
            .with_note("consider using a pointer type to break the cycle")
    }
}

struct NotAStructTypeDiagnostic {
    file_id: FileId,
    name: String,
    span: ast::TextSpan,
}

impl NotAStructTypeDiagnostic {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::TypeMistmatch.code())
            .with_message(format!("expected struct, found `{}`", self.name))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

struct UnknownStructFieldDiagnostic<'a> {
    file_id: FileId,
    struct_name: &'a str,
    field_name: &'a str,
    field_span: ast::TextSpan,
}

impl UnknownStructFieldDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UnknownStructField.code())
            .with_message(format!(
                "no such field `{}` in struct `{}`",
                self.field_name, self.struct_name
            ))
            .with_label(Label::primary(self.file_id, self.field_span))
    }
}

struct DuplicateStructFieldInitDiagnostic<'a> {
    file_id: FileId,
    field_name: &'a str,
    first_span: ast::TextSpan,
    second_span: ast::TextSpan,
}

impl DuplicateStructFieldInitDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::DuplicateStructFieldInit.code())
            .with_message(format!(
                "field `{}` specified more than once",
                self.field_name
            ))
            .with_label(Label::primary(self.file_id, self.second_span))
            .with_label(
                Label::secondary(self.file_id, self.first_span)
                    .with_message("first use of this field"),
            )
    }
}

struct MissingStructFieldsDiagnostic<'a> {
    file_id: FileId,
    struct_name: &'a str,
    missing_fields: Box<[&'a str]>,
    init_span: ast::TextSpan,
}

impl MissingStructFieldsDiagnostic<'_> {
    fn report(self) -> Diagnostic<FileId> {
        let fields_str = self
            .missing_fields
            .iter()
            .map(|f| format!("`{}`", f))
            .collect::<Vec<_>>()
            .join(", ");
        Diagnostic::error()
            .with_code(DiagnosticCode::MissingStructFields.code())
            .with_message(format!(
                "missing fields {} in initializer of `{}`",
                fields_str, self.struct_name
            ))
            .with_label(Label::primary(self.file_id, self.init_span))
    }
}

struct Builder<'interner> {
    file_id: ast::FileId,
    interner: &'interner mut ast::StringInterner,

    defined_functions: HashMap<FunctionIndex, Function>,
    defined_globals: HashMap<GlobalIndex, Global>,
    exports: HashMap<SymbolU32, ExportItem>,
    diagnostics: Vec<Diagnostic<FileId>>,
    namespaces: Vec<Namespace>,
    declared_globals: Vec<DeclaredGlobal>,
    declared_functions: Vec<DeclaredFunction>,
    declared_consts: Vec<DeclaredConst>,
    const_pool: Vec<ConstValue>,
    type_pool: TypePool,
    symbol_lookup: HashMap<(SymbolNamespace, SymbolU32), GlobalValue>,
    impl_members: HashMap<TypeIndex, HashMap<SymbolU32, ImplEntry>>,
    structs: Vec<Struct>,
    ptr_size: PointerSize,
}

enum BlockState<T> {
    Exhaustive(T),
    Incomplete(T),
}

impl Builder<'_> {
    pub fn resolve_type(&mut self, type_expr: &Spanned<ast::TypeExpression>) -> TypeIndex {
        match &type_expr.inner {
            ast::TypeExpression::Identifier { symbol } => {
                let symbol = *symbol;
                let text = self.interner.resolve(symbol).unwrap();
                if let Ok(ty) = Type::try_from(text) {
                    return self.type_pool.intern(ty);
                }
                match self.symbol_lookup.get(&(SymbolNamespace::Type, symbol)) {
                    Some(GlobalValue::Namespace { namespace_index }) => {
                        self.type_pool.intern(Type::Namespace {
                            namespace_index: *namespace_index,
                        })
                    }
                    Some(GlobalValue::Struct { struct_index }) => {
                        self.type_pool.intern(Type::Struct {
                            struct_index: *struct_index,
                        })
                    }
                    _ => {
                        self.diagnostics.push(
                            UndeclaredTypeDiagnostic {
                                file_id: self.file_id,
                                span: type_expr.span,
                            }
                            .report(),
                        );
                        ERROR_IDX
                    }
                }
            }
            ast::TypeExpression::Function { params, result } => {
                let result_idx = match result {
                    Some(result) => self.resolve_type(result),
                    None => UNIT_IDX,
                };
                let params_count = params.inner.len();
                let items: Box<[TypeIndex]> = params
                    .inner
                    .iter()
                    .map(|ty| self.resolve_type(&ty.inner.inner.ty))
                    .chain(Some(result_idx))
                    .collect();
                self.type_pool.intern(Type::Function {
                    items,
                    params_count,
                })
            }
            ast::TypeExpression::Pointer { mutability, inner } => {
                let to = self.resolve_type(inner);
                self.type_pool.intern(Type::Pointer {
                    to,
                    mutable: mutability.is_some(),
                })
            }
            ast::TypeExpression::Slice { mutability, inner } => {
                let of = self.resolve_type(inner);
                self.type_pool.intern(Type::Slice {
                    of,
                    mutable: mutability.is_some(),
                })
            }
            ast::TypeExpression::Array {
                size,
                mutability,
                inner,
            } => {
                let of = self.resolve_type(inner);
                self.type_pool.intern(Type::Array {
                    of,
                    size: size.inner as u32,
                    mutable: mutability.is_some(),
                })
            }
            ast::TypeExpression::Tuple { elements } => {
                let elems: Box<[TypeIndex]> =
                    elements.iter().map(|e| self.resolve_type(e)).collect();
                self.type_pool.intern(Type::Tuple { elements: elems })
            }
        }
    }

    fn declare_function(
        &mut self,
        name: ast::Spanned<SymbolU32>,
        params: Box<[DeclaredFunctionParam]>,
        result: Option<Spanned<TypeIndex>>,
        source: ItemSource,
    ) -> FunctionIndex {
        let result_idx = match &result {
            Some(r) => r.inner,
            None => UNIT_IDX,
        };
        let params_count = params.len();
        let items: Box<[TypeIndex]> = params
            .iter()
            .map(|p| p.ty.inner)
            .chain(Some(result_idx))
            .collect();
        let signature_index = self.type_pool.intern(Type::Function {
            items,
            params_count,
        });

        let func_index = self.declared_functions.len() as u32;
        self.declared_functions.push(DeclaredFunction {
            source,
            signature_index,
            name,
            accesses: Vec::new(),
            pub_span: None,
            params,
            result,
        });
        func_index
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

    pub fn display_type(&self, idx: TypeIndex) -> String {
        match self.type_pool.get(idx) {
            Type::Unknown => "unknown".to_string(),
            Type::Error => "error".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::U8 => "u8".to_string(),
            Type::I8 => "i8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::I16 => "i16".to_string(),
            Type::Never => "never".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Pointer { to, mutable } => {
                let (to, mutable) = (*to, *mutable);
                let mutability = if mutable { "mut " } else { "" };
                format!("*{}{}", mutability, self.display_type(to))
            }
            Type::Slice { of, mutable } => {
                let (of, mutable) = (*of, *mutable);
                let mutability = if mutable { "mut " } else { "" };
                format!("[]{}{}", mutability, self.display_type(of))
            }
            Type::Array { of, size, mutable } => {
                let (of, size, mutable) = (*of, *size, *mutable);
                let mutability = if mutable { "mut " } else { "" };
                format!("[{}]{}{}", size, mutability, self.display_type(of))
            }
            Type::Tuple { elements } => {
                let elems = elements
                    .iter()
                    .map(|&e| self.display_type(e))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", elems)
            }
            Type::Struct { struct_index } => {
                let s = &self.structs[*struct_index as usize];
                self.interner.resolve(s.name.inner).unwrap().to_string()
            }
            Type::Namespace { namespace_index } => {
                let ns = &self.namespaces[*namespace_index as usize];
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
                        self.interner.resolve(enum_.name.inner).unwrap().to_string()
                    }
                }
            }
            Type::Function {
                items,
                params_count,
            } => {
                let params_count = *params_count;
                let params = items[..params_count]
                    .iter()
                    .map(|&p| self.display_type(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                let result = self.display_type(items[params_count]);
                format!("fn({}) -> {}", params, result)
            }
        }
    }

    fn define_item(&mut self, item: &ast::Item) -> Result<(), ()> {
        match item {
            ast::Item::Function { signature, .. } => {
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
                                file_id: self.file_id,
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

                let mut seen_params: HashMap<SymbolU32, ast::TextSpan> = HashMap::new();
                let params: Box<[DeclaredFunctionParam]> = signature
                    .params
                    .inner
                    .iter()
                    .map(|param| {
                        let name = &param.inner.inner.name;
                        if let Some(&first_span) = seen_params.get(&name.inner) {
                            let name_str = self.interner.resolve(name.inner).unwrap();
                            self.diagnostics.push(
                                DuplicateParameterDiagnostic {
                                    file_id: self.file_id,
                                    name: name_str,
                                    first_definition: first_span,
                                    second_definition: name.span,
                                }
                                .report(),
                            );
                        } else {
                            seen_params.insert(name.inner, name.span);
                        }
                        DeclaredFunctionParam {
                            name: Some(name.clone()),
                            ty: match &param.inner.inner.ty {
                                Some(ty) => Spanned {
                                    inner: self.resolve_type(&ty),
                                    span: ty.span,
                                },
                                None => Spanned {
                                    inner: ERROR_IDX,
                                    span: param.inner.inner.name.span,
                                },
                            },
                        }
                    })
                    .collect();
                let result = signature.result.as_ref().map(|result| Spanned {
                    inner: self.resolve_type(result),
                    span: result.span,
                });

                let func_index = self.declare_function(
                    signature.name.clone(),
                    params,
                    result,
                    ItemSource::Defined,
                );
                self.symbol_lookup.insert(
                    (SymbolNamespace::Value, signature.name.inner),
                    GlobalValue::Function { func_index },
                );
                Ok(())
            }
            ast::Item::Global {
                mut_span, name, ty, ..
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
                                file_id: self.file_id,
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

                let (ty, ty_span) = match ty {
                    Some(ty) => (self.resolve_type(&ty), ty.span),
                    None => {
                        self.diagnostics.push(
                            TypeAnnotationRequiredDiagnostic {
                                file_id: self.file_id,
                                span: name.span,
                            }
                            .report(),
                        );
                        // Use Error type but still register the global
                        (ERROR_IDX, name.span)
                    }
                };

                let global_index = self.declared_globals.len() as u32;
                self.symbol_lookup.insert(
                    (SymbolNamespace::Value, name.inner),
                    GlobalValue::Global { global_index },
                );

                self.declared_globals.push(DeclaredGlobal {
                    source: ItemSource::Defined,
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
                            file_id: self.file_id,
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
            ast::Item::Enum { .. } => {
                // TODO: lower enum items in TIR
                Ok(())
            }
            ast::Item::Const { name, ty, value } => {
                match self
                    .symbol_lookup
                    .get(&(SymbolNamespace::Value, name.inner))
                    .cloned()
                {
                    Some(first_definition) => {
                        let name_str = self.interner.resolve(name.inner).unwrap();
                        let first_definition_span = match first_definition {
                            GlobalValue::Function { func_index } => {
                                self.declared_functions[func_index as usize].name.span
                            }
                            GlobalValue::Global { global_index } => {
                                self.declared_globals[global_index as usize].name.span
                            }
                            _ => name.span,
                        };
                        self.diagnostics.push(
                            DuplicateDefinitionDiagnostic {
                                file_id: self.file_id,
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
                }

                let (ty_idx, ty_span) = match ty {
                    Some(ty) => (self.resolve_type(ty), ty.span),
                    None => (UNKNOWN_IDX, name.span),
                };

                let expected_ty = if ty_idx == UNKNOWN_IDX { ERROR_IDX } else { ty_idx };
                let value_expr = self.build_const_expression(value, expected_ty)?;

                let const_value = match &value_expr.kind {
                    ExprKind::Int { value } => ConstValue::Int(*value),
                    ExprKind::Float { value } => ConstValue::Float(*value),
                    _ => {
                        self.diagnostics.push(
                            NonConstantGlobalInitializerDiagnostic {
                                file_id: self.file_id,
                                span: value.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                };

                let resolved_ty = if ty_idx == UNKNOWN_IDX { value_expr.ty } else { ty_idx };
                let const_index = self.declared_consts.len() as ConstIndex;
                self.declared_consts.push(DeclaredConst {
                    name: name.clone(),
                    ty: ast::Spanned { inner: resolved_ty, span: ty_span },
                });
                self.const_pool.push(const_value);
                self.symbol_lookup.insert(
                    (SymbolNamespace::Value, name.inner),
                    GlobalValue::Const { const_index },
                );

                Ok(())
            }
            ast::Item::Struct { pub_span: _, name, fields } => {
                // Check for duplicate struct name in the type namespace
                if let Some(existing) = self
                    .symbol_lookup
                    .get(&(SymbolNamespace::Type, name.inner))
                    .cloned()
                {
                    let name_str = self.interner.resolve(name.inner).unwrap();
                    let first_span = match existing {
                        GlobalValue::Struct { struct_index } => {
                            self.structs[struct_index as usize].name.span
                        }
                        _ => name.span,
                    };
                    self.diagnostics.push(
                        DuplicateDefinitionDiagnostic {
                            file_id: self.file_id,
                            name: name_str,
                            namespace: SymbolNamespace::Type,
                            first_definition: first_span,
                            second_definition: name.span,
                        }
                        .report(),
                    );
                    return Err(());
                }

                let struct_index = self.structs.len() as u32;
                let struct_name = self.interner.resolve(name.inner).unwrap().to_string();

                // Reserve the slot so recursive field types can detect the cycle
                self.symbol_lookup.insert(
                    (SymbolNamespace::Type, name.inner),
                    GlobalValue::Struct { struct_index },
                );
                self.structs.push(Struct {
                    name: name.clone(),
                    fields: Box::new([]),
                    lookup: HashMap::new(),
                });

                let mut seen_fields: HashMap<SymbolU32, ast::TextSpan> = HashMap::new();
                let mut tir_fields: Vec<StructField> = Vec::new();
                let mut lookup: HashMap<SymbolU32, usize> = HashMap::new();
                let mut has_error = false;

                for field in fields.inner.iter() {
                    let field = &field.inner.inner;
                    let field_name_sym = field.name.inner;
                    let field_name = self.interner.resolve(field_name_sym).unwrap().to_string();

                    // Duplicate field name in the declaration
                    if let Some(&first_span) = seen_fields.get(&field_name_sym) {
                        self.diagnostics.push(
                            DuplicateStructFieldDiagnostic {
                                file_id: self.file_id,
                                name: &field_name,
                                first_span,
                                second_span: field.name.span,
                            }
                            .report(),
                        );
                        has_error = true;
                        continue;
                    }

                    let field_ty = self.resolve_type(&field.ty);

                    // Recursive struct: field type is this struct itself
                    if field_ty == self.type_pool.intern(Type::Struct { struct_index }) {
                        self.diagnostics.push(
                            RecursiveStructTypeDiagnostic {
                                file_id: self.file_id,
                                struct_name: &struct_name,
                                field_span: field.name.span,
                            }
                            .report(),
                        );
                        has_error = true;
                        continue;
                    }

                    seen_fields.insert(field_name_sym, field.name.span);
                    let field_index = tir_fields.len();
                    lookup.insert(field_name_sym, field_index);
                    tir_fields.push(StructField {
                        name: field.name.clone(),
                        ty: ast::Spanned {
                            inner: field_ty,
                            span: field.ty.span,
                        },
                    });
                }

                // Reorder fields for optimal memory layout: sort by alignment
                // descending so larger-aligned fields come first, minimising
                // padding. Uses a stable sort so equal-alignment fields keep
                // their declaration order.
                let mut pairs: Vec<(u32, StructField)> = tir_fields
                    .into_iter()
                    .map(|field| {
                        let align = compute_layout(
                            self.type_pool.as_slice(),
                            &self.structs,
                            field.ty.inner,
                            self.ptr_size,
                        )
                        .align;
                        (align, field)
                    })
                    .collect();
                pairs.sort_by(|a, b| b.0.cmp(&a.0));
                lookup = pairs
                    .iter()
                    .enumerate()
                    .map(|(phys, (_, f))| (f.name.inner, phys))
                    .collect();
                let tir_fields: Vec<StructField> =
                    pairs.into_iter().map(|(_, f)| f).collect();

                // Patch the reserved slot with real fields
                self.structs[struct_index as usize].fields = tir_fields.into_boxed_slice();
                self.structs[struct_index as usize].lookup = lookup;

                if has_error { Err(()) } else { Ok(()) }
            }
            ast::Item::Memory { .. } => {
                // TODO: lower memory items in TIR
                Ok(())
            }
            ast::Item::Impl { target, items } => {
                let self_type = self.resolve_type(target);
                let self_symbol = self.interner.get_or_intern("self");
                for item in items.inner.iter() {
                    match &item.inner.inner {
                        ast::ImplItem::Const { name, ty, value } => {
                            let resolved_ty = match ty {
                                Some(ty_expr) => self.resolve_type(ty_expr),
                                None => UNKNOWN_IDX,
                            };
                            let const_value = match self.eval_impl_const_int(value, resolved_ty) {
                                Ok(v) => v,
                                Err(()) => continue,
                            };
                            self.impl_members.entry(self_type).or_default().insert(
                                name.inner,
                                ImplEntry::AssociatedConst {
                                    ty: resolved_ty,
                                    value: const_value,
                                },
                            );
                        }
                        ast::ImplItem::Method { signature: sig, .. } => {
                            let params = sig
                                .params
                                .inner
                                .iter()
                                .map(|p| {
                                    let is_self = p.inner.inner.name.inner == self_symbol;
                                    DeclaredFunctionParam {
                                        name: Some(p.inner.inner.name.clone()),
                                        ty: match &p.inner.inner.ty {
                                            Some(ty) => Spanned {
                                                inner: self.resolve_type(ty),
                                                span: ty.span,
                                            },
                                            None => Spanned {
                                                inner: if is_self { self_type } else { ERROR_IDX },
                                                span: p.inner.inner.name.span,
                                            },
                                        },
                                    }
                                })
                                .collect();
                            let result = sig.result.as_ref().map(|r| Spanned {
                                inner: self.resolve_type(r),
                                span: r.span,
                            });
                            let func_index = self.declare_function(
                                sig.name.clone(),
                                params,
                                result,
                                ItemSource::Defined,
                            );
                            let is_method = sig
                                .params
                                .inner
                                .first()
                                .map(|p| p.inner.inner.name.inner == self_symbol)
                                .unwrap_or(false);
                            let entry = if is_method {
                                ImplEntry::Method(func_index)
                            } else {
                                ImplEntry::AssociatedFn(func_index)
                            };
                            self.impl_members
                                .entry(self_type)
                                .or_default()
                                .insert(sig.name.inner, entry);
                        }
                    }
                }
                Ok(())
            }
        }
    }

    fn define_import_module(
        &mut self,
        external_name: ast::Spanned<SymbolU32>,
        internal_name: Option<ast::Spanned<SymbolU32>>,
        entries: &ast::Grouped<Box<[ast::Separated<Spanned<ast::ImportEntry>>]>>,
    ) -> Result<ImportModule, ()> {
        let mut module = ImportModule {
            lookup: HashMap::new(),
            external_name,
            internal_name,
        };

        for entry in &entries.inner {
            let entry_name_symbol = match &entry.inner.inner.declaration {
                ast::ImportDeclaration::Function { name, .. } => name.inner,
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
                            file_id: self.file_id,
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
                ast::ImportDeclaration::Function {
                    name,
                    params,
                    result,
                } => {
                    let mut seen_params: HashMap<SymbolU32, ast::TextSpan> = HashMap::new();
                    let params: Box<[DeclaredFunctionParam]> = params
                        .inner
                        .iter()
                        .map(|param| {
                            let param_name = match param.inner.inner.name.clone() {
                                Some(name) => Some(name),
                                None => {
                                    self.diagnostics.push(
                                        MissingImportParamNameDiagnostic {
                                            file_id: self.file_id,
                                            span: param.inner.inner.ty.span,
                                        }
                                        .report(),
                                    );
                                    None
                                }
                            };
                            if let Some(explicit_name) = &param.inner.inner.name {
                                if let Some(&first_span) = seen_params.get(&explicit_name.inner) {
                                    let name_str =
                                        self.interner.resolve(explicit_name.inner).unwrap();
                                    self.diagnostics.push(
                                        DuplicateParameterDiagnostic {
                                            file_id: self.file_id,
                                            name: name_str,
                                            first_definition: first_span,
                                            second_definition: explicit_name.span,
                                        }
                                        .report(),
                                    );
                                } else {
                                    seen_params.insert(explicit_name.inner, explicit_name.span);
                                }
                            }
                            DeclaredFunctionParam {
                                name: param_name,
                                ty: Spanned {
                                    inner: self.resolve_type(&param.inner.inner.ty),
                                    span: param.inner.inner.ty.span,
                                },
                            }
                        })
                        .collect();
                    let result = result.as_ref().map(|result| Spanned {
                        inner: self.resolve_type(result),
                        span: result.span,
                    });
                    let func_index =
                        self.declare_function(name.clone(), params, result, ItemSource::Imported);
                    ImportValue::Function { func_index }
                }
                ast::ImportDeclaration::Global { name, mut_span, ty } => {
                    let ty = ast::Spanned {
                        inner: self.resolve_type(&ty),
                        span: ty.span,
                    };
                    let global_index = self.declared_globals.len() as u32;
                    self.declared_globals.push(DeclaredGlobal {
                        source: ItemSource::Imported,
                        name: name.clone(),
                        ty,
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
            ast::Item::Function { pub_span, signature, block } => {
                let func_index = self.resolve_func(signature.name.inner).unwrap();
                if let Some(span) = pub_span {
                    self.declared_functions[func_index as usize].pub_span = Some(*span);
                }
                self.build_function_definition(signature, block, func_index)
            }
            ast::Item::Global {
                name,
                value,
                mut_span,
                ..
            } => {
                let global_index = match self.resolve_value(name.inner) {
                    Some(GlobalValue::Global { global_index }) => global_index,
                    _ => return Err(()),
                };

                let global_type_idx = self.declared_globals[global_index as usize].ty.inner;
                let value_expr = self.build_const_expression(value, global_type_idx)?;

                match value_expr.ty {
                    _ if value_expr.ty == UNKNOWN_IDX => {
                        self.diagnostics.push(
                            TypeAnnotationRequiredDiagnostic {
                                file_id: self.file_id,
                                span: value.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                    ty if !self.type_pool.coercible_to(ty, global_type_idx) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.file_id,
                                expected_type: global_type_idx,
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
                        ty: ast::Spanned {
                            inner: global_type_idx,
                            span: self.declared_globals[global_index as usize].ty.span,
                        },
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
                                    file_id: self.file_id,
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
                                    file_id: self.file_id,
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
                                    file_id: self.file_id,
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
            ast::Item::Enum { .. } => {
                // TODO: lower enum items in TIR
                Ok(())
            }
            ast::Item::Const { .. } => {
                // Evaluated eagerly in define_item; nothing to do here.
                Ok(())
            }
            ast::Item::Struct { .. } => {
                // TODO: lower struct items in TIR
                Ok(())
            }
            ast::Item::Memory { .. } => {
                // TODO: lower memory items in TIR
                Ok(())
            }
            ast::Item::Impl { target, items } => {
                let target_type = self.resolve_type(target);
                for item in items.inner.iter() {
                    let (sig, block, pub_span) = match &item.inner.inner {
                        ast::ImplItem::Method { pub_span, signature, block } => (signature, block, pub_span),
                        ast::ImplItem::Const { .. } => continue,
                    };
                    let func_index = match self
                        .impl_members
                        .get(&target_type)
                        .and_then(|m| m.get(&sig.name.inner))
                    {
                        Some(ImplEntry::Method(i) | ImplEntry::AssociatedFn(i)) => *i,
                        Some(ImplEntry::AssociatedConst { .. }) => continue,
                        None => continue,
                    };
                    if let Some(span) = pub_span {
                        self.declared_functions[func_index as usize].pub_span = Some(*span);
                    }
                    self.build_function_definition(sig, block, func_index)?;
                }
                Ok(())
            }
        }
    }

    /// Evaluates a constant expression that must produce an integer value.
    /// Only integer types (I32, I64, U32, U64) are supported; other types
    /// cause a `todo!` panic. Non-constant expressions emit a diagnostic.
    fn eval_impl_const_int(
        &mut self,
        expr: &ast::Spanned<ast::Expression>,
        ty: TypeIndex,
    ) -> Result<i64, ()> {
        match ty {
            I32_IDX | I64_IDX | U32_IDX | U64_IDX | UNKNOWN_IDX => {}
            _ => todo!("associated constants are only supported for integer types"),
        }
        self.eval_const_int_expr(expr)
    }

    fn eval_const_int_expr(&mut self, expr: &ast::Spanned<ast::Expression>) -> Result<i64, ()> {
        match &expr.inner {
            ast::Expression::Int { value } => Ok(*value),
            ast::Expression::Grouping { value } => self.eval_const_int_expr(value),
            ast::Expression::Unary { operator, operand } => {
                let v = self.eval_const_int_expr(operand)?;
                match operator.inner {
                    ast::UnaryOp::InvertSign => Ok(v.wrapping_neg()),
                    ast::UnaryOp::BitNot => Ok(!v),
                    ast::UnaryOp::Not => {
                        self.diagnostics.push(
                            NonConstantGlobalInitializerDiagnostic {
                                file_id: self.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                        Err(())
                    }
                }
            }
            ast::Expression::Binary {
                left,
                right,
                operator,
            } => {
                let l = self.eval_const_int_expr(left)?;
                let r = self.eval_const_int_expr(right)?;
                match operator.inner {
                    ast::BinaryOp::Add => Ok(l.wrapping_add(r)),
                    ast::BinaryOp::Sub => Ok(l.wrapping_sub(r)),
                    ast::BinaryOp::Mul => Ok(l.wrapping_mul(r)),
                    ast::BinaryOp::Div => Ok(l.wrapping_div(r)),
                    ast::BinaryOp::Rem => Ok(l.wrapping_rem(r)),
                    ast::BinaryOp::BitAnd => Ok(l & r),
                    ast::BinaryOp::BitOr => Ok(l | r),
                    ast::BinaryOp::BitXor => Ok(l ^ r),
                    ast::BinaryOp::LeftShift => Ok(l.wrapping_shl(r as u32)),
                    ast::BinaryOp::RightShift => Ok(l.wrapping_shr(r as u32)),
                    _ => {
                        self.diagnostics.push(
                            NonConstantGlobalInitializerDiagnostic {
                                file_id: self.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                        Err(())
                    }
                }
            }
            _ => {
                self.diagnostics.push(
                    NonConstantGlobalInitializerDiagnostic {
                        file_id: self.file_id,
                        span: expr.span,
                    }
                    .report(),
                );
                Err(())
            }
        }
    }

    fn build_const_expression(
        &mut self,
        expr: &ast::Spanned<ast::Expression>,
        expected_type: TypeIndex,
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
                        ty: BOOL_IDX,
                        span: expr.span,
                    }),
                    Some(GlobalValue::False) => Ok(Expression {
                        kind: ExprKind::Bool { value: false },
                        ty: BOOL_IDX,
                        span: expr.span,
                    }),
                    Some(GlobalValue::Const { const_index }) => {
                        let ty = self.declared_consts[const_index as usize].ty.inner;
                        match self.const_pool[const_index as usize].clone() {
                            ConstValue::Int(v) => Ok(Expression {
                                kind: ExprKind::Int { value: v },
                                ty,
                                span: expr.span,
                            }),
                            ConstValue::Float(v) => Ok(Expression {
                                kind: ExprKind::Float { value: v },
                                ty,
                                span: expr.span,
                            }),
                        }
                    }
                    _ => {
                        self.diagnostics.push(
                            NonConstantGlobalInitializerDiagnostic {
                                file_id: self.file_id,
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
                    operator if operator.is_comparison() || operator.is_logical() => BOOL_IDX,
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
                self.build_const_expression(value, expected_type)
            }
            ast::Expression::Cast { value, ty } => {
                let cast_type = self.resolve_type(&ty);
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
                        file_id: self.file_id,
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
        func_index: FunctionIndex,
    ) -> Result<(), ()> {
        let lookup = signature
            .params
            .inner
            .iter()
            .enumerate()
            .map(|(index, param)| {
                (
                    (0 as ScopeIndex, param.inner.inner.name.inner),
                    index as LocalIndex,
                )
            })
            .collect();

        let params: Box<[FunctionParam]> = signature
            .params
            .inner
            .iter()
            .zip(self.declared_functions[func_index as usize].params.iter())
            .map(|(ast_param, declared_param)| FunctionParam {
                mut_span: ast_param.inner.inner.mut_span,
                name: ast_param.inner.inner.name.clone(),
                ty: Spanned {
                    inner: declared_param.ty.inner,
                    span: declared_param.ty.span,
                },
            })
            .collect();

        let result_span = match &signature.result {
            Some(result) => Some(result.span),
            None => None,
        };
        let signature_index = self.declared_functions[func_index as usize].signature_index;
        let result_type_idx = self.type_pool.function_result(signature_index);

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
            expected_type: Some(result_type_idx),
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
                result_span,
                signature_index,
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
                            file_id: self.file_id,
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
                let inferred_type = scope.inferred_type.unwrap_or(NEVER_IDX);
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
                            expected_type: Some(UNIT_IDX),
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
                        .unwrap_or(NEVER_IDX),
                    span: block.span,
                })
            }
            BlockKind::Block => {
                let result = self.build_block_result(ctx, result.as_deref())?;

                self.report_local_warnings(&ctx.frame.scopes[ctx.scope_index as usize]);

                let scope = &ctx.frame.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.expect("should have inferred type");
                match scope.expected_type {
                    Some(expected_type)
                        if !self.type_pool.coercible_to(inferred_type, expected_type) =>
                    {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.file_id,
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
            if local.accesses.is_empty() && local.ty != ERROR_IDX {
                self.diagnostics.push(
                    UnusedVariableDiagnostic {
                        file_id: self.file_id,
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
                            file_id: self.file_id,
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
            let decl = &self.declared_functions[*func_index as usize];
            if decl.accesses.is_empty() && decl.pub_span.is_none() {
                let name = self.interner.resolve(func.name.inner).unwrap().to_string();
                self.diagnostics.push(
                    UnusedFunctionDiagnostic {
                        file_id: self.file_id,
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
                        file_id: self.file_id,
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
                if result.ty == UNKNOWN_IDX {
                    _ = self.coerce_untyped_expr(&mut result, inferred_type);
                }

                Ok(Some(result))
            }
            None => {
                let scope = &mut ctx.frame.scopes[ctx.scope_index as usize];
                let inferred_type = scope.inferred_type.unwrap_or(UNIT_IDX);
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
                _ if expr.ty == NEVER_IDX => {
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
        if value.ty == UNKNOWN_IDX {
            match scope.inferred_type.or(scope.expected_type) {
                Some(ty) => return Ok(ty),
                None => {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.file_id,
                            span: value.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
            }
        }
        let result_type = value.ty;
        match scope.inferred_type {
            Some(inferred_type) if !self.type_pool.coercible_to(result_type, inferred_type) => {
                self.diagnostics.push(
                    TypeMistmatchDiagnostic {
                        file_id: self.file_id,
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
                Some(expected_type) if !self.type_pool.coercible_to(result_type, expected_type) => {
                    self.diagnostics.push(
                        TypeMistmatchDiagnostic {
                            file_id: self.file_id,
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
        if value.ty == UNIT_IDX {
            return Ok(value);
        } else if value.ty == ERROR_IDX {
            // Skip reporting unused value for error types, as the error has already been
            // reported
            return Ok(value);
        } else if value.ty == NEVER_IDX {
            let scope = ctx.frame.scopes.get_mut(ctx.scope_index as usize).unwrap();
            scope.inferred_type = scope.inferred_type.or(Some(NEVER_IDX));
            return Ok(value);
        } else if value.ty == UNKNOWN_IDX {
            self.diagnostics.push(
                TypeAnnotationRequiredDiagnostic {
                    file_id: self.file_id,
                    span: value.span,
                }
                .report(),
            );
            return Err(());
        }
        self.diagnostics.push(
            UnusedValueDiagnostic {
                file_id: self.file_id,
                span: value.span,
            }
            .report(),
        );
        Ok(value)
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
                ty: UNKNOWN_IDX,
                span: expr.span,
            }),
            ast::Expression::Float { value } => Ok(Expression {
                kind: ExprKind::Float { value: *value },
                ty: UNKNOWN_IDX,
                span: expr.span,
            }),
            ast::Expression::Unreachable => Ok(Expression {
                kind: ExprKind::Unreachable,
                ty: NEVER_IDX,
                span: expr.span,
            }),
            ast::Expression::Error => Ok(Expression {
                kind: ExprKind::Error,
                ty: ERROR_IDX,
                span: expr.span,
            }),
            ast::Expression::String { symbol } => {
                let unescaped = unescape_string(self.interner.resolve(*symbol).unwrap());
                let symbol = self.interner.get_or_intern(&unescaped);
                Ok(Expression {
                    kind: ExprKind::String { symbol },
                    ty: STRING_IDX,
                    span: expr.span,
                })
            }
            ast::Expression::Char { symbol } => {
                let raw = self.interner.resolve(*symbol).unwrap();
                match parse_char_literal(raw) {
                    Ok(value) => Ok(Expression {
                        kind: ExprKind::Char { value },
                        ty: CHAR_IDX,
                        span: expr.span,
                    }),
                    Err(CharLiteralError::Empty) => {
                        self.diagnostics.push(
                            EmptyCharLiteralDiagnostic {
                                file_id: self.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                        Err(())
                    }
                    Err(CharLiteralError::TooLong) => {
                        self.diagnostics.push(
                            CharLiteralTooLongDiagnostic {
                                file_id: self.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                        Err(())
                    }
                }
            }
            ast::Expression::Identifier { .. } => {
                self.build_identifier_expression(func_ctx, expr, access_ctx)
            }
            ast::Expression::Binary { .. } => {
                self.build_binary_expression(func_ctx, expr, access_ctx)
            }
            ast::Expression::Grouping { value } => {
                self.build_expression(func_ctx, value, access_ctx)
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
            ast::Expression::TupleFieldAccess { .. } => {
                todo!("tuple field access in TIR")
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
            ast::Expression::StructInit { name, fields } => {
                self.build_struct_init_expression(func_ctx, expr.span, name.clone(), &fields.inner)
            }
            ast::Expression::Tuple { .. } => {
                todo!("tuple in TIR")
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
        let entry = self
            .impl_members
            .get(&object.ty)
            .and_then(|members| members.get(&member.inner));
        match entry {
            Some(ImplEntry::Method(func_index)) => {
                let func_index = *func_index;
                let func = &mut self.declared_functions[func_index as usize];
                func.accesses.push(member.span);
                let ty = func.signature_index;
                return Ok(Expression {
                    kind: ExprKind::ObjectAccess {
                        object: Box::new(object),
                        member: member.clone(),
                    },
                    ty,
                    span: ast::TextSpan::merge(object_expr.span, member.span),
                });
            }
            Some(ImplEntry::AssociatedFn(_)) => {
                // TODO: emit "this is an associated function, use Type::name()
                // syntax" diagnostic
            }
            Some(ImplEntry::AssociatedConst { ty, value }) => {
                // For now, we only support associated constants for enums, so we can directly
                // resolve the value here without needing to create a separate expression kind for it
                return Ok(Expression {
                    kind: ExprKind::Int { value: *value },
                    ty: *ty,
                    span: ast::TextSpan::merge(object_expr.span, member.span),
                });
            }
            None => {}
        }

        // Check struct fields
        if let Type::Struct { struct_index } = *self.type_pool.get(object.ty) {
            if let Some(&field_index) = self.structs[struct_index as usize].lookup.get(&member.inner) {
                let field_ty = self.structs[struct_index as usize].fields[field_index].ty.inner;
                return Ok(Expression {
                    kind: ExprKind::ObjectAccess {
                        object: Box::new(object),
                        member: member.clone(),
                    },
                    ty: field_ty,
                    span: ast::TextSpan::merge(object_expr.span, member.span),
                });
            }
        }

        self.diagnostics.push(
            UndeclaredIdentifierDiagnostic {
                file_id: self.file_id,
                span: member.span,
            }
            .report(),
        );
        Err(())
    }

    /// Ensures built-in constants (`SIZE`, `ALIGN`) are present for `ty` and
    /// returns a reference to its impl-member map. This is the single entry
    /// point for all impl-member lookups; never access `impl_members` directly
    /// for namespace-style resolution.
    fn ensure_impl_members(&mut self, ty: TypeIndex) -> &HashMap<SymbolU32, ImplEntry> {
        let size_sym = self.interner.get_or_intern("SIZE");
        let already_seeded = self
            .impl_members
            .get(&ty)
            .map_or(false, |m| m.contains_key(&size_sym));
        if !already_seeded {
            let is_sized = !matches!(
                self.type_pool.get(ty),
                Type::Error | Type::Unknown | Type::Namespace { .. } | Type::Function { .. }
            );
            if is_sized {
                let align_sym = self.interner.get_or_intern("ALIGN");
                let layout =
                    compute_layout(self.type_pool.as_slice(), &self.structs, ty, self.ptr_size);
                let result_ty = match self.ptr_size {
                    PointerSize::P32 => U32_IDX,
                    PointerSize::P64 => U64_IDX,
                };
                let members = self.impl_members.entry(ty).or_default();
                members.insert(
                    size_sym,
                    ImplEntry::AssociatedConst {
                        ty: result_ty,
                        value: layout.size as i64,
                    },
                );
                members.insert(
                    align_sym,
                    ImplEntry::AssociatedConst {
                        ty: result_ty,
                        value: layout.align as i64,
                    },
                );
            }
        }
        self.impl_members.entry(ty).or_default()
    }

    fn build_namespace_access_expression(
        &mut self,
        namespace_expr: &Spanned<ast::TypeExpression>,
        member: Spanned<SymbolU32>,
    ) -> Result<Expression, ()> {
        let namespace_ty = self.resolve_type(&namespace_expr);
        if namespace_ty == ERROR_IDX {
            return Err(());
        }

        let full_span = ast::TextSpan::merge(namespace_expr.span, member.span);

        let entry = self
            .ensure_impl_members(namespace_ty)
            .get(&member.inner)
            .copied();
        if let Some(entry) = entry {
            match entry {
                ImplEntry::AssociatedConst { ty, value: _ } => {
                    return Ok(Expression {
                        kind: ExprKind::NamespaceAccess {
                            ty: ast::Spanned {
                                inner: namespace_ty,
                                span: namespace_expr.span,
                            },
                            member: member.clone(),
                        },
                        ty,
                        span: full_span,
                    });
                }
                ImplEntry::AssociatedFn(_) | ImplEntry::Method(_) => {
                    // TODO: emit a diagnostic — use Type::fn() call syntax
                }
            }
        }

        let namespace_index = match self.type_pool.get(namespace_ty) {
            Type::Namespace { namespace_index } => *namespace_index,
            _ => {
                let diag = NotANamespaceDiagnostic {
                    file_id: self.file_id,
                    span: namespace_expr.span,
                    ty: namespace_ty,
                }
                .report(self);
                self.diagnostics.push(diag);
                return Err(());
            }
        };

        let namespace = &self.namespaces[namespace_index as usize];
        match namespace {
            Namespace::Enum(enum_) => match enum_.lookup.get(&member.inner) {
                Some(_variant_index) => Ok(Expression {
                    kind: ExprKind::NamespaceAccess {
                        ty: ast::Spanned {
                            inner: namespace_ty,
                            span: namespace_expr.span,
                        },
                        member: member.clone(),
                    },
                    ty: enum_.ty,
                    span: ast::TextSpan::merge(namespace_expr.span, member.span),
                }),
                _ => {
                    self.diagnostics.push(
                        UndeclaredIdentifierDiagnostic {
                            file_id: self.file_id,
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
                    let ty = func.signature_index;
                    Ok(Expression {
                        kind: ExprKind::NamespaceAccess {
                            ty: ast::Spanned {
                                inner: namespace_ty,
                                span: namespace_expr.span,
                            },
                            member: member.clone(),
                        },
                        ty,
                        span: ast::TextSpan::merge(namespace_expr.span, member.span),
                    })
                }
                Some(ImportValue::Global { global_index }) => {
                    let global = &mut self.declared_globals[*global_index as usize];
                    global.accesses.push(member.span);
                    let ty = global.ty.inner;
                    Ok(Expression {
                        kind: ExprKind::NamespaceAccess {
                            ty: ast::Spanned {
                                inner: namespace_ty,
                                span: namespace_expr.span,
                            },
                            member: member.clone(),
                        },
                        ty,
                        span: ast::TextSpan::merge(namespace_expr.span, member.span),
                    })
                }
                None => {
                    self.diagnostics.push(
                        UndeclaredIdentifierDiagnostic {
                            file_id: self.file_id,
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
                        if !self.type_pool.coercible_to(inferred_type, expected_type) =>
                    {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.file_id,
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
                            file_id: self.file_id,
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
            ty: NEVER_IDX,
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
                expected_type: Some(BOOL_IDX),
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

                match self.type_pool.unify(then_block.ty, else_block.ty) {
                    Ok(ty) => (Some(else_block), ty),
                    Err(_) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.file_id,
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
            None => {
                if then_block.ty == UNIT_IDX || then_block.ty == NEVER_IDX {
                    (None, UNIT_IDX)
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
        expr: &Spanned<ast::Expression>,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let (value, cast_type) = match &expr.inner {
            ast::Expression::Cast { value, ty } => (value, ty),
            _ => unreachable!(),
        };

        let cast_type_idx = self.resolve_type(&cast_type);
        if cast_type_idx == ERROR_IDX {
            return self.build_expression(ctx, value, access_ctx);
        }
        let cast_type = cast_type_idx;
        let mut value = self.build_expression(
            ctx,
            value,
            AccessContext {
                expected_type: Some(cast_type),
                access_kind: access_ctx.access_kind,
            },
        )?;
        if value.ty == UNKNOWN_IDX {
            self.coerce_untyped_expr(&mut value, cast_type)?;
        } else if value.ty == cast_type {
            // TODO: report redundant cast
        } else if *self.type_pool.get(value.ty) == Type::Bool
            && self.type_pool.get(cast_type).is_integer()
        {
            value.ty = cast_type;
        } else if is_numeric_cast(value.ty, cast_type) {
            value.ty = cast_type;
        } else if value.ty != cast_type {
            self.diagnostics.push(
                TypeMistmatchDiagnostic {
                    file_id: self.file_id,
                    expected_type: cast_type,
                    actual_type: value.ty,
                    span: value.span,
                }
                .report(&self),
            );
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
                Some(scope_index) => scope_index,
                None => {
                    self.diagnostics.push(
                        UndeclaredLabelDiagnostic {
                            file_id: self.file_id,
                            span: label.span,
                        }
                        .report(),
                    );

                    // TODO: how to handle this better? we don't parse the value if the label is
                    // undeclared
                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: NEVER_IDX,
                        span: expr.span,
                    });
                }
            },
            None => match ctx.get_closest_loop_block() {
                Some(scope_index) => scope_index,
                None => {
                    self.diagnostics.push(
                        BreakOutsideOfLoopDiagnostic {
                            file_id: self.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );

                    // TODO: same as above, we don't parse the value if the break is outside of a
                    // loop
                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: NEVER_IDX,
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
                    if value.ty == UNKNOWN_IDX {
                        self.coerce_untyped_expr(&mut value, inferred_type)?;
                    }
                    scope.inferred_type = Some(inferred_type);

                    Ok(Expression {
                        kind: ExprKind::Break {
                            scope_index,
                            value: Some(Box::new(value)),
                        },
                        ty: NEVER_IDX,
                        span: expr.span,
                    })
                })
                .unwrap_or(Expression {
                    kind: ExprKind::Unreachable,
                    ty: NEVER_IDX,
                    span: expr.span,
                })),
            None => {
                let scope = ctx.frame.scopes.get_mut(scope_index as usize).unwrap();
                match scope.inferred_type {
                    Some(inferred) => {
                        if !self.type_pool.coercible_to(UNIT_IDX, inferred) {
                            self.diagnostics.push(
                                TypeMistmatchDiagnostic {
                                    file_id: self.file_id,
                                    expected_type: inferred,
                                    actual_type: UNIT_IDX,
                                    span: expr.span,
                                }
                                .report(&self),
                            );
                        }
                    }
                    None => {
                        scope.inferred_type = Some(UNIT_IDX);
                    }
                }

                Ok(Expression {
                    kind: ExprKind::Break {
                        scope_index,
                        value: None,
                    },
                    ty: NEVER_IDX,
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
                    ty: BOOL_IDX,
                    span: expr.span,
                }),
                GlobalValue::False => Ok(Expression {
                    kind: ExprKind::Bool { value: false },
                    ty: BOOL_IDX,
                    span: expr.span,
                }),
                GlobalValue::Placeholder => Ok(Expression {
                    kind: ExprKind::Placeholder,
                    ty: access_ctx.expected_type.unwrap_or(ERROR_IDX),
                    span: expr.span,
                }),
                GlobalValue::Function { func_index } => {
                    let func = &mut self.declared_functions[func_index as usize];
                    func.accesses.push(expr.span);
                    let ty = func.signature_index;
                    Ok(Expression {
                        kind: ExprKind::Function { func_index },
                        ty,
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
                GlobalValue::Const { const_index } => {
                    let ty = self.declared_consts[const_index as usize].ty.inner;
                    match self.const_pool[const_index as usize].clone() {
                        ConstValue::Int(v) => Ok(Expression {
                            kind: ExprKind::Int { value: v },
                            ty,
                            span: expr.span,
                        }),
                        ConstValue::Float(v) => Ok(Expression {
                            kind: ExprKind::Float { value: v },
                            ty,
                            span: expr.span,
                        }),
                    }
                }
                // this must be handled in the namespace access expression
                GlobalValue::Namespace { .. } => todo!(),
                GlobalValue::Unreachable => unreachable!(),
                // Struct names are type-namespace values, not usable as expressions
                GlobalValue::Struct { .. } => {
                    self.diagnostics.push(
                        UndeclaredIdentifierDiagnostic {
                            file_id: self.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                    Ok(Expression {
                        kind: ExprKind::Error,
                        ty: access_ctx.expected_type.unwrap_or(ERROR_IDX),
                        span: expr.span,
                    })
                }
            },
            None => {
                self.diagnostics.push(
                    UndeclaredIdentifierDiagnostic {
                        file_id: self.file_id,
                        span: expr.span,
                    }
                    .report(),
                );

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: access_ctx.expected_type.unwrap_or(ERROR_IDX),
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
            ast::UnaryOp::InvertSign | ast::UnaryOp::BitNot => {
                let ty_val = self.type_pool.get(operand.ty);
                if ty_val.is_primitive() || operand.ty == UNKNOWN_IDX {
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
                if operand.ty == BOOL_IDX {
                    Ok(Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: BOOL_IDX,
                        span: expr.span,
                    })
                } else if operand.ty == UNKNOWN_IDX {
                    _ = self.coerce_untyped_expr(&mut operand, BOOL_IDX);
                    Ok(Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: BOOL_IDX,
                        span: expr.span,
                    })
                } else {
                    let ty = operand.ty;
                    self.diagnostics.push(
                        UnaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.file_id,
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
                        ty: BOOL_IDX,
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
            left,
            AccessContext {
                expected_type: Some(BOOL_IDX),
                access_kind: AccessKind::Read,
            },
        )?;
        if left.ty == ERROR_IDX {
            // Error already reported
        } else if left.ty == UNKNOWN_IDX {
            self.diagnostics.push(
                TypeAnnotationRequiredDiagnostic {
                    file_id: self.file_id,
                    span: left.span,
                }
                .report(),
            );
        } else if left.ty != BOOL_IDX {
            self.diagnostics.push(
                TypeMistmatchDiagnostic {
                    file_id: self.file_id,
                    expected_type: BOOL_IDX,
                    actual_type: left.ty,
                    span: left.span,
                }
                .report(&self),
            );
        }
        let right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: Some(BOOL_IDX),
                access_kind: AccessKind::Read,
            },
        )?;
        if right.ty == ERROR_IDX {
            // Error already reported
        } else if right.ty == UNKNOWN_IDX {
            self.diagnostics.push(
                TypeAnnotationRequiredDiagnostic {
                    file_id: self.file_id,
                    span: right.span,
                }
                .report(),
            );
        } else if right.ty != BOOL_IDX {
            self.diagnostics.push(
                TypeMistmatchDiagnostic {
                    file_id: self.file_id,
                    expected_type: BOOL_IDX,
                    actual_type: right.ty,
                    span: right.span,
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
            ty: BOOL_IDX,
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
                expected_type: match *self.type_pool.get(left.ty) {
                    Type::Unknown | Type::Error | Type::Never | Type::Unit => {
                        access_ctx.expected_type
                    }
                    _ => Some(left.ty),
                },
                access_kind: access_ctx.access_kind,
            },
        )?;

        match (left.ty, right.ty) {
            // Allow operations with Error type (error already reported elsewhere)
            (l, r) if l == ERROR_IDX || r == ERROR_IDX => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: access_ctx.expected_type.unwrap_or(ERROR_IDX),
                span: expr.span,
            }),
            (l, r) if l == UNKNOWN_IDX && r == UNKNOWN_IDX => match access_ctx.expected_type {
                Some(expected_type) => {
                    self.coerce_untyped_expr(&mut left, expected_type)?;
                    self.coerce_untyped_expr(&mut right, expected_type)?;

                    if !self.type_pool.get(expected_type).is_integer() {
                        self.diagnostics.push(
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.file_id,
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
                            file_id: self.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                    Err(())
                }
            },
            (l, right_type) if l == UNKNOWN_IDX => {
                if !self.type_pool.get(right_type).is_integer() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.file_id,
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
            (left_type, r) if r == UNKNOWN_IDX => {
                if !self.type_pool.get(left_type).is_integer() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.file_id,
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
            (left_type, right_type)
                if left_type == right_type && self.type_pool.get(left_type).is_integer() =>
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
                self.diagnostics.push(
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
                    }
                    .report(&self),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: access_ctx.expected_type.unwrap_or(UNKNOWN_IDX),
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
            (l, r) if l == ERROR_IDX || r == ERROR_IDX => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: BOOL_IDX,
                span: expr.span,
            }),
            (l, r) if l == UNKNOWN_IDX && r == UNKNOWN_IDX => {
                self.diagnostics.push(
                    ComparisonTypeAnnotationRequiredDiagnostic {
                        file_id: self.file_id,
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
                    ty: BOOL_IDX,
                    span: expr.span,
                })
            }
            (l, ty) if l == UNKNOWN_IDX => {
                self.coerce_untyped_expr(&mut left, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: BOOL_IDX,
                    span: expr.span,
                })
            }
            (ty, r) if r == UNKNOWN_IDX => {
                self.coerce_untyped_expr(&mut right, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: BOOL_IDX,
                    span: expr.span,
                })
            }
            (l, r) if l == BOOL_IDX && r == BOOL_IDX => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: BOOL_IDX,
                span: expr.span,
            }),
            (left_type, right_type)
                if left_type == right_type && self.type_pool.get(left_type).is_primitive() =>
            {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: BOOL_IDX,
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
                    }
                    .report(&self),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: BOOL_IDX,
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
                                file_id: self.file_id,
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
                                file_id: self.file_id,
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
                if right.ty == UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right, local_type)?;
                } else if !self.type_pool.coercible_to(right.ty, local_type) {
                    self.diagnostics.push(
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
                        }
                        .report(&self),
                    );
                }

                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: UNIT_IDX,
                    span: expr.span,
                })
            }
            ExprKind::Global { global_index } => {
                let global = &self.declared_globals[global_index as usize];
                match global.mut_span {
                    None => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.file_id,
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
                if right.ty == UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right, global_type)?;
                } else if !self.type_pool.coercible_to(right.ty, global_type) {
                    self.diagnostics.push(
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
                        }
                        .report(&self),
                    );
                }

                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: UNIT_IDX,
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
                if right.ty == UNKNOWN_IDX {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.file_id,
                            span: right.span,
                        }
                        .report(),
                    );
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
                    ty: UNIT_IDX,
                    span: expr.span,
                });
            }
            _ => {
                self.diagnostics.push(
                    InvalidAssignmentTargetDiagnostic {
                        file_id: self.file_id,
                        span: left.span,
                    }
                    .report(),
                );

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: UNIT_IDX,
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
                                file_id: self.file_id,
                                span: left.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                };
                // Allow operations with Error type (error already reported elsewhere)
                if local.ty == ERROR_IDX {
                    let right = self.build_expression(
                        ctx,
                        right,
                        AccessContext {
                            expected_type: Some(ERROR_IDX),
                            access_kind: AccessKind::Read,
                        },
                    )?;
                    return Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ty: UNIT_IDX,
                        span: expr.span,
                    });
                }
                if !self.type_pool.get(local.ty).is_primitive() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.file_id,
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
                            file_id: self.file_id,
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
                if right.ty == UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right, local_type)?;
                } else if !self.type_pool.coercible_to(right.ty, local_type) {
                    self.diagnostics.push(
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
                        }
                        .report(&self),
                    );
                }

                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: UNIT_IDX,
                    span: expr.span,
                })
            }
            ExprKind::Global { global_index } => {
                let global = self
                    .declared_globals
                    .get_mut(global_index as usize)
                    .unwrap();

                if !self.type_pool.get(global.ty.inner).is_primitive() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.file_id,
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
                            file_id: self.file_id,
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
                if right.ty == UNKNOWN_IDX {
                    self.coerce_untyped_expr(&mut right, global_type)?;
                } else if !self.type_pool.coercible_to(right.ty, global_type) {
                    self.diagnostics.push(
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
                        }
                        .report(&self),
                    );
                }

                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: UNIT_IDX,
                    span: expr.span,
                })
            }
            _ => {
                self.diagnostics.push(
                    InvalidAssignmentTargetDiagnostic {
                        file_id: self.file_id,
                        span: left.span,
                    }
                    .report(),
                );

                Ok(Expression {
                    kind: ExprKind::Error,
                    ty: UNIT_IDX,
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
                    if value.ty == UNKNOWN_IDX {
                        self.coerce_untyped_expr(&mut value, inferred_type)?;
                    }

                    match scope.expected_type {
                        Some(expected_type)
                            if !self.type_pool.coercible_to(inferred_type, expected_type) =>
                        {
                            self.diagnostics.push(
                                TypeMistmatchDiagnostic {
                                    file_id: self.file_id,
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
                        ty: NEVER_IDX,
                        span: expr.span,
                    })
                })
                .unwrap_or(Expression {
                    kind: ExprKind::Unreachable,
                    ty: NEVER_IDX,
                    span: expr.span,
                })),
            None => {
                let scope = ctx.frame.scopes.get_mut(ctx.scope_index as usize).unwrap();

                let inferred_type = scope.inferred_type.unwrap_or(UNIT_IDX);
                scope.inferred_type = Some(inferred_type);

                match scope.expected_type {
                    Some(expected_type)
                        if self.type_pool.coercible_to(inferred_type, expected_type) =>
                    {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.file_id,
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
                    ty: NEVER_IDX,
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
                expected_type: match *self.type_pool.get(left.ty) {
                    Type::Unknown | Type::Error | Type::Never | Type::Unit => {
                        access_ctx.expected_type
                    }
                    _ => Some(left.ty),
                },
                access_kind: AccessKind::Read,
            },
        )?;

        match (left.ty, right.ty) {
            (l, r) if l == UNKNOWN_IDX && r == UNKNOWN_IDX => match access_ctx.expected_type {
                Some(_) => Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: UNKNOWN_IDX,
                    span: expr.span,
                }),
                None => {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                    Err(())
                }
            },
            (l, ty) if l == UNKNOWN_IDX => {
                if !self.type_pool.get(ty).is_primitive() {
                    self.diagnostics.push(
                        BinaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.file_id,
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
                        ty: access_ctx.expected_type.unwrap_or(UNKNOWN_IDX),
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
            (ty, r) if r == UNKNOWN_IDX => {
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
            (l, _) if l == NEVER_IDX => {
                self.diagnostics.push(
                    UnreachableCodeDiagnostic {
                        file_id: self.file_id,
                        span: right.span,
                    }
                    .report(),
                );

                Ok(left)
            }
            (_, r) if r == NEVER_IDX => {
                self.diagnostics.push(
                    UnreachableCodeDiagnostic {
                        file_id: self.file_id,
                        span: operator.span,
                    }
                    .report(),
                );

                Ok(right)
            }
            (left_type, right_type)
                if left_type == right_type && self.type_pool.get(left_type).is_primitive() =>
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
                self.diagnostics.push(
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

    fn build_call_arguments(
        &mut self,
        ctx: &mut FunctionContext,
        arguments: &[Separated<Spanned<ast::Expression>>],
        params: &[TypeIndex],
    ) -> Result<Box<[Expression]>, ()> {
        arguments
            .iter()
            .enumerate()
            .map(|(index, argument)| {
                let expected_type: Option<TypeIndex> = params.get(index).copied();

                let mut argument = self.build_expression(
                    ctx,
                    &argument.inner,
                    AccessContext {
                        expected_type,
                        access_kind: AccessKind::Read,
                    },
                )?;

                if let Some(expected_type) = expected_type {
                    if argument.ty == UNKNOWN_IDX {
                        self.coerce_untyped_expr(&mut argument, expected_type)?;
                    } else if !self.type_pool.coercible_to(argument.ty, expected_type) {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.file_id,
                                expected_type,
                                actual_type: argument.ty,
                                span: argument.span,
                            }
                            .report(&self),
                        );
                    }
                }

                Ok(argument)
            })
            .collect::<Result<Box<_>, _>>()
    }

    fn build_call_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &Spanned<ast::Expression>,
    ) -> Result<Expression, ()> {
        let (ast_callee, arguments) = match &expr.inner {
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
        let signature_index = match self.type_pool.get(callee.ty) {
            Type::Function { .. } => callee.ty,
            _ => {
                self.diagnostics.push(
                    CannotCallExpressionDiagnostic {
                        file_id: self.file_id,
                        span: ast_callee.span,
                        ty: callee.ty,
                    }
                    .report(&self),
                );

                return Ok(Expression {
                    kind: ExprKind::Call {
                        callee: Box::new(callee),
                        arguments: Box::new([]),
                    },
                    ty: ERROR_IDX,
                    span: expr.span,
                });
            }
        };

        let params_all = self.type_pool.function_params(signature_index).to_vec();
        let result_ty = self.type_pool.function_result(signature_index);
        match callee.kind {
            ExprKind::ObjectAccess { member, object } => {
                match self
                    .impl_members
                    .get(&object.ty)
                    .and_then(|m| m.get(&member.inner))
                    .copied()
                {
                    Some(ImplEntry::Method(func_index)) => {
                        let params = &params_all[1..];
                        if arguments.len() != params.len() {
                            self.diagnostics.push(
                                ArgumentCountMismatchDiagnostic {
                                    file_id: self.file_id,
                                    actual_count: arguments.len(),
                                    params,
                                    call_span: callee.span,
                                    is_method: true,
                                }
                                .report(&self),
                            );
                        }

                        let arguments = self.build_call_arguments(ctx, arguments, params)?;

                        Ok(Expression {
                            kind: ExprKind::MethodCall {
                                object,
                                arguments,
                                func_index,
                            },
                            ty: result_ty,
                            span: expr.span,
                        })
                    }
                    _ => todo!(
                        "report error for calling unknown member or associated function as method"
                    ),
                }
            }
            _ => {
                let params = &params_all[..];
                if arguments.len() != params.len() {
                    self.diagnostics.push(
                        ArgumentCountMismatchDiagnostic {
                            file_id: self.file_id,
                            actual_count: arguments.len(),
                            params,
                            call_span: callee.span,
                            is_method: false,
                        }
                        .report(&self),
                    );
                }

                let arguments = self.build_call_arguments(ctx, arguments, params)?;

                Ok(Expression {
                    kind: ExprKind::Call {
                        callee: Box::new(callee),
                        arguments,
                    },
                    ty: result_ty,
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
            ast::Statement::LocalDefinition {
                mut_span,
                name,
                ty,
                value,
                ..
            } => (mut_span.clone(), name.clone(), ty, value),
            _ => unreachable!(),
        };

        let expected_type = match ty {
            Some(ty) => Some(self.resolve_type(ty)),
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

        let ty: TypeIndex = match (value.ty, expected_type) {
            (v, None) if v == UNKNOWN_IDX => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.file_id,
                        span: name.span,
                    }
                    .report(),
                );
                // Use Error type for recovery - this allows later references to work
                // without cascading "undeclared identifier" errors
                ERROR_IDX
            }
            (ty, None) => ty,
            (v, Some(expected_type)) if v == UNKNOWN_IDX => {
                self.coerce_untyped_expr(&mut value, expected_type)?;
                expected_type
            }
            (actual_type, Some(expected_type)) => {
                if self.type_pool.coercible_to(actual_type, expected_type) {
                    expected_type
                } else {
                    self.diagnostics.push(
                        TypeMistmatchDiagnostic {
                            file_id: self.file_id,
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
            ty: if ty == NEVER_IDX { NEVER_IDX } else { UNIT_IDX },
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

        if target_idx == I32_IDX {
            if value > i32::MAX as i64 || value < i32::MIN as i64 {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: I32_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = I32_IDX;
            Ok(())
        } else if target_idx == I64_IDX {
            if value > i64::MAX || value < i64::MIN {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: I64_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = I64_IDX;
            Ok(())
        } else if target_idx == U32_IDX {
            if value > u32::MAX as i64 || value < 0 {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: U32_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = U32_IDX;
            Ok(())
        } else if target_idx == U64_IDX {
            // i64 is at most i64::MAX which always fits in u64; only negative values are
            // invalid
            if value < 0 {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: U64_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = U64_IDX;
            Ok(())
        } else if target_idx == U8_IDX {
            if value < 0 || value > u8::MAX as i64 {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: U8_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = U8_IDX;
            Ok(())
        } else if target_idx == I8_IDX {
            if value < i8::MIN as i64 || value > i8::MAX as i64 {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: I8_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = I8_IDX;
            Ok(())
        } else if target_idx == U16_IDX {
            if value < 0 || value > u16::MAX as i64 {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: U16_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = U16_IDX;
            Ok(())
        } else if target_idx == I16_IDX {
            if value < i16::MIN as i64 || value > i16::MAX as i64 {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: I16_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = I16_IDX;
            Ok(())
        } else if target_idx == CHAR_IDX {
            if value < 0 || value > u32::MAX as i64 {
                self.diagnostics.push(
                    IntegerLiteralOutOfRangeDiagnostic {
                        file_id: self.file_id,
                        ty: CHAR_IDX,
                        value,
                        span: expr.span,
                    }
                    .report(&self),
                );
            }
            expr.ty = CHAR_IDX;
            Ok(())
        } else if target_idx == F32_IDX || target_idx == F64_IDX {
            self.diagnostics.push(
                IntegerLiteralForFloatTypeDiagnostic {
                    file_id: self.file_id,
                    span: expr.span,
                }
                .report(),
            );
            Err(())
        } else {
            self.diagnostics.push(
                UnableToCoerceDiagnostic {
                    file_id: self.file_id,
                    target_type: target_idx,
                    span: expr.span,
                }
                .report(&self),
            );
            Err(())
        }
    }

    fn coerce_untyped_float_expr(
        &mut self,
        expr: &mut Expression,
        target_idx: TypeIndex,
    ) -> Result<(), ()> {
        if target_idx == F32_IDX {
            // TODO: add a diagnostic if the literal is out of range
            expr.ty = F32_IDX;
            Ok(())
        } else if target_idx == F64_IDX {
            // TODO: add a diagnostic if the literal is out of range
            expr.ty = F64_IDX;
            Ok(())
        } else {
            self.diagnostics.push(
                UnableToCoerceDiagnostic {
                    file_id: self.file_id,
                    target_type: target_idx,
                    span: expr.span,
                }
                .report(&self),
            );
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
                let is_valid = target_idx == I32_IDX || target_idx == I64_IDX;
                if !is_valid {
                    self.diagnostics.push(
                        UnableToCoerceDiagnostic {
                            file_id: self.file_id,
                            target_type: target_idx,
                            span: expr.span,
                        }
                        .report(&self),
                    );
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
                if !self.type_pool.get(target_idx).is_primitive() {
                    self.diagnostics.push(
                        UnableToCoerceDiagnostic {
                            file_id: self.file_id,
                            target_type: target_idx,
                            span: expr.span,
                        }
                        .report(&self),
                    );
                    return Err(());
                }
            }
            operator if operator.is_bitwise() => {
                let is_integer = target_idx == I32_IDX
                    || target_idx == I64_IDX
                    || target_idx == U32_IDX
                    || target_idx == U64_IDX;
                if !is_integer {
                    self.diagnostics.push(
                        UnableToCoerceDiagnostic {
                            file_id: self.file_id,
                            target_type: target_idx,
                            span: expr.span,
                        }
                        .report(&self),
                    );
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
            Some(GlobalValue::Struct { struct_index }) => struct_index,
            Some(_) => {
                self.diagnostics.push(
                    NotAStructTypeDiagnostic {
                        file_id: self.file_id,
                        name: self.interner.resolve(name.inner).unwrap().to_string(),
                        span: name.span,
                    }
                    .report(),
                );
                return Err(());
            }
            None => {
                self.diagnostics.push(
                    UndeclaredIdentifierDiagnostic {
                        file_id: self.file_id,
                        span: name.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        let struct_name = self
            .interner
            .resolve(self.structs[struct_index as usize].name.inner)
            .unwrap()
            .to_string();
        let field_count = self.structs[struct_index as usize].fields.len();
        // Tracks the field name span of the first mention of each field (regardless of
        // whether the value built successfully). Used for duplicate detection and to
        // distinguish genuinely-missing fields from errored ones.
        let mut first_mention: Vec<Option<ast::TextSpan>> =
            (0..field_count).map(|_| None).collect();
        let mut field_slots: Vec<Option<Expression>> = (0..field_count).map(|_| None).collect();

        for field in fields.iter() {
            let field = &field.inner.inner;
            let field_name = self.interner.resolve(field.name.inner).unwrap();

            let field_index = match self.structs[struct_index as usize]
                .lookup
                .get(&field.name.inner)
                .copied()
            {
                Some(idx) => idx,
                None => {
                    self.diagnostics.push(
                        UnknownStructFieldDiagnostic {
                            file_id: self.file_id,
                            struct_name: &struct_name,
                            field_name,
                            field_span: field.name.span,
                        }
                        .report(),
                    );
                    continue;
                }
            };

            if let Some(first_span) = first_mention[field_index] {
                self.diagnostics.push(
                    DuplicateStructFieldInitDiagnostic {
                        file_id: self.file_id,
                        field_name: &field_name,
                        first_span,
                        second_span: field.name.span,
                    }
                    .report(),
                );
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
            let expected_ty = self.structs[struct_index as usize].fields[field_index]
                .ty
                .inner;
            let mut field_expr = match self.build_expression(
                func_ctx,
                field_value,
                AccessContext {
                    expected_type: Some(expected_ty),
                    access_kind: AccessKind::Read,
                },
            ) {
                Ok(e) => e,
                Err(_) => continue,
            };

            if field_expr.ty == UNKNOWN_IDX {
                match self.coerce_untyped_expr(&mut field_expr, expected_ty) {
                    Ok(_) => {}
                    Err(_) => continue,
                }
            } else if !self.type_pool.coercible_to(field_expr.ty, expected_ty) {
                self.diagnostics.push(
                    TypeMistmatchDiagnostic {
                        file_id: self.file_id,
                        expected_type: expected_ty,
                        actual_type: field_expr.ty,
                        span: field_expr.span,
                    }
                    .report(&self),
                );
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
                    .resolve(self.structs[struct_index as usize].fields[i].name.inner)
                    .unwrap()
            })
            .collect();
        let ty = self.type_pool.intern(Type::Struct { struct_index });

        if !missing.is_empty() {
            self.diagnostics.push(
                MissingStructFieldsDiagnostic {
                    file_id: self.file_id,
                    struct_name: &struct_name,
                    missing_fields: missing,
                    init_span,
                }
                .report(),
            );
        }

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
}

/// Interning pool for `Type` values. All type construction goes through here,
/// guaranteeing deduplication and stable `TypeIndex` handles.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypePool {
    pool: Vec<Type>,
    lookup: HashMap<Type, TypeIndex>,
}

impl TypePool {
    pub fn new() -> Self {
        let mut p = TypePool {
            pool: Vec::new(),
            lookup: HashMap::new(),
        };
        // Order MUST match the IDX constants defined at the top of this file.
        p.intern_raw(Type::Error); // 0 = ERROR_IDX
        p.intern_raw(Type::Unit); // 1 = UNIT_IDX
        p.intern_raw(Type::Never); // 2 = NEVER_IDX
        p.intern_raw(Type::Unknown); // 3 = UNKNOWN_IDX
        p.intern_raw(Type::I32); // 4 = I32_IDX
        p.intern_raw(Type::I64); // 5 = I64_IDX
        p.intern_raw(Type::F32); // 6 = F32_IDX
        p.intern_raw(Type::F64); // 7 = F64_IDX
        p.intern_raw(Type::U32); // 8 = U32_IDX
        p.intern_raw(Type::U64); // 9 = U64_IDX
        p.intern_raw(Type::Bool); // 10 = BOOL_IDX
        p.intern_raw(Type::Char); // 11 = CHAR_IDX
        p.intern_raw(Type::Struct {
            struct_index: BUILTIN_STRING_STRUCT,
        }); // 12 = STRING_IDX
        p.intern_raw(Type::U8); // 13 = U8_IDX
        p.intern_raw(Type::I8); // 14 = I8_IDX
        p.intern_raw(Type::U16); // 15 = U16_IDX
        p.intern_raw(Type::I16); // 16 = I16_IDX
        p
    }

    fn intern_raw(&mut self, ty: Type) -> TypeIndex {
        let idx = self.pool.len() as TypeIndex;
        self.lookup.insert(ty.clone(), idx);
        self.pool.push(ty);
        idx
    }

    pub fn intern(&mut self, ty: Type) -> TypeIndex {
        if let Some(&idx) = self.lookup.get(&ty) {
            return idx;
        }
        self.intern_raw(ty)
    }

    pub fn get(&self, idx: TypeIndex) -> &Type {
        &self.pool[idx as usize]
    }

    pub fn as_slice(&self) -> &[Type] {
        &self.pool
    }

    /// Returns `true` if a value of type `a` can be coerced to type `b`.
    pub fn coercible_to(&self, a: TypeIndex, b: TypeIndex) -> bool {
        a == b || a == NEVER_IDX || a == ERROR_IDX || b == ERROR_IDX
    }

    /// Unified type of `a` and `b`, or `Err` if they are incompatible.
    pub fn unify(&self, a: TypeIndex, b: TypeIndex) -> Result<TypeIndex, ()> {
        if a == b {
            return Ok(a);
        }
        if a == NEVER_IDX {
            return Ok(b);
        }
        if b == NEVER_IDX {
            return Ok(a);
        }
        if a == ERROR_IDX || b == ERROR_IDX {
            return Ok(ERROR_IDX);
        }
        Err(())
    }

    /// Returns the parameter types of a function type (excludes the result).
    pub fn function_params(&self, func_type_idx: TypeIndex) -> &[TypeIndex] {
        match self.get(func_type_idx) {
            Type::Function {
                items,
                params_count,
            } => &items[..*params_count],
            _ => panic!("not a function type"),
        }
    }

    /// Returns the result type index of a function type.
    pub fn function_result(&self, func_type_idx: TypeIndex) -> TypeIndex {
        match self.get(func_type_idx) {
            Type::Function {
                items,
                params_count,
            } => items[*params_count],
            _ => panic!("not a function type"),
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TIR {
    pub file_id: ast::FileId,
    pub type_pool: Vec<Type>,
    #[cfg_attr(test, serde(serialize_with = "serialize_sorted_map"))]
    pub defined_functions: HashMap<FunctionIndex, Function>,
    #[cfg_attr(test, serde(serialize_with = "serialize_sorted_map"))]
    pub defined_globals: HashMap<GlobalIndex, Global>,
    pub namespaces: Vec<Namespace>,
    pub exports: Vec<ExportItem>,
    pub declared_functions: Vec<DeclaredFunction>,
    pub declared_globals: Vec<DeclaredGlobal>,
    pub diagnostics: Vec<Diagnostic<FileId>>,
    pub structs: Vec<Struct>,
    #[cfg_attr(test, serde(serialize_with = "serialize_sorted_nested_map"))]
    pub impl_members: HashMap<TypeIndex, HashMap<SymbolU32, ImplEntry>>,
}

#[derive(Clone, Copy)]
#[repr(u32)]
pub enum PointerSize {
    /// 32-bit addressing.
    P32 = 4,
    /// 64-bit addressing.
    P64 = 8,
}

/// Memory layout of a type: size in bytes and required alignment in bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Layout {
    pub size: u32,
    pub align: u32,
}

impl Layout {
    pub const ZERO: Layout = Layout { size: 0, align: 1 };

    fn ptr(ptr_size: PointerSize) -> Layout {
        let n = ptr_size as u32;
        Layout { size: n, align: n }
    }

    fn pad_to_align(self) -> Self {
        Layout {
            size: (self.size + self.align - 1) & !(self.align - 1),
            align: self.align,
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Namespace {
    Enum(Enum),
    ImportModule(ImportModule),
}

/// Compute the memory layout (size and alignment) of a type.
///
/// Struct fields are laid out in declaration order with C-style alignment
/// padding, and the struct's total size is rounded up to its own alignment.
///
/// Panics on `Error`, `Unknown`, `Namespace`, or `Function` — these have no
/// runtime representation.
pub fn compute_layout(
    types: &[Type],
    structs: &[Struct],
    idx: TypeIndex,
    ptr_size: PointerSize,
) -> Layout {
    match &types[idx as usize] {
        Type::Unit | Type::Never => Layout::ZERO,
        Type::I32 | Type::U32 | Type::F32 | Type::Char => Layout { size: 4, align: 4 },
        Type::U8 | Type::I8 => Layout { size: 1, align: 1 },
        Type::U16 | Type::I16 => Layout { size: 2, align: 2 },
        Type::Bool => Layout { size: 1, align: 1 },
        Type::I64 | Type::U64 | Type::F64 => Layout { size: 8, align: 8 },
        Type::Pointer { .. } => Layout::ptr(ptr_size),
        // Slice is a fat pointer: (data_ptr, length)
        Type::Slice { .. } => {
            let p = ptr_size as u32;
            Layout {
                size: p * 2,
                align: p,
            }
        }
        Type::Array { of, size, .. } => {
            let elem = compute_layout(types, structs, *of, ptr_size).pad_to_align();
            Layout {
                size: elem.size * size,
                align: elem.align,
            }
        }
        Type::Tuple { elements } => {
            let indices: Box<[TypeIndex]> = elements.clone();
            compute_aggregate_layout(types, structs, &indices, ptr_size)
        }
        Type::Struct { struct_index } => {
            let si = *struct_index;
            if si == BUILTIN_STRING_STRUCT {
                let p = ptr_size as u32;
                return Layout {
                    size: p * 2,
                    align: p,
                };
            }
            let field_types: Box<[TypeIndex]> = structs[si as usize]
                .fields
                .iter()
                .map(|f| f.ty.inner)
                .collect();
            compute_aggregate_layout(types, structs, &field_types, ptr_size)
        }
        Type::Function { .. } => Layout::ptr(ptr_size),
        Type::Error | Type::Unknown | Type::Namespace { .. } => {
            panic!("compute_layout called on non-value type")
        }
    }
}

/// C-style aggregate layout for a sequence of field types.
pub fn compute_aggregate_layout(
    types: &[Type],
    structs: &[Struct],
    fields: &[TypeIndex],
    ptr_size: PointerSize,
) -> Layout {
    let mut size: u32 = 0;
    let mut align: u32 = 1;
    for &field_idx in fields {
        let field = compute_layout(types, structs, field_idx, ptr_size);
        size = (size + field.align - 1) & !(field.align - 1);
        size += field.size;
        align = align.max(field.align);
    }
    size = (size + align - 1) & !(align - 1);
    Layout { size, align }
}

impl TIR {
    /// Compute the memory layout (size and alignment) of the type at `idx`.
    pub fn layout_of(&self, idx: TypeIndex, ptr_size: PointerSize) -> Layout {
        compute_layout(&self.type_pool, &self.structs, idx, ptr_size)
    }

    /// Size of the type in bytes.
    pub fn size_of(&self, idx: TypeIndex, ptr_size: PointerSize) -> u32 {
        self.layout_of(idx, ptr_size).size
    }

    /// Required alignment of the type in bytes.
    pub fn align_of(&self, idx: TypeIndex, ptr_size: PointerSize) -> u32 {
        self.layout_of(idx, ptr_size).align
    }

    /// Build TIR from one or more ASTs processed in order.
    ///
    /// Pass stdlib ASTs before user ASTs. All items from every AST go through
    /// the define pass first, then the build pass, so forward references across
    /// files work as expected.
    ///
    /// `TIR::file_id` is set to the last AST's file ID (the primary user file).
    pub fn build(asts: &[&ast::AST], interner: &mut ast::StringInterner) -> TIR {
        assert!(!asts.is_empty(), "TIR::build requires at least one AST");

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
            file_id: asts[0].file_id,
            interner,
            defined_globals: HashMap::new(),
            exports: HashMap::new(),
            defined_functions: HashMap::new(),
            diagnostics: Vec::new(),
            declared_functions: Vec::new(),
            declared_globals: Vec::new(),
            declared_consts: Vec::new(),
            const_pool: Vec::new(),
            namespaces: Vec::new(),
            type_pool: TypePool::new(),
            symbol_lookup,
            impl_members: HashMap::new(),
            ptr_size: PointerSize::P32,
            structs: Vec::new(),
        };

        for ast in asts.iter() {
            builder.file_id = ast.file_id;
            for item in ast.items.iter() {
                let _ = builder.define_item(&item.inner.inner);
            }
        }

        // Validate that stdlib defined the built-in types at the expected indices.
        assert_eq!(
            builder.structs.get(BUILTIN_STRING_STRUCT as usize).map(|s| s.name.inner),
            Some(builder.interner.get_or_intern("string")),
            "stdlib must define `string` as the first struct (BUILTIN_STRING_STRUCT = {})",
            BUILTIN_STRING_STRUCT,
        );

        for ast in asts.iter() {
            builder.file_id = ast.file_id;
            for item in ast.items.iter() {
                let _ = builder.build_item(&item.inner.inner);
            }
        }

        builder.report_unused_items();

        let primary_file_id = asts.last().unwrap().file_id;
        TIR {
            file_id: primary_file_id,
            type_pool: builder.type_pool.pool,
            defined_functions: builder.defined_functions,
            defined_globals: builder.defined_globals,
            namespaces: builder.namespaces,
            exports: builder.exports.into_values().collect(),
            declared_functions: builder.declared_functions,
            declared_globals: builder.declared_globals,
            diagnostics: builder.diagnostics,
            structs: builder.structs,
            impl_members: builder.impl_members,
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
        interner: ast::StringInterner,
        files: Files,
        ast: AST,
        tir: TIR,
    }

    impl<'case> TestCase {
        fn new(source: &str) -> Self {
            let mut interner = ast::StringInterner::new();
            let mut files = Files::new();

            let stdlib_id = files
                .add(crate::STDLIB_FILENAME.to_string(), crate::STDLIB_SOURCE.to_string())
                .unwrap();
            let stdlib_ast = ast::Parser::parse(
                stdlib_id,
                &files.get(stdlib_id).unwrap().source,
                &mut interner,
            );

            let file_id = files
                .add("main.wx".to_string(), source.to_string())
                .unwrap();
            let ast =
                ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

            let tir = TIR::build(&[&stdlib_ast, &ast], &mut interner);

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

    /// Using an undeclared name in struct init position should produce an
    /// error.
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

    // ── Multi-file / stdlib integration tests ────────────────────────────────

    /// `string` and `char` defined in std.wx must be available in user code.
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
            case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
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
            case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    /// `pub fn` methods defined in std.wx must be callable from user code
    /// without producing unused-function warnings.
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
            case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
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
            case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
        // u32 is 4 bytes, aligned to 4
        let size_sym = case.interner.get("SIZE").unwrap();
        let align_sym = case.interner.get("ALIGN").unwrap();
        let u32_idx = crate::tir::U32_IDX;
        let members = case.tir.impl_members.get(&u32_idx).unwrap();
        assert!(matches!(members[&size_sym], ImplEntry::AssociatedConst { value: 4, .. }));
        assert!(matches!(members[&align_sym], ImplEntry::AssociatedConst { value: 4, .. }));
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
            case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
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
        let mixed_ti = case.tir.type_pool.iter().position(|t| {
            if let crate::tir::Type::Struct { struct_index } = t {
                case.tir.structs[*struct_index as usize].name.inner == mixed_sym
            } else {
                false
            }
        }).unwrap() as u32;

        let layout = case.tir.layout_of(mixed_ti, crate::tir::PointerSize::P32);
        // Optimal order: i64(8), f64(8), u32(4), bool(1) + 3B padding = 24B
        assert_eq!(layout.size, 24, "expected 24B after reordering, got {}", layout.size);
        assert_eq!(layout.align, 8);

        // Verify physical field order in the struct
        let struct_index = case.tir.type_pool.iter().find_map(|t| {
            if let crate::tir::Type::Struct { struct_index } = t {
                if case.tir.structs[*struct_index as usize].name.inner == mixed_sym {
                    Some(*struct_index)
                } else { None }
            } else { None }
        }).unwrap();
        let s = &case.tir.structs[struct_index as usize];
        let field_names: Vec<&str> = s.fields.iter()
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
            case.tir.diagnostics.iter().any(|d| d.message == "function `unused` is never used"),
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
            case.tir.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

}
