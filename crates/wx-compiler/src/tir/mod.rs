use std::collections::HashMap;
use std::hash::Hash;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use string_interner::symbol::SymbolU32;

use crate::ast::{self, DefId, Separated, Spanned, TextSpan};
use crate::vfs::{CompilationGraph, FileId};

mod builder;
#[cfg(test)]
mod tests;

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq)]
pub struct SourceSpan {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl SourceSpan {
    pub const fn new(file_id: FileId, span: TextSpan) -> Self {
        Self { file_id, span }
    }

    fn primary_label(self) -> Label<FileId> {
        Label::primary(self.file_id, self.span)
    }

    fn secondary_label(self) -> Label<FileId> {
        Label::secondary(self.file_id, self.span)
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    items: Box<[TypeIndex]>,
    params_count: usize,
}

impl FunctionSignature {
    pub fn params(&self) -> &[TypeIndex] {
        &self.items[..self.params_count]
    }

    pub fn result(&self) -> TypeIndex {
        self.items.get(self.params_count).copied().unwrap()
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeParamOwner {
    Function(DefId),
    Struct(DefId),
    /// `Self` type parameter implicit in trait items (consts, assoc types).
    Trait(TraitIndex),
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Error,
    Unit,
    Never,
    Integer,
    Float,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    F32,
    F64,
    Bool,
    Char,
    Tuple {
        elements: Box<[TypeIndex]>,
    },
    Struct {
        struct_index: u32,
        /// Empty for non-generic structs; populated when instantiated with
        /// concrete type args, e.g. `Point<i32>` → `args = [i32_idx]`.
        args: Box<[TypeIndex]>,
    },
    Function {
        signature: FunctionSignature,
    },
    /// Named function reference before coercion to a fn pointer. `type_args` is
    /// empty for uninstantiated generics.
    FunctionItem {
        id: DefId,
        type_args: Box<[TypeIndex]>,
    },
    Pointer {
        to: TypeIndex,
        mutable: bool,
        memory: TypeIndex,
    },
    Array {
        of: TypeIndex,
        size: u32,
        mutable: bool,
        memory: TypeIndex,
    },
    Slice {
        of: TypeIndex,
        mutable: bool,
        memory: TypeIndex,
    },
    Module {
        namespace_idx: u32,
    },
    Enum {
        enum_index: u32,
    },
    Memory {
        id: DefId,
        kind: MemoryKind,
    },
    Trait {
        trait_index: u32,
    },
    /// Index into `Function::type_params`. All uses of the same param in a
    /// function share one interned instance.
    TypeParam {
        owner: TypeParamOwner,
        param_index: u32,
    },
    /// A closed compile-time set of concrete types used as a bound.
    TypeSet {
        typeset_index: TypesetIndex,
    },
    /// `M::Size` — opaque until monomorphisation substitutes `M`.
    AssociatedType {
        trait_index: TraitIndex,
        assoc_name: SymbolU32,
    },
    /// `M::Size` in a function signature: a projection carrying the owning type
    /// param's index so `substitute_type` resolves it in a single pass.
    AssocTypeProjection {
        trait_index: TraitIndex,
        assoc_name: SymbolU32,
        param_index: u32,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize, PartialOrd, Ord))]
pub struct TypeIndex(u32);

impl TypeIndex {
    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0
    }

    #[inline]
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }

    #[inline]
    pub fn is_comptime_number(self) -> bool {
        self == TypeIndex::INTEGER || self == TypeIndex::FLOAT
    }

    #[inline]
    pub fn is_primitive(self) -> bool {
        self == TypeIndex::U8
            || self == TypeIndex::I8
            || self == TypeIndex::U16
            || self == TypeIndex::I16
            || self == TypeIndex::U32
            || self == TypeIndex::I32
            || self == TypeIndex::U64
            || self == TypeIndex::I64
            || self == TypeIndex::F32
            || self == TypeIndex::F64
            || self == TypeIndex::CHAR
    }

    #[inline]
    pub fn is_integer(self) -> bool {
        self == TypeIndex::U8
            || self == TypeIndex::I8
            || self == TypeIndex::U16
            || self == TypeIndex::I16
            || self == TypeIndex::U32
            || self == TypeIndex::I32
            || self == TypeIndex::U64
            || self == TypeIndex::I64
    }

    #[inline]
    pub fn is_float(self) -> bool {
        self == TypeIndex::F32 || self == TypeIndex::F64
    }

    #[inline]
    pub fn is_numeric(self) -> bool {
        self.is_integer() || self.is_float()
    }

    // Pre-allocated indices for primitive types. The `TypePool` reserves these
    // slots at startup so comparisons like `ty == TypeIndex::U32` work without
    // a pool lookup.
    pub const ERROR: TypeIndex = TypeIndex(0);
    pub const UNIT: TypeIndex = TypeIndex(1);
    pub const NEVER: TypeIndex = TypeIndex(2);
    pub const INTEGER: TypeIndex = TypeIndex(3);
    pub const FLOAT: TypeIndex = TypeIndex(4);
    pub const U8: TypeIndex = TypeIndex(5);
    pub const I8: TypeIndex = TypeIndex(6);
    pub const U16: TypeIndex = TypeIndex(7);
    pub const I16: TypeIndex = TypeIndex(8);
    pub const U32: TypeIndex = TypeIndex(9);
    pub const I32: TypeIndex = TypeIndex(10);
    pub const U64: TypeIndex = TypeIndex(11);
    pub const I64: TypeIndex = TypeIndex(12);
    pub const F32: TypeIndex = TypeIndex(13);
    pub const F64: TypeIndex = TypeIndex(14);
    pub const BOOL: TypeIndex = TypeIndex(15);
    pub const CHAR: TypeIndex = TypeIndex(16);
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
            "never" => Ok(Type::Never),
            _ => Err(()),
        }
    }
}

pub type LocalIndex = u32;
pub type ScopeIndex = u32;
pub type FunctionIndex = u32;
pub type GlobalIndex = u32;
pub type ConstIndex = u32;
pub type NamespaceIndex = u32;
pub type MemoryIndex = u32;
pub type EnumVariantIndex = u32;
pub type TraitIndex = u32;
pub type TraitImplIndex = u32;
pub type TypesetIndex = u32;

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Constant {
    pub id: ast::DefId,
    pub file_id: FileId,
    pub namespace: Option<NamespaceIndex>,
    pub pub_span: Option<ast::TextSpan>,
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<TypeIndex>,
    pub value: Option<Box<Expression>>,
    pub accesses: Vec<SourceSpan>,
}

pub struct TraitAssocType {
    pub id: ast::DefId,
    pub name_span: ast::TextSpan,
    pub bounds: Box<[TraitIndex]>,
    pub typeset_bound: Option<TypesetIndex>,
    pub accesses: Vec<SourceSpan>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Trait {
    pub file_id: FileId,
    pub namespace: Option<NamespaceIndex>,
    pub name: ast::Spanned<SymbolU32>,
    pub supertraits: Vec<TraitIndex>,
    #[cfg_attr(test, serde(serialize_with = "crate::testing::serialize_sorted_map"))]
    pub members: HashMap<SymbolU32, ImplEntry>,
    #[cfg_attr(test, serde(skip))]
    pub assoc_types: HashMap<SymbolU32, TraitAssocType>,
    /// Used to demand-resolve members before reading `members`.
    #[cfg_attr(test, serde(skip))]
    pub member_def_ids: Vec<ast::DefId>,
    /// E.g. `trait Foo: Bar<Assoc = u32>` → {(Bar_idx, "Assoc") → u32}.
    #[cfg_attr(test, serde(skip))]
    pub supertrait_bindings: HashMap<(TraitIndex, SymbolU32), TypeIndex>,
    #[cfg_attr(test, serde(skip))]
    pub accesses: Vec<SourceSpan>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TraitImpl {
    pub trait_index: TraitIndex,
    pub target: TypeIndex,
    pub members: HashMap<SymbolU32, ImplEntry>,
    /// Span of the trait name in the header; anchors conformance diagnostics.
    #[cfg_attr(test, serde(skip))]
    pub span: TextSpan,
    pub file_id: FileId,
}

/// The intersection of representable value ranges across a set of integer types.
///
/// `min` is stored as the two's-complement bit pattern of an `i64` (reinterpret with `as i64`).
/// `max` is stored as a plain `u64` upper bound.
///
/// Use [`IntegerRange::contains`] to test whether a literal fits; use
/// [`IntegerRange::intersect`] to narrow two ranges to their overlap.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct IntegerRange {
    min: u64,
    max: u64,
}

impl IntegerRange {
    /// Returns the range for a single integer primitive type, or `None` if `ty` is not one.
    pub fn for_integer_type(ty: TypeIndex) -> Option<Self> {
        if ty == TypeIndex::I8 {
            Some(Self { min: i8::MIN as i64 as u64, max: i8::MAX as u64 })
        } else if ty == TypeIndex::U8 {
            Some(Self { min: 0, max: u8::MAX as u64 })
        } else if ty == TypeIndex::I16 {
            Some(Self { min: i16::MIN as i64 as u64, max: i16::MAX as u64 })
        } else if ty == TypeIndex::U16 {
            Some(Self { min: 0, max: u16::MAX as u64 })
        } else if ty == TypeIndex::I32 {
            Some(Self { min: i32::MIN as i64 as u64, max: i32::MAX as u64 })
        } else if ty == TypeIndex::U32 {
            Some(Self { min: 0, max: u32::MAX as u64 })
        } else if ty == TypeIndex::I64 {
            Some(Self { min: i64::MIN as u64, max: i64::MAX as u64 })
        } else if ty == TypeIndex::U64 {
            Some(Self { min: 0, max: u64::MAX })
        } else {
            None
        }
    }

    /// The widest possible range — the identity element for [`intersect`](Self::intersect).
    pub fn widest() -> Self {
        Self { min: i64::MIN as u64, max: u64::MAX }
    }

    /// Narrows this range to the overlap with `other` (greatest lower bound, least upper bound).
    pub fn intersect(self, other: Self) -> Self {
        let min = if (self.min as i64) >= (other.min as i64) { self.min } else { other.min };
        let max = self.max.min(other.max);
        Self { min, max }
    }

    /// Returns `true` if the i64 `value` falls within this range.
    pub fn contains(&self, value: i64) -> bool {
        if value < 0 {
            value >= (self.min as i64)
        } else {
            (value as u64) <= self.max
        }
    }

    pub fn min_i64(&self) -> i64 {
        self.min as i64
    }

    pub fn max_u64(&self) -> u64 {
        self.max
    }
}

/// A closed compile-time set of concrete types, used as a type param bound.
/// `typeset Integer { u8, i8, u16, i16, u32, i32, u64, i64 }`
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypeSet {
    pub id: ast::DefId,
    pub file_id: FileId,
    pub name: ast::Spanned<SymbolU32>,
    pub pub_span: Option<ast::TextSpan>,
    pub members: Box<[TypeIndex]>,
    /// Intersection of the representable ranges of all member types.
    /// Integer literals inside generic bodies bounded by this typeset are
    /// validated against this range at TIR time (before monomorphization).
    pub intersection_range: IntegerRange,
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
        id: DefId,
    },
    Function {
        id: DefId,
    },
    Memory {
        id: DefId,
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
    Return {
        value: Option<Box<Expression>>,
    },
    EnumVariant {
        enum_index: u32,
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
    /// `type_args[i]` = concrete type substituted for `TypeParam { param_index:
    /// i }`.
    GenericCall {
        id: DefId,
        type_args: Box<[TypeIndex]>,
        arguments: Box<[Expression]>,
    },
    /// `type_args[0]` = Self (receiver), `type_args[1..]` = explicit generics.
    GenericMethodCall {
        id: DefId,
        type_args: Box<[TypeIndex]>,
        arguments: Box<[Expression]>,
    },
    MethodCall {
        arguments: Box<[Expression]>,
        id: ast::DefId,
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
        namespace: ast::Spanned<TypeIndex>,
        member: Box<Expression>,
    },
    Const {
        id: ast::DefId,
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
    Deref {
        pointer: Box<Expression>,
        memory: TypeIndex,
    },
    StructInit {
        struct_index: u32,
        fields: Box<[Expression]>,
    },
    TupleInit {
        elements: Box<[Expression]>,
    },
    IntrinsicCall {
        name: SymbolU32,
        type_args: Box<[TypeIndex]>,
        arguments: Box<[Expression]>,
    },
    /// `[a, b, c]` — all elements are compile-time constants; placed in static data.
    ArrayLiteral {
        elements: Box<[Expression]>,
        memory: TypeIndex,
    },
    /// `[value; count]` — repeat form; placed in static data.
    ArrayRepeat {
        value: Box<Expression>,
        count: u32,
        memory: TypeIndex,
    },
    /// `object[index]`
    Index {
        object: Box<Expression>,
        index: Box<Expression>,
        memory: TypeIndex,
    },
    /// `object[start..end]` — exclusive slice range.
    /// `None` means the bound was omitted: `start = None` is `0`, `end = None`
    /// is the object's length.  MIR fills these in during lowering.
    SliceRange {
        object: Box<Expression>,
        start: Option<Box<Expression>>,
        end: Option<Box<Expression>>,
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
    /// Type inferred from `break`, not the final expression.
    Loop,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct BlockLabel {
    pub name: SymbolU32,
    pub span: ast::TextSpan,
    pub accesses: Vec<ast::TextSpan>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct BlockScope {
    pub kind: BlockKind,
    pub label: Option<BlockLabel>,
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
#[derive(Clone)]
pub struct FunctionParam {
    pub mut_span: Option<ast::TextSpan>,
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<TypeIndex>,
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
#[derive(Clone)]
pub enum ExportItem {
    Function {
        internal_name: Spanned<SymbolU32>,
        external_name: Option<Spanned<SymbolU32>>,
        id: DefId,
    },
    Global {
        internal_name: Spanned<SymbolU32>,
        external_name: Option<Spanned<SymbolU32>>,
        id: DefId,
    },
    Memory {
        internal_name: Spanned<SymbolU32>,
        external_name: Option<Spanned<SymbolU32>>,
        id: DefId,
    },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Enum {
    pub file_id: FileId,
    pub namespace: Option<NamespaceIndex>,
    pub pub_span: Option<ast::TextSpan>,
    pub name: ast::Spanned<SymbolU32>,
    pub ty: TypeIndex,
    pub variants: Box<[EnumVariant]>,
    #[cfg_attr(test, serde(serialize_with = "crate::testing::serialize_sorted_map"))]
    pub lookup: HashMap<SymbolU32, EnumVariantIndex>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct EnumVariant {
    pub name: ast::Spanned<SymbolU32>,
    pub value: Box<Expression>,
    pub accesses: Vec<SourceSpan>,
}

#[derive(Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum SymbolKind {
    Enum {
        enum_index: u32,
    },
    Struct {
        struct_index: u32,
    },
    Module {
        namespace_idx: u32,
    },
    Memory {
        memory_index: u32,
        kind: MemoryKind,
    },
    Trait {
        trait_index: u32,
    },
    TypeSet {
        typeset_index: TypesetIndex,
    },
    Global {
        global_index: GlobalIndex,
    },
    Function {
        func_index: FunctionIndex,
    },
    Const {
        const_index: ConstIndex,
    },
    True,
    False,
    Unreachable,
    Placeholder,
    /// Resolved form of a trait associated type (`type Size`). Replaces
    /// `Pending` in the symbol lookup after `ensure_signature` processes the
    /// declaration, so bare uses of `Size` as a type identifier don't stall.
    TraitAssocType {
        trait_index: TraitIndex,
        assoc_name: SymbolU32,
    },
    /// Registered during pre-scan but not yet resolved; replaced by the real
    /// kind when `ensure_signature` runs for this `DefId`.
    Pending(ast::DefId),
}

#[derive(Clone, Copy)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImportValue {
    Function { id: DefId },
    Global { id: DefId },
    Memory { id: DefId },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Memory {
    pub id: DefId,
    pub file_id: FileId,
    pub name: ast::Spanned<SymbolU32>,
    pub kind: MemoryKind,
    pub min_pages: Option<u32>,
    pub max_pages: Option<u32>,
}

/// Back-pointer to whichever declaration created this namespace.
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ModuleDeclarationKind {
    /// Index into `TIR::module_decls`.
    Module(u32),
    /// Index into `TIR::import_decls`.
    Import(u32),
}

/// The symbol table for a module namespace — shared concept for both local
/// modules (`module foo;` / `module foo { }`) and import blocks (`import "env" { }`).
#[cfg_attr(test, derive(serde::Serialize))]
pub struct ModuleNamespace {
    pub name: SymbolU32,
    /// `None` when the parent is the root namespace (not stored in `TIR::namespaces`).
    pub parent: Option<NamespaceIndex>,
    pub declaration: ModuleDeclarationKind,
    #[cfg_attr(test, serde(serialize_with = "crate::testing::serialize_sorted_map"))]
    pub symbols: HashMap<(SymbolNamespace, SymbolU32), SymbolKind>,
}

/// Declaration-site metadata for a locally-defined module (`module foo;` / `module foo { }`).
#[cfg_attr(test, derive(serde::Serialize))]
pub struct ModuleDecl {
    /// Index into `TIR::namespaces` for this module's symbol table.
    pub namespace_idx: NamespaceIndex,
    /// File containing the `module foo;` or `module foo { }` declaration.
    pub declaring_file_id: FileId,
    /// File that IS this module (`foo.wx`). `None` for inline modules.
    pub own_file_id: Option<FileId>,
    pub name: ast::Spanned<SymbolU32>,
    pub pub_span: Option<ast::TextSpan>,
    pub accesses: Vec<SourceSpan>,
}

/// Declaration-site metadata for an import block (`import "env" { }`).
#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportDecl {
    /// Index into `TIR::namespaces` for this import module's symbol table.
    pub namespace_idx: NamespaceIndex,
    pub file_id: FileId,
    pub accesses: Vec<SourceSpan>,
    pub external_name: ast::Spanned<SymbolU32>,
    pub internal_name: Option<ast::Spanned<SymbolU32>>,
    /// Maps item names to imported values — used by MIR to emit the WASM import section.
    #[cfg_attr(test, serde(serialize_with = "crate::testing::serialize_sorted_map"))]
    pub lookup: HashMap<SymbolU32, ImportValue>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum SymbolNamespace {
    Type,
    Value,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FileKind {
    /// Unused-item warnings are suppressed.
    Library,
    Module,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum MemoryKind {
    Memory32,
    Memory64,
}

impl MemoryKind {
    #[inline]
    pub fn pointer_size(self) -> u32 {
        match self {
            MemoryKind::Memory32 => 4,
            MemoryKind::Memory64 => 8,
        }
    }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImplEntry {
    Method(FunctionIndex),
    AssociatedFn(FunctionIndex),
    /// Value computed from type layout during codegen.
    AssociatedConst {
        id: ast::DefId,
        ty: TypeIndex,
    },
    /// `ty` is `TypeParam` in trait declarations (a placeholder) and the
    /// concrete type in impls.
    AssociatedType {
        ty: TypeIndex,
    },
}

/// One `impl<Params> Target { ... }` block, kept for generic method dispatch.
/// `target` contains `TypeParam` refs; impl params are folded into each member
/// function's own `type_params` as well.
pub struct GenericImplBlock {
    pub type_params: Box<[TypeParamInfo]>,
    pub target: TypeIndex,
    pub members: HashMap<SymbolU32, ImplEntry>,
}

/// Outer type constructor for generic impl target types.
/// Used as part of the dispatch key so method lookup is O(1) rather than a
/// linear scan over all generic impl blocks.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GenericImplTargetKind {
    Slice,
    Pointer,
    Array,
    Struct(u32),
}

impl GenericImplTargetKind {
    /// Extracts the outer type constructor from `ty`, returning `None` for bare
    /// type params (rejected at registration) or other non-dispatchable types.
    pub fn from_type(ty: &Type) -> Option<Self> {
        match ty {
            Type::Slice { .. } => Some(Self::Slice),
            Type::Pointer { .. } => Some(Self::Pointer),
            Type::Array { .. } => Some(Self::Array),
            Type::Struct { struct_index, .. } => Some(Self::Struct(*struct_index)),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ItemAttribute {
    Inline,
    Lang(SymbolU32),
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum FunctionAccessKind {
    DirectCall,
    Reference,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionAccess {
    pub caller: Option<DefId>,
    pub kind: FunctionAccessKind,
    pub file_id: FileId,
    pub span: ast::TextSpan,
}

#[derive(PartialEq, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum FunctionKind {
    Free,
    Impl,
    Trait,
    TraitImpl { trait_impl_index: TraitImplIndex },
    Intrinsic,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypeParamInfo {
    pub name: SymbolU32,
    pub name_span: ast::TextSpan,
    pub bounds: Box<[TraitIndex]>,
    pub typeset_bound: Option<TypesetIndex>,
    pub accesses: Vec<SourceSpan>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
    pub id: DefId,
    pub file_id: FileId,
    pub namespace: Option<NamespaceIndex>,
    pub pub_span: Option<ast::TextSpan>,
    pub kind: FunctionKind,
    /// Empty = monomorphic.
    pub type_params: Box<[TypeParamInfo]>,
    pub signature_index: TypeIndex,
    pub name: ast::Spanned<SymbolU32>,
    pub params: Box<[FunctionParam]>,
    pub result: Option<Spanned<TypeIndex>>,
    pub accesses: Vec<FunctionAccess>,
    pub attributes: Box<[ItemAttribute]>,
    pub body: Option<FunctionBody>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionBody {
    pub stack: StackFrame,
    pub block: Box<Expression>,
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
        LiteralTypeMismatch => "E1006",
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
        UnknownStructField => "E1025",
        DuplicateStructFieldInit => "E1026",
        MissingStructFields => "E1027",
        CannotMutateImmutable => "W1000",
        UnusedVariable => "W1001",
        UnnecessaryMutability => "W1002",
        UnreachableCode => "W1003",
        UnusedItem => "W1004",
        MissingImportParamName => "W1005",
        MissingFunctionBody => "E1028",
        InvalidMemoryKind => "E1029",
        NamespaceUsedAsValue => "E1030",
        ExpectedTrait => "E1031",
        CyclicTypeDependency => "E1032",
        MissingTraitImplItem => "E1033",
        MissingSupertraitImpl => "E1034",
        AssociatedTypeInInherentImpl => "E1035",
        MissingEnumRepr => "E1036",
        CannotDerefNonPointer => "E1037",
        NoMemoryForPointer => "E1038",
        AmbiguousPointerMemory => "E1039",
        TypeArgCountMismatch => "E1040",
        InvalidCast => "E1041",
        IndexOnNonIndexable => "E1042",
        ArraySizeMismatch => "E1043",
        ArrayRepeatCountNotConst => "E1044",
        ArrayElementNotConst => "E1045",
        TypesetMemberNotInteger => "E1046",
        TypesetBoundViolation => "E1047",
        MultipleTypesetBounds => "E1048",
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
    pub id: DefId,
    pub file_id: FileId,
    pub namespace: Option<NamespaceIndex>,
    pub accesses: Vec<SourceSpan>,
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<TypeIndex>,
    pub pub_span: Option<ast::TextSpan>,
    pub mut_span: Option<ast::TextSpan>,
    pub value: Option<Box<ast::Spanned<Expression>>>,
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
    pub id: ast::DefId,
    pub file_id: FileId,
    pub namespace: Option<NamespaceIndex>,
    pub pub_span: Option<ast::TextSpan>,
    pub name: ast::Spanned<SymbolU32>,
    /// Empty for non-generic structs.
    pub type_params: Box<[TypeParamInfo]>,
    pub fields: Box<[StructField]>,
    #[cfg_attr(test, serde(serialize_with = "crate::testing::serialize_sorted_map"))]
    pub lookup: HashMap<SymbolU32, usize>,
    pub accesses: Vec<SourceSpan>,
}

pub struct TypeFormatter<'a> {
    tir: &'a TIR,
    interner: &'a ast::StringInterner,
    type_params: &'a [TypeParamInfo],
}

impl<'a> TypeFormatter<'a> {
    pub fn new(tir: &'a TIR, interner: &'a ast::StringInterner) -> Self {
        Self {
            tir,
            interner,
            type_params: &[],
        }
    }

    pub fn with_type_params(mut self, type_params: &'a [TypeParamInfo]) -> Self {
        self.type_params = type_params;
        self
    }

    pub fn display_type(&self, idx: TypeIndex) -> Result<String, std::fmt::Error> {
        let mut buffer = String::new();
        self.write_type(&mut buffer, idx)?;
        Ok(buffer)
    }

    fn write_type(&self, f: &mut impl std::fmt::Write, idx: TypeIndex) -> std::fmt::Result {
        match &self.tir.type_pool[idx.as_usize()] {
            Type::Integer => f.write_str("{integer}"),
            Type::Float => f.write_str("{float}"),
            Type::Error => f.write_str("{unknown}"),
            Type::Unit => f.write_str("()"),
            Type::Bool => f.write_str("bool"),
            Type::Char => f.write_str("char"),
            Type::U8 => f.write_str("u8"),
            Type::I8 => f.write_str("i8"),
            Type::U16 => f.write_str("u16"),
            Type::I16 => f.write_str("i16"),
            Type::Never => f.write_str("never"),
            Type::I32 => f.write_str("i32"),
            Type::I64 => f.write_str("i64"),
            Type::F32 => f.write_str("f32"),
            Type::F64 => f.write_str("f64"),
            Type::U32 => f.write_str("u32"),
            Type::U64 => f.write_str("u64"),
            Type::Pointer {
                to,
                mutable,
                memory,
            } => {
                self.write_type(f, *memory)?;
                f.write_str("::*")?;
                if *mutable {
                    f.write_str("mut ")?;
                }
                self.write_type(f, *to)?;
                Ok(())
            }
            Type::Slice {
                of,
                mutable,
                memory,
            } => {
                self.write_type(f, *memory)?;
                f.write_str("::[]")?;
                if *mutable {
                    f.write_str("mut ")?;
                }
                self.write_type(f, *of)?;
                Ok(())
            }
            Type::Array {
                of,
                size,
                mutable,
                memory,
            } => {
                self.write_type(f, *memory)?;
                write!(f, "::[{}]{}", size, if *mutable { "mut " } else { "" })?;
                self.write_type(f, *of)?;
                Ok(())
            }
            Type::Tuple { elements } => {
                f.write_char('(')?;
                for (i, element) in elements.iter().copied().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    self.write_type(f, element)?;
                }
                f.write_char(')')?;
                Ok(())
            }
            Type::Struct { struct_index, args } => {
                self.interner
                    .resolve(self.tir.structs[*struct_index as usize].name.inner)
                    .ok_or(std::fmt::Error)
                    .and_then(|name| f.write_str(name))?;
                if !args.is_empty() {
                    f.write_char('<')?;
                    for (i, arg) in args.iter().copied().enumerate() {
                        if i > 0 {
                            f.write_str(", ")?;
                        }
                        self.write_type(f, arg)?;
                    }
                    f.write_char('>')?;
                }
                Ok(())
            }
            Type::Enum { enum_index } => self
                .interner
                .resolve(self.tir.enums[*enum_index as usize].name.inner)
                .ok_or(std::fmt::Error)
                .and_then(|name| f.write_str(name)),
            Type::Memory { id, .. } => {
                let memory_index = self.tir.memory_index_lookup[id];
                self.interner
                    .resolve(self.tir.memories[memory_index as usize].name.inner)
                    .ok_or(std::fmt::Error)
                    .and_then(|name| f.write_str(name))
            }
            Type::Trait { trait_index } => self
                .interner
                .resolve(self.tir.traits[*trait_index as usize].name.inner)
                .ok_or(std::fmt::Error)
                .and_then(|name| f.write_str(name)),
            Type::TypeSet { typeset_index } => self
                .interner
                .resolve(self.tir.typesets[*typeset_index as usize].name.inner)
                .ok_or(std::fmt::Error)
                .and_then(|name| f.write_str(name)),
            Type::Module { namespace_idx } => self
                .interner
                .resolve(self.tir.namespaces[*namespace_idx as usize].name)
                .ok_or(std::fmt::Error)
                .and_then(|name| f.write_str(name)),
            Type::Function { signature } => {
                f.write_str("fn(")?;
                for (i, param) in signature.params().iter().copied().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    self.write_type(f, param)?;
                }
                f.write_str(") -> ")?;
                self.write_type(f, signature.result())?;
                Ok(())
            }
            Type::FunctionItem { id, .. } => {
                f.write_str("fn ")?;
                let func = &self.tir.functions[self.tir.function_index_lookup[id] as usize];
                self.interner
                    .resolve(func.name.inner)
                    .ok_or(std::fmt::Error)
                    .and_then(|name| f.write_str(name))?;
                if !func.type_params.is_empty() {
                    f.write_char('<')?;
                    for (i, param_info) in func.type_params.iter().enumerate() {
                        if i > 0 {
                            f.write_str(", ")?;
                        }
                        self.interner
                            .resolve(param_info.name)
                            .ok_or(std::fmt::Error)
                            .and_then(|name| f.write_str(name))?;
                    }
                    f.write_char('>')?;
                }
                f.write_char('(')?;
                for (i, param) in func.params.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    self.write_type(f, param.ty.inner)?;
                }
                f.write_str(") -> ")?;
                match &func.result {
                    Some(result) => self.write_type(f, result.inner)?,
                    None => f.write_str("()")?,
                };
                Ok(())
            }
            Type::TypeParam { owner, param_index } => {
                let name = match owner {
                    TypeParamOwner::Function(def_id) => {
                        let symbol = self.tir.functions
                            [self.tir.function_index_lookup[def_id] as usize]
                            .type_params[*param_index as usize]
                            .name;
                        self.interner.resolve(symbol).ok_or(std::fmt::Error)?
                    }
                    TypeParamOwner::Struct(def_id) => {
                        let symbol = self.tir.structs
                            [self.tir.struct_index_lookup[def_id] as usize]
                            .type_params[*param_index as usize]
                            .name;
                        self.interner.resolve(symbol).ok_or(std::fmt::Error)?
                    }
                    TypeParamOwner::Trait(_) => "Self",
                };
                f.write_str(name)
            }
            Type::AssociatedType {
                assoc_name,
                trait_index,
            } => {
                self.interner
                    .resolve(self.tir.traits[*trait_index as usize].name.inner)
                    .ok_or(std::fmt::Error)
                    .and_then(|trait_name| f.write_str(trait_name))?;
                f.write_str("::")?;
                self.interner
                    .resolve(*assoc_name)
                    .ok_or(std::fmt::Error)
                    .and_then(|type_name| f.write_str(type_name))?;
                Ok(())
            }
            Type::AssocTypeProjection {
                assoc_name,
                trait_index,
                param_index,
            } => {
                self.type_params
                    .get(*param_index as usize)
                    .and_then(|param_info| self.interner.resolve(param_info.name))
                    .or_else(|| {
                        self.interner
                            .resolve(self.tir.traits[*trait_index as usize].name.inner)
                    })
                    .ok_or(std::fmt::Error)
                    .and_then(|prefix| f.write_str(prefix))?;
                f.write_str("::")?;
                self.interner
                    .resolve(*assoc_name)
                    .ok_or(std::fmt::Error)
                    .and_then(|type_name| f.write_str(type_name))?;
                Ok(())
            }
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TIR {
    pub type_pool: Vec<Type>,
    pub diagnostics: Vec<Diagnostic<FileId>>,
    pub functions: Vec<Function>,
    #[cfg_attr(test, serde(skip))]
    pub function_index_lookup: HashMap<ast::DefId, FunctionIndex>,
    pub globals: Vec<Global>,
    #[cfg_attr(test, serde(skip))]
    pub global_index_lookup: HashMap<ast::DefId, GlobalIndex>,
    pub memories: Vec<Memory>,
    #[cfg_attr(test, serde(skip))]
    pub memory_index_lookup: HashMap<ast::DefId, MemoryIndex>,
    pub namespaces: Vec<ModuleNamespace>,
    pub module_decls: Vec<ModuleDecl>,
    pub import_decls: Vec<ImportDecl>,
    pub enums: Vec<Enum>,
    #[cfg_attr(test, serde(serialize_with = "crate::testing::serialize_sorted_map"))]
    pub exports: HashMap<SymbolU32, ExportItem>,
    pub structs: Vec<Struct>,
    #[cfg_attr(test, serde(skip))]
    pub struct_index_lookup: HashMap<ast::DefId, u32>,
    #[cfg_attr(
        test,
        serde(serialize_with = "crate::testing::serialize_sorted_nested_map")
    )]
    pub impl_members: HashMap<TypeIndex, HashMap<SymbolU32, ImplEntry>>,
    #[cfg_attr(test, serde(skip))]
    pub generic_impl_list: Vec<GenericImplBlock>,
    /// Dispatch index: `(outer type constructor, member name) → block index`.
    /// Populated during `ensure_signature`; enables O(1) generic method lookup.
    #[cfg_attr(test, serde(skip))]
    pub generic_impl_dispatch: HashMap<(GenericImplTargetKind, SymbolU32), usize>,
    pub traits: Vec<Trait>,
    pub trait_impls: Vec<TraitImpl>,
    #[cfg_attr(test, serde(serialize_with = "crate::testing::serialize_sorted_map"))]
    pub trait_impl_lookup: HashMap<(TypeIndex, TraitIndex), TraitImplIndex>,
    #[cfg_attr(test, serde(serialize_with = "crate::testing::serialize_sorted_map"))]
    pub type_trait_impls: HashMap<TypeIndex, Vec<TraitImplIndex>>,
    pub constants: Vec<Constant>,
    #[cfg_attr(test, serde(skip))]
    pub const_index_lookup: HashMap<ast::DefId, ConstIndex>,
    #[cfg_attr(test, serde(skip))]
    pub lang_items: HashMap<SymbolU32, ast::DefId>,
    pub typesets: Vec<TypeSet>,
    #[cfg_attr(test, serde(skip))]
    pub typeset_index_lookup: HashMap<ast::DefId, TypesetIndex>,
}

#[derive(PartialEq)]
enum WasmPrimitive {
    I32,
    I64,
    F32,
    F64,
}

impl TIR {
    pub fn formatter<'a>(&'a self, interner: &'a ast::StringInterner) -> TypeFormatter<'a> {
        TypeFormatter::new(self, interner)
    }

    pub fn is_import_namespace(&self, namespace: Option<NamespaceIndex>) -> bool {
        match namespace {
            Some(idx) => match self.namespaces[idx as usize].declaration {
                ModuleDeclarationKind::Import(_) => true,
                ModuleDeclarationKind::Module(_) => false,
            },
            None => false,
        }
    }

    #[inline]
    pub fn build(compilation: &mut CompilationGraph) -> TIR {
        builder::build(compilation)
    }

    fn type_primitive(&self, ty: TypeIndex) -> Option<WasmPrimitive> {
        match &self.type_pool[ty.as_usize()] {
            Type::Bool
            | Type::U8
            | Type::I8
            | Type::U16
            | Type::I16
            | Type::I32
            | Type::U32
            | Type::Char
            | Type::Function { .. } => Some(WasmPrimitive::I32),
            Type::Enum { enum_index } => {
                let repr_type = self.enums[*enum_index as usize].ty;
                self.type_primitive(repr_type)
            }
            Type::U64 | Type::I64 => Some(WasmPrimitive::I64),
            Type::F32 => Some(WasmPrimitive::F32),
            Type::F64 => Some(WasmPrimitive::F64),
            Type::Array { memory, .. } | Type::Pointer { memory, .. } => {
                match &self.type_pool[memory.as_usize()] {
                    Type::Memory { id, .. } => {
                        let kind = self.memories[self.memory_index_lookup[id] as usize].kind;
                        match kind {
                            MemoryKind::Memory32 => Some(WasmPrimitive::I32),
                            MemoryKind::Memory64 => Some(WasmPrimitive::I64),
                        }
                    }
                    _ => None,
                }
            }
            Type::Tuple { .. }
            | Type::AssociatedType { .. }
            | Type::AssocTypeProjection { .. }
            | Type::FunctionItem { .. }
            | Type::Struct { .. }
            | Type::Slice { .. }
            | Type::Module { .. }
            | Type::Memory { .. }
            | Type::Trait { .. }
            | Type::TypeSet { .. }
            | Type::TypeParam { .. }
            | Type::Error
            | Type::Never
            | Type::Unit
            | Type::Integer
            | Type::Float => None,
        }
    }
}
