use std::collections::HashMap;
use std::hash::Hash;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use string_interner::symbol::SymbolU32;

use crate::ast::{self, DefId, Separated, Spanned, TextSpan};
use crate::vfs::{CompilationGraph, CrateId, FileId};

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
	pub fn new(file_id: FileId, span: TextSpan) -> Self {
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
	params_count: u32,
}

impl FunctionSignature {
	pub fn params(&self) -> &[TypeIndex] {
		&self.items[..self.params_count as usize]
	}

	pub fn result(&self) -> TypeIndex {
		self.items.get(self.params_count as usize).copied().unwrap()
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
	/// Non-trait generic impl block: `impl<Params> Target { }`.
	/// Value is the index into `TIR::generic_impl_list`.
	ImplBlock(u32),
	/// `type Alias<T> = ...;` — the alias's own type parameters.
	TypeAlias(DefId),
}

/// The block a function/constant is a member of, if any — an impl block or
/// a trait declaration. `None` means the item is free-standing: callable by
/// its bare name (subject to namespace visibility). This answers a
/// different question than `TypeParamOwner`: that's about where an item's
/// *inherited* type parameters come from, and only applies when there's
/// something to inherit. A non-generic `impl Point { }` has an `ItemParent`
/// but no `TypeParamOwner` — there's nothing to inherit, but it's still a
/// member of a block that gates how the item can be referenced (`Point::new()`,
/// never bare `new()`).
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemParent {
	/// Non-generic `impl Target { }` / `impl Trait for Target { }`. `Target`
	/// is already fully resolved, so this points straight at its `TypeIndex`
	/// — the same key used in `TIR::impl_members`.
	Impl(TypeIndex),
	/// Generic `impl<Params> Target { }` / `impl<Params> Trait for Target { }`.
	/// Index into `TIR::generic_impl_list`.
	GenericImpl(u32),
	/// Trait item — a method/const declaration or default body, scoped to
	/// the trait's implicit `Self`.
	Trait(TraitIndex),
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
	Error,
	/// A type inference placeholder — written `_` in source, or injected
	/// internally when a generic type argument cannot yet be determined.
	/// Must never reach MIR or codegen; the TIR checker reports an error
	/// whenever `Infer` survives past the call site that created it.
	Infer,
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
		/// Encodes three states via length:
		///   - non-generic struct → always empty
		///   - generic struct, not yet instantiated → empty
		///   - generic struct, instantiated → one entry per type param, e.g. `Vec<i32, u8>` → `[i32_idx, u8_idx]`
		args: Box<[TypeIndex]>,
	},
	Function {
		signature: FunctionSignature,
	},
	/// Named function reference before coercion to a fn pointer.
	/// Encodes three states via length (same convention as `Struct::args`):
	///   - non-generic function → always empty
	///   - generic function, not yet instantiated → empty
	///   - generic function, instantiated → one entry per type param
	FunctionItem {
		id: DefId,
		type_args: Box<[TypeIndex]>,
	},
	Pointer {
		to: TypeIndex,
		memory: TypeIndex,
		mutable: bool,
	},
	Array {
		of: TypeIndex,
		size: u32,
		memory: TypeIndex,
		mutable: bool,
	},
	Slice {
		of: TypeIndex,
		memory: TypeIndex,
		mutable: bool,
	},
	Namespace {
		namespace_idx: u32,
	},
	Enum {
		enum_index: u32,
	},
	Memory {
		id: DefId,
		/// `TypeIndex::U32` or `TypeIndex::U64` — the memory's index type.
		kind: TypeIndex,
	},
	/// Index into `Function::type_params`. All uses of the same param in a
	/// function share one interned instance.
	TypeParam {
		owner: TypeParamOwner,
		param_index: u32,
	},
	/// `M::Size` — opaque until monomorphisation substitutes `M`.
	AssociatedType {
		trait_index: TraitIndex,
		assoc_name: SymbolU32,
	},
	/// `M::Size` or `A::M::Size` in a signature: a projection from a base type
	/// (a `TypeParam` or another `AssocTypeProjection`) resolved by `substitute_type`.
	AssocTypeProjection {
		trait_index: TraitIndex,
		assoc_name: SymbolU32,
		base: TypeIndex,
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

	/// Use wherever `INFER` acts as an absent-type sentinel and a concrete fallback is needed.
	#[inline]
	pub fn infer_or(self, other: TypeIndex) -> TypeIndex {
		if self == TypeIndex::INFER {
			other
		} else {
			self
		}
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
	pub const INFER: TypeIndex = TypeIndex(1);
	pub const UNIT: TypeIndex = TypeIndex(2);
	pub const NEVER: TypeIndex = TypeIndex(3);
	pub const INTEGER: TypeIndex = TypeIndex(4);
	pub const FLOAT: TypeIndex = TypeIndex(5);
	pub const U8: TypeIndex = TypeIndex(6);
	pub const I8: TypeIndex = TypeIndex(7);
	pub const U16: TypeIndex = TypeIndex(8);
	pub const I16: TypeIndex = TypeIndex(9);
	pub const U32: TypeIndex = TypeIndex(10);
	pub const I32: TypeIndex = TypeIndex(11);
	pub const U64: TypeIndex = TypeIndex(12);
	pub const I64: TypeIndex = TypeIndex(13);
	pub const F32: TypeIndex = TypeIndex(14);
	pub const F64: TypeIndex = TypeIndex(15);
	pub const BOOL: TypeIndex = TypeIndex(16);
	pub const CHAR: TypeIndex = TypeIndex(17);
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
pub type LabelIndex = u32;
pub type FunctionIndex = u32;
pub type GlobalIndex = u32;
pub type ConstIndex = u32;
pub type NamespaceIndex = u32;
pub type MemoryIndex = u32;
pub type EnumVariantIndex = u32;
pub type EnumIndex = u32;
pub type TraitIndex = u32;
pub type TraitImplIndex = u32;
pub type TypesetIndex = u32;

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Constant {
	pub id: DefId,
	pub file_id: FileId,
	pub namespace: Option<NamespaceIndex>,
	/// `Some` for associated consts (`impl Target { const FOO }` / trait
	/// consts) — `None` for free top-level consts. See `ItemParent`.
	pub parent: Option<ItemParent>,
	pub pub_span: Option<TextSpan>,
	pub name: ast::Spanned<SymbolU32>,
	pub ty: ast::Spanned<TypeIndex>,
	pub value: Option<Box<Expression>>,
	/// Compile-time value of `value`, if it folds — see `Builder::eval_const_expr`.
	pub const_value: Option<ConstValue>,
	pub accesses: Vec<SourceSpan>,
}

pub struct TraitAssocType {
	pub id: ast::DefId,
	pub name_span: ast::TextSpan,
	pub bounds: Bounds,
	pub accesses: Vec<SourceSpan>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Trait {
	pub id: ast::DefId,
	pub file_id: FileId,
	pub namespace: Option<NamespaceIndex>,
	pub name: ast::Spanned<SymbolU32>,
	/// The implicit `Self` type parameter owned by this trait. All trait
	/// methods inherit it via `type_param_parent = TypeParamOwner::Trait(idx)`.
	pub self_type_param: TypeParamInfo,
	pub supertraits: Vec<TraitIndex>,
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
	pub members: HashMap<SymbolU32, ImplEntry>,
	#[cfg_attr(test, serde(skip))]
	pub assoc_types: HashMap<SymbolU32, TraitAssocType>,
	/// Used to demand-resolve members before reading `members`.
	#[cfg_attr(test, serde(skip))]
	pub member_ids: Vec<ast::DefId>,
	/// E.g. `trait Foo: Bar<Assoc = u32>` → {(Bar_idx, "Assoc") → u32}.
	#[cfg_attr(test, serde(skip))]
	pub supertrait_bindings: HashMap<(TraitIndex, SymbolU32), TypeIndex>,
	#[cfg_attr(test, serde(skip))]
	pub accesses: Vec<SourceSpan>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TraitImpl {
	pub id: ast::DefId,
	pub trait_index: TraitIndex,
	pub target: TypeIndex,
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
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
			Some(Self {
				min: i8::MIN as i64 as u64,
				max: i8::MAX as u64,
			})
		} else if ty == TypeIndex::U8 {
			Some(Self {
				min: 0,
				max: u8::MAX as u64,
			})
		} else if ty == TypeIndex::I16 {
			Some(Self {
				min: i16::MIN as i64 as u64,
				max: i16::MAX as u64,
			})
		} else if ty == TypeIndex::U16 {
			Some(Self {
				min: 0,
				max: u16::MAX as u64,
			})
		} else if ty == TypeIndex::I32 {
			Some(Self {
				min: i32::MIN as i64 as u64,
				max: i32::MAX as u64,
			})
		} else if ty == TypeIndex::U32 {
			Some(Self {
				min: 0,
				max: u32::MAX as u64,
			})
		} else if ty == TypeIndex::I64 {
			Some(Self {
				min: i64::MIN as u64,
				max: i64::MAX as u64,
			})
		} else if ty == TypeIndex::U64 {
			Some(Self {
				min: 0,
				max: u64::MAX,
			})
		} else {
			None
		}
	}

	/// The widest possible range — the identity element for [`intersect`](Self::intersect).
	pub fn widest() -> Self {
		Self {
			min: i64::MIN as u64,
			max: u64::MAX,
		}
	}

	/// Narrows this range to the overlap with `other` (greatest lower bound, least upper bound).
	pub fn intersect(self, other: Self) -> Self {
		let min = if (self.min as i64) >= (other.min as i64) {
			self.min
		} else {
			other.min
		};
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
	pub namespace: Option<NamespaceIndex>,
	pub name: ast::Spanned<SymbolU32>,
	pub pub_span: Option<ast::TextSpan>,
	pub members: Box<[TypeIndex]>,
	/// Intersection of the representable ranges of all member types.
	/// Integer literals inside generic bodies bounded by this typeset are
	/// validated against this range at TIR time (before monomorphization).
	pub intersection_range: IntegerRange,
	pub accesses: Vec<SourceSpan>,
}

/// A location that lives inside linear memory (a "place" in the sense of
/// Rust's place / value distinction).  Every `Place` carries the memory it
/// belongs to and whether it was reached through a mutable pointer — both
/// propagated at build time so callers never need a recursive walk.
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Place {
	pub kind: PlaceKind,
	/// Type of the value stored at this location.
	pub ty: TypeIndex,
	/// `TypeIndex` of the `Type::Memory` this place lives in.
	pub memory: TypeIndex,
	/// `true` if the root `Deref` was through a mutable pointer.
	pub mutable: bool,
	pub span: ast::TextSpan,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum PlaceKind {
	/// `ptr.*` — the only way to enter linear memory.
	Deref { pointer: Box<Expression> },
	/// `place.field` — field projection; memory/mutable inherited from object.
	Field {
		object: Box<Place>,
		member: ast::Spanned<SymbolU32>,
	},
	/// `expr[index]` — index into an array/slice value; memory/mutable come from
	/// the expression's type, not from a parent place.
	Index {
		object: Box<Expression>,
		index: Box<Expression>,
	},
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

	StructInit {
		struct_index: u32,
		fields: Box<[Expression]>,
	},
	TupleInit {
		elements: Box<[Expression]>,
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

	/// `object[start..end]` — exclusive slice range.
	/// `None` means the bound was omitted: `start = None` is `0`, `end = None`
	/// is the object's length.  MIR fills these in during lowering.
	SliceRange {
		object: Box<Expression>,
		start: Option<Box<Expression>>,
		end: Option<Box<Expression>>,
	},
	/// Load a value from a memory place (`place.*` or `place.field`, etc.).
	Load {
		place: Box<Place>,
	},
	/// Take the address of a memory place (`.&` / `.&mut` postfix operators).
	AddressOf {
		place: Box<Place>,
		mutable: bool,
	},
	/// Store a value to a memory place (assignment through a place).
	Store {
		target: Box<Place>,
		value: Box<Expression>,
	},
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Expression {
	pub kind: ExprKind,
	pub ty: TypeIndex,
	pub span: ast::TextSpan,
}

/// The compile-time value of a constant expression (enum variant value, `const`
/// initializer). Cached alongside the built `Expression` tree rather than replacing
/// it, so the tree stays available for LSP/semantic-analysis use while codegen and
/// validation can read the value directly without re-interpreting the tree.
#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ConstValue {
	Int(i64),
	Float(f64),
	Bool(bool),
	Char(char),
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
	/// Hint for type inference at this expression site.
	/// `TypeIndex::INFER` means "no constraint" (replaces `Option::None`).
	/// A type containing `TypeIndex::INFER` (e.g. `Layout<_>`) is a partial
	/// constraint — positions marked INFER act as wildcards.
	expected_type: TypeIndex,
	access_kind: AccessKind,
}

#[derive(Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct LocalAccess {
	pub span: TextSpan,
	pub kind: AccessKind,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Local {
	pub name: Spanned<SymbolU32>,
	pub ty: TypeIndex,
	pub mut_span: Option<TextSpan>,
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
	pub name: Spanned<SymbolU32>,
	pub accesses: Vec<TextSpan>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct BlockScope {
	pub kind: BlockKind,
	pub label: Option<LabelIndex>,
	pub parent: Option<ScopeIndex>,
	pub span: TextSpan,
	pub locals: Vec<Local>,
	/// Type accumulated from `break` arms; `INFER` means nothing seen yet.
	pub inferred_type: TypeIndex,
	/// Hint from the surrounding context; `INFER` means unconstrained.
	pub expected_type: TypeIndex,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StackFrame {
	pub labels: Vec<BlockLabel>,
	pub scopes: Vec<BlockScope>,
}

impl StackFrame {
	#[inline]
	fn push_local(&mut self, scope_index: u32, local: Local) -> LocalIndex {
		let scope = &mut self.scopes[scope_index as usize];
		let local_index = scope.locals.len() as LocalIndex;
		scope.locals.push(local);
		local_index
	}

	#[inline]
	fn push_label(&mut self, label: Spanned<SymbolU32>) -> LabelIndex {
		let label_index = self.labels.len() as LabelIndex;
		self.labels.push(BlockLabel {
			name: label,
			accesses: Vec::new(),
		});
		label_index
	}

	#[inline]
	fn get_local(
		&self,
		scope_index: ScopeIndex,
		local_index: LocalIndex,
	) -> &Local {
		&self.scopes[scope_index as usize].locals[local_index as usize]
	}

	#[inline]
	fn record_local_access(
		&mut self,
		scope_index: ScopeIndex,
		local_index: LocalIndex,
		access: LocalAccess,
	) {
		self.scopes[scope_index as usize].locals[local_index as usize]
			.accesses
			.push(access);
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
	pub id: ast::DefId,
	pub file_id: FileId,
	pub namespace: Option<NamespaceIndex>,
	pub pub_span: Option<ast::TextSpan>,
	pub name: ast::Spanned<SymbolU32>,
	pub repr_type: TypeIndex,
	pub self_type: TypeIndex,
	pub variants: Box<[EnumVariant]>,
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
	pub variant_lookup: HashMap<SymbolU32, EnumVariantIndex>,
	pub accesses: Vec<SourceSpan>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct EnumVariant {
	pub name: ast::Spanned<SymbolU32>,
	pub value: Option<Box<Expression>>,
	/// Compile-time value of `value`, if it folds — see `Builder::eval_const_expr`.
	pub const_value: Option<ConstValue>,
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
		/// `TypeIndex::U32` or `TypeIndex::U64` — the memory's index type.
		kind: TypeIndex,
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
	/// Resolved form of a trait associated type (`type Size`). Replaces
	/// `Pending` in the symbol lookup after `ensure_signature` processes the
	/// declaration, so bare uses of `Size` as a type identifier don't stall.
	TraitAssocType {
		trait_index: TraitIndex,
		assoc_name: SymbolU32,
	},
	TypeAlias {
		type_alias_index: u32,
	},
	/// Registered during pre-scan but not yet resolved; replaced by the real
	/// kind when `ensure_signature` runs for this `DefId`.
	Pending(ast::DefId),
}

/// Result of resolving a single-segment name. Separates the two categories
/// of symbols: global items (registered in the symbol table) and local
/// variables (stack-scoped within a function body).
#[derive(Clone, Copy)]
pub enum ResolvedSymbol {
	Local {
		scope_index: ScopeIndex,
		local_index: LocalIndex,
	},
	Global(SymbolKind),
}

/// The kind of item found when resolving a member within a type namespace.
#[derive(Clone)]
pub enum ResolvedMember {
	Function {
		func_index: u32,
		type_args: Box<[TypeIndex]>,
	},
	Const {
		id: ast::DefId,
		ty: TypeIndex,
	},
	Global {
		global_index: u32,
	},
	EnumVariant {
		enum_index: u32,
		variant_index: u32,
	},
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
	/// `TypeIndex::U32` or `TypeIndex::U64` — the memory's index type.
	pub kind: TypeIndex,
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
	/// Top-level namespace created implicitly for a named library crate.
	/// Carries the root module's `FileId` for diagnostic spans.
	Crate(CrateId, FileId),
}

/// The symbol table for a module namespace — shared concept for both local
/// modules (`module foo;` / `module foo { }`) and import blocks (`import "env" { }`).
#[cfg_attr(test, derive(serde::Serialize))]
pub struct ModuleNamespace {
	pub name: SymbolU32,
	/// `None` when the parent is the root namespace (not stored in `TIR::namespaces`).
	pub parent: Option<NamespaceIndex>,
	pub declaration: ModuleDeclarationKind,
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
	pub symbols: HashMap<(SymbolNamespace, SymbolU32), SymbolKind>,
	/// Namespaces brought into scope via `use path::*;`.  Checked during lookup
	/// after direct symbols but before walking to the parent.
	pub wildcard_imports: Vec<NamespaceIndex>,
	/// Source spans where this namespace is referenced (e.g. path segments in
	/// `use` statements).  Used by the IDE for go-to-definition.
	pub accesses: Vec<SourceSpan>,
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
}

/// Declaration-site metadata for an import block (`import "env" { }`).
#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportDecl {
	/// Index into `TIR::namespaces` for this import module's symbol table.
	pub namespace_idx: NamespaceIndex,
	pub file_id: FileId,
	pub external_name: ast::Spanned<SymbolU32>,
	pub internal_name: Option<ast::Spanned<SymbolU32>>,
	/// Maps item names to imported values — used by MIR to emit the WASM import section.
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
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
/// This is the canonical owner of the impl-level type parameters; member
/// functions reference them via `TypeParamOwner::ImplBlock` rather than
/// storing a copy.
pub struct GenericImplBlock {
	/// Synthetic `DefId` used to demand-drive this block's `ensure_signature`.
	pub id: ast::DefId,
	pub file_id: FileId,
	pub type_params: Box<[TypeParamInfo]>,
	/// `TypeIndex::ERROR` until `ensure_signature` for this block runs.
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
	Array,
	Struct(u32),
}

impl GenericImplTargetKind {
	/// Extracts the outer type constructor from `ty`, returning `None` for bare
	/// type params, pointer types (impl on pointers is disallowed), or other
	/// non-dispatchable types.
	pub fn from_type(ty: &Type) -> Option<Self> {
		match ty {
			Type::Slice { .. } => Some(Self::Slice),
			Type::Array { .. } => Some(Self::Array),
			Type::Struct { struct_index, .. } => {
				Some(Self::Struct(*struct_index))
			}
			_ => None,
		}
	}
}

#[derive(Clone, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ItemAttribute {
	Inline,
	Intrinsic,
	Tag(SymbolU32),
}

#[derive(PartialEq, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum FunctionKind {
	Free,
	Impl,
	Trait,
	TraitImpl { trait_impl_index: TraitImplIndex },
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TraitBound {
	pub trait_index: TraitIndex,
	/// Resolved RHS types from `where { AssocType = RhsType }` bindings,
	/// sorted by assoc-type name for deterministic equality.
	pub bindings: Box<[(SymbolU32, TypeIndex)]>,
}

#[derive(Clone, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Bounds {
	pub traits: Box<[TraitBound]>,
	pub typeset: Option<TypesetIndex>,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypeParamInfo {
	pub name: SymbolU32,
	pub name_span: ast::TextSpan,
	pub bounds: Bounds,
	pub accesses: Vec<SourceSpan>,
}

impl TypeParamInfo {
	pub fn new(name: SymbolU32, name_span: ast::TextSpan) -> Self {
		Self {
			name,
			name_span,
			bounds: Bounds::default(),
			accesses: Vec::new(),
		}
	}
}

impl Function {
	/// Total number of type parameters visible to this function's body:
	/// inherited params from the parent impl block plus the function's own params.
	pub fn total_type_param_count(&self) -> usize {
		self.inherited_type_param_count + self.type_params.len()
	}
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
	pub id: DefId,
	pub file_id: FileId,
	pub namespace: Option<NamespaceIndex>,
	/// `Some` for methods/associated functions (impl or trait members) —
	/// `None` for free top-level and imported functions. See `ItemParent`.
	pub parent: Option<ItemParent>,
	pub pub_span: Option<ast::TextSpan>,
	/// Own type parameters only — does not include params inherited from a
	/// parent impl block. For the full ordered list, prepend the params from
	/// `type_param_parent`. Empty for monomorphic functions.
	pub type_params: Box<[TypeParamInfo]>,
	/// For functions inside `impl<Params> Target { }`, the impl block that
	/// owns the inherited type parameters. `None` for top-level functions.
	pub type_param_parent: Option<TypeParamOwner>,
	/// Number of type parameters inherited from `type_param_parent`.
	/// `Type::TypeParam::param_index` values for own params start at this
	/// offset; impl-block params use absolute indices starting at 0.
	pub inherited_type_param_count: usize,
	pub signature_index: TypeIndex,
	pub name: ast::Spanned<SymbolU32>,
	pub params: Box<[FunctionParam]>,
	pub result: Option<Spanned<TypeIndex>>,
	pub accesses: Vec<SourceSpan>,
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
		UnusedTypeParam => "W1006",
		UnusedStructField => "W1007",
		UnusedLabel => "W1008",
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
		MethodNotFound => "E1049",
		NotAMethod => "E1050",
		InferInSignature => "E1051",
		MissingElseBlock => "E1052",
		InvalidSelfType => "E1053",
		ContinueOutsideOfLoop => "E1054",
		EnumReprNotInteger => "E1055",
		EnumDuplicateValue => "E1056",
		NotConstEvaluatable => "E1057",
		UnusedEnumVariant => "W1009",
	}
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
	pub id: DefId,
	pub file_id: FileId,
	pub namespace: Option<NamespaceIndex>,
	pub accesses: Vec<SourceSpan>,
	pub name: Spanned<SymbolU32>,
	pub ty: Spanned<TypeIndex>,
	pub pub_span: Option<TextSpan>,
	pub mut_span: Option<TextSpan>,
	pub value: Option<FunctionBody>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum FieldAccessKind {
	Read,
	Init,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FieldAccess {
	pub kind: FieldAccessKind,
	pub file_id: FileId,
	pub span: TextSpan,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructField {
	pub name: Spanned<SymbolU32>,
	pub ty: Spanned<TypeIndex>,
	pub pub_span: Option<TextSpan>,
	pub accesses: Vec<FieldAccess>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Struct {
	pub id: DefId,
	pub file_id: FileId,
	pub namespace: Option<NamespaceIndex>,
	pub pub_span: Option<TextSpan>,
	pub name: Spanned<SymbolU32>,
	/// Empty for non-generic structs.
	pub type_params: Box<[TypeParamInfo]>,
	/// `Type::Struct { struct_index, args: [] }` for this struct.
	#[cfg_attr(test, serde(skip))]
	pub self_type: TypeIndex,
	pub fields: Box<[StructField]>,
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
	pub lookup: HashMap<SymbolU32, usize>,
	pub accesses: Vec<SourceSpan>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypeAlias {
	pub id: DefId,
	pub file_id: FileId,
	pub namespace: Option<NamespaceIndex>,
	pub pub_span: Option<TextSpan>,
	pub name: Spanned<SymbolU32>,
	/// Empty for non-generic aliases.
	pub type_params: Box<[TypeParamInfo]>,
	/// The alias's target type, fully resolved. For a generic alias this may
	/// contain `Type::TypeParam { owner: TypeParamOwner::TypeAlias(id), .. }`
	/// placeholders, substituted via `substitute_type` at each reference site
	/// that supplies concrete type arguments — the alias is transparent and
	/// never appears past TIR.
	pub template: TypeIndex,
	pub accesses: Vec<SourceSpan>,
}

pub struct TypeFormatter<'a> {
	tir: &'a TIR,
	pub interner: &'a ast::StringInterner,
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

	pub fn with_type_params(
		mut self,
		type_params: &'a [TypeParamInfo],
	) -> Self {
		self.type_params = type_params;
		self
	}

	pub fn display_kind(&self, idx: TypeIndex) -> &'static str {
		match &self.tir.types[idx.as_usize()] {
			Type::Struct { .. } => "struct",
			Type::Function { .. } | Type::FunctionItem { .. } => "function",
			Type::Enum { .. } => "enum",
			Type::F32 | Type::F64 | Type::Float => "float",
			Type::I8
			| Type::I16
			| Type::I32
			| Type::I64
			| Type::U8
			| Type::U16
			| Type::U32
			| Type::U64
			| Type::Integer => "integer",
			Type::Bool => "bool",
			Type::Char => "char",
			Type::Namespace { .. } => "module",
			Type::Memory { .. } => "memory",
			Type::Unit { .. } => "unit",
			Type::Array { .. } => "array",
			Type::Slice { .. } => "slice",
			Type::Pointer { .. } => "pointer",
			Type::Tuple { .. } => "tuple",
			Type::Error { .. } => "{unknown}",
			Type::Infer => "_",
			Type::Never { .. } => "never",
			Type::AssocTypeProjection { .. } | Type::AssociatedType { .. } => {
				"type"
			}
			Type::TypeParam { .. } => "generic",
		}
	}

	pub fn display_type(
		&self,
		idx: TypeIndex,
	) -> Result<String, std::fmt::Error> {
		let mut buffer = String::new();
		self.write_type(&mut buffer, idx)?;
		Ok(buffer)
	}

	pub fn display_bounds(
		&self,
		bounds: &Bounds,
	) -> Result<String, std::fmt::Error> {
		let mut buffer = String::new();
		self.write_bounds(&mut buffer, bounds)?;
		Ok(buffer)
	}

	fn write_type(
		&self,
		f: &mut impl std::fmt::Write,
		idx: TypeIndex,
	) -> std::fmt::Result {
		match &self.tir.types[idx.as_usize()] {
			Type::Integer => f.write_str("{integer}"),
			Type::Float => f.write_str("{float}"),
			Type::Error => f.write_str("{unknown}"),
			Type::Infer => f.write_str("_"),
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
				memory,
				mutable,
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
				memory,
				mutable,
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
				memory,
				mutable,
			} => {
				self.write_type(f, *memory)?;
				write!(
					f,
					"::[{}]{}",
					size,
					if *mutable { "mut " } else { "" }
				)?;
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
					.resolve(
						self.tir.structs[*struct_index as usize].name.inner,
					)
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
				let memory_index = self.tir.expect_memory_index(*id);
				self.interner
					.resolve(
						self.tir.memories[memory_index as usize].name.inner,
					)
					.ok_or(std::fmt::Error)
					.and_then(|name| f.write_str(name))
			}
			Type::Namespace { namespace_idx } => self
				.interner
				.resolve(self.tir.namespaces[*namespace_idx as usize].name)
				.ok_or(std::fmt::Error)
				.and_then(|name| f.write_str(name)),
			Type::Function { signature } => {
				f.write_str("fn(")?;
				for (i, param) in signature.params().iter().copied().enumerate()
				{
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
				let func = &self.tir.functions
					[self.tir.expect_function_index(*id) as usize];
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
						let has_bounds = !param_info.bounds.traits.is_empty()
							|| param_info.bounds.typeset.is_some();
						if has_bounds {
							f.write_str(": ")?;
							self.write_bounds(f, &param_info.bounds)?;
						}
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
						let func = &self.tir.functions
							[self.tir.expect_function_index(*def_id) as usize];
						let own_idx = *param_index as usize
							- func.inherited_type_param_count;
						let symbol = func.type_params[own_idx].name;
						self.interner.resolve(symbol).ok_or(std::fmt::Error)?
					}
					TypeParamOwner::Struct(def_id) => {
						let symbol = self.tir.structs
							[self.tir.expect_struct_index(*def_id) as usize]
							.type_params[*param_index as usize]
							.name;
						self.interner.resolve(symbol).ok_or(std::fmt::Error)?
					}
					TypeParamOwner::Trait(trait_idx) => {
						let symbol = self.tir.traits[*trait_idx as usize]
							.self_type_param
							.name;
						self.interner.resolve(symbol).ok_or(std::fmt::Error)?
					}
					TypeParamOwner::ImplBlock(block_idx) => {
						let symbol = self.tir.generic_impl_list
							[*block_idx as usize]
							.type_params[*param_index as usize]
							.name;
						self.interner.resolve(symbol).ok_or(std::fmt::Error)?
					}
					TypeParamOwner::TypeAlias(def_id) => {
						let symbol = self.tir.type_aliases[self
							.tir
							.expect_type_alias_index(*def_id)
							as usize]
							.type_params[*param_index as usize]
							.name;
						self.interner.resolve(symbol).ok_or(std::fmt::Error)?
					}
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
				assoc_name, base, ..
			} => {
				let (assoc_name, base) = (*assoc_name, *base);
				self.write_type(f, base)?;
				f.write_str("::")?;
				self.interner
					.resolve(assoc_name)
					.ok_or(std::fmt::Error)
					.and_then(|type_name| f.write_str(type_name))?;
				Ok(())
			}
		}
	}

	fn write_bounds(
		&self,
		f: &mut impl std::fmt::Write,
		bounds: &Bounds,
	) -> std::fmt::Result {
		let mut first = true;
		for trait_bound in bounds.traits.iter() {
			if !first {
				f.write_str(" + ")?;
			}
			first = false;
			self.interner
				.resolve(
					self.tir.traits[trait_bound.trait_index as usize]
						.name
						.inner,
				)
				.ok_or(std::fmt::Error)
				.and_then(|name| f.write_str(name))?;
			if !trait_bound.bindings.is_empty() {
				f.write_str(" where { ")?;
				for (i, (assoc_name, assoc_type)) in
					trait_bound.bindings.iter().enumerate()
				{
					if i > 0 {
						f.write_str(", ")?;
					}
					self.interner
						.resolve(*assoc_name)
						.ok_or(std::fmt::Error)
						.and_then(|name| f.write_str(name))?;
					f.write_str(" = ")?;
					self.write_type(f, *assoc_type)?;
				}
				f.write_str(" }")?;
			}
		}
		if let Some(typeset_idx) = bounds.typeset {
			if !first {
				f.write_str(" + ")?;
			}
			self.interner
				.resolve(self.tir.typesets[typeset_idx as usize].name.inner)
				.ok_or(std::fmt::Error)
				.and_then(|name| f.write_str(name))?;
		}
		Ok(())
	}
}

/// Index of a named item in its kind-specific Vec, carried by [`TIR::item_lookup`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ItemIndex {
	Function(FunctionIndex),
	Global(GlobalIndex),
	Memory(MemoryIndex),
	Struct(u32),
	Const(ConstIndex),
	TypeSet(TypesetIndex),
	Trait(TraitIndex),
	TraitImpl(TraitImplIndex),
	Enum(EnumIndex),
	TypeAlias(u32),
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TIR {
	pub types: Vec<Type>,
	pub diagnostics: Vec<Diagnostic<FileId>>,
	pub functions: Vec<Function>,
	pub globals: Vec<Global>,
	pub memories: Vec<Memory>,
	pub namespaces: Vec<ModuleNamespace>,
	/// Symbol table for the implicit root namespace (`namespace = None`) —
	/// the counterpart of `ModuleNamespace::symbols` for items that aren't
	/// nested inside any `module { }` block.
	#[cfg_attr(test, serde(skip))]
	pub root_symbols: HashMap<(SymbolNamespace, SymbolU32), SymbolKind>,
	/// Namespaces brought into the root scope via `use path::*;`. Parallel to
	/// `ModuleNamespace::wildcard_imports`.
	#[cfg_attr(test, serde(skip))]
	pub root_wildcard_imports: Vec<NamespaceIndex>,
	pub module_decls: Vec<ModuleDecl>,
	pub import_decls: Vec<ImportDecl>,
	pub enums: Vec<Enum>,
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
	pub exports: HashMap<SymbolU32, ExportItem>,
	pub structs: Vec<Struct>,
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
	pub generic_impl_dispatch:
		HashMap<(GenericImplTargetKind, SymbolU32), usize>,
	pub traits: Vec<Trait>,
	pub trait_impls: Vec<TraitImpl>,
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
	pub trait_impl_lookup: HashMap<(TypeIndex, TraitIndex), TraitImplIndex>,
	#[cfg_attr(
		test,
		serde(serialize_with = "crate::testing::serialize_sorted_map")
	)]
	pub type_trait_impls: HashMap<TypeIndex, Vec<TraitImplIndex>>,
	pub constants: Vec<Constant>,
	#[cfg_attr(test, serde(skip))]
	pub tagged_items: HashMap<SymbolU32, DefId>,
	pub typesets: Vec<TypeSet>,
	pub type_aliases: Vec<TypeAlias>,
	/// Unified lookup: maps every named item's `DefId` to its kind and dense Vec index.
	/// Replaces the previous per-kind hashmaps (`function_index_lookup`, etc.).
	#[cfg_attr(test, serde(skip))]
	pub item_lookup: HashMap<DefId, ItemIndex>,
}

impl TIR {
	#[inline]
	pub fn function_index(&self, id: ast::DefId) -> Option<FunctionIndex> {
		match self.item_lookup.get(&id)? {
			ItemIndex::Function(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_function_index(&self, id: DefId) -> FunctionIndex {
		match self.item_lookup[&id] {
			ItemIndex::Function(i) => i,
			#[cfg(debug_assertions)]
			other => {
				unreachable!("expected Function for {id:?}, found {other:?}")
			}
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn struct_index(&self, id: DefId) -> Option<u32> {
		match self.item_lookup.get(&id)? {
			ItemIndex::Struct(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_struct_index(&self, id: DefId) -> u32 {
		match self.item_lookup[&id] {
			ItemIndex::Struct(i) => i,
			#[cfg(debug_assertions)]
			other => {
				unreachable!("expected Struct for {id:?}, found {other:?}")
			}
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn type_alias_index(&self, id: DefId) -> Option<u32> {
		match self.item_lookup.get(&id)? {
			ItemIndex::TypeAlias(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_type_alias_index(&self, id: DefId) -> u32 {
		match self.item_lookup[&id] {
			ItemIndex::TypeAlias(i) => i,
			#[cfg(debug_assertions)]
			other => {
				unreachable!("expected TypeAlias for {id:?}, found {other:?}")
			}
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn const_index(&self, id: DefId) -> Option<ConstIndex> {
		match self.item_lookup.get(&id)? {
			ItemIndex::Const(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_const_index(&self, id: DefId) -> ConstIndex {
		match self.item_lookup[&id] {
			ItemIndex::Const(i) => i,
			#[cfg(debug_assertions)]
			other => unreachable!("expected Const for {id:?}, found {other:?}"),
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn global_index(&self, id: DefId) -> Option<GlobalIndex> {
		match self.item_lookup.get(&id)? {
			ItemIndex::Global(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_global_index(&self, id: DefId) -> GlobalIndex {
		match self.item_lookup[&id] {
			ItemIndex::Global(i) => i,
			#[cfg(debug_assertions)]
			other => {
				unreachable!("expected Global for {id:?}, found {other:?}")
			}
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn memory_index(&self, id: DefId) -> Option<MemoryIndex> {
		match self.item_lookup.get(&id)? {
			ItemIndex::Memory(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_memory_index(&self, id: DefId) -> MemoryIndex {
		match self.item_lookup[&id] {
			ItemIndex::Memory(i) => i,
			#[cfg(debug_assertions)]
			other => {
				unreachable!("expected Memory for {id:?}, found {other:?}")
			}
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn typeset_index(&self, id: DefId) -> Option<TypesetIndex> {
		match self.item_lookup.get(&id)? {
			ItemIndex::TypeSet(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_typeset_index(&self, id: DefId) -> TypesetIndex {
		match self.item_lookup[&id] {
			ItemIndex::TypeSet(i) => i,
			#[cfg(debug_assertions)]
			other => {
				unreachable!("expected TypeSet for {id:?}, found {other:?}")
			}
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn trait_index(&self, id: DefId) -> Option<TraitIndex> {
		match self.item_lookup.get(&id)? {
			ItemIndex::Trait(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_trait_index(&self, id: DefId) -> TraitIndex {
		match self.item_lookup[&id] {
			ItemIndex::Trait(i) => i,
			#[cfg(debug_assertions)]
			other => unreachable!("expected Trait for {id:?}, found {other:?}"),
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn trait_impl_index(&self, id: DefId) -> Option<TraitImplIndex> {
		match self.item_lookup.get(&id)? {
			ItemIndex::TraitImpl(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_trait_impl_index(&self, id: DefId) -> TraitImplIndex {
		match self.item_lookup[&id] {
			ItemIndex::TraitImpl(i) => i,
			#[cfg(debug_assertions)]
			other => {
				unreachable!("expected TraitImpl for {id:?}, found {other:?}")
			}
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	#[inline]
	pub fn enum_index(&self, id: DefId) -> Option<EnumIndex> {
		match self.item_lookup.get(&id)? {
			ItemIndex::Enum(i) => Some(*i),
			_ => None,
		}
	}

	#[inline]
	pub fn expect_enum_index(&self, id: DefId) -> EnumIndex {
		match self.item_lookup[&id] {
			ItemIndex::Enum(index) => index,
			#[cfg(debug_assertions)]
			other => unreachable!("expected Enum for {id:?}, found {other:?}"),
			#[cfg(not(debug_assertions))]
			_ => unreachable!(),
		}
	}

	/// Returns the `TypeParamInfo` for the type parameter at `abs_index`
	/// (absolute, 0-based across the full owner chain) under `owner`.
	///
	/// For `Function` owners, params inherited from a parent `ImplBlock` occupy
	/// indices `0..inherited_count`; the function's own params start at
	/// `inherited_count`. For all other owners, `abs_index` indexes directly
	/// into the owner's `type_params` slice.
	pub fn type_param_info(
		&self,
		owner: TypeParamOwner,
		abs_index: usize,
	) -> &TypeParamInfo {
		match owner {
			TypeParamOwner::ImplBlock(block_idx) => {
				&self.generic_impl_list[block_idx as usize].type_params
					[abs_index]
			}
			TypeParamOwner::Function(id) => {
				let func_idx = self.expect_function_index(id) as usize;
				let inherited =
					self.functions[func_idx].inherited_type_param_count;
				&self.functions[func_idx].type_params[abs_index - inherited]
			}
			TypeParamOwner::Struct(id) => {
				let struct_idx = self.expect_struct_index(id) as usize;
				&self.structs[struct_idx].type_params[abs_index]
			}
			TypeParamOwner::Trait(trait_idx) => {
				debug_assert_eq!(
					abs_index, 0,
					"only Self (index 0) is owned by a Trait"
				);
				&self.traits[trait_idx as usize].self_type_param
			}
			TypeParamOwner::TypeAlias(id) => {
				let alias_idx = self.expect_type_alias_index(id) as usize;
				&self.type_aliases[alias_idx].type_params[abs_index]
			}
		}
	}

	/// Mutable counterpart of [`TIR::type_param_info`].
	pub fn type_param_info_mut(
		&mut self,
		owner: TypeParamOwner,
		abs_index: usize,
	) -> &mut TypeParamInfo {
		match owner {
			TypeParamOwner::ImplBlock(block_idx) => {
				&mut self.generic_impl_list[block_idx as usize].type_params
					[abs_index]
			}
			TypeParamOwner::Function(id) => {
				let func_idx = self.expect_function_index(id) as usize;
				let inherited =
					self.functions[func_idx].inherited_type_param_count;
				&mut self.functions[func_idx].type_params[abs_index - inherited]
			}
			TypeParamOwner::Struct(id) => {
				let struct_idx = self.expect_struct_index(id) as usize;
				&mut self.structs[struct_idx].type_params[abs_index]
			}
			TypeParamOwner::Trait(trait_idx) => {
				debug_assert_eq!(
					abs_index, 0,
					"only Self (index 0) is owned by a Trait"
				);
				&mut self.traits[trait_idx as usize].self_type_param
			}
			TypeParamOwner::TypeAlias(id) => {
				let alias_idx = self.expect_type_alias_index(id) as usize;
				&mut self.type_aliases[alias_idx].type_params[abs_index]
			}
		}
	}

	/// Iterates all type parameters visible to `func_index` in absolute-index
	/// order: inherited params from the parent first, then the function's own
	/// params. For trait methods the parent is the trait's implicit `Self`;
	/// for generic impl methods the parent is the impl block's type params.
	pub fn function_type_params_iter(
		&self,
		func_index: FunctionIndex,
	) -> impl Iterator<Item = &TypeParamInfo> {
		let func = &self.functions[func_index as usize];
		let parent_params: &[TypeParamInfo] = match func.type_param_parent {
			Some(TypeParamOwner::ImplBlock(block_idx)) => {
				&self.generic_impl_list[block_idx as usize].type_params
			}
			Some(TypeParamOwner::Trait(trait_idx)) => std::slice::from_ref(
				&self.traits[trait_idx as usize].self_type_param,
			),
			_ => &[],
		};
		parent_params.iter().chain(func.type_params.iter())
	}
}

#[derive(PartialEq)]
enum WasmScalar {
	I32,
	I64,
	F32,
	F64,
}

impl TIR {
	pub fn formatter<'a>(
		&'a self,
		interner: &'a ast::StringInterner,
	) -> TypeFormatter<'a> {
		TypeFormatter::new(self, interner)
	}

	pub fn is_import_namespace(
		&self,
		namespace: Option<NamespaceIndex>,
	) -> bool {
		match namespace {
			Some(idx) => match self.namespaces[idx as usize].declaration {
				ModuleDeclarationKind::Import(_) => true,
				ModuleDeclarationKind::Module(_)
				| ModuleDeclarationKind::Crate(..) => false,
			},
			None => false,
		}
	}

	#[inline]
	pub fn build(compilation: &mut CompilationGraph) -> TIR {
		builder::build(compilation)
	}
}
