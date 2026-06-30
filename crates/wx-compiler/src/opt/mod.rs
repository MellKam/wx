//! Sea-of-nodes SSA IR for per-function optimization.
//!
//! Pipeline position: `mir::Function` → [`builder`] → [`Function`] →
//! [`scheduler`] → codegen
//!
//! # Structure
//! - [`DataNode`] — a pure value computation (constants, arithmetic, phis,
//!   aggregates). Nodes with identical [`DataNodeKind`] are deduplicated (CSE)
//!   via `Builder::node`.
//! - [`ControlNode`] — a side-effecting operation or control-flow construct.
//!   Placed sequentially inside [`Block`]s.
//! - [`Block`] — a linear sequence of `ControlNode`s, one per MIR scope.
//! - [`Function`] — the complete graph for one MIR function.

use std::collections::HashMap;

pub mod builder;
mod liveness;
pub mod scheduler;

#[cfg(test)]
mod tests;

use crate::{ast, mir};

pub type DataNodeIndex = u32;
pub type BlockIndex = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ScalarType {
	I32,
	I64,
	F32,
	F64,
}

/// Sign only matters for narrow loads: `i32.load8_s` vs `i32.load8_u`.
/// Full-width loads and stores are always unsigned.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(serde::Serialize))]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum MemAccess {
	I8S,
	I8U,
	I16S,
	I16U,
	I32,
	I64,
	F32,
	F64,
}

impl MemAccess {
	pub fn from_mir(ty: mir::Type) -> Self {
		match ty {
			mir::Type::I8 => Self::I8S,
			mir::Type::U8 => Self::I8U,
			mir::Type::I16 => Self::I16S,
			mir::Type::U16 => Self::I16U,
			mir::Type::I64 | mir::Type::U64 => Self::I64,
			mir::Type::F32 => Self::F32,
			mir::Type::F64 => Self::F64,
			_ => Self::I32,
		}
	}

	pub fn scalar_type(self) -> ScalarType {
		match self {
			Self::I8S | Self::I8U | Self::I16S | Self::I16U | Self::I32 => {
				ScalarType::I32
			}
			Self::I64 => ScalarType::I64,
			Self::F32 => ScalarType::F32,
			Self::F64 => ScalarType::F64,
		}
	}

	/// Log2 of the natural alignment in bytes (WASM memarg encoding).
	pub fn align_log2(self) -> u32 {
		match self {
			Self::I8S | Self::I8U => 0,
			Self::I16S | Self::I16U => 1,
			Self::I32 | Self::F32 => 2,
			Self::I64 | Self::F64 => 3,
		}
	}
}

impl TryFrom<mir::Type> for ScalarType {
	type Error = ();
	fn try_from(ty: mir::Type) -> Result<Self, ()> {
		Ok(match ty {
			mir::Type::I32
			| mir::Type::U32
			| mir::Type::Bool
			| mir::Type::U8
			| mir::Type::I8
			| mir::Type::U16
			| mir::Type::I16
			| mir::Type::Pointer { .. }
			| mir::Type::Function { .. } => ScalarType::I32,
			mir::Type::I64 | mir::Type::U64 => ScalarType::I64,
			mir::Type::F32 => ScalarType::F32,
			mir::Type::F64 => ScalarType::F64,
			_ => return Err(()),
		})
	}
}

impl From<ScalarType> for crate::codegen::ValueType {
	fn from(ty: ScalarType) -> Self {
		match ty {
			ScalarType::I32 => crate::codegen::ValueType::I32,
			ScalarType::I64 => crate::codegen::ValueType::I64,
			ScalarType::F32 => crate::codegen::ValueType::F32,
			ScalarType::F64 => crate::codegen::ValueType::F64,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeType {
	Scalar(ScalarType),
	Aggregate(mir::AggregateIndex),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackResult {
	Value(DataNodeIndex),
	Unit,
	Never,
}

impl StackResult {
	pub fn unwrap_value(self) -> DataNodeIndex {
		match self {
			StackResult::Value(idx) => idx,
			r => panic!("expected Value, got {:?}", r),
		}
	}
}

#[derive(Clone, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum DataNodeKind {
	// ── Constants ──────────────────────────────────────────────────────────
	Int {
		value: i64,
		ty: ScalarType,
	},
	/// Float bits stored as u64 to allow hashing.
	Float {
		bits: u64,
		ty: ScalarType,
	},

	// ── Inputs ─────────────────────────────────────────────────────────────
	Param {
		index: u32,
		ty: ScalarType,
	},
	/// Read from a mutable module global. Excluded from CSE.
	GlobalGet {
		id: ast::DefId,
		ty: ScalarType,
	},
	/// Constant index into the WASM function table.
	FunctionRef {
		id: ast::DefId,
	},
	/// Pointer into the static data segment for a string or array constant.
	StaticDataRef {
		data_index: u32,
	},
	/// Byte offset of the end of the data section (link-time constant).
	MemoryOffset {
		memory: ast::DefId,
	},
	/// WASM linear-memory index as an integer constant, resolved at codegen.
	MemoryIndex {
		memory: ast::DefId,
	},
	/// Result of a `MemorySize` control node. Excluded from CSE; always spilled.
	MemorySizeResult {
		memory: ast::DefId,
	},

	// ── Arithmetic ─────────────────────────────────────────────────────────
	Add {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	Sub {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	Mul {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	Div {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	Rem {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},

	// ── Bitwise ────────────────────────────────────────────────────────────
	BitAnd {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	BitOr {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	BitXor {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	Shl {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	ShrS {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	ShrU {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},

	// ── Unary ──────────────────────────────────────────────────────────────
	Neg {
		operand: DataNodeIndex,
		ty: ScalarType,
	},
	BitNot {
		operand: DataNodeIndex,
		ty: ScalarType,
	},
	/// `i32.eqz` — produces I32.
	Eqz {
		operand: DataNodeIndex,
	},
	/// `i64.extend_i32_s` — sign-extends I32 to I64.
	I64ExtendI32S {
		operand: DataNodeIndex,
	},
	/// `i64.extend_i32_u` — zero-extends I32 to I64.
	I64ExtendI32U {
		operand: DataNodeIndex,
	},
	/// `i32.wrap_i64` — truncates I64 to I32.
	I32WrapI64 {
		operand: DataNodeIndex,
	},

	// ── Comparisons (always produce I32 / WASM bool) ───────────────────────
	Eq {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	NotEq {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	LtS {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	LtU {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	LtEqS {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	LtEqU {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	GtS {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	GtU {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	GtEqS {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},
	GtEqU {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},

	// ── Aggregates (structs as SSA values) ─────────────────────────────────
	Aggregate {
		fields: Box<[DataNodeIndex]>,
		aggregate_index: mir::AggregateIndex,
	},
	/// Folds immediately when `aggregate` is a known `Aggregate` node.
	AggregateGet {
		aggregate: DataNodeIndex,
		field_index: u32,
		ty: ScalarType,
	},

	// ── Control-flow joins ─────────────────────────────────────────────────
	/// Merge two scalar values at a branch join point.
	/// Aggregate phis are decomposed field-by-field by the builder.
	Phi {
		left: DataNodeIndex,
		right: DataNodeIndex,
		ty: ScalarType,
	},

	/// A scalar value that flows around a loop. `after` starts as `before` and
	/// is patched once the loop body is built (see
	/// `Function::patch_loop_param`).
	LoopParam {
		block_index: BlockIndex,
		before: DataNodeIndex,
		after: DataNodeIndex,
		ty: ScalarType,
	},

	// ── Call / memory results (excluded from CSE: side effects) ────────────
	CallResult {
		callee: DataNodeIndex,
		args: Box<[DataNodeIndex]>,
		ty: ScalarType,
	},
	/// Unlike `Aggregate`, has no concrete field sub-nodes — values come from
	/// the WASM multi-return stack. `AggregateGet` of this node does not fold.
	AggregateCallResult {
		aggregate_index: mir::AggregateIndex,
	},
	MemoryGrowResult {
		memory: ast::DefId,
		delta: DataNodeIndex,
	},
	/// Value produced by a `ControlNode::PointerLoad`. Always spilled.
	PointerLoadResult {
		address: DataNodeIndex,
		access: MemAccess,
	},
}

impl DataNodeKind {
	pub fn node_type(&self) -> NodeType {
		match self {
			DataNodeKind::Int { ty, .. }
			| DataNodeKind::Param { ty, .. }
			| DataNodeKind::Add { ty, .. }
			| DataNodeKind::Sub { ty, .. }
			| DataNodeKind::Mul { ty, .. }
			| DataNodeKind::Div { ty, .. }
			| DataNodeKind::Rem { ty, .. }
			| DataNodeKind::BitAnd { ty, .. }
			| DataNodeKind::BitOr { ty, .. }
			| DataNodeKind::BitXor { ty, .. }
			| DataNodeKind::Shl { ty, .. }
			| DataNodeKind::ShrS { ty, .. }
			| DataNodeKind::ShrU { ty, .. }
			| DataNodeKind::Neg { ty, .. }
			| DataNodeKind::BitNot { ty, .. }
			| DataNodeKind::AggregateGet { ty, .. }
			| DataNodeKind::Phi { ty, .. }
			| DataNodeKind::LoopParam { ty, .. }
			| DataNodeKind::CallResult { ty, .. }
			| DataNodeKind::Float { ty, .. } => NodeType::Scalar(*ty),

			DataNodeKind::Eqz { .. }
			| DataNodeKind::Eq { .. }
			| DataNodeKind::NotEq { .. }
			| DataNodeKind::LtS { .. }
			| DataNodeKind::LtU { .. }
			| DataNodeKind::LtEqS { .. }
			| DataNodeKind::LtEqU { .. }
			| DataNodeKind::GtS { .. }
			| DataNodeKind::GtU { .. }
			| DataNodeKind::GtEqS { .. }
			| DataNodeKind::GtEqU { .. }
			| DataNodeKind::FunctionRef { .. }
			| DataNodeKind::StaticDataRef { .. }
			| DataNodeKind::MemoryOffset { .. }
			| DataNodeKind::MemoryIndex { .. }
			| DataNodeKind::MemorySizeResult { .. }
			| DataNodeKind::MemoryGrowResult { .. }
			| DataNodeKind::I32WrapI64 { .. } => NodeType::Scalar(ScalarType::I32),
			DataNodeKind::I64ExtendI32S { .. }
			| DataNodeKind::I64ExtendI32U { .. } => NodeType::Scalar(ScalarType::I64),
			DataNodeKind::GlobalGet { ty, .. } => NodeType::Scalar(*ty),

			DataNodeKind::PointerLoadResult { access, .. } => {
				NodeType::Scalar(access.scalar_type())
			}

			DataNodeKind::Aggregate {
				aggregate_index, ..
			}
			| DataNodeKind::AggregateCallResult { aggregate_index } => {
				NodeType::Aggregate(*aggregate_index)
			}
		}
	}

	pub fn unwrap_scalar(&self) -> ScalarType {
		match self.node_type() {
			NodeType::Scalar(s) => s,
			NodeType::Aggregate(_) => {
				panic!("expected scalar node type, got aggregate")
			}
		}
	}

	/// Returns true when two nodes with the same inputs are guaranteed to
	/// produce the same value and can be deduplicated. Impure nodes (reads of
	/// mutable state, call results, memory ops) and LoopParams (mutated after
	/// creation) are not pure and each represent a distinct value.
	fn is_pure(&self) -> bool {
		match self {
			DataNodeKind::GlobalGet { .. }
			| DataNodeKind::MemorySizeResult { .. }
			| DataNodeKind::CallResult { .. }
			| DataNodeKind::AggregateCallResult { .. }
			| DataNodeKind::MemoryGrowResult { .. }
			| DataNodeKind::PointerLoadResult { .. }
			| DataNodeKind::LoopParam { .. } => false,
			_ => true,
		}
	}
}

pub struct DataNode {
	pub kind: DataNodeKind,
	pub uses: Vec<DataNodeIndex>,
}

pub enum ControlNode {
	Return {
		value: StackResult,
	},
	GlobalSet {
		id: ast::DefId,
		value: DataNodeIndex,
	},
	Call {
		callee: DataNodeIndex,
		args: Box<[DataNodeIndex]>,
		result: StackResult,
		/// MIR signature index for this call; used by the scheduler to emit
		/// `CallIndirectSym` when the callee is not a statically known
		/// `FunctionRef`.
		callee_sig: u32,
	},
	IfElse {
		condition: DataNodeIndex,
		then_block: BlockIndex,
		else_block: Option<BlockIndex>,
		/// Phi nodes produced at the join point (one per differing binding).
		/// Aggregate bindings contribute one phi per field.
		outputs: Box<[DataNodeIndex]>,
		result: StackResult,
	},
	Loop {
		body: BlockIndex,
		/// LoopParam nodes for bindings that change across the loop.
		/// Aggregate bindings contribute one loop-param per field.
		outputs: Box<[DataNodeIndex]>,
		result: StackResult,
	},
	Break {
		target: BlockIndex,
		value: StackResult,
	},
	Continue {
		target: BlockIndex,
	},
	Unreachable,
	MemorySize {
		memory: ast::DefId,
		result: DataNodeIndex,
	},
	MemoryGrow {
		memory: ast::DefId,
		delta: DataNodeIndex,
		result: DataNodeIndex,
	},
	MemoryFill {
		memory: ast::DefId,
		dst: DataNodeIndex,
		val: DataNodeIndex,
		len: DataNodeIndex,
	},
	MemoryCopy {
		dst_memory: ast::DefId,
		src_memory: ast::DefId,
		dst: DataNodeIndex,
		src: DataNodeIndex,
		len: DataNodeIndex,
	},
	PointerLoad {
		address: DataNodeIndex,
		/// Byte offset added to `address` at the WASM instruction level (memarg).
		offset: u32,
		result: DataNodeIndex,
		memory: ast::DefId,
		access: MemAccess,
	},
	PointerStore {
		address: DataNodeIndex,
		/// Byte offset added to `address` at the WASM instruction level (memarg).
		offset: u32,
		value: DataNodeIndex,
		memory: ast::DefId,
		access: MemAccess,
	},
}

pub struct Block {
	pub is_loop: bool,
	pub parent: Option<BlockIndex>,
	pub statements: Vec<ControlNode>,
	/// Exit value. For loop blocks, overwritten with `body_fallthrough` after
	/// `build_loop`; the pre-overwrite value is saved into `ControlNode::Loop.result`.
	pub result: StackResult,
	/// Phi nodes at the loop-exit join point for `break <value>` paths.
	/// Scheduler pre-allocates WASM locals for these; each `Break` stores into
	/// them before the `br`. Empty when all breaks carry the same value.
	pub break_result_outputs: Vec<DataNodeIndex>,
}

pub struct Function {
	pub id: ast::DefId,
	pub data_nodes: Vec<DataNode>,
	/// One slot per MIR scope (indexed by scope index). `None` until the scope
	/// is built.
	pub blocks: Vec<Option<Block>>,
	/// CSE map: `DataNodeKind` → existing `DataNodeIndex`. Impure nodes excluded.
	data_lookup: HashMap<DataNodeKind, DataNodeIndex>,
}

impl Function {
	pub fn new(id: ast::DefId, scope_count: usize) -> Self {
		Function {
			id,
			data_nodes: Vec::new(),
			blocks: (0..scope_count).map(|_| None).collect(),
			data_lookup: HashMap::new(),
		}
	}

	/// Get or create a data node via CSE only. Does not apply any algebraic
	/// simplification — call `Builder::node` for that.
	pub fn intern_node(&mut self, kind: DataNodeKind) -> DataNodeIndex {
		if kind.is_pure() {
			if let Some(&id) = self.data_lookup.get(&kind) {
				return id;
			}
		}

		let id = self.data_nodes.len() as DataNodeIndex;
		self.register_uses(&kind, id);

		if kind.is_pure() {
			self.data_lookup.insert(kind.clone(), id);
		}
		self.data_nodes.push(DataNode {
			kind,
			uses: Vec::new(),
		});
		id
	}

	/// Create a loop-param placeholder for `before` with `after = before`.
	/// Call `patch_loop_param` once the loop body has been built.
	pub fn push_loop_param(
		&mut self,
		block_index: BlockIndex,
		before: DataNodeIndex,
		ty: ScalarType,
	) -> DataNodeIndex {
		let id = self.data_nodes.len() as DataNodeIndex;
		self.data_nodes.push(DataNode {
			kind: DataNodeKind::LoopParam {
				block_index,
				before,
				after: before,
				ty,
			},
			uses: Vec::new(),
		});
		id
	}

	/// Finalize a loop-param once the loop body is fully built.
	/// If `after == before` (the binding was never mutated), the node is left
	/// as-is and no uses are registered — the scheduler will see zero uses and
	/// skip it.
	pub fn patch_loop_param(
		&mut self,
		id: DataNodeIndex,
		after: DataNodeIndex,
	) {
		let (block_index, before, ty) = match self.data_nodes[id as usize].kind
		{
			DataNodeKind::LoopParam {
				block_index,
				before,
				ty,
				..
			} => (block_index, before, ty),
			_ => panic!("patch_loop_param called on non-LoopParam node"),
		};
		if before == after {
			return;
		}
		self.data_nodes[id as usize].kind = DataNodeKind::LoopParam {
			block_index,
			before,
			after,
			ty,
		};
		self.data_nodes[before as usize].uses.push(id);
		self.data_nodes[after as usize].uses.push(id);
	}

	fn register_uses(&mut self, kind: &DataNodeKind, user_id: DataNodeIndex) {
		match kind {
            DataNodeKind::Add { left, right, .. }
            | DataNodeKind::Sub { left, right, .. }
            | DataNodeKind::Mul { left, right, .. }
            | DataNodeKind::Div { left, right, .. }
            | DataNodeKind::Rem { left, right, .. }
            | DataNodeKind::BitAnd { left, right, .. }
            | DataNodeKind::BitOr  { left, right, .. }
            | DataNodeKind::BitXor { left, right, .. }
            | DataNodeKind::Shl    { left, right, .. }
            | DataNodeKind::ShrS   { left, right, .. }
            | DataNodeKind::ShrU   { left, right, .. }
            | DataNodeKind::Eq     { left, right, .. }
            | DataNodeKind::NotEq  { left, right, .. }
            | DataNodeKind::LtS    { left, right, .. }
            | DataNodeKind::LtU    { left, right, .. }
            | DataNodeKind::LtEqS  { left, right, .. }
            | DataNodeKind::LtEqU  { left, right, .. }
            | DataNodeKind::GtS    { left, right, .. }
            | DataNodeKind::GtU    { left, right, .. }
            | DataNodeKind::GtEqS  { left, right, .. }
            | DataNodeKind::GtEqU  { left, right, .. }
            | DataNodeKind::Phi    { left, right, .. } => {
                self.data_nodes[*left as usize].uses.push(user_id);
                self.data_nodes[*right as usize].uses.push(user_id);
            }

            DataNodeKind::Neg    { operand, .. }
            | DataNodeKind::BitNot { operand, .. }
            | DataNodeKind::Eqz    { operand }
            | DataNodeKind::I64ExtendI32S { operand }
            | DataNodeKind::I64ExtendI32U { operand }
            | DataNodeKind::I32WrapI64 { operand }
            | DataNodeKind::AggregateGet { aggregate: operand, .. } => {
                self.data_nodes[*operand as usize].uses.push(user_id);
            }

            DataNodeKind::Aggregate { fields, .. } => {
                for &f in fields.iter() {
                    self.data_nodes[f as usize].uses.push(user_id);
                }
            }

            DataNodeKind::CallResult { callee, args, .. } => {
                self.data_nodes[*callee as usize].uses.push(user_id);
                for &a in args.iter() {
                    self.data_nodes[a as usize].uses.push(user_id);
                }
            }

            DataNodeKind::MemoryGrowResult { delta, .. } => {
                self.data_nodes[*delta as usize].uses.push(user_id);
            }

            DataNodeKind::PointerLoadResult { address, .. } => {
                self.data_nodes[*address as usize].uses.push(user_id);
            }

            // Leaf nodes: no inputs to register.
            DataNodeKind::Int { .. }
            | DataNodeKind::Float { .. }
            | DataNodeKind::Param { .. }
            | DataNodeKind::GlobalGet { .. }
            | DataNodeKind::FunctionRef { .. }
            | DataNodeKind::StaticDataRef { .. }
            | DataNodeKind::MemoryOffset { .. }
            | DataNodeKind::MemoryIndex { .. }
            | DataNodeKind::MemorySizeResult { .. }
            | DataNodeKind::AggregateCallResult { .. }
            // LoopParam uses are registered by patch_loop_param after both
            // `before` and `after` are known.
            | DataNodeKind::LoopParam { .. } => {}
        }
	}
}
