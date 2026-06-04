//! Sea-of-nodes SSA IR for per-function optimization.
//!
//! Pipeline position: `mir::Function` → [`builder`] → [`Function`] →
//! [`scheduler`] → codegen
//!
//! # Structure
//! - [`DataNode`] — a pure value computation (constants, arithmetic, phis,
//!   aggregates). Nodes with identical [`DataNodeKind`] are deduplicated (CSE)
//!   via `Function::ensure_node`.
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

// ── Types ─────────────────────────────────────────────────────────────────────

/// Primitive WASM value types. Unsigned/sub-word MIR types all lower to one of
/// these.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ScalarType {
    I32,
    I64,
    F32,
    F64,
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

/// The type of a data node's output — either a WASM scalar or a struct value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeType {
    Scalar(ScalarType),
    Aggregate(mir::AggregateIndex),
}

/// What a MIR expression produces when lowered into the sea-of-nodes graph.
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

// ── Data nodes
// ────────────────────────────────────────────────────────────────

/// A pure (or near-pure) value computation.
///
/// All variants except `GlobalGet`, `MemorySizeResult`, `CallResult`,
/// `MemoryGrowResult`, and `LoopParam` are inserted into the CSE map; identical
/// computations reuse the same node.
#[derive(Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Debug))]
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
    },
    /// Function reference (constant index into the WASM table). CSE-eligible.
    FunctionRef {
        id: ast::DefId,
    },
    /// Pointer into the static data segment for a string or array constant.
    /// CSE-eligible.
    StaticDataRef {
        data_index: u32,
    },
    /// Byte offset of the end of the data section (a link-time constant).
    /// CSE-eligible.
    MemoryOffset {
        memory: ast::DefId,
    },
    /// Wasm linear-memory index as an integer constant, resolved at codegen.
    /// CSE-eligible.
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
    /// Construct an aggregate from its component scalar nodes.
    Aggregate {
        fields: Box<[DataNodeIndex]>,
        aggregate_index: mir::AggregateIndex,
    },
    /// Extract one scalar field from an aggregate value.
    /// Folds immediately if `aggregate` is a known `Aggregate` node.
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
    /// Aggregate value returned by a call. Unlike `Aggregate`, there are no
    /// concrete field sub-nodes — the per-field values come from the WASM
    /// multi-return stack. The scheduler captures them into per-field locals
    /// immediately when the call instruction is emitted.
    /// `AggregateGet` of this node does not fold (no known fields to inline).
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
        ty: ScalarType,
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
            | DataNodeKind::GlobalGet { .. }
            | DataNodeKind::FunctionRef { .. }
            | DataNodeKind::StaticDataRef { .. }
            | DataNodeKind::MemoryOffset { .. }
            | DataNodeKind::MemoryIndex { .. }
            | DataNodeKind::MemorySizeResult { .. }
            | DataNodeKind::MemoryGrowResult { .. } => NodeType::Scalar(ScalarType::I32),

            DataNodeKind::PointerLoadResult { ty, .. } => NodeType::Scalar(*ty),

            DataNodeKind::Aggregate {
                aggregate_index, ..
            }
            | DataNodeKind::AggregateCallResult { aggregate_index } => {
                NodeType::Aggregate(*aggregate_index)
            }
        }
    }

    pub fn scalar_type(&self) -> ScalarType {
        match self.node_type() {
            NodeType::Scalar(s) => s,
            NodeType::Aggregate(_) => panic!("expected scalar node type, got aggregate"),
        }
    }

    /// Returns true for kinds excluded from the CSE map.
    fn no_cse(&self) -> bool {
        matches!(
            self,
            DataNodeKind::GlobalGet { .. }
                | DataNodeKind::MemorySizeResult { .. }
                | DataNodeKind::CallResult { .. }
                | DataNodeKind::AggregateCallResult { .. }
                | DataNodeKind::MemoryGrowResult { .. }
                | DataNodeKind::PointerLoadResult { .. }
                // LoopParam nodes are mutated after creation, so they cannot be CSE'd.
                | DataNodeKind::LoopParam { .. }
        )
    }
}

/// A value node with its reverse-edge use list.
pub struct DataNode {
    pub kind: DataNodeKind,
    /// Indices of nodes whose computation depends on this node's output.
    pub uses: Vec<DataNodeIndex>,
}

// ── Control nodes
// ─────────────────────────────────────────────────────────────

pub enum ControlNode {
    Return {
        value: StackResult,
    },
    GlobalSet {
        id: ast::DefId,
        value: DataNodeIndex,
    },
    /// A function call with possible side effects.
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
        /// The `MemorySizeResult` data node produced by this operation.
        result: DataNodeIndex,
    },
    MemoryGrow {
        memory: ast::DefId,
        delta: DataNodeIndex,
        /// The `MemoryGrowResult` data node produced by this operation.
        result: DataNodeIndex,
    },
    /// Load a scalar value from a raw pointer address. Sequenced with stores.
    PointerLoad {
        address: DataNodeIndex,
        /// Byte offset added to `address` at the WASM instruction level (memarg).
        offset: u32,
        /// The `PointerLoadResult` data node that carries the loaded value.
        result: DataNodeIndex,
        memory: ast::DefId,
    },
    /// Store a scalar value to a raw pointer address.
    PointerStore {
        address: DataNodeIndex,
        /// Byte offset added to `address` at the WASM instruction level (memarg).
        offset: u32,
        value: DataNodeIndex,
        memory: ast::DefId,
    },
}

// ── Blocks ────────────────────────────────────────────────────────────────────

pub struct Block {
    /// Whether this block is a loop body (affects `Continue` semantics).
    pub is_loop: bool,
    pub parent: Option<BlockIndex>,
    pub statements: Vec<ControlNode>,
    pub result: StackResult,
}

// ── Function (the graph)
// ──────────────────────────────────────────────────────

pub struct Function {
    pub id: ast::DefId,
    pub data_nodes: Vec<DataNode>,
    /// One slot per MIR scope (indexed by scope index). `None` until the scope
    /// is built.
    pub blocks: Vec<Option<Block>>,
    /// CSE map: `DataNodeKind` → existing `DataNodeIndex`.
    /// Excludes nodes where `DataNodeKind::no_cse()` is true.
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

    /// Get or create a data node, applying CSE and constant folding.
    ///
    /// For `AggregateGet` of a known `Aggregate`, returns the field node
    /// directly (no new node is created). For arithmetic on two `Int`
    /// constants, folds to a new `Int` node.
    pub fn ensure_node(&mut self, kind: DataNodeKind) -> DataNodeIndex {
        // ── Structural / constant-folding optimizations ───────────────────
        // These produce a result without creating a new node for `kind`.

        // AggregateGet of a concrete Aggregate folds to the field node directly.
        if let DataNodeKind::AggregateGet {
            aggregate,
            field_index,
            ..
        } = &kind
        {
            if let DataNodeKind::Aggregate { fields, .. } =
                &self.data_nodes[*aggregate as usize].kind
            {
                return fields[*field_index as usize];
            }
        }

        // Phi with equal operands is a no-op.
        if let DataNodeKind::Phi { left, right, .. } = &kind {
            if left == right {
                return *left;
            }
        }

        // Integer constant folding for binary arithmetic.
        if let Some(folded) = self.try_fold_int(&kind) {
            return folded;
        }

        // ── CSE lookup ────────────────────────────────────────────────────
        if !kind.no_cse() {
            if let Some(&id) = self.data_lookup.get(&kind) {
                return id;
            }
        }

        // ── Allocate new node ─────────────────────────────────────────────
        let id = self.data_nodes.len() as DataNodeIndex;
        self.register_uses(&kind, id);

        if !kind.no_cse() {
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
    pub fn patch_loop_param(&mut self, id: DataNodeIndex, after: DataNodeIndex) {
        let (block_index, before, ty) = match self.data_nodes[id as usize].kind {
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

    // ── Private helpers ───────────────────────────────────────────────────

    /// Integer constant folding. Returns `Some(node_id)` when the expression
    /// reduces to an `Int` constant.
    fn try_fold_int(&mut self, kind: &DataNodeKind) -> Option<DataNodeIndex> {
        let (left, right, ty) = match *kind {
            DataNodeKind::Add { left, right, ty } => (left, right, ty),
            DataNodeKind::Sub { left, right, ty } => (left, right, ty),
            DataNodeKind::Mul { left, right, ty } => (left, right, ty),
            DataNodeKind::Div { left, right, ty } => (left, right, ty),
            DataNodeKind::Rem { left, right, ty } => (left, right, ty),
            DataNodeKind::BitAnd { left, right, ty } => (left, right, ty),
            DataNodeKind::BitOr { left, right, ty } => (left, right, ty),
            DataNodeKind::BitXor { left, right, ty } => (left, right, ty),
            _ => return None,
        };

        let l = self.as_int(left)?;
        let r = self.as_int(right)?;

        let result = match kind {
            DataNodeKind::Add { .. } => l.wrapping_add(r),
            DataNodeKind::Sub { .. } => l.wrapping_sub(r),
            DataNodeKind::Mul { .. } => l.wrapping_mul(r),
            DataNodeKind::Div { .. } if r == 0 => return None,
            DataNodeKind::Div { .. } => l.wrapping_div(r),
            DataNodeKind::Rem { .. } if r == 0 => return None,
            DataNodeKind::Rem { .. } => l.wrapping_rem(r),
            DataNodeKind::BitAnd { .. } => l & r,
            DataNodeKind::BitOr { .. } => l | r,
            DataNodeKind::BitXor { .. } => l ^ r,
            _ => unreachable!(),
        };

        Some(self.ensure_node(DataNodeKind::Int { value: result, ty }))
    }

    fn as_int(&self, node: DataNodeIndex) -> Option<i64> {
        match self.data_nodes[node as usize].kind {
            DataNodeKind::Int { value, .. } => Some(value),
            _ => None,
        }
    }

    /// Walk a node's inputs and push `user_id` into their use lists.
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
