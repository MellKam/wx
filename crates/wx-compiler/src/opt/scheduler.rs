//! Lower a sea-of-nodes [`Function`] to a flat sequence of WASM stack-machine
//! instructions.
//!
//! # Algorithm
//!
//! 1. **Spill decision** — `should_spill` decides whether a node must be
//!    materialised into a WASM local or can be inlined at its single use site.
//!    Constants and params are always inlined. Multi-use nodes and call results
//!    are always spilled.
//!
//! 2. **Dead pointer-load elimination** — before emission we compute the set of
//!    data nodes reachable backwards from control-node data sinks. Pure
//!    `PointerLoad` control nodes whose result is not live are skipped.
//!
//! 3. **Scheduling order** — we walk the block tree recursively and emit
//!    instructions in a post-order traversal of data dependencies: each node's
//!    inputs are emitted (or loaded from their local) before the node itself.
//!
//! # Output
//!
//! The scheduler produces a [`ScheduledFunction`] containing
//! - the extra WASM locals needed for spilled nodes (appended after params)
//! - a flat `Vec<Instruction>` for the function body
//!
//! Encoding [`Instruction`]s to bytes is left to the codegen layer.

use std::collections::HashMap;

use crate::codegen::ValueType;
use crate::mir;
use crate::opt::liveness::DataLiveness;
use crate::opt::{
    BlockIndex, ControlNode, DataNode, DataNodeIndex, DataNodeKind, Function, MemAccess,
    ScalarType, StackResult,
};

// ── Output types
// ──────────────────────────────────────────────────────────────

/// The `memarg` immediate carried by every WebAssembly memory instruction.
#[cfg_attr(test, derive(Debug, serde::Serialize))]
#[derive(Clone, Copy)]
pub struct MemArg {
    /// Log2 of the alignment hint in bytes (e.g. 2 = 4-byte aligned).
    pub align: u32,
    /// Static byte offset added to the runtime address.
    pub offset: u32,
    pub memory: crate::ast::DefId,
}

/// A WASM local variable declaration.
#[cfg_attr(test, derive(serde::Serialize))]
#[cfg_attr(test, serde(transparent))]
pub struct Local {
    pub ty: ScalarType,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ScheduledFunction {
    /// Locals in declaration order (params first, then spill slots).
    pub locals: Vec<Local>,
    /// Flat WASM stack-machine instruction sequence for the function body.
    pub body: Vec<Instruction>,
}

/// A subset of WASM instructions produced by the scheduler.
/// Each variant maps 1-to-1 to a WASM opcode; operands are pushed onto the
/// implicit value stack by the preceding instructions.
#[cfg_attr(test, derive(Debug, serde::Serialize))]
#[derive(Clone)]
pub enum Instruction {
    // Constants
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    // Locals
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    // Globals
    GlobalGet(crate::ast::DefId),
    GlobalSet(crate::ast::DefId),
    // Arithmetic — i32
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32RemS,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32LeS,
    I32LeU,
    I32GtS,
    I32GtU,
    I32GeS,
    I32GeU,
    I32Clz,
    I32Ctz,
    // Arithmetic — i64
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64RemS,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64LeS,
    I64LeU,
    I64GtS,
    I64GtU,
    I64GeS,
    I64GeU,
    // Arithmetic — f32 / f64
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Neg,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Neg,
    F32Eq,
    F32Ne,
    F32Lt,
    F32Le,
    F32Gt,
    F32Ge,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Le,
    F64Gt,
    F64Ge,
    // Control flow
    Block {
        ty: BlockType,
    },
    Loop {
        ty: BlockType,
    },
    If {
        ty: BlockType,
    },
    Else,
    End,
    Br(u32), // break by depth
    BrIf(u32),
    Return,
    Unreachable,
    Drop,
    // Calls
    /// Direct call; the encoder resolves the WASM function index from
    /// `func_wasm_index`, covering both internal and imported functions.
    Call(crate::ast::DefId),
    /// Indirect call via the function table; the encoder resolves `type_index`
    /// from the referenced MIR signature.
    CallIndirectSym {
        mir_sig_index: u32,
    },
    // Memory
    MemorySize(crate::ast::DefId),
    MemoryGrow(crate::ast::DefId),
    MemoryFill(crate::ast::DefId),
    MemoryCopy { dst: crate::ast::DefId, src: crate::ast::DefId },
    /// Wasm linear-memory index as an `i32.const`, resolved at codegen.
    MemoryIndex {
        memory: crate::ast::DefId,
    },
    // Pointer load/store
    I32Load8S(MemArg),
    I32Load8U(MemArg),
    I32Load16S(MemArg),
    I32Load16U(MemArg),
    I32Load(MemArg),
    I64Load(MemArg),
    F32Load(MemArg),
    F64Load(MemArg),
    I32Store8(MemArg),
    I32Store16(MemArg),
    I32Store(MemArg),
    I64Store(MemArg),
    F32Store(MemArg),
    F64Store(MemArg),
    // Conversion
    I64ExtendI32S,
    I64ExtendI32U,
    I32WrapI64,
    // Nop (used as a placeholder)
    Nop,
    // Symbolic references — resolved to concrete i32.const values by the
    // codegen encoder, which has access to the string pool and function table.
    /// A function referenced as a value; the encoder pushes it into the
    /// function table and emits `i32.const <table_index>`.
    FunctionPointer(crate::ast::DefId),
    /// End of the static data section for a given memory (base of writable
    /// heap); the encoder emits `i32.const <data_section_end>`.
    DataSectionEnd {
        memory: crate::ast::DefId,
    },
    /// A static array; the encoder resolves the index to a byte offset in the
    /// data segment and emits `i32.const <byte_offset>`.
    StaticDataPointer(u32),
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
#[derive(Clone, Copy)]
pub enum BlockType {
    Empty,
    Value(ValueType),
}

// ── Scheduler
// ─────────────────────────────────────────────────────────────────

pub struct Scheduler<'f> {
    func: &'f Function,
    mir: &'f mir::MIR,
    /// Data nodes reachable from control-node sinks under the scheduler's
    /// current emission semantics.
    live_data: DataLiveness,
    /// WASM locals: params (already allocated) + spill slots added by
    /// `ensure_local`.
    locals: Vec<Local>,
    /// Maps a scalar data-node index to its WASM local index.
    node_to_local: HashMap<DataNodeIndex, u32>,
    /// Maps an aggregate data-node index to its per-field WASM local indices.
    node_to_aggregate_locals: HashMap<DataNodeIndex, Box<[u32]>>,
    /// Output instruction stream.
    body: Vec<Instruction>,
}

impl<'f> Scheduler<'f> {
    pub fn schedule(func: &'f Function, mir: &'f mir::MIR) -> ScheduledFunction {
        let sig = &mir.signatures[{
            // Find the function's signature via its DefId.
            mir.functions
                .iter()
                .find(|f| f.id == func.id)
                .expect("function not found")
                .signature_index as usize
        }];

        // Aggregate params are flattened to one local per field.
        let locals: Vec<Local> = sig
            .params()
            .iter()
            .copied()
            .flat_map(|ty| Self::flatten_mir_type(ty, &mir.aggregates))
            .map(|ty| Local { ty })
            .collect();
        let params_count = locals.len();

        let mut sched = Scheduler {
            func,
            mir,
            live_data: DataLiveness::compute(func),
            locals,
            node_to_local: HashMap::new(),
            node_to_aggregate_locals: HashMap::new(),
            body: Vec::new(),
        };

        let root = func.blocks[0].as_ref().expect("root block must exist");
        for stmt in &root.statements {
            sched.emit_control(0, stmt);
        }

        if matches!(sched.body.last(), Some(Instruction::Return)) {
            sched.body.pop();
        }

        coalesce_locals(&mut sched.body, &mut sched.locals, params_count);
        peephole_local_tee(&mut sched.body);

        ScheduledFunction {
            locals: sched.locals,
            body: sched.body,
        }
    }

    // ── Control emission ──────────────────────────────────────────────────────

    fn emit_control(&mut self, block_idx: BlockIndex, stmt: &ControlNode) {
        match stmt {
            ControlNode::Return { value } => {
                if let StackResult::Value(node) = value {
                    self.emit_value(*node);
                }
                self.body.push(Instruction::Return);
            }

            ControlNode::GlobalSet { id, value } => {
                self.emit_value(*value);
                self.body.push(Instruction::GlobalSet(*id));
            }

            ControlNode::Call {
                callee,
                args,
                result,
                callee_sig,
            } => {
                for &arg in args.iter() {
                    self.emit_value(arg);
                }
                self.emit_call(*callee, *callee_sig);
                if let StackResult::Value(result_node) = result {
                    match self.func.data_nodes[*result_node as usize].kind {
                        DataNodeKind::AggregateCallResult { aggregate_index } => {
                            // Multi-value return: fields arrive deepest-first; pop in
                            // reverse so each local.set captures the correct field.
                            //
                            // Always spill: result may be referenced via control-node
                            // args (return, call) not tracked in DataNode::uses.
                            let mut locals = Vec::with_capacity(
                                self.mir.aggregates[aggregate_index as usize].values.len(),
                            );
                            for &t in self.mir.aggregates[aggregate_index as usize]
                                .values
                                .iter()
                                .rev()
                            {
                                let ty = ScalarType::try_from(t).expect("field must be scalar");
                                let local = self.alloc_local(ty);
                                self.body.push(Instruction::LocalSet(local));
                                locals.push(local);
                            }
                            locals.reverse();
                            self.node_to_aggregate_locals
                                .insert(*result_node, locals.into_boxed_slice());
                        }
                        _ => {
                            // Scalar result: spill to a local if used, drop otherwise.
                            if self.should_spill(&self.func.data_nodes[*result_node as usize]) {
                                let ty = self.func.data_nodes[*result_node as usize]
                                    .kind
                                    .unwrap_scalar();
                                let local = self.alloc_local(ty);
                                self.node_to_local.insert(*result_node, local);
                                self.body.push(Instruction::LocalSet(local));
                            } else {
                                self.body.push(Instruction::Drop);
                            }
                        }
                    }
                }
            }

            ControlNode::IfElse {
                condition,
                then_block,
                else_block,
                outputs,
                result,
            } => {
                // Pre-allocate WASM locals for phi outputs.
                self.pre_alloc_phi_outputs(outputs);

                self.emit_value(*condition);

                // When phis are stored via LocalSet inside branches, the if block
                // must have an empty result type — the branches consume the value.
                let result_block_ty = if outputs.is_empty() {
                    self.stack_result_block_type(*result)
                } else {
                    BlockType::Empty
                };
                self.body.push(Instruction::If {
                    ty: result_block_ty,
                });

                self.emit_block(*then_block);
                // When there are no phi outputs, the if block has a value result
                // type; each branch must leave its result on the stack.
                if outputs.is_empty() {
                    let then_block_result = self.func.blocks[*then_block as usize]
                        .as_ref()
                        .unwrap()
                        .result;
                    if let StackResult::Value(n) = then_block_result {
                        self.emit_value(n);
                    }
                }
                self.emit_phi_stores_for_branch(*then_block, outputs, true);
                if let Some(eb) = else_block {
                    self.body.push(Instruction::Else);
                    self.emit_block(*eb);
                    if outputs.is_empty() {
                        let else_block_result =
                            self.func.blocks[*eb as usize].as_ref().unwrap().result;
                        if let StackResult::Value(n) = else_block_result {
                            self.emit_value(n);
                        }
                    }
                    self.emit_phi_stores_for_branch(*eb, outputs, false);
                }
                self.body.push(Instruction::End);

                // Value-type block: the if-End leaves the result on the WASM stack.
                // Capture it into a local so the parent's emit_value(result_node) can
                // read from the local rather than pushing a second copy.
                if outputs.is_empty() {
                    if let StackResult::Value(result_node) = *result {
                        let local = if let Some(&l) = self.node_to_local.get(&result_node) {
                            l
                        } else {
                            let ty = self.func.data_nodes[result_node as usize]
                                .kind
                                .unwrap_scalar();
                            let l = self.alloc_local(ty);
                            self.node_to_local.insert(result_node, l);
                            l
                        };
                        self.body.push(Instruction::LocalSet(local));
                    }
                }
            }

            ControlNode::Loop {
                body,
                outputs,
                result: _,
            } => {
                // Allocate a fresh local for each loop param and initialise it
                // with the `before` value.  Each param gets its own local so that
                // two params with the same `before` node (e.g. both init to 1)
                // don't share a slot and overwrite each other.
                for &lp in outputs.iter() {
                    if let DataNodeKind::LoopParam { before, ty, .. } =
                        self.func.data_nodes[lp as usize].kind
                    {
                        let local = self.alloc_local(ty);
                        self.emit_value(before);
                        self.body.push(Instruction::LocalSet(local));
                        self.node_to_local.insert(lp, local);
                    }
                }

                // Pre-allocate WASM locals for break-result phi nodes so that
                // Break handlers inside the body can write to them before `br`.
                for phi in self.func.blocks[*body as usize]
                    .as_ref()
                    .unwrap()
                    .break_result_outputs
                    .iter()
                    .copied()
                {
                    if self.node_to_local.contains_key(&phi) {
                        continue;
                    }
                    let ty = self.func.data_nodes[phi as usize].kind.unwrap_scalar();
                    let local = self.alloc_local(ty);
                    self.node_to_local.insert(phi, local);
                }

                // The outer block has an empty result type: break values are held
                // in phi locals, not passed via WASM block result.
                self.body.push(Instruction::Block {
                    ty: BlockType::Empty,
                });
                self.body.push(Instruction::Loop {
                    ty: BlockType::Empty,
                });

                self.emit_block(*body);

                // Push all after-values first, then pop in reverse — avoids
                // swap-corruption when two params share an input node.
                for &lp in outputs.iter() {
                    if let DataNodeKind::LoopParam { after, .. } =
                        self.func.data_nodes[lp as usize].kind
                    {
                        self.emit_value(after);
                    }
                }
                for &lp in outputs.iter().rev() {
                    if let DataNodeKind::LoopParam { .. } = self.func.data_nodes[lp as usize].kind {
                        let lp_local = *self.node_to_local.get(&lp).unwrap();
                        self.body.push(Instruction::LocalSet(lp_local));
                    }
                }

                // Back-edge; unreachable if the body always breaks or returns.
                self.body.push(Instruction::Br(0));

                self.body.push(Instruction::End); // Loop
                self.body.push(Instruction::End); // Block
            }

            ControlNode::Break { target, value } => {
                if let StackResult::Value(v) = value {
                    // Store break value into phi locals; LocalSet in reverse
                    // because emit_value pushes fields lowest-first.
                    let n_phis = self.func.blocks[*target as usize]
                        .as_ref()
                        .unwrap()
                        .break_result_outputs
                        .len();
                    if n_phis > 0 {
                        self.emit_value(*v);
                        for phi in self.func.blocks[*target as usize]
                            .as_ref()
                            .unwrap()
                            .break_result_outputs
                            .iter()
                            .copied()
                            .rev()
                        {
                            let phi_local = *self.node_to_local.get(&phi).expect(
                                "break result phi local must be pre-allocated by Loop handler",
                            );
                            self.body.push(Instruction::LocalSet(phi_local));
                        }
                    }
                }
                let depth = self.break_depth(block_idx, *target);
                self.body.push(Instruction::Br(depth));
            }

            ControlNode::Continue { target } => {
                let depth = self.continue_depth(block_idx, *target);
                self.body.push(Instruction::Br(depth));
            }

            ControlNode::Unreachable => {
                self.body.push(Instruction::Unreachable);
            }

            ControlNode::MemorySize { memory, result } => {
                self.body.push(Instruction::MemorySize(*memory));
                let local = self.alloc_local(ScalarType::I32);
                self.node_to_local.insert(*result, local);
                self.body.push(Instruction::LocalSet(local));
            }

            ControlNode::MemoryGrow {
                memory,
                delta,
                result,
            } => {
                self.emit_value(*delta);
                self.body.push(Instruction::MemoryGrow(*memory));
                if self.should_spill(&self.func.data_nodes[*result as usize]) {
                    let local = self.alloc_local(ScalarType::I32);
                    self.node_to_local.insert(*result, local);
                    self.body.push(Instruction::LocalSet(local));
                }
            }

            ControlNode::PointerLoad {
                address,
                offset,
                result,
                memory,
                access,
            } => {
                if !self.live_data.is_live(*result) {
                    return;
                }
                self.emit_value(*address);
                let m = MemArg {
                    align: access.align_log2(),
                    offset: *offset,
                    memory: *memory,
                };
                let load_instr = match access {
                    MemAccess::I8S => Instruction::I32Load8S(m),
                    MemAccess::I8U => Instruction::I32Load8U(m),
                    MemAccess::I16S => Instruction::I32Load16S(m),
                    MemAccess::I16U => Instruction::I32Load16U(m),
                    MemAccess::I32 => Instruction::I32Load(m),
                    MemAccess::I64 => Instruction::I64Load(m),
                    MemAccess::F32 => Instruction::F32Load(m),
                    MemAccess::F64 => Instruction::F64Load(m),
                };
                self.body.push(load_instr);
                // PointerLoadResult always spills (no_cse + always_spill).
                let scalar_ty = access.scalar_type();
                let local = self.alloc_local(scalar_ty);
                self.node_to_local.insert(*result, local);
                self.body.push(Instruction::LocalSet(local));
            }

            ControlNode::MemoryFill { memory, dst, val, len } => {
                self.emit_value(*dst);
                self.emit_value(*val);
                self.emit_value(*len);
                self.body.push(Instruction::MemoryFill(*memory));
            }

            ControlNode::MemoryCopy { dst_memory, src_memory, dst, src, len } => {
                self.emit_value(*dst);
                self.emit_value(*src);
                self.emit_value(*len);
                self.body.push(Instruction::MemoryCopy { dst: *dst_memory, src: *src_memory });
            }

            ControlNode::PointerStore {
                address,
                offset,
                value,
                memory,
                access,
            } => {
                self.emit_value(*address);
                self.emit_value(*value);
                let m = MemArg {
                    align: access.align_log2(),
                    offset: *offset,
                    memory: *memory,
                };
                let store_instr = match access {
                    MemAccess::I8S | MemAccess::I8U => Instruction::I32Store8(m),
                    MemAccess::I16S | MemAccess::I16U => Instruction::I32Store16(m),
                    MemAccess::I32 => Instruction::I32Store(m),
                    MemAccess::I64 => Instruction::I64Store(m),
                    MemAccess::F32 => Instruction::F32Store(m),
                    MemAccess::F64 => Instruction::F64Store(m),
                };
                self.body.push(store_instr);
            }
        }
    }

    fn emit_block(&mut self, block_idx: BlockIndex) {
        // Collect statement count first, then index into the block per-iteration.
        // This avoids holding an immutable borrow on `self.func` while `emit_control`
        // needs `&mut self`.
        let stmt_count = self.func.blocks[block_idx as usize]
            .as_ref()
            .unwrap()
            .statements
            .len();
        for i in 0..stmt_count {
            // Re-borrow per iteration so the mutable borrow in emit_control is allowed.
            // Safety: we only read at index `i` and do not mutate `func.blocks`.
            let stmt_ptr = &self.func.blocks[block_idx as usize]
                .as_ref()
                .unwrap()
                .statements[i] as *const ControlNode;
            // SAFETY: `emit_control` never modifies `func.blocks`, so the pointer remains
            // valid.
            let stmt = unsafe { &*stmt_ptr };
            self.emit_control(block_idx, stmt);
        }
    }

    // ── Value emission ────────────────────────────────────────────────────────

    /// Emit instructions that push `node`'s value onto the WASM stack.
    /// If the node is spilled to a local, emits `local.get`; otherwise inlines.
    /// Aggregate nodes push all their field locals in field order.
    fn emit_value(&mut self, node: DataNodeIndex) {
        if matches!(
            self.func.data_nodes[node as usize].kind,
            DataNodeKind::Aggregate { .. } | DataNodeKind::AggregateCallResult { .. }
        ) {
            self.ensure_aggregate_locals(node);
            for i in 0..self.node_to_aggregate_locals[&node].len() {
                let local = self.node_to_aggregate_locals[&node][i];
                self.body.push(Instruction::LocalGet(local));
            }
            return;
        }
        if self.should_spill(&self.func.data_nodes[node as usize]) {
            let local = self.ensure_local(node);
            self.body.push(Instruction::LocalGet(local));
            return;
        }
        self.emit_value_inline(node);
    }

    /// Compute and push the value without checking / writing a local.
    fn emit_value_inline(&mut self, node: DataNodeIndex) {
        match self.func.data_nodes[node as usize].kind.clone() {
            DataNodeKind::Int { value, ty } => match ty {
                ScalarType::I32 => self.body.push(Instruction::I32Const(value as i32)),
                ScalarType::I64 => self.body.push(Instruction::I64Const(value)),
                _ => unreachable!("Int node with float type"),
            },
            DataNodeKind::Float { bits, ty } => match ty {
                ScalarType::F32 => self
                    .body
                    .push(Instruction::F32Const(f32::from_bits(bits as u32))),
                ScalarType::F64 => self.body.push(Instruction::F64Const(f64::from_bits(bits))),
                _ => unreachable!("Float node with int type"),
            },
            DataNodeKind::Param { index, .. } => {
                self.body.push(Instruction::LocalGet(index));
            }
            DataNodeKind::GlobalGet { id, .. } => {
                self.body.push(Instruction::GlobalGet(id));
            }
            DataNodeKind::FunctionRef { id } => {
                self.body.push(Instruction::FunctionPointer(id));
            }
            DataNodeKind::StaticDataRef { data_index } => {
                self.body.push(Instruction::StaticDataPointer(data_index));
            }
            DataNodeKind::MemoryOffset { memory } => {
                self.body.push(Instruction::DataSectionEnd { memory });
            }
            DataNodeKind::MemoryIndex { memory } => {
                self.body.push(Instruction::MemoryIndex { memory });
            }
            DataNodeKind::MemorySizeResult { .. } => {
                unreachable!("MemorySizeResult must be read from a local, not emitted inline")
            }

            // Arithmetic / bitwise — push left, push right, emit opcode.
            DataNodeKind::Add { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Add,
                    ScalarType::I64 => Instruction::I64Add,
                    ScalarType::F32 => Instruction::F32Add,
                    ScalarType::F64 => Instruction::F64Add,
                });
            }
            DataNodeKind::Sub { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Sub,
                    ScalarType::I64 => Instruction::I64Sub,
                    ScalarType::F32 => Instruction::F32Sub,
                    ScalarType::F64 => Instruction::F64Sub,
                });
            }
            DataNodeKind::Mul { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Mul,
                    ScalarType::I64 => Instruction::I64Mul,
                    ScalarType::F32 => Instruction::F32Mul,
                    ScalarType::F64 => Instruction::F64Mul,
                });
            }
            DataNodeKind::Div { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32DivS,
                    ScalarType::I64 => Instruction::I64DivS,
                    ScalarType::F32 => Instruction::F32Div,
                    ScalarType::F64 => Instruction::F64Div,
                });
            }
            DataNodeKind::Rem { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32RemS,
                    ScalarType::I64 => Instruction::I64RemS,
                    _ => unimplemented!("float remainder"),
                });
            }
            DataNodeKind::BitAnd { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32And,
                    ScalarType::I64 => Instruction::I64And,
                    _ => unimplemented!(),
                });
            }
            DataNodeKind::BitOr { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Or,
                    ScalarType::I64 => Instruction::I64Or,
                    _ => unimplemented!(),
                });
            }
            DataNodeKind::BitXor { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Xor,
                    ScalarType::I64 => Instruction::I64Xor,
                    _ => unimplemented!(),
                });
            }
            DataNodeKind::Shl { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Shl,
                    ScalarType::I64 => Instruction::I64Shl,
                    _ => unimplemented!(),
                });
            }
            DataNodeKind::ShrS { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32ShrS,
                    ScalarType::I64 => Instruction::I64ShrS,
                    _ => unimplemented!(),
                });
            }
            DataNodeKind::ShrU { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32ShrU,
                    ScalarType::I64 => Instruction::I64ShrU,
                    _ => unimplemented!(),
                });
            }

            DataNodeKind::Neg { operand, ty } => match ty {
                ScalarType::F32 => {
                    self.emit_value(operand);
                    self.body.push(Instruction::F32Neg);
                }
                ScalarType::F64 => {
                    self.emit_value(operand);
                    self.body.push(Instruction::F64Neg);
                }
                ScalarType::I32 => {
                    self.body.push(Instruction::I32Const(0));
                    self.emit_value(operand);
                    self.body.push(Instruction::I32Sub);
                }
                ScalarType::I64 => {
                    self.body.push(Instruction::I64Const(0));
                    self.emit_value(operand);
                    self.body.push(Instruction::I64Sub);
                }
            },
            DataNodeKind::BitNot { operand, ty } => {
                // WASM has no bitwise-not; emit `x ^ -1`.
                self.emit_value(operand);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Const(-1),
                    ScalarType::I64 => Instruction::I64Const(-1),
                    _ => unimplemented!(),
                });
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Xor,
                    ScalarType::I64 => Instruction::I64Xor,
                    _ => unimplemented!(),
                });
            }
            DataNodeKind::Eqz { operand } => {
                self.emit_value(operand);
                self.body.push(Instruction::I32Eqz);
            }
            DataNodeKind::I64ExtendI32S { operand } => {
                self.emit_value(operand);
                self.body.push(Instruction::I64ExtendI32S);
            }
            DataNodeKind::I64ExtendI32U { operand } => {
                self.emit_value(operand);
                self.body.push(Instruction::I64ExtendI32U);
            }
            DataNodeKind::I32WrapI64 { operand } => {
                self.emit_value(operand);
                self.body.push(Instruction::I32WrapI64);
            }

            DataNodeKind::Eq { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Eq,
                    ScalarType::I64 => Instruction::I64Eq,
                    ScalarType::F32 => Instruction::F32Eq,
                    ScalarType::F64 => Instruction::F64Eq,
                });
            }
            DataNodeKind::NotEq { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32Ne,
                    ScalarType::I64 => Instruction::I64Ne,
                    ScalarType::F32 => Instruction::F32Ne,
                    ScalarType::F64 => Instruction::F64Ne,
                });
            }
            DataNodeKind::LtS { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32LtS,
                    ScalarType::I64 => Instruction::I64LtS,
                    ScalarType::F32 => Instruction::F32Lt,
                    ScalarType::F64 => Instruction::F64Lt,
                });
            }
            DataNodeKind::LtU { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32LtU,
                    ScalarType::I64 => Instruction::I64LtU,
                    _ => unimplemented!("float unsigned cmp"),
                });
            }
            DataNodeKind::LtEqS { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32LeS,
                    ScalarType::I64 => Instruction::I64LeS,
                    ScalarType::F32 => Instruction::F32Le,
                    ScalarType::F64 => Instruction::F64Le,
                });
            }
            DataNodeKind::LtEqU { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32LeU,
                    ScalarType::I64 => Instruction::I64LeU,
                    _ => unimplemented!(),
                });
            }
            DataNodeKind::GtS { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32GtS,
                    ScalarType::I64 => Instruction::I64GtS,
                    ScalarType::F32 => Instruction::F32Gt,
                    ScalarType::F64 => Instruction::F64Gt,
                });
            }
            DataNodeKind::GtU { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32GtU,
                    ScalarType::I64 => Instruction::I64GtU,
                    _ => unimplemented!(),
                });
            }
            DataNodeKind::GtEqS { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32GeS,
                    ScalarType::I64 => Instruction::I64GeS,
                    ScalarType::F32 => Instruction::F32Ge,
                    ScalarType::F64 => Instruction::F64Ge,
                });
            }
            DataNodeKind::GtEqU { left, right, ty } => {
                self.emit_value(left);
                self.emit_value(right);
                self.body.push(match ty {
                    ScalarType::I32 => Instruction::I32GeU,
                    ScalarType::I64 => Instruction::I64GeU,
                    _ => unimplemented!(),
                });
            }

            DataNodeKind::AggregateGet {
                aggregate,
                field_index,
                ..
            } => {
                // Ensure the aggregate's per-field locals are populated, then read
                // the requested field.
                self.ensure_aggregate_locals(aggregate);
                let field_local = self.node_to_aggregate_locals[&aggregate][field_index as usize];
                self.body.push(Instruction::LocalGet(field_local));
            }

            DataNodeKind::Phi { .. } => {
                // A phi's local was pre-allocated by `pre_alloc_phi_outputs`.
                // The scheduler reads it here; the branches write it via LocalSet.
                let local = self.node_to_local[&node];
                self.body.push(Instruction::LocalGet(local));
            }

            DataNodeKind::LoopParam { before, .. } => {
                // Unmodified loop param (before == after, should_spill = false):
                // the value never changes, so just re-emit the `before` value directly.
                self.emit_value(before);
            }

            DataNodeKind::CallResult { .. }
            | DataNodeKind::MemoryGrowResult { .. }
            | DataNodeKind::PointerLoadResult { .. } => {
                // Always spilled; should have been caught by `should_spill` above.
                let local = self.node_to_local[&node];
                self.body.push(Instruction::LocalGet(local));
            }

            // Aggregates are intercepted in `emit_value` before reaching here.
            DataNodeKind::Aggregate { .. } | DataNodeKind::AggregateCallResult { .. } => {
                unreachable!("aggregate nodes are handled by emit_value, not emit_value_inline")
            }
        }
    }

    /// Returns true if this node must be computed into a WASM local rather than
    /// inlined at each use site.
    fn should_spill(&self, node: &DataNode) -> bool {
        match &node.kind {
            // Constants and params are always cheaper to re-emit than to spill.
            DataNodeKind::Int { .. }
            | DataNodeKind::Float { .. }
            | DataNodeKind::Param { .. }
            | DataNodeKind::FunctionRef { .. }
            | DataNodeKind::StaticDataRef { .. }
            | DataNodeKind::MemoryOffset { .. } => false,

            // Loop params whose before == after were never modified; skip.
            DataNodeKind::LoopParam { before, after, .. } => before != after,

            // Phi nodes that folded away (left == right) don't need a local.
            DataNodeKind::Phi { left, right, .. } => left != right,

            // Control-node results always produce a result that must be captured.
            DataNodeKind::CallResult { .. }
            | DataNodeKind::AggregateCallResult { .. }
            | DataNodeKind::MemorySizeResult { .. }
            | DataNodeKind::MemoryGrowResult { .. }
            | DataNodeKind::PointerLoadResult { .. } => true,

            // GlobalGet reads mutable state. Control nodes (Return, Break,
            // GlobalSet) use data-node values but never call register_uses, so
            // a GlobalGet whose only non-control use is a single data node still
            // has uses.len()==1 and would not be spilled by the catch-all below.
            // If a GlobalSet to the same global is emitted between the read and
            // a late emit_value_inline call, the inline re-read returns the
            // post-write value instead of the original. Always spilling ensures
            // the value is captured in a local on the first use (which is always
            // scheduled before any write to the same global that depends on the
            // read's descendants), and subsequent uses read the saved local.
            DataNodeKind::GlobalGet { .. } => true,

            // Aggregates live in per-field locals, not on the stack.
            DataNodeKind::Aggregate { .. } => true,

            // For all other ops: spill only if the result is consumed more than once.
            _ => node.uses.len() > 1,
        }
    }

    /// Recursively flatten a MIR type into its constituent scalar types.
    /// Mirrors `codegen::Builder::flatten_type` but yields `ScalarType`.
    fn flatten_mir_type(ty: mir::Type, aggregates: &[mir::Aggregate]) -> Vec<ScalarType> {
        match ty {
            mir::Type::Unit | mir::Type::Never => vec![],
            mir::Type::Aggregate { aggregate_index } => aggregates[aggregate_index as usize]
                .values
                .iter()
                .flat_map(|&f| Self::flatten_mir_type(f, aggregates))
                .collect(),
            _ => vec![ScalarType::try_from(ty).expect("must be scalar")],
        }
    }

    /// Ensure per-field WASM locals exist for an `Aggregate` node.
    /// Emits each field expression and spills it to a fresh local, then records
    /// the mapping in `node_to_aggregate_locals`.
    ///
    /// For `AggregateCallResult` nodes this must never be called — their locals
    /// are populated by `emit_control` when the call instruction is emitted.
    fn ensure_aggregate_locals(&mut self, node: DataNodeIndex) {
        if self.node_to_aggregate_locals.contains_key(&node) {
            return;
        }
        let (fields, aggregate_index) = match self.func.data_nodes[node as usize].kind.clone() {
            DataNodeKind::Aggregate {
                fields,
                aggregate_index,
            } => (fields, aggregate_index),
            DataNodeKind::AggregateCallResult { .. } => {
                unreachable!(
                    "AggregateCallResult locals must be populated by emit_control before use"
                )
            }
            _ => panic!("ensure_aggregate_locals called on non-aggregate node"),
        };
        let field_types: Vec<ScalarType> = self.mir.aggregates[aggregate_index as usize]
            .values
            .iter()
            .map(|&t| ScalarType::try_from(t).expect("aggregate field must be scalar"))
            .collect();
        let mut locals = Vec::with_capacity(fields.len());
        for (i, &field_node) in fields.iter().enumerate() {
            self.emit_value(field_node);
            let local = self.alloc_local(field_types[i]);
            self.body.push(Instruction::LocalSet(local));
            locals.push(local);
        }
        self.node_to_aggregate_locals
            .insert(node, locals.into_boxed_slice());
    }

    /// Ensure a WASM local exists for a scalar node. Computes and stores the
    /// value if not yet materialised.
    fn ensure_local(&mut self, node: DataNodeIndex) -> u32 {
        if let Some(&l) = self.node_to_local.get(&node) {
            return l;
        }
        self.emit_value_inline(node);
        let ty = self.func.data_nodes[node as usize].kind.unwrap_scalar();
        let local = self.alloc_local(ty);
        self.body.push(Instruction::LocalSet(local));
        self.node_to_local.insert(node, local);
        local
    }

    fn alloc_local(&mut self, ty: ScalarType) -> u32 {
        let idx = self.locals.len() as u32;
        self.locals.push(Local { ty });
        idx
    }

    /// Pre-allocate WASM locals for phi nodes produced by an if-else join.
    /// Both branches will write to these locals; the code after the if-else
    /// reads them.
    fn pre_alloc_phi_outputs(&mut self, outputs: &[DataNodeIndex]) {
        for &phi in outputs {
            if self.node_to_local.contains_key(&phi) {
                continue;
            }
            let ty = self.func.data_nodes[phi as usize].kind.unwrap_scalar();
            let local = self.alloc_local(ty);
            self.node_to_local.insert(phi, local);
        }
    }

    /// Emit `local.set` instructions at the end of a branch for each phi
    /// output.
    fn emit_phi_stores_for_branch(
        &mut self,
        _branch_block: BlockIndex,
        outputs: &[DataNodeIndex],
        is_then: bool,
    ) {
        for &phi in outputs {
            let (input, local) = match &self.func.data_nodes[phi as usize].kind {
                DataNodeKind::Phi { left, right, .. } => {
                    let input = if is_then { *left } else { *right };
                    let local = *self.node_to_local.get(&phi).unwrap();
                    (input, local)
                }
                _ => continue,
            };
            self.emit_value(input);
            self.body.push(Instruction::LocalSet(local));
        }
    }

    // ── Depth computation ──────────────────────────────────────────────────────

    /// WASM `br` depth for a `break` targeting `target_block` from
    /// `current_block`.
    fn break_depth(&self, current: BlockIndex, target: BlockIndex) -> u32 {
        // Walk up the block tree. is_loop blocks cost 2 WASM levels (block + loop);
        // others cost 1. Add +1 when the target is_loop so we exit via the outer
        // `block` (break) rather than the inner `loop` (continue).
        let mut depth = 0u32;
        let mut idx = current;
        loop {
            let block = self.func.blocks[idx as usize].as_ref().unwrap();
            if idx == target {
                if block.is_loop {
                    depth += 1;
                }
                return depth;
            }
            depth += if block.is_loop { 2 } else { 1 };
            idx = block.parent.unwrap();
        }
    }

    /// WASM `br` depth for a `continue` (branch to loop header) from
    /// `current_block`.
    fn continue_depth(&self, current: BlockIndex, target: BlockIndex) -> u32 {
        // break_depth targets the outer block; subtract 1 to reach the inner loop.
        self.break_depth(current, target) - 1
    }

    // ── Index resolution ───────────────────────────────────────────────────────

    fn emit_call(&mut self, callee_node: DataNodeIndex, callee_sig: u32) {
        match &self.func.data_nodes[callee_node as usize].kind {
            DataNodeKind::FunctionRef { id } => {
                self.body.push(Instruction::Call(*id));
            }
            _ => {
                self.emit_value(callee_node);
                self.body.push(Instruction::CallIndirectSym {
                    mir_sig_index: callee_sig,
                });
            }
        }
    }

    fn stack_result_block_type(&self, result: StackResult) -> BlockType {
        match result {
            StackResult::Value(node) => {
                let ty = self.func.data_nodes[node as usize].kind.unwrap_scalar();
                BlockType::Value(ValueType::from(ty))
            }
            _ => BlockType::Empty,
        }
    }
}

// ── Local coalescing ──────────────────────────────────────────────────────────

/// Reuse WASM local slots for spilled values whose live ranges do not overlap.
///
/// Each spill slot has a live range `[first_write, last_read]` measured in flat
/// instruction-list positions. Two slots of the same WASM type can share a slot
/// number when one range ends strictly before the other begins.
///
/// Param slots (indices `0..params_count`) are never remapped — WASM passes
/// arguments via the first N locals and the ABI cannot be changed.
fn coalesce_locals(body: &mut Vec<Instruction>, locals: &mut Vec<Local>, params_count: usize) {
    let n = locals.len();
    if n <= params_count {
        return;
    }

    // ── Step 1: compute live ranges ──────────────────────────────────────────
    let mut first_write = vec![usize::MAX; n];
    let mut last_read = vec![0usize; n];

    for (i, instr) in body.iter().enumerate() {
        match instr {
            Instruction::LocalSet(s) => {
                let s = *s as usize;
                if s >= params_count {
                    first_write[s] = first_write[s].min(i);
                }
            }
            Instruction::LocalGet(s) => {
                let s = *s as usize;
                if s >= params_count {
                    last_read[s] = last_read[s].max(i);
                }
            }
            Instruction::LocalTee(s) => {
                let s = *s as usize;
                if s >= params_count {
                    first_write[s] = first_write[s].min(i);
                    last_read[s] = last_read[s].max(i);
                }
            }
            _ => {}
        }
    }

    // Normalize: dead stores (written, never read) collapse to a point range.
    // Slots never written (shouldn't normally happen) get range [0, 0].
    for s in params_count..n {
        if first_write[s] == usize::MAX {
            first_write[s] = 0;
        }
        if last_read[s] < first_write[s] {
            last_read[s] = first_write[s];
        }
    }

    // ── Step 2: linear scan ──────────────────────────────────────────────────
    let mut order: Vec<usize> = (params_count..n).collect();
    order.sort_unstable_by_key(|&s| first_write[s]);

    let ty_idx = |ty: ScalarType| match ty {
        ScalarType::I32 => 0usize,
        ScalarType::I64 => 1,
        ScalarType::F32 => 2,
        ScalarType::F64 => 3,
    };

    // Per-type free lists of slot numbers available for reuse.
    let mut free: [Vec<u32>; 4] = [Vec::new(), Vec::new(), Vec::new(), Vec::new()];
    // Active intervals: (last_read, new_slot_number, type_index).
    let mut active: Vec<(usize, u32, usize)> = Vec::new();
    let mut next_slot = params_count as u32;
    let mut mapping = vec![0u32; n];
    for i in 0..params_count {
        mapping[i] = i as u32;
    }

    for old in order {
        let start = first_write[old];
        let end = last_read[old];
        let ti = ty_idx(locals[old].ty);

        // Expire intervals that ended strictly before this one starts.
        let mut i = 0;
        while i < active.len() {
            if active[i].0 < start {
                let (_, freed, freed_ti) = active.swap_remove(i);
                free[freed_ti].push(freed);
            } else {
                i += 1;
            }
        }

        let new_slot = free[ti].pop().unwrap_or_else(|| {
            let s = next_slot;
            next_slot += 1;
            s
        });

        mapping[old] = new_slot;
        active.push((end, new_slot, ti));
    }

    // ── Step 3: rebuild locals and rewrite instructions ──────────────────────
    let new_spill_count = (next_slot - params_count as u32) as usize;
    let mut spill_types = vec![ScalarType::I32; new_spill_count];
    for old in params_count..n {
        let new = mapping[old] as usize;
        if new >= params_count {
            spill_types[new - params_count] = locals[old].ty;
        }
    }
    locals.truncate(params_count);
    locals.extend(spill_types.into_iter().map(|ty| Local { ty }));

    for instr in body.iter_mut() {
        match instr {
            Instruction::LocalGet(s) | Instruction::LocalSet(s) | Instruction::LocalTee(s) => {
                *s = mapping[*s as usize];
            }
            _ => {}
        }
    }
}

/// Replace `LocalSet(n), LocalGet(n)` pairs with a single `LocalTee(n)`,
/// and eliminate `LocalTee(n), LocalSet(n)` by dropping the redundant tee.
///
/// `local.tee` writes the top-of-stack value to the local *and* leaves a copy
/// on the stack, which is exactly what set+get does in two instructions.
/// Conversely, `local.tee(n)` immediately followed by `local.set(n)` writes
/// the same value to the local twice — the tee is redundant; a plain set suffices.
fn peephole_local_tee(body: &mut Vec<Instruction>) {
    let mut out = Vec::with_capacity(body.len());
    let mut i = 0;
    while i < body.len() {
        if let Instruction::LocalSet(s) = body[i] {
            // set(N), get(N), set(N) → set(N): the middle get feeds back into a set
            // of the same slot, so both the tee and the redundant write collapse.
            if i + 2 < body.len() {
                if let (Instruction::LocalGet(g), Instruction::LocalSet(s2)) =
                    (&body[i + 1], &body[i + 2])
                {
                    if *g == s && *s2 == s {
                        out.push(Instruction::LocalSet(s));
                        i += 3;
                        continue;
                    }
                }
            }
            // set(N), get(N) → tee(N)
            if i + 1 < body.len() {
                if let Instruction::LocalGet(g) = body[i + 1] {
                    if s == g {
                        out.push(Instruction::LocalTee(s));
                        i += 2;
                        continue;
                    }
                }
            }
        }
        out.push(body[i].clone());
        i += 1;
    }
    *body = out;
}
