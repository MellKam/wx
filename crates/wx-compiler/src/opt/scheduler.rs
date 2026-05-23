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
//! 2. **Dead-node elimination** — nodes with an empty `uses` list are never
//!    scheduled. This is a direct consequence of tracking use-counts in the
//!    builder.
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
use crate::opt::{
    BlockIndex, ControlNode, DataNode, DataNodeIndex, DataNodeKind, Function, ScalarType,
    StackResult,
};

// ── Output types
// ──────────────────────────────────────────────────────────────

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
    GlobalGet(u32),
    GlobalSet(u32),
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
    Block { ty: BlockType },
    Loop { ty: BlockType },
    If { ty: BlockType },
    Else,
    End,
    Br(u32), // break by depth
    BrIf(u32),
    Return,
    Unreachable,
    Drop,
    // Calls
    Call(u32), // direct call by function index
    CallIndirect { type_index: u32, table_index: u32 },
    // Memory
    MemorySize(u32),
    MemoryGrow(u32),
    // Nop (used as a placeholder)
    Nop,
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

        let locals: Vec<Local> = sig
            .params()
            .iter()
            .map(|ty| Local {
                ty: ScalarType::try_from(*ty).expect("param must be scalar"),
            })
            .collect();

        let mut sched = Scheduler {
            func,
            mir,
            locals,
            node_to_local: HashMap::new(),
            node_to_aggregate_locals: HashMap::new(),
            body: Vec::new(),
        };

        let root = func.blocks[0].as_ref().expect("root block must exist");
        for stmt in &root.statements {
            sched.emit_control(0, stmt);
        }

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
                let global_wasm_index = self.resolve_global_id(*id);
                self.body.push(Instruction::GlobalSet(global_wasm_index));
            }

            ControlNode::Call {
                callee,
                args,
                result,
            } => {
                for &arg in args.iter() {
                    self.emit_value(arg);
                }
                self.emit_call(*callee);
                // If the call produces a value that is later used, spill it.
                if let StackResult::Value(result_node) = result {
                    if self.should_spill(&self.func.data_nodes[*result_node as usize]) {
                        let local = self.alloc_local(ScalarType::I32); // placeholder — see note
                        self.node_to_local.insert(*result_node, local);
                        self.body.push(Instruction::LocalSet(local));
                    } else {
                        self.body.push(Instruction::Drop);
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
                self.emit_phi_stores_for_branch(*then_block, outputs, true);
                if let Some(eb) = else_block {
                    self.body.push(Instruction::Else);
                    self.emit_block(*eb);
                    self.emit_phi_stores_for_branch(*eb, outputs, false);
                }
                self.body.push(Instruction::End);
            }

            ControlNode::Loop {
                body,
                outputs,
                result,
            } => {
                // Spill each `before` value into a local and immediately map the
                // LoopParam to that local.  This must happen *before* emit_block
                // so that uses of the LoopParam inside the body read the correct local.
                for &lp in outputs.iter() {
                    if let DataNodeKind::LoopParam { before, .. } =
                        self.func.data_nodes[lp as usize].kind
                    {
                        let local = self.ensure_local(before);
                        self.node_to_local.insert(lp, local);
                    }
                }

                let result_block_ty = self.stack_result_block_type(*result);
                self.body.push(Instruction::Block {
                    ty: result_block_ty,
                });
                self.body.push(Instruction::Loop {
                    ty: BlockType::Empty,
                });

                self.emit_block(*body);

                // Write the `after` value back into the LoopParam's local so the
                // next iteration reads the updated value.
                for &lp in outputs.iter() {
                    if let DataNodeKind::LoopParam { after, .. } =
                        self.func.data_nodes[lp as usize].kind
                    {
                        let lp_local = *self.node_to_local.get(&lp).unwrap();
                        self.emit_value(after);
                        self.body.push(Instruction::LocalSet(lp_local));
                    }
                }

                // Implicit loop continue — branch back to the loop header.
                // If the body always breaks or returns, this instruction is
                // unreachable but harmless under WASM validation.
                self.body.push(Instruction::Br(0));

                self.body.push(Instruction::End); // Loop
                self.body.push(Instruction::End); // Block
            }

            ControlNode::Break { target, value } => {
                if let StackResult::Value(v) = value {
                    self.emit_value(*v);
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

            ControlNode::MemoryGrow {
                memory_index,
                delta,
                result,
            } => {
                self.emit_value(*delta);
                self.body.push(Instruction::MemoryGrow(*memory_index));
                if self.should_spill(&self.func.data_nodes[*result as usize]) {
                    let local = self.alloc_local(ScalarType::I32);
                    self.node_to_local.insert(*result, local);
                    self.body.push(Instruction::LocalSet(local));
                }
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
    fn emit_value(&mut self, node: DataNodeIndex) {
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
            DataNodeKind::GlobalGet { id } => {
                let idx = self.resolve_global_id(id);
                self.body.push(Instruction::GlobalGet(idx));
            }
            DataNodeKind::FunctionRef { id } => {
                let idx = self.resolve_function_id(id);
                self.body.push(Instruction::I32Const(idx as i32));
            }
            DataNodeKind::StringRef { string_index } => {
                // Strings are emitted as an i32 offset; exact value computed by codegen.
                self.body.push(Instruction::I32Const(string_index as i32)); // placeholder
            }
            DataNodeKind::MemoryOffset { .. } => {
                // Link-time constant; codegen fills the actual value.
                self.body.push(Instruction::I32Const(0)); // placeholder
            }
            DataNodeKind::MemorySize { memory_index } => {
                self.body.push(Instruction::MemorySize(memory_index));
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

            DataNodeKind::Neg { operand, ty } => {
                self.emit_value(operand);
                self.body.push(match ty {
                    ScalarType::F32 => Instruction::F32Neg,
                    ScalarType::F64 => Instruction::F64Neg,
                    _ => unimplemented!("integer negation"),
                });
            }
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
                // Aggregate locals are decomposed into per-field WASM locals.
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
                // Loop params share a local with their `before` value.
                let local = self.ensure_local(before);
                self.body.push(Instruction::LocalGet(local));
            }

            DataNodeKind::CallResult { .. } | DataNodeKind::MemoryGrowResult { .. } => {
                // Always spilled; should have been caught by `should_spill` above.
                let local = self.node_to_local[&node];
                self.body.push(Instruction::LocalGet(local));
            }

            DataNodeKind::Aggregate { .. } => {
                // Aggregate values don't exist on the WASM stack; their fields
                // are accessed individually via `AggregateGet` nodes.
                // If this is reached, it means an aggregate was used directly
                // (e.g. as a return value) — needs decomposition at the call site.
                unimplemented!("aggregate value on WASM stack — decompose at use site")
            }
        }
    }

    // ── Spill helpers ─────────────────────────────────────────────────────────

    /// Returns true if this node must be computed into a WASM local rather than
    /// inlined at each use site.
    fn should_spill(&self, node: &DataNode) -> bool {
        match &node.kind {
            // Constants and params are always cheaper to re-emit than to spill.
            DataNodeKind::Int { .. }
            | DataNodeKind::Float { .. }
            | DataNodeKind::Param { .. }
            | DataNodeKind::FunctionRef { .. }
            | DataNodeKind::StringRef { .. }
            | DataNodeKind::MemoryOffset { .. } => false,

            // Loop params whose before == after were never modified; skip.
            DataNodeKind::LoopParam { before, after, .. } => before != after,

            // Phi nodes that folded away (left == right) don't need a local.
            DataNodeKind::Phi { left, right, .. } => left != right,

            // Calls and memory.grow always produce a single result that must be captured.
            DataNodeKind::CallResult { .. } | DataNodeKind::MemoryGrowResult { .. } => true,

            // Aggregates live in per-field locals, not on the stack.
            DataNodeKind::Aggregate { .. } => true,

            // For all other ops: spill only if the result is consumed more than once.
            _ => node.uses.len() > 1,
        }
    }

    /// Ensure a WASM local exists for a scalar node. Computes and stores the
    /// value if not yet materialised.
    fn ensure_local(&mut self, node: DataNodeIndex) -> u32 {
        if let Some(&l) = self.node_to_local.get(&node) {
            return l;
        }
        self.emit_value_inline(node);
        let ty = self.func.data_nodes[node as usize].kind.scalar_type();
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
            let ty = self.func.data_nodes[phi as usize].kind.scalar_type();
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
        // Walk up the block tree counting nesting levels.
        // Each regular block adds 1 level; loop blocks add 1 for the outer `block`
        // wrapper (used to break out of the loop).
        let mut depth = 0u32;
        let mut idx = current;
        loop {
            if idx == target {
                return depth;
            }
            depth += 1;
            idx = self.func.blocks[idx as usize]
                .as_ref()
                .unwrap()
                .parent
                .unwrap();
        }
    }

    /// WASM `br` depth for a `continue` (branch to loop header) from
    /// `current_block`.
    fn continue_depth(&self, current: BlockIndex, target: BlockIndex) -> u32 {
        // A `continue` targets the inner `loop` instruction, which is one level
        // deeper than the outer `block` wrapper.
        self.break_depth(current, target) + 1
    }

    // ── Index resolution ───────────────────────────────────────────────────────

    fn resolve_global_id(&self, id: crate::ast::DefId) -> u32 {
        self.mir
            .globals
            .iter()
            .position(|g| g.id == id)
            .expect("global not found") as u32
    }

    fn resolve_function_id(&self, id: crate::ast::DefId) -> u32 {
        self.mir
            .functions
            .iter()
            .position(|f| f.id == id)
            .expect("function not found") as u32
    }

    fn emit_call(&mut self, callee_node: DataNodeIndex) {
        match &self.func.data_nodes[callee_node as usize].kind {
            DataNodeKind::FunctionRef { id } => {
                let idx = self.resolve_function_id(*id);
                self.body.push(Instruction::Call(idx));
            }
            _ => {
                // Indirect call via function table — emit the table index then call_indirect.
                self.emit_value(callee_node);
                // TODO: look up the correct type index for the signature.
                self.body.push(Instruction::CallIndirect {
                    type_index: 0,
                    table_index: 0,
                });
            }
        }
    }

    fn stack_result_block_type(&self, result: StackResult) -> BlockType {
        match result {
            StackResult::Value(node) => {
                let ty = self.func.data_nodes[node as usize].kind.scalar_type();
                BlockType::Value(ValueType::from(ty))
            }
            _ => BlockType::Empty,
        }
    }
}
