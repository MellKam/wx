use crate::mir::{self, ExprKind};
use crate::opt::{
    Block, BlockIndex, ControlNode, DataNodeIndex, DataNodeKind, Function, NodeType, ScalarType,
    StackResult,
};

pub struct Builder<'mir> {
    mir: &'mir mir::MIR,
    /// The specific MIR function being lowered (stored to avoid indexing into
    /// `mir.functions` by index, which would be wrong for non-first functions).
    mir_func: &'mir mir::Function,
    func: Function,
    /// For each MIR scope, the flat offset into `data_bindings` where its
    /// locals start. Computed once from the scope parent-chain before
    /// building begins.
    locals_offsets: Box<[u32]>,
}

impl<'mir> Builder<'mir> {
    /// Lower one MIR function into a sea-of-nodes `Function`.
    pub fn build(mir: &'mir mir::MIR, mir_func: &'mir mir::Function) -> Function {
        let locals_offsets = Self::compute_locals_offsets(&mir_func.scopes);
        let mut b = Builder {
            mir,
            mir_func,
            func: Function::new(mir_func.id, mir_func.scopes.len()),
            locals_offsets,
        };
        b.build_function();
        b.func
    }

    /// Compute the flat `data_bindings` offset for each scope.
    ///
    /// Each scope's locals are appended directly after its parent's locals.
    /// Sibling scopes share the same offset range (they are never active
    /// simultaneously), so `data_bindings` grows to the depth of the
    /// deepest path through the scope tree.
    fn compute_locals_offsets(scopes: &[mir::BlockScope]) -> Box<[u32]> {
        let mut offsets = Vec::with_capacity(scopes.len());
        offsets.push(0u32);
        for scope in scopes.iter().skip(1) {
            let parent = scope.parent.unwrap() as usize;
            offsets.push(offsets[parent] + scopes[parent].locals.len() as u32);
        }
        offsets.into_boxed_slice()
    }

    fn build_function(&mut self) {
        let mir_func = self.mir_func;
        let root_scope = &mir_func.scopes[0];
        let sig = &self.mir.signatures[mir_func.signature_index as usize];

        // Seed data_bindings for the root scope (params + non-param locals).
        let mut data_bindings = vec![StackResult::Unit; root_scope.locals.len()];

        let params_count = sig.params_count;
        // `wasm_idx` tracks the flattened WASM local index: aggregate params
        // occupy one slot per field, scalar params occupy one slot each.
        let mut wasm_idx = 0u32;
        for (i, local) in root_scope.locals[..params_count].iter().enumerate() {
            data_bindings[i] = match local.ty {
                mir::Type::Aggregate { aggregate_index } => {
                    let fields: Box<[_]> = self.mir.aggregates[aggregate_index as usize]
                        .values
                        .iter()
                        .map(|&ft| {
                            let ty =
                                ScalarType::try_from(ft).expect("aggregate field must be scalar");
                            let node = self.func.ensure_node(DataNodeKind::Param {
                                index: wasm_idx,
                                ty,
                            });
                            wasm_idx += 1;
                            node
                        })
                        .collect();
                    StackResult::Value(self.func.ensure_node(DataNodeKind::Aggregate {
                        fields,
                        aggregate_index,
                    }))
                }
                _ => {
                    let ty = ScalarType::try_from(local.ty).expect("param must be scalar");
                    let node = self.func.ensure_node(DataNodeKind::Param {
                        index: wasm_idx,
                        ty,
                    });
                    wasm_idx += 1;
                    StackResult::Value(node)
                }
            };
        }
        for (i, local) in root_scope.locals[params_count..].iter().enumerate() {
            data_bindings[params_count + i] = self.default_value(local.ty);
        }

        self.func.blocks[0] = Some(Block {
            is_loop: false,
            parent: None,
            statements: Vec::new(),
            result: StackResult::Never,
        });

        let body_exprs = match &mir_func.block.kind {
            ExprKind::Block { expressions, .. } => expressions,
            _ => unreachable!("function body must be a Block"),
        };

        // MIR uses an implicit return: the last expression's value is returned
        // without an explicit `return` keyword. Capture it and emit a Return node.
        let mut last = StackResult::Unit;
        for expr in body_exprs.iter() {
            last = self.build_expr(0, &mut data_bindings, expr);
            if last == StackResult::Never {
                break;
            }
        }

        if last != StackResult::Never {
            let fn_result = self.func.blocks[0].as_ref().unwrap().result;
            let merged = self.merge_stack_results(fn_result, last);
            self.func.blocks[0].as_mut().unwrap().result = merged;
            self.push_stmt(0, ControlNode::Return { value: last });
        }
    }

    // ── Expression builder ────────────────────────────────────────────────────

    fn build_expr(
        &mut self,
        block_idx: BlockIndex,
        bindings: &mut Vec<StackResult>,
        expr: &mir::Expression,
    ) -> StackResult {
        match &expr.kind {
            // ── Literals ──────────────────────────────────────────────────
            ExprKind::Int { value } => {
                let ty = ScalarType::try_from(expr.ty).expect("Int must be scalar");
                StackResult::Value(
                    self.func
                        .ensure_node(DataNodeKind::Int { value: *value, ty }),
                )
            }
            ExprKind::Float { value } => {
                let ty = ScalarType::try_from(expr.ty).expect("Float must be scalar");
                let bits = match ty {
                    ScalarType::F32 => (*value as f32).to_bits() as u64,
                    _ => value.to_bits(),
                };
                StackResult::Value(self.func.ensure_node(DataNodeKind::Float { bits, ty }))
            }
            ExprKind::Bool { value } => {
                let node = self.func.ensure_node(DataNodeKind::Int {
                    value: if *value { 1 } else { 0 },
                    ty: ScalarType::I32,
                });
                StackResult::Value(node)
            }
            ExprKind::Noop => StackResult::Unit,

            // ── Locals ───────────────────────────────────────────────────
            ExprKind::LocalGet {
                scope_index,
                local_index,
            } => {
                let idx = self.flat_index(*scope_index, *local_index);
                self.ensure_bindings_capacity(bindings, idx + 1);
                bindings[idx]
            }
            ExprKind::LocalSet {
                scope_index,
                local_index,
                value,
            } => {
                let new_val = self.build_expr(block_idx, bindings, value);
                let idx = self.flat_index(*scope_index, *local_index);
                self.ensure_bindings_capacity(bindings, idx + 1);
                bindings[idx] = new_val;
                StackResult::Unit
            }

            // ── Module-level state ────────────────────────────────────────
            ExprKind::Global { id } => {
                let node = self.func.ensure_node(DataNodeKind::GlobalGet { id: *id });
                StackResult::Value(node)
            }
            ExprKind::GlobalSet { id, value } => {
                let val = self.build_expr(block_idx, bindings, value).unwrap_value();
                self.push_stmt(
                    block_idx,
                    ControlNode::GlobalSet {
                        id: *id,
                        value: val,
                    },
                );
                StackResult::Unit
            }

            // ── Constants / refs ─────────────────────────────────────────
            ExprKind::Function { id } => {
                let node = self.func.ensure_node(DataNodeKind::FunctionRef { id: *id });
                StackResult::Value(node)
            }
            ExprKind::StaticPointer { data_index } => {
                let node = self.func.ensure_node(DataNodeKind::StaticDataRef {
                    data_index: *data_index,
                });
                StackResult::Value(node)
            }
            ExprKind::MemoryOffset { memory } => {
                let node = self.func.ensure_node(DataNodeKind::MemoryOffset { memory: *memory });
                StackResult::Value(node)
            }
            ExprKind::MemoryIndex { memory } => {
                let node = self.func.ensure_node(DataNodeKind::MemoryIndex { memory: *memory });
                StackResult::Value(node)
            }
            ExprKind::MemorySize { memory } => {
                let result_node =
                    self.func.ensure_node(DataNodeKind::MemorySizeResult { memory: *memory });
                self.push_stmt(
                    block_idx,
                    ControlNode::MemorySize {
                        memory: *memory,
                        result: result_node,
                    },
                );
                StackResult::Value(result_node)
            }

            // ── Arithmetic ────────────────────────────────────────────────
            ExprKind::Add { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::Add {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::Sub { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::Sub {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::Mul { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::Mul {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::Div { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::Div {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::Rem { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::Rem {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }

            // ── Bitwise ───────────────────────────────────────────────────
            ExprKind::And { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::BitAnd {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::Or { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::BitOr {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::BitAnd { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::BitAnd {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::BitOr { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::BitOr {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::BitXor { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::BitXor {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::LeftShift { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::Shl {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::RightShift { left, right } => {
                self.build_binary(block_idx, bindings, left, right, expr.ty, |l, r, ty| {
                    DataNodeKind::ShrS {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }

            // ── Comparisons ───────────────────────────────────────────────
            ExprKind::Eq { left, right } => {
                self.build_cmp(block_idx, bindings, left, right, |l, r, ty| {
                    DataNodeKind::Eq {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::NotEq { left, right } => {
                self.build_cmp(block_idx, bindings, left, right, |l, r, ty| {
                    DataNodeKind::NotEq {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::Less { left, right } => {
                self.build_cmp(block_idx, bindings, left, right, |l, r, ty| {
                    DataNodeKind::LtS {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::LessEq { left, right } => {
                self.build_cmp(block_idx, bindings, left, right, |l, r, ty| {
                    DataNodeKind::LtEqS {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::Greater { left, right } => {
                self.build_cmp(block_idx, bindings, left, right, |l, r, ty| {
                    DataNodeKind::GtS {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }
            ExprKind::GreaterEq { left, right } => {
                self.build_cmp(block_idx, bindings, left, right, |l, r, ty| {
                    DataNodeKind::GtEqS {
                        left: l,
                        right: r,
                        ty,
                    }
                })
            }

            // ── Unary ─────────────────────────────────────────────────────
            ExprKind::Neg { value } => {
                let ty = ScalarType::try_from(expr.ty).expect("Neg must be scalar");
                let operand = self.build_expr(block_idx, bindings, value).unwrap_value();
                StackResult::Value(self.func.ensure_node(DataNodeKind::Neg { operand, ty }))
            }
            ExprKind::BitNot { value } => {
                let ty = ScalarType::try_from(expr.ty).expect("BitNot must be scalar");
                let operand = self.build_expr(block_idx, bindings, value).unwrap_value();
                StackResult::Value(self.func.ensure_node(DataNodeKind::BitNot { operand, ty }))
            }
            ExprKind::Eqz { value } => {
                let operand = self.build_expr(block_idx, bindings, value).unwrap_value();
                StackResult::Value(self.func.ensure_node(DataNodeKind::Eqz { operand }))
            }

            // ── Aggregates ────────────────────────────────────────────────
            ExprKind::Aggregate { values } => {
                let aggregate_index = match expr.ty {
                    mir::Type::Aggregate { aggregate_index } => aggregate_index,
                    _ => panic!("Aggregate expression must have Aggregate type"),
                };
                let fields: Box<[_]> = values
                    .iter()
                    .map(|v| self.build_expr(block_idx, bindings, v).unwrap_value())
                    .collect();
                let node = self.func.ensure_node(DataNodeKind::Aggregate {
                    fields,
                    aggregate_index,
                });
                StackResult::Value(node)
            }
            ExprKind::AggregateGet {
                scope_index,
                local_index,
                value_index,
            } => {
                let idx = self.flat_index(*scope_index, *local_index);
                self.ensure_bindings_capacity(bindings, idx + 1);
                let aggregate = bindings[idx].unwrap_value();
                let agg_ty = &self.mir.aggregates[{
                    match self.func.data_nodes[aggregate as usize].kind.node_type() {
                        NodeType::Aggregate(i) => i as usize,
                        _ => panic!("AggregateGet on non-aggregate binding"),
                    }
                }];
                let field_ty = ScalarType::try_from(agg_ty.values[*value_index as usize])
                    .expect("aggregate field must be scalar");
                let node = self.func.ensure_node(DataNodeKind::AggregateGet {
                    aggregate,
                    field_index: *value_index,
                    ty: field_ty,
                });
                StackResult::Value(node)
            }

            // ── Control flow ──────────────────────────────────────────────
            ExprKind::Return { value } => {
                let result = match value {
                    Some(v) => self.build_expr(block_idx, bindings, v),
                    None => StackResult::Unit,
                };
                // Merge return value into function block result.
                let fn_result = self.func.blocks[0].as_ref().unwrap().result;
                let merged = self.merge_stack_results(fn_result, result);
                self.func.blocks[0].as_mut().unwrap().result = merged;
                self.push_stmt(block_idx, ControlNode::Return { value: result });
                StackResult::Never
            }
            ExprKind::Drop { value } => {
                self.build_expr(block_idx, bindings, value);
                StackResult::Unit
            }
            ExprKind::Unreachable => {
                self.push_stmt(block_idx, ControlNode::Unreachable);
                StackResult::Never
            }

            ExprKind::Block {
                scope_index,
                expressions,
            } => self.build_block_expr(block_idx, bindings, *scope_index, expressions),
            ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => self.build_if_else(
                block_idx,
                bindings,
                condition,
                then_block,
                else_block.as_deref(),
            ),
            ExprKind::Loop { scope_index, block } => {
                self.build_loop(block_idx, bindings, *scope_index, block)
            }
            ExprKind::Break { scope_index, value } => {
                let val = match value {
                    Some(v) => self.build_expr(block_idx, bindings, v),
                    None => StackResult::Unit,
                };
                // Propagate break value into the target block's result.
                let target = *scope_index as BlockIndex;
                let existing = self.func.blocks[target as usize].as_ref().unwrap().result;
                let merged = self.merge_stack_results(existing, val);
                self.func.blocks[target as usize].as_mut().unwrap().result = merged;
                self.push_stmt(block_idx, ControlNode::Break { target, value: val });
                StackResult::Never
            }
            ExprKind::Continue { scope_index } => {
                self.push_stmt(
                    block_idx,
                    ControlNode::Continue {
                        target: *scope_index as BlockIndex,
                    },
                );
                StackResult::Never
            }

            // ── Calls ─────────────────────────────────────────────────────
            ExprKind::Call { callee, arguments } => {
                self.build_call(block_idx, bindings, callee, arguments, expr.ty)
            }

            // ── Memory ────────────────────────────────────────────────────
            ExprKind::PointerLoad { pointer, offset: base_offset, memory } => {
                let address = self.build_expr(block_idx, bindings, pointer).unwrap_value();
                match expr.ty {
                    mir::Type::Aggregate { aggregate_index } => {
                        let agg = &self.mir.aggregates[aggregate_index as usize];
                        let field_tys: Vec<(ScalarType, u32)> = agg
                            .values
                            .iter()
                            .zip(agg.offsets.iter())
                            .map(|(&ft, &off)| {
                                (ScalarType::try_from(ft).expect("aggregate field must be scalar"), off)
                            })
                            .collect();
                        let fields: Box<[DataNodeIndex]> = field_tys
                            .iter()
                            .map(|&(ty, field_offset)| {
                                let result = self.func.ensure_node(DataNodeKind::PointerLoadResult {
                                    address,
                                    ty,
                                });
                                self.push_stmt(
                                    block_idx,
                                    ControlNode::PointerLoad {
                                        address,
                                        offset: base_offset + field_offset,
                                        result,
                                        memory: *memory,
                                    },
                                );
                                result
                            })
                            .collect();
                        StackResult::Value(self.func.ensure_node(DataNodeKind::Aggregate {
                            fields,
                            aggregate_index,
                        }))
                    }
                    _ => {
                        let ty = ScalarType::try_from(expr.ty).expect("pointer load must be scalar");
                        let result = self
                            .func
                            .ensure_node(DataNodeKind::PointerLoadResult { address, ty });
                        self.push_stmt(
                            block_idx,
                            ControlNode::PointerLoad { address, offset: *base_offset, result, memory: *memory },
                        );
                        StackResult::Value(result)
                    }
                }
            }

            ExprKind::PointerStore { pointer, value, offset: base_offset, memory } => {
                let address = self.build_expr(block_idx, bindings, pointer).unwrap_value();
                match value.ty {
                    mir::Type::Aggregate { .. } => {
                        let value_node = self.build_expr(block_idx, bindings, value).unwrap_value();
                        let (fields, aggregate_index) = self.extract_aggregate_fields(value_node);
                        let offsets: Vec<u32> = self.mir.aggregates[aggregate_index as usize]
                            .offsets
                            .to_vec();
                        for (&field_node, &field_offset) in fields.iter().zip(offsets.iter()) {
                            self.push_stmt(
                                block_idx,
                                ControlNode::PointerStore {
                                    address,
                                    offset: base_offset + field_offset,
                                    value: field_node,
                                    memory: *memory,
                                },
                            );
                        }
                    }
                    _ => {
                        let value_node = self.build_expr(block_idx, bindings, value).unwrap_value();
                        self.push_stmt(
                            block_idx,
                            ControlNode::PointerStore { address, offset: *base_offset, value: value_node, memory: *memory },
                        );
                    }
                }
                StackResult::Unit
            }

            ExprKind::MemoryGrow { memory, delta } => {
                let delta_node = self.build_expr(block_idx, bindings, delta).unwrap_value();
                let result_node = self.func.ensure_node(DataNodeKind::MemoryGrowResult {
                    memory: *memory,
                    delta: delta_node,
                });
                self.push_stmt(
                    block_idx,
                    ControlNode::MemoryGrow {
                        memory: *memory,
                        delta: delta_node,
                        result: result_node,
                    },
                );
                StackResult::Value(result_node)
            }
        }
    }

    // ── Control-flow builders ─────────────────────────────────────────────────

    fn build_block_expr(
        &mut self,
        block_idx: BlockIndex,
        bindings: &mut Vec<StackResult>,
        scope_index: mir::ScopeIndex,
        expressions: &[mir::Expression],
    ) -> StackResult {
        let mut child = self.extend_bindings(bindings, scope_index);
        let mut result = StackResult::Unit;
        for expr in expressions {
            result = self.build_expr(block_idx, &mut child, expr);
            if result == StackResult::Never {
                break;
            }
        }
        // Write back mutations to parent locals.
        let parent_len = bindings.len();
        bindings[..parent_len].copy_from_slice(&child[..parent_len]);
        result
    }

    fn build_if_else(
        &mut self,
        block_idx: BlockIndex,
        bindings: &mut Vec<StackResult>,
        condition_expr: &mir::Expression,
        then_expr: &mir::Expression,
        else_expr: Option<&mir::Expression>,
    ) -> StackResult {
        let condition = self
            .build_expr(block_idx, bindings, condition_expr)
            .unwrap_value();

        let (then_scope, then_exprs) = Self::unwrap_block(then_expr);
        let mut then_bindings = self.extend_bindings(bindings, then_scope);
        self.func.blocks[then_scope as usize] = Some(Block {
            is_loop: false,
            parent: Some(block_idx),
            statements: Vec::new(),
            result: StackResult::Never,
        });
        let then_result = self.build_block_exprs(then_scope, &mut then_bindings, then_exprs);
        self.func.blocks[then_scope as usize]
            .as_mut()
            .unwrap()
            .result = then_result;

        let (else_result, else_bindings, else_scope) = match else_expr {
            Some(e) => {
                let (scope, exprs) = Self::unwrap_block(e);
                let mut eb = self.extend_bindings(bindings, scope);
                self.func.blocks[scope as usize] = Some(Block {
                    is_loop: false,
                    parent: Some(block_idx),
                    statements: Vec::new(),
                    result: StackResult::Never,
                });
                let r = self.build_block_exprs(scope, &mut eb, exprs);
                self.func.blocks[scope as usize].as_mut().unwrap().result = r;
                (r, eb, Some(scope))
            }
            None => (StackResult::Unit, bindings.clone(), None),
        };

        let parent_len = bindings.len();
        let mut outputs = Vec::new();
        let result = self.merge_branches(
            then_result,
            else_result,
            &then_bindings,
            &else_bindings,
            parent_len,
            bindings,
            &mut outputs,
        );

        self.push_stmt(
            block_idx,
            ControlNode::IfElse {
                condition,
                then_block: then_scope,
                else_block: else_scope,
                outputs: outputs.into_boxed_slice(),
                result,
            },
        );
        result
    }

    fn build_loop(
        &mut self,
        parent_block: BlockIndex,
        bindings: &mut Vec<StackResult>,
        _scope_index: mir::ScopeIndex,
        body_expr: &mir::Expression,
    ) -> StackResult {
        let (body_scope, body_exprs) = Self::unwrap_block(body_expr);
        let body_block = body_scope as BlockIndex;

        // Create loop-param placeholders for all parent bindings.
        let loop_params = self.create_loop_params(bindings, body_block);
        let mut loop_bindings = loop_params.clone();
        // Extend for the body scope's own locals.
        self.extend_bindings_in_place(&mut loop_bindings, body_scope);

        self.func.blocks[body_block as usize] = Some(Block {
            is_loop: true,
            parent: Some(parent_block),
            statements: Vec::new(),
            result: StackResult::Never,
        });
        let body_fallthrough = self.build_block_exprs(body_block, &mut loop_bindings, body_exprs);
        // blocks[body_block].result accumulates the result type from all `break`
        // statements. Save it before overwriting with the body's fallthrough
        // result (which is always Unit/Never since loops exit via break, not
        // fallthrough).
        let break_result = self.func.blocks[body_block as usize]
            .as_ref()
            .unwrap()
            .result;
        self.func.blocks[body_block as usize]
            .as_mut()
            .unwrap()
            .result = body_fallthrough;

        // Patch loop params and collect outputs.
        let mut outputs = Vec::new();
        let parent_len = bindings.len();
        for i in 0..parent_len {
            self.patch_loop_binding(i, &loop_params, &loop_bindings, bindings, &mut outputs);
        }

        self.push_stmt(
            parent_block,
            ControlNode::Loop {
                body: body_block,
                outputs: outputs.into_boxed_slice(),
                result: break_result,
            },
        );
        break_result
    }

    fn build_call(
        &mut self,
        block_idx: BlockIndex,
        bindings: &mut Vec<StackResult>,
        callee_expr: &mir::Expression,
        arguments: &[mir::Expression],
        result_ty: mir::Type,
    ) -> StackResult {
        let callee_sig = match callee_expr.ty {
            mir::Type::Function { signature_index } => signature_index,
            _ => unreachable!(),
        };
        let callee = self
            .build_expr(block_idx, bindings, callee_expr)
            .unwrap_value();
        let args: Box<[_]> = arguments
            .iter()
            .map(|a| self.build_expr(block_idx, bindings, a).unwrap_value())
            .collect();

        let result = match result_ty {
            mir::Type::Unit | mir::Type::Never => StackResult::Unit,
            mir::Type::Aggregate { aggregate_index } => StackResult::Value(
                self.func
                    .ensure_node(DataNodeKind::AggregateCallResult { aggregate_index }),
            ),
            _ => {
                let ty = ScalarType::try_from(result_ty).expect("scalar call result type");
                StackResult::Value(self.func.ensure_node(DataNodeKind::CallResult {
                    callee,
                    args: args.clone(),
                    ty,
                }))
            }
        };

        self.push_stmt(
            block_idx,
            ControlNode::Call {
                callee,
                args,
                result,
                callee_sig,
            },
        );
        result
    }

    // ── Binding helpers ───────────────────────────────────────────────────────

    /// Extend `parent` bindings with the locals of `scope_index`.
    fn extend_bindings(
        &mut self,
        parent: &[StackResult],
        scope_index: mir::ScopeIndex,
    ) -> Vec<StackResult> {
        let mut child = parent.to_vec();
        for local in &self.mir_func.scopes[scope_index as usize].locals {
            child.push(self.default_value(local.ty));
        }
        child
    }

    /// Extend `bindings` in-place with the locals of `scope_index`.
    fn extend_bindings_in_place(
        &mut self,
        bindings: &mut Vec<StackResult>,
        scope_index: mir::ScopeIndex,
    ) {
        for local in &self.mir_func.scopes[scope_index as usize].locals {
            bindings.push(self.default_value(local.ty));
        }
    }

    /// Create loop-param placeholders for every scalar / aggregate binding in
    /// `parent`.
    fn create_loop_params(
        &mut self,
        parent: &[StackResult],
        block_index: BlockIndex,
    ) -> Vec<StackResult> {
        let mut params = Vec::with_capacity(parent.len());
        for &binding in parent {
            let param = match binding {
                StackResult::Value(node_id) => {
                    match self.func.data_nodes[node_id as usize].kind.node_type() {
                        NodeType::Scalar(ty) => {
                            let lp = self.func.push_loop_param(block_index, node_id, ty);
                            StackResult::Value(lp)
                        }
                        NodeType::Aggregate(_) => {
                            // One loop-param per field; reassemble as an Aggregate node.
                            let (fields, aggregate_index) = self.extract_aggregate_fields(node_id);
                            let agg_def = &self.mir.aggregates[aggregate_index as usize];
                            let lp_fields: Box<[_]> = fields
                                .iter()
                                .zip(agg_def.values.iter())
                                .map(|(&field_node, &field_mir_ty)| {
                                    let ty = ScalarType::try_from(field_mir_ty)
                                        .expect("aggregate field must be scalar");
                                    self.func.push_loop_param(block_index, field_node, ty)
                                })
                                .collect();
                            let new_agg = self.func.ensure_node(DataNodeKind::Aggregate {
                                fields: lp_fields,
                                aggregate_index,
                            });
                            StackResult::Value(new_agg)
                        }
                    }
                }
                other => other,
            };
            params.push(param);
        }
        params
    }

    /// Patch loop params for binding `i` once the loop body is built.
    fn patch_loop_binding(
        &mut self,
        i: usize,
        loop_params: &[StackResult],
        loop_final: &[StackResult],
        parent_bindings: &mut Vec<StackResult>,
        outputs: &mut Vec<DataNodeIndex>,
    ) {
        let param = match loop_params[i] {
            StackResult::Value(n) => n,
            _ => return,
        };
        let after = match loop_final[i] {
            StackResult::Value(n) => n,
            _ => return,
        };

        // If loop_final still holds the LoopParam (or the same aggregate wrapper)
        // that was installed at loop entry, the binding was never written inside
        // the loop body.  Restore the parent binding to the pre-loop value and
        // skip patching so we don't create a self-referential LoopParam node.
        if param == after {
            let before = match self.func.data_nodes[param as usize].kind {
                DataNodeKind::LoopParam { before, .. } => before,
                // Aggregate wrapper whose fields were all unmodified.
                _ => {
                    // Restore the parent binding to whatever it was before the loop.
                    // The original value was `loop_params[i]`'s `before` field, but
                    // for aggregates we just leave the binding as-is (it's already correct
                    // since the aggregate node CSE-deduplicates to the pre-loop one).
                    return;
                }
            };
            parent_bindings[i] = StackResult::Value(before);
            return;
        }

        match self.func.data_nodes[param as usize].kind.node_type() {
            NodeType::Scalar(_) => {
                self.func.patch_loop_param(param, after);
                // Only expose as output if the binding was actually mutated.
                if matches!(self.func.data_nodes[param as usize].kind, DataNodeKind::LoopParam { before, after, .. } if before != after)
                {
                    parent_bindings[i] = StackResult::Value(param);
                    outputs.push(param);
                }
            }
            NodeType::Aggregate(_) => {
                let (lp_fields, aggregate_index) = self.extract_aggregate_fields(param);
                let (after_fields, _) = self.extract_aggregate_fields(after);
                let mut any_changed = false;
                let mut new_fields = lp_fields.to_vec();
                for (j, (&lp_field, &after_field)) in
                    lp_fields.iter().zip(after_fields.iter()).enumerate()
                {
                    self.func.patch_loop_param(lp_field, after_field);
                    if matches!(self.func.data_nodes[lp_field as usize].kind, DataNodeKind::LoopParam { before, after, .. } if before != after)
                    {
                        new_fields[j] = lp_field;
                        outputs.push(lp_field);
                        any_changed = true;
                    }
                }
                if any_changed {
                    let new_agg = self.func.ensure_node(DataNodeKind::Aggregate {
                        fields: new_fields.into_boxed_slice(),
                        aggregate_index,
                    });
                    parent_bindings[i] = StackResult::Value(new_agg);
                }
            }
        }
    }

    /// Merge bindings from two branches, creating Phi nodes for values that
    /// differ. Updates `parent_bindings` with the merged results and
    /// appends phi indices to `outputs`.
    #[allow(clippy::too_many_arguments)]
    fn merge_branches(
        &mut self,
        then_result: StackResult,
        else_result: StackResult,
        then_bindings: &[StackResult],
        else_bindings: &[StackResult],
        parent_len: usize,
        parent_bindings: &mut Vec<StackResult>,
        outputs: &mut Vec<DataNodeIndex>,
    ) -> StackResult {
        for i in 0..parent_len {
            let t = then_bindings[i];
            let e = else_bindings[i];
            if t == e {
                parent_bindings[i] = t;
                continue;
            }
            match (t, e) {
                (StackResult::Value(l), StackResult::Value(r)) => {
                    let merged = self.merge_values(l, r, outputs);
                    parent_bindings[i] = StackResult::Value(merged);
                }
                (StackResult::Never, other) | (other, StackResult::Never) => {
                    parent_bindings[i] = other;
                }
                _ => {}
            }
        }
        // Merge the branch expression results. Any phi created here must also
        // go into `outputs` so the scheduler can pre-allocate its local.
        match (then_result, else_result) {
            (StackResult::Never, other) | (other, StackResult::Never) => other,
            (StackResult::Unit, StackResult::Unit) => StackResult::Unit,
            (StackResult::Value(l), StackResult::Value(r)) => {
                StackResult::Value(self.merge_values(l, r, outputs))
            }
            _ => panic!(
                "cannot merge branch results {:?} and {:?}",
                then_result, else_result
            ),
        }
    }

    /// Merge two scalar-or-aggregate value nodes, creating Phi(s) as needed.
    fn merge_values(
        &mut self,
        l: DataNodeIndex,
        r: DataNodeIndex,
        outputs: &mut Vec<DataNodeIndex>,
    ) -> DataNodeIndex {
        match (
            self.func.data_nodes[l as usize].kind.node_type(),
            self.func.data_nodes[r as usize].kind.node_type(),
        ) {
            (NodeType::Scalar(ty), NodeType::Scalar(_)) => {
                let phi = self.func.ensure_node(DataNodeKind::Phi {
                    left: l,
                    right: r,
                    ty,
                });
                if phi != l && phi != r {
                    outputs.push(phi);
                }
                phi
            }
            (NodeType::Aggregate(_), NodeType::Aggregate(_)) => {
                let (l_fields, aggregate_index) = self.extract_aggregate_fields(l);
                let (r_fields, _) = self.extract_aggregate_fields(r);
                let agg_def = &self.mir.aggregates[aggregate_index as usize];
                let phi_fields: Box<[_]> = l_fields
                    .iter()
                    .zip(r_fields.iter())
                    .zip(agg_def.values.iter())
                    .map(|((&lf, &rf), &ft)| {
                        if lf == rf {
                            return lf;
                        }
                        let ty = ScalarType::try_from(ft).expect("aggregate field must be scalar");
                        let phi = self.func.ensure_node(DataNodeKind::Phi {
                            left: lf,
                            right: rf,
                            ty,
                        });
                        if phi != lf && phi != rf {
                            outputs.push(phi);
                        }
                        phi
                    })
                    .collect();
                self.func.ensure_node(DataNodeKind::Aggregate {
                    fields: phi_fields,
                    aggregate_index,
                })
            }
            _ => panic!("type mismatch when merging branch values"),
        }
    }

    /// Return the per-field data node indices for any aggregate node.
    ///
    /// For `Aggregate { fields }` the existing field nodes are returned
    /// directly. For `AggregateCallResult` — which has no concrete fields —
    /// a fresh `AggregateGet` node is synthesized for each field.  Those
    /// get nodes do not fold (the fold only applies to `Aggregate`), so
    /// they remain visible to the scheduler, which reads them from
    /// `node_to_aggregate_locals`.
    fn extract_aggregate_fields(
        &mut self,
        node: DataNodeIndex,
    ) -> (Box<[DataNodeIndex]>, mir::AggregateIndex) {
        match self.func.data_nodes[node as usize].kind.clone() {
            DataNodeKind::Aggregate {
                fields,
                aggregate_index,
            } => (fields, aggregate_index),
            DataNodeKind::AggregateCallResult { aggregate_index } => {
                let fields: Box<[_]> = self.mir.aggregates[aggregate_index as usize]
                    .values
                    .iter()
                    .enumerate()
                    .map(|(i, &ft)| {
                        let ty = ScalarType::try_from(ft).expect("aggregate field must be scalar");
                        self.func.ensure_node(DataNodeKind::AggregateGet {
                            aggregate: node,
                            field_index: i as u32,
                            ty,
                        })
                    })
                    .collect();
                (fields, aggregate_index)
            }
            _ => panic!("expected aggregate node"),
        }
    }

    /// Merge two `StackResult`s at a control-flow join. `Never` defers to the
    /// other side.
    fn merge_stack_results(&mut self, a: StackResult, b: StackResult) -> StackResult {
        match (a, b) {
            (StackResult::Never, other) | (other, StackResult::Never) => other,
            (StackResult::Unit, StackResult::Unit) => StackResult::Unit,
            (StackResult::Value(l), StackResult::Value(r)) => {
                let mut dummy = Vec::new();
                StackResult::Value(self.merge_values(l, r, &mut dummy))
            }
            _ => panic!("cannot merge {:?} and {:?}", a, b),
        }
    }

    // ── Small helpers ─────────────────────────────────────────────────────────

    fn build_block_exprs(
        &mut self,
        block_idx: BlockIndex,
        bindings: &mut Vec<StackResult>,
        expressions: &[mir::Expression],
    ) -> StackResult {
        let mut result = StackResult::Unit;
        for expr in expressions {
            result = self.build_expr(block_idx, bindings, expr);
            if result == StackResult::Never {
                break;
            }
        }
        result
    }

    fn build_binary(
        &mut self,
        block_idx: BlockIndex,
        bindings: &mut Vec<StackResult>,
        left: &mir::Expression,
        right: &mir::Expression,
        result_ty: mir::Type,
        make: impl FnOnce(DataNodeIndex, DataNodeIndex, ScalarType) -> DataNodeKind,
    ) -> StackResult {
        let ty = ScalarType::try_from(result_ty).expect("binary op must be scalar");
        let l = self.build_expr(block_idx, bindings, left).unwrap_value();
        let r = self.build_expr(block_idx, bindings, right).unwrap_value();
        StackResult::Value(self.func.ensure_node(make(l, r, ty)))
    }

    fn build_cmp(
        &mut self,
        block_idx: BlockIndex,
        bindings: &mut Vec<StackResult>,
        left: &mir::Expression,
        right: &mir::Expression,
        make: impl FnOnce(DataNodeIndex, DataNodeIndex, ScalarType) -> DataNodeKind,
    ) -> StackResult {
        // Comparison operand type comes from the operands, not the result (which is
        // always I32).
        let l = self.build_expr(block_idx, bindings, left).unwrap_value();
        let r = self.build_expr(block_idx, bindings, right).unwrap_value();
        let operand_ty = self.func.data_nodes[l as usize].kind.scalar_type();
        StackResult::Value(self.func.ensure_node(make(l, r, operand_ty)))
    }

    /// Flat index into `data_bindings` for a MIR (scope_index, local_index)
    /// pair.
    fn flat_index(&self, scope_index: mir::ScopeIndex, local_index: mir::LocalIndex) -> usize {
        (self.locals_offsets[scope_index as usize] + local_index) as usize
    }

    fn ensure_bindings_capacity(&self, bindings: &mut Vec<StackResult>, len: usize) {
        if bindings.len() < len {
            bindings.resize(len, StackResult::Unit);
        }
    }

    fn default_value(&mut self, ty: mir::Type) -> StackResult {
        match ty {
            mir::Type::Unit | mir::Type::Never => StackResult::Unit,
            mir::Type::Aggregate { aggregate_index } => {
                let field_types: Vec<mir::Type> = self.mir.aggregates[aggregate_index as usize]
                    .values
                    .to_vec();
                let fields: Box<[_]> = field_types
                    .iter()
                    .map(|&ft| self.default_value(ft).unwrap_value())
                    .collect();
                StackResult::Value(self.func.ensure_node(DataNodeKind::Aggregate {
                    fields,
                    aggregate_index,
                }))
            }
            _ => {
                let scalar_ty = ScalarType::try_from(ty).expect("unexpected local type");
                let node = match scalar_ty {
                    ScalarType::F32 => self.func.ensure_node(DataNodeKind::Float {
                        bits: 0,
                        ty: ScalarType::F32,
                    }),
                    ScalarType::F64 => self.func.ensure_node(DataNodeKind::Float {
                        bits: 0,
                        ty: ScalarType::F64,
                    }),
                    _ => self.func.ensure_node(DataNodeKind::Int {
                        value: 0,
                        ty: scalar_ty,
                    }),
                };
                StackResult::Value(node)
            }
        }
    }

    fn push_stmt(&mut self, block_idx: BlockIndex, stmt: ControlNode) {
        self.func.blocks[block_idx as usize]
            .as_mut()
            .unwrap()
            .statements
            .push(stmt);
    }

    /// Expect a `Block` expression and return its scope index + expressions.
    fn unwrap_block(expr: &mir::Expression) -> (mir::ScopeIndex, &[mir::Expression]) {
        match &expr.kind {
            ExprKind::Block {
                scope_index,
                expressions,
            } => (*scope_index, expressions),
            _ => panic!("expected Block expression"),
        }
    }
}
