use crate::mir::{self, *};
use crate::{ast, hir};

pub struct Builder<'hir> {
    hir: &'hir hir::HIR,
    mir: MIR,
    locals_offsets: Box<[u32]>,
    func_index: usize,
}

impl<'hir> Builder<'hir> {
    pub fn build_function(hir: &'hir hir::HIR, func_index: usize) -> Result<MIR, ()> {
        let func = &hir.functions[func_index];

        let mut locals_offsets: Vec<u32> = Vec::with_capacity(func.stack.scopes.len());
        locals_offsets.push(0);
        for scope in func.stack.scopes.iter().skip(1) {
            let parent_index = scope.parent.unwrap().0 as usize;
            let locals_offset =
                &locals_offsets[parent_index] + func.stack.scopes[parent_index].locals.len() as u32;
            locals_offsets.push(locals_offset);
        }

        let mut builder = Builder {
            hir,
            mir: MIR {
                name: func.name.symbol,
                params: func
                    .params
                    .iter()
                    .map(|param| mir::FunctionParam {
                        name: param.name.symbol,
                        ty: ResultType::from_hir(hir, param.ty.ty).unwrap_value(),
                    })
                    .collect(),
                data_nodes: Vec::new(),
                data_lookup: HashMap::new(),
                blocks: vec![None; func.stack.scopes.len()].into_boxed_slice(),
            },
            locals_offsets: locals_offsets.into_boxed_slice(),
            func_index,
        };

        match &func.block.kind {
            hir::ExprKind::Block {
                scope_index: _,
                expressions,
                result,
            } => {
                let root_scope = func.stack.scopes.first().unwrap();
                let mut data_bindings: Vec<StackResult> =
                    Vec::with_capacity(root_scope.locals.len());
                for (local_index, local) in
                    root_scope.locals[0..func.params.len()].iter().enumerate()
                {
                    let node_index = builder.mir.ensure_data_node(DataNodeKind::Param {
                        index: local_index as u32,
                        symbol: local.name.symbol,
                        ty: ResultType::from_hir(hir, local.ty).unwrap_value(),
                    });
                    data_bindings.push(StackResult::Value { node_index });
                }
                for local in root_scope.locals[func.params.len()..].iter() {
                    data_bindings.push(builder.create_default_local_value(local.ty));
                }
                builder.mir.blocks[0] = Some(Block {
                    parent_index: None,
                    statements: Vec::new(),
                    result: StackResult::Never,
                });
                let mut data_bindings = data_bindings.into_boxed_slice();

                for expr in expressions.iter() {
                    match builder.build_expr(0, &mut data_bindings, expr) {
                        StackResult::Unit => {}
                        StackResult::Never => break,
                        StackResult::Value { node_index } => {
                            panic!(
                                "unused value in statement: {:#?}",
                                builder.mir.data_nodes[node_index as usize].kind
                            );
                        }
                        StackResult::Branch { .. } => unreachable!(),
                    };
                }

                match result {
                    Some(result) => {
                        let result = builder.build_expr(0, &mut data_bindings, result);
                        match result {
                            StackResult::Value { node_index } => {
                                builder.mir.blocks[0].as_mut().unwrap().statements.push(
                                    ControlNode::Return {
                                        value: StackResult::Value { node_index },
                                    },
                                );
                                builder.mir.blocks[0].as_mut().unwrap().result =
                                    builder.mir.merge_data(
                                        builder.mir.blocks[0].as_ref().unwrap().result,
                                        StackResult::Value { node_index },
                                    );
                            }
                            _ => {}
                        }
                    }
                    None => {}
                };
            }
            _ => unreachable!(),
        }

        Ok(builder.mir)
    }

    fn create_default_local_value(&mut self, ty: hir::Type) -> StackResult {
        match ty {
            hir::Type::I32 | hir::Type::Bool | hir::Type::U32 | hir::Type::Function(_) => {
                StackResult::Value {
                    node_index: self.mir.ensure_data_node(DataNodeKind::Int {
                        value: 0,
                        ty: ValueType::I32,
                    }),
                }
            }
            hir::Type::I64 | hir::Type::U64 => StackResult::Value {
                node_index: self.mir.ensure_data_node(DataNodeKind::Int {
                    value: 0,
                    ty: ValueType::I64,
                }),
            },
            hir::Type::F32 => StackResult::Value {
                node_index: self.mir.ensure_data_node(DataNodeKind::Float {
                    value: 0,
                    ty: ValueType::F32,
                }),
            },
            hir::Type::F64 => StackResult::Value {
                node_index: self.mir.ensure_data_node(DataNodeKind::Float {
                    value: 0,
                    ty: ValueType::F64,
                }),
            },
            hir::Type::Enum(enum_index) => {
                let enum_def = &self.hir.enums[enum_index.0 as usize];
                self.create_default_local_value(enum_def.ty)
            }
            hir::Type::Unit => StackResult::Unit,
            hir::Type::Never => StackResult::Never,
            hir::Type::Unknown => panic!("unknown type"),
        }
    }

    fn create_block_data_bindings(
        &mut self,
        parent_data_bindings: &Box<[StackResult]>,
        block_index: BlockIndex,
    ) -> Box<[StackResult]> {
        let scope = &self.hir.functions[self.func_index].stack.scopes[block_index as usize];
        let mut data_bindings = Vec::with_capacity(parent_data_bindings.len() + scope.locals.len());
        unsafe {
            data_bindings.set_len(parent_data_bindings.len() + scope.locals.len());
        }
        data_bindings[..parent_data_bindings.len()].copy_from_slice(&parent_data_bindings[..]);
        for (local_index, local) in scope.locals.iter().enumerate() {
            data_bindings[parent_data_bindings.len() + local_index] =
                self.create_default_local_value(local.ty);
        }

        data_bindings.into_boxed_slice()
    }

    fn create_loop_data_bindings(
        &mut self,
        parent_data_bindings: &Box<[StackResult]>,
        block_index: BlockIndex,
    ) -> Box<[StackResult]> {
        let scope = &self.hir.functions[self.func_index].stack.scopes[block_index as usize];
        let mut data_bindings = Vec::with_capacity(parent_data_bindings.len() + scope.locals.len());
        unsafe {
            data_bindings.set_len(parent_data_bindings.len() + scope.locals.len());
        }

        for i in 0..parent_data_bindings.len() {
            let node_id = parent_data_bindings[i].unwrap_value();
            let ty = self.mir.data_nodes[node_id as usize].kind.ty();
            data_bindings[i] = StackResult::Value {
                node_index: self.mir.data_nodes.len() as u32,
            };
            self.mir.data_nodes.push(DataNode {
                kind: DataNodeKind::LoopParam {
                    block_index,
                    before: node_id,
                    after: node_id,
                    ty,
                },
                uses: Vec::new(),
            });
        }
        for (local_index, local) in scope.locals.iter().enumerate() {
            let defalut_value = self.create_default_local_value(local.ty);
            data_bindings[parent_data_bindings.len() + local_index] = match defalut_value {
                StackResult::Value { node_index } => {
                    let loop_param_id = self.mir.data_nodes.len() as u32;
                    self.mir.data_nodes.push(DataNode {
                        kind: DataNodeKind::LoopParam {
                            block_index,
                            before: node_index,
                            after: node_index,
                            ty: mir::ResultType::from_hir(self.hir, local.ty).unwrap_value(),
                        },
                        uses: Vec::new(),
                    });
                    StackResult::Value {
                        node_index: loop_param_id,
                    }
                }
                _ => defalut_value,
            };
        }

        data_bindings.into_boxed_slice()
    }

    fn build_block(
        &mut self,
        block_index: BlockIndex,
        data_bindings: &mut Box<[StackResult]>,
        expressions: &Box<[hir::Expression]>,
        result: &Option<Box<hir::Expression>>,
    ) -> StackResult {
        for expr in expressions.iter() {
            match self.build_expr(block_index, data_bindings, expr) {
                StackResult::Unit => {}
                StackResult::Never | StackResult::Branch { .. } => {
                    return self.mir.blocks[block_index as usize]
                        .as_ref()
                        .unwrap()
                        .result;
                }
                StackResult::Value { node_index } => {
                    panic!(
                        "unused value in statement: {:#?}",
                        self.mir.data_nodes[node_index as usize].kind
                    );
                }
            };
        }

        match result {
            Some(result) => {
                let result = self.build_expr(block_index, data_bindings, result);
                let result = self.mir.merge_data(
                    self.mir.blocks[block_index as usize]
                        .as_ref()
                        .unwrap()
                        .result,
                    result,
                );

                self.mir.blocks[block_index as usize]
                    .as_mut()
                    .unwrap()
                    .result = result;
                result
            }
            None => {
                let result = self.mir.merge_data(
                    self.mir.blocks[block_index as usize]
                        .as_ref()
                        .unwrap()
                        .result,
                    StackResult::Unit,
                );
                self.mir.blocks[block_index as usize]
                    .as_mut()
                    .unwrap()
                    .result = result;
                result
            }
        }
    }

    fn build_loop_block(
        &mut self,
        block_index: BlockIndex,
        data_bindings: &mut Box<[StackResult]>,
        expressions: &Box<[hir::Expression]>,
        result: &Option<Box<hir::Expression>>,
    ) -> StackResult {
        for expr in expressions.iter() {
            match self.build_expr(block_index, data_bindings, expr) {
                StackResult::Unit => {}
                StackResult::Never | StackResult::Branch { .. } => {
                    return self.mir.blocks[block_index as usize]
                        .as_ref()
                        .unwrap()
                        .result;
                }
                StackResult::Value { node_index } => {
                    panic!(
                        "unused value in statement: {:#?}",
                        self.mir.data_nodes[node_index as usize].kind
                    );
                }
            };
        }

        match result {
            Some(result) => {
                let result = self.build_expr(block_index, data_bindings, result);
                println!("Loop block result before merge: {:#?}", result);
                println!(
                    "Merging into existing loop block result: {:#?}",
                    self.mir.blocks[block_index as usize]
                        .as_ref()
                        .unwrap()
                        .result
                );
                self.mir.blocks[block_index as usize]
                    .as_ref()
                    .unwrap()
                    .result
            }
            None => {
                self.mir.blocks[block_index as usize]
                    .as_ref()
                    .unwrap()
                    .result
            }
        }
    }

    fn build_if_else_expr(
        &mut self,
        block_index: BlockIndex,
        data_bindings: &mut Box<[StackResult]>,
        expr: &hir::Expression,
    ) -> StackResult {
        let (condition, then_block, else_block) = match &expr.kind {
            hir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => (condition, then_block, else_block),
            _ => unreachable!(),
        };

        let condition_id = self
            .build_expr(block_index, data_bindings, condition)
            .unwrap_value();
        let (then_result, then_bindings, then_block_index) = match &then_block.kind {
            hir::ExprKind::Block {
                expressions,
                result,
                scope_index,
            } => {
                let mut then_bindings =
                    self.create_block_data_bindings(&data_bindings, scope_index.0 as BlockIndex);
                self.mir.blocks[scope_index.0 as usize] = Some(Block {
                    parent_index: Some(block_index),
                    statements: Vec::new(),
                    result: StackResult::Never,
                });
                let then_result = self.build_block(
                    scope_index.0 as BlockIndex,
                    &mut then_bindings,
                    expressions,
                    result,
                );
                self.mir.blocks[scope_index.0 as usize]
                    .as_mut()
                    .unwrap()
                    .result = then_result;

                (then_result, then_bindings, scope_index.0 as BlockIndex)
            }
            _ => unreachable!(),
        };

        let (else_result, else_bindings, else_block_index) = match else_block {
            Some(else_block) => match &else_block.kind {
                hir::ExprKind::Block {
                    expressions,
                    result,
                    scope_index,
                } => {
                    let mut else_bindings = self
                        .create_block_data_bindings(&data_bindings, scope_index.0 as BlockIndex);
                    self.mir.blocks[scope_index.0 as usize] = Some(Block {
                        parent_index: Some(block_index),
                        statements: Vec::new(),
                        result: StackResult::Never,
                    });
                    let else_result = self.build_block(
                        scope_index.0 as BlockIndex,
                        &mut else_bindings,
                        expressions,
                        result,
                    );
                    self.mir.blocks[scope_index.0 as usize]
                        .as_mut()
                        .unwrap()
                        .result = else_result;

                    (
                        else_result,
                        else_bindings,
                        Some(scope_index.0 as BlockIndex),
                    )
                }
                _ => unreachable!(),
            },
            None => (StackResult::Unit, data_bindings.clone(), None),
        };

        let mut outputs = Vec::new();
        let result = match (then_result, else_result) {
            (StackResult::Never, StackResult::Value { .. } | StackResult::Unit) => {
                for i in 0..data_bindings.len() {
                    let before_value = data_bindings[i];
                    let after_value = else_bindings[i];
                    if before_value != after_value {
                        let left = before_value.unwrap_value();
                        let right = after_value.unwrap_value();
                        let ty = self.mir.data_nodes[left as usize].kind.ty();
                        let output =
                            self.mir
                                .ensure_data_node(DataNodeKind::Phi { left, right, ty });
                        outputs.push(output);
                    }
                }

                data_bindings.copy_from_slice(&else_bindings[0..data_bindings.len()]);
                else_result
            }
            (StackResult::Value { .. } | StackResult::Unit, StackResult::Never) => {
                for i in 0..data_bindings.len() {
                    let before_value = data_bindings[i];
                    let after_value = then_bindings[i];
                    if before_value != after_value {
                        let left = before_value.unwrap_value();
                        let right = after_value.unwrap_value();
                        let ty = self.mir.data_nodes[left as usize].kind.ty();
                        let output =
                            self.mir
                                .ensure_data_node(DataNodeKind::Phi { left, right, ty });
                        outputs.push(output);
                    }
                }

                data_bindings.copy_from_slice(&then_bindings[0..data_bindings.len()]);
                then_result
            }
            (StackResult::Value { .. }, StackResult::Value { .. })
            | (StackResult::Unit, StackResult::Unit) => {
                for i in 0..data_bindings.len() {
                    let then_value = then_bindings[i];
                    let else_value = else_bindings[i];
                    data_bindings[i] = if then_value == else_value {
                        then_value
                    } else {
                        let left = then_value.unwrap_value();
                        let right = else_value.unwrap_value();
                        let ty = self.mir.data_nodes[left as usize].kind.ty();
                        let output =
                            self.mir
                                .ensure_data_node(DataNodeKind::Phi { left, right, ty });
                        outputs.push(output);
                        StackResult::Value { node_index: output }
                    };
                }

                self.mir.merge_data(then_result, else_result)
            }
            _ => todo!(),
        };

        self.mir.blocks[block_index as usize]
            .as_mut()
            .unwrap()
            .statements
            .push(ControlNode::IfElse {
                condition: condition_id,
                then_block: then_block_index,
                else_block: else_block_index,
                outputs: outputs.into_boxed_slice(),
                result,
            });

        result
    }

    fn build_expr(
        &mut self,
        block_index: BlockIndex,
        data_bindings: &mut Box<[StackResult]>,
        expr: &hir::Expression,
    ) -> StackResult {
        match &expr.kind {
            hir::ExprKind::Int(value) => {
                let result = self.mir.ensure_data_node(DataNodeKind::Int {
                    value: *value,
                    ty: ResultType::from_hir(self.hir, expr.ty.unwrap()).unwrap_value(),
                });
                StackResult::Value { node_index: result }
            }
            hir::ExprKind::Bool(value) => {
                let node_index = self.mir.ensure_data_node(DataNodeKind::Int {
                    value: if *value { 1 } else { 0 },
                    ty: ValueType::I32,
                });
                StackResult::Value { node_index }
            }
            hir::ExprKind::Float(value) => {
                let node_index = self.mir.ensure_data_node(DataNodeKind::Float {
                    value: value.to_bits(),
                    ty: ResultType::from_hir(self.hir, expr.ty.unwrap()).unwrap_value(),
                });
                StackResult::Value { node_index }
            }
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let binding_index = self.locals_offsets[scope_index.0 as usize] + local_index.0;
                data_bindings[binding_index as usize]
            }
            hir::ExprKind::LocalDeclaration {
                name: _,
                scope_index,
                local_index,
                expr,
            } => {
                let result = self.build_expr(block_index, data_bindings, expr);
                let binding_index = self.locals_offsets[scope_index.0 as usize] + local_index.0;
                data_bindings[binding_index as usize] = result;
                StackResult::Unit
            }
            hir::ExprKind::Return { value } => {
                let result = match value {
                    Some(expr) => self.build_expr(block_index, data_bindings, expr),
                    None => StackResult::Unit,
                };
                self.mir.blocks[block_index as usize]
                    .as_mut()
                    .unwrap()
                    .statements
                    .push(ControlNode::Return { value: result });
                let block_result = self.mir.blocks[0].as_ref().unwrap().result;
                println!("Merging return result into function block");
                println!(
                    "Current function block result: {:#?}",
                    self.mir.blocks[0].as_ref().unwrap().result
                );
                println!("Return result: {:#?}", result);
                self.mir.blocks[0].as_mut().unwrap().result =
                    self.mir.merge_data(block_result, result);

                StackResult::Never
            }
            hir::ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => {
                let mut new_data_bindings =
                    self.create_block_data_bindings(data_bindings, scope_index.0 as BlockIndex);
                let result =
                    self.build_block(block_index, &mut new_data_bindings, expressions, result);
                data_bindings.copy_from_slice(&new_data_bindings[..data_bindings.len()]);

                result
            }
            hir::ExprKind::IfElse { .. } => {
                self.build_if_else_expr(block_index, data_bindings, expr)
            }
            hir::ExprKind::Binary { .. } => {
                self.build_binary_expr(block_index, data_bindings, expr)
            }
            hir::ExprKind::Call { .. } => self.build_call_expr(block_index, data_bindings, expr),
            hir::ExprKind::Loop { .. } => self.build_loop_expr(block_index, data_bindings, expr),
            hir::ExprKind::Break { scope_index, value } => {
                let value = match value {
                    Some(expr) => self.build_expr(block_index, data_bindings, expr),
                    None => StackResult::Unit,
                };
                self.mir.blocks[block_index as usize]
                    .as_mut()
                    .unwrap()
                    .statements
                    .push(ControlNode::Break {
                        target_block: scope_index.0 as BlockIndex,
                        value,
                    });
                self.mir.blocks[scope_index.0 as usize]
                    .as_mut()
                    .unwrap()
                    .result = self.mir.merge_data(
                    self.mir.blocks[scope_index.0 as usize]
                        .as_ref()
                        .unwrap()
                        .result,
                    value,
                );
                StackResult::Never
            }
            expr => todo!("build_expr: {:?}", expr),
        }
    }

    fn build_call_expr(
        &mut self,
        block_index: BlockIndex,
        data_bindings: &mut Box<[StackResult]>,
        expr: &hir::Expression,
    ) -> StackResult {
        let (func_index, arguments) = match &expr.kind {
            hir::ExprKind::Call { callee, arguments } => (
                match &callee.kind {
                    hir::ExprKind::Function(func_index) => func_index.0,
                    _ => unreachable!(),
                },
                arguments,
            ),
            _ => unreachable!(),
        };

        let arg_nodes = arguments
            .iter()
            .map(|arg| {
                self.build_expr(block_index, data_bindings, arg)
                    .unwrap_value()
            })
            .collect::<Box<[_]>>();

        let result_type =
            ResultType::from_hir(self.hir, self.hir.functions[func_index as usize].result.ty);
        let result = match result_type {
            ResultType::Value(ty) => {
                let node_index = self.mir.ensure_data_node(DataNodeKind::CallResult {
                    func_index,
                    args: arg_nodes.clone(),
                    ty,
                });
                StackResult::Value { node_index }
            }
            ResultType::Unit => StackResult::Unit,
            ResultType::Never => StackResult::Never,
        };

        self.mir.blocks[block_index as usize]
            .as_mut()
            .unwrap()
            .statements
            .push(ControlNode::Call {
                func_index,
                args: arg_nodes,
                result,
            });

        result
    }

    fn build_binary_expr(
        &mut self,
        block_index: BlockIndex,
        data_bindings: &mut Box<[StackResult]>,
        expr: &hir::Expression,
    ) -> StackResult {
        let (left, operator, right) = match &expr.kind {
            hir::ExprKind::Binary {
                left,
                operator,
                right,
            } => (left, operator.clone(), right),
            _ => unreachable!(),
        };

        let left_result = self.build_expr(block_index, data_bindings, &left);
        let right_result = self.build_expr(block_index, data_bindings, &right);
        if operator.kind == ast::BinOpKind::Assign {
            match &left.kind {
                hir::ExprKind::Local {
                    scope_index,
                    local_index,
                } => {
                    let binding_index = self.locals_offsets[scope_index.0 as usize] + local_index.0;
                    data_bindings[binding_index as usize] = right_result;
                    return StackResult::Unit;
                }
                _ => unimplemented!(),
            }
        }

        let left = left_result.unwrap_value();
        let right = right_result.unwrap_value();
        let ty = ResultType::from_hir(self.hir, expr.ty.unwrap()).unwrap_value();
        let node_kind = match operator.kind {
            ast::BinOpKind::Add => DataNodeKind::Add { left, right, ty },
            ast::BinOpKind::Sub => DataNodeKind::Sub { left, right, ty },
            ast::BinOpKind::Mul => DataNodeKind::Mul { left, right, ty },
            ast::BinOpKind::Div => DataNodeKind::Div { left, right, ty },
            ast::BinOpKind::Less => DataNodeKind::Lt { left, right, ty },
            ast::BinOpKind::LessEq => DataNodeKind::LtEq { left, right, ty },
            ast::BinOpKind::Greater => DataNodeKind::Gt { left, right, ty },
            ast::BinOpKind::GreaterEq => DataNodeKind::GtEq { left, right, ty },
            ast::BinOpKind::Eq => DataNodeKind::Eq { left, right, ty },
            ast::BinOpKind::NotEq => DataNodeKind::NotEq { left, right, ty },
            _ => todo!(),
        };
        StackResult::Value {
            node_index: self.mir.ensure_data_node(node_kind),
        }
    }

    fn build_loop_expr(
        &mut self,
        parent_block_index: BlockIndex,
        data_bindings: &mut Box<[StackResult]>,
        expr: &hir::Expression,
    ) -> StackResult {
        let (block_index, expressions, result) = match &expr.kind {
            hir::ExprKind::Loop { block, scope_index } => match &block.kind {
                hir::ExprKind::Block {
                    expressions,
                    result,
                    ..
                } => (scope_index.0 as BlockIndex, expressions, result),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        let loop_params = self.create_loop_data_bindings(data_bindings, block_index);
        let mut loop_data_bindings = loop_params.clone();
        self.mir.blocks[block_index as usize] = Some(Block {
            parent_index: Some(parent_block_index),
            statements: Vec::new(),
            result: StackResult::Never,
        });
        let result =
            self.build_loop_block(block_index, &mut loop_data_bindings, expressions, result);
        self.mir.blocks[block_index as usize]
            .as_mut()
            .unwrap()
            .result = result;
        println!("Loop block result: {:#?}", result);

        let mut outputs = Vec::new();
        let parent_bindings_len = data_bindings.len();
        for (index, loop_param) in loop_params.iter().copied().enumerate() {
            let loop_param_id = match loop_param {
                StackResult::Value { node_index } => node_index,
                _ => continue,
            };

            match self.mir.data_nodes[loop_param_id as usize].kind.clone() {
                DataNodeKind::LoopParam {
                    block_index,
                    before,
                    ty,
                    ..
                } => {
                    let after = loop_data_bindings[index].unwrap_value();
                    if self.mir.data_nodes[loop_param_id as usize].uses.len() > 0 {
                        self.mir.data_nodes[before as usize]
                            .uses
                            .push(loop_param_id);
                    }

                    if loop_param_id != after {
                        self.mir.data_nodes[loop_param_id as usize].kind =
                            DataNodeKind::LoopParam {
                                block_index,
                                before,
                                after,
                                ty,
                            };

                        self.mir.data_nodes[after as usize].uses.push(loop_param_id);
                        if index < parent_bindings_len {
                            data_bindings[index] = StackResult::Value {
                                node_index: loop_param_id,
                            };
                            outputs.push(loop_param_id);
                        }
                    }
                }
                _ => unreachable!(),
            }
        }

        println!("Loop outputs: {:#?}", outputs);

        self.mir.blocks[parent_block_index as usize]
            .as_mut()
            .unwrap()
            .statements
            .push(ControlNode::Loop {
                body: block_index,
                outputs: outputs.into_boxed_slice(),
                result,
            });

        result
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use string_interner::StringInterner;

    use super::*;
    use crate::files;

    #[test]
    fn build_simple_function() {
        let source = indoc! {"
            func test(): i32 {
                local mut i: i32 = 0;

                loop {
                    if i < 10 {
                        i = i + 1;
                    } else {
                        return i;
                    }
                }
            }
        "};
        let mut interner = StringInterner::new();
        let mut files = files::Files::new();
        let file_id = files.add("main".to_string(), source.to_string()).unwrap();

        let ast =
            ast::parser::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
        if ast.diagnostics.len() > 0 {
            println!("{:#?}", ast.diagnostics);
        }

        let hir = hir::Builder::build(&ast.ast, &mut interner);
        if hir.diagnostics.len() > 0 {
            panic!("{:#?}", hir.diagnostics);
        }

        let mir = Builder::build_function(&hir.hir, 0).unwrap();

        let json = serde_json::to_string_pretty(&mir.data_nodes).unwrap();
        println!("{}", json);

        let json = serde_json::to_string_pretty(&mir.blocks).unwrap();
        println!("{}", json);
    }
}
