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
                let mut data_bindings: Vec<ResultData> =
                    Vec::with_capacity(root_scope.locals.len());
                for (local_index, local) in
                    root_scope.locals[0..func.params.len()].iter().enumerate()
                {
                    let node_id = builder.mir.ensure_data_node(DataNodeKind::Parameter {
                        index: local_index as u32,
                        symbol: local.name.symbol,
                        ty: ResultType::from_hir(hir, local.ty).unwrap_value(),
                    });
                    data_bindings.push(ResultData::Value(node_id));
                }
                for local in root_scope.locals[func.params.len()..].iter() {
                    data_bindings.push(builder.create_default_local_value(local.ty));
                }
                builder.mir.blocks[0] = Some(Block {
                    parent_index: None,
                    statements: Vec::new(),
                    result: ResultData::Never,
                });
                let mut data_bindings = data_bindings.into_boxed_slice();

                for expr in expressions.iter() {
                    match builder.build_expr(0, &mut data_bindings, expr) {
                        ResultData::Unit => {}
                        ResultData::Never => break,
                        ResultData::Value(node_id) => {
                            panic!(
                                "unused value in statement: {:#?}",
                                builder.mir.data_nodes[node_id as usize].kind
                            );
                        }
                    };
                }

                match result {
                    Some(result) => {
                        let result = builder.build_expr(0, &mut data_bindings, result);
                        match result {
                            ResultData::Value(node_id) => {
                                builder.mir.blocks[0].as_mut().unwrap().statements.push(
                                    ControlNode::Return {
                                        value: ResultData::Value(node_id),
                                    },
                                );
                            }
                            _ => {}
                        }
                        let block_result = builder.mir.blocks[0].as_ref().unwrap().result;
                        builder.mir.blocks[0].as_mut().unwrap().result =
                            builder.mir.merge_data(block_result, result);
                    }
                    None => {}
                };
            }
            _ => unreachable!(),
        }

        Ok(builder.mir)
    }

    fn create_default_local_value(&mut self, ty: hir::Type) -> ResultData {
        match ty {
            hir::Type::I32 | hir::Type::Bool | hir::Type::U32 | hir::Type::Function(_) => {
                ResultData::Value(self.mir.ensure_data_node(DataNodeKind::Int {
                    value: 0,
                    ty: ValueType::I32,
                }))
            }
            hir::Type::I64 | hir::Type::U64 => {
                ResultData::Value(self.mir.ensure_data_node(DataNodeKind::Int {
                    value: 0,
                    ty: ValueType::I64,
                }))
            }
            hir::Type::F32 => ResultData::Value(self.mir.ensure_data_node(DataNodeKind::Float {
                value: 0,
                ty: ValueType::F32,
            })),
            hir::Type::F64 => ResultData::Value(self.mir.ensure_data_node(DataNodeKind::Float {
                value: 0,
                ty: ValueType::F64,
            })),
            hir::Type::Enum(enum_index) => {
                let enum_def = &self.hir.enums[enum_index.0 as usize];
                self.create_default_local_value(enum_def.ty)
            }
            hir::Type::Unit => ResultData::Unit,
            hir::Type::Never => ResultData::Never,
            hir::Type::Unknown => panic!("unknown type"),
        }
    }

    fn create_data_bindings(
        &mut self,
        parent_data_bindings: &Box<[ResultData]>,
        block_index: BlockIndex,
    ) -> Box<[ResultData]> {
        let scope = &self.hir.functions[self.func_index].stack.scopes[block_index as usize];
        let scope_offset = self.locals_offsets[block_index as usize] as usize;
        let mut data_bindings = Vec::with_capacity(scope_offset + scope.locals.len());
        unsafe {
            data_bindings.set_len(scope_offset + scope.locals.len());
        }
        data_bindings[..parent_data_bindings.len()].copy_from_slice(&parent_data_bindings[..]);
        for (local_index, local) in scope.locals.iter().enumerate() {
            data_bindings[scope_offset + local_index] = self.create_default_local_value(local.ty);
        }

        data_bindings.into_boxed_slice()
    }

    fn build_block(
        &mut self,
        block_index: BlockIndex,
        data_bindings: &mut Box<[ResultData]>,
        expressions: &Box<[hir::Expression]>,
        result: &Option<Box<hir::Expression>>,
    ) -> ResultData {
        for expr in expressions.iter() {
            match self.build_expr(block_index, data_bindings, expr) {
                ResultData::Unit => {}
                ResultData::Never => {
                    return self.mir.blocks[block_index as usize]
                        .as_ref()
                        .unwrap()
                        .result;
                }
                ResultData::Value(node_id) => {
                    panic!(
                        "unused value in statement: {:#?}",
                        self.mir.data_nodes[node_id as usize].kind
                    );
                }
            };
        }

        match result {
            Some(result) => self.build_expr(block_index, data_bindings, result),
            None => ResultData::Unit,
        }
    }

    fn build_expr(
        &mut self,
        block_index: BlockIndex,
        data_bindings: &mut Box<[ResultData]>,
        expr: &hir::Expression,
    ) -> ResultData {
        match &expr.kind {
            hir::ExprKind::Int(value) => {
                let result = self.mir.ensure_data_node(DataNodeKind::Int {
                    value: *value,
                    ty: ResultType::from_hir(self.hir, expr.ty.unwrap()).unwrap_value(),
                });
                ResultData::Value(result)
            }
            hir::ExprKind::Bool(value) => {
                let node_id = self.mir.ensure_data_node(DataNodeKind::Int {
                    value: if *value { 1 } else { 0 },
                    ty: ValueType::I32,
                });
                ResultData::Value(node_id)
            }
            hir::ExprKind::Float(value) => {
                let node_id = self.mir.ensure_data_node(DataNodeKind::Float {
                    value: value.to_bits(),
                    ty: ResultType::from_hir(self.hir, expr.ty.unwrap()).unwrap_value(),
                });
                ResultData::Value(node_id)
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
                ResultData::Unit
            }
            hir::ExprKind::Return { value } => {
                let result = match value {
                    Some(expr) => self.build_expr(block_index, data_bindings, expr),
                    None => ResultData::Unit,
                };
                self.mir.blocks[block_index as usize]
                    .as_mut()
                    .unwrap()
                    .statements
                    .push(ControlNode::Return { value: result });
                let block_result = self.mir.blocks[0].as_ref().unwrap().result;
                self.mir.blocks[0].as_mut().unwrap().result =
                    self.mir.merge_data(block_result, result);

                ResultData::Never
            }
            hir::ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => {
                let mut new_data_bindings =
                    self.create_data_bindings(data_bindings, scope_index.0 as BlockIndex);
                let result =
                    self.build_block(block_index, &mut new_data_bindings, expressions, result);
                data_bindings.copy_from_slice(&new_data_bindings[..data_bindings.len()]);

                result
            }
            hir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
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
                            self.create_data_bindings(&data_bindings, scope_index.0 as BlockIndex);

                        self.mir.blocks[scope_index.0 as usize] = Some(Block {
                            parent_index: Some(block_index),
                            statements: Vec::new(),
                            result: ResultData::Never,
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
                                .create_data_bindings(&data_bindings, scope_index.0 as BlockIndex);

                            self.mir.blocks[scope_index.0 as usize] = Some(Block {
                                parent_index: Some(block_index),
                                statements: Vec::new(),
                                result: ResultData::Never,
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
                                Some(else_result),
                                else_bindings,
                                Some(scope_index.0 as BlockIndex),
                            )
                        }
                        _ => unreachable!(),
                    },
                    None => (None, data_bindings.clone(), None),
                };

                let mut outputs = Vec::new();
                for i in 0..data_bindings.len() {
                    let then_value = then_bindings[i];
                    let else_value = else_bindings[i];
                    data_bindings[i] = if then_value == else_value {
                        then_value
                    } else {
                        let ty = self.mir.data_nodes[then_value.unwrap_value() as usize]
                            .kind
                            .ty();

                        let output = self.mir.ensure_data_node(DataNodeKind::Phi {
                            left: then_value.unwrap_value(),
                            right: else_value.unwrap_value(),
                            ty,
                        });
                        outputs.push(output);
                        ResultData::Value(output)
                    };
                }

                let result = if else_result.is_none() {
                    ResultData::Unit
                } else {
                    match (then_result, else_result.unwrap()) {
                        (ResultData::Value(then_id), ResultData::Value(else_id)) => {
                            let ty = self.mir.data_nodes[then_id as usize].kind.ty();
                            let result_id = self.mir.ensure_data_node(DataNodeKind::Phi {
                                left: then_id,
                                right: else_id,
                                ty,
                            });
                            ResultData::Value(result_id)
                        }
                        (ResultData::Unit, ResultData::Unit) => ResultData::Unit,
                        (ResultData::Never, result) | (result, ResultData::Never) => result,
                        _ => unimplemented!(),
                    }
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
            hir::ExprKind::Binary {
                left,
                operator,
                right,
            } => {
                let left_result = self.build_expr(block_index, data_bindings, &left);
                let right_result = self.build_expr(block_index, data_bindings, &right);
                if operator.kind == ast::BinOpKind::Assign {
                    match &left.kind {
                        hir::ExprKind::Local {
                            scope_index,
                            local_index,
                        } => {
                            let binding_index =
                                self.locals_offsets[scope_index.0 as usize] + local_index.0;
                            data_bindings[binding_index as usize] = right_result;
                            return ResultData::Unit;
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
                let result_id = self.mir.ensure_data_node(node_kind);
                self.mir.data_nodes[left as usize].uses.push(result_id);
                self.mir.data_nodes[right as usize].uses.push(result_id);
                ResultData::Value(result_id)
            }
            expr => todo!("build_expr: {:?}", expr),
        }
    }
}

// test code
#[cfg(test)]
mod tests {
    use indoc::indoc;
    use string_interner::StringInterner;

    use super::*;
    use crate::files;

    #[test]
    fn build_simple_function() {
        let source = indoc! {"
            export func test(n: i32): i32 {
                if n == 0 {
                    return 1;
                };
                local mut d = n * 10;
                local m: i32 = 10;
                if d < 100 {
                    d = d + 1;
                } else {
                    d = d - 1;
                };
                return { m / 2 * d };
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
        println!("{:#?}", mir.blocks);

        let json = serde_json::to_string_pretty(&mir.data_nodes).unwrap();
        println!("{}", json);
    }
}
