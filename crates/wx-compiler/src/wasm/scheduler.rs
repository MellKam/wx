use std::collections::HashMap;

use crate::{mir, wasm};

pub struct Scheduler<'mir> {
    mir: &'mir mir::MIR,
    locals: Vec<wasm::Local>,
    node_to_local: HashMap<mir::DataNodeIndex, wasm::LocalIndex>,
    expressions: Vec<wasm::Expression>,
}

impl From<mir::ValueType> for wasm::ValueType {
    fn from(ty: mir::ValueType) -> Self {
        match ty {
            mir::ValueType::I32 => wasm::ValueType::I32,
            mir::ValueType::I64 => wasm::ValueType::I64,
            mir::ValueType::F32 => wasm::ValueType::F32,
            mir::ValueType::F64 => wasm::ValueType::F64,
        }
    }
}

fn should_spill_node(node: &mir::DataNode) -> bool {
    match node.kind {
        mir::DataNodeKind::Int { .. }
        | mir::DataNodeKind::Float { .. }
        | mir::DataNodeKind::Param { .. }
        | mir::DataNodeKind::LoopParam { .. } => false,
        mir::DataNodeKind::Add { .. }
        | mir::DataNodeKind::Sub { .. }
        | mir::DataNodeKind::Div { .. }
        | mir::DataNodeKind::Mul { .. }
        | mir::DataNodeKind::Eq { .. }
        | mir::DataNodeKind::NotEq { .. }
        | mir::DataNodeKind::Gt { .. }
        | mir::DataNodeKind::GtEq { .. }
        | mir::DataNodeKind::Lt { .. }
        | mir::DataNodeKind::LtEq { .. } => node.uses.len() > 1,
        mir::DataNodeKind::Phi { .. } | mir::DataNodeKind::CallResult { .. } => true,
    }
}

impl<'mir> Scheduler<'mir> {
    pub fn schedule_function(mir: &'mir mir::MIR) -> wasm::FunctionBody {
        let mut scheduler = Scheduler {
            locals: mir
                .params
                .iter()
                .map(|param| wasm::Local {
                    ty: wasm::ValueType::from(param.ty),
                })
                .collect(),
            expressions: Vec::new(),
            node_to_local: HashMap::new(),
            mir,
        };

        for stmt in mir.blocks[0].as_ref().unwrap().statements.iter() {
            let expr = scheduler.schedule_statement(stmt);
            println!("Scheduled expr: {:#?}", expr);
            scheduler.expressions.push(expr);
        }

        wasm::FunctionBody {
            name: mir.name,
            locals: scheduler.locals.into_boxed_slice(),
            expressions: scheduler.expressions.into_boxed_slice(),
        }
    }

    fn ensure_local(&mut self, node_index: mir::DataNodeIndex) -> wasm::Expression {
        if let Some(&local_index) = self.node_to_local.get(&node_index) {
            return wasm::Expression::LocalGet { local_index };
        }

        let expr = self.schedule_expression_direct(node_index);
        let node = &self.mir.data_nodes[node_index as usize];
        let local_index = wasm::LocalIndex(self.locals.len() as u32);
        self.locals.push(wasm::Local {
            ty: wasm::ValueType::from(node.kind.ty()),
        });
        self.expressions.push(wasm::Expression::LocalSet {
            local_index,
            value: Box::new(expr),
        });

        self.node_to_local.insert(node_index, local_index);
        wasm::Expression::LocalGet { local_index }
    }

    fn schedule_statement(&mut self, stmt: &mir::ControlNode) -> wasm::Expression {
        match stmt {
            mir::ControlNode::Return { value } => {
                let value = match value {
                    mir::ResultData::Value { node_index } => {
                        Some(Box::new(self.schedule_expression(*node_index)))
                    }
                    _ => None,
                };
                wasm::Expression::Return { value }
            }
            mir::ControlNode::IfElse {
                condition,
                then_block,
                else_block,
                outputs,
                result,
            } => {
                let condition_expr = self.schedule_expression(*condition);
                let output_locals: Vec<wasm::LocalIndex> = outputs
                    .iter()
                    .map(|output| match &self.mir.data_nodes[*output as usize].kind {
                        mir::DataNodeKind::Phi { left, right, ty } => self
                            .node_to_local
                            .get(left)
                            .copied()
                            .or(self.node_to_local.get(right).copied())
                            .unwrap_or_else(|| {
                                let local_index = wasm::LocalIndex(self.locals.len() as u32);
                                self.locals.push(wasm::Local {
                                    ty: wasm::ValueType::from(*ty),
                                });
                                local_index
                            }),
                        _ => unreachable!(),
                    })
                    .collect();

                let then_branch = 'then_block: {
                    let mut expressions = Vec::new();
                    for stmt in self.mir.blocks[*then_block as usize]
                        .as_ref()
                        .unwrap()
                        .statements
                        .iter()
                    {
                        expressions.push(self.schedule_statement(stmt));
                    }
                    for (index, output) in outputs.iter().enumerate() {
                        match self.mir.data_nodes[*output as usize].kind {
                            mir::DataNodeKind::Phi { left, .. } => {
                                let local_index = output_locals[index];
                                expressions.push(wasm::Expression::LocalSet {
                                    local_index,
                                    value: Box::new(self.schedule_expression(left)),
                                });
                            }
                            _ => unreachable!(),
                        }
                    }

                    match result {
                        mir::ResultData::Value { node_index } => {
                            let node_id = match self.mir.data_nodes[*node_index as usize].kind {
                                mir::DataNodeKind::Phi { left, .. } => left,
                                _ => *node_index,
                            };
                            if expressions.is_empty() {
                                break 'then_block self.schedule_expression(node_id);
                            }
                            expressions.push(self.schedule_expression(node_id));
                        }
                        _ => {}
                    };
                    wasm::Expression::Block {
                        expressions: expressions.into_boxed_slice(),
                        result: match result {
                            mir::ResultData::Value { node_index } => {
                                let ty = self.mir.data_nodes[*node_index as usize].kind.ty();
                                wasm::BlockResult::SingleValue(wasm::ValueType::from(ty))
                            }
                            _ => wasm::BlockResult::Empty,
                        },
                    }
                };
                let else_branch = if let Some(else_block_index) = else_block {
                    let mut expressions = Vec::new();
                    for stmt in self.mir.blocks[*else_block_index as usize]
                        .as_ref()
                        .unwrap()
                        .statements
                        .iter()
                    {
                        expressions.push(self.schedule_statement(stmt));
                    }
                    for (index, output) in outputs.iter().enumerate() {
                        match self.mir.data_nodes[*output as usize].kind {
                            mir::DataNodeKind::Phi { right, .. } => {
                                let local_index = output_locals[index];
                                expressions.push(wasm::Expression::LocalSet {
                                    local_index,
                                    value: Box::new(self.schedule_expression(right)),
                                });
                            }
                            _ => unreachable!(),
                        }
                    }

                    match result {
                        mir::ResultData::Value { node_index } if expressions.is_empty() => {
                            let node_id = match self.mir.data_nodes[*node_index as usize].kind {
                                mir::DataNodeKind::Phi { right, .. } => right,
                                _ => *node_index,
                            };
                            Some(Box::new(self.schedule_expression(node_id)))
                        }
                        _ => {
                            match result {
                                mir::ResultData::Value { node_index } => {
                                    let node_id =
                                        match self.mir.data_nodes[*node_index as usize].kind {
                                            mir::DataNodeKind::Phi { right, .. } => right,
                                            _ => *node_index,
                                        };

                                    expressions.push(self.schedule_expression(node_id));
                                }
                                _ => {}
                            };
                            Some(Box::new(wasm::Expression::Block {
                                expressions: expressions.into_boxed_slice(),
                                result: match result {
                                    mir::ResultData::Value { node_index } => {
                                        let ty =
                                            self.mir.data_nodes[*node_index as usize].kind.ty();
                                        wasm::BlockResult::SingleValue(wasm::ValueType::from(ty))
                                    }
                                    _ => wasm::BlockResult::Empty,
                                },
                            }))
                        }
                    }
                } else {
                    None
                };

                for (index, output) in outputs.iter().enumerate() {
                    match self.mir.data_nodes[*output as usize].kind {
                        mir::DataNodeKind::Phi { .. } => {
                            let local_index = output_locals[index];
                            self.node_to_local.insert(*output, local_index);
                        }
                        _ => unreachable!(),
                    }
                }

                let expr = wasm::Expression::IfElse {
                    condition: Box::new(condition_expr),
                    then_branch: Box::new(then_branch),
                    else_branch,
                    result: match result {
                        mir::ResultData::Value { node_index } => {
                            let ty = self.mir.data_nodes[*node_index as usize].kind.ty();
                            wasm::BlockResult::SingleValue(wasm::ValueType::from(ty))
                        }
                        _ => wasm::BlockResult::Empty,
                    },
                };

                match result {
                    mir::ResultData::Value { node_index } => {
                        let ty = self.mir.data_nodes[*node_index as usize].kind.ty();
                        let local_index = wasm::LocalIndex(self.locals.len() as u32);
                        self.locals.push(wasm::Local {
                            ty: wasm::ValueType::from(ty),
                        });
                        self.node_to_local.insert(*node_index, local_index);
                        wasm::Expression::LocalSet {
                            local_index,
                            value: Box::new(expr),
                        }
                    }
                    _ => expr,
                }
            }
            _ => unimplemented!(),
        }
    }

    fn schedule_expression(&mut self, node_id: mir::DataNodeIndex) -> wasm::Expression {
        if should_spill_node(&self.mir.data_nodes[node_id as usize]) {
            return self.ensure_local(node_id);
        }

        self.schedule_expression_direct(node_id)
    }

    fn schedule_expression_direct(&mut self, node_id: mir::DataNodeIndex) -> wasm::Expression {
        match self.mir.data_nodes[node_id as usize].kind.clone() {
            mir::DataNodeKind::Int { value, ty } => match ty {
                mir::ValueType::I32 => wasm::Expression::I32Const {
                    value: value as i32,
                },
                mir::ValueType::I64 => wasm::Expression::I64Const { value },
                _ => unreachable!(),
            },
            mir::DataNodeKind::Param { index, .. } => wasm::Expression::LocalGet {
                local_index: wasm::LocalIndex(index),
            },
            mir::DataNodeKind::Float { value, ty } => match ty {
                mir::ValueType::F32 => wasm::Expression::F32Const {
                    value: f32::from_bits(value as u32),
                },
                mir::ValueType::F64 => wasm::Expression::F64Const {
                    value: f64::from_bits(value),
                },
                _ => unreachable!(),
            },
            mir::DataNodeKind::Add { left, right, ty } => {
                let left_expr = self.schedule_expression(left);
                let right_expr = self.schedule_expression(right);

                match ty {
                    mir::ValueType::I32 => wasm::Expression::I32Add {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    mir::ValueType::I64 => wasm::Expression::I64Add {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    _ => unreachable!(),
                }
            }
            mir::DataNodeKind::Sub { left, right, ty } => {
                let left_expr = self.schedule_expression(left);
                let right_expr = self.schedule_expression(right);

                match ty {
                    mir::ValueType::I32 => wasm::Expression::I32Sub {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    mir::ValueType::I64 => wasm::Expression::I64Sub {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    _ => unreachable!(),
                }
            }
            mir::DataNodeKind::Mul { left, right, ty } => {
                let left_expr = self.schedule_expression(left);
                let right_expr = self.schedule_expression(right);

                match ty {
                    mir::ValueType::I32 => wasm::Expression::I32Mul {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    mir::ValueType::I64 => wasm::Expression::I64Mul {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    _ => unreachable!(),
                }
            }
            mir::DataNodeKind::Div { left, right, ty } => {
                let left_expr = self.schedule_expression(left);
                let right_expr = self.schedule_expression(right);

                match ty {
                    mir::ValueType::I32 => wasm::Expression::I32DivS {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    mir::ValueType::I64 => wasm::Expression::I64DivS {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    _ => unreachable!(),
                }
            }
            mir::DataNodeKind::Eq { left, right, ty } => {
                let left_expr = self.schedule_expression(left);
                let right_expr = self.schedule_expression(right);

                match ty {
                    mir::ValueType::I32 => wasm::Expression::I32Eq {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    mir::ValueType::I64 => wasm::Expression::I64Eq {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    _ => unreachable!(),
                }
            }
            mir::DataNodeKind::Lt { left, right, ty } => {
                let left_expr = self.schedule_expression(left);
                let right_expr = self.schedule_expression(right);

                match ty {
                    mir::ValueType::I32 => wasm::Expression::I32LtS {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    mir::ValueType::I64 => wasm::Expression::I64LtS {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    _ => unreachable!(),
                }
            }
            mir::DataNodeKind::Gt { left, right, ty } => {
                let left_expr = self.schedule_expression(left);
                let right_expr = self.schedule_expression(right);

                match ty {
                    mir::ValueType::I32 => wasm::Expression::I32GtS {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    mir::ValueType::I64 => wasm::Expression::I64GtS {
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                    },
                    _ => unreachable!(),
                }
            }
            kind => unimplemented!("{:#?}", kind),
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use string_interner::StringInterner;

    use super::*;
    use crate::{ast, files, hir, mir};

    #[test]
    fn test_schedule_function_add() {
        let source = indoc! {"
           export func test(a: i32, b: i32): i32 {
                if a > b {
                    if a > 100 {
                        return 100;
                    } else {
                        return a;
                    }
                } else {
                    if b > 100 {
                        return 100;
                    } else {
                        return b; 
                    }
                }
            }
        "};
        let mut interner = StringInterner::new();
        let mut files = files::Files::new();
        let file_id = files.add("main".to_string(), source.to_string()).unwrap();

        let ast =
            ast::parser::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
        let hir = hir::Builder::build(&ast.ast, &mut interner);
        if hir.diagnostics.len() > 0 {
            panic!("{:#?}", hir.diagnostics);
        }

        let mir = mir::Builder::build_function(&hir.hir, 0).unwrap();
        println!("{:#?}", mir);
        let wasm_func = Scheduler::schedule_function(&mir);
        println!("{:#?}", wasm_func);
    }
}
