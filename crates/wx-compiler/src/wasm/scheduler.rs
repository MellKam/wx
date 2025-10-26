use std::collections::HashMap;

use crate::{mir, wasm};

pub struct Scheduler<'mir> {
    mir: &'mir mir::MIR,
    locals: Vec<wasm::Local>,
    node_to_local: HashMap<mir::DataNodeId, wasm::LocalIndex>,
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
        | mir::DataNodeKind::Parameter { .. } => false,
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
        mir::DataNodeKind::Phi { .. } => unimplemented!(),
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

        let result = match mir.block.result {
            mir::StackResult::Value(node_id) => scheduler.schedule_expression(node_id),
            _ => todo!("empty result"),
        };
        scheduler.expressions.push(result);

        wasm::FunctionBody {
            name: mir.symbol,
            locals: scheduler.locals.into_boxed_slice(),
            expressions: scheduler.expressions.into_boxed_slice(),
        }
    }

    fn ensure_local(&mut self, node_id: mir::DataNodeId) -> wasm::Expression {
        if let Some(&local_index) = self.node_to_local.get(&node_id) {
            return wasm::Expression::LocalGet { local_index };
        }

        let expr = self.schedule_expression_direct(node_id);
        let node = &self.mir.data_nodes[node_id as usize];
        let local_index = wasm::LocalIndex(self.locals.len() as u32);
        self.locals.push(wasm::Local {
            ty: wasm::ValueType::from(node.kind.ty()),
        });
        self.expressions.push(wasm::Expression::LocalSet {
            local_index,
            value: Box::new(expr),
        });

        self.node_to_local.insert(node_id, local_index);
        wasm::Expression::LocalGet { local_index }
    }

    fn schedule_expression(&mut self, node_id: mir::DataNodeId) -> wasm::Expression {
        if should_spill_node(&self.mir.data_nodes[node_id as usize]) {
            return self.ensure_local(node_id);
        }

        self.schedule_expression_direct(node_id)
    }

    fn schedule_expression_direct(&mut self, node_id: mir::DataNodeId) -> wasm::Expression {
        let node = &self.mir.data_nodes[node_id as usize];
        match node.kind {
            mir::DataNodeKind::Int { value, ty } => match ty {
                mir::ValueType::I32 => wasm::Expression::I32Const {
                    value: value as i32,
                },
                mir::ValueType::I64 => wasm::Expression::I64Const { value },
                _ => unreachable!(),
            },
            mir::DataNodeKind::Parameter { index, .. } => wasm::Expression::LocalGet {
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
            _ => unimplemented!(),
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
            export func test(x: i32): i32 {
                local base = x * x;
                local left = base + 10;   
                local right = base - 5;   
                return { left * right };
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
        let wasm_func = Scheduler::schedule_function(&mir);
        println!("{:#?}", wasm_func);
    }
}
