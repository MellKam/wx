use std::collections::HashMap;

use crate::mir;
use string_interner::symbol::SymbolU32;

#[cfg(test)]
use serde::Serialize;

pub type DataNodeId = usize;

#[derive(Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Debug, Serialize))]
#[cfg_attr(test, serde(tag = "type"))]
pub enum DataNodeKind {
    ConstInt {
        value: i64,
    },
    Parameter {
        name: SymbolU32,
        index: usize,
    },
    Add {
        left: DataNodeId,
        right: DataNodeId,
    },
    Mul {
        left: DataNodeId,
        right: DataNodeId,
    },
    Sub {
        left: DataNodeId,
        right: DataNodeId,
    },
    Div {
        left: DataNodeId,
        right: DataNodeId,
    },
    Lt {
        left: DataNodeId,
        right: DataNodeId,
    },
    Gt {
        left: DataNodeId,
        right: DataNodeId,
    },
    Le {
        left: DataNodeId,
        right: DataNodeId,
    },
    Ge {
        left: DataNodeId,
        right: DataNodeId,
    },
    Eq {
        left: DataNodeId,
        right: DataNodeId,
    },
    Ne {
        left: DataNodeId,
        right: DataNodeId,
    },
    Either {
        left: DataNodeId,
        right: DataNodeId,
    },
    LoopParameter {
        before: DataNodeId,
        after: DataNodeId,
    },
}

#[cfg_attr(test, derive(Debug, Serialize))]
pub struct DataNode {
    pub kind: DataNodeKind,
    pub uses: Vec<DataNodeId>,
}

#[cfg_attr(test, derive(Debug, Serialize))]
#[cfg_attr(test, serde(tag = "type"))]
enum ControlNode {
    Return {
        value: Option<DataNodeId>,
    },
    Break {
        block: BlockId,
        value: Option<DataNodeId>,
    },
    Continue {
        block: BlockId,
    },
    IfElse {
        condition: DataNodeId,
        then_block: BlockId,
        else_block: Option<BlockId>,
        result: Option<DataNodeId>,
    },
    Loop {
        block: BlockId,
    },
}

#[cfg_attr(test, derive(Debug, Serialize))]
struct Block {
    nodes: Vec<ControlNode>,
    result: Option<DataNodeId>,
}
type BlockId = usize;

struct BlockContext {
    locals: HashMap<u32, DataNodeId>,
}

#[cfg_attr(test, derive(Debug))]
struct ControlFlowTree {
    data_nodes: Vec<DataNode>,
    data_lookup: HashMap<DataNodeKind, DataNodeId>,
    locals: HashMap<(u32, u32), DataNodeId>,
    blocks: Vec<Block>,
}

impl ControlFlowTree {
    fn ensure_data_node(&mut self, kind: DataNodeKind) -> DataNodeId {
        match self.data_lookup.get(&kind).copied() {
            Some(id) => return id,
            None => {}
        }

        let id = self.data_nodes.len();
        self.data_nodes.push(DataNode {
            kind: kind.clone(),
            uses: Vec::new(),
        });
        self.data_lookup.insert(kind, id);
        id
    }

    pub fn build(mir: &mir::MIR, func_index: mir::FunctionIndex) -> Self {
        let mut cft = ControlFlowTree {
            data_nodes: Vec::new(),
            data_lookup: HashMap::new(),
            locals: HashMap::new(),
            blocks: Vec::new(),
        };

        let func = &mir.functions[func_index as usize];
        match &func.block.kind {
            mir::ExprKind::Block { expressions, .. } => {
                let scope = func.frame.first().unwrap();
                for (local_index, local) in scope.locals[0..func.ty.param_count].iter().enumerate()
                {
                    let node_id = cft.ensure_data_node(DataNodeKind::Parameter {
                        name: local.name,
                        index: local_index,
                    });
                    cft.locals.insert((0, local_index as u32), node_id);
                }

                cft.build_block(func, expressions);
            }
            _ => unreachable!(),
        }

        cft
    }

    fn build_block(&mut self, func: &mir::Function, expressions: &[mir::Expression]) -> BlockId {
        let block_id = self.blocks.len();
        self.blocks.push(Block {
            nodes: Vec::new(),
            result: None,
        });

        for expr in expressions {
            match &expr.kind {
                mir::ExprKind::Return { value } => {
                    let data_node_id = match value {
                        Some(value) => self.build_expr(func, &value),
                        None => None,
                    };

                    match (self.blocks.first().unwrap().result, data_node_id) {
                        (Some(left), Some(right)) => {
                            let phi_id =
                                self.ensure_data_node(DataNodeKind::Either { left, right });
                            self.data_nodes[left].uses.push(phi_id);
                            self.data_nodes[right].uses.push(phi_id);
                            self.blocks[0].result = Some(phi_id);
                        }
                        (None, Some(result)) => {
                            self.blocks[0].result = Some(result);
                        }
                        _ => {}
                    }
                    self.blocks[block_id].nodes.push(ControlNode::Return {
                        value: data_node_id,
                    });

                    return block_id;
                }
                mir::ExprKind::LocalSet {
                    scope_index,
                    local_index,
                    value,
                } => match self.build_expr(func, value) {
                    Some(value_id) => {
                        self.locals.insert((scope_index.0, *local_index), value_id);
                    }
                    None => {}
                },
                mir::ExprKind::Break { scope_index, value } => {
                    let data_node_id = match value {
                        Some(value) => self.build_expr(func, &value),
                        None => None,
                    };
                    let block = &mut self.blocks[block_id];
                    block.nodes.push(ControlNode::Break {
                        block: scope_index.0 as BlockId,
                        value: data_node_id,
                    });
                    if scope_index.0 == block_id as u32 {
                        block.result = data_node_id;
                    }
                    return block_id;
                }
                mir::ExprKind::IfElse {
                    condition,
                    then_block,
                    else_block,
                } => {
                    let condition_id = self.build_expr(func, condition).unwrap();
                    let then_block = match &then_block.kind {
                        mir::ExprKind::Block { expressions, .. } => {
                            self.build_block(func, expressions)
                        }
                        _ => unreachable!(),
                    };
                    let else_block = match else_block {
                        Some(else_block) => match &else_block.kind {
                            mir::ExprKind::Block { expressions, .. } => {
                                Some(self.build_block(func, expressions))
                            }
                            _ => unreachable!(),
                        },
                        None => None,
                    };

                    let then_result = self.blocks[then_block].result;
                    let else_result = else_block.and_then(|eb| self.blocks[eb].result);

                    let result = match (then_result, else_result) {
                        (Some(then_id), Some(else_id)) => {
                            let phi_id = self.ensure_data_node(DataNodeKind::PhiMerge {
                                left: then_id,
                                right: else_id,
                            });
                            Some(phi_id)
                        }
                        (Some(then_id), None) => Some(then_id),
                        _ => None,
                    };

                    let node = ControlNode::IfElse {
                        condition: condition_id,
                        then_block,
                        else_block,
                        result,
                    };
                    self.blocks[block_id].nodes.push(node);
                }
                _ => todo!(),
            }
        }

        block_id
    }

    fn build_expr(&mut self, func: &mir::Function, expr: &mir::Expression) -> Option<DataNodeId> {
        match &expr.kind {
            mir::ExprKind::Int { value } => {
                Some(self.ensure_data_node(DataNodeKind::ConstInt { value: *value }))
            }
            mir::ExprKind::LocalSet {
                local_index,
                scope_index,
                value,
            } => match self.build_expr(func, value) {
                Some(value_id) => {
                    self.locals.insert((scope_index.0, *local_index), value_id);
                    Some(value_id)
                }
                None => None,
            },
            mir::ExprKind::LocalGet {
                local_index,
                scope_index,
            } => {
                let local_index = *local_index;
                if scope_index.0 == 0 && (local_index as usize) < func.ty.param_count {
                    return Some(local_index as usize);
                }

                match self.locals.get(&(scope_index.0, local_index)).copied() {
                    Some(node_id) => return Some(node_id),
                    None => panic!("local not found in DFG"),
                }
            }
            mir::ExprKind::Block { expressions, .. } => {
                let block = self.build_block(func, expressions);
                self.blocks[block].result
            }
            mir::ExprKind::Add { left, right }
            | mir::ExprKind::Mul { left, right }
            | mir::ExprKind::Sub { left, right }
            | mir::ExprKind::Div { left, right }
            | mir::ExprKind::Less { left, right }
            | mir::ExprKind::LessEq { left, right }
            | mir::ExprKind::Greater { left, right }
            | mir::ExprKind::GreaterEq { left, right } => {
                let left_id = self.build_expr(func, left).unwrap();
                let right_id = self.build_expr(func, right).unwrap();
                let result_id = self.ensure_data_node(match expr.kind {
                    mir::ExprKind::Add { .. } => DataNodeKind::Add {
                        left: left_id,
                        right: right_id,
                    },
                    mir::ExprKind::Mul { .. } => DataNodeKind::Mul {
                        left: left_id,
                        right: right_id,
                    },
                    mir::ExprKind::Sub { .. } => DataNodeKind::Sub {
                        left: left_id,
                        right: right_id,
                    },
                    mir::ExprKind::Div { .. } => DataNodeKind::Div {
                        left: left_id,
                        right: right_id,
                    },
                    mir::ExprKind::Less { .. } => DataNodeKind::Lt {
                        left: left_id,
                        right: right_id,
                    },
                    mir::ExprKind::LessEq { .. } => DataNodeKind::Le {
                        left: left_id,
                        right: right_id,
                    },
                    mir::ExprKind::Greater { .. } => DataNodeKind::Gt {
                        left: left_id,
                        right: right_id,
                    },
                    mir::ExprKind::GreaterEq { .. } => DataNodeKind::Ge {
                        left: left_id,
                        right: right_id,
                    },
                    _ => unreachable!(),
                });
                self.data_nodes[left_id].uses.push(result_id);
                self.data_nodes[right_id].uses.push(result_id);
                Some(result_id)
            }
            _ => todo!("{:?}", expr.kind),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast, files, hir};
    use indoc::indoc;
    use string_interner::StringInterner;

    #[test]
    fn test_data_flow_graph() {
        let source = indoc! {"
            export func fibonacci_iterative(mut n: i32): i32 {
                if n <= 1 { return n };

                local mut a: i32 = 0;
                local mut b: i32 = 1;

                loop {
                    if n == 0 { break a };
                    if n == 1 { break b };

                    local c = a + b;
                    a = b;
                    b = c;
                    n -= 1;
                }
            }
        "};
        let mut interner = StringInterner::new();
        let mut files = files::Files::new();
        let file_id = files.add("test".to_string(), source.to_string()).unwrap();

        let ast =
            ast::parser::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
        let hir = hir::Builder::build(&ast.ast, &mut interner);
        if hir.diagnostics.len() > 0 {
            panic!("{:#?}", hir.diagnostics);
        }

        let mir = mir::Builder::build(&hir.hir);

        let cft = ControlFlowTree::build(&mir, 0);

        println!("{:#?}", cft.blocks);
        let json = serde_json::to_string_pretty(&cft.data_nodes).unwrap();
        println!("{}", json);
    }
}
