//! Data liveness analysis for scheduler dead-pointer-load elimination.
//!
//! Computes the set of data nodes reachable from control-node sinks.

use crate::opt::{BlockIndex, ControlNode, DataNodeIndex, DataNodeKind, Function, StackResult};

pub struct DataLiveness {
    pub live: Vec<bool>,
}

impl DataLiveness {
    pub fn compute(func: &Function) -> Self {
        let mut live = vec![false; func.data_nodes.len()];
        let mut worklist = Vec::new();
        mark_block_roots(func, 0, &mut live, &mut worklist);
        while let Some(node) = worklist.pop() {
            mark_node_inputs_live(func, node, &mut live, &mut worklist);
        }
        DataLiveness { live }
    }

    pub fn is_live(&self, node: DataNodeIndex) -> bool {
        self.live[node as usize]
    }
}

fn mark_block_roots(
    func: &Function,
    block_idx: BlockIndex,
    live: &mut [bool],
    worklist: &mut Vec<DataNodeIndex>,
) {
    let block = func.blocks[block_idx as usize]
        .as_ref()
        .expect("block must exist");
    for stmt in &block.statements {
        match stmt {
            ControlNode::Return { value } => {
                mark_stack_result_live(*value, live, worklist);
            }
            ControlNode::GlobalSet { value, .. } => {
                mark_node_live(*value, live, worklist);
            }
            ControlNode::Call { callee, args, .. } => {
                mark_node_live(*callee, live, worklist);
                for &arg in args.iter() {
                    mark_node_live(arg, live, worklist);
                }
            }
            ControlNode::IfElse {
                condition,
                then_block,
                else_block,
                outputs,
                result,
            } => {
                mark_node_live(*condition, live, worklist);
                for &output in outputs.iter() {
                    mark_node_live(output, live, worklist);
                }
                if outputs.is_empty() {
                    mark_stack_result_live(*result, live, worklist);
                }
                mark_block_roots(func, *then_block, live, worklist);
                if let Some(else_block) = else_block {
                    mark_block_roots(func, *else_block, live, worklist);
                }
            }
            ControlNode::Loop { body, outputs, .. } => {
                for &output in outputs.iter() {
                    mark_node_live(output, live, worklist);
                }
                mark_block_roots(func, *body, live, worklist);
            }
            ControlNode::Break { .. } | ControlNode::Continue { .. } | ControlNode::Unreachable => {
            }
            ControlNode::MemoryGrow { delta, .. } => {
                mark_node_live(*delta, live, worklist);
            }
            ControlNode::MemorySize { .. } => {}
            ControlNode::PointerLoad { .. } => {}
            ControlNode::PointerStore { address, value, .. } => {
                mark_node_live(*address, live, worklist);
                mark_node_live(*value, live, worklist);
            }
        }
    }
}

fn mark_stack_result_live(
    result: StackResult,
    live: &mut [bool],
    worklist: &mut Vec<DataNodeIndex>,
) {
    if let StackResult::Value(node) = result {
        mark_node_live(node, live, worklist);
    }
}

fn mark_node_live(node: DataNodeIndex, live: &mut [bool], worklist: &mut Vec<DataNodeIndex>) {
    let idx = node as usize;
    if live[idx] {
        return;
    }
    live[idx] = true;
    worklist.push(node);
}

fn mark_node_inputs_live(
    func: &Function,
    node: DataNodeIndex,
    live: &mut [bool],
    worklist: &mut Vec<DataNodeIndex>,
) {
    match &func.data_nodes[node as usize].kind {
        DataNodeKind::Add { left, right, .. }
        | DataNodeKind::Sub { left, right, .. }
        | DataNodeKind::Mul { left, right, .. }
        | DataNodeKind::Div { left, right, .. }
        | DataNodeKind::Rem { left, right, .. }
        | DataNodeKind::BitAnd { left, right, .. }
        | DataNodeKind::BitOr { left, right, .. }
        | DataNodeKind::BitXor { left, right, .. }
        | DataNodeKind::Shl { left, right, .. }
        | DataNodeKind::ShrS { left, right, .. }
        | DataNodeKind::ShrU { left, right, .. }
        | DataNodeKind::Eq { left, right, .. }
        | DataNodeKind::NotEq { left, right, .. }
        | DataNodeKind::LtS { left, right, .. }
        | DataNodeKind::LtU { left, right, .. }
        | DataNodeKind::LtEqS { left, right, .. }
        | DataNodeKind::LtEqU { left, right, .. }
        | DataNodeKind::GtS { left, right, .. }
        | DataNodeKind::GtU { left, right, .. }
        | DataNodeKind::GtEqS { left, right, .. }
        | DataNodeKind::GtEqU { left, right, .. }
        | DataNodeKind::Phi { left, right, .. } => {
            mark_node_live(*left, live, worklist);
            mark_node_live(*right, live, worklist);
        }
        DataNodeKind::Neg { operand, .. }
        | DataNodeKind::BitNot { operand, .. }
        | DataNodeKind::Eqz { operand }
        | DataNodeKind::I64ExtendI32S { operand }
        | DataNodeKind::I64ExtendI32U { operand }
        | DataNodeKind::I32WrapI64 { operand }
        | DataNodeKind::AggregateGet {
            aggregate: operand, ..
        } => {
            mark_node_live(*operand, live, worklist);
        }
        DataNodeKind::Aggregate { fields, .. } => {
            for &field in fields.iter() {
                mark_node_live(field, live, worklist);
            }
        }
        DataNodeKind::LoopParam { before, after, .. } => {
            mark_node_live(*before, live, worklist);
            mark_node_live(*after, live, worklist);
        }
        DataNodeKind::CallResult { callee, args, .. } => {
            mark_node_live(*callee, live, worklist);
            for &arg in args.iter() {
                mark_node_live(arg, live, worklist);
            }
        }
        DataNodeKind::MemoryGrowResult { delta, .. } => {
            mark_node_live(*delta, live, worklist);
        }
        DataNodeKind::PointerLoadResult { address, .. } => {
            mark_node_live(*address, live, worklist);
        }
        DataNodeKind::Int { .. }
        | DataNodeKind::Float { .. }
        | DataNodeKind::Param { .. }
        | DataNodeKind::GlobalGet { .. }
        | DataNodeKind::FunctionRef { .. }
        | DataNodeKind::StaticDataRef { .. }
        | DataNodeKind::MemoryOffset { .. }
        | DataNodeKind::MemoryIndex { .. }
        | DataNodeKind::MemorySizeResult { .. }
        | DataNodeKind::AggregateCallResult { .. } => {}
    }
}
