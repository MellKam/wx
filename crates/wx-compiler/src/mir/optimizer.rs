use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;
use std::vec;

use crate::mir;

struct IntType {
    min: i64,
    max: i64,
}

impl IntType {
    fn new_i64() -> Self {
        IntType {
            min: i64::MIN,
            max: i64::MAX,
        }
    }

    fn new_u64() -> Self {
        IntType {
            min: 0,
            max: u64::MAX as i64,
        }
    }

    fn new_i32() -> Self {
        IntType {
            min: i32::MIN as i64,
            max: i32::MAX as i64,
        }
    }

    fn new_u32() -> Self {
        IntType {
            min: 0,
            max: u32::MAX as i64,
        }
    }

    fn is_const(&self) -> bool {
        self.min == self.max
    }
}

pub type NodeId = u32;

pub struct Graph {
    pub nodes: HashSet<DataNode>,
}

#[derive(Hash)]
enum DataOp {
    Const { value: i64, ty: mir::Type },
    Add { left: RcDataNode, right: RcDataNode },
    Sub { left: RcDataNode, right: RcDataNode },
    Mul { left: RcDataNode, right: RcDataNode },
    Div { left: RcDataNode, right: RcDataNode },
}

pub struct DataNode {
    pub op: DataOp,
    pub ty: mir::Type,
}

pub struct RcDataNode(pub Rc<RefCell<DataNode>>);

impl Hash for RcDataNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.borrow().op.hash(state);
        self.0.borrow().ty.hash(state);
    }
}

enum ControlOp {
    Start,
    Return,
    Phi,
    Region,
    Projection { index: u32 },
}

struct GraphBuilder {
    graph: Graph,
    current_control: NodeId,
}

// impl GraphBuilder {
//     pub fn build(function: &mir::Function) -> Graph {
//         let mut builder = Self {
//             current_control: 0,
//             graph: Graph { nodes: Vec::new() },
//         };

//         let initial_control = builder.add_node(Op::Region, vec![],
// mir::Type::Unit);         let start_node = builder.add_node(Op::Start,
// vec![], mir::Type::Unit);     }

//     fn add_node(&mut self, op: Op, inputs: Vec<NodeId>, ty: mir::Type) ->
// NodeId {         let node_id = self.graph.nodes.len() as NodeId;
//         self.graph.nodes.push(Node {
//             id: node_id,
//             op,
//             deps: inputs,
//             ty,
//         });
//         node_id
//     }

//     fn visit_expr(&mut self, expr: &mir::Expression) -> NodeId {
//         match &expr.kind {
//             mir::ExprKind::Int { value } => {
//                 self.add_node(Op::ConstInt { value: *value }, vec![],
// expr.ty)             }
//             mir::ExprKind::Add { left, right } => {
//                 let left_node = self.visit_expr(left);
//                 let right_node = self.visit_expr(right);
//                 self.add_node(
//                     Op::Add,
//                     vec![left_node, right_node, self.current_control],
//                     expr.ty,
//                 )
//             }
//         }
//     }
// }
