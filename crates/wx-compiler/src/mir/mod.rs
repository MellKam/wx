use std::collections::HashMap;

#[cfg(test)]
use serde::Serialize;
use string_interner::symbol::SymbolU32;

pub mod builder;

pub use builder::*;

use crate::hir;

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug)]
pub struct FunctionParam {
    pub name: SymbolU32,
    pub ty: ValueType,
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug)]
pub struct MIR {
    pub name: SymbolU32,
    pub params: Box<[FunctionParam]>,
    pub data_nodes: Vec<DataNode>,
    pub data_lookup: HashMap<DataNodeKind, DataNodeIndex>,
    pub blocks: Box<[Option<Block>]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

#[cfg(test)]
impl serde::Serialize for ValueType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(match self {
            ValueType::I32 => "i32",
            ValueType::I64 => "i64",
            ValueType::F32 => "f32",
            ValueType::F64 => "f64",
        })
    }
}

impl ResultType {
    fn from_hir(hir: &hir::HIR, ty: hir::Type) -> ResultType {
        match ty {
            hir::Type::I32 => ResultType::Value(ValueType::I32),
            hir::Type::I64 => ResultType::Value(ValueType::I64),
            hir::Type::F32 => ResultType::Value(ValueType::F32),
            hir::Type::F64 => ResultType::Value(ValueType::F64),
            hir::Type::U32 => ResultType::Value(ValueType::I32),
            hir::Type::U64 => ResultType::Value(ValueType::I64),
            hir::Type::Bool => ResultType::Value(ValueType::I32),
            hir::Type::Unit => ResultType::Unit,
            hir::Type::Never => ResultType::Never,
            hir::Type::Function(_) => ResultType::Value(ValueType::I32),
            hir::Type::Enum(enum_index) => {
                let enum_def = &hir.enums[enum_index.0 as usize];
                ResultType::from_hir(hir, enum_def.ty)
            }
            hir::Type::Unknown => panic!("unknown type"),
        }
    }

    fn unwrap_value(self) -> ValueType {
        match self {
            ResultType::Value(ty) => ty,
            _ => panic!("expected value type"),
        }
    }
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResultType {
    Unit,
    Never,
    Value(ValueType),
}

#[cfg_attr(test, derive(Serialize))]
#[cfg_attr(test, serde(tag = "kind"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResultData {
    Unit,
    Never,
    Value { node_index: DataNodeIndex },
}

impl ResultData {
    pub fn unwrap_value(self) -> DataNodeIndex {
        match self {
            ResultData::Value { node_index } => node_index,
            _ => panic!("expected value result"),
        }
    }
}

impl MIR {
    fn ensure_data_node(&mut self, kind: DataNodeKind) -> DataNodeIndex {
        match self.data_lookup.get(&kind).copied() {
            Some(id) => return id,
            None => {}
        }

        let id = self.data_nodes.len() as u32;
        self.data_nodes.push(DataNode {
            kind: kind.clone(),
            uses: Vec::new(),
        });
        match kind {
            DataNodeKind::Add { left, right, .. }
            | DataNodeKind::Mul { left, right, .. }
            | DataNodeKind::Sub { left, right, .. }
            | DataNodeKind::Div { left, right, .. }
            | DataNodeKind::Lt { left, right, .. }
            | DataNodeKind::LtEq { left, right, .. }
            | DataNodeKind::Gt { left, right, .. }
            | DataNodeKind::GtEq { left, right, .. }
            | DataNodeKind::Eq { left, right, .. }
            | DataNodeKind::NotEq { left, right, .. }
            | DataNodeKind::Phi { left, right, .. }
            | DataNodeKind::LoopParam {
                before: left,
                after: right,
                ..
            } => {
                self.data_nodes[left as usize].uses.push(id);
                self.data_nodes[right as usize].uses.push(id);
            }
            DataNodeKind::CallResult { ref args, .. } => {
                for &arg in args.iter() {
                    self.data_nodes[arg as usize].uses.push(id);
                }
            }
            DataNodeKind::Int { .. } | DataNodeKind::Float { .. } | DataNodeKind::Param { .. } => {}
        }
        self.data_lookup.insert(kind, id);
        id
    }

    fn merge_data(&mut self, left: ResultData, right: ResultData) -> ResultData {
        match (left, right) {
            (ResultData::Never, result) | (result, ResultData::Never) => result,
            (ResultData::Unit, ResultData::Unit) => ResultData::Unit,
            (ResultData::Value { node_index: left }, ResultData::Value { node_index: right }) => {
                if right == left {
                    return ResultData::Value { node_index: left };
                }

                match (
                    &self.data_nodes[left as usize].kind,
                    &self.data_nodes[right as usize].kind,
                ) {
                    (
                        DataNodeKind::Phi {
                            left: phi_left,
                            right: phi_right,
                            ..
                        },
                        _,
                    ) if *phi_left == right || *phi_right == right => {
                        return ResultData::Value { node_index: right };
                    }
                    (
                        _,
                        DataNodeKind::Phi {
                            left: phi_left,
                            right: phi_right,
                            ..
                        },
                    ) if *phi_left == left || *phi_right == left => {
                        return ResultData::Value { node_index: left };
                    }
                    _ => {}
                };

                let ty = self.data_nodes[left as usize].kind.ty();
                ResultData::Value {
                    node_index: self.ensure_data_node(DataNodeKind::Phi { left, right, ty }),
                }
            }
            _ => panic!("cannot merge {:?} and {:?}", left, right),
        }
    }
}

pub type DataNodeIndex = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
#[cfg_attr(test, serde(tag = "kind"))]
pub enum DataNodeKind {
    Int {
        value: i64,
        ty: ValueType,
    },
    Float {
        value: u64,
        ty: ValueType,
    },
    Param {
        index: u32,
        symbol: SymbolU32,
        ty: ValueType,
    },
    Add {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    Mul {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    Sub {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    Div {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    Lt {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    LtEq {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    Gt {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    GtEq {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    Eq {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    NotEq {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    Phi {
        left: DataNodeIndex,
        right: DataNodeIndex,
        ty: ValueType,
    },
    CallResult {
        func_index: u32,
        args: Box<[DataNodeIndex]>,
        ty: ValueType,
    },
    LoopParam {
        block_index: BlockIndex,
        before: DataNodeIndex,
        after: DataNodeIndex,
        ty: ValueType,
    },
}

impl DataNodeKind {
    pub fn ty(&self) -> ValueType {
        match self {
            DataNodeKind::Int { ty, .. } => *ty,
            DataNodeKind::Float { ty, .. } => *ty,
            DataNodeKind::Param { ty, .. } => *ty,
            DataNodeKind::Add { ty, .. } => *ty,
            DataNodeKind::Mul { ty, .. } => *ty,
            DataNodeKind::Sub { ty, .. } => *ty,
            DataNodeKind::Div { ty, .. } => *ty,
            DataNodeKind::Lt { ty, .. } => *ty,
            DataNodeKind::LtEq { ty, .. } => *ty,
            DataNodeKind::Gt { ty, .. } => *ty,
            DataNodeKind::GtEq { ty, .. } => *ty,
            DataNodeKind::Eq { ty, .. } => *ty,
            DataNodeKind::NotEq { ty, .. } => *ty,
            DataNodeKind::Phi { ty, .. } => *ty,
            DataNodeKind::CallResult { ty, .. } => *ty,
            DataNodeKind::LoopParam { ty, .. } => *ty,
        }
    }
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone)]
pub struct DataNode {
    pub kind: DataNodeKind,
    pub uses: Vec<DataNodeIndex>,
}

#[cfg_attr(test, derive(Serialize))]
#[cfg_attr(test, serde(tag = "kind"))]
#[derive(Debug, Clone)]
pub enum ControlNode {
    Return {
        value: ResultData,
    },
    IfElse {
        condition: DataNodeIndex,
        then_block: BlockIndex,
        else_block: Option<BlockIndex>,
        outputs: Box<[DataNodeIndex]>,
        result: ResultData,
    },
    Call {
        func_index: u32,
        args: Box<[DataNodeIndex]>,
        result: ResultData,
    },
    Break {
        target_block: BlockIndex,
        value: ResultData,
    },
    Loop {
        body: BlockIndex,
        outputs: Box<[DataNodeIndex]>,
        result: ResultData,
    },
}

pub type BlockIndex = u32;

#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Block {
    pub parent_index: Option<BlockIndex>,
    pub statements: Vec<ControlNode>,
    pub result: ResultData,
}
