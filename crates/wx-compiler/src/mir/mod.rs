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
    pub data_nodes: Vec<DataNode>,
    pub data_lookup: HashMap<DataNodeKind, DataNodeId>,
    pub block: Block,
    pub params: Box<[FunctionParam]>,
    pub symbol: SymbolU32,
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResultType {
    Unit,
    Never,
    Value(ValueType),
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StackResult {
    Unit,
    Never,
    Value(DataNodeId),
}

impl StackResult {
    pub fn unwrap_value(&self) -> DataNodeId {
        match self {
            StackResult::Value(id) => *id,
            _ => panic!("expected value result"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
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

impl MIR {
    fn ensure_data_node(&mut self, kind: DataNodeKind) -> DataNodeId {
        match self.data_lookup.get(&kind).copied() {
            Some(id) => return id,
            None => {}
        }

        let id = self.data_nodes.len() as u32;
        self.data_nodes.push(DataNode {
            kind: kind.clone(),
            uses: Vec::new(),
        });
        self.data_lookup.insert(kind, id);
        id
    }
}

pub type DataNodeId = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(Serialize))]
pub enum DataNodeKind {
    Int {
        value: i64,
        ty: ValueType,
    },
    Float {
        value: u64,
        ty: ValueType,
    },
    Parameter {
        index: u32,
        symbol: SymbolU32,
        ty: ValueType,
    },
    Add {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    Mul {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    Sub {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    Div {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    Lt {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    LtEq {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    Gt {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    GtEq {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    Eq {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    NotEq {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
    Phi {
        left: DataNodeId,
        right: DataNodeId,
        ty: ValueType,
    },
}

impl DataNodeKind {
    pub fn ty(&self) -> ValueType {
        match self {
            DataNodeKind::Int { ty, .. } => *ty,
            DataNodeKind::Float { ty, .. } => *ty,
            DataNodeKind::Parameter { ty, .. } => *ty,
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
        }
    }
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone)]
pub struct DataNode {
    pub kind: DataNodeKind,
    pub uses: Vec<DataNodeId>,
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Debug, Clone)]
pub enum ControlNode {
    Return {
        value: StackResult,
    },
    IfElse {
        condition: DataNodeId,
        then_block: Block,
        else_block: Option<Block>,
        result: StackResult,
    },
}

#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<ControlNode>,
    pub result: StackResult,
    pub relative_locals: Box<[StackResult]>,
}
