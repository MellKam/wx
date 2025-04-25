pub mod builder;

use crate::ast::{BinaryOperator, BindingType, UnaryOperator};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    Unit,
    I32,
    I64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuncIndex(u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LocalIndex(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct HIR {
    pub functions: Vec<HIRFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HIRStmt {
    pub kind: HIRStmtKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HIRStmtKind {
    Return {
        expr: HIRExpr,
    },
    Expr {
        expr: HIRExpr,
    },
    Local {
        local_index: LocalIndex,
        expr: HIRExpr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct HIRExpr {
    pub kind: HIRExprKind,
    pub ty: PrimitiveType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HIRExprKind {
    Binary {
        operator: BinaryOperator,
        lhs: Box<HIRExpr>,
        rhs: Box<HIRExpr>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<HIRExpr>,
    },
    Int(i64),
    Local(LocalIndex),
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct HIRLocal {
    index: LocalIndex,
    ty: PrimitiveType,
    binding: BindingType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HIRFunction {
    locals: Vec<HIRLocal>,
    result: PrimitiveType,
    body: Vec<HIRStmt>,
}
