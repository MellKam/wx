pub mod builder;

use std::str;

use string_interner::symbol::SymbolU32;

use crate::ast::{BinaryOperator, UnaryOperator};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    I32,
    I64,
    Unit,
    Never,
    ComptimeInt,
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "never" => Ok(Type::Never),
            "()" => Ok(Type::Unit),
            _ => Err(()),
        }
    }
}

impl Type {
    pub fn can_coerce_into(self, other: Self) -> Result<(), ()> {
        match (self, other) {
            (Type::ComptimeInt, Type::I32 | Type::I64) => Ok(()),
            _ if self == other => Ok(()),
            _ => Err(()),
        }
    }
}

pub type LocalIndex = u32;
pub type FunctionIndex = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct HIR {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return { expr: Expression },
    Expr { expr: Expression },
    Assign { index: LocalIndex, expr: Expression },
    Local { index: LocalIndex, expr: Expression },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Binary {
        operator: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Int(i64),
    Local(LocalIndex),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mutability {
    Mutable,
    Const,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub name: SymbolU32,
    pub ty: Type,
    pub mutability: Mutability,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub name: SymbolU32,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub params: Vec<FunctionParam>,
    pub result: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: SymbolU32,
    pub signature: FunctionSignature,
    pub locals: Vec<Local>,
    pub body: Vec<Statement>,
}
