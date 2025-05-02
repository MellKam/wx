pub mod builder;

use std::str;

use string_interner::symbol::SymbolU32;

use crate::ast::{BinaryOperator, UnaryOperator};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    I32,
    I64,
    Unit,
    Never,
}

impl TryFrom<&str> for PrimitiveType {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i32" => Ok(PrimitiveType::I32),
            "i64" => Ok(PrimitiveType::I64),
            "never" => Ok(PrimitiveType::Never),
            "()" => Ok(PrimitiveType::Unit),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComptimeType {
    Int,
}

impl ComptimeType {
    pub fn can_coerce_into(self, ty: PrimitiveType) -> bool {
        match (self, ty) {
            (ComptimeType::Int, PrimitiveType::I32 | PrimitiveType::I64) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<PrimitiveType>,
    pub result: PrimitiveType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Comptime(ComptimeType),
    Function(FunctionType),
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
    Decl { index: LocalIndex, expr: Expression },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Local(LocalIndex),
    Function(FunctionIndex),
    Placeholder,
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Binary {
        operator: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mutability {
    Mutable,
    Const,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub name: SymbolU32,
    pub ty: PrimitiveType,
    pub mutability: Mutability,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: SymbolU32,
    pub signature: FunctionType,
    pub locals: Vec<Local>,
    pub body: Vec<Statement>,
}

impl Function {
    pub fn params(&self) -> &[Local] {
        self.locals
            .get(..self.signature.params.len())
            .unwrap_or(&[])
    }
}
