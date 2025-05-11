pub mod builder;
pub mod diagnostics;
use std::str;

pub use builder::*;
use string_interner::symbol::SymbolU32;

use crate::ast::{BinaryOperator, UnaryOperator};

#[derive(Debug, Clone)]
pub struct HIR {
    pub functions: Vec<Function>,
    pub enums: Vec<Enum>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: SymbolU32,
    pub ty: PrimitiveType,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: SymbolU32,
    pub value: i64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    I32,
    I64,
}

impl TryFrom<&str> for PrimitiveType {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i32" => Ok(PrimitiveType::I32),
            "i64" => Ok(PrimitiveType::I64),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Function(FunctionIndex),
    Enum(EnumIndex),
    Unit,
    Never,
    Unknown,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Primitive(ty) => write!(f, "{:?}", ty),
            Type::Function(index) => write!(f, "function({})", index),
            Type::Enum(index) => write!(f, "enum({})", index),
            Type::Unit => write!(f, "unit"),
            Type::Never => write!(f, "never"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub result: Type,
}

pub type LocalIndex = u32;
pub type FunctionIndex = u32;
pub type EnumIndex = u32;
pub type EnumVariantIndex = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i64),
    LocalDeclaration {
        index: LocalIndex,
        expr: Box<Expression>,
    },
    Local(LocalIndex),
    Function(FunctionIndex),
    Return(Box<Expression>),
    EnumVariant {
        enum_index: EnumIndex,
        variant_index: EnumVariantIndex,
    },
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
    IfElse {
        condition: Box<Expression>,
        then: Block,
        else_: Option<Block>,
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
    pub ty: Type,
    pub mutability: Mutability,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub locals: Vec<Local>,
    pub expressions: Vec<Expression>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub export: bool,
    pub name: SymbolU32,
    pub ty: FunctionType,
    pub block: Block,
}

impl Function {
    pub fn params(&self) -> &[Local] {
        self.block.locals.get(..self.ty.params.len()).unwrap_or(&[])
    }
}
