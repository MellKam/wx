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
    pub fn coercible_to(self, ty: PrimitiveType) -> bool {
        match (self, ty) {
            (ComptimeType::Int, PrimitiveType::I32 | PrimitiveType::I64) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Comptime(ComptimeType),
    Function(FunctionIndex),
    Enum(EnumIndex),
}

impl Type {
    pub fn coercible_to(self, ty: PrimitiveType) -> bool {
        match (self, ty) {
            (Type::Primitive(ty1), ty2) => ty1 == ty2,
            (Type::Comptime(comptime_type), PrimitiveType::I32 | PrimitiveType::I64) => {
                comptime_type.coercible_to(ty)
            }
            _ => false,
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
    pub statements: Vec<Statement>,
    pub result: Option<Expression>,
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
