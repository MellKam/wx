pub mod builder;

use string_interner::symbol::SymbolU32;

use crate::ast::{BinaryOperator, UnaryOperator};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RuntimeType {
    Unit,
    I32,
    I64,
}

impl RuntimeType {
    #[inline]
    pub fn resolve(left: RuntimeType, right: RuntimeType) -> Result<RuntimeType, ()> {
        match (left, right) {
            (RuntimeType::I32, RuntimeType::I32) => Ok(RuntimeType::I32),
            (RuntimeType::I64, RuntimeType::I64) => Ok(RuntimeType::I64),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComptimeType {
    Int,
}

impl ComptimeType {
    #[inline]
    pub fn resolve(left: ComptimeType, right: ComptimeType) -> Result<ComptimeType, ()> {
        match (left, right) {
            (ComptimeType::Int, ComptimeType::Int) => Ok(ComptimeType::Int),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Runtime(RuntimeType),
    Comptime(ComptimeType),
}

impl Type {
    pub fn resolve(left: Type, right: Type) -> Result<Type, ()> {
        use Type::*;
        match (left, right) {
            (Runtime(left), Runtime(right)) => RuntimeType::resolve(left, right).map(Runtime),
            (Comptime(left), Comptime(right)) => ComptimeType::resolve(left, right).map(Comptime),
            (Runtime(left), Comptime(right)) => match (left, right) {
                (RuntimeType::I32, ComptimeType::Int) => Ok(Runtime(RuntimeType::I32)),
                (RuntimeType::I64, ComptimeType::Int) => Ok(Runtime(RuntimeType::I64)),
                _ => Err(()),
            },
            (Comptime(left), Runtime(right)) => match (left, right) {
                (ComptimeType::Int, RuntimeType::I32) => Ok(Runtime(RuntimeType::I32)),
                (ComptimeType::Int, RuntimeType::I64) => Ok(Runtime(RuntimeType::I64)),
                _ => Err(()),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunctionIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LocalIndex(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct HIR {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return {
        ty: RuntimeType,
        expr: Expression,
    },
    Expr {
        ty: RuntimeType,
        expr: Expression,
    },
    Local {
        index: LocalIndex,
        ty: RuntimeType,
        expr: Expression,
    },
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
pub enum BindingType {
    Param,
    Mutable,
    Const,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub name: SymbolU32,
    pub index: LocalIndex,
    pub ty: RuntimeType,
    pub binding: BindingType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: SymbolU32,
    pub locals: Vec<Local>,
    pub result: RuntimeType,
    pub body: Vec<Statement>,
}
