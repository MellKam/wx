use string_interner::symbol::SymbolU32;

pub mod builder;
pub use builder::*;

use crate::hir;

#[derive(Debug)]
pub struct MIR {
    pub functions: Vec<Function>,
}

pub type FunctionIndex = u32;
pub type LocalIndex = u32;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub param_count: usize,
    pub params_results: Vec<Type>,
}

impl FunctionType {
    pub fn params(&self) -> &[Type] {
        self.params_results.get(..self.param_count).unwrap_or(&[])
    }

    pub fn result(&self) -> &Type {
        self.params_results
            .get(self.param_count..)
            .unwrap_or(&[])
            .first()
            .unwrap_or(&Type::Unit)
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    I32,
    I64,
    Unit,
    Never,
    Function(FunctionIndex),
}

#[derive(Debug)]
pub enum ExprKind {
    Noop,
    Local {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    Function {
        index: FunctionIndex,
    },
    Int {
        value: i64,
    },
    Assign {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Mul {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Return {
        value: Box<Expression>,
    },
    Drop {
        value: Box<Expression>,
    },
    Call {
        callee: FunctionIndex,
        arguments: Box<[Expression]>,
    },
    Equal {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Block {
        scope_index: ScopeIndex,
        expressions: Box<[Expression]>,
    },
    Break {
        scope_index: ScopeIndex,
        value: Option<Box<Expression>>,
    },
    IfElse {
        condition: Box<Expression>,
        then_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Local {
    pub name: SymbolU32,
    pub ty: Type,
    pub mutability: hir::Mutability,
}

#[derive(Debug)]
pub struct Function {
    pub export: bool,
    pub name: SymbolU32,
    pub ty: FunctionType,
    pub scopes: Vec<LocalScope>,
    pub block: Expression,
}

#[derive(Debug, Clone, Copy)]
pub struct ScopeIndex(pub u32);

#[derive(Debug)]
pub struct LocalScope {
    pub parent_scope: Option<ScopeIndex>,
    pub locals: Vec<Local>,
}
