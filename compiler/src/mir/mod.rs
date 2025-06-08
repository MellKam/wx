use string_interner::symbol::SymbolU32;

pub mod builder;
pub use builder::*;

use crate::hir;

#[derive(Debug)]
pub struct MIR {
    pub functions: Vec<Function>,
    pub exports: Vec<hir::ExportItem>,
}

pub type FunctionIndex = u32;
pub type LocalIndex = u32;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub param_count: usize,
    pub params_results: Box<[Type]>,
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
    Bool,
    Function(FunctionIndex),
}

#[derive(Debug)]
pub enum ExprKind {
    Noop,
    Local {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    Bool {
        value: bool,
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
    Div {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Rem {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    And {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Or {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    Drop {
        value: Box<Expression>,
    },
    Call {
        callee: FunctionIndex,
        arguments: Box<[Expression]>,
    },
    Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Eqz {
        value: Box<Expression>,
    },
    NotEq {
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
    Continue {
        scope_index: ScopeIndex,
    },
    Unreachable,
    IfElse {
        condition: Box<Expression>,
        then_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
    BitAnd {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitOr {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitXor {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitNot {
        value: Box<Expression>,
    },
    LeftShift {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    RightShift {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Less {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    LessEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Greater {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    GreaterEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Loop {
        scope_index: ScopeIndex,
        block: Box<Expression>,
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
    pub result: Type,
}
