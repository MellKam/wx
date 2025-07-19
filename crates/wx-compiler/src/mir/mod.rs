use serde::Serialize;
use string_interner::symbol::SymbolU32;

pub mod builder;
pub use builder::*;

use crate::hir;

#[derive(Debug, Serialize)]
pub struct MIR {
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub exports: Vec<hir::ExportItem>,
}

pub type FunctionIndex = u32;
pub type LocalIndex = u32;
pub type GlobalIndex = u32;

#[derive(Debug, Clone, Serialize)]
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

#[derive(Debug, Clone, Copy, Serialize)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    U32,
    U64,
    Unit,
    Never,
    Bool,
    Function(FunctionIndex),
}

#[derive(Debug, Serialize)]
pub enum ExprKind {
    Noop,
    Local {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    Global {
        global_index: GlobalIndex,
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
    Float {
        value: f64,
    },
    LocalSet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    GlobalSet {
        global_index: GlobalIndex,
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
        callee: Box<Expression>,
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
    Neg {
        value: Box<Expression>,
    },
}

#[derive(Debug, Serialize)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum Mutability {
    Mutable,
    Const,
}

#[derive(Debug, Serialize)]
pub struct Local {
    pub name: SymbolU32,
    pub ty: Type,
    pub mutability: Mutability,
}

#[derive(Debug, Serialize)]
pub struct Function {
    pub name: SymbolU32,
    pub ty: FunctionType,
    pub frame: Vec<BlockScope>,
    pub block: Expression,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct ScopeIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum BlockKind {
    Block,
    Loop,
}

#[derive(Debug, Serialize)]
pub struct BlockScope {
    pub kind: BlockKind,
    pub parent: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub result: Type,
}

#[derive(Debug, Serialize)]
pub struct Global {
    pub name: SymbolU32,
    pub ty: Type,
    pub mutability: Mutability,
    pub value: Expression,
}
