use string_interner::symbol::SymbolU32;

pub mod builder;
pub use builder::*;

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
        index: LocalIndex,
    },
    Function {
        index: FunctionIndex,
    },
    Int {
        value: i64,
    },
    Assign {
        index: LocalIndex,
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
        arguments: Vec<Expression>,
    },
    Equal {
        left: Box<Expression>,
        right: Box<Expression>,
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
}

#[derive(Debug)]
pub struct Block {
    pub locals: Vec<Local>,
    pub expressions: Vec<Expression>,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Function {
    pub export: bool,
    pub name: SymbolU32,
    pub ty: FunctionType,
    pub block: Block,
}

impl Function {
    pub fn params(&self) -> &[Local] {
        self.block
            .locals
            .get(0..self.ty.params().len())
            .unwrap_or(&[])
    }
}
