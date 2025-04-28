use string_interner::symbol::SymbolU32;

pub mod builder;

#[derive(Debug)]
pub struct MIR {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Unit,
    I32,
    I64,
}

#[derive(Debug)]
pub enum ExprKind {
    Local {
        index: u32,
    },
    Int {
        value: i64,
    },
    Assign {
        index: u32,
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
    Negate {
        operand: Box<Expression>,
    },
    Return {
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Function {
    pub name: SymbolU32,
    pub param_count: u32,
    pub locals: Vec<Type>,
    pub output: Vec<Type>,
    pub body: Vec<Expression>,
}
