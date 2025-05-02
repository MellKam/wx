use string_interner::symbol::SymbolU32;

pub mod builder;

#[derive(Debug)]
pub struct MIR {
    pub functions: Vec<Function>,
}

pub type LocalIndex = u32;

#[derive(Debug, Clone, Copy)]
pub enum Type {
    I32,
    I64,
    Unit,
    Never,
}

#[derive(Debug)]
pub enum ExprKind {
    Local {
        index: LocalIndex,
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
pub struct Local {
    pub name: SymbolU32,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Function {
    pub name: SymbolU32,
    pub param_count: usize,
    pub locals: Vec<Local>,
    pub result: Type,
    pub body: Vec<Expression>,
}

impl Function {
    pub fn params(&self) -> &[Local] {
        self.locals.get(0..self.param_count).unwrap_or(&[])
    }
}
