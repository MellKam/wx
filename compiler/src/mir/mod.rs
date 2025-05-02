use string_interner::symbol::SymbolU32;

pub mod builder;

#[derive(Debug)]
pub struct MIR {
    pub functions: Vec<Function>,
}

pub type FunctionIndex = u32;
pub type LocalIndex = u32;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub result: Box<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    I32,
    I64,
    Unit,
    Never,
    Function(FunctionType),
}

#[derive(Debug)]
pub enum ExprKind {
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
