pub mod builder;

#[derive(Debug)]
pub struct MIR {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct LocalIndex(pub u32);

#[derive(Debug)]
pub enum Type {
    Unit,
    I32,
    I64,
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
    pub index: LocalIndex,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Function {
    pub locals: Vec<Local>,
    pub output: Vec<Type>,
    pub body: Vec<Expression>,
}
