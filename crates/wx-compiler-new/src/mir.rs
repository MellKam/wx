use string_interner::symbol::SymbolU32;

use crate::tir;

type LocalIndex = u32;
type ScopeIndex = u32;
type GlobalIndex = u32;
type SignatureIndex = u32;
type FunctionIndex = u32;

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ExprKind {
    Noop,
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
    LocalGet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    LocalSet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    Global {
        global_index: GlobalIndex,
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

#[cfg_attr(test, derive(serde::Serialize))]
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
    Function { signature_index: SignatureIndex },
}

#[cfg_attr(test, derive(serde::Serialize))]
struct Expression {
    kind: ExprKind,
    ty: Type,
}

// the role of MIR is to desugar the syntax like x += 1 into x = x + 1 and lower the concepts like enums into primitive constants, convert labels from symbols in interner into numeric indices

#[cfg_attr(test, derive(serde::Serialize))]
pub struct MIR {
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub exports: Vec<tir::ExportItem>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Local {
    pub ty: Type,
    pub mutability: Mutability,
}

#[cfg_attr(test, derive(serde::Serialize))]
struct BlockScope {
    pub kind: tir::BlockKind,
    pub label: Option<ScopeIndex>,
    pub parent: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub ty: Type,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
    pub signature: tir::FunctionSignature,
    pub frame: Vec<tir::BlockScope>,
    pub block: Expression,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
    pub name: SymbolU32,
    pub ty: Type,
    pub mutability: Mutability,
    pub value: Expression,
}
