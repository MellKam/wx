use std::cell::RefCell;
use std::rc::{Rc, Weak};

use string_interner::symbol::SymbolU32;

#[cfg(test)]
use serde::{Serialize, Serializer};

pub mod builder;
pub use builder::*;

use crate::hir;

#[cfg_attr(test, derive(Debug, Serialize))]
pub struct MIR {
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub exports: Vec<hir::ExportItem>,
}

pub type FunctionIndex = u32;
pub type GlobalIndex = u32;

#[derive(Clone)]
#[cfg_attr(test, derive(Debug, Serialize))]
pub struct FunctionType {
    pub param_count: usize,
    pub params_results: Box<[Type]>,
}

impl FunctionType {
    pub fn params(&self) -> &[Type] {
        self.params_results.get(..self.param_count).unwrap_or(&[])
    }

    pub fn result(&self) -> Type {
        self.params_results
            .get(self.param_count..)
            .unwrap_or(&[])
            .first()
            .copied()
            .unwrap_or(Type::Unit)
    }
}

#[derive(Clone, Copy)]
#[cfg_attr(test, derive(Debug, Serialize))]
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

#[cfg_attr(test, derive(Debug, Serialize))]
pub enum ExprKind {
    Noop,
    Local {
        local: Weak<RefCell<Local>>,
        #[cfg_attr(test, serde(serialize_with = "serialize_scope_index"))]
        scope: Weak<RefCell<BlockScope>>,
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
        local: Weak<RefCell<Local>>,
        #[cfg_attr(test, serde(serialize_with = "serialize_scope_index"))]
        scope: Weak<RefCell<BlockScope>>,
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
        scope: Rc<RefCell<BlockScope>>,
        expressions: Box<[Expression]>,
    },
    Break {
        scope: Weak<RefCell<BlockScope>>,
        value: Option<Box<Expression>>,
    },
    Continue {
        scope: Weak<RefCell<BlockScope>>,
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
        block: Box<Expression>,
    },
    Neg {
        value: Box<Expression>,
    },
}

#[cfg_attr(test, derive(Debug, Serialize))]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(test, derive(Debug, Serialize))]
pub enum Mutability {
    Mutable,
    Const,
}

#[cfg_attr(test, derive(Debug, Serialize))]
pub struct Local {
    pub index: u32,
    pub symbol: SymbolU32,
    pub ty: Type,
    pub mutability: Mutability,
    #[cfg_attr(test, serde(skip))]
    pub next: Option<Rc<RefCell<Local>>>,
    #[cfg_attr(test, serde(skip))]
    pub prev: Option<Weak<RefCell<Local>>>,
}

#[cfg_attr(test, derive(Debug, Serialize))]
pub struct Function {
    pub symbol: SymbolU32,
    pub ty: FunctionType,
    pub block: Expression,
}

#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(test, derive(Debug, Serialize))]
pub enum BlockKind {
    Block,
    Loop,
}

#[cfg_attr(test, derive(Debug, Serialize))]
pub struct BlockScope {
    pub index: u32,
    pub kind: BlockKind,
    #[cfg_attr(test, serde(serialize_with = "serialize_block_scope_parent"))]
    pub parent: Option<Weak<RefCell<BlockScope>>>,
    #[cfg_attr(test, serde(serialize_with = "serialize_block_scope_children"))]
    pub children: Vec<Weak<RefCell<BlockScope>>>,
    #[cfg_attr(test, serde(skip))]
    pub locals: Option<Rc<RefCell<Local>>>,
    pub result: Type,
}

#[cfg(test)]
fn serialize_block_scope_parent<S>(
    parent: &Option<Weak<RefCell<BlockScope>>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    parent
        .as_ref()
        .map(|parent| parent.upgrade().map(|p| p.borrow().index))
        .serialize(serializer)
}

#[cfg(test)]
fn serialize_block_scope_children<S>(
    children: &Vec<Weak<RefCell<BlockScope>>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let indicies: Vec<_> = children
        .iter()
        .map(|child| child.upgrade().map(|child| child.borrow().index))
        .collect();
    indicies.serialize(serializer)
}

#[cfg(test)]
fn serialize_scope_index<S>(
    scope: &Weak<RefCell<BlockScope>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match scope.upgrade() {
        Some(scope_ref) => scope_ref.borrow().index.serialize(serializer),
        None => serializer.serialize_none(),
    }
}

#[cfg_attr(test, derive(Debug, Serialize))]
pub struct Global {
    pub name: SymbolU32,
    pub ty: Type,
    pub mutability: Mutability,
    pub value: Expression,
}
