use bumpalo::collections::Vec;
use lexer::TokenKind;
use string_interner::symbol::SymbolU32;

mod diagnostics;
mod lexer;
mod parser;
mod unescape;

pub use parser::*;

use crate::files::FileId;
use crate::span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExprId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StmtId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ItemId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    InvertSign,
    Not,
    BitNot,
}

impl TryFrom<TokenKind> for UnaryOp {
    type Error = ();

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::Minus => Ok(UnaryOp::InvertSign),
            TokenKind::Bang => Ok(UnaryOp::Not),
            TokenKind::Caret => Ok(UnaryOp::BitNot),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    // Logical
    And,
    Or,
    // Assignment
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(tag: TokenKind) -> Result<Self, Self::Error> {
        match tag {
            // Arithmetic
            TokenKind::Plus => Ok(BinaryOp::Add),
            TokenKind::Minus => Ok(BinaryOp::Sub),
            TokenKind::Star => Ok(BinaryOp::Mul),
            TokenKind::Slash => Ok(BinaryOp::Div),
            TokenKind::Percent => Ok(BinaryOp::Rem),
            // Relational
            TokenKind::EqEq => Ok(BinaryOp::Eq),
            TokenKind::BangEq => Ok(BinaryOp::NotEq),
            TokenKind::Less => Ok(BinaryOp::Less),
            TokenKind::LessEq => Ok(BinaryOp::LessEq),
            TokenKind::Greater => Ok(BinaryOp::Greater),
            TokenKind::GreaterEq => Ok(BinaryOp::GreaterEq),
            // Logical
            TokenKind::AmperAmper => Ok(BinaryOp::And),
            TokenKind::VbarVbar => Ok(BinaryOp::Or),
            // Assignment
            TokenKind::Eq => Ok(BinaryOp::Assign),
            TokenKind::PlusEq => Ok(BinaryOp::AddAssign),
            TokenKind::MinusEq => Ok(BinaryOp::SubAssign),
            TokenKind::StarEq => Ok(BinaryOp::MulAssign),
            TokenKind::SlashEq => Ok(BinaryOp::DivAssign),
            TokenKind::PercentEq => Ok(BinaryOp::RemAssign),
            // Bitwise
            TokenKind::Amper => Ok(BinaryOp::BitAnd),
            TokenKind::Vbar => Ok(BinaryOp::BitOr),
            TokenKind::Caret => Ok(BinaryOp::BitXor),
            TokenKind::LeftShift => Ok(BinaryOp::LeftShift),
            TokenKind::RightShift => Ok(BinaryOp::RightShift),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub symbol: SymbolU32,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub left: ExprId,
    pub operator: BinaryOp,
    pub operator_span: TextSpan,
    pub right: ExprId,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// `1`
    Int { value: i64 },
    /// `({expr})`
    Grouping { value: ExprId },
    /// `x`
    Identifier { symbol: SymbolU32 },
    /// `5 as i32`
    Cast { value: ExprId, ty: Identifier },
    /// `-{expr}`
    Unary { operator: UnaryOp, operand: ExprId },
    /// `{expr} + {expr}`
    Binary(BinaryExpression),
    /// `{expr}()`
    Call {
        callee: ExprId,
        arguments: Box<[ExprId]>,
    },
    /// `{expr}::{expr}`
    NamespaceMember {
        namespace: Identifier,
        member: Identifier,
    },
    /// `return {expr}`
    Return { value: Option<ExprId> },
    /// `{ ... }`
    Block {
        statements: Box<[StmtId]>,
        result: Option<ExprId>,
    },
    /// `if {expr} { ... }`
    IfElse {
        condition: ExprId,
        then_block: ExprId,
        else_block: Option<ExprId>,
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// `{expr};`
    DelimitedExpression { value: ExprId },
    /// `const {identifier}(: {type})? = {expr};`
    ConstDefinition {
        name: Identifier,
        ty: Option<Identifier>,
        value: ExprId,
    },
    /// `mut {identifier}(: {type})? = {expr};`
    MutableDefinition {
        name: Identifier,
        ty: Option<Identifier>,
        value: ExprId,
    },
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StmtKind,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: Identifier,
    pub ty: Identifier,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub params: Box<[FunctionParam]>,
    pub result: Option<Identifier>,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub struct ItemFunctionDefinition {
    pub export: Option<TextSpan>,
    pub signature: FunctionSignature,
    pub block: ExprId,
}

#[derive(Debug, Clone)]
pub struct ItemEnum {
    pub name: Identifier,
    pub ty: Identifier,
    pub variants: Box<[EnumVariant]>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: Identifier,
    pub value: ExprId,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    FunctionDefinition(ItemFunctionDefinition),
    Enum(ItemEnum),
    FunctionDeclaration { signature: FunctionSignature },
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub span: TextSpan,
}

#[derive(Debug)]
pub struct Ast<'bump> {
    pub file_id: FileId,
    pub expressions: Vec<'bump, Expression>,
    pub statements: Vec<'bump, Statement>,
    pub items: Vec<'bump, Item>,
}

impl<'bump> Ast<'bump> {
    pub fn new(allocator: &'bump bumpalo::Bump, file_id: FileId) -> Self {
        Self {
            file_id,
            expressions: Vec::new_in(allocator),
            statements: Vec::new_in(allocator),
            items: Vec::new_in(allocator),
        }
    }

    pub fn push_expr(&mut self, kind: ExprKind, span: TextSpan) -> ExprId {
        let id = ExprId(self.expressions.len() as u32);
        self.expressions.push(Expression { kind, span });
        return id;
    }

    pub fn get_expr(&self, id: ExprId) -> Option<&Expression> {
        self.expressions.get(id.0 as usize)
    }

    pub fn set_expr(&mut self, id: ExprId, cb: impl FnOnce(&mut Expression)) -> Result<(), ()> {
        match self.expressions.get_mut(id.0 as usize) {
            Some(expr) => {
                cb(expr);
                Ok(())
            }
            None => Err(()),
        }
    }

    pub fn push_item(&mut self, kind: ItemKind, span: TextSpan) -> ItemId {
        let id = ItemId(self.items.len() as u32);
        self.items.push(Item { kind, span });
        return id;
    }

    pub fn get_stmt(&self, id: StmtId) -> Option<&Statement> {
        self.statements.get(id.0 as usize)
    }

    pub fn push_stmt(&mut self, stmt: Statement) -> StmtId {
        let id = StmtId(self.statements.len() as u32);
        self.statements.push(stmt);
        return id;
    }

    pub fn get_item(&self, id: ItemId) -> Option<&Item> {
        self.items.get(id.0 as usize)
    }

    pub fn set_item(&mut self, id: ItemId, cb: impl FnOnce(&mut Item)) -> Result<(), ()> {
        match self.items.get_mut(id.0 as usize) {
            Some(item) => {
                cb(item);
                Ok(())
            }
            None => Err(()),
        }
    }
}
