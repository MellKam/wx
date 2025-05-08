use lexer::TokenKind;
use string_interner::symbol::SymbolU32;

mod diagnostics;
mod lexer;
mod parser;
mod unescape;

pub use parser::*;

use crate::files::FileId;
use crate::span::Span;

pub type ExprId = u32;
pub type StmtId = u32;
pub type ItemId = u32;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    /// Sign inversion `-x`
    Invert,
    // /// Logical negation `!x`
    // Negate,
}

impl TryFrom<TokenKind> for UnaryOperator {
    type Error = ();

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::Minus => Ok(UnaryOperator::Invert),
            // TokenKind::Bang => Ok(UnaryOperator::Negate),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Assign,
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    // Relational
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

impl TryFrom<TokenKind> for BinaryOperator {
    type Error = ();

    fn try_from(tag: TokenKind) -> Result<Self, Self::Error> {
        match tag {
            TokenKind::Plus => Ok(BinaryOperator::Add),
            TokenKind::Minus => Ok(BinaryOperator::Subtract),
            TokenKind::Star => Ok(BinaryOperator::Multiply),
            TokenKind::Slash => Ok(BinaryOperator::Divide),
            TokenKind::Percent => Ok(BinaryOperator::Remainder),
            TokenKind::EqEq => Ok(BinaryOperator::Eq),
            TokenKind::BangEq => Ok(BinaryOperator::NotEq),
            TokenKind::OpenAngle => Ok(BinaryOperator::Less),
            TokenKind::LessEq => Ok(BinaryOperator::LessEq),
            TokenKind::CloseAngle => Ok(BinaryOperator::Greater),
            TokenKind::GreaterEq => Ok(BinaryOperator::GreaterEq),
            TokenKind::Eq => Ok(BinaryOperator::Assign),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// `1`
    Int { value: i64 },
    /// `x`
    Identifier { symbol: SymbolU32 },
    /// `-x`
    Unary {
        operator: UnaryOperator,
        operand: ExprId,
    },
    /// `x + y`
    Binary {
        left: ExprId,
        operator: BinaryOperator,
        right: ExprId,
    },
    /// `x()`
    Call {
        callee: ExprId,
        arguments: Vec<ExprId>,
    },
    /// `x::y`
    NamespaceMember { namespace: ExprId, member: ExprId },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {
    Expression {
        expr: ExprId,
    },
    ConstDefinition {
        name: SymbolU32,
        ty: Option<SymbolU32>,
        value: ExprId,
    },
    MutableDefinition {
        name: SymbolU32,
        ty: Option<SymbolU32>,
        value: ExprId,
    },
    Return {
        value: ExprId,
    },
}

#[derive(Debug, PartialEq)]
pub struct Statement {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub name: Identifier,
    pub ty: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub params: Vec<FunctionParam>,
    pub result: Option<Identifier>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<StmtId>,
    pub result: Option<ExprId>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemFunctionDefinition {
    pub export: Option<Span>,
    pub signature: FunctionSignature,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub symbol: SymbolU32,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemEnum {
    pub name: Identifier,
    pub ty: Identifier,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Identifier,
    pub value: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    FunctionDefinition(ItemFunctionDefinition),
    Enum(ItemEnum),
    FunctionDeclaration { signature: FunctionSignature },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub file_id: FileId,
    pub expressions: Vec<Expression>,
    pub statements: Vec<Statement>,
    pub items: Vec<Item>,
}

impl Ast {
    pub fn new(file_id: FileId) -> Self {
        Self {
            file_id,
            expressions: Vec::new(),
            statements: Vec::new(),
            items: Vec::new(),
        }
    }

    pub fn push_expr(&mut self, kind: ExprKind, span: Span) -> ExprId {
        let id = self.expressions.len() as ExprId;
        self.expressions.push(Expression { kind, span });
        return id;
    }

    pub fn get_expr(&self, id: ExprId) -> Option<&Expression> {
        self.expressions.get(id as usize)
    }

    pub fn push_stmt(&mut self, kind: StmtKind, span: Span) -> StmtId {
        let id = self.statements.len() as StmtId;
        self.statements.push(Statement { kind, span });
        return id;
    }

    pub fn get_stmt(&self, id: StmtId) -> Option<&Statement> {
        self.statements.get(id as usize)
    }

    pub fn push_item(&mut self, kind: ItemKind, span: Span) -> ItemId {
        let id = self.items.len() as ItemId;
        self.items.push(Item { kind, span });
        return id;
    }

    pub fn get_item(&self, id: ItemId) -> Option<&Item> {
        self.items.get(id as usize)
    }

    pub fn set_item(&mut self, id: ItemId, cb: impl FnOnce(&mut Item)) -> Result<(), ()> {
        match self.items.get_mut(id as usize) {
            Some(item) => {
                cb(item);
                Ok(())
            }
            None => Err(()),
        }
    }
}
