use codespan::Span;
use lexer::{TokenKind, TokenTag};
use string_interner::symbol::SymbolU32;

mod diagnostics;
mod lexer;
mod parser;
mod unescape;

pub use diagnostics::*;
pub use parser::*;

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

impl TryFrom<TokenTag> for BinaryOperator {
    type Error = ();

    fn try_from(tag: TokenTag) -> Result<Self, Self::Error> {
        match tag {
            TokenTag::Plus => Ok(BinaryOperator::Add),
            TokenTag::Minus => Ok(BinaryOperator::Subtract),
            TokenTag::Star => Ok(BinaryOperator::Multiply),
            TokenTag::Slash => Ok(BinaryOperator::Divide),
            TokenTag::Percent => Ok(BinaryOperator::Remainder),
            TokenTag::EqEq => Ok(BinaryOperator::Eq),
            TokenTag::BangEq => Ok(BinaryOperator::NotEq),
            TokenTag::OpenAngle => Ok(BinaryOperator::Less),
            TokenTag::LessEq => Ok(BinaryOperator::LessEq),
            TokenTag::CloseAngle => Ok(BinaryOperator::Greater),
            TokenTag::GreaterEq => Ok(BinaryOperator::GreaterEq),
            TokenTag::Eq => Ok(BinaryOperator::Assign),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int {
        value: i64,
    },
    // Float {
    //     value: f64,
    // },
    // String {
    //     symbol: SymbolU32,
    // },
    Identifier {
        symbol: SymbolU32,
    },
    Unary {
        operator: UnaryOperator,
        operand: ExprId,
    },
    Binary {
        left: ExprId,
        right: ExprId,
        operator: BinaryOperator,
    },
    Call {
        callee: ExprId,
        arguments: Vec<ExprId>,
    },
    // Member {
    //     object: ExprId,
    //     property: SymbolU32,
    // },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: Span,
    pub id: ExprId,
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
    pub id: StmtId,
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunctionParam {
    pub name: SymbolU32,
    pub ty: SymbolU32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub name: SymbolU32,
    pub params: Vec<FunctionParam>,
    pub result: Option<SymbolU32>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub export: Option<Span>,
    pub signature: FunctionSignature,
    pub body: Vec<StmtId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    FunctionDefinition(FunctionDefinition),
    FunctionDeclaration { signature: FunctionSignature },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub id: ItemId,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub expressions: Vec<Expression>,
    pub statements: Vec<Statement>,
    pub items: Vec<Item>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            expressions: Vec::new(),
            statements: Vec::new(),
            items: Vec::new(),
        }
    }

    pub fn push_expr(&mut self, kind: ExprKind, span: Span) -> ExprId {
        let id = self.expressions.len() as ExprId;
        self.expressions.push(Expression { kind, span, id });
        return id;
    }

    pub fn get_expr(&self, id: ExprId) -> Option<&Expression> {
        self.expressions.get(id as usize)
    }

    pub fn push_stmt(&mut self, kind: StmtKind, span: Span) -> StmtId {
        let id = self.statements.len() as StmtId;
        self.statements.push(Statement { id, kind, span });
        return id;
    }

    pub fn get_stmt(&self, id: StmtId) -> Option<&Statement> {
        self.statements.get(id as usize)
    }

    pub fn push_item(&mut self, kind: ItemKind, span: Span) -> ItemId {
        let id = self.items.len() as ItemId;
        self.items.push(Item { id, kind, span });
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
