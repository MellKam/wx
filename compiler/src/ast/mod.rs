use lexer::TokenKind;
use string_interner::symbol::SymbolU32;

mod diagnostics;
mod lexer;
mod parser;
mod unescape;

pub use parser::*;

use crate::files::FileId;
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExprId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StmtId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ItemId(pub u32);

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

impl BinaryOperator {
    pub fn is_arithmetic(self) -> bool {
        match self {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Remainder => true,
            _ => false,
        }
    }

    pub fn is_relational(self) -> bool {
        match self {
            BinaryOperator::Eq
            | BinaryOperator::NotEq
            | BinaryOperator::Less
            | BinaryOperator::LessEq
            | BinaryOperator::Greater
            | BinaryOperator::GreaterEq => true,
            _ => false,
        }
    }

    pub fn is_assignment(self) -> bool {
        match self {
            BinaryOperator::Assign => true,
            _ => false,
        }
    }
}

// impl std::fmt::Display for BinaryOperator {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let s = match self {
//             BinaryOperator::Add => "+",
//             BinaryOperator::Subtract => "-",
//             BinaryOperator::Multiply => "*",
//             BinaryOperator::Divide => "/",
//             BinaryOperator::Remainder => "%",
//             BinaryOperator::Eq => "==",
//             BinaryOperator::NotEq => "!=",
//             BinaryOperator::Less => "<",
//             BinaryOperator::LessEq => "<=",
//             BinaryOperator::Greater => ">",
//             BinaryOperator::GreaterEq => ">=",
//             BinaryOperator::Assign => "=",
//         };
//         write!(f, "{}", s)
//     }
// }

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
pub struct Identifier {
    pub symbol: SymbolU32,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// `1`
    Int { value: i64 },
    /// `({expr})`
    Grouping { value: ExprId },
    /// `x`
    Identifier { symbol: SymbolU32 },
    /// `-{expr}`
    Unary {
        operator: UnaryOperator,
        operand: ExprId,
    },
    /// `{expr} + {expr}`
    Binary {
        left: ExprId,
        operator: BinaryOperator,
        right: ExprId,
    },
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
    Return { value: ExprId },
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

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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
    pub params: Box<[FunctionParam]>,
    pub result: Option<Identifier>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemFunctionDefinition {
    pub export: Option<Span>,
    pub signature: FunctionSignature,
    pub block: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemEnum {
    pub name: Identifier,
    pub ty: Identifier,
    pub variants: Box<[EnumVariant]>,
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

    pub fn push_item(&mut self, kind: ItemKind, span: Span) -> ItemId {
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
