use lexer::TokenKind;
use string_interner::symbol::SymbolU32;

pub mod diagnostics;
mod lexer;
mod parser;
mod unescape;

pub use parser::*;

use crate::files::FileId;
use crate::span::TextSpan;

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

impl BinaryOp {
    pub fn is_assignment(&self) -> bool {
        match self {
            BinaryOp::Assign
            | BinaryOp::AddAssign
            | BinaryOp::SubAssign
            | BinaryOp::MulAssign
            | BinaryOp::DivAssign
            | BinaryOp::RemAssign => true,
            _ => false,
        }
    }

    pub fn is_comparison(&self) -> bool {
        match self {
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => true,
            _ => false,
        }
    }

    pub fn is_logical(&self) -> bool {
        match self {
            BinaryOp::And | BinaryOp::Or => true,
            _ => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match self {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => true,
            _ => false,
        }
    }

    pub fn is_bitwise(&self) -> bool {
        match self {
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::LeftShift
            | BinaryOp::RightShift => true,
            _ => false,
        }
    }
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

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Rem => "%",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEq => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEq => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Assign => "=",
            BinaryOp::AddAssign => "+=",
            BinaryOp::SubAssign => "-=",
            BinaryOp::MulAssign => "*=",
            BinaryOp::DivAssign => "/=",
            BinaryOp::RemAssign => "%=",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::LeftShift => "<<",
            BinaryOp::RightShift => ">>",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    pub symbol: SymbolU32,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// `1`
    Int { value: i64 },
    /// `({expr})`
    Grouping { value: Box<Expression> },
    /// `x`
    Identifier { symbol: SymbolU32 },
    /// `5 as i32`
    Cast {
        value: Box<Expression>,
        ty: Box<TypeExpression>,
    },
    /// `-{expr}`
    Unary {
        operator: UnaryOp,
        operand: Box<Expression>,
    },
    /// `{expr} + {expr}`
    Binary {
        left: Box<Expression>,
        operator: BinaryOp,
        operator_span: TextSpan,
        right: Box<Expression>,
    },
    /// `{expr}()`
    Call {
        callee: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    /// `{expr}::{expr}`
    Namespace {
        namespace: Box<TypeExpression>,
        member: Identifier,
    },
    /// `return {expr}`
    Return { value: Option<Box<Expression>> },
    /// `{ ... }`
    Block {
        statements: Box<[Statement]>,
        result: Option<Box<Expression>>,
    },
    /// `{identifier}: { ... }`
    Label {
        label: Identifier,
        block: Box<Expression>,
    },
    /// `break (:{label})? {expr}?`
    Break {
        label: Option<Identifier>,
        value: Option<Box<Expression>>,
    },
    /// `if {expr} { ... }`
    IfElse {
        condition: Box<Expression>,
        then_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
    /// `continue (:{label})?`
    Continue { label: Option<Identifier> },
    /// `loop { ... }`
    Loop { block: Box<Expression> },
    /// `unreachable`
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub enum TypeExprKind {
    /// `i32`
    Identifier(Identifier),
    /// `func(i32, i32) -> i32`
    Function {
        params: Box<[TypeExpression]>,
        result: Option<Box<TypeExpression>>,
    },
}

#[derive(Debug, Clone)]
pub struct TypeExpression {
    pub kind: TypeExprKind,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// `{expr};`
    DelimitedExpression { value: Box<Expression> },
    /// `local (mut)? {identifier}(: {type})? = {expr};`
    LocalDefinition {
        mutable: Option<Identifier>,
        name: Identifier,
        ty: Option<Box<TypeExpression>>,
        value: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StmtKind,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub mutable: Option<TextSpan>,
    pub name: Identifier,
    pub ty: TypeExpression,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub params: Box<[FunctionParam]>,
    pub result: Option<Box<TypeExpression>>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: Identifier,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    FunctionDefinition {
        signature: FunctionSignature,
        block: Box<Expression>,
    },
    EnumDefinition {
        name: Identifier,
        ty: TypeExpression,
        variants: Box<[EnumVariant]>,
    },
    ExportModifier {
        export: TextSpan,
        item: Box<Item>,
    },
    // FunctionDeclaration {
    //     signature: FunctionSignature,
    // },
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub span: TextSpan,
}

#[derive(Debug)]
pub struct Ast {
    pub file_id: FileId,
    pub items: Vec<Item>,
}

impl Ast {
    pub fn new(file_id: FileId) -> Self {
        Self {
            file_id,
            items: Vec::new(),
        }
    }
}
