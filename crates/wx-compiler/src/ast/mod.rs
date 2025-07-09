use lexer::TokenKind;
use string_interner::symbol::SymbolU32;

pub mod diagnostics;
mod lexer;
mod parser;

pub use parser::*;

use crate::files::FileId;
use crate::span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpKind {
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

impl BinOpKind {
    pub fn is_assignment(&self) -> bool {
        match self {
            BinOpKind::Assign
            | BinOpKind::AddAssign
            | BinOpKind::SubAssign
            | BinOpKind::MulAssign
            | BinOpKind::DivAssign
            | BinOpKind::RemAssign => true,
            _ => false,
        }
    }

    pub fn is_comparison(&self) -> bool {
        match self {
            BinOpKind::Eq
            | BinOpKind::NotEq
            | BinOpKind::Less
            | BinOpKind::LessEq
            | BinOpKind::Greater
            | BinOpKind::GreaterEq => true,
            _ => false,
        }
    }

    pub fn is_logical(&self) -> bool {
        match self {
            BinOpKind::And | BinOpKind::Or => true,
            _ => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match self {
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Rem => {
                true
            }
            _ => false,
        }
    }

    pub fn is_bitwise(&self) -> bool {
        match self {
            BinOpKind::BitAnd
            | BinOpKind::BitOr
            | BinOpKind::BitXor
            | BinOpKind::LeftShift
            | BinOpKind::RightShift => true,
            _ => false,
        }
    }
}

impl TryFrom<TokenKind> for BinOpKind {
    type Error = ();

    fn try_from(tag: TokenKind) -> Result<Self, Self::Error> {
        match tag {
            // Arithmetic
            TokenKind::Plus => Ok(BinOpKind::Add),
            TokenKind::Minus => Ok(BinOpKind::Sub),
            TokenKind::Star => Ok(BinOpKind::Mul),
            TokenKind::Slash => Ok(BinOpKind::Div),
            TokenKind::Percent => Ok(BinOpKind::Rem),
            // Relational
            TokenKind::EqEq => Ok(BinOpKind::Eq),
            TokenKind::BangEq => Ok(BinOpKind::NotEq),
            TokenKind::Less => Ok(BinOpKind::Less),
            TokenKind::LessEq => Ok(BinOpKind::LessEq),
            TokenKind::Greater => Ok(BinOpKind::Greater),
            TokenKind::GreaterEq => Ok(BinOpKind::GreaterEq),
            // Logical
            TokenKind::AmperAmper => Ok(BinOpKind::And),
            TokenKind::VbarVbar => Ok(BinOpKind::Or),
            // Assignment
            TokenKind::Eq => Ok(BinOpKind::Assign),
            TokenKind::PlusEq => Ok(BinOpKind::AddAssign),
            TokenKind::MinusEq => Ok(BinOpKind::SubAssign),
            TokenKind::StarEq => Ok(BinOpKind::MulAssign),
            TokenKind::SlashEq => Ok(BinOpKind::DivAssign),
            TokenKind::PercentEq => Ok(BinOpKind::RemAssign),
            // Bitwise
            TokenKind::Amper => Ok(BinOpKind::BitAnd),
            TokenKind::Vbar => Ok(BinOpKind::BitOr),
            TokenKind::Caret => Ok(BinOpKind::BitXor),
            TokenKind::LeftShift => Ok(BinOpKind::LeftShift),
            TokenKind::RightShift => Ok(BinOpKind::RightShift),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOpKind {
    InvertSign,
    Not,
    BitNot,
}

impl TryFrom<TokenKind> for UnOpKind {
    type Error = ();

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::Minus => Ok(UnOpKind::InvertSign),
            TokenKind::Bang => Ok(UnOpKind::Not),
            TokenKind::Caret => Ok(UnOpKind::BitNot),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Rem => "%",
            BinOpKind::Eq => "==",
            BinOpKind::NotEq => "!=",
            BinOpKind::Less => "<",
            BinOpKind::LessEq => "<=",
            BinOpKind::Greater => ">",
            BinOpKind::GreaterEq => ">=",
            BinOpKind::And => "&&",
            BinOpKind::Or => "||",
            BinOpKind::Assign => "=",
            BinOpKind::AddAssign => "+=",
            BinOpKind::SubAssign => "-=",
            BinOpKind::MulAssign => "*=",
            BinOpKind::DivAssign => "/=",
            BinOpKind::RemAssign => "%=",
            BinOpKind::BitAnd => "&",
            BinOpKind::BitOr => "|",
            BinOpKind::BitXor => "^",
            BinOpKind::LeftShift => "<<",
            BinOpKind::RightShift => ">>",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub symbol: SymbolU32,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub kind: BinOpKind,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub kind: UnOpKind,
    pub span: TextSpan,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// `1`
    Int { value: i64 },
    /// `1.0`
    Float { value: f64 },
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
    Identifier { symbol: SymbolU32 },
    /// `func(i32, i32) -> i32`
    Function {
        params: Box<[TypeExpression]>,
        result: Box<TypeExpression>,
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
    pub result: Box<TypeExpression>,
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
    GlobalDefinition {
        name: Identifier,
        ty: TypeExpression,
        mutable: Option<TextSpan>,
        value: Box<Expression>,
    },
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
