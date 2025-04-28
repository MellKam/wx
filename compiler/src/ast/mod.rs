use diagnostics::TextSpan;
use lexer::TokenKind;
use string_interner::symbol::SymbolU32;

mod diagnostics;
mod lexer;
mod parser;
mod printer;
mod unescape;

pub use diagnostics::*;
pub use parser::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExprId(u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StmtId(u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ItemId(u32);

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
pub enum BindingType {
    Mutable,
    Const,
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

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::Eq => Ok(BinaryOperator::Assign),
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
    // Call {
    //     callee: ExprId,
    //     arguments: Vec<ExprId>,
    // },
    // Member {
    //     object: ExprId,
    //     property: SymbolU32,
    // },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: TextSpan,
    pub id: ExprId,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {
    Expression {
        expr: ExprId,
    },
    ConstDefinition {
        name: SymbolU32,
        ty: SymbolU32,
        value: ExprId,
    },
    MutableDefinition {
        name: SymbolU32,
        ty: SymbolU32,
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
    pub span: TextSpan,
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
    pub output: Option<SymbolU32>,
    pub span: TextSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    FunctionDefinition {
        signature: FunctionSignature,
        body: Vec<StmtId>,
    },
    FunctionDeclaration {
        signature: FunctionSignature,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub id: ItemId,
    pub kind: ItemKind,
    pub span: TextSpan,
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

    pub fn push_expr(&mut self, kind: ExprKind, span: TextSpan) -> ExprId {
        let id = ExprId(self.expressions.len() as u32);
        self.expressions.push(Expression { kind, span, id });
        return id;
    }

    pub fn get_expr(&self, id: ExprId) -> Option<&Expression> {
        self.expressions.get(id.0 as usize)
    }

    pub fn push_stmt(&mut self, kind: StmtKind, span: TextSpan) -> StmtId {
        let id = StmtId(self.statements.len() as u32);
        self.statements.push(Statement { id, kind, span });
        return id;
    }

    pub fn get_stmt(&self, id: StmtId) -> Option<&Statement> {
        self.statements.get(id.0 as usize)
    }

    pub fn push_item(&mut self, kind: ItemKind, span: TextSpan) -> ItemId {
        let id = ItemId(self.items.len() as u32);
        self.items.push(Item { id, kind, span });
        return id;
    }

    pub fn get_item(&self, id: ItemId) -> Option<&Item> {
        self.items.get(id.0 as usize)
    }
}
