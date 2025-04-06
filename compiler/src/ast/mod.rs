use lexer::TokenKind;
use string_interner::symbol::SymbolU32;

mod diagnostics;
mod lexer;
mod parser;
mod unescape;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprId(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct StmtId(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct ItemId(u32);

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    /// Sign inversion `-x`
    Invert,
    /// Logical negation `!x`
    Negate,
}

impl UnaryOperator {
    fn from_token(kind: lexer::TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Minus => Some(UnaryOperator::Invert),
            TokenKind::Bang => Some(UnaryOperator::Negate),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    fn from_token(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Eq => Some(BinaryOperator::Assign),
            TokenKind::Plus => Some(BinaryOperator::Add),
            TokenKind::Minus => Some(BinaryOperator::Subtract),
            TokenKind::Star => Some(BinaryOperator::Multiply),
            TokenKind::Slash => Some(BinaryOperator::Divide),
            TokenKind::Percent => Some(BinaryOperator::Remainder),
            TokenKind::EqEq => Some(BinaryOperator::Eq),
            TokenKind::NotEq => Some(BinaryOperator::NotEq),
            TokenKind::Less => Some(BinaryOperator::Less),
            TokenKind::LessEq => Some(BinaryOperator::LessEq),
            TokenKind::Greater => Some(BinaryOperator::Greater),
            TokenKind::GreaterEq => Some(BinaryOperator::GreaterEq),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int {
        value: i64,
    },
    Float {
        value: f64,
    },
    String {
        symbol: SymbolU32,
    },
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
    Assignment {
        left: ExprId,
        right: ExprId,
    },
    Call {
        callee: ExprId,
        arguments: Vec<ExprId>,
    },
    Member {
        object: ExprId,
        property: SymbolU32,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    id: ExprId,
    kind: ExprKind,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {
    Expression {
        expr: ExprId,
    },
    Block {
        statements: Vec<StmtId>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Function {
        name: SymbolU32,
        params: Vec<(SymbolU32, SymbolU32)>,
        body: StmtId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub id: ItemId,
    pub kind: ItemKind,
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    expressions: Vec<Expression>,
    statements: Vec<Statement>,
    items: Vec<Item>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            expressions: Vec::new(),
            statements: Vec::new(),
            items: Vec::new(),
        }
    }

    pub fn add_expression(&mut self, kind: ExprKind) -> ExprId {
        let id = ExprId(self.expressions.len() as u32);
        self.expressions.push(Expression {
            id: id.clone(),
            kind,
        });
        return id;
    }

    pub fn get_expression(&self, id: ExprId) -> Option<&Expression> {
        self.expressions.get(id.0 as usize)
    }

    pub fn add_statement(&mut self, kind: StmtKind) -> StmtId {
        let id = StmtId(self.statements.len() as u32);
        self.statements.push(Statement {
            id: id.clone(),
            kind,
        });
        return id;
    }

    pub fn get_statement(&self, id: StmtId) -> Option<&Statement> {
        self.statements.get(id.0 as usize)
    }

    pub fn add_item(&mut self, kind: ItemKind) -> ItemId {
        let id = ItemId(self.items.len() as u32);
        self.items.push(Item {
            id: id.clone(),
            kind,
        });
        return id;
    }

    pub fn get_item(&self, id: ItemId) -> Option<&Item> {
        self.items.get(id.0 as usize)
    }
}

#[cfg(test)]
mod tests {
    use string_interner::symbol::SymbolU32;

    use super::parser::Parser;

    #[test]
    fn basic() {
        let mut parser = Parser::new(
            "
        fn add(a: i32, b: i32) {
            return a + b;
        }

        fn main() {
            const x: i32 = 5;
            gweo y: i32 = 10;
            const result: i32 = add(x, y);
            println(result);
        }
        ",
        );
        let result = parser.parse();

        println!("{:?}", result);
        println!("{:#?}", parser.ast);
        println!("{:#?}", parser.diagnostics.borrow().diagnostics);
        // println!(
        //     "{:#?}",
        //     parser.interner.iter().collect::<Vec<(SymbolU32, &str)>>()
        // );
    }
}
