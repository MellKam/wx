use super::{
    diagnostics::{Diagnostic, DiagnosticKind, DiagnosticStoreCell},
    lexer::{Lexer, LexerError, Token, TokenWithSpan},
};
use std::{cell::Cell, iter::Peekable, ops::Range};
use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolU32};

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionId(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct StatementId(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct IntExpression {
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatExpression {
    pub value: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringExpression {
    pub value: SymbolU32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierExpression {
    pub value: SymbolU32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperationKind {
    Minus,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperator {
    pub kind: UnaryOperationKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: ExpressionId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperationKind {
    // Arithmetic
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    // Relational
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperator {
    pub kind: BinaryOperationKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: ExpressionId,
    pub right: ExpressionId,
    pub operator: BinaryOperator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub left: ExpressionId,
    pub right: ExpressionId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: ExpressionId,
    pub arguments: Vec<ExpressionId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpression {
    pub object: ExpressionId,
    pub property: ExpressionId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Int(IntExpression),
    Float(FloatExpression),
    String(StringExpression),
    Identifier(IdentifierExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Assignment(AssignmentExpression),
    Call(CallExpression),
    Member(MemberExpression),
}

#[derive(Debug, Clone, PartialEq)]
struct Expression {
    id: ExpressionId,
    kind: ExpressionKind,
}

#[derive(Debug, PartialEq)]
pub enum StatementKind {
    Expression(ExpressionId),
}

#[derive(Debug, PartialEq)]
pub struct Statement {
    id: StatementId,
    kind: StatementKind,
}

#[derive(Debug, PartialEq)]
struct Ast {
    expressions: Vec<Expression>,
    statements: Vec<Statement>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            expressions: Vec::new(),
            statements: Vec::new(),
        }
    }

    pub fn add_expression(&mut self, kind: ExpressionKind) -> ExpressionId {
        let id = ExpressionId(self.expressions.len());
        self.expressions.push(Expression {
            id: id.clone(),
            kind,
        });
        return id;
    }

    pub fn add_statement(&mut self, kind: StatementKind) -> StatementId {
        let id = StatementId(self.statements.len());
        self.statements.push(Statement {
            id: id.clone(),
            kind,
        });
        return id;
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum BindingPower {
    Default,
    Assignment,
    Logical,
    Relational,
    Additive,
    Multiplicative,
    Call,
    Member,
    Primary,
}

type NudHandler = fn(parser: &mut Parser) -> Result<ExpressionId, String>;
type LedHandler =
    fn(parser: &mut Parser, left: ExpressionId, bp: BindingPower) -> Result<ExpressionId, String>;

struct Parser<'a> {
    interner: StringInterner<StringBackend>,
    diagnostics: DiagnosticStoreCell,
    source: &'a str,
    lexer: Peekable<Lexer<'a>>,
    ast: Ast,
}

enum ParserError {
    UnexpectedEndOfInput,
    NudHandlerNotFound,
}

impl<'a> Parser<'a> {
    pub fn parse() -> Result<Ast, ()> {
        todo!()
    }

    // fn peek(&mut self) -> Option<TokenWithSpan> {
    //     loop {
    //         let result = match self.lexer.peek() {
    //             Some(result) => result,
    //             None => return None,
    //         };

    //         match result {
    //             Ok(token) => return Some(token.clone()),
    //             Err(error) => {
    //                 match error.kind {
    //                     LexerError::UnexpectedCharacter => {
    //                         self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
    //                             message: "unexpected character".to_string(),
    //                             span: error.span.clone(),
    //                             kind: DiagnosticKind::Error,
    //                         });
    //                     }
    //                     _ => {

    //                     }
    //                 }

    //                 self.lexer.next();
    //             }

    fn parse_expression(
        &mut self,
        min_binding_power: BindingPower,
    ) -> Result<ExpressionId, String> {
        let token = match self.lexer.peek() {
            Some(Ok(token)) => token.clone(),
            _ => return Err("enexpected end of input".to_string()),
        };

        let nud_handler = match Parser::nud_lookup(token.kind) {
            Some(handler) => handler,
            None => return Err("nud handler not found".to_string()),
        };
        let mut left = nud_handler(self).unwrap();

        while let Some(Ok(token)) = self.lexer.peek() {
            let led_handler = match Parser::led_lookup(token.kind.clone()) {
                Some(handler) => handler,
                None => break,
            };

            let operator_binding_power = Parser::bp_lookup(token.kind.clone());
            if operator_binding_power <= min_binding_power {
                break;
            }

            left = led_handler(self, left, operator_binding_power).unwrap();
        }

        Ok(left)
    }

    fn nud_lookup(token: Token) -> Option<NudHandler> {
        match token {
            Token::Int => Some(parse_int_expression),
            Token::Float => Some(parse_float_expression),
            Token::Identifier => Some(parse_identifier_expression),
            _ => None,
        }
    }

    fn led_lookup(token: Token) -> Option<LedHandler> {
        todo!()
    }

    fn bp_lookup(token: Token) -> BindingPower {
        match token {
            Token::Plus | Token::Minus => BindingPower::Additive,
            Token::Star | Token::Slash => BindingPower::Multiplicative,
            Token::EqualEqual
            | Token::BangEqual
            | Token::LeftAngle
            | Token::LessEqual
            | Token::RightAngle
            | Token::GreaterEqual => BindingPower::Relational,
            Token::Equal => BindingPower::Assignment,
            Token::AmperAmper | Token::VbarVbar => BindingPower::Logical,
            Token::Dot => BindingPower::Member,
            Token::LeftParen => BindingPower::Call,
            _ => BindingPower::Default,
        }
    }
}

fn parse_int_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let token = parser.lexer.next().unwrap().unwrap();
    if token.kind != Token::Int {
        return Err("expected integer".to_string());
    }

    let value = match parser.source[token.span.clone()].parse::<i64>() {
        Ok(value) => value,
        Err(_) => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "Invalid integer literal".to_string(),
                    span: token.span.clone(),
                    kind: DiagnosticKind::Error,
                });

            // continue parsing with a dummy value
            0
        }
    };

    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Int(IntExpression { value }));
    Ok(expression_id)
}

fn parse_float_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let token = parser.lexer.next().unwrap().unwrap();
    if token.kind != Token::Float {
        return Err("expected float".to_string());
    }

    let value = match parser.source[token.span.clone()].parse::<f64>() {
        Ok(value) => value,
        Err(_) => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "Invalid float literal".to_string(),
                    span: token.span.clone(),
                    kind: DiagnosticKind::Error,
                });

            // continue parsing with a dummy value
            0.0
        }
    };

    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Float(FloatExpression { value }));
    Ok(expression_id)
}

fn parse_identifier_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let token = parser.lexer.next().unwrap().unwrap();
    if token.kind != Token::Identifier {
        return Err("expected identifier".to_string());
    }

    let value = parser
        .interner
        .get_or_intern(&parser.source[token.span.clone()]);

    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Identifier(IdentifierExpression { value }));
    Ok(expression_id)
}

fn parse_grouping_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let left_paren = parser.lexer.next().unwrap().unwrap();
    let expression_id = parser.parse_expression(BindingPower::Primary)?;
    let right_paren = parser.lexer.next().unwrap().unwrap();
    if right_paren.kind != Token::RightParen {
        return Err("expected right parenthesis".to_string());
    }

    Ok(expression_id)
}
