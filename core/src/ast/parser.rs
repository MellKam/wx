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
    Comma,
    Assignment,
    Logical,
    Relational,
    Additive,
    Multiplicative,
    Unary,
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

        let (nud_handler, _) = match Parser::nud_lookup(token.kind) {
            Some(result) => result,
            None => return Err("nud handler not found".to_string()),
        };
        let mut left = nud_handler(self).unwrap();

        while let Some(Ok(token)) = self.lexer.peek() {
            let (led_handler, operator_binding_power) = match Parser::led_lookup(token.kind.clone())
            {
                Some((_, bp)) if bp < min_binding_power => break,
                Some((handler, bp)) => (handler, bp),
                None => break,
            };

            left = led_handler(self, left, operator_binding_power).unwrap();
        }

        Ok(left)
    }

    fn nud_lookup(token: Token) -> Option<(NudHandler, BindingPower)> {
        match token {
            Token::Int => Some((parse_int_expression, BindingPower::Primary)),
            Token::Float => Some((parse_float_expression, BindingPower::Primary)),
            Token::Identifier => Some((parse_identifier_expression, BindingPower::Primary)),
            Token::LeftParen => Some((parse_grouping_expression, BindingPower::Default)),
            Token::Minus | Token::Bang => Some((parse_unary_expression, BindingPower::Unary)),
            _ => None,
        }
    }

    fn led_lookup(token: Token) -> Option<(LedHandler, BindingPower)> {
        match token {
            Token::Plus | Token::Minus => Some((parse_binary_expression, BindingPower::Additive)),
            Token::Star | Token::Slash | Token::Percent => {
                Some((parse_binary_expression, BindingPower::Multiplicative))
            }
            Token::EqualEqual
            | Token::BangEqual
            | Token::LeftAngle
            | Token::LessEqual
            | Token::RightAngle
            | Token::GreaterEqual => Some((parse_binary_expression, BindingPower::Relational)),
            Token::VbarVbar | Token::AmperAmper => {
                Some((parse_binary_expression, BindingPower::Logical))
            }
            Token::Equal => Some((parse_assignment_expression, BindingPower::Assignment)),
            Token::LeftParen => Some((parse_call_expression, BindingPower::Call)),
            Token::Dot => Some((parse_member_expression, BindingPower::Member)),
            _ => None,
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

fn parse_unary_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let operator = parser.lexer.next().unwrap().unwrap();
    let operator_kind = match operator.kind {
        Token::Minus => UnaryOperationKind::Minus,
        Token::Bang => UnaryOperationKind::Not,
        _ => return Err("expected unary operator".to_string()),
    };

    let operand = parser.parse_expression(BindingPower::Primary)?;

    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Unary(UnaryExpression {
            operator: UnaryOperator {
                kind: operator_kind,
                span: operator.span.clone(),
            },
            operand,
        }));

    Ok(expression_id)
}

fn parse_binary_expression(
    parser: &mut Parser,
    left: ExpressionId,
    bp: BindingPower,
) -> Result<ExpressionId, String> {
    let operator = parser.lexer.next().unwrap().unwrap();
    let operator_kind = match operator.kind {
        Token::Plus => BinaryOperationKind::Plus,
        Token::Minus => BinaryOperationKind::Minus,
        Token::Star => BinaryOperationKind::Multiply,
        Token::Slash => BinaryOperationKind::Divide,
        Token::Percent => BinaryOperationKind::Modulo,
        Token::EqualEqual => BinaryOperationKind::Equals,
        Token::BangEqual => BinaryOperationKind::NotEquals,
        Token::LeftAngle => BinaryOperationKind::LessThan,
        Token::LessEqual => BinaryOperationKind::LessThanOrEqual,
        Token::RightAngle => BinaryOperationKind::GreaterThan,
        Token::GreaterEqual => BinaryOperationKind::GreaterThanOrEqual,
        _ => return Err("expected binary operator".to_string()),
    };

    let right = parser.parse_expression(bp)?;
    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Binary(BinaryExpression {
            left,
            right,
            operator: BinaryOperator {
                kind: operator_kind,
                span: operator.span.clone(),
            },
        }));
    Ok(expression_id)
}

fn parse_assignment_expression(
    parser: &mut Parser,
    left: ExpressionId,
    bp: BindingPower,
) -> Result<ExpressionId, String> {
    let operator = parser.lexer.next().unwrap().unwrap();
    if operator.kind != Token::Equal {
        return Err("expected assignment operator".to_string());
    }

    let right = parser.parse_expression(bp)?;
    let expression_id =
        parser
            .ast
            .add_expression(ExpressionKind::Assignment(AssignmentExpression {
                left,
                right,
            }));
    Ok(expression_id)
}

fn parse_call_expression(
    parser: &mut Parser,
    callee: ExpressionId,
    _: BindingPower,
) -> Result<ExpressionId, String> {
    let _ = parser.lexer.next();
    let mut arguments = Vec::new();
    loop {
        let token = parser.lexer.peek().clone().unwrap().clone().unwrap();
        if token.kind == Token::RightParen {
            let _ = parser.lexer.next();
            break;
        }

        let argument = parser.parse_expression(BindingPower::Primary)?;
        arguments.push(argument);

        let token = parser.lexer.peek().clone().unwrap().clone().unwrap();
        if token.kind == Token::Comma {
            let _ = parser.lexer.next();
        }
    }

    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Call(CallExpression { callee, arguments }));
    Ok(expression_id)
}

fn parse_member_expression(
    parser: &mut Parser,
    object: ExpressionId,
    _: BindingPower,
) -> Result<ExpressionId, String> {
    let _ = parser.lexer.next();
    let property = parser.parse_expression(BindingPower::Primary)?;
    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Member(MemberExpression {
            object,
            property,
        }));
    Ok(expression_id)
}
