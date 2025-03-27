use super::{
    diagnostics::{Diagnostic, DiagnosticKind, DiagnosticStoreCell},
    lexer::{Lexer, LexerError, LexerErrorKind, Token, TokenKind},
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

impl UnaryOperationKind {
    fn from_token(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Minus => Some(UnaryOperationKind::Minus),
            TokenKind::Bang => Some(UnaryOperationKind::Not),
            _ => None,
        }
    }
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

impl BinaryOperationKind {
    fn from_token(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(BinaryOperationKind::Plus),
            TokenKind::Minus => Some(BinaryOperationKind::Minus),
            TokenKind::Star => Some(BinaryOperationKind::Multiply),
            TokenKind::Slash => Some(BinaryOperationKind::Divide),
            TokenKind::Percent => Some(BinaryOperationKind::Modulo),
            TokenKind::EqualEqual => Some(BinaryOperationKind::Equals),
            TokenKind::BangEqual => Some(BinaryOperationKind::NotEquals),
            TokenKind::OpenAngle => Some(BinaryOperationKind::LessThan),
            TokenKind::LessEqual => Some(BinaryOperationKind::LessThanOrEqual),
            TokenKind::CloseAngle => Some(BinaryOperationKind::GreaterThan),
            TokenKind::GreaterEqual => Some(BinaryOperationKind::GreaterThanOrEqual),
            _ => None,
        }
    }
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
    pub property: SymbolU32,
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
pub struct Ast {
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

pub struct Parser<'a> {
    pub interner: StringInterner<StringBackend>,
    pub diagnostics: DiagnosticStoreCell,
    source: &'a str,
    lexer: Peekable<Lexer<'a>>,
    pub ast: Ast,
}

enum ParserError {
    UnexpectedEndOfInput,
    NudHandlerNotFound,
}

impl<'a> Parser<'a> {
    pub fn new(
        interner: StringInterner<StringBackend>,
        diagnostics: DiagnosticStoreCell,
        source: &'a str,
        lexer: Lexer<'a>,
    ) -> Self {
        Self {
            interner,
            diagnostics,
            source,
            lexer: lexer.peekable(),
            ast: Ast::new(),
        }
    }

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

    fn peek_token(&mut self) -> Option<Token> {
        loop {
            let result = match self.lexer.peek() {
                Some(result) => result,
                None => return None,
            };

            match result {
                Ok(token) => return Some(token.clone()),
                Err(error) => match error.kind {
                    LexerErrorKind::UnexpectedCharacter => {
                        self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
                            message: "unexpected character".to_string(),
                            span: error.span.clone(),
                            kind: DiagnosticKind::Error,
                        });
                        // just skip the token
                        _ = self.lexer.next();
                        continue;
                    }
                    LexerErrorKind::UnterminatedString => {
                        self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
                            message: "unterminated string".to_string(),
                            span: error.span.clone(),
                            kind: DiagnosticKind::Error,
                        });
                        // what to do here? is there a way to recover from this?
                        _ = self.lexer.next();
                        return None;
                    }
                },
            }
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, String> {
        let token = self.lexer.next().unwrap().unwrap();
        if token.kind != kind {
            self.diagnostics
                .borrow_mut()
                .reporn_unexpected_token(kind.clone(), token.clone());
            return Err(format!("expected token {:?}, found {:?}", kind, token.kind));
        }
        Ok(token)
    }

    pub fn parse_expression(
        &mut self,
        min_binding_power: BindingPower,
    ) -> Result<ExpressionId, String> {
        let token = self.peek_token().ok_or("unexpected end of input")?;
        let (nud_handler, _) = match Parser::nud_lookup(token.kind.clone()) {
            Some(result) => result,
            None => {
                return Err(format!("nud handler not found for token {}", token.kind));
            }
        };
        let mut left = nud_handler(self).unwrap();

        while let Some(token) = self.peek_token() {
            let (led_handler, operator_binding_power) = match Parser::led_lookup(token.kind) {
                Some((_, bp)) if bp < min_binding_power => break,
                Some((handler, bp)) => (handler, bp),
                None => break,
            };

            left = led_handler(self, left, operator_binding_power)?;
        }

        Ok(left)
    }

    fn nud_lookup(token: TokenKind) -> Option<(NudHandler, BindingPower)> {
        match token {
            TokenKind::Int => Some((parse_int_expression, BindingPower::Primary)),
            TokenKind::Float => Some((parse_float_expression, BindingPower::Primary)),
            TokenKind::Identifier => Some((parse_identifier_expression, BindingPower::Primary)),
            TokenKind::String => Some((parse_string_expression, BindingPower::Primary)),
            TokenKind::OpenParen => Some((parse_grouping_expression, BindingPower::Default)),
            TokenKind::Minus | TokenKind::Bang => {
                Some((parse_unary_expression, BindingPower::Unary))
            }
            _ => None,
        }
    }

    fn led_lookup(token: TokenKind) -> Option<(LedHandler, BindingPower)> {
        match token {
            TokenKind::Plus | TokenKind::Minus => {
                Some((parse_binary_expression, BindingPower::Additive))
            }
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
                Some((parse_binary_expression, BindingPower::Multiplicative))
            }
            TokenKind::EqualEqual
            | TokenKind::BangEqual
            | TokenKind::OpenAngle
            | TokenKind::LessEqual
            | TokenKind::CloseAngle
            | TokenKind::GreaterEqual => Some((parse_binary_expression, BindingPower::Relational)),
            TokenKind::VbarVbar | TokenKind::AmperAmper => {
                Some((parse_binary_expression, BindingPower::Logical))
            }
            TokenKind::Equal => Some((parse_assignment_expression, BindingPower::Assignment)),
            TokenKind::OpenParen => Some((parse_call_expression, BindingPower::Call)),
            TokenKind::Dot => Some((parse_member_expression, BindingPower::Member)),
            _ => None,
        }
    }
}

fn parse_int_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let token = parser.expect(TokenKind::Int)?;

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
    let token = parser.expect(TokenKind::Float)?;

    let value = match parser.source[token.span.clone()].parse::<f64>() {
        Ok(value) => value,
        Err(_) => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "Invalid float literal".to_string(),
                    span: token.span,
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
    let token = parser.expect(TokenKind::Identifier)?;

    let value = parser.interner.get_or_intern(&parser.source[token.span]);

    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Identifier(IdentifierExpression { value }));
    Ok(expression_id)
}

fn parse_string_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let token = parser.expect(TokenKind::String)?;

    let value = parser.interner.get_or_intern(&parser.source[token.span]);

    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::String(StringExpression { value }));
    Ok(expression_id)
}

fn parse_grouping_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let _ = parser.expect(TokenKind::OpenParen)?;
    let expression_id = parser.parse_expression(BindingPower::Default)?;
    let _ = parser.expect(TokenKind::CloseParen)?;

    Ok(expression_id)
}

fn parse_unary_expression(parser: &mut Parser) -> Result<ExpressionId, String> {
    let operator = parser.lexer.next().unwrap().unwrap();
    let operator_kind = match UnaryOperationKind::from_token(operator.kind) {
        Some(kind) => kind,
        None => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "invalid unary operator".to_string(),
                    span: operator.span.clone(),
                    kind: DiagnosticKind::Error,
                });
            return Err("invalid unary operator".to_string());
        }
    };

    let operand = parser.parse_expression(BindingPower::Unary)?;
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
    _bp: BindingPower,
) -> Result<ExpressionId, String> {
    let operator = parser.lexer.next().unwrap().unwrap();
    let operator_kind = match BinaryOperationKind::from_token(operator.kind.clone()) {
        Some(kind) => kind,
        None => {
            parser
                .diagnostics
                .borrow_mut()
                .report_invalid_unary_operator(operator);
            return Err("invalid binary operator".to_string());
        }
    };

    let right = parser.parse_expression(BindingPower::Default)?;
    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Binary(BinaryExpression {
            left,
            right,
            operator: BinaryOperator {
                kind: operator_kind,
                span: operator.span,
            },
        }));
    Ok(expression_id)
}

fn parse_assignment_expression(
    parser: &mut Parser,
    left: ExpressionId,
    bp: BindingPower,
) -> Result<ExpressionId, String> {
    let _ = parser.expect(TokenKind::Equal)?;

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
        let token = parser.peek_token().ok_or("unexpected end of input")?;
        if token.kind == TokenKind::CloseParen {
            let _ = parser.lexer.next();
            break;
        }

        let argument = parser.parse_expression(BindingPower::Comma)?;
        arguments.push(argument);

        let token = parser.peek_token().ok_or("unexpected end of input")?;
        if token.kind == TokenKind::Comma {
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
    let _ = parser.expect(TokenKind::Dot)?;
    let property = parser.expect(TokenKind::Identifier)?;
    let expression_id = parser
        .ast
        .add_expression(ExpressionKind::Member(MemberExpression {
            object,
            property: parser.interner.get_or_intern(&parser.source[property.span]),
        }));
    Ok(expression_id)
}
