use codespan_reporting::diagnostic::Diagnostic;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::lexer::{Lexer, PeekableLexer, Token, TokenKind};
use super::{
    Ast, BinaryOp, EnumVariant, ExprKind, FunctionParam, Identifier, ItemKind, Statement, StmtKind,
    UnaryOp,
};
use crate::ast::diagnostics::{
    ChainedComparisonsDiagnostic, IncompleteBinaryExpressionDiagnostic,
    InvalidIntegerLiteralDiagnostic, InvalidItemDiagnostic, InvalidNamespaceDiagnostic,
    MissingClosingParenDiagnostic, MissingLocalInitializerDiagnostic,
    MissingStatementDelimiterDiagnostic, MissingUnaryOperandDiagnostic,
    ReservedIdentifierDiagnostic, UnexpectedEofDiagnostic, UnexpectedTokenDiagnostic,
};
use crate::ast::{Expression, FunctionSignature, Item, TypeExprKind, TypeExpression};
use crate::files::FileId;
use crate::span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum BindingPower {
    Default,
    Assignment,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Comparison,
    BitwiseShift,
    Additive,
    Multiplicative,
    Unary,
    Cast,
    Call,
    Member,
    Primary,
}

impl From<BinaryOp> for BindingPower {
    fn from(operator: BinaryOp) -> Self {
        match operator {
            BinaryOp::Assign => BindingPower::Assignment,
            BinaryOp::AddAssign => BindingPower::Assignment,
            BinaryOp::SubAssign => BindingPower::Assignment,
            BinaryOp::MulAssign => BindingPower::Assignment,
            BinaryOp::DivAssign => BindingPower::Assignment,
            BinaryOp::RemAssign => BindingPower::Assignment,

            BinaryOp::Or => BindingPower::LogicalOr,
            BinaryOp::And => BindingPower::LogicalAnd,

            BinaryOp::BitOr => BindingPower::BitwiseOr,
            BinaryOp::BitXor => BindingPower::BitwiseXor,
            BinaryOp::BitAnd => BindingPower::BitwiseAnd,

            BinaryOp::Eq => BindingPower::Comparison,
            BinaryOp::NotEq => BindingPower::Comparison,
            BinaryOp::Less => BindingPower::Comparison,
            BinaryOp::LessEq => BindingPower::Comparison,
            BinaryOp::Greater => BindingPower::Comparison,
            BinaryOp::GreaterEq => BindingPower::Comparison,

            BinaryOp::LeftShift => BindingPower::BitwiseShift,
            BinaryOp::RightShift => BindingPower::BitwiseShift,

            BinaryOp::Add => BindingPower::Additive,
            BinaryOp::Sub => BindingPower::Additive,

            BinaryOp::Mul => BindingPower::Multiplicative,
            BinaryOp::Div => BindingPower::Multiplicative,
            BinaryOp::Rem => BindingPower::Multiplicative,
        }
    }
}

#[derive(Debug, Clone)]
enum Keyword {
    Export,
    Local,
    Mut,
    Enum,
    Func,
    Loop,
    Break,
    Continue,
    Return,
    If,
    Else,
    As,
    Unreachable,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(text: &str) -> Result<Self, Self::Error> {
        match text {
            "export" => Ok(Keyword::Export),
            "local" => Ok(Keyword::Local),
            "mut" => Ok(Keyword::Mut),
            "enum" => Ok(Keyword::Enum),
            "func" => Ok(Keyword::Func),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "loop" => Ok(Keyword::Loop),
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "return" => Ok(Keyword::Return),
            "as" => Ok(Keyword::As),
            "unreachable" => Ok(Keyword::Unreachable),
            _ => Err(()),
        }
    }
}

pub struct Parser<'input> {
    source: &'input str,
    lexer: PeekableLexer<'input>,
    interner: &'input mut StringInterner<StringBackend<SymbolU32>>,
    diagnostics: Vec<Diagnostic<FileId>>,
    ast: Ast,
}

impl<'input> Parser<'input> {
    fn next_expect(&mut self, expected_kind: TokenKind) -> Result<Token, ()> {
        let token = self.lexer.next();
        match TokenKind::discriminant_equals(token.kind, expected_kind) {
            true => Ok(token),
            false => {
                self.diagnostics.push(
                    UnexpectedTokenDiagnostic {
                        file_id: self.ast.file_id,
                        received: token,
                        expected_kind,
                    }
                    .report(),
                );
                Err(())
            }
        }
    }

    fn peek_expect(&mut self, expected_kind: TokenKind) -> Result<Token, ()> {
        let token = self.lexer.peek();
        match TokenKind::discriminant_equals(token.kind, expected_kind) {
            true => Ok(token),
            false => {
                self.diagnostics.push(
                    UnexpectedTokenDiagnostic {
                        file_id: self.ast.file_id,
                        received: token,
                        expected_kind,
                    }
                    .report(),
                );
                Err(())
            }
        }
    }

    pub fn parse(
        file_id: FileId,
        source: &'input str,
        interner: &'input mut StringInterner<StringBackend<SymbolU32>>,
    ) -> (Ast, Vec<Diagnostic<FileId>>) {
        let lexer: PeekableLexer<'input> = PeekableLexer::new(Lexer::new(source));

        let mut parser = Self {
            source,
            lexer,
            interner,
            diagnostics: Vec::new(),
            ast: Ast::new(file_id),
        };

        loop {
            let start_token = parser.lexer.peek();
            if start_token.kind == TokenKind::Eof {
                break;
            }

            let item_handler = match parser.get_item_handler(start_token.clone()) {
                Ok(handler) => handler,
                Err(_) => match parser.recover_from_invalid_item(start_token) {
                    Some(handler) => handler,
                    None => break,
                },
            };

            match item_handler(&mut parser) {
                Ok(item) => {
                    parser.ast.items.push(item);
                }
                Err(_) => continue,
            }
        }

        (parser.ast, parser.diagnostics)
    }

    fn get_item_handler(
        &mut self,
        token: Token,
    ) -> Result<fn(parser: &mut Parser) -> Result<Item, ()>, ()> {
        match self.resolve_keyword(token.clone()) {
            Some(Keyword::Func) => Ok(Parser::parse_function_definition),
            Some(Keyword::Enum) => Ok(Parser::parse_enum_item),
            Some(Keyword::Export) => Ok(Parser::parse_exported_function_definition),
            _ => return Err(()),
        }
    }

    fn recover_from_invalid_item(
        &mut self,
        start_token: Token,
    ) -> Option<fn(parser: &mut Parser) -> Result<Item, ()>> {
        let mut end_token = None;
        let handler = loop {
            let token = self.lexer.peek();
            if token.kind == TokenKind::Eof {
                break None;
            }

            match self.get_item_handler(token.clone()) {
                Ok(handler) => break Some(handler),
                Err(_) => {
                    end_token = Some(self.lexer.next());
                }
            }
        };

        self.diagnostics.push(
            InvalidItemDiagnostic {
                file_id: self.ast.file_id,
                span: match end_token {
                    Some(end_token) => TextSpan::merge(start_token.span, end_token.span),
                    None => start_token.span,
                },
            }
            .report(),
        );

        handler
    }

    fn parse_function_definition(parser: &mut Parser) -> Result<Item, ()> {
        let fn_keyword = parser.lexer.next();
        let name_token = parser.next_expect(TokenKind::Identifier)?;
        let name_symbol = parser.intern_identifier(name_token.span)?;
        let name = Identifier {
            symbol: name_symbol,
            span: name_token.span,
        };

        let params = Parser::parse_function_params(parser)?;

        let token = parser.lexer.peek();
        let result = match token.kind {
            TokenKind::Colon => {
                _ = parser.lexer.next();
                let ty = parser.parse_type_expr()?;

                Some(ty)
            }
            TokenKind::OpenBrace => None,
            _ => {
                parser.diagnostics.push(
                    UnexpectedTokenDiagnostic {
                        file_id: parser.ast.file_id,
                        received: token,
                        expected_kind: TokenKind::Colon,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        let block = Parser::parse_block_expression(parser)?;
        let span = TextSpan::merge(fn_keyword.span, block.span);
        Ok(Item {
            kind: ItemKind::FunctionDefinition {
                signature: FunctionSignature {
                    name,
                    params: params.into_boxed_slice(),
                    result: result.map(Box::new),
                },
                block: Box::new(block),
            },
            span,
        })
    }

    fn parse_function_params(parser: &mut Parser) -> Result<Vec<FunctionParam>, ()> {
        parser.next_expect(TokenKind::OpenParen)?;

        let mut params = Vec::new();

        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseParen => {
                    _ = parser.lexer.next();
                    break;
                }
                TokenKind::Identifier => {}
                TokenKind::Comma => {
                    let token = parser.lexer.next();
                    parser.diagnostics.push(
                        UnexpectedTokenDiagnostic {
                            file_id: parser.ast.file_id,
                            received: token,
                            expected_kind: TokenKind::Identifier,
                        }
                        .report(),
                    );
                    continue;
                }
                _ => {
                    parser.diagnostics.push(
                        UnexpectedTokenDiagnostic {
                            file_id: parser.ast.file_id,
                            received: token,
                            expected_kind: TokenKind::Identifier,
                        }
                        .report(),
                    );
                    return Err(());
                }
            }

            let mutable = match Keyword::try_from(token.span.text(parser.source)) {
                Ok(Keyword::Mut) => {
                    let mut_token = parser.lexer.next();
                    Some(mut_token.span)
                }
                _ => None,
            };

            let name_token = parser.next_expect(TokenKind::Identifier)?;
            match Keyword::try_from(name_token.span.text(parser.source)) {
                Ok(_) => {
                    parser.diagnostics.push(
                        ReservedIdentifierDiagnostic {
                            file_id: parser.ast.file_id,
                            span: name_token.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
                _ => {}
            }
            let name = Identifier {
                symbol: parser.intern_identifier(name_token.span)?,
                span: name_token.span,
            };
            parser.next_expect(TokenKind::Colon)?;
            let ty = parser.parse_type_expr()?;

            params.push(FunctionParam { mutable, name, ty });

            // TODO:

            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::Comma => {
                    _ = parser.lexer.next();
                }
                TokenKind::CloseParen => {
                    _ = parser.lexer.next();
                    break;
                }
                _ => {
                    parser.diagnostics.push(
                        UnexpectedTokenDiagnostic {
                            file_id: parser.ast.file_id,
                            expected_kind: TokenKind::CloseParen,
                            received: token,
                        }
                        .report(),
                    );
                    return Err(());
                }
            }
        }

        Ok(params)
    }

    fn parse_exported_function_definition(parser: &mut Parser) -> Result<Item, ()> {
        let export_keyword = parser.lexer.next();
        let func = Parser::parse_function_definition(parser)?;

        match &func.kind {
            ItemKind::FunctionDefinition { .. } => {
                let span = TextSpan::merge(export_keyword.span, func.span);
                return Ok(Item {
                    kind: ItemKind::ExportModifier {
                        export: export_keyword.span,
                        item: Box::new(func),
                    },
                    span,
                });
            }
            _ => unreachable!(),
        }
    }

    fn parse_enum_item(parser: &mut Parser) -> Result<Item, ()> {
        let enum_keyword = parser.lexer.next();

        let name_token = parser.next_expect(TokenKind::Identifier)?;
        let name_symbol = parser.intern_identifier(name_token.span)?;
        let name = Identifier {
            symbol: name_symbol,
            span: name_token.span,
        };

        _ = parser.next_expect(TokenKind::Colon)?;
        let ty = parser.parse_type_expr()?;

        _ = parser.next_expect(TokenKind::OpenBrace)?;
        let mut variants = Vec::new();
        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseBrace => break,
                TokenKind::Comma => {
                    let _ = parser.lexer.next();
                    continue;
                }
                TokenKind::Eof => {
                    parser.diagnostics.push(
                        UnexpectedEofDiagnostic {
                            file_id: parser.ast.file_id,
                            span: token.span,
                        }
                        .report(),
                    );

                    return Err(());
                }
                _ => {}
            }

            let variant_name_token = parser.next_expect(TokenKind::Identifier)?;
            let variant_name_symbol = parser.intern_identifier(variant_name_token.span)?;
            _ = parser.next_expect(TokenKind::Eq)?;
            let value_expr = parser.parse_expression(BindingPower::Default)?;

            variants.push(EnumVariant {
                name: Identifier {
                    symbol: variant_name_symbol,
                    span: variant_name_token.span,
                },
                value: Box::new(value_expr),
            });
        }

        let close_brace = parser.lexer.next();

        Ok(Item {
            kind: ItemKind::EnumDefinition {
                name,
                ty,
                variants: variants.into_boxed_slice(),
            },
            span: TextSpan::merge(enum_keyword.span, close_brace.span),
        })
    }

    fn parse_expression(&mut self, limit_bp: BindingPower) -> Result<Expression, ()> {
        let token = self.lexer.peek();
        let nud_handler = match self.nud_lookup(token.clone()) {
            Some((nud_handler, _)) => nud_handler,
            None => return Err(()),
        };
        let mut left = nud_handler(self)?;

        loop {
            let token = self.lexer.peek();

            let (led_handler, operator_bp) = match self.led_lookup(token) {
                Some((_, bp)) if bp <= limit_bp => break,
                Some((handler, bp)) => (handler, bp),
                None => break,
            };

            left = led_handler(self, left, operator_bp)?;
        }
        Ok(left)
    }

    fn nud_lookup(
        &mut self,
        token: Token,
    ) -> Option<(
        fn(parser: &mut Parser) -> Result<Expression, ()>,
        BindingPower,
    )> {
        match token.kind {
            TokenKind::Identifier => match Keyword::try_from(token.span.text(self.source)) {
                Ok(Keyword::Return) => {
                    Some((Parser::parse_return_expression, BindingPower::Primary))
                }
                Ok(Keyword::If) => Some((Parser::parse_if_else_expression, BindingPower::Primary)),
                Ok(Keyword::Loop) => Some((Parser::parse_loop_expression, BindingPower::Primary)),
                Ok(Keyword::Break) => Some((Parser::parse_break_expression, BindingPower::Primary)),
                Ok(Keyword::Continue) => {
                    Some((Parser::parse_continue_expression, BindingPower::Primary))
                }
                Ok(Keyword::Unreachable) => {
                    Some((Parser::parse_unreachable_expression, BindingPower::Primary))
                }
                _ => Some((Parser::parse_identifier_expression, BindingPower::Primary)),
            },
            TokenKind::Int => Some((Parser::parse_int_expression, BindingPower::Primary)),
            TokenKind::Float => Some((Parser::parse_float_expression, BindingPower::Primary)),
            TokenKind::OpenBrace => Some((Parser::parse_block_expression, BindingPower::Primary)),
            TokenKind::OpenParen => {
                Some((Parser::parse_grouping_expression, BindingPower::Default))
            }
            TokenKind::Minus | TokenKind::Bang | TokenKind::Caret => {
                Some((Parser::parse_unary_expression, BindingPower::Unary))
            }
            // TokenKind::String { .. } => Some((parse_string_expression, BindingPower::Primary)),
            // TokenKind::Char { .. } => Some((parse_string_expression, BindingPower::Primary)),
            _ => None,
        }
    }

    fn led_lookup(
        &mut self,
        token: Token,
    ) -> Option<(
        fn(parser: &mut Parser, left: Expression, bp: BindingPower) -> Result<Expression, ()>,
        BindingPower,
    )> {
        match token.kind {
            TokenKind::Plus | TokenKind::Minus => {
                Some((Parser::parse_binary_expression, BindingPower::Additive))
            }
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Some((
                Parser::parse_binary_expression,
                BindingPower::Multiplicative,
            )),
            TokenKind::Eq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::StarEq
            | TokenKind::PercentEq => {
                Some((Parser::parse_binary_expression, BindingPower::Assignment))
            }
            TokenKind::VbarVbar => Some((Parser::parse_binary_expression, BindingPower::LogicalOr)),
            TokenKind::AmperAmper => {
                Some((Parser::parse_binary_expression, BindingPower::LogicalAnd))
            }
            TokenKind::Vbar => Some((Parser::parse_binary_expression, BindingPower::BitwiseOr)),
            TokenKind::Caret => Some((Parser::parse_binary_expression, BindingPower::BitwiseXor)),
            TokenKind::Amper => Some((Parser::parse_binary_expression, BindingPower::BitwiseAnd)),
            TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::LessEq
            | TokenKind::Less
            | TokenKind::Greater
            | TokenKind::GreaterEq => {
                Some((Parser::parse_binary_expression, BindingPower::Comparison))
            }
            TokenKind::LeftShift | TokenKind::RightShift => {
                Some((Parser::parse_binary_expression, BindingPower::BitwiseShift))
            }
            TokenKind::OpenParen => Some((Parser::parse_call_expression, BindingPower::Call)),
            TokenKind::ColonColon => Some((
                Parser::parse_namespace_member_expression,
                BindingPower::Member,
            )),
            TokenKind::Identifier => match self.resolve_keyword(token) {
                Some(Keyword::As) => Some((Parser::parse_cast_expression, BindingPower::Cast)),
                _ => None,
            },
            TokenKind::Colon => Some((Parser::parse_labelled_expression, BindingPower::Primary)),
            // TokenKind::Dot => Some((parse_member_expression, BindingPower::Member)),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ()> {
        let name_token = self.next_expect(TokenKind::Identifier)?;
        let name_symbol = self.intern_identifier(name_token.span)?;

        Ok(Identifier {
            symbol: name_symbol,
            span: name_token.span,
        })
    }

    fn parse_identifier_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let token = parser.lexer.next();
        let symbol = parser.intern_identifier(token.span)?;

        Ok(Expression {
            kind: ExprKind::Identifier { symbol },
            span: token.span,
        })
    }

    fn parse_unreachable_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let unreachable_keyword = parser.lexer.next();
        Ok(Expression {
            kind: ExprKind::Unreachable,
            span: unreachable_keyword.span,
        })
    }

    fn parse_local_definition(parser: &mut Parser) -> Result<Statement, ()> {
        let local_keyword = parser.lexer.next();

        let mut_or_name_token = parser.next_expect(TokenKind::Identifier)?;
        let mut_or_name_text = match mut_or_name_token.kind {
            TokenKind::Identifier => mut_or_name_token.span.text(parser.source),
            _ => unreachable!(),
        };

        let (mutable, name) = match Keyword::try_from(mut_or_name_text) {
            Ok(Keyword::Mut) => {
                let mutable = Identifier {
                    symbol: parser.interner.get_or_intern(mut_or_name_text),
                    span: mut_or_name_token.span,
                };
                let name = parser.parse_identifier()?;

                (Some(mutable), name)
            }
            Ok(_) => panic!("can't use keyword as variable identifier"),
            Err(_) => {
                let name = Identifier {
                    symbol: parser.interner.get_or_intern(mut_or_name_text),
                    span: mut_or_name_token.span,
                };

                (None, name)
            }
        };

        let ty = match parser.lexer.peek().kind {
            TokenKind::Colon => {
                _ = parser.lexer.next();
                Some(parser.parse_type_expr()?)
            }
            _ => None,
        };

        let eq_token = parser.lexer.peek();
        match eq_token.kind {
            TokenKind::Eq => {
                _ = parser.lexer.next();
            }
            _ => {
                let token = parser.lexer.peek();
                parser.diagnostics.push(
                    MissingLocalInitializerDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
                return Err(());
            }
        }
        let value = match parser.parse_expression(BindingPower::Default) {
            Ok(value) => value,
            Err(_) => {
                let token = parser.lexer.peek();
                parser.diagnostics.push(
                    MissingLocalInitializerDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };
        let semicolon = parser.lexer.peek();
        let span = match semicolon.kind {
            TokenKind::SemiColon => {
                _ = parser.lexer.next();
                TextSpan::merge(local_keyword.span, semicolon.span)
            }
            _ => {
                parser.diagnostics.push(
                    MissingStatementDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        span: TextSpan::new(value.span.end().0, value.span.end().0),
                    }
                    .report(),
                );

                TextSpan::merge(local_keyword.span, value.span)
            }
        };
        Ok(Statement {
            kind: StmtKind::LocalDefinition {
                mutable,
                name,
                ty: ty.map(Box::new),
                value: Box::new(value),
            },
            span,
        })
    }

    fn parse_return_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let return_keyword = parser.lexer.next();
        match parser.parse_expression(BindingPower::Default) {
            Ok(expr) => {
                let span = TextSpan::merge(return_keyword.span, expr.span);
                Ok(Expression {
                    kind: ExprKind::Return {
                        value: Some(Box::new(expr)),
                    },
                    span,
                })
            }
            Err(_) => Ok(Expression {
                kind: ExprKind::Return { value: None },
                span: return_keyword.span,
            }),
        }
    }

    fn parse_grouping_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let open_paren = parser.lexer.next();
        let value = parser.parse_expression(BindingPower::Default)?;

        match parser.lexer.peek().kind {
            TokenKind::CloseParen => {
                let close_paren = parser.lexer.next();

                Ok(Expression {
                    kind: ExprKind::Grouping {
                        value: Box::new(value),
                    },
                    span: TextSpan::merge(open_paren.span, close_paren.span),
                })
            }
            _ => {
                parser.diagnostics.push(
                    MissingClosingParenDiagnostic {
                        file_id: parser.ast.file_id,
                        opening_paren: open_paren.span,
                        expr_span: value.span,
                    }
                    .report(),
                );

                let span = TextSpan::merge(open_paren.span, value.span);
                Ok(Expression {
                    kind: ExprKind::Grouping {
                        value: Box::new(value),
                    },
                    span,
                })
            }
        }
    }

    fn parse_cast_expression(
        parser: &mut Parser,
        value: Expression,
        _: BindingPower,
    ) -> Result<Expression, ()> {
        _ = parser.lexer.next();
        let ty = parser.parse_type_expr()?;

        let span = TextSpan::merge(value.span, ty.span);
        Ok(Expression {
            kind: ExprKind::Cast {
                value: Box::new(value),
                ty: Box::new(ty),
            },
            span,
        })
    }

    fn parse_unary_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let operator_token = parser.lexer.next();
        let operator = match UnaryOp::try_from(operator_token.kind) {
            Ok(operator) => operator,
            Err(_) => panic!("invalid unary operator"),
        };

        let operand = match parser.parse_expression(BindingPower::Unary) {
            Ok(operand) => operand,
            Err(_) => {
                parser.diagnostics.push(
                    MissingUnaryOperandDiagnostic {
                        file_id: parser.ast.file_id,
                        span: operator_token.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        let span = TextSpan::merge(operator_token.span, operand.span);
        Ok(Expression {
            kind: ExprKind::Unary {
                operator,
                operand: Box::new(operand),
            },
            span,
        })
    }

    fn parse_binary_expression(
        parser: &mut Parser,
        left: Expression,
        bp: BindingPower,
    ) -> Result<Expression, ()> {
        let operator_token = parser.lexer.next();
        let operator = match BinaryOp::try_from(operator_token.kind) {
            Ok(operator) => operator,
            Err(_) => unreachable!("invalid binary operator"),
        };

        let right = match parser.parse_expression(bp) {
            Ok(expr) => expr,
            Err(_) => {
                parser.diagnostics.push(
                    IncompleteBinaryExpressionDiagnostic {
                        file_id: parser.ast.file_id,
                        span: operator_token.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        if bp == BindingPower::Comparison {
            match left.kind {
                ExprKind::Binary {
                    operator,
                    operator_span,
                    ..
                } if BindingPower::from(operator) == BindingPower::Comparison => {
                    parser.diagnostics.push(
                        ChainedComparisonsDiagnostic {
                            file_id: parser.ast.file_id,
                            first_operator_span: operator_span,
                            second_operator_span: operator_token.span,
                        }
                        .report(),
                    );
                }
                _ => {}
            }
            match right.kind {
                ExprKind::Binary {
                    operator,
                    operator_span,
                    ..
                } if BindingPower::from(operator) == BindingPower::Comparison => {
                    parser.diagnostics.push(
                        ChainedComparisonsDiagnostic {
                            file_id: parser.ast.file_id,
                            first_operator_span: operator_token.span,
                            second_operator_span: operator_span,
                        }
                        .report(),
                    );
                }
                _ => {}
            }
        }

        let span = TextSpan::merge(left.span, right.span);
        Ok(Expression {
            kind: ExprKind::Binary {
                left: Box::new(left),
                right: Box::new(right),
                operator,
                operator_span: operator_token.span,
            },
            span,
        })
    }

    fn parse_int_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let token = parser.lexer.next();
        let value = match token.kind {
            TokenKind::Int => token.span.text(parser.source).parse::<i64>().ok(),
            _ => unreachable!(),
        };

        let value = match value {
            Some(value) => value,
            None => {
                parser.diagnostics.push(
                    InvalidIntegerLiteralDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );

                0
            }
        };

        Ok(Expression {
            kind: ExprKind::Int { value },
            span: token.span,
        })
    }

    fn parse_float_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let token = parser.lexer.next();
        let value = match token.kind {
            TokenKind::Float => token.span.text(parser.source).parse::<f64>().ok(),
            _ => unreachable!(),
        };

        let value = match value {
            Some(value) => value,
            None => {
                parser.diagnostics.push(
                    InvalidIntegerLiteralDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );

                0.0
            }
        };

        Ok(Expression {
            kind: ExprKind::Float { value },
            span: token.span,
        })
    }

    fn intern_identifier(&mut self, span: TextSpan) -> Result<SymbolU32, ()> {
        let text = span.text(self.source);

        match Keyword::try_from(text) {
            Ok(_) => {
                self.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: self.ast.file_id,
                        span,
                    }
                    .report(),
                );

                Err(())
            }
            Err(_) => Ok(self.interner.get_or_intern(text)),
        }
    }

    fn resolve_keyword(&mut self, token: Token) -> Option<Keyword> {
        match token.kind {
            TokenKind::Identifier => Keyword::try_from(token.span.text(self.source)).ok(),
            _ => None,
        }
    }

    fn parse_break_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let break_keyword = parser.lexer.next();
        let label = match parser.lexer.peek().kind {
            TokenKind::Colon => {
                let _ = parser.lexer.next();
                let label = parser.parse_identifier()?;

                Some(label)
            }
            _ => None,
        };
        let value = parser.parse_expression(BindingPower::Default).ok();

        let span = match (label.clone(), &value) {
            (_, Some(value)) => TextSpan::merge(break_keyword.span, value.span),
            (Some(label), None) => TextSpan::merge(break_keyword.span, label.span),
            (None, None) => break_keyword.span,
        };
        Ok(Expression {
            kind: ExprKind::Break {
                label,
                value: value.map(Box::new),
            },
            span,
        })
    }

    fn parse_continue_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let continue_keyword = parser.lexer.next();
        let label = match parser.lexer.peek().kind {
            TokenKind::Colon => {
                let _ = parser.lexer.next();
                let label = parser.parse_identifier()?;

                Some(label)
            }
            _ => None,
        };

        let span = match &label {
            Some(label) => TextSpan::merge(continue_keyword.span, label.span),
            None => continue_keyword.span,
        };
        Ok(Expression {
            kind: ExprKind::Continue { label },
            span,
        })
    }

    fn parse_labelled_expression(
        parser: &mut Parser,
        label_expr: Expression,
        _: BindingPower,
    ) -> Result<Expression, ()> {
        let label = match label_expr.kind {
            ExprKind::Identifier { symbol } => Identifier {
                symbol,
                span: label_expr.span,
            },
            expr => panic!("expected an identifier for label, got: {:#?}", expr),
        };
        _ = parser.next_expect(TokenKind::Colon)?;

        let block = parser.parse_expression(BindingPower::Default)?;
        match block.kind {
            ExprKind::Block { .. } => {}
            ExprKind::IfElse { .. } => {}
            ExprKind::Loop { .. } => {}
            _ => panic!("expected a block expression after label"),
        }

        let span = TextSpan::merge(label.span, block.span);
        Ok(Expression {
            kind: ExprKind::Label {
                label,
                block: Box::new(block),
            },
            span,
        })
    }

    fn parse_loop_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let loop_keyword = parser.lexer.next();
        let block = Parser::parse_block_expression(parser)?;

        let span = TextSpan::merge(loop_keyword.span, block.span);
        Ok(Expression {
            kind: ExprKind::Loop {
                block: Box::new(block),
            },
            span,
        })
    }

    fn skip_to_next_statement(&mut self) {
        loop {
            match self.lexer.peek().kind {
                TokenKind::SemiColon => {
                    let _ = self.lexer.next();
                    break;
                }
                TokenKind::CloseBrace | TokenKind::Eof => break,
                _ => {
                    let _ = self.lexer.next();
                    continue;
                }
            }
        }
    }

    fn parse_block_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let open_brace = parser.next_expect(TokenKind::OpenBrace)?;
        let mut statements = Vec::new();
        let mut result: Option<Box<Expression>> = None;

        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseBrace => break,
                TokenKind::Eof => {
                    parser.diagnostics.push(
                        UnexpectedTokenDiagnostic {
                            file_id: parser.ast.file_id,
                            received: token.clone(),
                            expected_kind: TokenKind::CloseBrace,
                        }
                        .report(),
                    );

                    return Ok(Expression {
                        kind: ExprKind::Block {
                            statements: statements.into_boxed_slice(),
                            result,
                        },
                        span: TextSpan::merge(open_brace.span, token.span),
                    });
                }
                _ => {}
            };

            match parser.resolve_keyword(token.clone()) {
                Some(Keyword::Local) => {
                    match Parser::parse_local_definition(parser) {
                        Ok(stmt) => statements.push(stmt),
                        Err(_) => {
                            parser.skip_to_next_statement();
                            continue;
                        }
                    };
                    continue;
                }
                // Some(_) => {
                //     parser.diagnostics.push(
                //         UnexpectedTokenDiagnostic {
                //             file_id: parser.ast.file_id,
                //             received: token.clone(),
                //             expected_kind: TokenKind::CloseBrace,
                //         }
                //         .report(),
                //     );

                //     let span = TextSpan::merge(
                //         open_brace.span,
                //         statements.last().map(|s| s.span).unwrap_or(open_brace.span),
                //     );
                //     return Ok(Expression {
                //         kind: ExprKind::Block {
                //             statements: statements.into_boxed_slice(),
                //             result: None,
                //         },
                //         span,
                //     });
                // }
                _ => {}
            };

            let value = match parser.parse_expression(BindingPower::Default) {
                Ok(value) => value,
                Err(_) => {
                    parser.skip_to_next_statement();
                    continue;
                }
            };

            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::SemiColon => {
                    let semicolon = parser.lexer.next();
                    let span = TextSpan::merge(value.span, semicolon.span);
                    statements.push(Statement {
                        kind: StmtKind::DelimitedExpression {
                            value: Box::new(value),
                        },
                        span,
                    });
                }
                TokenKind::CloseBrace => {
                    result = Some(Box::new(value));

                    let close_brace = parser.lexer.next();
                    return Ok(Expression {
                        kind: ExprKind::Block {
                            statements: statements.into_boxed_slice(),
                            result,
                        },
                        span: TextSpan::merge(open_brace.span, close_brace.span),
                    });
                }
                _ => {
                    parser.diagnostics.push(
                        MissingStatementDelimiterDiagnostic {
                            file_id: parser.ast.file_id,
                            span: TextSpan::new(value.span.end().0, value.span.end().0),
                        }
                        .report(),
                    );

                    let span = value.span;
                    statements.push(Statement {
                        kind: StmtKind::DelimitedExpression {
                            value: Box::new(value),
                        },
                        span,
                    });
                }
            }
        }

        let close_brace = parser.lexer.next();
        Ok(Expression {
            kind: ExprKind::Block {
                statements: statements.into_boxed_slice(),
                result,
            },
            span: TextSpan::merge(open_brace.span, close_brace.span),
        })
    }

    fn parse_call_expression(
        parser: &mut Parser,
        callee: Expression,
        _: BindingPower,
    ) -> Result<Expression, ()> {
        let _ = parser.lexer.next();
        let mut arguments = Vec::new();
        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseParen => break,
                TokenKind::Eof => {
                    parser.diagnostics.push(
                        UnexpectedEofDiagnostic {
                            file_id: parser.ast.file_id,
                            span: token.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
                _ => {}
            }

            let argument = match parser.parse_expression(BindingPower::Default) {
                Ok(argument) => argument,
                Err(_) => continue,
            };
            arguments.push(argument);

            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseParen => break,
                TokenKind::Comma => {
                    let _ = parser.lexer.next();
                    continue;
                }
                _ => {
                    parser.diagnostics.push(
                        UnexpectedTokenDiagnostic {
                            file_id: parser.ast.file_id,
                            received: token,
                            expected_kind: TokenKind::CloseParen,
                        }
                        .report(),
                    );
                    return Err(());
                }
            }
        }

        let close_paren = parser.lexer.next();
        let span = TextSpan::merge(callee.span, close_paren.span);

        Ok(Expression {
            kind: ExprKind::Call {
                callee: Box::new(callee),
                arguments: arguments.into_boxed_slice(),
            },
            span,
        })
    }

    fn parse_namespace_member_expression(
        parser: &mut Parser,
        namespace_expr: Expression,
        _: BindingPower,
    ) -> Result<Expression, ()> {
        let _double_colon = parser.lexer.next();
        let member = parser.parse_identifier()?;
        let namespace = match namespace_expr.kind {
            ExprKind::Identifier { symbol } => TypeExpression {
                kind: TypeExprKind::Identifier { symbol },
                span: namespace_expr.span,
            },
            _ => {
                parser.diagnostics.push(
                    InvalidNamespaceDiagnostic {
                        file_id: parser.ast.file_id,
                        span: namespace_expr.span,
                    }
                    .report(),
                );

                return Err(());
            }
        };

        let span = TextSpan::merge(namespace_expr.span, member.span);
        Ok(Expression {
            kind: ExprKind::Namespace {
                namespace: Box::new(namespace),
                member,
            },
            span,
        })
    }

    fn parse_if_else_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let if_keyword = parser.lexer.next();
        let condition = parser.parse_expression(BindingPower::Default)?;

        let then_block = Parser::parse_block_expression(parser)?;
        let maybe_else_token = parser.lexer.peek();
        match parser.resolve_keyword(maybe_else_token) {
            Some(Keyword::Else) => {
                let _ = parser.lexer.next();
                let else_block = Parser::parse_block_expression(parser)?;
                let span = TextSpan::merge(if_keyword.span, else_block.span);
                Ok(Expression {
                    kind: ExprKind::IfElse {
                        condition: Box::new(condition),
                        then_block: Box::new(then_block),
                        else_block: Some(Box::new(else_block)),
                    },
                    span,
                })
            }
            _ => {
                let span = TextSpan::merge(if_keyword.span, then_block.span);
                Ok(Expression {
                    kind: ExprKind::IfElse {
                        condition: Box::new(condition),
                        then_block: Box::new(then_block),
                        else_block: None,
                    },
                    span,
                })
            }
        }
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpression, ()> {
        let token = self.peek_expect(TokenKind::Identifier)?;

        match Keyword::try_from(token.span.text(self.source)) {
            Ok(Keyword::Func) => Parser::parse_function_type_expression(self),
            Ok(_) => panic!("invalid type"),
            Err(_) => Parser::parse_identifier_type_expression(self),
        }
    }

    fn parse_function_type_params(parser: &mut Parser) -> Result<Vec<TypeExpression>, ()> {
        _ = parser.next_expect(TokenKind::OpenParen)?;
        let mut params = Vec::new();

        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseParen => {
                    _ = parser.lexer.next();
                    break;
                }
                TokenKind::Eof => {
                    parser.diagnostics.push(
                        UnexpectedEofDiagnostic {
                            file_id: parser.ast.file_id,
                            span: token.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
                _ => {}
            }

            let param = parser.parse_type_expr()?;
            params.push(param);
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::Comma => {
                    _ = parser.lexer.next();
                }
                _ => {}
            }
        }

        Ok(params)
    }

    fn parse_function_type_expression(parser: &mut Parser) -> Result<TypeExpression, ()> {
        let func_keyword = parser.lexer.next();
        let params = Parser::parse_function_type_params(parser)?;
        let result = match parser.lexer.peek().kind {
            TokenKind::Arrow => {
                let _ = parser.lexer.next();
                Some(parser.parse_type_expr()?)
            }
            _ => None,
        };

        Ok(TypeExpression {
            kind: TypeExprKind::Function {
                params: params.into_boxed_slice(),
                result: result.map(Box::new),
            },
            span: func_keyword.span,
        })
    }

    fn parse_identifier_type_expression(parser: &mut Parser) -> Result<TypeExpression, ()> {
        let token = parser.next_expect(TokenKind::Identifier)?;
        let symbol = parser.intern_identifier(token.span)?;

        Ok(TypeExpression {
            kind: TypeExprKind::Identifier { symbol },
            span: token.span,
        })
    }
}

// fn parse_float_expression(parser: &mut Parser) -> Option<ExprId> {
//     let token = parser.lexer.next();
//     let content = token.span.get_content(parser.source)?;

//     let value = content.parse::<f64>().ok();

//     let expr = match value {
//         Some(value) => ExprKind::Float { value },
//         None => {
//             parser
//                 .diagnostics
//                 .borrow_mut()
//                 .diagnostics
//                 .push(Diagnostic {
//                     message: "Invalid float literal".to_string(),
//                     span: token.span.clone(),
//                     kind: DiagnosticKind::Error,
//                 });

//             ExprKind::Float { value: 0.0 }
//         }
//     };

//     let expr_id = parser.ast.add_expression(expr);
//     Some(expr_id)
// }

// fn parse_string_expression(parser: &mut Parser) -> Option<ExprId> {
//     let span = match parser.lexer.next() {
//         Token {
//             kind: TokenKind::String { terminated },
//             span,
//         } => {
//             if !terminated {
//                 parser
//                     .diagnostics
//                     .borrow_mut()
//                     .diagnostics
//                     .push(Diagnostic {
//                         message: "Unterminated string literal".to_string(),
//                         span,
//                         kind: DiagnosticKind::Error,
//                     });
//                 return None;
//             }

//             span
//         }
//         _ => unreachable!(),
//     };
//     let content = span.get_content(parser.source)?;

//     let unescaped = match unescape(content) {
//         Ok(str) => str,
//         Err(_) => {
//             parser
//                 .diagnostics
//                 .borrow_mut()
//                 .diagnostics
//                 .push(Diagnostic {
//                     message: "Invalid escaped string literal".to_string(),
//                     span: span.clone(),
//                     kind: DiagnosticKind::Error,
//                 });

//             content.to_string()
//         }
//     };
//     let symbol = parser.interner.get_or_intern(unescaped);

//     let expr_id = parser.ast.add_expression(ExprKind::String { symbol });
//     Some(expr_id)
// }

// tests

#[cfg(test)]
mod tests {
    use string_interner::StringInterner;

    use super::*;
    use crate::files::Files;

    #[test]
    fn should_recover_from_invalid_item() {
        let mut interner = StringInterner::new();

        let mut files = Files::new();
        let file_id = files
            .add("test.wx".to_string(), "foo func test() {}".to_string())
            .unwrap();

        let (ast, diagnostics) =
            Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics.get(0).unwrap().code,
            Some("invalid-item".to_string())
        );
        assert_eq!(ast.items.len(), 1);
    }

    #[test]
    fn should_recover_from_unclosed_function_block() {
        let mut interner = StringInterner::new();

        let mut files = Files::new();
        let file_id = files
            .add("test.wx".to_string(), "func test() { ".to_string())
            .unwrap();

        let (ast, diagnostics) =
            Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        assert_eq!(ast.items.len(), 1);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].code, Some("unexpected-token".to_string()));
    }

    #[test]
    fn should_recover_local_definition_from_missing_semicolon() {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add(
                "test.wx".to_string(),
                "func test() { local x = 5 }".to_string(),
            )
            .unwrap();
        let (ast, diagnostics) =
            Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        assert_eq!(ast.items.len(), 1);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].code,
            Some("missing-statement-delimiter".to_string())
        );
    }

    #[test]
    fn should_recover_delimited_expression_semicolon() {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add(
                "test.wx".to_string(),
                "func test() { hello() 5 }".to_string(),
            )
            .unwrap();
        let (ast, diagnostics) =
            Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].code,
            Some("missing-statement-delimiter".to_string())
        );
        assert_eq!(ast.items.len(), 1);
        match &ast.items[0].kind {
            ItemKind::FunctionDefinition { block, .. } => match &block.kind {
                ExprKind::Block { result, statements } => {
                    assert_eq!(statements.len(), 1);
                    assert_eq!(result.is_some(), true);
                }
                _ => {
                    panic!("expected a block expression");
                }
            },
            _ => panic!("expected a call expression"),
        };
    }

    #[test]
    fn should_report_local_definition_without_value() {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add(
                "test.wx".to_string(),
                "func test() { local x; }".to_string(),
            )
            .unwrap();
        let (ast, diagnostics) =
            Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        assert_eq!(ast.items.len(), 1);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].code, Some("missing-initializer".to_string()));
    }

    #[test]
    fn should_parse_float_expression() {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add(
                "test.wx".to_string(),
                "func test(): f32 { 3.14 }".to_string(),
            )
            .unwrap();
        let (ast, diagnostics) =
            Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        assert_eq!(diagnostics.len(), 0);
        assert_eq!(ast.items.len(), 1);
        match &ast.items[0].kind {
            ItemKind::FunctionDefinition { block, .. } => match &block.kind {
                ExprKind::Block { result, .. } => match result.clone().unwrap().kind {
                    ExprKind::Float { value } => {
                        assert_eq!(value, 3.14);
                    }
                    _ => panic!("expected a float expression"),
                },
                _ => panic!("expected a block expression"),
            },
            _ => panic!("expected a function definition"),
        };
    }
}
