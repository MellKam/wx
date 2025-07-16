use codespan_reporting::diagnostic::Diagnostic;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::lexer::{Lexer, PeekableLexer, Token, TokenKind};
use super::*;
use crate::ast::diagnostics::*;
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

impl From<BinOpKind> for BindingPower {
    fn from(operator: BinOpKind) -> Self {
        match operator {
            BinOpKind::Assign => BindingPower::Assignment,
            BinOpKind::AddAssign => BindingPower::Assignment,
            BinOpKind::SubAssign => BindingPower::Assignment,
            BinOpKind::MulAssign => BindingPower::Assignment,
            BinOpKind::DivAssign => BindingPower::Assignment,
            BinOpKind::RemAssign => BindingPower::Assignment,

            BinOpKind::Or => BindingPower::LogicalOr,
            BinOpKind::And => BindingPower::LogicalAnd,

            BinOpKind::BitOr => BindingPower::BitwiseOr,
            BinOpKind::BitXor => BindingPower::BitwiseXor,
            BinOpKind::BitAnd => BindingPower::BitwiseAnd,

            BinOpKind::Eq => BindingPower::Comparison,
            BinOpKind::NotEq => BindingPower::Comparison,
            BinOpKind::Less => BindingPower::Comparison,
            BinOpKind::LessEq => BindingPower::Comparison,
            BinOpKind::Greater => BindingPower::Comparison,
            BinOpKind::GreaterEq => BindingPower::Comparison,

            BinOpKind::LeftShift => BindingPower::BitwiseShift,
            BinOpKind::RightShift => BindingPower::BitwiseShift,

            BinOpKind::Add => BindingPower::Additive,
            BinOpKind::Sub => BindingPower::Additive,

            BinOpKind::Mul => BindingPower::Multiplicative,
            BinOpKind::Div => BindingPower::Multiplicative,
            BinOpKind::Rem => BindingPower::Multiplicative,
        }
    }
}

#[derive(Debug, Clone)]
enum Keyword {
    Export,
    Local,
    Global,
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
            "global" => Ok(Keyword::Global),
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

#[derive(Debug, Serialize)]
pub struct ParserResult {
    pub ast: Ast,
    pub diagnostics: Vec<Diagnostic<FileId>>,
}

impl<'source> Parser<'source> {
    pub fn parse(
        file_id: FileId,
        source: &'source str,
        interner: &'source mut StringInterner<StringBackend<SymbolU32>>,
    ) -> ParserResult {
        let lexer: PeekableLexer<'source> = PeekableLexer::new(Lexer::new(source));

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

        ParserResult {
            ast: parser.ast,
            diagnostics: parser.diagnostics,
        }
    }

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

    fn get_item_handler(
        &mut self,
        token: Token,
    ) -> Result<fn(parser: &mut Parser) -> Result<Item, ()>, ()> {
        match self.resolve_keyword(token.clone()) {
            Some(Keyword::Func) => Ok(Parser::parse_function_definition),
            Some(Keyword::Global) => Ok(Parser::parse_global_definition),
            Some(Keyword::Enum) => Ok(Parser::parse_enum_item),
            Some(Keyword::Export) => Ok(Parser::parse_exported_item),
            _ => return Err(()),
        }
    }

    fn parse_exported_item(parser: &mut Parser) -> Result<Item, ()> {
        let export_keyword = parser.lexer.next();
        let next_token = parser.lexer.peek();
        let item = match parser.resolve_keyword(next_token) {
            Some(Keyword::Func) => Parser::parse_function_definition(parser)?,
            Some(Keyword::Global) => Parser::parse_global_definition(parser)?,
            Some(Keyword::Enum) => Parser::parse_enum_item(parser)?,
            _ => {
                parser.diagnostics.push(
                    InvalidItemDiagnostic {
                        file_id: parser.ast.file_id,
                        span: export_keyword.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        let span = TextSpan::merge(export_keyword.span, item.span);
        Ok(Item {
            kind: ItemKind::ExportModifier {
                export: export_keyword.span,
                item: Box::new(item),
            },
            span,
        })
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

        let params = Parser::parse_separated_group(
            parser,
            TokenKind::OpenParen,
            TokenKind::CloseParen,
            TokenKind::Comma,
            |parser: &mut Parser| {
                let token = parser.lexer.peek();
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
                    }
                    _ => {}
                }
                let name = Identifier {
                    symbol: parser.intern_identifier(name_token.span)?,
                    span: name_token.span,
                };
                let type_annotation = parser
                    .lexer
                    .next_if(TokenKind::Colon)
                    .ok_or(())
                    .and_then(|colon| {
                        let ty = parser.parse_type_expr()?;
                        Ok(TypeAnnotation {
                            separator: colon.span,
                            ty: Box::new(ty),
                        })
                    })
                    .unwrap_or_else(|_| {
                        let name_end = name_token.span.end_position();
                        parser.diagnostics.push(
                            MissingParamTypeAnnotationDiagnostic {
                                file_id: parser.ast.file_id,
                                span: name_end,
                            }
                            .report(),
                        );

                        TypeAnnotation {
                            separator: name_end,
                            ty: Box::new(TypeExpression {
                                kind: TypeExprKind::Error,
                                span: name_end,
                            }),
                        }
                    });

                let span = TextSpan::merge(name_token.span, type_annotation.ty.span);
                Ok((
                    FunctionParam {
                        mutable,
                        name,
                        annotation: type_annotation,
                    },
                    span,
                ))
            },
        )?;

        let result = parser
            .lexer
            .next_if(TokenKind::Colon)
            .ok_or(())
            .and_then(|colon| {
                let ty = parser.parse_type_expr()?;
                Ok(TypeAnnotation {
                    separator: colon.span,
                    ty: Box::new(ty),
                })
            })
            .unwrap_or_else(|_| {
                let params_end = params.close.end_position();
                parser.diagnostics.push(
                    MissingReturnTypeDiagnostic {
                        file_id: parser.ast.file_id,
                        span: params_end,
                    }
                    .report(),
                );

                TypeAnnotation {
                    separator: params_end,
                    ty: Box::new(TypeExpression {
                        kind: TypeExprKind::Error,
                        span: params_end,
                    }),
                }
            });

        let block = Parser::parse_block_expression(parser)?;
        let span = TextSpan::merge(fn_keyword.span, block.span);
        Ok(Item {
            kind: ItemKind::FunctionDefinition {
                signature: FunctionSignature {
                    name,
                    params,
                    result,
                },
                block: Box::new(block),
            },
            span,
        })
    }

    fn parse_separated_group<T>(
        parser: &mut Parser,
        open_token: TokenKind,
        close_token: TokenKind,
        separator_token: TokenKind,
        handler: fn(parser: &mut Parser) -> Result<(T, TextSpan), ()>,
    ) -> Result<Grouped<Box<[Separated<T>]>>, ()> {
        let open = parser.next_expect(open_token)?.span;
        let mut elements: Vec<Separated<T>> = Vec::new();
        let close = 'parse: loop {
            let token = parser.lexer.peek();
            if token.kind == TokenKind::Eof {
                let expected_close_span = match elements.last() {
                    Some(last) => last.span.end_position(),
                    None => open.end_position(),
                };
                parser.diagnostics.push(
                    UnclosedGroupingDiagnotic {
                        file_id: parser.ast.file_id,
                        close_token,
                        open_span: open,
                        expected_close_span,
                    }
                    .report(),
                );
                break expected_close_span;
            }
            if token.kind == close_token {
                break parser.lexer.next().span;
            }

            let (element, element_span) = loop {
                match handler(parser) {
                    Ok(element) => break element,
                    Err(_) => 'recover: loop {
                        let token = parser.lexer.peek();
                        match token.kind {
                            kind if kind == separator_token => {
                                _ = parser.lexer.next();
                                continue 'parse;
                            }
                            kind if kind == close_token || kind == TokenKind::Eof => {
                                continue 'parse;
                            }
                            _ => {
                                _ = parser.lexer.next();
                                continue 'recover;
                            }
                        }
                    },
                }
            };

            let token = parser.lexer.peek();
            if token.kind == separator_token {
                let delimiter = parser.lexer.next();
                elements.push(Separated {
                    inner: element,
                    span: element_span,
                    separator: Some(delimiter.span),
                });
                continue;
            }
            if token.kind == close_token {
                let close = parser.lexer.next();
                elements.push(Separated {
                    inner: element,
                    span: element_span,
                    separator: None,
                });
                break close.span;
            }

            elements.push(Separated {
                inner: element,
                span: element_span,
                separator: None,
            });

            if token.kind == TokenKind::Eof {
                let span = TextSpan::new(token.span.start().0, token.span.end().0);
                parser.diagnostics.push(
                    UnclosedGroupingDiagnotic {
                        file_id: parser.ast.file_id,
                        close_token,
                        open_span: open,
                        expected_close_span: span,
                    }
                    .report(),
                );

                break span;
            }

            parser.diagnostics.push(
                MissingSeparatorDiagnostic {
                    file_id: parser.ast.file_id,
                    span: element_span.end_position(),
                    delimiter: separator_token,
                }
                .report(),
            );
        };

        Ok(Grouped {
            open,
            inner: elements.into_boxed_slice(),
            close,
        })
    }

    fn parse_enum_item(parser: &mut Parser) -> Result<Item, ()> {
        let enum_keyword = parser.lexer.next();

        let name_token = parser.next_expect(TokenKind::Identifier)?;
        let name_symbol = parser.intern_identifier(name_token.span)?;
        let name = Identifier {
            symbol: name_symbol,
            span: name_token.span,
        };

        let ty = parser
            .lexer
            .next_if(TokenKind::Colon)
            .ok_or(())
            .and_then(|colon| {
                let ty = parser.parse_type_expr()?;
                Ok(TypeAnnotation {
                    separator: colon.span,
                    ty: Box::new(ty),
                })
            })
            .unwrap_or_else(|_| {
                let name_end = name.span.end_position();
                parser.diagnostics.push(
                    MissingEnumTypeAnnotationDiagnostic {
                        file_id: parser.ast.file_id,
                        span: name_end,
                    }
                    .report(),
                );

                TypeAnnotation {
                    separator: name_end,
                    ty: Box::new(TypeExpression {
                        kind: TypeExprKind::Error,
                        span: name_end,
                    }),
                }
            });

        let variants = Parser::parse_separated_group(
            parser,
            TokenKind::OpenBrace,
            TokenKind::CloseBrace,
            TokenKind::Comma,
            |parser| {
                let variant_name_token = parser.next_expect(TokenKind::Identifier)?;
                let variant_name_symbol = parser.intern_identifier(variant_name_token.span)?;
                _ = parser.next_expect(TokenKind::Eq)?;
                let value_expr = parser.parse_expression(BindingPower::Default)?;

                let span = TextSpan::merge(variant_name_token.span, value_expr.span);
                Ok((
                    EnumVariant {
                        name: Identifier {
                            symbol: variant_name_symbol,
                            span: variant_name_token.span,
                        },
                        value: Box::new(value_expr),
                    },
                    span,
                ))
            },
        )?;

        let span = TextSpan::merge(enum_keyword.span, variants.close);
        Ok(Item {
            kind: ItemKind::EnumDefinition {
                name,
                annotation: ty,
                variants,
            },
            span,
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

    fn parse_local_definition(parser: &mut Parser) -> Result<(StmtKind, TextSpan), ()> {
        let local_keyword = parser.lexer.next();

        let token = parser.lexer.peek();
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
            }
            _ => {}
        }
        let name = Identifier {
            symbol: parser.intern_identifier(name_token.span)?,
            span: name_token.span,
        };
        let type_annotation = parser
            .lexer
            .next_if(TokenKind::Colon)
            .ok_or(())
            .and_then(|colon| {
                let ty = parser.parse_type_expr()?;
                Ok(TypeAnnotation {
                    separator: colon.span,
                    ty: Box::new(ty),
                })
            })
            .ok(); // todo: recover from invalid type

        let (eq, value) = parser
            .lexer
            .next_if(TokenKind::Eq)
            .ok_or(())
            .and_then(|eq_token| {
                let expr = parser.parse_expression(BindingPower::Default)?;
                Ok((eq_token.span, expr))
            })
            .map_err(|_| {
                let token = parser.lexer.peek();
                parser.diagnostics.push(
                    MissingLocalInitializerDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
            })?;

        let span = TextSpan::merge(local_keyword.span, value.span);
        Ok((
            StmtKind::LocalDefinition {
                mutable,
                name,
                annotation: type_annotation,
                eq,
                value: Box::new(value),
            },
            span,
        ))
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
        let open = parser.lexer.next().span;
        let value = parser.parse_expression(BindingPower::Default)?;

        match parser.lexer.peek().kind {
            TokenKind::CloseParen => {
                let close = parser.lexer.next().span;

                Ok(Expression {
                    kind: ExprKind::Grouping {
                        value: Grouped {
                            open,
                            inner: Box::new(value),
                            close,
                        },
                    },
                    span: TextSpan::merge(open, close),
                })
            }
            _ => {
                parser.diagnostics.push(
                    UnclosedGroupingDiagnotic {
                        file_id: parser.ast.file_id,
                        open_span: open,
                        close_token: TokenKind::CloseParen,
                        expected_close_span: value.span.end_position(),
                    }
                    .report(),
                );

                let span = TextSpan::merge(open, value.span);
                Ok(Expression {
                    kind: ExprKind::Grouping {
                        value: Grouped {
                            open,
                            inner: Box::new(value),
                            close: open.end_position(),
                        },
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
        let operator = match UnOpKind::try_from(operator_token.kind) {
            Ok(operator) => operator,
            Err(_) => unreachable!(),
        };

        let operand = match parser.parse_expression(BindingPower::Unary) {
            Ok(operand) => operand,
            Err(_) => {
                parser.diagnostics.push(
                    IncompleteUnaryExpressionDiagnostic {
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
                operator: UnaryOp {
                    kind: operator,
                    span: operator_token.span,
                },
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
        let operator = match BinOpKind::try_from(operator_token.kind) {
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
            match &left.kind {
                ExprKind::Binary { operator, .. }
                    if BindingPower::from(operator.kind) == BindingPower::Comparison =>
                {
                    parser.diagnostics.push(
                        ChainedComparisonsDiagnostic {
                            file_id: parser.ast.file_id,
                            first_operator_span: operator.span,
                            second_operator_span: operator_token.span,
                        }
                        .report(),
                    );
                }
                _ => {}
            }
            match &right.kind {
                ExprKind::Binary { operator, .. }
                    if BindingPower::from(operator.kind) == BindingPower::Comparison =>
                {
                    parser.diagnostics.push(
                        ChainedComparisonsDiagnostic {
                            file_id: parser.ast.file_id,
                            first_operator_span: operator_token.span,
                            second_operator_span: operator.span,
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
                operator: BinaryOp {
                    kind: operator,
                    span: operator_token.span,
                },
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

    fn parse_block_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let block = Parser::parse_separated_group(
            parser,
            TokenKind::OpenBrace,
            TokenKind::CloseBrace,
            TokenKind::SemiColon,
            |parser| {
                let token = parser.lexer.peek();
                match parser.resolve_keyword(token.clone()) {
                    Some(Keyword::Local) => Parser::parse_local_definition(parser),
                    _ => {
                        let expr = parser.parse_expression(BindingPower::Default)?;
                        let span = expr.span;
                        Ok((
                            StmtKind::Expression {
                                expr: Box::new(expr),
                            },
                            span,
                        ))
                    }
                }
            },
        )?;

        let span = TextSpan::merge(block.open, block.close);
        Ok(Expression {
            kind: ExprKind::Block(block),
            span,
        })
    }

    fn parse_call_expression(
        parser: &mut Parser,
        callee: Expression,
        _: BindingPower,
    ) -> Result<Expression, ()> {
        let arguments = Parser::parse_separated_group(
            parser,
            TokenKind::OpenParen,
            TokenKind::CloseParen,
            TokenKind::Comma,
            |parser| {
                parser.parse_expression(BindingPower::Default).map(|expr| {
                    let span = expr.span;
                    (expr, span)
                })
            },
        )?;

        let span = TextSpan::merge(callee.span, arguments.close);
        Ok(Expression {
            kind: ExprKind::Call {
                callee: Box::new(callee),
                arguments,
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
        match token.kind {
            TokenKind::Identifier => {}
            _ => panic!("invalid type"),
        }

        match Keyword::try_from(token.span.text(self.source)) {
            Ok(Keyword::Func) => Parser::parse_function_type_expression(self),
            Ok(_) => panic!("invalid type"),
            Err(_) => Parser::parse_identifier_type_expression(self),
        }
    }

    fn parse_function_type_expression(parser: &mut Parser) -> Result<TypeExpression, ()> {
        let func_keyword = parser.lexer.next();
        let params = Parser::parse_separated_group(
            parser,
            TokenKind::OpenParen,
            TokenKind::CloseParen,
            TokenKind::Comma,
            |parser| {
                parser.parse_type_expr().map(|ty| {
                    let span = ty.span;
                    (ty, span)
                })
            },
        )?;
        let arrow = parser.next_expect(TokenKind::Arrow)?.span;
        let result = parser.parse_type_expr()?;
        let result = TypeAnnotation {
            separator: arrow,
            ty: Box::new(result),
        };

        Ok(TypeExpression {
            kind: TypeExprKind::Function { params, result },
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

    fn parse_global_definition(parser: &mut Parser) -> Result<Item, ()> {
        let global_keyword = parser.lexer.next();
        let mut_or_name_token = parser.next_expect(TokenKind::Identifier)?;
        let text = mut_or_name_token.span.text(parser.source);

        let (mutable, name) = match Keyword::try_from(text) {
            Ok(Keyword::Mut) => {
                let name = parser.parse_identifier()?;
                (Some(mut_or_name_token.span), name)
            }
            Ok(_) => {
                parser.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: parser.ast.file_id,
                        span: mut_or_name_token.span,
                    }
                    .report(),
                );

                return Err(());
            }
            Err(_) => (
                None,
                Identifier {
                    symbol: parser.interner.get_or_intern(text),
                    span: mut_or_name_token.span,
                },
            ),
        };

        let _ = parser.next_expect(TokenKind::Colon)?;
        let ty = parser.parse_type_expr()?;

        let _ = parser.next_expect(TokenKind::Eq)?;
        let value = parser.parse_expression(BindingPower::Default)?;

        let span = TextSpan::merge(global_keyword.span, value.span);
        Ok(Item {
            kind: ItemKind::GlobalDefinition {
                name,
                ty,
                mutable,
                value: Box::new(value),
            },
            span,
        })
    }
}
