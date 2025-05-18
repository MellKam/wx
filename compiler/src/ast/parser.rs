use core::panic;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::diagnostics::DiagnosticContext;
use super::lexer::{Lexer, PeekableLexer, Token, TokenKind};
use super::{
    Ast, BinaryOperator, EnumVariant, ExprId, ExprKind, FunctionParam, FunctionSignature,
    Identifier, ItemEnum, ItemFunctionDefinition, ItemId, ItemKind, Statement, StmtId, StmtKind,
    UnaryOperator,
};
use crate::files::FileId;
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mutability {
    Mutable,
    Const,
}

impl From<BinaryOperator> for BindingPower {
    fn from(operator: BinaryOperator) -> Self {
        match operator {
            BinaryOperator::Add => BindingPower::Additive,
            BinaryOperator::Subtract => BindingPower::Additive,
            BinaryOperator::Multiply => BindingPower::Multiplicative,
            BinaryOperator::Divide => BindingPower::Multiplicative,
            BinaryOperator::Remainder => BindingPower::Multiplicative,
            BinaryOperator::Eq => BindingPower::Relational,
            BinaryOperator::NotEq => BindingPower::Relational,
            BinaryOperator::Less => BindingPower::Relational,
            BinaryOperator::Greater => BindingPower::Relational,
            BinaryOperator::LessEq => BindingPower::Relational,
            BinaryOperator::GreaterEq => BindingPower::Relational,
            BinaryOperator::Assign => BindingPower::Assignment,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Export,
    Const,
    Mut,
    Enum,
    Fn,
    Loop,
    Break,
    Continue,
    Match,
    Return,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(text: &str) -> Result<Self, Self::Error> {
        match text {
            "export" => Ok(Keyword::Export),
            "const" => Ok(Keyword::Const),
            "mut" => Ok(Keyword::Mut),
            "enum" => Ok(Keyword::Enum),
            "fn" => Ok(Keyword::Fn),
            "loop" => Ok(Keyword::Loop),
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "match" => Ok(Keyword::Match),
            "return" => Ok(Keyword::Return),
            _ => Err(()),
        }
    }
}

pub struct Parser<'a> {
    source: &'a str,
    lexer: PeekableLexer<'a>,
    interner: &'a mut StringInterner<StringBackend<SymbolU32>>,
    diagnostics: Vec<DiagnosticContext>,
    ast: Ast,
}

impl<'a> Parser<'a> {
    pub fn parse(
        file_id: FileId,
        source: &'a str,
        interner: &'a mut StringInterner<StringBackend<SymbolU32>>,
    ) -> (Ast, Vec<DiagnosticContext>) {
        let lexer: PeekableLexer<'a> = PeekableLexer::new(Lexer::new(source));

        let mut parser = Self {
            source,
            lexer,
            interner,
            diagnostics: Vec::new(),
            ast: Ast::new(file_id),
        };
        loop {
            let token = parser.lexer.peek();
            if token.kind == TokenKind::Eof {
                break;
            }

            match parser.parse_item() {
                Ok(_) => {}
                Err(_) => break,
            };
        }

        (parser.ast, parser.diagnostics)
    }

    fn parse_item(&mut self) -> Result<ItemId, ()> {
        let token = self.lexer.peek();
        let item_handler: fn(parser: &mut Parser) -> Result<ItemId, ()> =
            match self.intern_keyword(token.clone()) {
                Ok(Keyword::Fn) => Parser::parse_function_definition,
                Ok(Keyword::Enum) => Parser::parse_enum_item,
                Ok(Keyword::Export) => Parser::parse_exported_function_definition,
                _ => return Err(()),
            };

        let item_id = item_handler(self)?;
        Ok(item_id)
    }

    fn parse_exported_function_definition(parser: &mut Parser) -> Result<ItemId, ()> {
        let export_keyword = parser.lexer.next();
        let item_id = Parser::parse_function_definition(parser)?;

        parser
            .ast
            .set_item(item_id, |item| match &mut item.kind {
                ItemKind::FunctionDefinition(def) => {
                    def.export = Some(export_keyword.span);
                    item.span = Span::merge(export_keyword.span, item.span);
                }
                _ => unreachable!(),
            })
            .unwrap();

        Ok(item_id)
    }

    fn parse_expression(&mut self, limit_bp: BindingPower) -> Result<ExprId, ()> {
        let token = self.lexer.peek();
        let nud_handler = match self.nud_lookup(token.clone()) {
            Some((nud_handler, _)) => nud_handler,
            None => return Err(()),
        };
        let mut left = nud_handler(self)?;

        loop {
            let token = self.lexer.peek();

            let (led_handler, operator_bp) = match Parser::led_lookup(token.kind) {
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
    ) -> Option<(fn(parser: &mut Parser) -> Result<ExprId, ()>, BindingPower)> {
        match token.kind {
            TokenKind::Int => Some((Parser::parse_int_expression, BindingPower::Primary)),
            TokenKind::OpenBrace => Some((Parser::parse_block_expression, BindingPower::Primary)),
            // TokenKind::Float { .. } => Some((parse_float_expression, BindingPower::Primary)),
            // TokenKind::String { .. } => Some((parse_string_expression, BindingPower::Primary)),
            // TokenKind::Char { .. } => Some((parse_string_expression, BindingPower::Primary)),
            TokenKind::Identifier => {
                let text = self
                    .source
                    .get(token.span.start().to_usize()..token.span.end().to_usize())
                    .expect("failed to get text");
                match Keyword::try_from(text) {
                    Ok(Keyword::Return) => {
                        Some((Parser::parse_return_expression, BindingPower::Primary))
                    }
                    _ => Some((Parser::parse_identifier_expression, BindingPower::Primary)),
                }
            }
            TokenKind::OpenParen => {
                Some((Parser::parse_grouping_expression, BindingPower::Default))
            }
            TokenKind::Minus | TokenKind::Bang => {
                Some((Parser::parse_unary_expression, BindingPower::Unary))
            }
            _ => None,
        }
    }

    fn led_lookup(
        token: TokenKind,
    ) -> Option<(
        fn(parser: &mut Parser, left: ExprId, bp: BindingPower) -> Result<ExprId, ()>,
        BindingPower,
    )> {
        match token {
            TokenKind::Plus | TokenKind::Minus => {
                Some((Parser::parse_binary_expression, BindingPower::Additive))
            }
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Some((
                Parser::parse_binary_expression,
                BindingPower::Multiplicative,
            )),
            TokenKind::Eq => Some((Parser::parse_binary_expression, BindingPower::Assignment)),
            TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::OpenAngle
            | TokenKind::LessEq
            | TokenKind::CloseAngle
            | TokenKind::GreaterEq => {
                Some((Parser::parse_binary_expression, BindingPower::Relational))
            }
            TokenKind::ColonColon => Some((
                Parser::parse_namespace_member_expression,
                BindingPower::Member,
            )),
            // TokenKind::VbarVbar | TokenKind::AmperAmper => {
            //     Some((parse_binary_expression, BindingPower::Logical))
            // }
            TokenKind::OpenParen => Some((Parser::parse_call_expression, BindingPower::Call)),
            // TokenKind::Dot => Some((parse_member_expression, BindingPower::Member)),
            _ => None,
        }
    }

    fn parse_identifier_expression(parser: &mut Parser) -> Result<ExprId, ()> {
        let token = parser.lexer.next();
        let symbol = parser.intern_identifier(token.clone())?;

        let expr_id = parser
            .ast
            .push_expr(ExprKind::Identifier { symbol }, token.span);
        Ok(expr_id)
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ()> {
        let name_token = self.next_expect(TokenKind::Identifier)?;
        let name_symbol = self.intern_identifier(name_token.clone())?;

        Ok(Identifier {
            symbol: name_symbol,
            span: name_token.span,
        })
    }

    fn parse_variable_definition(parser: &mut Parser) -> Result<StmtId, ()> {
        let mutability_token = parser.next_expect(TokenKind::Identifier)?;
        let mutability = match parser.intern_keyword(mutability_token.clone()) {
            Ok(Keyword::Const) => Mutability::Const,
            Ok(Keyword::Mut) => Mutability::Mutable,
            _ => unreachable!(),
        };

        let name = parser.parse_identifier()?;
        let ty = match parser.lexer.peek().kind {
            TokenKind::Colon => {
                let _ = parser.lexer.next();
                Some(parser.parse_identifier()?)
            }
            _ => None,
        };

        _ = parser.next_expect(TokenKind::Eq)?;
        let value_expr_id = parser.parse_expression(BindingPower::Default)?;
        let value_expr = parser.ast.get_expr(value_expr_id).unwrap();

        let stmt = Statement {
            kind: match mutability {
                Mutability::Const => StmtKind::ConstDefinition {
                    name,
                    ty,
                    value: value_expr_id,
                },
                Mutability::Mutable => StmtKind::MutableDefinition {
                    name,
                    ty,
                    value: value_expr_id,
                },
            },
            span: Span::merge(mutability_token.span, value_expr.span),
        };
        let stmt_id = parser.ast.push_stmt(stmt);
        Ok(stmt_id)
    }

    fn parse_return_expression(parser: &mut Parser) -> Result<ExprId, ()> {
        let return_keyword = parser.lexer.next();
        let value_expr_id = parser.parse_expression(BindingPower::Default)?;
        let value_expr = parser.ast.get_expr(value_expr_id).unwrap();

        let stmt_id = parser.ast.push_expr(
            ExprKind::Return {
                value: value_expr_id,
            },
            Span::merge(return_keyword.span, value_expr.span),
        );
        Ok(stmt_id)
    }

    fn parse_grouping_expression(parser: &mut Parser) -> Result<ExprId, ()> {
        let open_paren = parser.lexer.next();
        let expr_id = parser.parse_expression(BindingPower::Default)?;

        match parser.lexer.peek().kind {
            TokenKind::CloseParen => {
                let close_paren = parser.lexer.next();
                let expr_id = parser.ast.push_expr(
                    ExprKind::Grouping { value: expr_id },
                    Span::merge(open_paren.span, close_paren.span),
                );
                Ok(expr_id)
            }
            _ => {
                let expr = parser.ast.get_expr(expr_id).unwrap();
                parser
                    .diagnostics
                    .push(DiagnosticContext::MissingClosingParen {
                        file_id: parser.ast.file_id,
                        opening_paren: open_paren.span,
                        expr_span: expr.span,
                    });

                let expr_id = parser.ast.push_expr(
                    ExprKind::Grouping { value: expr_id },
                    Span::merge(open_paren.span, expr.span),
                );
                Ok(expr_id)
            }
        }
    }

    fn parse_unary_expression(parser: &mut Parser) -> Result<ExprId, ()> {
        let operator = parser.lexer.next();
        let operator_kind = match UnaryOperator::try_from(operator.kind) {
            Ok(operator) => operator,
            Err(_) => panic!("invalid unary operator"),
        };

        let operand_id = match parser.parse_expression(BindingPower::Unary) {
            Ok(operand_id) => operand_id,
            Err(_) => {
                parser
                    .diagnostics
                    .push(DiagnosticContext::MissingUnaryOperand {
                        file_id: parser.ast.file_id,
                        span: operator.span.clone(),
                    });
                return Err(());
            }
        };
        let operand = parser.ast.get_expr(operand_id).unwrap();

        let expr_id = parser.ast.push_expr(
            ExprKind::Unary {
                operator: operator_kind,
                operand: operand_id,
            },
            Span::merge(operator.span, operand.span),
        );
        Ok(expr_id)
    }

    fn parse_binary_expression(
        parser: &mut Parser,
        left: ExprId,
        bp: BindingPower,
    ) -> Result<ExprId, ()> {
        let operator = parser.lexer.next();
        let operator_kind = match BinaryOperator::try_from(operator.kind) {
            Ok(operator) => operator,
            Err(_) => panic!("invalid binary operator"),
        };

        let right_id = match parser.parse_expression(bp) {
            Ok(right_id) => right_id,
            Err(_) => {
                parser
                    .diagnostics
                    .push(DiagnosticContext::IncompleteBinaryExpression {
                        file_id: parser.ast.file_id,
                        span: operator.span.clone(),
                    });
                return Err(());
            }
        };

        let right_expr = parser.ast.get_expr(right_id).unwrap();
        let left_expr = parser.ast.get_expr(left).unwrap();
        let expr_id = parser.ast.push_expr(
            ExprKind::Binary {
                left,
                right: right_id,
                operator: operator_kind,
            },
            Span::merge(left_expr.span, right_expr.span),
        );
        Ok(expr_id)
    }

    fn parse_int_expression(parser: &mut Parser) -> Result<ExprId, ()> {
        let token = parser.lexer.next();
        let text = parser
            .source
            .get(token.span.start().to_usize()..token.span.end().to_usize())
            .expect("failed to get text");
        let value = match token.kind {
            TokenKind::Int => text.parse::<i64>().ok(),
            _ => unreachable!(),
        };

        let expr = match value {
            Some(value) => ExprKind::Int { value },
            None => {
                parser
                    .diagnostics
                    .push(DiagnosticContext::InvalidIntegerLiteral {
                        file_id: parser.ast.file_id,
                        span: token.span.clone(),
                    });

                ExprKind::Int { value: 0 }
            }
        };

        let expr_id = parser.ast.push_expr(expr, token.span);
        Ok(expr_id)
    }

    fn intern_identifier(&mut self, token: Token) -> Result<SymbolU32, ()> {
        let text = match token.kind {
            TokenKind::Identifier => self
                .source
                .get(token.span.start().to_usize()..token.span.end().to_usize())
                .expect("failed to get text"),
            _ => return Err(()),
        };

        match Keyword::try_from(text) {
            Ok(_) => {
                self.diagnostics
                    .push(DiagnosticContext::ReservedIdentifier {
                        file_id: self.ast.file_id,
                        span: token.span,
                    });

                Err(())
            }
            Err(_) => Ok(self.interner.get_or_intern(text)),
        }
    }

    fn intern_keyword(&mut self, token: Token) -> Result<Keyword, ()> {
        match token.kind {
            TokenKind::Identifier => self
                .source
                .get(token.span.start().to_usize()..token.span.end().to_usize())
                .map(|text| Keyword::try_from(text))
                .ok_or(())
                .and_then(|result| result),
            _ => Err(()),
        }
    }

    fn parse_function_signature(parser: &mut Parser) -> Result<FunctionSignature, ()> {
        let name_token = parser.next_expect(TokenKind::Identifier)?;
        let name_symbol = parser.intern_identifier(name_token.clone())?;

        _ = parser.next_expect(TokenKind::OpenParen)?;
        let mut params = Vec::new();
        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseParen => break,
                TokenKind::Comma => {
                    let _ = parser.lexer.next();
                    continue;
                }
                TokenKind::Eof => {
                    parser.diagnostics.push(DiagnosticContext::UnexpectedEof {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    });
                    return Err(());
                }
                _ => {}
            }

            let param_name_token = parser.next_expect(TokenKind::Identifier)?;
            let param_name_symbol = parser.intern_identifier(param_name_token.clone())?;
            _ = parser.next_expect(TokenKind::Colon)?;
            let param_type = parser.next_expect(TokenKind::Identifier)?;
            let param_type_symbol = parser.intern_identifier(param_type.clone())?;

            params.push(FunctionParam {
                name: Identifier {
                    symbol: param_name_symbol,
                    span: param_name_token.span,
                },
                ty: Identifier {
                    symbol: param_type_symbol,
                    span: param_type.span,
                },
            });
        }

        let close_paren = parser.lexer.next();
        match parser.lexer.peek().kind {
            TokenKind::Colon => {
                let _ = parser.lexer.next();
                let return_type_token = parser.next_expect(TokenKind::Identifier)?;
                let return_type_symbol = parser.intern_identifier(return_type_token.clone())?;

                Ok(FunctionSignature {
                    name: Identifier {
                        symbol: name_symbol,
                        span: name_token.span,
                    },
                    params: params.into_boxed_slice(),
                    result: Some(Identifier {
                        symbol: return_type_symbol,
                        span: return_type_token.span,
                    }),
                    span: Span::merge(name_token.span, return_type_token.span),
                })
            }
            _ => Ok(FunctionSignature {
                name: Identifier {
                    symbol: name_symbol,
                    span: name_token.span,
                },
                params: params.into_boxed_slice(),
                result: None,
                span: Span::merge(name_token.span, close_paren.span),
            }),
        }
    }

    // fn parse_function_declaration(parser: &mut Parser) -> Option<ItemId> {
    //     let fn_keyword = parser.lexer.next();
    //     let signature = Parser::parse_function_signature(parser)?;

    //     let item_span = Span::merge(fn_keyword.span, signature.span);
    //     let item_id = parser
    //         .ast
    //         .push_item(ItemKind::FunctionDeclaration { signature }, item_span);
    //     Some(item_id)
    // }

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

    fn parse_block_expression(parser: &mut Parser) -> Result<ExprId, ()> {
        let open_brace = parser.lexer.next();
        let mut statements = Vec::new();
        let mut result: Option<ExprId> = None;

        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseBrace => break,
                TokenKind::Eof => {
                    parser.diagnostics.push(DiagnosticContext::UnexpectedEof {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    });
                    return Err(());
                }
                _ => {}
            };

            let handler: Option<fn(&mut Parser) -> Result<StmtId, ()>> =
                match parser.intern_keyword(token.clone()) {
                    Ok(Keyword::Const) => Some(Parser::parse_variable_definition),
                    Ok(Keyword::Mut) => Some(Parser::parse_variable_definition),
                    _ => None,
                };

            match handler {
                Some(handler) => {
                    match handler(parser) {
                        Ok(stmt_id) => statements.push(stmt_id),
                        Err(_) => parser.skip_to_next_statement(),
                    };
                    continue;
                }
                None => {}
            }

            let expr_id = match parser.parse_expression(BindingPower::Default) {
                Ok(expr_id) => expr_id,
                Err(_) => {
                    parser.skip_to_next_statement();
                    continue;
                }
            };
            let expr = parser.ast.get_expr(expr_id).unwrap();

            match parser.lexer.peek().kind {
                TokenKind::SemiColon => {
                    let semicolon = parser.lexer.next();
                    let stmt_id = parser.ast.push_stmt(Statement {
                        kind: StmtKind::DelimitedExpression { value: expr_id },
                        span: Span::merge(expr.span, semicolon.span),
                    });
                    statements.push(stmt_id);
                }
                TokenKind::CloseBrace => {
                    result = Some(expr_id);
                    break;
                }
                _ => {
                    parser
                        .diagnostics
                        .push(DiagnosticContext::MissingStatementDelimiter {
                            file_id: parser.ast.file_id,
                            position: expr.span.end(),
                        });

                    let stmt_id = parser.ast.push_stmt(Statement {
                        kind: StmtKind::DelimitedExpression { value: expr_id },
                        span: expr.span,
                    });
                    statements.push(stmt_id);
                }
            }
        }

        let close_brace = parser.lexer.next();
        let expr_id = parser.ast.push_expr(
            ExprKind::Block {
                statements: statements.into_boxed_slice(),
                result,
            },
            Span::merge(open_brace.span, close_brace.span),
        );

        Ok(expr_id)
    }

    fn parse_function_definition(parser: &mut Parser) -> Result<ItemId, ()> {
        let fn_keyword = parser.lexer.next();
        let signature = Parser::parse_function_signature(parser)?;

        match parser.lexer.peek().kind {
            TokenKind::OpenBrace => {}
            _ => {
                parser.diagnostics.push(
                    DiagnosticContext::MissingFunctionBody {
                        file_id: parser.ast.file_id,
                        span: Span::merge(fn_keyword.span, signature.span),
                    }
                    .into(),
                );

                return Err(());
            }
        }

        let block_expr_id = Parser::parse_block_expression(parser)?;
        let block = parser.ast.get_expr(block_expr_id).unwrap();
        let item_id = parser.ast.push_item(
            ItemKind::FunctionDefinition(ItemFunctionDefinition {
                export: None,
                signature,
                block: block_expr_id,
            }),
            Span::merge(fn_keyword.span, block.span),
        );

        Ok(item_id)
    }

    fn parse_call_expression(
        parser: &mut Parser,
        callee: ExprId,
        _: BindingPower,
    ) -> Result<ExprId, ()> {
        let _ = parser.lexer.next();
        let mut arguments = Vec::new();
        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseParen => break,
                TokenKind::Eof => {
                    parser.diagnostics.push(DiagnosticContext::UnexpectedEof {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    });
                    return Err(());
                }
                _ => {}
            }

            let argument = match parser.parse_expression(BindingPower::Comma) {
                Ok(argument) => argument,
                Err(_) => continue,
            };
            arguments.push(argument);

            match parser.lexer.peek().kind {
                TokenKind::CloseParen => break,
                TokenKind::Comma => {
                    let _ = parser.lexer.next();
                    continue;
                }
                _ => panic!("unexpected token"),
            }
        }

        let close_paren = parser.lexer.next();

        let callee_expr = parser.ast.get_expr(callee).unwrap();
        let expr_id = parser.ast.push_expr(
            ExprKind::Call {
                callee,
                arguments: arguments.into_boxed_slice(),
            },
            Span::merge(callee_expr.span, close_paren.span),
        );
        Ok(expr_id)
    }

    fn parse_namespace_member_expression(
        parser: &mut Parser,
        namespace_expr_id: ExprId,
        _: BindingPower,
    ) -> Result<ExprId, ()> {
        let _double_colon = parser.lexer.next();
        let member = parser.parse_identifier()?;

        let namespace_expr = parser.ast.get_expr(namespace_expr_id).unwrap();
        let namespace_symbol = match namespace_expr.kind {
            ExprKind::Identifier { symbol } => symbol,
            _ => {
                parser
                    .diagnostics
                    .push(DiagnosticContext::InvalidNamespace {
                        file_id: parser.ast.file_id,
                        span: namespace_expr.span,
                    });

                return Err(());
            }
        };

        let span = Span::merge(namespace_expr.span, member.span);
        let expr_id = parser.ast.push_expr(
            ExprKind::NamespaceMember {
                namespace: Identifier {
                    symbol: namespace_symbol,
                    span: namespace_expr.span,
                },
                member,
            },
            span,
        );
        Ok(expr_id)
    }

    fn next_expect(&mut self, kind: TokenKind) -> Result<Token, ()> {
        self.lexer.next_expect(kind).map_err(|token| {
            self.diagnostics.push(DiagnosticContext::UnexpectedToken {
                file_id: self.ast.file_id,
                received: token,
                expected_kind: kind,
            });
            ()
        })
    }

    fn parse_enum_item(parser: &mut Parser) -> Result<ItemId, ()> {
        let enum_keyword = parser.lexer.next();

        let name_token = parser.next_expect(TokenKind::Identifier)?;
        let name_symbol = parser.intern_identifier(name_token.clone())?;

        _ = parser.next_expect(TokenKind::Colon)?;
        let type_token = parser.next_expect(TokenKind::Identifier)?;
        let type_symbol = parser.intern_identifier(type_token.clone())?;

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
                    parser.diagnostics.push(DiagnosticContext::UnexpectedEof {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    });

                    return Err(());
                }
                _ => {}
            }

            let variant_name_token = parser.next_expect(TokenKind::Identifier)?;
            let variant_name_symbol = parser.intern_identifier(variant_name_token.clone())?;
            _ = parser.next_expect(TokenKind::Eq)?;
            let value_expr_id = parser.parse_expression(BindingPower::Default)?;

            variants.push(EnumVariant {
                name: Identifier {
                    symbol: variant_name_symbol,
                    span: variant_name_token.span,
                },
                value: value_expr_id,
            });
        }

        let close_brace = parser.lexer.next();

        let item_id = parser.ast.push_item(
            ItemKind::Enum(ItemEnum {
                name: Identifier {
                    symbol: name_symbol,
                    span: name_token.span,
                },
                ty: Identifier {
                    symbol: type_symbol,
                    span: type_token.span,
                },
                variants: variants.into_boxed_slice(),
            }),
            Span::merge(enum_keyword.span, close_brace.span),
        );
        Ok(item_id)
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

// fn parse_block_statement(parser: &mut Parser) -> Option<StmtId> {
//     let open_brace = parser.lexer.next();
//     let mut statements = Vec::new();

//     loop {
//         let token = parser.lexer.peek();
//         if token.kind == TokenKind::CloseBrace {
//             break;
//         }

//         let stmt_id = match parser.parse_statement() {
//             Some(stmt_id) => stmt_id,
//             None => {
//                 parser.skip_statement();
//                 continue;
//             }
//         };
//         statements.push(stmt_id);
//     }

//     let close_brace = parser.lexer.next();

//     let stmt_id = parser.ast.push_stmt(
//         StmtKind::Block { statements },
//         TextSpan::combine(open_brace.span, close_brace.span),
//     );
//     Some(stmt_id)
// }
