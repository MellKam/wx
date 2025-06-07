use core::panic;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::diagnostics::DiagnosticContext;
use super::lexer::{Lexer, PeekableLexer, Token, TokenKind};
use super::{
    Ast, BinaryOp, EnumVariant, ExprKind, FunctionParam, FunctionSignature, Identifier, ItemKind,
    Statement, StmtKind, UnaryOp,
};
use crate::ast::{Expression, Item};
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

#[derive(Debug, Clone, Copy, PartialEq)]
enum Mutability {
    Mutable,
    Const,
}

#[derive(Debug, Clone)]
enum Keyword {
    Export,
    Local,
    Mut,
    Enum,
    Fn,
    Loop,
    Break,
    Continue,
    Return,
    If,
    Else,
    As,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(text: &str) -> Result<Self, Self::Error> {
        match text {
            "export" => Ok(Keyword::Export),
            "local" => Ok(Keyword::Local),
            "mut" => Ok(Keyword::Mut),
            "enum" => Ok(Keyword::Enum),
            "fn" => Ok(Keyword::Fn),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "loop" => Ok(Keyword::Loop),
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "return" => Ok(Keyword::Return),
            "as" => Ok(Keyword::As),
            _ => Err(()),
        }
    }
}

pub struct Parser<'input> {
    source: &'input str,
    lexer: PeekableLexer<'input>,
    interner: &'input mut StringInterner<StringBackend<SymbolU32>>,
    diagnostics: Vec<DiagnosticContext>,
    ast: Ast,
}

impl<'input> Parser<'input> {
    pub fn parse(
        file_id: FileId,
        source: &'input str,
        interner: &'input mut StringInterner<StringBackend<SymbolU32>>,
    ) -> (Ast, Vec<DiagnosticContext>) {
        let lexer: PeekableLexer<'input> = PeekableLexer::new(Lexer::new(source));

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
                Ok(item) => parser.ast.items.push(item),
                Err(_) => break,
            };
        }

        (parser.ast, parser.diagnostics)
    }

    fn parse_item(&mut self) -> Result<Item, ()> {
        let token = self.lexer.peek();
        let item_handler: fn(parser: &mut Parser) -> Result<Item, ()> =
            match self.resolve_keyword(token.clone()) {
                Ok(Keyword::Fn) => Parser::parse_function_definition,
                Ok(Keyword::Enum) => Parser::parse_enum_item,
                Ok(Keyword::Export) => Parser::parse_exported_function_definition,
                _ => return Err(()),
            };

        let item_id = item_handler(self)?;
        Ok(item_id)
    }

    fn parse_function_definition(parser: &mut Parser) -> Result<Item, ()> {
        let fn_keyword = parser.lexer.next();
        let signature = Parser::parse_function_signature(parser)?;

        match parser.lexer.peek().kind {
            TokenKind::OpenBrace => {}
            _ => {
                parser.diagnostics.push(
                    DiagnosticContext::MissingFunctionBody {
                        file_id: parser.ast.file_id,
                        span: TextSpan::merge(fn_keyword.span, signature.span),
                    }
                    .into(),
                );

                return Err(());
            }
        }

        let block = Parser::parse_block_expression(parser)?;
        let span = TextSpan::merge(fn_keyword.span, block.span);
        Ok(Item {
            kind: ItemKind::FunctionDefinition {
                signature,
                block: Box::new(block),
            },
            span,
        })
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
        let name_symbol = parser.intern_identifier(name_token.clone())?;
        let name = Identifier {
            symbol: name_symbol,
            span: name_token.span,
        };

        _ = parser.next_expect(TokenKind::Colon)?;
        let type_token = parser.next_expect(TokenKind::Identifier)?;
        let type_symbol = parser.intern_identifier(type_token.clone())?;
        let ty = Identifier {
            symbol: type_symbol,
            span: type_token.span,
        };

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
            TokenKind::Identifier => {
                let text = self
                    .source
                    .get(token.span.start().to_usize()..token.span.end().to_usize())
                    .expect("token span shouldn't be empty");
                match Keyword::try_from(text) {
                    Ok(Keyword::Return) => {
                        Some((Parser::parse_return_expression, BindingPower::Primary))
                    }
                    Ok(Keyword::If) => {
                        Some((Parser::parse_if_else_expression, BindingPower::Primary))
                    }
                    Ok(Keyword::Loop) => {
                        Some((Parser::parse_loop_expression, BindingPower::Primary))
                    }
                    Ok(Keyword::Break) => {
                        Some((Parser::parse_break_expression, BindingPower::Primary))
                    }
                    Ok(Keyword::Continue) => {
                        Some((Parser::parse_continue_expression, BindingPower::Primary))
                    }
                    _ => Some((Parser::parse_identifier_expression, BindingPower::Primary)),
                }
            }
            TokenKind::Int => Some((Parser::parse_int_expression, BindingPower::Primary)),
            TokenKind::OpenBrace => Some((Parser::parse_block_expression, BindingPower::Primary)),
            TokenKind::OpenParen => {
                Some((Parser::parse_grouping_expression, BindingPower::Default))
            }
            TokenKind::Minus | TokenKind::Bang | TokenKind::Caret => {
                Some((Parser::parse_unary_expression, BindingPower::Unary))
            }
            // TokenKind::Float { .. } => Some((parse_float_expression, BindingPower::Primary)),
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
                Ok(Keyword::As) => Some((Parser::parse_cast_expression, BindingPower::Cast)),
                _ => None,
            },
            TokenKind::Colon => Some((Parser::parse_labelled_expression, BindingPower::Primary)),
            // TokenKind::Dot => Some((parse_member_expression, BindingPower::Member)),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ()> {
        let name_token = self.next_expect(TokenKind::Identifier)?;
        let name_symbol = self.intern_identifier(name_token.clone())?;

        Ok(Identifier {
            symbol: name_symbol,
            span: name_token.span,
        })
    }

    fn parse_identifier_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let token = parser.lexer.next();
        let symbol = parser.intern_identifier(token.clone())?;

        Ok(Expression {
            kind: ExprKind::Identifier { symbol },
            span: token.span,
        })
    }

    fn parse_local_definition(parser: &mut Parser) -> Result<Statement, ()> {
        let local_keyword = parser.lexer.next();

        let mut_or_name_token = parser.next_expect(TokenKind::Identifier)?;
        let mut_or_name_text = match mut_or_name_token.kind {
            TokenKind::Identifier => parser
                .source
                .get(
                    mut_or_name_token.span.start().to_usize()
                        ..mut_or_name_token.span.end().to_usize(),
                )
                .unwrap(),
            _ => unreachable!(),
        };

        let (mutable, name) = match Keyword::try_from(mut_or_name_text) {
            Ok(Keyword::Mut) => {
                let mutable_token = mut_or_name_token;
                let mutable_symbol = parser.interner.get_or_intern(mut_or_name_text);
                let name = parser.parse_identifier()?;

                (
                    Some(Identifier {
                        symbol: mutable_symbol,
                        span: mutable_token.span,
                    }),
                    name,
                )
            }
            Ok(_) => panic!("can't use keyword as variable identifier"),
            Err(_) => {
                let name_token = mut_or_name_token;
                let name_symbol = parser.interner.get_or_intern(mut_or_name_text);

                (
                    None,
                    Identifier {
                        symbol: name_symbol,
                        span: name_token.span,
                    },
                )
            }
        };

        let ty = match parser.lexer.peek().kind {
            TokenKind::Colon => {
                let _ = parser.lexer.next();
                Some(parser.parse_identifier()?)
            }
            _ => None,
        };

        _ = parser.next_expect(TokenKind::Eq)?;
        let value = parser.parse_expression(BindingPower::Default)?;
        let span = TextSpan::merge(local_keyword.span, value.span);
        Ok(Statement {
            kind: StmtKind::LocalDefinition {
                mutable,
                name,
                ty,
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
                parser
                    .diagnostics
                    .push(DiagnosticContext::MissingClosingParen {
                        file_id: parser.ast.file_id,
                        opening_paren: open_paren.span,
                        expr_span: value.span,
                    });

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
        let ty = parser.parse_identifier()?;

        let span = TextSpan::merge(value.span, ty.span);
        Ok(Expression {
            kind: ExprKind::Cast {
                value: Box::new(value),
                ty,
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
                parser
                    .diagnostics
                    .push(DiagnosticContext::MissingUnaryOperand {
                        file_id: parser.ast.file_id,
                        span: operator_token.span.clone(),
                    });
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
                parser
                    .diagnostics
                    .push(DiagnosticContext::IncompleteBinaryExpression {
                        file_id: parser.ast.file_id,
                        span: operator_token.span.clone(),
                    });
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
                    parser
                        .diagnostics
                        .push(DiagnosticContext::ChainedComparisons {
                            file_id: parser.ast.file_id,
                            first_operator_span: operator_span,
                            second_operator_span: operator_token.span.clone(),
                        });
                }
                _ => {}
            }
            match right.kind {
                ExprKind::Binary {
                    operator,
                    operator_span,
                    ..
                } if BindingPower::from(operator) == BindingPower::Comparison => {
                    parser
                        .diagnostics
                        .push(DiagnosticContext::ChainedComparisons {
                            file_id: parser.ast.file_id,
                            first_operator_span: operator_token.span.clone(),
                            second_operator_span: operator_span,
                        });
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
        let text = parser
            .source
            .get(token.span.start().to_usize()..token.span.end().to_usize())
            .expect("failed to get text");
        let value = match token.kind {
            TokenKind::Int => text.parse::<i64>().ok(),
            _ => unreachable!(),
        };

        Ok(Expression {
            kind: match value {
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
            },
            span: token.span,
        })
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

    fn resolve_keyword(&mut self, token: Token) -> Result<Keyword, ()> {
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
            _ => panic!("expected an identifier for label"),
        };
        _ = parser.next_expect(TokenKind::Colon)?;

        let block = parser.parse_expression(BindingPower::Default)?;
        match block.kind {
            ExprKind::Block { .. } => {}
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
        let block = parser.parse_expression(BindingPower::Default)?;

        let span = TextSpan::merge(loop_keyword.span, block.span);
        Ok(Expression {
            kind: ExprKind::Loop {
                block: Box::new(block),
            },
            span,
        })
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
                    span: TextSpan::merge(name_token.span, return_type_token.span),
                })
            }
            _ => Ok(FunctionSignature {
                name: Identifier {
                    symbol: name_symbol,
                    span: name_token.span,
                },
                params: params.into_boxed_slice(),
                result: None,
                span: TextSpan::merge(name_token.span, close_paren.span),
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

    fn parse_block_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let open_brace = parser.lexer.next();
        let mut statements = Vec::new();
        let mut result: Option<Expression> = None;

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

            match parser.resolve_keyword(token.clone()) {
                Ok(Keyword::Local) => match Parser::parse_local_definition(parser) {
                    Ok(stmt) => statements.push(stmt),
                    Err(_) => parser.skip_to_next_statement(),
                },
                _ => match parser.parse_expression(BindingPower::Default) {
                    Ok(value) => match parser.lexer.peek().kind {
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
                            result = Some(value);
                            break;
                        }
                        _ => {
                            parser
                                .diagnostics
                                .push(DiagnosticContext::MissingStatementDelimiter {
                                    file_id: parser.ast.file_id,
                                    position: value.span.end(),
                                });

                            let span = value.span;
                            statements.push(Statement {
                                kind: StmtKind::DelimitedExpression {
                                    value: Box::new(value),
                                },
                                span,
                            });
                        }
                    },
                    Err(_) => parser.skip_to_next_statement(),
                },
            };
        }

        let close_brace = parser.lexer.next();
        Ok(Expression {
            kind: ExprKind::Block {
                statements: statements.into_boxed_slice(),
                result: result.map(Box::new),
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
                    parser.diagnostics.push(DiagnosticContext::UnexpectedEof {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    });
                    return Err(());
                }
                _ => {}
            }

            let argument = match parser.parse_expression(BindingPower::Default) {
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
            ExprKind::Identifier { symbol } => Identifier {
                symbol,
                span: namespace_expr.span,
            },
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

        let span = TextSpan::merge(namespace_expr.span, member.span);
        Ok(Expression {
            kind: ExprKind::Namespace { namespace, member },
            span,
        })
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

    fn parse_if_else_expression(parser: &mut Parser) -> Result<Expression, ()> {
        let if_keyword = parser.lexer.next();
        let condition = parser.parse_expression(BindingPower::Default)?;

        let then_block = Parser::parse_block_expression(parser)?;
        let maybe_else_keyword = parser.lexer.peek();
        match parser.resolve_keyword(maybe_else_keyword) {
            Ok(Keyword::Else) => {
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
