use core::panic;
use std::cell::RefCell;
use std::rc::Rc;

use codespan::Span;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::diagnostics::DiagnosticKind;
use super::files::FileId;
use super::lexer::{Lexer, PeekableLexer, Token, TokenKind};
use super::{
    Ast, BinaryOperator, Block, EnumVariant, ExprId, ExprKind, FunctionParam, FunctionSignature,
    Identifier, InvalidIntegerLiteralDiagnostic, ItemEnum, ItemFunctionDefinition, ItemId,
    ItemKind, MissingClosingParenDiagnostic, MissingFunctionBodyDiagnostic,
    ReservedIdentifierDiagnostic, StmtId, StmtKind, UnaryOperator,
};

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
    Underscore,
    Match,
    Return,
    True,
    False,
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
            "_" => Ok(Keyword::Underscore),
            "match" => Ok(Keyword::Match),
            "return" => Ok(Keyword::Return),
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            _ => Err(()),
        }
    }
}

pub struct Parser<'a> {
    file_id: FileId,
    source: &'a str,
    lexer: PeekableLexer<'a>,
    interner: &'a mut StringInterner<StringBackend>,
    diagnostics: Rc<RefCell<Vec<DiagnosticKind>>>,
    ast: Ast,
}

impl<'a> Parser<'a> {
    pub fn parse(
        file_id: FileId,
        source: &'a str,
        diagnostics: Rc<RefCell<Vec<DiagnosticKind>>>,
        interner: &'a mut StringInterner<StringBackend>,
    ) -> Ast {
        let lexer: PeekableLexer<'a> = PeekableLexer::new(Lexer::new(source));

        let mut parser = Self {
            file_id,
            source,
            lexer,
            interner,
            diagnostics,
            ast: Ast::new(),
        };
        loop {
            let token = parser.lexer.peek();
            if token.kind == TokenKind::Eof {
                break;
            }

            match parser.parse_item() {
                Some(_) => {}
                None => break,
            };
        }

        parser.ast
    }

    fn parse_item(&mut self) -> Option<ItemId> {
        let token = self.lexer.peek();
        let symbol = match token.kind {
            TokenKind::Identifier => self.get_identifier_symbol(token)?,
            _ => return None,
        };

        let item_handler: fn(parser: &mut Parser) -> Option<ItemId> =
            match Keyword::try_from(self.interner.resolve(symbol)?) {
                Ok(Keyword::Fn) => Parser::parse_function_definition,
                Ok(Keyword::Enum) => Parser::parse_enum_item,
                Ok(Keyword::Export) => Parser::parse_exported_function_definition,
                _ => return None,
            };

        let item_id = item_handler(self)?;
        Some(item_id)
    }

    fn parse_exported_function_definition(parser: &mut Parser) -> Option<ItemId> {
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

        Some(item_id)
    }

    fn parse_statement(&mut self) -> Option<StmtId> {
        let token = self.lexer.peek();
        let keyword = match token.kind {
            TokenKind::Identifier => self
                .get_identifier_symbol(token.clone())
                .map(|symbol| self.interner.resolve(symbol))
                .flatten()
                .map(|text| Keyword::try_from(text).ok())
                .flatten(),
            _ => None,
        };

        match keyword {
            Some(Keyword::Const) => Parser::parse_variable_definition(self),
            Some(Keyword::Mut) => Parser::parse_variable_definition(self),
            Some(Keyword::Return) => Parser::parse_return_statement(self),
            _ => {
                let expr_id = self.parse_expression(BindingPower::Default)?;
                let expr = self.ast.get_expr(expr_id).unwrap();
                let stmt_id = self
                    .ast
                    .push_stmt(StmtKind::Expression { expr: expr_id }, expr.span);

                Some(stmt_id)
            }
        }
    }

    fn parse_expression(&mut self, limit_bp: BindingPower) -> Option<ExprId> {
        let token = self.lexer.peek();
        let nud_handler = match self.nud_lookup(token.clone()) {
            Some((nud_handler, _)) => nud_handler,
            None => return None,
        };
        let mut left = nud_handler(self).unwrap();

        loop {
            let token = self.lexer.peek();

            let (led_handler, operator_bp) = match Parser::led_lookup(token.kind) {
                Some((_, bp)) if bp <= limit_bp => break,
                Some((handler, bp)) => (handler, bp),
                None => break,
            };

            left = led_handler(self, left, operator_bp)?;
        }
        Some(left)
    }

    fn nud_lookup(
        &mut self,
        token: Token,
    ) -> Option<(fn(parser: &mut Parser) -> Option<ExprId>, BindingPower)> {
        match token.kind {
            TokenKind::Int => Some((Parser::parse_int_expression, BindingPower::Primary)),
            // TokenKind::Float { .. } => Some((parse_float_expression, BindingPower::Primary)),
            // TokenKind::String { .. } => Some((parse_string_expression, BindingPower::Primary)),
            // TokenKind::Char { .. } => Some((parse_string_expression, BindingPower::Primary)),
            TokenKind::Identifier => {
                Some((Parser::parse_identifier_expression, BindingPower::Primary))
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
        fn(parser: &mut Parser, left: ExprId, bp: BindingPower) -> Option<ExprId>,
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

    fn parse_identifier_expression(parser: &mut Parser) -> Option<ExprId> {
        let token = parser.lexer.next();
        let text = parser
            .source
            .get(token.span.start().to_usize()..token.span.end().to_usize())?;
        let symbol = parser.interner.get_or_intern(text);

        let expr_id = parser
            .ast
            .push_expr(ExprKind::Identifier { symbol }, token.span);
        Some(expr_id)
    }

    fn parse_variable_definition(parser: &mut Parser) -> Option<StmtId> {
        let binding_type_token = parser.lexer.next();
        let binding_type_symbol = parser.get_identifier_symbol(binding_type_token.clone())?;
        let binding_type = match Keyword::try_from(parser.interner.resolve(binding_type_symbol)?) {
            Ok(Keyword::Const) => Mutability::Const,
            Ok(Keyword::Mut) => Mutability::Mutable,
            _ => unreachable!(),
        };

        let name = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
        let name_symbol = parser.get_identifier_symbol(name.clone())?;
        match Keyword::try_from(parser.interner.resolve(name_symbol)?) {
            Ok(_) => parser.diagnostics.borrow_mut().push(
                ReservedIdentifierDiagnostic {
                    file_id: parser.file_id,
                    span: name.span,
                }
                .into(),
            ),
            Err(_) => {}
        }

        let type_symbol = match parser.lexer.peek().kind {
            TokenKind::Colon => {
                let _ = parser.lexer.next();
                let type_token = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
                Some(parser.get_identifier_symbol(type_token)?)
            }
            _ => None,
        };

        let _ = parser.lexer.next_expect(TokenKind::Eq).ok()?;
        let value = parser.parse_expression(BindingPower::Default)?;
        let value_expr = parser.ast.get_expr(value).unwrap();

        let stmt_id = parser.ast.push_stmt(
            match binding_type {
                Mutability::Const => StmtKind::ConstDefinition {
                    name: name_symbol,
                    ty: type_symbol,
                    value,
                },
                Mutability::Mutable => StmtKind::MutableDefinition {
                    name: name_symbol,
                    ty: type_symbol,
                    value,
                },
            },
            Span::merge(binding_type_token.span, value_expr.span),
        );
        Some(stmt_id)
    }

    fn parse_return_statement(parser: &mut Parser) -> Option<StmtId> {
        let return_keyword = parser.lexer.next();
        let value = parser.parse_expression(BindingPower::Default)?;
        let value_expr = parser.ast.get_expr(value).unwrap();

        let stmt_id = parser.ast.push_stmt(
            StmtKind::Return { value },
            Span::merge(return_keyword.span, value_expr.span),
        );
        Some(stmt_id)
    }

    fn parse_grouping_expression(parser: &mut Parser) -> Option<ExprId> {
        let open_paren = parser.lexer.next();
        let inner_expr_id = parser.parse_expression(BindingPower::Default)?;

        match parser.lexer.peek().kind {
            TokenKind::CloseParen => {
                let _ = parser.lexer.next();
                Some(inner_expr_id)
            }
            _ => {
                let expr = parser.ast.get_expr(inner_expr_id).unwrap();
                parser.diagnostics.borrow_mut().push(
                    MissingClosingParenDiagnostic {
                        file_id: parser.file_id,
                        opening_paren: open_paren.span,
                        expected_closing_paren_position: expr.span.end(),
                    }
                    .into(),
                );
                None
            }
        }
    }

    fn parse_unary_expression(parser: &mut Parser) -> Option<ExprId> {
        let operator = parser.lexer.next();
        let operator_kind = match UnaryOperator::try_from(operator.kind) {
            Ok(operator) => operator,
            Err(_) => unreachable!(),
        };

        let operand_id = parser.parse_expression(BindingPower::Unary)?;
        let operand = parser.ast.get_expr(operand_id).unwrap();

        let expr_id = parser.ast.push_expr(
            ExprKind::Unary {
                operator: operator_kind,
                operand: operand_id,
            },
            Span::merge(operator.span, operand.span),
        );
        Some(expr_id)
    }

    fn parse_binary_expression(
        parser: &mut Parser,
        left: ExprId,
        bp: BindingPower,
    ) -> Option<ExprId> {
        let operator = parser.lexer.next();
        let operator_kind = match BinaryOperator::try_from(operator.kind) {
            Ok(operator) => operator,
            Err(_) => unreachable!(),
        };

        let right_id = parser.parse_expression(bp)?;

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
        Some(expr_id)
    }

    fn parse_int_expression(parser: &mut Parser) -> Option<ExprId> {
        let token = parser.lexer.next();
        let text = parser
            .source
            .get(token.span.start().to_usize()..token.span.end().to_usize())?;

        let value = match token.kind {
            TokenKind::Int => text.parse::<i64>().ok(),
            _ => unreachable!(),
        };

        let expr = match value {
            Some(value) => ExprKind::Int { value },
            None => {
                parser.diagnostics.borrow_mut().push(
                    InvalidIntegerLiteralDiagnostic {
                        file_id: parser.file_id,
                        span: token.span.clone(),
                    }
                    .into(),
                );

                ExprKind::Int { value: 0 }
            }
        };

        let expr_id = parser.ast.push_expr(expr, token.span);
        Some(expr_id)
    }

    fn get_identifier_symbol(&mut self, token: Token) -> Option<SymbolU32> {
        match token.kind {
            TokenKind::Identifier => {
                let text = self
                    .source
                    .get(token.span.start().to_usize()..token.span.end().to_usize())?;
                Some(self.interner.get_or_intern(text))
            }
            _ => None,
        }
    }

    fn parse_function_signature(parser: &mut Parser) -> Option<FunctionSignature> {
        let name_token = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
        let name_symbol = parser.get_identifier_symbol(name_token.clone())?;

        let _ = parser.lexer.next_expect(TokenKind::OpenParen).ok()?;
        let mut params = Vec::new();
        loop {
            let token = parser.lexer.peek();
            if token.kind == TokenKind::CloseParen {
                break;
            }

            let param_name_token = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
            let param_name_symbol = parser.get_identifier_symbol(param_name_token.clone())?;

            let _ = parser.lexer.next_expect(TokenKind::Colon).ok()?;
            let param_type = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
            let param_type_symbol = parser.get_identifier_symbol(param_type.clone())?;

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

            let token = parser.lexer.peek();
            if token.kind == TokenKind::Comma {
                let _ = parser.lexer.next();
            }
        }

        let close_paren = parser.lexer.next();
        match parser.lexer.peek().kind {
            TokenKind::Colon => {
                let _ = parser.lexer.next();
                let return_type_token = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
                let return_type_symbol =
                    Parser::get_identifier_symbol(parser, return_type_token.clone())?;

                Some(FunctionSignature {
                    name: Identifier {
                        symbol: name_symbol,
                        span: name_token.span,
                    },
                    params,
                    result: Some(Identifier {
                        symbol: return_type_symbol,
                        span: return_type_token.span,
                    }),
                    span: Span::merge(name_token.span, return_type_token.span),
                })
            }
            _ => Some(FunctionSignature {
                name: Identifier {
                    symbol: name_symbol,
                    span: name_token.span,
                },
                params,
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

    fn parse_block(parser: &mut Parser) -> Option<Block> {
        let open_brace = parser.lexer.next();
        let mut block = Block {
            statements: Vec::new(),
            result: None,
            span: open_brace.span,
        };

        loop {
            match parser.lexer.peek().kind {
                TokenKind::CloseBrace => break,
                TokenKind::Eof => todo!("handle eof"),
                _ => {}
            }

            let stmt_id = match parser.parse_statement() {
                Some(stmt_id) => stmt_id,
                None => {
                    parser.skip_to_next_statement();
                    continue;
                }
            };

            match parser.lexer.peek().kind {
                TokenKind::SemiColon => {
                    let _ = parser.lexer.next();
                    block.statements.push(stmt_id);
                    continue;
                }
                TokenKind::CloseBrace => {
                    let stmt = parser.ast.get_stmt(stmt_id).unwrap();
                    match stmt.kind {
                        StmtKind::Expression { expr } => {
                            block.result = Some(expr);
                        }
                        _ => {
                            block.statements.push(stmt_id);
                        }
                    }
                    break;
                }
                kind => {
                    println!("unexpected token: {:?}", kind);
                    block.statements.push(stmt_id);
                    println!("consider adding a semicolon");
                    continue;
                }
            }
        }

        let close_brace = parser.lexer.next();
        block.span = Span::merge(open_brace.span, close_brace.span);

        Some(block)
    }

    fn parse_function_definition(parser: &mut Parser) -> Option<ItemId> {
        let fn_keyword = parser.lexer.next();
        let signature = Parser::parse_function_signature(parser)?;

        match parser.lexer.peek().kind {
            TokenKind::OpenBrace => {}
            _ => {
                parser.diagnostics.borrow_mut().push(
                    MissingFunctionBodyDiagnostic {
                        file_id: parser.file_id,
                        span: Span::merge(fn_keyword.span, signature.span),
                    }
                    .into(),
                );

                return None;
            }
        }

        let block = Parser::parse_block(parser)?;
        let span = Span::merge(fn_keyword.span, block.span);
        let item_id = parser.ast.push_item(
            ItemKind::FunctionDefinition(ItemFunctionDefinition {
                export: None,
                signature,
                block,
            }),
            span,
        );

        Some(item_id)
    }

    fn parse_call_expression(
        parser: &mut Parser,
        callee: ExprId,
        _: BindingPower,
    ) -> Option<ExprId> {
        let _ = parser.lexer.next();
        let mut arguments = Vec::new();
        loop {
            let token = parser.lexer.peek();
            if token.kind == TokenKind::CloseParen {
                break;
            }

            let argument = parser.parse_expression(BindingPower::Comma)?;
            arguments.push(argument);

            let token = parser.lexer.peek();
            if token.kind == TokenKind::Comma {
                let _ = parser.lexer.next();
            }
        }

        let close_paren = parser.lexer.next();

        let callee_expr = parser.ast.get_expr(callee).unwrap();
        let expr_id = parser.ast.push_expr(
            ExprKind::Call { callee, arguments },
            Span::merge(callee_expr.span, close_paren.span),
        );
        Some(expr_id)
    }

    fn parse_namespace_member_expression(
        parser: &mut Parser,
        namespace_expr_id: ExprId,
        _: BindingPower,
    ) -> Option<ExprId> {
        let _double_colon = parser.lexer.next_expect(TokenKind::ColonColon).ok()?;
        let member_expr_id = parser.parse_expression(BindingPower::Member)?;
        let member = parser.ast.get_expr(member_expr_id).unwrap();

        let namespace = parser.ast.get_expr(namespace_expr_id).unwrap();
        let expr_id = parser.ast.push_expr(
            ExprKind::NamespaceMember {
                namespace: namespace_expr_id,
                member: member_expr_id,
            },
            Span::merge(namespace.span, member.span),
        );
        Some(expr_id)
    }

    fn parse_enum_item(parser: &mut Parser) -> Option<ItemId> {
        let enum_keyword = parser.lexer.next();

        let name_token = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
        let name_symbol = parser.get_identifier_symbol(name_token.clone())?;

        _ = parser.lexer.next_expect(TokenKind::Colon).ok()?;
        let type_token = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
        let type_symbol = parser.get_identifier_symbol(type_token.clone())?;

        _ = parser.lexer.next_expect(TokenKind::OpenBrace).ok()?;
        let mut variants = Vec::new();
        loop {
            let token = parser.lexer.peek();
            match token.kind {
                TokenKind::CloseBrace => break,
                TokenKind::Comma => {
                    let _ = parser.lexer.next();
                    continue;
                }
                TokenKind::Eof => panic!("unexpected EOF"),
                _ => {}
            }

            let variant_name_token = parser.lexer.next_expect(TokenKind::Identifier).ok()?;
            let variant_name_symbol = parser.get_identifier_symbol(variant_name_token.clone())?;

            _ = parser.lexer.next_expect(TokenKind::Eq).ok()?;
            let value_expr_id = parser.parse_expression(BindingPower::Default)?;

            variants.push(EnumVariant {
                name: Identifier {
                    symbol: variant_name_symbol,
                    span: variant_name_token.span,
                },
                value: value_expr_id,
            });

            let token = parser.lexer.peek();
            if token.kind == TokenKind::Comma {
                let _ = parser.lexer.next();
            }
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
                variants,
            }),
            Span::merge(enum_keyword.span, close_brace.span),
        );
        Some(item_id)
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
