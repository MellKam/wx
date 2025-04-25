use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;
use string_interner::{StringInterner, Symbol};

use super::diagnostics::{Diagnostic, DiagnosticKind, DiagnosticsBag};
use super::lexer::{BufferedLexer, Lexer, Token, TokenKind};
use super::span::TextSpan;
use super::unescape::unescape;
use super::{
    Ast, BinaryOperator, BindingType, ExprId, ExprKind, FunctionParam, FunctionSignature, Item,
    ItemId, ItemKind, SpanMultilinePrinter, SpanPrinter, StmtId, StmtKind, UnaryOperator,
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
    Const,
    Mut,
    Fn,
    Loop,
    Break,
    Continue,
    Underscore,
    Match,
    Return,
}

const KEYWORDS: [&'static str; 9] = [
    "const", "mut", "fn", "loop", "break", "continue", "_", "match", "return",
];

type Interner = StringInterner<StringBackend<SymbolU32>>;
pub fn create_interner_with_keywords() -> Interner {
    let mut interner = Interner::new();
    for keyword in KEYWORDS {
        interner.get_or_intern(keyword);
    }
    interner
}

impl TryFrom<SymbolU32> for Keyword {
    type Error = ();

    fn try_from(symbol: SymbolU32) -> Result<Self, Self::Error> {
        let index = symbol.to_usize();
        if index < KEYWORDS.len() {
            Ok(unsafe { std::mem::transmute(index as u8) })
        } else {
            Err(())
        }
    }
}

impl Keyword {
    pub fn to_symbol(&self) -> SymbolU32 {
        SymbolU32::try_from_usize(self.clone() as usize).unwrap()
    }
}

type NudHandler = fn(parser: &mut Parser) -> Option<ExprId>;
type LedHandler = fn(parser: &mut Parser, left: ExprId, bp: BindingPower) -> Option<ExprId>;
type StmtHandler = fn(parser: &mut Parser) -> Option<StmtId>;
type ItemHandler = fn(parser: &mut Parser) -> Option<ItemId>;

pub struct Parser<'source> {
    source: &'source str,
    lexer: BufferedLexer<'source>,
    span_printer: SpanMultilinePrinter<io::Stdout>,
    pub interner: StringInterner<StringBackend>,
    pub diagnostics: Rc<RefCell<DiagnosticsBag>>,
    pub ast: Ast,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Self {
        let diagnostics = Rc::new(RefCell::new(DiagnosticsBag::new()));
        let lexer = BufferedLexer::new(Lexer::new(source), diagnostics.clone());

        Self {
            source,
            lexer,
            interner: create_interner_with_keywords(),
            span_printer: SpanMultilinePrinter::default(),
            diagnostics,
            ast: Ast::new(),
        }
    }

    pub fn parse(&mut self) {
        loop {
            let token = self.lexer.peek();
            if token.kind == TokenKind::Eof {
                break;
            }

            match self.parse_item() {
                Some(_) => {}
                None => break,
            };
        }
    }

    pub fn parse_statement(&mut self) -> Option<StmtId> {
        match self.stmt_lookup() {
            Some(stmt_handler) => {
                let stmt_id = stmt_handler(self)?;

                match self.lexer.peek().kind {
                    TokenKind::SemiColon => {
                        let _ = self.lexer.next();
                    }
                    _ => {
                        self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
                            message: "expected `;` after statement".to_string(),
                            span: self.ast.get_stmt(stmt_id).unwrap().span,
                            kind: DiagnosticKind::Error,
                        });
                    }
                }
                return Some(stmt_id);
            }
            None => {}
        }

        let first_token = self.lexer.peek();
        if first_token.kind == TokenKind::SemiColon {
            let _ = self.lexer.next();
            return None;
        }

        match parse_expression_statement(self) {
            Some(stmt_id) => return Some(stmt_id),
            None => {}
        };

        loop {
            match self.lexer.peek().kind {
                TokenKind::SemiColon | TokenKind::CloseBrace | TokenKind::Eof => break,
                _ => {
                    let _ = self.lexer.next();
                    continue;
                }
            }
        }
        self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
            message: "unable to parse statement".to_string(),
            span: TextSpan {
                start: first_token.span.start,
                end: self.lexer.peek().span.end,
            },
            kind: DiagnosticKind::Error,
        });
        return None;
    }

    pub fn parse_item(&mut self) -> Option<ItemId> {
        let item_handler = self.item_lookup()?;
        let item_id = item_handler(self)?;
        Some(item_id)
    }

    pub fn parse_expression(&mut self, min_binding_power: BindingPower) -> Option<ExprId> {
        let token = self.lexer.peek();
        let nud_handler = match Parser::nud_lookup(token.kind.clone()) {
            Some((nud_handler, _)) => nud_handler,
            None => return None,
        };
        let mut left = nud_handler(self).unwrap();

        loop {
            let token = self.lexer.peek();

            let (led_handler, operator_binding_power) = match Parser::led_lookup(token.kind) {
                Some((_, bp)) if bp < min_binding_power => break,
                Some((handler, bp)) => (handler, bp),
                None => break,
            };

            left = led_handler(self, left, operator_binding_power)?;
        }
        Some(left)
    }

    fn nud_lookup(token: TokenKind) -> Option<(NudHandler, BindingPower)> {
        match token {
            TokenKind::Int { .. } => Some((parse_int_expression, BindingPower::Primary)),
            // TokenKind::Float { .. } => Some((parse_float_expression, BindingPower::Primary)),
            // TokenKind::String { .. } => Some((parse_string_expression, BindingPower::Primary)),
            // TokenKind::Char { .. } => Some((parse_string_expression, BindingPower::Primary)),
            TokenKind::Identifier => Some((parse_identifier_expression, BindingPower::Primary)),
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
            TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::OpenAngle
            | TokenKind::LessEq
            | TokenKind::CloseAngle
            | TokenKind::GreaterEq => Some((parse_binary_expression, BindingPower::Relational)),
            // TokenKind::VbarVbar | TokenKind::AmperAmper => {
            //     Some((parse_binary_expression, BindingPower::Logical))
            // }
            TokenKind::Eq => Some((parse_binary_expression, BindingPower::Assignment)),
            // TokenKind::OpenParen => Some((parse_call_expression, BindingPower::Call)),
            // TokenKind::Dot => Some((parse_member_expression, BindingPower::Member)),
            _ => None,
        }
    }

    pub fn skip_statement(&mut self) {
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

    pub fn stmt_lookup(&mut self) -> Option<StmtHandler> {
        let token = self.lexer.peek();
        let symbol = match token.kind {
            // TokenKind::OpenBrace => return Some(parse_block_statement),
            TokenKind::Identifier => get_symbol(self, token.clone())?,
            _ => return None,
        };

        match Keyword::try_from(symbol) {
            Ok(Keyword::Const) => Some(parse_variable_definition),
            Ok(Keyword::Mut) => Some(parse_variable_definition),
            Ok(Keyword::Return) => Some(parse_return_statement),
            _ => return None,
        }
    }

    pub fn item_lookup(&mut self) -> Option<ItemHandler> {
        let token = self.lexer.peek();
        let symbol = get_symbol(self, token)?;
        match Keyword::try_from(symbol) {
            Ok(Keyword::Fn) => Some(parse_function_definition),
            _ => None,
        }
    }
}

fn parse_int_expression(parser: &mut Parser) -> Option<ExprId> {
    let token = parser.lexer.next();
    let text = token.span.get_text(parser.source)?;

    let value = match token.kind {
        TokenKind::Int => text.parse::<i64>().ok(),
        _ => unreachable!(),
    };

    let expr = match value {
        Some(value) => ExprKind::Int { value },
        None => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "invalid integer literal".to_string(),
                    span: token.span,
                    kind: DiagnosticKind::Error,
                });

            ExprKind::Int { value: 0 }
        }
    };

    let expr_id = parser.ast.push_expr(expr, token.span);
    Some(expr_id)
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

fn parse_identifier_expression(parser: &mut Parser) -> Option<ExprId> {
    let token = parser.lexer.next();
    let text = token.span.get_text(parser.source)?;
    let symbol = parser.interner.get_or_intern(text);

    let expr_id = parser
        .ast
        .push_expr(ExprKind::Identifier { symbol }, token.span);
    Some(expr_id)
}

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

fn parse_grouping_expression(parser: &mut Parser) -> Option<ExprId> {
    let _ = parser.lexer.next();
    let inner_expr_id = parser.parse_expression(BindingPower::Default)?;

    match parser.lexer.peek() {
        Token {
            kind: TokenKind::CloseParen,
            ..
        } => {
            let _ = parser.lexer.next();
            Some(inner_expr_id)
        }
        token => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "expected closing parenthesis ')'".to_string(),
                    span: token.span,
                    kind: DiagnosticKind::Error,
                });
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
        TextSpan::combine(operator.span, operand.span),
    );
    Some(expr_id)
}

fn parse_binary_expression(parser: &mut Parser, left: ExprId, bp: BindingPower) -> Option<ExprId> {
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
        TextSpan::combine(left_expr.span, right_expr.span),
    );
    Some(expr_id)
}

// fn parse_call_expression(parser: &mut Parser, callee: ExprId, _:
// BindingPower) -> Option<ExprId> {     let _ = parser.lexer.next();
//     let mut arguments = Vec::new();
//     loop {
//         let token = parser.lexer.peek();
//         if token.kind == TokenKind::CloseParen {
//             let _ = parser.lexer.next();
//             break;
//         }

//         let argument = parser.parse_expression(BindingPower::Comma)?;
//         arguments.push(argument);

//         let token = parser.lexer.peek();
//         if token.kind == TokenKind::Comma {
//             let _ = parser.lexer.next();
//         }
//     }

//     let expr_id = parser
//         .ast
//         .add_expression(ExprKind::Call { callee, arguments });
//     Some(expr_id)
// }

// fn parse_member_expression(parser: &mut Parser, object: ExprId, _:
// BindingPower) -> Option<ExprId> {     let _ = parser.lexer.next();
//     let property = parser.lexer.next_expect(TokenKind::Identifier)?;
//     let content = property.span.get_content(parser.source)?;
//     let expr_id = parser.ast.add_expression(ExprKind::Member {
//         object,
//         property: parser.interner.get_or_intern(content),
//     });
//     Some(expr_id)
// }

fn parse_expression_statement(parser: &mut Parser) -> Option<StmtId> {
    let expr_id = match parser.parse_expression(BindingPower::Default) {
        Some(expr_id) => expr_id,
        None => return None,
    };

    match parser.lexer.peek() {
        Token {
            kind: TokenKind::SemiColon,
            ..
        } => {
            let _ = parser.lexer.next();
            let expr = parser.ast.get_expr(expr_id).unwrap();
            let stmt_id = parser
                .ast
                .push_stmt(StmtKind::Expression { expr: expr_id }, expr.span);

            Some(stmt_id)
        }
        Token {
            kind: TokenKind::CloseBrace,
            ..
        } => {
            let expr = parser.ast.get_expr(expr_id).unwrap();
            let stmt_id = parser
                .ast
                .push_stmt(StmtKind::Expression { expr: expr_id }, expr.span);

            Some(stmt_id)
        }
        _ => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "expected `;` after expression".to_string(),
                    span: parser.ast.get_expr(expr_id).unwrap().span,
                    kind: DiagnosticKind::Error,
                });

            let expr = parser.ast.get_expr(expr_id).unwrap();
            let stmt_id = parser
                .ast
                .push_stmt(StmtKind::Expression { expr: expr_id }, expr.span);

            Some(stmt_id)
        }
    }
}

fn parse_variable_definition(parser: &mut Parser) -> Option<StmtId> {
    let mutability_type = parser.lexer.next();
    let binding_type = match Keyword::try_from(get_symbol(parser, mutability_type.clone())?) {
        Ok(Keyword::Const) => BindingType::Const,
        Ok(Keyword::Mut) => BindingType::Mutable,
        _ => unreachable!(),
    };

    let name = parser.lexer.next_expect(TokenKind::Identifier)?;
    let name_symbol = get_symbol(parser, name)?;

    let _ = parser.lexer.next_expect(TokenKind::Colon)?;
    let type_token = parser.lexer.next_expect(TokenKind::Identifier)?;
    let type_symbol = get_symbol(parser, type_token)?;

    let _ = parser.lexer.next_expect(TokenKind::Eq)?;
    let value = parser.parse_expression(BindingPower::Default)?;
    let value_expr = parser.ast.get_expr(value).unwrap();

    let stmt_id = parser.ast.push_stmt(
        match binding_type {
            BindingType::Const => StmtKind::ConstDefinition {
                name: name_symbol,
                ty: type_symbol,
                value,
            },
            BindingType::Mutable => StmtKind::MutableDefinition {
                name: name_symbol,
                ty: type_symbol,
                value,
            },
        },
        TextSpan::combine(mutability_type.span, value_expr.span),
    );
    Some(stmt_id)
}

fn parse_return_statement(parser: &mut Parser) -> Option<StmtId> {
    let return_keyword = parser.lexer.next();
    let value = parser.parse_expression(BindingPower::Default)?;
    let value_expr = parser.ast.get_expr(value).unwrap();

    let stmt_id = parser.ast.push_stmt(
        StmtKind::Return { value },
        TextSpan::combine(return_keyword.span, value_expr.span),
    );
    Some(stmt_id)
}

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

fn get_symbol(parser: &mut Parser, token: Token) -> Option<SymbolU32> {
    match token.kind {
        TokenKind::Identifier => {
            let text = token.span.get_text(parser.source)?;
            Some(parser.interner.get_or_intern(text))
        }
        _ => None,
    }
}

fn parse_function_signature(parser: &mut Parser) -> Option<FunctionSignature> {
    let name_token = parser.lexer.next_expect(TokenKind::Identifier)?;
    let name_symbol = parser
        .interner
        .get_or_intern(name_token.span.get_text(parser.source)?);

    let _ = parser.lexer.next_expect(TokenKind::OpenParen)?;
    let mut params = Vec::new();
    loop {
        let token = parser.lexer.peek();
        if token.kind == TokenKind::CloseParen {
            break;
        }

        let param_name = parser.lexer.next_expect(TokenKind::Identifier)?;
        let param_name_symbol = get_symbol(parser, param_name)?;
        let _ = parser.lexer.next_expect(TokenKind::Colon)?;
        let param_type = parser.lexer.next_expect(TokenKind::Identifier)?;
        let param_type_symbol = get_symbol(parser, param_type)?;

        params.push(FunctionParam {
            id: param_name_symbol,
            ty: param_type_symbol,
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
            let return_type_token = parser.lexer.next_expect(TokenKind::Identifier)?;
            let return_type_symbol = get_symbol(parser, return_type_token.clone())?;

            Some(FunctionSignature {
                id: name_symbol,
                params,
                result: Some(return_type_symbol),
                span: TextSpan::combine(name_token.span, return_type_token.span),
            })
        }
        _ => Some(FunctionSignature {
            id: name_symbol,
            params,
            result: None,
            span: TextSpan::combine(name_token.span, close_paren.span),
        }),
    }
}

fn parse_function_declaration(parser: &mut Parser) -> Option<ItemId> {
    let fn_keyword = parser.lexer.next();
    let signature = parse_function_signature(parser)?;

    let item_span = TextSpan::combine(fn_keyword.span, signature.span);
    let item_id = parser
        .ast
        .push_item(ItemKind::FunctionDeclaration { signature }, item_span);
    Some(item_id)
}

fn parse_function_definition(parser: &mut Parser) -> Option<ItemId> {
    let fn_keyword = parser.lexer.next();
    let signature = parse_function_signature(parser)?;

    match parser.lexer.peek().kind {
        TokenKind::OpenBrace => {
            let _ = parser.lexer.next();
        }
        _ => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "Expected `{` after function signature".to_string(),
                    span: TextSpan::combine(fn_keyword.span, signature.span),
                    kind: DiagnosticKind::Error,
                });

            return None;
        }
    }

    let mut body = Vec::new();
    loop {
        let token = parser.lexer.peek();
        if token.kind == TokenKind::CloseBrace {
            break;
        }

        let stmt_id = match parser.parse_statement() {
            Some(stmt_id) => stmt_id,
            None => {
                parser.skip_statement();
                continue;
            }
        };
        body.push(stmt_id);
    }

    let close_brace = parser.lexer.next();
    let item_id = parser.ast.push_item(
        ItemKind::FunctionDefinition { signature, body },
        TextSpan::combine(fn_keyword.span, close_brace.span),
    );

    Some(item_id)
}
