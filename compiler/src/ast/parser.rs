use std::cell::RefCell;
use std::rc::Rc;

use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;
use string_interner::{StringInterner, Symbol};

use super::diagnostics::{Diagnostic, DiagnosticKind, DiagnosticsBag};
use super::lexer::{BufferedLexer, Lexer, Radix, Token, TokenKind};
use super::unescape::unescape;
use super::{
    Ast, BinaryOperator, ExprId, ExprKind, Item, ItemId, ItemKind, StmtId, StmtKind, UnaryOperator,
};

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

#[derive(Debug, Clone)]
#[repr(u8)]
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

impl Keyword {
    pub fn from_symbol(symbol: SymbolU32) -> Option<Self> {
        let index = symbol.to_usize();
        if index < KEYWORDS.len() {
            Some(unsafe { std::mem::transmute(index as u8) })
        } else {
            None
        }
    }

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
                Some(stmt_id)
            }
            None => {
                let expr_id = self.parse_expression(BindingPower::Default)?;
                let stmt_id = self
                    .ast
                    .add_statement(StmtKind::Expression { expr: expr_id });
                let _ = self.lexer.next_expect(TokenKind::SemiColon)?;
                return Some(stmt_id);
            }
        }
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
            None => {
                self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
                    message: format!("Unexpected token `{:#?}`", token.kind),
                    span: token.span.clone(),
                    kind: DiagnosticKind::Error,
                });
                return None;
            }
        };
        let mut left = nud_handler(self).unwrap();

        loop {
            let token = self.lexer.peek();
            if token.kind == TokenKind::Eof {
                break;
            }

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
            TokenKind::Float { .. } => Some((parse_float_expression, BindingPower::Primary)),
            TokenKind::String { .. } => Some((parse_string_expression, BindingPower::Primary)),
            TokenKind::Char { .. } => Some((parse_string_expression, BindingPower::Primary)),
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
            | TokenKind::NotEq
            | TokenKind::Less
            | TokenKind::LessEq
            | TokenKind::Greater
            | TokenKind::GreaterEq => Some((parse_binary_expression, BindingPower::Relational)),
            // TokenKind::VbarVbar | TokenKind::AmperAmper => {
            //     Some((parse_binary_expression, BindingPower::Logical))
            // }
            TokenKind::Eq => Some((parse_assignment_expression, BindingPower::Assignment)),
            TokenKind::OpenParen => Some((parse_call_expression, BindingPower::Call)),
            TokenKind::Dot => Some((parse_member_expression, BindingPower::Member)),
            _ => None,
        }
    }

    pub fn stmt_lookup(&mut self) -> Option<StmtHandler> {
        let token = self.lexer.peek();
        match token.kind {
            TokenKind::OpenBrace => return Some(parse_block_statement),
            _ => {}
        }

        match get_keyword(self, token.clone()) {
            Some(Keyword::Const) => Some(parse_variable_definition),
            Some(Keyword::Mut) => Some(parse_variable_definition),
            Some(Keyword::Return) => Some(parse_return_statement),
            _ => None,
        }
    }

    pub fn item_lookup(&mut self) -> Option<ItemHandler> {
        let token = self.lexer.peek();
        match get_keyword(self, token) {
            Some(Keyword::Fn) => Some(parse_function_item),
            _ => None,
        }
    }
}

fn parse_int_expression(parser: &mut Parser) -> Option<ExprId> {
    let token = parser.lexer.next();
    let content = &parser.source[(token.span.start as usize)..(token.span.end as usize)];

    let radix = match token.kind {
        TokenKind::Int { radix } => radix,
        _ => unreachable!(),
    };
    let value = match radix {
        Radix::Decimal => content.parse::<i64>().ok(),
        _ => todo!("handle other radices"),
    };

    let expr = match value {
        Some(value) => ExprKind::Int { value },
        None => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "Invalid integer literal".to_string(),
                    span: token.span.clone(),
                    kind: DiagnosticKind::Error,
                });

            ExprKind::Int { value: 0 }
        }
    };

    let expr_id = parser.ast.add_expression(expr);
    Some(expr_id)
}

fn parse_float_expression(parser: &mut Parser) -> Option<ExprId> {
    let token = parser.lexer.next();
    let content = &parser.source[(token.span.start as usize)..(token.span.end as usize)];

    let value = content.parse::<f64>().ok();

    let expr = match value {
        Some(value) => ExprKind::Float { value },
        None => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "Invalid float literal".to_string(),
                    span: token.span.clone(),
                    kind: DiagnosticKind::Error,
                });

            ExprKind::Float { value: 0.0 }
        }
    };

    let expr_id = parser.ast.add_expression(expr);
    Some(expr_id)
}

fn parse_identifier_expression(parser: &mut Parser) -> Option<ExprId> {
    let token = parser.lexer.next();
    let content = &parser.source[(token.span.start as usize)..(token.span.end as usize)];

    let symbol = parser.interner.get_or_intern(content);

    let expr_id = parser.ast.add_expression(ExprKind::Identifier { symbol });
    Some(expr_id)
}

fn parse_string_expression(parser: &mut Parser) -> Option<ExprId> {
    let token = parser.lexer.next();
    let content = &parser.source[(token.span.start as usize) + 1..(token.span.end as usize) - 1];

    let unescaped = match unescape(content) {
        Ok(str) => str,
        Err(_) => {
            parser
                .diagnostics
                .borrow_mut()
                .diagnostics
                .push(Diagnostic {
                    message: "Invalid escaped string literal".to_string(),
                    span: token.span.clone(),
                    kind: DiagnosticKind::Error,
                });

            content.to_string()
        }
    };
    let symbol = parser.interner.get_or_intern(unescaped);

    let expr_id = parser.ast.add_expression(ExprKind::String { symbol });
    Some(expr_id)
}

fn parse_grouping_expression(parser: &mut Parser) -> Option<ExprId> {
    let _ = parser.lexer.next();
    let inner_expr_id = parser.parse_expression(BindingPower::Default)?;
    let _ = parser.lexer.next_expect(TokenKind::CloseParen)?;
    // TODO: we can probably try to recover from the case when there is no closing
    // parenthesis just need to use peek instaed of next, to keep the next token

    Some(inner_expr_id)
}

fn parse_unary_expression(parser: &mut Parser) -> Option<ExprId> {
    let operator = parser.lexer.next();
    let operator_kind = match UnaryOperator::from_token(operator.kind) {
        Some(operator) => operator,
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

            // TODO: maybe we can recover from this as well
            // what if we ignore the operator and parse only operand
            return None;
        }
    };

    let operand = parser.parse_expression(BindingPower::Unary)?;
    let expr_id = parser.ast.add_expression(ExprKind::Unary {
        operator: operator_kind,
        operand,
    });
    Some(expr_id)
}

fn parse_binary_expression(parser: &mut Parser, left: ExprId, bp: BindingPower) -> Option<ExprId> {
    let operator = parser.lexer.next();
    let operator_kind = match BinaryOperator::from_token(operator.kind.clone()) {
        Some(operator) => operator,
        None => unreachable!(),
    };

    let right = parser.parse_expression(bp)?;
    let expr_id = parser.ast.add_expression(ExprKind::Binary {
        left,
        right,
        operator: operator_kind,
    });
    Some(expr_id)
}

fn parse_assignment_expression(
    parser: &mut Parser,
    left: ExprId,
    bp: BindingPower,
) -> Option<ExprId> {
    let _ = parser.lexer.next();

    let right = parser.parse_expression(bp)?;
    let expr_id = parser
        .ast
        .add_expression(ExprKind::Assignment { left, right });
    Some(expr_id)
}

fn parse_call_expression(parser: &mut Parser, callee: ExprId, _: BindingPower) -> Option<ExprId> {
    let _ = parser.lexer.next();
    let mut arguments = Vec::new();
    loop {
        let token = parser.lexer.peek();
        if token.kind == TokenKind::CloseParen {
            let _ = parser.lexer.next();
            break;
        }

        let argument = parser.parse_expression(BindingPower::Comma)?;
        arguments.push(argument);

        let token = parser.lexer.peek();
        if token.kind == TokenKind::Comma {
            let _ = parser.lexer.next();
        }
    }

    let expr_id = parser
        .ast
        .add_expression(ExprKind::Call { callee, arguments });
    Some(expr_id)
}

fn parse_member_expression(parser: &mut Parser, object: ExprId, _: BindingPower) -> Option<ExprId> {
    let _ = parser.lexer.next();
    let property = parser.lexer.next_expect(TokenKind::Identifier)?;
    let content = &parser.source[(property.span.start as usize)..(property.span.end as usize)];
    let expr_id = parser.ast.add_expression(ExprKind::Member {
        object,
        property: parser.interner.get_or_intern(content),
    });
    Some(expr_id)
}

fn get_keyword(parser: &mut Parser, token: Token) -> Option<Keyword> {
    let content = match token.kind {
        TokenKind::Identifier => {
            &parser.source[(token.span.start as usize)..(token.span.end as usize)]
        }
        _ => return None,
    };
    let symbol = parser.interner.get(content)?;
    Keyword::from_symbol(symbol)
}

fn parse_variable_definition(parser: &mut Parser) -> Option<StmtId> {
    let _ = parser.lexer.next();
    let name = parser.lexer.next_expect(TokenKind::Identifier)?;
    let name_symbol = get_symbol(parser, name)?;
    let _ = parser.lexer.next_expect(TokenKind::Colon)?;
    let type_token = parser.lexer.next_expect(TokenKind::Identifier)?;
    let type_symbol = get_symbol(parser, type_token)?;

    let _ = parser.lexer.next_expect(TokenKind::Eq)?;
    let value = parser.parse_expression(BindingPower::Default)?;
    let _ = parser.lexer.next_expect(TokenKind::SemiColon)?;

    let stmt_id = parser.ast.add_statement(StmtKind::ConstDefinition {
        name: name_symbol,
        ty: type_symbol,
        value,
    });
    Some(stmt_id)
}

fn parse_return_statement(parser: &mut Parser) -> Option<StmtId> {
    let _ = parser.lexer.next();
    let value = parser.parse_expression(BindingPower::Default)?;
    let _ = parser.lexer.next_expect(TokenKind::SemiColon)?;

    let stmt_id = parser.ast.add_statement(StmtKind::Return { value });
    Some(stmt_id)
}

fn parse_block_statement(parser: &mut Parser) -> Option<StmtId> {
    let _ = parser.lexer.next();
    let mut statements = Vec::new();

    loop {
        let token = parser.lexer.peek();
        if token.kind == TokenKind::CloseBrace {
            let _ = parser.lexer.next();
            break;
        }

        let stmt_id = parser.parse_statement()?;
        statements.push(stmt_id);
    }

    let stmt_id = parser.ast.add_statement(StmtKind::Block { statements });
    Some(stmt_id)
}

fn get_symbol(parser: &mut Parser, token: Token) -> Option<SymbolU32> {
    match token.kind {
        TokenKind::Identifier => {
            let content = &parser.source[(token.span.start as usize)..(token.span.end as usize)];
            Some(parser.interner.get_or_intern(content))
        }
        _ => None,
    }
}

fn parse_function_item(parser: &mut Parser) -> Option<ItemId> {
    let _ = parser.lexer.next();
    let name = parser.lexer.next_expect(TokenKind::Identifier)?;
    let name_content = &parser.source[(name.span.start as usize)..(name.span.end as usize)];
    let name_symbol = parser.interner.get_or_intern(name_content);

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

        params.push((param_name_symbol, param_type_symbol));

        let token = parser.lexer.peek();
        if token.kind == TokenKind::Comma {
            let _ = parser.lexer.next();
        }
    }
    let _ = parser.lexer.next_expect(TokenKind::CloseParen)?;

    let body_stmt_id = parse_block_statement(parser)?;

    let item_id = parser.ast.add_item(ItemKind::Function {
        name: name_symbol,
        params,
        body: body_stmt_id,
    });
    Some(item_id)
}
