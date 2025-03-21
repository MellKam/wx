use crate::{Token, TokenKind, TokenValue};
use std::{collections::HashMap, iter::Peekable, slice::Iter};
use string_interner::symbol::SymbolU32;

#[derive(Debug, PartialEq)]
pub struct IntExpression {
    pub value: i64,
}

#[derive(Debug, PartialEq)]
pub struct FloatExpression {
    pub value: f64,
}

#[derive(Debug, PartialEq)]
pub struct StringExpression {
    pub value: SymbolU32,
}

#[derive(Debug, PartialEq)]
pub struct IdentifierExpression {
    pub value: SymbolU32,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpression {
    pub operator: Token,
    pub operand: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct AssignmentExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct MemberExpression {
    pub object: Box<Expression>,
    pub property: Box<Expression>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub start: usize,
    pub end: usize,
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

type NudHandler =
    fn(&ExpressionParser, &mut Peekable<Iter<'_, Token>>) -> Result<Expression, String>;
type LedHandler = fn(
    &ExpressionParser,
    &mut Peekable<Iter<'_, Token>>,
    Expression,
    BindingPower,
) -> Result<Expression, String>;

pub struct ExpressionParser {
    nud_lookup: HashMap<TokenKind, NudHandler>,
    led_lookup: HashMap<TokenKind, LedHandler>,
    bp_lookup: HashMap<TokenKind, BindingPower>,
}

impl ExpressionParser {
    pub fn new() -> Self {
        let mut nud_lookup = HashMap::<TokenKind, NudHandler>::new();
        let mut led_lookup = HashMap::<TokenKind, LedHandler>::new();
        let mut bp_lookup = HashMap::<TokenKind, BindingPower>::new();

        nud_lookup.insert(TokenKind::Int, parse_int_expression);
        bp_lookup.insert(TokenKind::Int, BindingPower::Primary);

        nud_lookup.insert(TokenKind::Float, parse_float_expression);
        bp_lookup.insert(TokenKind::Float, BindingPower::Primary);

        nud_lookup.insert(TokenKind::String, parse_string_expression);
        bp_lookup.insert(TokenKind::String, BindingPower::Primary);

        nud_lookup.insert(TokenKind::Identifier, parse_identifier_expression);
        bp_lookup.insert(TokenKind::Identifier, BindingPower::Primary);

        nud_lookup.insert(TokenKind::Dash, parse_unary_expression);
        bp_lookup.insert(TokenKind::Dash, BindingPower::Primary);

        nud_lookup.insert(TokenKind::OpenParen, parse_grouping_expression);
        bp_lookup.insert(TokenKind::OpenParen, BindingPower::Primary);

        led_lookup.insert(TokenKind::Plus, parse_binary_expression);
        bp_lookup.insert(TokenKind::Plus, BindingPower::Additive);

        led_lookup.insert(TokenKind::Dash, parse_binary_expression);
        bp_lookup.insert(TokenKind::Dash, BindingPower::Additive);

        led_lookup.insert(TokenKind::Star, parse_binary_expression);
        bp_lookup.insert(TokenKind::Star, BindingPower::Multiplicative);

        led_lookup.insert(TokenKind::Slash, parse_binary_expression);
        bp_lookup.insert(TokenKind::Slash, BindingPower::Multiplicative);

        led_lookup.insert(TokenKind::Percent, parse_binary_expression);
        bp_lookup.insert(TokenKind::Percent, BindingPower::Multiplicative);

        led_lookup.insert(TokenKind::Equal, parse_assignment_expression);
        bp_lookup.insert(TokenKind::Equal, BindingPower::Assignment);

        led_lookup.insert(TokenKind::EqualEqual, parse_binary_expression);
        bp_lookup.insert(TokenKind::EqualEqual, BindingPower::Relational);

        nud_lookup.insert(TokenKind::Bang, parse_unary_expression);
        bp_lookup.insert(TokenKind::Bang, BindingPower::Primary);

        led_lookup.insert(TokenKind::NotEqual, parse_binary_expression);
        bp_lookup.insert(TokenKind::NotEqual, BindingPower::Relational);

        led_lookup.insert(TokenKind::OpenAngle, parse_binary_expression);
        bp_lookup.insert(TokenKind::OpenAngle, BindingPower::Relational);

        led_lookup.insert(TokenKind::CloseAngle, parse_binary_expression);
        bp_lookup.insert(TokenKind::CloseAngle, BindingPower::Relational);

        led_lookup.insert(TokenKind::OpenAngleEqual, parse_binary_expression);
        bp_lookup.insert(TokenKind::OpenAngleEqual, BindingPower::Relational);

        led_lookup.insert(TokenKind::CloseAngleEqual, parse_binary_expression);
        bp_lookup.insert(TokenKind::CloseAngleEqual, BindingPower::Relational);

        led_lookup.insert(TokenKind::And, parse_binary_expression);
        bp_lookup.insert(TokenKind::And, BindingPower::Logical);

        led_lookup.insert(TokenKind::Or, parse_binary_expression);
        bp_lookup.insert(TokenKind::Or, BindingPower::Logical);

        led_lookup.insert(TokenKind::OpenParen, parse_call_expression);
        bp_lookup.insert(TokenKind::OpenParen, BindingPower::Call);

        led_lookup.insert(TokenKind::Dot, parse_member_expression);
        bp_lookup.insert(TokenKind::Dot, BindingPower::Member);

        Self {
            nud_lookup,
            led_lookup,
            bp_lookup,
        }
    }

    pub fn parse(
        &self,
        tokens: &mut Peekable<Iter<'_, Token>>,
        min_binding_power: BindingPower,
    ) -> Result<Expression, String> {
        let token = tokens.peek().ok_or("no token found")?;
        let nud_handler = self
            .nud_lookup
            .get(&token.kind)
            .ok_or("no nud handler found")?;
        let mut left = nud_handler(&self, tokens)?;

        while let Some(token) = tokens.peek() {
            let led_handler = match self.led_lookup.get(&token.kind) {
                Some(handler) => handler,
                None => break,
            };

            let operator_binding_power = self
                .bp_lookup
                .get(&token.kind)
                .ok_or(format!("no binding power found for {:?}", token.kind))?
                .clone();
            if operator_binding_power <= min_binding_power {
                break;
            }

            left = led_handler(&self, tokens, left, operator_binding_power)?;
        }

        Ok(left)
    }
}

fn parse_int_expression(
    _parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
) -> Result<Expression, String> {
    let token = tokens.next().ok_or("no token found")?;

    if token.kind != TokenKind::Int {
        return Err("expected int token".to_string());
    }

    if let TokenValue::Int(value) = token.value {
        return Ok(Expression {
            kind: ExpressionKind::Int(IntExpression { value }),
            start: token.start,
            end: token.end,
        });
    }

    Err("expected int value".to_string())
}

fn parse_float_expression(
    _parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
) -> Result<Expression, String> {
    let token = tokens.next().ok_or("no token found")?;
    if token.kind != TokenKind::Float {
        return Err("expected float token".to_string());
    }

    if let TokenValue::Float(value) = token.value {
        return Ok(Expression {
            kind: ExpressionKind::Float(FloatExpression { value }),
            start: token.start,
            end: token.end,
        });
    }

    Err("expected float value".to_string())
}

fn parse_identifier_expression(
    _parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
) -> Result<Expression, String> {
    let token = tokens.next().ok_or("No token found")?;
    if token.kind != TokenKind::Identifier {
        return Err("expected identifier token".into());
    }

    if let TokenValue::String(value) = token.value {
        return Ok(Expression {
            kind: ExpressionKind::Identifier(IdentifierExpression { value }),
            start: token.start,
            end: token.end,
        });
    }

    Err("expected string value".into())
}

fn parse_string_expression(
    _parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
) -> Result<Expression, String> {
    let token = tokens.next().ok_or("no token found")?;
    if token.kind != TokenKind::String {
        return Err("expected string token".to_string());
    }

    if let TokenValue::String(value) = token.value {
        return Ok(Expression {
            kind: ExpressionKind::String(StringExpression { value }),
            start: token.start,
            end: token.end,
        });
    }

    Err("expected string value".to_string())
}

fn parse_unary_expression(
    parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
) -> Result<Expression, String> {
    let operator = tokens.next().ok_or("No token found")?;
    let right = parser.parse(tokens, BindingPower::Primary)?;
    let end = right.end;

    Ok(Expression {
        kind: ExpressionKind::Unary(UnaryExpression {
            operator: operator.clone(),
            operand: Box::new(right),
        }),
        start: operator.start,
        end,
    })
}

fn parse_grouping_expression(
    parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
) -> Result<Expression, String> {
    let _ = tokens.next().ok_or("no token found")?;
    let expression = parser.parse(tokens, BindingPower::Default)?;
    match tokens.next() {
        Some(token) => {
            if token.kind != TokenKind::CloseParen {
                return Err("expected closing parenthesis".into());
            }
        }
        None => return Err("expected closing parenthesis".into()),
    };

    Ok(expression)
}

fn parse_binary_expression(
    parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
    left: Expression,
    binding_power: BindingPower,
) -> Result<Expression, String> {
    let operator = tokens.next().ok_or("no token found")?;
    let right = parser.parse(tokens, binding_power)?;
    let start = left.start;
    let end = right.end;

    Ok(Expression {
        kind: ExpressionKind::Binary(BinaryExpression {
            left: Box::new(left),
            operator: operator.clone(),
            right: Box::new(right),
        }),
        start,
        end,
    })
}

fn parse_assignment_expression(
    parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
    left: Expression,
    binding_power: BindingPower,
) -> Result<Expression, String> {
    let _ = tokens.next().expect("expected equal sign token");
    let right = parser.parse(tokens, binding_power)?;
    let start = left.start;
    let end = right.end;

    Ok(Expression {
        kind: ExpressionKind::Assignment(AssignmentExpression {
            left: Box::new(left),
            right: Box::new(right),
        }),
        start,
        end,
    })
}

fn parse_call_expression(
    parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
    left: Expression,
    _binding_power: BindingPower,
) -> Result<Expression, String> {
    let _ = tokens.next().expect("expected open paren token");
    let mut arguments = Vec::new();
    while let Some(token) = tokens.peek() {
        if token.kind == TokenKind::CloseParen {
            break;
        }

        let argument = parser.parse(tokens, BindingPower::Default)?;

        arguments.push(argument);

        if let Some(token) = tokens.peek() {
            if token.kind == TokenKind::Comma {
                let _ = tokens.next().expect("expected comma token");
            }
        }
    }

    let close_paren = tokens.next().expect("expected close paren token");
    let start = left.start;
    let end = close_paren.end;

    Ok(Expression {
        kind: ExpressionKind::Call(CallExpression {
            callee: Box::new(left),
            arguments,
        }),
        start,
        end,
    })
}

fn parse_member_expression(
    parser: &ExpressionParser,
    tokens: &mut Peekable<Iter<'_, Token>>,
    left: Expression,
    _binding_power: BindingPower,
) -> Result<Expression, String> {
    let _ = tokens.next().expect("expected dot token");
    let property = parser.parse(tokens, BindingPower::Primary)?;
    let start = left.start;
    let end = property.end;

    Ok(Expression {
        kind: ExpressionKind::Member(MemberExpression {
            object: Box::new(left),
            property: Box::new(property),
        }),
        start,
        end,
    })
}

#[cfg(test)]
mod tests {
    use string_interner::{StringInterner, backend::StringBackend};

    use super::*;

    #[test]
    fn parse_int_expression_works() {
        let input = vec![Token {
            kind: TokenKind::Int,
            value: TokenValue::Int(42),
            start: 0,
            end: 2,
        }];
        let mut tokens = input.iter().peekable();
        let parser = ExpressionParser::new();
        let result = parser.parse(&mut tokens, BindingPower::Default);
        assert_eq!(
            result,
            Ok(Expression {
                kind: ExpressionKind::Int(IntExpression { value: 42 }),
                start: 0,
                end: 2
            })
        );
    }

    #[test]
    fn parse_float_expression_works() {
        let input = vec![Token {
            kind: TokenKind::Float,
            value: TokenValue::Float(42.0),
            start: 0,
            end: 4,
        }];
        let mut tokens = input.iter().peekable();
        let parser = ExpressionParser::new();
        let result = parser.parse(&mut tokens, BindingPower::Default);
        assert_eq!(
            result,
            Ok(Expression {
                kind: ExpressionKind::Float(FloatExpression { value: 42.0 }),
                start: 0,
                end: 4
            })
        );
    }

    #[test]
    fn parse_identifier_expression_works() {
        let mut interner = StringInterner::<StringBackend>::new();
        let symbol = interner.get_or_intern("foo");
        let input = vec![Token {
            kind: TokenKind::Identifier,
            value: TokenValue::String(symbol),
            start: 0,
            end: 3,
        }];
        let mut tokens = input.iter().peekable();
        let parser = ExpressionParser::new();
        let result = parser.parse(&mut tokens, BindingPower::Default);
        assert_eq!(
            result,
            Ok(Expression {
                kind: ExpressionKind::Identifier(IdentifierExpression { value: symbol }),
                start: 0,
                end: 3
            })
        );
    }

    #[test]
    fn parse_unary_expression_works() {
        let input = vec![
            Token {
                kind: TokenKind::Int,
                value: TokenValue::Int(2),
                start: 0,
                end: 1,
            },
            Token {
                kind: TokenKind::Plus,
                value: TokenValue::None,
                start: 1,
                end: 2,
            },
            Token {
                kind: TokenKind::Int,
                value: TokenValue::Int(2),
                start: 2,
                end: 3,
            },
        ];
        let mut tokens = input.iter().peekable();
        let parser = ExpressionParser::new();
        let result = parser.parse(&mut tokens, BindingPower::Default);
        assert_eq!(
            result,
            Ok(Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    left: Box::new(Expression {
                        kind: ExpressionKind::Int(IntExpression { value: 2 }),
                        start: 0,
                        end: 1
                    }),
                    operator: Token {
                        kind: TokenKind::Plus,
                        value: TokenValue::None,
                        start: 1,
                        end: 2
                    },
                    right: Box::new(Expression {
                        kind: ExpressionKind::Int(IntExpression { value: 2 }),
                        start: 2,
                        end: 3
                    }),
                }),
                start: 0,
                end: 3
            })
        );
    }
}
