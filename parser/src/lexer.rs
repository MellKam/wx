use crate::unescape::{UnescapeError, unescape};
use regex::{self, Regex};
use std::{
    self,
    num::{ParseFloatError, ParseIntError},
    str::ParseBoolError,
};
use string_interner::{StringInterner, backend::StringBackend, symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Int,
    Float,
    Bool,
    String,
    LetKeyword,
    FnKeyword,
    ReturnKeyword,
    ExportKeyword,
    ImportKeyword,
    Identifier,
    /// +
    Plus,
    /// -
    Dash,
    /// /
    Slash,
    /// *
    Star,
    /// %
    Percent,
    /// =
    Equal,
    /// ==
    EqualEqual,
    /// !
    Bang,
    /// !=
    NotEqual,
    /// <
    OpenAngle,
    /// <=
    OpenAngleEqual,
    /// >
    CloseAngle,
    /// >=
    CloseAngleEqual,
    /// &&
    And,
    /// ||
    Or,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// {
    OpenCurely,
    /// }
    CloseCurely,
    /// ;
    Semicolon,
    /// :
    Colon,
    /// ,
    Comma,
    /// .
    Dot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    None,
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(symbol::SymbolU32),
}

#[derive(Debug)]
enum TokenValueError {
    Int(ParseIntError),
    Float(ParseFloatError),
    Bool(ParseBoolError),
    String(UnescapeError),
}

impl TokenValue {
    fn from_kind(
        interner: &mut StringInterner<StringBackend>,
        kind: TokenKind,
        value: &str,
    ) -> Result<TokenValue, TokenValueError> {
        match kind {
            TokenKind::Int => match value.parse::<i64>() {
                Ok(value) => Ok(TokenValue::Int(value)),
                Err(e) => Err(TokenValueError::Int(e)),
            },
            TokenKind::Float => match value.parse::<f64>() {
                Ok(value) => Ok(TokenValue::Float(value)),
                Err(e) => Err(TokenValueError::Float(e)),
            },
            TokenKind::Bool => match value.parse::<bool>() {
                Ok(value) => Ok(TokenValue::Boolean(value)),
                Err(e) => Err(TokenValueError::Bool(e)),
            },
            TokenKind::String => match unescape(&value[1..value.len() - 1]) {
                Ok(value) => {
                    let symbol = interner.get_or_intern(value);
                    Ok(TokenValue::String(symbol))
                }
                Err(e) => Err(TokenValueError::String(e)),
            },
            TokenKind::Identifier => {
                let symbol = interner.get_or_intern(value.to_string());
                Ok(TokenValue::String(symbol))
            }
            _ => Ok(TokenValue::None),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: TokenValue,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq)]
pub struct LexerError {
    pub position: usize,
}

impl std::error::Error for LexerError {}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "Unrecognized token at position {}", self.position);
    }
}

pub struct Lexer {
    whitespace: regex::Regex,
    patterns: Vec<(regex::Regex, TokenKind)>,
}

impl Lexer {
    pub fn new() -> Lexer {
        let patterns = vec![
            (Regex::new(r"^\d+\.\d+").unwrap(), TokenKind::Float),
            (Regex::new(r"^\d+").unwrap(), TokenKind::Int),
            (Regex::new(r"^true|false").unwrap(), TokenKind::Bool),
            (Regex::new(r"^let").unwrap(), TokenKind::LetKeyword),
            (Regex::new(r"^fn").unwrap(), TokenKind::FnKeyword),
            (Regex::new(r"^return").unwrap(), TokenKind::ReturnKeyword),
            (Regex::new(r"^export").unwrap(), TokenKind::ExportKeyword),
            (Regex::new(r"^import").unwrap(), TokenKind::ImportKeyword),
            (
                Regex::new(r#"^"([^\\"]|\\.)*""#).unwrap(),
                TokenKind::String,
            ),
            (
                Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap(),
                TokenKind::Identifier,
            ),
            (Regex::new(r"^==").unwrap(), TokenKind::EqualEqual),
            (Regex::new(r"^!").unwrap(), TokenKind::Bang),
            (Regex::new(r"^!=").unwrap(), TokenKind::NotEqual),
            (Regex::new(r"^<=").unwrap(), TokenKind::OpenAngleEqual),
            (Regex::new(r"^<").unwrap(), TokenKind::OpenAngle),
            (Regex::new(r"^>=").unwrap(), TokenKind::CloseAngleEqual),
            (Regex::new(r"^>").unwrap(), TokenKind::CloseAngle),
            (Regex::new(r"^&&").unwrap(), TokenKind::And),
            (Regex::new(r"^\|\|").unwrap(), TokenKind::Or),
            (Regex::new(r"^\(").unwrap(), TokenKind::OpenParen),
            (Regex::new(r"^\)").unwrap(), TokenKind::CloseParen),
            (Regex::new(r"^\{").unwrap(), TokenKind::OpenCurely),
            (Regex::new(r"^\}").unwrap(), TokenKind::CloseCurely),
            (Regex::new(r"^;").unwrap(), TokenKind::Semicolon),
            (Regex::new(r"^:").unwrap(), TokenKind::Colon),
            (Regex::new(r"^,").unwrap(), TokenKind::Comma),
            (Regex::new(r"^=").unwrap(), TokenKind::Equal),
            (Regex::new(r"^\+").unwrap(), TokenKind::Plus),
            (Regex::new(r"^-").unwrap(), TokenKind::Dash),
            (Regex::new(r"^/").unwrap(), TokenKind::Slash),
            (Regex::new(r"^\*").unwrap(), TokenKind::Star),
            (Regex::new(r"^%").unwrap(), TokenKind::Percent),
            (Regex::new(r"^\.").unwrap(), TokenKind::Dot),
        ];

        return Lexer {
            patterns,
            whitespace: Regex::new(r"^\s+").unwrap(),
        };
    }

    pub fn tokenize(
        self,
        interner: &mut StringInterner<StringBackend>,
        source: String,
    ) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::<Token>::new();
        let mut position = 0;

        while position < source.len() {
            let mut found_match = false;

            if let Some(whitespace_match) = self.whitespace.find(&source[position..]) {
                position += whitespace_match.end();
                continue;
            }

            for pattern in self.patterns.iter() {
                let (regex, token_kind) = pattern;

                if let Some(matched) = regex.find(&source[position..]) {
                    let value =
                        TokenValue::from_kind(interner, token_kind.clone(), matched.as_str())
                            .unwrap();

                    tokens.push(Token {
                        kind: token_kind.clone(),
                        value,
                        start: position,
                        end: position + matched.end(),
                    });

                    position += matched.end();
                    found_match = true;
                    break;
                }
            }

            if !found_match {
                return Err(LexerError { position });
            }
        }

        return Ok(tokens);
    }
}

#[cfg(test)]
mod tests {
    use string_interner::symbol::SymbolU32;

    use super::*;

    #[test]
    fn simple_expression() {
        let mut interner = StringInterner::<StringBackend<SymbolU32>>::new();
        let result = Lexer::new().tokenize(&mut interner, "(8 - 3) * (4 / 2)".to_string());
        assert_eq!(
            result,
            Ok(vec![
                Token {
                    kind: TokenKind::OpenParen,
                    value: TokenValue::None,
                    start: 0,
                    end: 1,
                },
                Token {
                    kind: TokenKind::Int,
                    value: TokenValue::Int(8),
                    start: 1,
                    end: 2,
                },
                Token {
                    kind: TokenKind::Dash,
                    value: TokenValue::None,
                    start: 3,
                    end: 4,
                },
                Token {
                    kind: TokenKind::Int,
                    value: TokenValue::Int(3),
                    start: 5,
                    end: 6,
                },
                Token {
                    kind: TokenKind::CloseParen,
                    value: TokenValue::None,
                    start: 6,
                    end: 7,
                },
                Token {
                    kind: TokenKind::Star,
                    value: TokenValue::None,
                    start: 8,
                    end: 9,
                },
                Token {
                    kind: TokenKind::OpenParen,
                    value: TokenValue::None,
                    start: 10,
                    end: 11,
                },
                Token {
                    kind: TokenKind::Int,
                    value: TokenValue::Int(4),
                    start: 11,
                    end: 12,
                },
                Token {
                    kind: TokenKind::Slash,
                    value: TokenValue::None,
                    start: 13,
                    end: 14,
                },
                Token {
                    kind: TokenKind::Int,
                    value: TokenValue::Int(2),
                    start: 15,
                    end: 16,
                },
                Token {
                    kind: TokenKind::CloseParen,
                    value: TokenValue::None,
                    start: 16,
                    end: 17,
                },
            ])
        );
    }

    #[test]
    fn simple_variable_definition() {
        let mut interner = StringInterner::<StringBackend<SymbolU32>>::new();
        let result = Lexer::new().tokenize(&mut interner, "let x = 2 + 2;".to_string());

        assert_eq!(
            result,
            Ok(vec![
                Token {
                    kind: TokenKind::LetKeyword,
                    value: TokenValue::None,
                    start: 0,
                    end: 3,
                },
                Token {
                    kind: TokenKind::Identifier,
                    value: TokenValue::String(interner.get("x").unwrap()),
                    start: 4,
                    end: 5,
                },
                Token {
                    kind: TokenKind::Equal,
                    value: TokenValue::None,
                    start: 6,
                    end: 7,
                },
                Token {
                    kind: TokenKind::Int,
                    value: TokenValue::Int(2),
                    start: 8,
                    end: 9,
                },
                Token {
                    kind: TokenKind::Plus,
                    value: TokenValue::None,
                    start: 10,
                    end: 11,
                },
                Token {
                    kind: TokenKind::Int,
                    value: TokenValue::Int(2),
                    start: 12,
                    end: 13,
                },
                Token {
                    kind: TokenKind::Semicolon,
                    value: TokenValue::None,
                    start: 13,
                    end: 14,
                },
            ])
        );
    }
}
