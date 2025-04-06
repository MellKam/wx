use std::cell::RefCell;
use std::rc::Rc;
use std::{self};

use super::diagnostics::{Diagnostic, DiagnosticKind, DiagnosticsBag};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Int {
        radix: Radix,
    },
    Float {
        radix: Radix,
    },
    Char {
        terminated: bool,
    },
    String {
        terminated: bool,
    },
    Identifier,
    /// `:`
    Colon,
    /// `;`
    SemiColon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `<`
    Less,
    /// `>`
    Greater,
    /// `=`
    Eq,
    /// `!`
    Bang,
    /// `-`
    Minus,
    /// `&`
    Amper,
    /// `|`
    Vbar,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `==`
    EqEq,
    /// `!=`
    NotEq,
    /// `<=`
    LessEq,
    /// `>=`
    GreaterEq,
    /// Any whitespace character sequence.
    Whitespace,
    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,
    /// End of input
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    offset: usize,
    peeked: Option<Token>,
}

const EOF_CHAR: char = '\0';

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.chars(),
            offset: 0,
            peeked: None,
        }
    }

    pub fn next(&mut self) -> Token {
        let start = self.chars.as_str().len();
        let token_kind = match self.chars.next().unwrap_or(EOF_CHAR) {
            '\t' | '\n' | '\x0C' | '\r' | ' ' => self.consume_whitespace(),
            '0'..='9' => self.consume_number(),
            'A'..='Z' | 'a'..='z' | '_' => self.consume_identifier(),
            ';' => TokenKind::SemiColon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '<' => self.consume_and_check('=', TokenKind::LessEq, TokenKind::Less),
            '>' => self.consume_and_check('=', TokenKind::GreaterEq, TokenKind::Greater),
            ':' => TokenKind::Colon,
            '=' => self.consume_and_check('=', TokenKind::EqEq, TokenKind::Eq),
            '!' => self.consume_and_check('=', TokenKind::NotEq, TokenKind::Bang),
            '-' => TokenKind::Minus,
            '&' => TokenKind::Amper,
            '|' => TokenKind::Vbar,
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '%' => TokenKind::Percent,
            '\'' => self.consume_char(),
            '"' => self.consume_string(),
            '\0' => TokenKind::Eof,
            _ => TokenKind::Unknown,
        };
        let length = start - self.chars.as_str().len();

        let token = Token {
            kind: token_kind,
            span: Span {
                start: self.offset as u32,
                end: (self.offset + length) as u32,
            },
        };
        self.offset += length;

        return token;
    }

    fn consume_and_check(
        &mut self,
        expect: char,
        token: TokenKind,
        fallback: TokenKind,
    ) -> TokenKind {
        match self.chars.clone().next().unwrap_or(EOF_CHAR) {
            ch if ch == expect => {
                _ = self.chars.next();
                return token;
            }
            _ => return fallback,
        }
    }

    fn consume_string(&mut self) -> TokenKind {
        while let Some(char) = self.chars.next() {
            match char {
                '\\' => {
                    _ = self.chars.next();
                }
                '"' => {
                    return TokenKind::String { terminated: true };
                }
                _ => (),
            }
        }

        TokenKind::String { terminated: false }
    }

    fn consume_char(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        let terminated = loop {
            match peeker.next().unwrap_or(EOF_CHAR) {
                EOF_CHAR => break false,
                '\'' => {
                    _ = self.chars.next();
                    break true;
                }
                '\n' => {
                    _ = self.chars.next();
                    match self.chars.next().unwrap_or(EOF_CHAR) {
                        '\'' => break true,
                        _ => break false,
                    }
                }
                '\\' => {
                    _ = self.chars.next();
                    _ = self.chars.next();
                }
                _ => {
                    _ = self.chars.next();
                }
            }
        };

        TokenKind::Char { terminated }
    }

    fn consume_whitespace(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        while let Some(ch) = peeker.next() {
            if ch.is_whitespace() {
                _ = self.chars.next();
            } else {
                break;
            }
        }
        TokenKind::Whitespace
    }

    fn consume_number(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        let mut seen_dot = false;
        while let Some(char) = peeker.next() {
            match char {
                '0'..='9' => {
                    _ = self.chars.next();
                }
                '.' if !seen_dot => {
                    seen_dot = true;
                    _ = self.chars.next();
                }
                _ => break,
            }
        }

        match seen_dot {
            true => TokenKind::Float {
                radix: Radix::Decimal,
            },
            false => TokenKind::Int {
                radix: Radix::Decimal,
            },
        }
    }

    fn consume_identifier(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        while let Some(char) = peeker.next() {
            match char {
                'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
                    _ = self.chars.next();
                }
                _ => break,
            }
        }

        TokenKind::Identifier
    }
}

pub struct BufferedLexer<'l> {
    lexer: Lexer<'l>,
    diagnostics: Rc<RefCell<DiagnosticsBag>>,
    peeked: Option<Token>,
}

impl<'l> BufferedLexer<'l> {
    pub fn new(lexer: Lexer<'l>, diagnostics: Rc<RefCell<DiagnosticsBag>>) -> Self {
        Self {
            lexer,
            diagnostics,
            peeked: None,
        }
    }

    pub fn next_expect(&mut self, expected: TokenKind) -> Option<Token> {
        let token = self.next();
        match token.kind.clone() {
            kind if kind == expected => return Some(token),
            TokenKind::Eof => {
                self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
                    message: format!("Unexpected end of input, expected `{:#?}`.", expected),
                    span: token.span.clone(),
                    kind: DiagnosticKind::Error,
                });
                return None;
            }
            _ => {
                self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
                    message: format!(
                        "Unexpected token `{:#?}`, expected `{:#?}`.",
                        token.kind, expected
                    ),
                    span: token.span.clone(),
                    kind: DiagnosticKind::Error,
                });
                return None;
            }
        }
    }

    pub fn next(&mut self) -> Token {
        if let Some(token) = self.peeked.take() {
            return token;
        }

        loop {
            let token = self.lexer.next();
            match token.kind {
                TokenKind::Whitespace => continue,
                TokenKind::Unknown => {
                    self.diagnostics.borrow_mut().diagnostics.push(Diagnostic {
                        message: format!("Unknown token at position {}.", token.span.start),
                        span: token.span,
                        kind: DiagnosticKind::Error,
                    });
                    continue;
                }
                _ => return token,
            }
        }
    }

    pub fn peek(&mut self) -> Token {
        match &self.peeked {
            Some(token) => return token.clone(),
            None => {
                let token = self.next();
                self.peeked = Some(token.clone());
                return token.clone();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_token(token: Token, kind: TokenKind, span: Span) {
        assert_eq!(token, Token { kind, span });
    }

    #[test]
    fn should_lex_numbers() {
        let mut lexer = Lexer::new("1 + 2.5 - 3.");

        assert_token(
            lexer.next(),
            TokenKind::Int {
                radix: Radix::Decimal,
            },
            Span { start: 0, end: 1 },
        );
        assert_token(
            lexer.next(),
            TokenKind::Whitespace,
            Span { start: 1, end: 2 },
        );
        assert_token(lexer.next(), TokenKind::Plus, Span { start: 2, end: 3 });
        assert_token(
            lexer.next(),
            TokenKind::Whitespace,
            Span { start: 3, end: 4 },
        );
        assert_token(
            lexer.next(),
            TokenKind::Float {
                radix: Radix::Decimal,
            },
            Span { start: 4, end: 7 },
        );
        assert_token(
            lexer.next(),
            TokenKind::Whitespace,
            Span { start: 7, end: 8 },
        );
        assert_token(lexer.next(), TokenKind::Minus, Span { start: 8, end: 9 });
        assert_token(
            lexer.next(),
            TokenKind::Whitespace,
            Span { start: 9, end: 10 },
        );
        assert_token(
            lexer.next(),
            TokenKind::Float {
                radix: Radix::Decimal,
            },
            Span { start: 10, end: 12 },
        );
        assert_token(lexer.next(), TokenKind::Eof, Span { start: 12, end: 12 });
    }

    #[test]
    fn should_lex_identifiers() {
        let mut lexer = Lexer::new("foo Bar _baz");

        assert_token(
            lexer.next(),
            TokenKind::Identifier,
            Span { start: 0, end: 3 },
        );
        assert_token(
            lexer.next(),
            TokenKind::Whitespace,
            Span { start: 3, end: 4 },
        );
        assert_token(
            lexer.next(),
            TokenKind::Identifier,
            Span { start: 4, end: 7 },
        );
        assert_token(
            lexer.next(),
            TokenKind::Whitespace,
            Span { start: 7, end: 8 },
        );
        assert_token(
            lexer.next(),
            TokenKind::Identifier,
            Span { start: 8, end: 12 },
        );
        assert_token(lexer.next(), TokenKind::Eof, Span { start: 12, end: 12 });
    }

    #[test]
    fn should_lex_unicode_string_literals() {
        let mut lexer = Lexer::new("\"hello, 世界\"");

        assert_token(
            lexer.next(),
            TokenKind::String { terminated: true },
            Span { start: 0, end: 15 },
        );
        assert_token(lexer.next(), TokenKind::Eof, Span { start: 15, end: 15 });
    }

    #[test]
    fn should_handle_empty_input() {
        let mut lexer = Lexer::new("");
        assert_token(lexer.next(), TokenKind::Eof, Span { start: 0, end: 0 });
    }

    #[test]
    fn should_handle_unterminated_string() {
        let mut lexer = Lexer::new("\"hello");
        assert_token(
            lexer.next(),
            TokenKind::String { terminated: false },
            Span { start: 0, end: 6 },
        );
    }
}
