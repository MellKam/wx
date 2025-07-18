use serde::Serialize;

use crate::span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum TokenKind {
    // Literals
    Int,
    Float,
    Char { terminated: bool },
    String { terminated: bool },
    Identifier,
    // Delimiters
    Colon,
    ColonColon,
    SemiColon,
    Comma,
    Dot,
    // Grouping
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    // Operators
    Eq,
    EqEq,
    Bang,
    BangEq,
    Less,
    LessEq,
    LeftShift,
    Greater,
    GreaterEq,
    RightShift,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Percent,
    PercentEq,
    Amper,
    AmperAmper,
    Vbar,
    VbarVbar,
    Caret,
    Arrow,
    // Special
    Comment,
    Whitespace,
    Unknown,
    Eof,
}

impl TokenKind {
    pub fn discriminant_equals(self, other: TokenKind) -> bool {
        std::mem::discriminant(&self) == std::mem::discriminant(&other)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        let text = match self {
            Int => "integer literal",
            Float => "float literal",
            Char { .. } => "char literal",
            String { .. } => "string literal",
            Identifier => "identifier",

            Colon => ":",
            ColonColon => "::",
            SemiColon => ";",
            Comma => ",",
            Dot => ".",

            OpenParen => "(",
            CloseParen => ")",
            OpenBrace => "{",
            CloseBrace => "}",
            OpenBracket => "[",
            CloseBracket => "]",

            Eq => "=",
            Bang => "!",
            EqEq => "==",
            BangEq => "!=",
            Less => "<",
            LessEq => "<=",
            LeftShift => "<<",
            Greater => ">",
            GreaterEq => ">=",
            RightShift => ">>",
            Plus => "+",
            PlusEq => "+=",
            Minus => "-",
            MinusEq => "-=",
            Star => "*",
            StarEq => "*=",
            Slash => "/",
            SlashEq => "/=",
            Percent => "%",
            PercentEq => "%=",
            Amper => "&",
            AmperAmper => "&&",
            Vbar => "|",
            VbarVbar => "||",
            Caret => "^",
            Arrow => "->",

            Comment => "comment",
            Whitespace => "whitespace",
            Unknown => "unknown token",
            Eof => "end of file",
        };
        write!(f, "{}", text)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TextSpan,
}

pub struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    offset: usize,
}

const EOF_CHAR: char = '\0';

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.chars(),
            offset: 0,
        }
    }

    pub fn next(&mut self) -> Token {
        let start = self.chars.as_str().len();
        let token_kind = match self.chars.next().unwrap_or(EOF_CHAR) {
            // Patterns are ordered by frequency of occurrence in typical source code
            // Most Frequent
            '\t' | '\n' | '\r' | ' ' => self.consume_whitespace(),
            'A'..='Z' | 'a'..='z' | '_' => self.consume_identifier(),
            '0'..='9' => self.consume_number(),

            // Common Punctuation & Operators
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            ';' => TokenKind::SemiColon,
            ',' => TokenKind::Comma,
            '=' => self.consume_and_check('=', TokenKind::EqEq, TokenKind::Eq),
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '.' => TokenKind::Dot,
            ':' => self.consume_and_check(':', TokenKind::ColonColon, TokenKind::Colon),
            '/' => self.consume_slash(), // Handles division and comments

            // Moderately Frequent
            '+' => self.consume_and_check('=', TokenKind::PlusEq, TokenKind::Plus),
            '-' => self.consume_dash(),
            '*' => self.consume_and_check('=', TokenKind::StarEq, TokenKind::Star),
            '<' => self.consume_open_angle(),
            '>' => self.consume_close_angle(),
            '!' => self.consume_and_check('=', TokenKind::BangEq, TokenKind::Bang),
            '&' => self.consume_and_check('&', TokenKind::AmperAmper, TokenKind::Amper),
            '|' => self.consume_and_check('|', TokenKind::VbarVbar, TokenKind::Vbar),
            '"' => self.consume_string(),

            // Less Frequent
            '%' => self.consume_and_check('=', TokenKind::PercentEq, TokenKind::Percent),
            '^' => TokenKind::Caret,
            '\'' => self.consume_char(),
            '\0' => TokenKind::Eof,
            _ => TokenKind::Unknown,
        };
        let length = start - self.chars.as_str().len();

        let token = Token {
            kind: token_kind,
            span: TextSpan::new(self.offset as u32, (self.offset + length) as u32),
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

    fn consume_dash(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF_CHAR) {
            '=' => {
                _ = self.chars.next();
                return TokenKind::MinusEq;
            }
            '>' => {
                _ = self.chars.next();
                return TokenKind::Arrow;
            }
            _ => return TokenKind::Minus,
        }
    }

    fn consume_open_angle(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF_CHAR) {
            '=' => {
                _ = self.chars.next();
                return TokenKind::LessEq;
            }
            '<' => {
                _ = self.chars.next();
                return TokenKind::LeftShift;
            }
            _ => return TokenKind::Less,
        }
    }

    fn consume_close_angle(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF_CHAR) {
            '=' => {
                _ = self.chars.next();
                return TokenKind::GreaterEq;
            }
            '>' => {
                _ = self.chars.next();
                return TokenKind::RightShift;
            }
            _ => return TokenKind::Greater,
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
            true => TokenKind::Float,
            false => TokenKind::Int,
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

    fn consume_slash(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF_CHAR) {
            '/' => {
                _ = self.chars.next();
            }
            '=' => {
                _ = self.chars.next();
                return TokenKind::SlashEq;
            }
            _ => return TokenKind::Slash,
        }

        while let Some(char) = peeker.next() {
            match char {
                '\n' | EOF_CHAR => break,
                _ => {
                    _ = self.chars.next();
                }
            }
        }

        TokenKind::Comment
    }
}

pub struct PeekableLexer<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Token>,
}

impl<'a> PeekableLexer<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            peeked: None,
        }
    }

    pub fn next(&mut self) -> Token {
        if let Some(token) = self.peeked.take() {
            return token;
        }

        let mut unknown_span: Option<TextSpan> = None;
        loop {
            let token = self.lexer.next();
            match token.kind {
                TokenKind::Whitespace | TokenKind::Comment => continue,
                TokenKind::Unknown => {
                    unknown_span = match unknown_span {
                        Some(span) => Some(TextSpan::merge(span, token.span)),
                        None => Some(token.span),
                    };
                    continue;
                }
                _ => match unknown_span {
                    Some(span) => {
                        self.peeked = Some(token.clone());
                        return Token {
                            kind: TokenKind::Unknown,
                            span,
                        };
                    }
                    None => return token,
                },
            }
        }
    }

    pub fn peek(&mut self) -> Token {
        match &self.peeked {
            Some(token) => return token.clone(),
            None => {
                let token = self.next();
                self.peeked = Some(token.clone());
                return token;
            }
        }
    }

    pub fn next_if(&mut self, expected_kind: TokenKind) -> Option<Token> {
        let token = self.peek();
        if TokenKind::discriminant_equals(token.kind, expected_kind) {
            self.peeked = None;
            Some(token)
        } else {
            None
        }
    }
}
