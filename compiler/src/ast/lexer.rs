use codespan::Span;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenTag {
    Int,
    Float,
    Char,
    String,
    Identifier,
    Colon,
    SemiColon,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenAngle,
    CloseAngle,
    Eq,
    Bang,
    Minus,
    Amper,
    Vbar,
    Plus,
    Star,
    Slash,
    Percent,
    EqEq,
    BangEq,
    LessEq,
    GreaterEq,
    Whitespace,
    Unknown,
    Eof,
}

impl std::fmt::Display for TokenTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenTag::*;
        let text = match self {
            Int => "<int>",
            Float => "<float>",
            Char => "<char>",
            String => "<string>",
            Identifier => "<identifier>",
            Colon => ":",
            SemiColon => ";",
            Comma => ",",
            Dot => ".",
            OpenParen => "(",
            CloseParen => ")",
            OpenBrace => "{",
            CloseBrace => "}",
            OpenBracket => "[",
            CloseBracket => "]",
            OpenAngle => "<",
            CloseAngle => ">",
            Eq => "=",
            Bang => "!",
            Minus => "-",
            Amper => "&",
            Vbar => "|",
            Plus => "+",
            Star => "*",
            Slash => "/",
            Percent => "%",
            EqEq => "==",
            BangEq => "!=",
            LessEq => "<=",
            GreaterEq => ">=",
            Whitespace => "<whitespace>",
            Unknown => "<unknown>",
            Eof => "<eof>",
        };
        write!(f, "{}", text)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Int,
    Float,
    Char { terminated: bool },
    String { terminated: bool },
    Identifier,
    Colon,
    SemiColon,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenAngle,
    CloseAngle,
    Eq,
    Bang,
    Minus,
    Amper,
    Vbar,
    Plus,
    Star,
    Slash,
    Percent,
    EqEq,
    BangEq,
    LessEq,
    GreaterEq,
    Whitespace,
    Unknown,
    Eof,
}

impl TokenKind {
    pub fn tag(&self) -> TokenTag {
        match self {
            TokenKind::Int => TokenTag::Int,
            TokenKind::Float => TokenTag::Float,
            TokenKind::Char { .. } => TokenTag::Char,
            TokenKind::String { .. } => TokenTag::String,
            TokenKind::Identifier => TokenTag::Identifier,
            TokenKind::Colon => TokenTag::Colon,
            TokenKind::SemiColon => TokenTag::SemiColon,
            TokenKind::Comma => TokenTag::Comma,
            TokenKind::Dot => TokenTag::Dot,
            TokenKind::OpenParen => TokenTag::OpenParen,
            TokenKind::CloseParen => TokenTag::CloseParen,
            TokenKind::OpenBrace => TokenTag::OpenBrace,
            TokenKind::CloseBrace => TokenTag::CloseBrace,
            TokenKind::OpenBracket => TokenTag::OpenBracket,
            TokenKind::CloseBracket => TokenTag::CloseBracket,
            TokenKind::OpenAngle => TokenTag::OpenAngle,
            TokenKind::CloseAngle => TokenTag::CloseAngle,
            TokenKind::Eq => TokenTag::Eq,
            TokenKind::Bang => TokenTag::Bang,
            TokenKind::Minus => TokenTag::Minus,
            TokenKind::Amper => TokenTag::Amper,
            TokenKind::Vbar => TokenTag::Vbar,
            TokenKind::Plus => TokenTag::Plus,
            TokenKind::Star => TokenTag::Star,
            TokenKind::Slash => TokenTag::Slash,
            TokenKind::Percent => TokenTag::Percent,
            TokenKind::EqEq => TokenTag::EqEq,
            TokenKind::BangEq => TokenTag::BangEq,
            TokenKind::LessEq => TokenTag::LessEq,
            TokenKind::GreaterEq => TokenTag::GreaterEq,
            TokenKind::Whitespace => TokenTag::Whitespace,
            TokenKind::Unknown => TokenTag::Unknown,
            TokenKind::Eof => TokenTag::Eof,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
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
            '\t' | '\n' | '\r' | ' ' => self.consume_whitespace(),
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
            '<' => self.consume_and_check('=', TokenKind::LessEq, TokenKind::OpenAngle),
            '>' => self.consume_and_check('=', TokenKind::GreaterEq, TokenKind::CloseAngle),
            ':' => TokenKind::Colon,
            '=' => self.consume_and_check('=', TokenKind::EqEq, TokenKind::Eq),
            '!' => self.consume_and_check('=', TokenKind::BangEq, TokenKind::Bang),
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
            span: Span::new(self.offset as u32, (self.offset + length) as u32),
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
            true => TokenKind::Float,
            false => TokenKind::Int,
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

    pub fn next_expect(&mut self, expected: TokenTag) -> Result<Token, Token> {
        let token = self.next();
        match token.kind.tag() {
            tag if tag == expected => return Ok(token),
            _ => Err(token),
        }
    }

    pub fn next(&mut self) -> Token {
        if let Some(token) = self.peeked.take() {
            return token;
        }

        let mut unknown_span: Option<Span> = None;
        loop {
            let token = self.lexer.next();
            match token.kind {
                TokenKind::Whitespace => continue,
                TokenKind::Unknown => {
                    unknown_span = match unknown_span {
                        Some(span) => Some(Span::merge(span, token.span)),
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
}
