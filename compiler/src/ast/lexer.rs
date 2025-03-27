use std::{iter::Peekable, ops::Range, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Literals
    Int,
    Float,
    String,
    Identifier,
    // Keywords
    Let,
    If,
    Else,
    Loop,
    Break,
    Continue,
    Return,
    Fn,
    Import,
    Export,
    // Punctuation
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Equal,        // =
    EqualEqual,   // ==
    Bang,         // !
    BangEqual,    // !=
    LessEqual,    // <=
    GreaterEqual, // >=
    Amper,        // &
    AmperAmper,   // &&
    Vbar,         // |
    VbarVbar,     // ||
    Colon,        // :
    Semicolon,    // ;
    Comma,        // ,
    Dot,          // .
    // Grouping
    OpenParen,   // (
    CloseParen,  // )
    OpenSquare,  // [
    CloseSquare, // ]
    OpenBrace,   // {
    CloseBrace,  // }
    OpenAngle,   // <
    CloseAngle,  // >
}

impl TokenKind {
    fn is_keyword(str: &str) -> Option<TokenKind> {
        match str {
            "let" => Some(TokenKind::Let),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "loop" => Some(TokenKind::Loop),
            "break" => Some(TokenKind::Break),
            "continue" => Some(TokenKind::Continue),
            "return" => Some(TokenKind::Return),
            "fn" => Some(TokenKind::Fn),
            "import" => Some(TokenKind::Import),
            "export" => Some(TokenKind::Export),
            _ => None,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Int => write!(f, "integer literal"),
            TokenKind::Float => write!(f, "float literal"),
            TokenKind::String => write!(f, "string literal"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Amper => write!(f, "&"),
            TokenKind::AmperAmper => write!(f, "&&"),
            TokenKind::Vbar => write!(f, "|"),
            TokenKind::VbarVbar => write!(f, "||"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::OpenSquare => write!(f, "["),
            TokenKind::CloseSquare => write!(f, "]"),
            TokenKind::OpenBrace => write!(f, "{{"),
            TokenKind::CloseBrace => write!(f, "}}"),
            TokenKind::OpenAngle => write!(f, "<"),
            TokenKind::CloseAngle => write!(f, ">"),
            TokenKind::Let => write!(f, "let keyword"),
            TokenKind::If => write!(f, "if keyword"),
            TokenKind::Else => write!(f, "else keyword"),
            TokenKind::Loop => write!(f, "loop keyword"),
            TokenKind::Break => write!(f, "break keyword"),
            TokenKind::Continue => write!(f, "continue keyword"),
            TokenKind::Return => write!(f, "return keyword"),
            TokenKind::Fn => write!(f, "fn keyword"),
            TokenKind::Import => write!(f, "import keyword"),
            TokenKind::Export => write!(f, "export keyword"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Range<usize>,
    pub kind: TokenKind,
}

pub struct Lexer<'db> {
    text: &'db str,
    chars: Chars<'db>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexerErrorKind {
    UnexpectedCharacter,
    UnterminatedString,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexerError {
    pub span: Range<usize>,
    pub kind: LexerErrorKind,
}

impl<'db> Lexer<'db> {
    pub fn new(input: &'db str) -> Lexer<'db> {
        Lexer {
            text: input,
            chars: input.chars(),
        }
    }

    /// Returns the current position in the input string
    fn cursor(&self) -> usize {
        self.text.len() - self.chars.as_str().len()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut char = self.chars.next()?;
        while char.is_whitespace() {
            char = self.chars.next()?;
        }
        let start = self.cursor() - char.len_utf8();

        let with_span = move |token: TokenKind, length: usize| -> Option<Self::Item> {
            Some(Ok(Token {
                kind: token,
                span: Range {
                    start,
                    end: start + length,
                },
            }))
        };

        match char {
            '+' => return with_span(TokenKind::Plus, 1),
            '-' => return with_span(TokenKind::Minus, 1),
            '*' => return with_span(TokenKind::Star, 1),
            '/' => return with_span(TokenKind::Slash, 1),
            '%' => return with_span(TokenKind::Percent, 1),
            ':' => return with_span(TokenKind::Colon, 1),
            ';' => return with_span(TokenKind::Semicolon, 1),
            ',' => return with_span(TokenKind::Comma, 1),
            '.' => return with_span(TokenKind::Dot, 1),
            '(' => return with_span(TokenKind::OpenParen, 1),
            ')' => return with_span(TokenKind::CloseParen, 1),
            '[' => return with_span(TokenKind::OpenSquare, 1),
            ']' => return with_span(TokenKind::CloseSquare, 1),
            '{' => return with_span(TokenKind::OpenBrace, 1),
            '}' => return with_span(TokenKind::CloseBrace, 1),
            '=' => match self.chars.clone().next() {
                Some('=') => {
                    _ = self.chars.next();
                    return with_span(TokenKind::EqualEqual, 2);
                }
                _ => return with_span(TokenKind::Equal, 1),
            },
            '!' => match self.chars.clone().next() {
                Some('=') => {
                    _ = self.chars.next();
                    return with_span(TokenKind::BangEqual, 2);
                }
                _ => return with_span(TokenKind::Bang, 1),
            },
            '<' => match self.chars.clone().next() {
                Some('=') => {
                    _ = self.chars.next();
                    return with_span(TokenKind::LessEqual, 2);
                }
                _ => return with_span(TokenKind::OpenAngle, 1),
            },
            '>' => match self.chars.clone().next() {
                Some('=') => {
                    _ = self.chars.next();
                    return with_span(TokenKind::GreaterEqual, 2);
                }
                _ => return with_span(TokenKind::CloseAngle, 1),
            },
            '&' => match self.chars.clone().next() {
                Some('&') => {
                    _ = self.chars.next();
                    return with_span(TokenKind::AmperAmper, 2);
                }
                _ => return with_span(TokenKind::Amper, 1),
            },
            '|' => match self.chars.clone().next() {
                Some('|') => {
                    _ = self.chars.next();
                    return with_span(TokenKind::VbarVbar, 2);
                }
                _ => return with_span(TokenKind::Vbar, 1),
            },
            '"' => loop {
                match self.chars.next() {
                    Some('\\') => {
                        // skip escaped character
                        _ = self.chars.next();
                    }
                    Some('"') => {
                        return Some(Ok(Token {
                            kind: TokenKind::String,
                            span: Range {
                                start,
                                end: self.cursor(),
                            },
                        }));
                    }
                    Some(_) => {}
                    None => {
                        return Some(Err(LexerError {
                            kind: LexerErrorKind::UnterminatedString,
                            span: Range {
                                start,
                                end: self.cursor(),
                            },
                        }));
                    }
                }
            },
            '0'..='9' => {
                let mut seen_dot = false;
                let mut peeker = self.chars.clone();
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

                return Some(Ok(Token {
                    kind: if seen_dot {
                        TokenKind::Float
                    } else {
                        TokenKind::Int
                    },
                    span: Range {
                        start,
                        end: self.cursor(),
                    },
                }));
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut peeker = self.chars.clone();
                while let Some(char) = peeker.next() {
                    match char {
                        'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
                            _ = self.chars.next();
                        }
                        _ => {
                            break;
                        }
                    }
                }
                let end = self.cursor();

                return Some(Ok(Token {
                    kind: TokenKind::is_keyword(&self.text[start..end])
                        .unwrap_or(TokenKind::Identifier),
                    span: Range { start, end },
                }));
            }
            _ => {
                return Some(Err(LexerError {
                    kind: LexerErrorKind::UnexpectedCharacter,
                    span: Range {
                        start,
                        end: self.cursor(),
                    },
                }));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_lex_numbers() {
        let mut lexer = Lexer::new("1 + 2.5 - 3.");

        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Int,
                span: Range { start: 0, end: 1 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Plus,
                span: Range { start: 2, end: 3 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Float,
                span: Range { start: 4, end: 7 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Minus,
                span: Range { start: 8, end: 9 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Float,
                span: Range { start: 10, end: 12 }
            }))
        );
    }

    #[test]
    fn should_lex_identifiers() {
        let mut lexer = Lexer::new("foo Bar _baz");

        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Identifier,
                span: Range { start: 0, end: 3 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Identifier,
                span: Range { start: 4, end: 7 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Identifier,
                span: Range { start: 8, end: 12 }
            }))
        );
    }

    #[test]
    fn should_lex_unicode_string_literals() {
        let mut lexer = Lexer::new("\"hello, 世界\"");

        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::String,
                span: Range { start: 0, end: 15 }
            }))
        );
    }

    #[test]
    fn should_handle_empty_input() {
        let mut lexer = Lexer::new("");
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn should_handle_unterminated_string() {
        let mut lexer = Lexer::new("\"hello");
        assert_eq!(
            lexer.next(),
            Some(Err(LexerError {
                kind: LexerErrorKind::UnterminatedString,
                span: Range { start: 0, end: 6 },
            }))
        );
    }
}
