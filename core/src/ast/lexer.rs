use std::{iter::Peekable, ops::Range, str::CharIndices};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // Literals
    Int,
    Float,
    String,
    Identifier,
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
    LeftParen,   // (
    RightParen,  // )
    LeftSquare,  // [
    RightSquare, // ]
    LeftBrace,   // {
    RightBrace,  // }
    LeftAngle,   // <
    RightAngle,  // >
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
}

impl Token {
    fn is_keyword(str: &str) -> Option<Token> {
        match str {
            "let" => Some(Token::Let),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "loop" => Some(Token::Loop),
            "break" => Some(Token::Break),
            "continue" => Some(Token::Continue),
            "return" => Some(Token::Return),
            "fn" => Some(Token::Fn),
            "import" => Some(Token::Import),
            "export" => Some(Token::Export),
            _ => None,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Int => write!(f, "<int>"),
            Token::Float => write!(f, "<float>"),
            Token::String => write!(f, "<string>"),
            Token::Identifier => write!(f, "<identifier>"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::Amper => write!(f, "&"),
            Token::AmperAmper => write!(f, "&&"),
            Token::Vbar => write!(f, "|"),
            Token::VbarVbar => write!(f, "||"),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftSquare => write!(f, "["),
            Token::RightSquare => write!(f, "]"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftAngle => write!(f, "<"),
            Token::RightAngle => write!(f, ">"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Loop => write!(f, "loop"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Return => write!(f, "return"),
            Token::Fn => write!(f, "fn"),
            Token::Import => write!(f, "import"),
            Token::Export => write!(f, "export"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenWithSpan {
    pub span: Range<usize>,
    pub kind: Token,
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            source,
            chars: source.char_indices().peekable(),
        }
    }

    // pub fn expect(&mut self, token: Token) {
    //     match self.next() {}
    // }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter,
    UnterminatedString,
    NonAsciiIdentifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexerErrorWithSpan {
    pub span: Range<usize>,
    pub kind: LexerError,
}

impl Iterator for Lexer<'_> {
    type Item = Result<TokenWithSpan, LexerErrorWithSpan>;

    fn next(&mut self) -> Option<Self::Item> {
        let (mut start, mut char) = self.chars.next()?;
        while char.is_whitespace() {
            (start, char) = self.chars.next()?;
        }

        let with_span = move |token: Token, length: usize| -> Option<Self::Item> {
            Some(Ok(TokenWithSpan {
                kind: token,
                span: Range {
                    start,
                    end: start + length,
                },
            }))
        };

        match char {
            '+' => return with_span(Token::Plus, 1),
            '-' => return with_span(Token::Minus, 1),
            '*' => return with_span(Token::Star, 1),
            '/' => return with_span(Token::Slash, 1),
            '%' => return with_span(Token::Percent, 1),
            ':' => return with_span(Token::Colon, 1),
            ';' => return with_span(Token::Semicolon, 1),
            ',' => return with_span(Token::Comma, 1),
            '.' => return with_span(Token::Dot, 1),
            '(' => return with_span(Token::LeftParen, 1),
            ')' => return with_span(Token::RightParen, 1),
            '[' => return with_span(Token::LeftSquare, 1),
            ']' => return with_span(Token::RightSquare, 1),
            '{' => return with_span(Token::LeftBrace, 1),
            '}' => return with_span(Token::RightBrace, 1),
            '=' => match self.chars.peek() {
                Some((_, '=')) => {
                    self.chars.next();
                    return with_span(Token::EqualEqual, 2);
                }
                _ => return with_span(Token::Equal, 1),
            },
            '!' => match self.chars.peek() {
                Some((_, '=')) => {
                    self.chars.next();
                    return with_span(Token::BangEqual, 2);
                }
                _ => return with_span(Token::Bang, 1),
            },
            '<' => match self.chars.peek() {
                Some((_, '=')) => {
                    self.chars.next();
                    return with_span(Token::LessEqual, 2);
                }
                _ => return with_span(Token::LeftAngle, 1),
            },
            '>' => match self.chars.peek() {
                Some((_, '=')) => {
                    self.chars.next();
                    return with_span(Token::GreaterEqual, 2);
                }
                _ => return with_span(Token::RightAngle, 1),
            },
            '&' => match self.chars.peek() {
                Some((_, '&')) => {
                    self.chars.next();
                    return with_span(Token::AmperAmper, 2);
                }
                _ => return with_span(Token::Amper, 1),
            },
            '|' => match self.chars.peek() {
                Some((_, '|')) => {
                    self.chars.next();
                    return with_span(Token::VbarVbar, 2);
                }
                _ => return with_span(Token::Vbar, 1),
            },
            '"' => {
                let mut end = start + 1;
                let mut escaped = false;

                while let Some((_, char)) = self.chars.next() {
                    end += char.len_utf8();
                    if escaped {
                        escaped = false;
                    } else if char == '\\' {
                        escaped = true;
                    } else if char == '"' {
                        return Some(Ok(TokenWithSpan {
                            kind: Token::String,
                            span: Range { start, end },
                        }));
                    }
                }

                return Some(Err(LexerErrorWithSpan {
                    kind: LexerError::UnterminatedString,
                    span: Range { start, end },
                }));
            }
            '0'..='9' => {
                let mut end = start + 1;
                let mut seen_dot = false;

                while let Some((_, char)) = self.chars.peek() {
                    match *char {
                        '0'..='9' => {}
                        '.' if !seen_dot => seen_dot = true,
                        _ => break,
                    }

                    end += char.len_utf8();
                    self.chars.next();
                }

                return Some(Ok(TokenWithSpan {
                    kind: if seen_dot { Token::Float } else { Token::Int },
                    span: Range { start, end },
                }));
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut end = start + 1;
                while let Some((_, char)) = self.chars.peek() {
                    if !char.is_ascii_alphabetic() && !char.is_ascii_digit() && *char != '_' {
                        break;
                    }
                    end += 1;
                    self.chars.next();
                }

                return Some(Ok(TokenWithSpan {
                    kind: Token::is_keyword(&self.source[start..end]).unwrap_or(Token::Identifier),
                    span: Range { start, end },
                }));
            }
            char if !char.is_ascii() => {
                let mut end = start + char.len_utf8();
                while let Some((_, char)) = self.chars.peek() {
                    if char.is_ascii() {
                        break;
                    }
                    end += char.len_utf8();
                    self.chars.next();
                }

                return Some(Err(LexerErrorWithSpan {
                    kind: LexerError::NonAsciiIdentifier,
                    span: Range { start, end },
                }));
            }
            _ => {
                return Some(Err(LexerErrorWithSpan {
                    kind: LexerError::UnexpectedCharacter,
                    span: Range {
                        start,
                        end: start + 1,
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
            Some(Ok(TokenWithSpan {
                kind: Token::Int,
                span: Range { start: 0, end: 1 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithSpan {
                kind: Token::Plus,
                span: Range { start: 2, end: 3 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithSpan {
                kind: Token::Float,
                span: Range { start: 4, end: 7 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithSpan {
                kind: Token::Minus,
                span: Range { start: 8, end: 9 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithSpan {
                kind: Token::Float,
                span: Range { start: 10, end: 12 }
            }))
        );
    }

    #[test]
    fn should_lex_identifiers() {
        let mut lexer = Lexer::new("foo Bar _baz");

        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithSpan {
                kind: Token::Identifier,
                span: Range { start: 0, end: 3 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithSpan {
                kind: Token::Identifier,
                span: Range { start: 4, end: 7 }
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithSpan {
                kind: Token::Identifier,
                span: Range { start: 8, end: 12 }
            }))
        );
    }

    #[test]
    fn should_lex_unicode_string_literals() {
        let mut lexer = Lexer::new("\"hello, 世界\"");

        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithSpan {
                kind: Token::String,
                span: Range { start: 0, end: 15 }
            }))
        );
    }

    #[test]
    fn should_handle_errors_for_nonascii_identifiers() {
        let mut lexer = Lexer::new("привет");

        assert_eq!(
            lexer.next(),
            Some(Err(LexerErrorWithSpan {
                kind: LexerError::NonAsciiIdentifier,
                span: Range { start: 0, end: 12 },
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
            Some(Err(LexerErrorWithSpan {
                kind: LexerError::UnterminatedString,
                span: Range { start: 0, end: 6 },
            }))
        );
    }
}
