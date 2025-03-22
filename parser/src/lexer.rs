use std::{iter::Peekable, str::CharIndices};

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
    NotEqual,     // !=
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
}

impl Token {
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Token::Int => None,
            Token::Float => None,
            Token::String => None,
            Token::Identifier => None,
            Token::Plus => Some("+"),
            Token::Minus => Some("-"),
            Token::Star => Some("*"),
            Token::Slash => Some("/"),
            Token::Percent => Some("%"),
            Token::Equal => Some("="),
            Token::EqualEqual => Some("=="),
            Token::Bang => Some("!"),
            Token::NotEqual => Some("!="),
            Token::LessEqual => Some("<="),
            Token::GreaterEqual => Some(">="),
            Token::Amper => Some("&"),
            Token::AmperAmper => Some("&&"),
            Token::Vbar => Some("|"),
            Token::VbarVbar => Some("||"),
            Token::Colon => Some(":"),
            Token::Semicolon => Some(";"),
            Token::Comma => Some(","),
            Token::Dot => Some("."),
            Token::LeftParen => Some("("),
            Token::RightParen => Some(")"),
            Token::LeftSquare => Some("["),
            Token::RightSquare => Some("]"),
            Token::LeftBrace => Some("{"),
            Token::RightBrace => Some("}"),
            Token::LeftAngle => Some("<"),
            Token::RightAngle => Some(">"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenWithPosition {
    pub token: Token,
    pub start: usize,
    pub end: usize,
}

pub struct Lexer<'a> {
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            chars: source.char_indices().peekable(),
        }
    }
}

impl Lexer<'_> {
    fn lex_number(&mut self, start: usize) -> TokenWithPosition {
        let mut seen_dot = false;
        let mut end = start + 1;

        while let Some((_, char)) = self.chars.peek() {
            if *char == '.' {
                if seen_dot {
                    break;
                }
                seen_dot = true;
            } else if !char.is_digit(10) {
                break;
            }
            end += 1;
            self.chars.next();
        }

        TokenWithPosition {
            token: if seen_dot { Token::Float } else { Token::Int },
            start,
            end,
        }
    }

    fn lex_string(&mut self, start: usize) -> TokenWithPosition {
        let mut end = start + 1;
        let mut escaped = false;

        while let Some((_, c)) = self.chars.next() {
            end += 1;
            if escaped {
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == '"' {
                break;
            }
        }

        TokenWithPosition {
            token: Token::String,
            start,
            end,
        }
    }

    fn lex_identifier(&mut self, start: usize) -> TokenWithPosition {
        let mut end = start + 1;
        while let Some((_, c)) = self.chars.peek() {
            if !c.is_alphanumeric() && *c != '_' {
                break;
            }
            end += 1;
            self.chars.next();
        }

        TokenWithPosition {
            token: Token::Identifier,
            start,
            end,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<TokenWithPosition, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let (mut index, mut char) = self.chars.next()?;
        while char.is_whitespace() {
            (index, char) = self.chars.next()?;
        }

        let token: Option<Token> = match char {
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Star),
            '/' => Some(Token::Slash),
            '%' => Some(Token::Percent),
            ':' => Some(Token::Colon),
            ';' => Some(Token::Semicolon),
            ',' => Some(Token::Comma),
            '.' => Some(Token::Dot),
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            '[' => Some(Token::LeftSquare),
            ']' => Some(Token::RightSquare),
            '{' => Some(Token::LeftBrace),
            '}' => Some(Token::RightBrace),
            '=' => match self.chars.peek() {
                Some((_, '=')) => {
                    self.chars.next();
                    Some(Token::EqualEqual)
                }
                _ => Some(Token::Equal),
            },
            '!' => match self.chars.peek() {
                Some((_, '=')) => {
                    self.chars.next();
                    Some(Token::NotEqual)
                }
                _ => Some(Token::Bang),
            },
            '<' => match self.chars.peek() {
                Some((_, '=')) => {
                    self.chars.next();
                    Some(Token::LessEqual)
                }
                _ => Some(Token::LeftAngle),
            },
            '>' => match self.chars.peek() {
                Some((_, '=')) => {
                    self.chars.next();
                    Some(Token::GreaterEqual)
                }
                _ => Some(Token::RightAngle),
            },
            '&' => match self.chars.peek() {
                Some((_, '&')) => {
                    self.chars.next();
                    Some(Token::AmperAmper)
                }
                _ => Some(Token::Amper),
            },
            '|' => match self.chars.peek() {
                Some((_, '|')) => {
                    self.chars.next();
                    Some(Token::VbarVbar)
                }
                _ => Some(Token::Vbar),
            },
            _ => None,
        };
        if let Some(token) = token {
            let (end, _) = *self.chars.peek().unwrap();
            return Some(Ok(TokenWithPosition {
                token,
                start: index,
                end,
            }));
        }

        return Some(Ok(match char {
            '0'..='9' => self.lex_number(index),
            '"' => self.lex_string(index),
            char if char.is_alphabetic() || char == '_' => self.lex_identifier(index),
            _ => return Some(Err(())),
        }));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_lex_numbers() {
        let mut lexer = Lexer::new("1 + 2.5");

        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithPosition {
                token: Token::Int,
                start: 0,
                end: 1
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithPosition {
                token: Token::Plus,
                start: 2,
                end: 3
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithPosition {
                token: Token::Float,
                start: 4,
                end: 7
            }))
        );
    }

    #[test]
    fn should_lex_strings() {
        let mut lexer = Lexer::new("\"hello, world\"");

        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithPosition {
                token: Token::String,
                start: 0,
                end: 14
            }))
        );
    }

    #[test]
    fn should_lex_identifiers() {
        let mut lexer = Lexer::new("foo Bar _baz");

        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithPosition {
                token: Token::Identifier,
                start: 0,
                end: 3
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithPosition {
                token: Token::Identifier,
                start: 4,
                end: 7
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(TokenWithPosition {
                token: Token::Identifier,
                start: 8,
                end: 12
            }))
        );
    }
}
