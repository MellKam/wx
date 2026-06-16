use codespan_reporting::diagnostic::{Diagnostic, Label};
use string_interner::symbol::SymbolU32;

use crate::vfs::{self, FileId};

#[cfg(test)]
mod tests;

#[derive(Copy, Clone, PartialEq)]
#[cfg_attr(test, derive(serde::Serialize))]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TextSpan {
    pub start: u32,
    pub end: u32,
}

impl TextSpan {
    pub fn new(start: u32, end: u32) -> TextSpan {
        assert!(end >= start);
        TextSpan { start, end }
    }

    pub fn merge(self, other: TextSpan) -> TextSpan {
        let start = core::cmp::min(self.start, other.start);
        let end = core::cmp::max(self.end, other.end);
        TextSpan { start, end }
    }

    #[inline]
    pub fn extract_str<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start as usize..self.end as usize]
    }
}

impl Into<core::ops::Range<usize>> for TextSpan {
    fn into(self) -> core::ops::Range<usize> {
        self.start as usize..self.end as usize
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Token {
    // Literals
    Int,
    Float,
    Char,
    String,
    Identifier,
    AtIdent,
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
    LeftArrow,
    LeftArrowEq,
    DoubleLeftArrow,
    RightArrow,
    RightArrowEq,
    DoubleRightArrow,
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
    DoubleAmper,
    Vbar,
    DoubleVbar,
    Caret,
    Hash,
    MinusRightArrow,
    // Special
    Comment,
    Whitespace,
    Unknown,
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        let text = match self {
            Int => "integer",
            Float => "float",
            Char => "char",
            String => "string",
            Identifier => "identifier",
            AtIdent => "@identifier",
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
            LeftArrow => "<",
            LeftArrowEq => "<=",
            DoubleLeftArrow => "<<",
            RightArrow => ">",
            RightArrowEq => ">=",
            DoubleRightArrow => ">>",
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
            DoubleAmper => "&&",
            Vbar => "|",
            DoubleVbar => "||",
            Caret => "^",
            Hash => "#",
            MinusRightArrow => "->",
            Comment => "comment",
            Whitespace => "whitespace",
            Unknown => "unknown token",
            Eof => "end of file",
        };
        write!(f, "{}", text)
    }
}

macro_rules! define_diagnostic_codes {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $(
                $variant:ident => $code:literal,
            )*
        }
    ) => {
        $(#[$meta])*
        $vis enum $name {
            $($variant,)*
        }

        impl $name {
            pub const fn code(&self) -> &'static str {
                match self {
                    $(Self::$variant => $code,)*
                }
            }
        }

        impl std::str::FromStr for $name {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($code => Ok(Self::$variant),)*
                    _ => Err(()),
                }
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(self.code())
            }
        }
    };
}

define_diagnostic_codes! {
    pub enum DiagnosticCode {
        UnknownToken => "E0001",
        UnexpectedToken => "E0002",
        MissingSeparator => "E0003",
        UnclosedDelimiter => "E0004",
        InvalidLiteral => "E0005",
        IncompleteExpression => "E0006",
        ChainedComparison => "E0007",
        ReservedIdentifier => "E0008",
        InvalidItem => "E0009",
        MissingInitializer => "E0010",
        InvalidAttribute => "E0012",
        InvalidNamespace => "E0013",
    }
}

fn report_unexpected_token(
    file_id: FileId,
    received: Spanned<Token>,
    expected: Token,
) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::UnexpectedToken.code())
        .with_message(format!(
            "expected `{}`, found `{}`",
            expected, received.inner
        ))
        .with_label(
            Label::primary(file_id, received.span).with_message(format!("expected `{}`", expected)),
        )
}

fn report_missing_separator(
    file_id: FileId,
    span: TextSpan,
    separator: Token,
) -> Diagnostic<FileId> {
    Diagnostic::warning()
        .with_code(DiagnosticCode::MissingSeparator.code())
        .with_message("missing separator")
        .with_label(
            Label::primary(file_id, span)
                .with_message(format!("consider adding `{}` here", separator)),
        )
}

pub struct UnclosedDelimiterDiagnostic {
    pub file_id: FileId,
    pub open_span: TextSpan,
    pub close_token: Token,
    pub expected_close_span: TextSpan,
}

fn report_unclosed_delimiter(details: UnclosedDelimiterDiagnostic) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::UnclosedDelimiter.code())
        .with_message("unclosed delimiter")
        .with_label(
            Label::primary(details.file_id, details.expected_close_span)
                .with_message(format!("consider adding `{}` here", details.close_token)),
        )
        .with_label(
            Label::secondary(details.file_id, details.open_span).with_message("unclosed delimiter"),
        )
}

fn report_invalid_integer_literal(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::InvalidLiteral.code())
        .with_message("invalid integer literal")
        .with_label(Label::primary(file_id, span))
}

fn report_invalid_float_literal(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::InvalidLiteral.code())
        .with_message("invalid float literal")
        .with_label(Label::primary(file_id, span))
}

fn report_incomplete_binary_expression(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::IncompleteExpression.code())
        .with_message("incomplete binary expression")
        .with_label(Label::primary(file_id, span))
        .with_label(
            Label::secondary(file_id, span)
                .with_message("consider adding a right-hand side operand"),
        )
}

fn report_incomplete_unary_expression(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::IncompleteExpression.code())
        .with_message("incomplete unary expression")
        .with_label(Label::primary(file_id, span))
        .with_label(Label::secondary(file_id, span).with_message("consider adding an operand"))
}

fn report_chained_comparisons(
    file_id: FileId,
    first_operator_span: TextSpan,
    second_operator_span: TextSpan,
) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::ChainedComparison.code())
        .with_message("comparison operators cannot be chained")
        .with_label(Label::primary(file_id, first_operator_span))
        .with_label(Label::primary(file_id, second_operator_span))
        .with_note("consider using logical operator like `&&` or `||` to split the comparisons or use parentheses to group them")
}

fn report_reserved_identifier(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::ReservedIdentifier.code())
        .with_message("cannot use keyword as identifier")
        .with_label(Label::primary(file_id, span))
}

fn report_invalid_namespace(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::InvalidNamespace.code())
        .with_message("invalid namespace")
        .with_label(
            Label::primary(file_id, span).with_message("namespace must be a valid identifier"),
        )
}

fn report_invalid_item(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::InvalidItem.code())
        .with_message("invalid item")
        .with_label(Label::primary(file_id, span))
}

fn report_missing_local_initializer(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::MissingInitializer.code())
        .with_message("missing initial value for local variable")
        .with_note("example syntax: local x: i32 = 0")
        .with_label(Label::primary(file_id, span))
}

fn report_missing_global_initializer(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::MissingInitializer.code())
        .with_message("missing initial value for global variable")
        .with_note("example syntax: global x: i32 = 0")
        .with_label(Label::primary(file_id, span))
}

fn report_invalid_attribute(file_id: FileId, span: TextSpan) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_code(DiagnosticCode::InvalidAttribute.code())
        .with_message("invalid attribute")
        .with_label(Label::primary(file_id, span).with_message("expected attribute name here"))
}

struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    offset: usize,
    peeked: Option<Spanned<Token>>,
}

const EOF: char = '\0';

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.chars(),
            offset: 0,
            peeked: None,
        }
    }

    fn peek(&mut self) -> Spanned<Token> {
        match &self.peeked {
            Some(token) => return token.clone(),
            None => {
                let token = self.next();
                self.peeked = Some(token.clone());
                return token;
            }
        }
    }

    fn next_if(&mut self, expected: Token) -> Option<Spanned<Token>> {
        let peeked = match &self.peeked {
            Some(token) => token.clone(),
            None => {
                let token = self.next();
                self.peeked = Some(token.clone());
                token
            }
        };
        if peeked.inner == expected {
            self.peeked = None;
            Some(peeked)
        } else {
            None
        }
    }

    fn next(&mut self) -> Spanned<Token> {
        if let Some(token) = self.peeked.take() {
            return token;
        }

        let mut unknown_span: Option<TextSpan> = None;
        loop {
            let token = self.advance();
            match token.inner {
                Token::Whitespace | Token::Comment => continue,
                Token::Unknown => {
                    unknown_span = match unknown_span {
                        Some(span) => Some(TextSpan::new(span.start, token.span.end)),
                        None => Some(token.span),
                    };
                    continue;
                }
                _ => match unknown_span {
                    Some(span) => {
                        self.peeked = Some(token.clone());
                        return Spanned {
                            inner: Token::Unknown,
                            span,
                        };
                    }
                    None => return token,
                },
            }
        }
    }

    fn advance(&mut self) -> Spanned<Token> {
        let start = self.chars.as_str().len();
        let token = match self.chars.next().unwrap_or(EOF) {
            // Patterns are ordered by frequency of occurrence in typical source code
            // Most Frequent
            ' ' | '\t' | '\n' | '\r' => self.consume_whitespace(),
            'A'..='Z' | 'a'..='z' | '_' => self.consume_identifier(),
            '0'..='9' => self.consume_number(),

            // Common Punctuation & Operators
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            '=' => self.consume_and_check('=', Token::EqEq, Token::Eq),
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,
            '#' => Token::Hash,
            '[' => Token::OpenBracket,
            ']' => Token::CloseBracket,
            '.' => Token::Dot,
            ':' => self.consume_and_check(':', Token::ColonColon, Token::Colon),
            '/' => self.consume_slash(),

            // Moderately Frequent
            '+' => self.consume_and_check('=', Token::PlusEq, Token::Plus),
            '-' => self.consume_dash(),
            '*' => self.consume_and_check('=', Token::StarEq, Token::Star),
            '<' => self.consume_open_angle(),
            '>' => self.consume_close_angle(),
            '!' => self.consume_and_check('=', Token::BangEq, Token::Bang),
            '&' => self.consume_and_check('&', Token::DoubleAmper, Token::Amper),
            '|' => self.consume_and_check('|', Token::DoubleVbar, Token::Vbar),
            '"' => self.consume_string(),

            // Less Frequent
            '%' => self.consume_and_check('=', Token::PercentEq, Token::Percent),
            '^' => Token::Caret,
            '\'' => self.consume_char(),
            '@' => self.consume_at_ident(),
            '\0' => Token::Eof,
            _ => Token::Unknown,
        };
        let length = start - self.chars.as_str().len();
        let span = TextSpan::new(self.offset as u32, (self.offset + length) as u32);
        self.offset += length;

        Spanned { inner: token, span }
    }

    fn consume_and_check(&mut self, expect: char, token: Token, fallback: Token) -> Token {
        match self.chars.clone().next().unwrap_or(EOF) {
            ch if ch == expect => {
                _ = self.chars.next();
                return token;
            }
            _ => return fallback,
        }
    }

    fn consume_dash(&mut self) -> Token {
        let mut lookahead = self.chars.clone();
        match lookahead.next().unwrap_or(EOF) {
            '=' => {
                _ = self.chars.next();
                return Token::MinusEq;
            }
            '>' => {
                _ = self.chars.next();
                return Token::MinusRightArrow;
            }
            _ => return Token::Minus,
        }
    }

    fn consume_open_angle(&mut self) -> Token {
        let mut lookahead = self.chars.clone();
        match lookahead.next().unwrap_or(EOF) {
            '=' => {
                _ = self.chars.next();
                return Token::LeftArrowEq;
            }
            '<' => {
                _ = self.chars.next();
                return Token::DoubleLeftArrow;
            }
            _ => return Token::LeftArrow,
        }
    }

    fn consume_close_angle(&mut self) -> Token {
        let mut lookahead = self.chars.clone();
        match lookahead.next().unwrap_or(EOF) {
            '=' => {
                _ = self.chars.next();
                return Token::RightArrowEq;
            }
            '>' => {
                _ = self.chars.next();
                return Token::DoubleRightArrow;
            }
            _ => return Token::RightArrow,
        }
    }

    fn consume_identifier(&mut self) -> Token {
        let mut lookahead = self.chars.clone();
        while let Some(ch) = lookahead.next() {
            match ch {
                'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
                    _ = self.chars.next();
                }
                _ => break,
            }
        }

        Token::Identifier
    }

    fn consume_at_ident(&mut self) -> Token {
        match self.chars.clone().next().unwrap_or(EOF) {
            'A'..='Z' | 'a'..='z' | '_' => _ = self.chars.next(),
            _ => return Token::Unknown,
        }
        let mut lookahead = self.chars.clone();
        while let Some(ch) = lookahead.next() {
            match ch {
                'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => _ = self.chars.next(),
                _ => break,
            }
        }
        Token::AtIdent
    }

    fn consume_whitespace(&mut self) -> Token {
        let mut lookahead = self.chars.clone();
        while let Some(ch) = lookahead.next() {
            if ch.is_whitespace() {
                _ = self.chars.next();
            } else {
                break;
            }
        }
        Token::Whitespace
    }

    fn consume_number(&mut self) -> Token {
        let radix_marker = self.chars.clone().next();
        match radix_marker {
            Some('b' | 'B') => {
                _ = self.chars.next();
                let mut lookahead = self.chars.clone();
                while let Some(ch) = lookahead.next() {
                    match ch {
                        '0' | '1' | '_' => {
                            _ = self.chars.next();
                        }
                        _ => break,
                    }
                }
                return Token::Int;
            }
            Some('x' | 'X') => {
                _ = self.chars.next();
                let mut lookahead = self.chars.clone();
                while let Some(ch) = lookahead.next() {
                    match ch {
                        '0'..='9' | 'a'..='f' | 'A'..='F' | '_' => {
                            _ = self.chars.next();
                        }
                        _ => break,
                    }
                }
                return Token::Int;
            }
            _ => {}
        }

        let mut seen_dot = false;
        let mut lookahead = self.chars.clone();
        while let Some(ch) = lookahead.next() {
            match ch {
                '0'..='9' | '_' => {
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
            true => Token::Float,
            false => Token::Int,
        }
    }

    fn consume_string(&mut self) -> Token {
        loop {
            match self.chars.next().unwrap_or(EOF) {
                '\\' => {
                    _ = self.chars.next();
                }
                '"' | EOF => return Token::String,
                _ => (),
            }
        }
    }

    fn consume_char(&mut self) -> Token {
        loop {
            match self.chars.next().unwrap_or(EOF) {
                '\\' => {
                    _ = self.chars.next();
                }
                '\'' | EOF => return Token::Char,
                _ => (),
            }
        }
    }

    fn consume_slash(&mut self) -> Token {
        let mut lookahead = self.chars.clone();
        match lookahead.next().unwrap_or(EOF) {
            '/' => {
                _ = self.chars.next();
            }
            '=' => {
                _ = self.chars.next();
                return Token::SlashEq;
            }
            _ => return Token::Slash,
        }

        while let Some(ch) = lookahead.next() {
            match ch {
                '\n' | EOF => break,
                _ => {
                    _ = self.chars.next();
                }
            }
        }

        Token::Comment
    }
}

#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    // Logical
    And,
    Or,
    // Assignment
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
}

impl BinaryOp {
    pub const fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Rem => "%",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEq => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEq => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Assign => "=",
            BinaryOp::AddAssign => "+=",
            BinaryOp::SubAssign => "-=",
            BinaryOp::MulAssign => "*=",
            BinaryOp::DivAssign => "/=",
            BinaryOp::RemAssign => "%=",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::LeftShift => "<<",
            BinaryOp::RightShift => ">>",
        }
    }

    pub fn is_assignment(&self) -> bool {
        match self {
            BinaryOp::Assign
            | BinaryOp::AddAssign
            | BinaryOp::SubAssign
            | BinaryOp::MulAssign
            | BinaryOp::DivAssign
            | BinaryOp::RemAssign => true,
            _ => false,
        }
    }

    pub fn is_comparison(&self) -> bool {
        match self {
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => true,
            _ => false,
        }
    }

    pub fn is_logical(&self) -> bool {
        match self {
            BinaryOp::And | BinaryOp::Or => true,
            _ => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match self {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => true,
            _ => false,
        }
    }

    pub fn is_bitwise(&self) -> bool {
        match self {
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::LeftShift
            | BinaryOp::RightShift => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl TryFrom<Token> for BinaryOp {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            // Arithmetic
            Token::Plus => Ok(BinaryOp::Add),
            Token::Minus => Ok(BinaryOp::Sub),
            Token::Star => Ok(BinaryOp::Mul),
            Token::Slash => Ok(BinaryOp::Div),
            Token::Percent => Ok(BinaryOp::Rem),
            // Relational
            Token::EqEq => Ok(BinaryOp::Eq),
            Token::BangEq => Ok(BinaryOp::NotEq),
            Token::LeftArrow => Ok(BinaryOp::Less),
            Token::LeftArrowEq => Ok(BinaryOp::LessEq),
            Token::RightArrow => Ok(BinaryOp::Greater),
            Token::RightArrowEq => Ok(BinaryOp::GreaterEq),
            // Logical
            Token::DoubleAmper => Ok(BinaryOp::And),
            Token::DoubleVbar => Ok(BinaryOp::Or),
            // Assignment
            Token::Eq => Ok(BinaryOp::Assign),
            Token::PlusEq => Ok(BinaryOp::AddAssign),
            Token::MinusEq => Ok(BinaryOp::SubAssign),
            Token::StarEq => Ok(BinaryOp::MulAssign),
            Token::SlashEq => Ok(BinaryOp::DivAssign),
            Token::PercentEq => Ok(BinaryOp::RemAssign),
            // Bitwise
            Token::Amper => Ok(BinaryOp::BitAnd),
            Token::Vbar => Ok(BinaryOp::BitOr),
            Token::Caret => Ok(BinaryOp::BitXor),
            Token::DoubleLeftArrow => Ok(BinaryOp::LeftShift),
            Token::DoubleRightArrow => Ok(BinaryOp::RightShift),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum UnaryOp {
    InvertSign,
    Not,
    BitNot,
}

impl TryFrom<Token> for UnaryOp {
    type Error = ();

    fn try_from(kind: Token) -> Result<Self, Self::Error> {
        match kind {
            Token::Minus => Ok(UnaryOp::InvertSign),
            Token::Bang => Ok(UnaryOp::Not),
            Token::Caret => Ok(UnaryOp::BitNot),
            _ => Err(()),
        }
    }
}

impl UnaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::InvertSign => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "^",
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Spanned<T> {
    pub inner: T,
    pub span: TextSpan,
}

impl<T: Copy> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Spanned {
            inner: self.inner,
            span: self.span,
        }
    }
}

/// Represents an item in a separated list, storing both the item and its
/// trailing separator.
///
/// Used for comma-separated lists like function parameters `(a, b, c)` or
/// statement sequences. The `separator` field holds the token that follows this
/// item (e.g., `,` or `;`), if present.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Separated<T> {
    pub inner: T,
    pub separator: Option<TextSpan>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Expression {
    Error,
    /// `1`
    Int {
        value: i64,
    },
    /// `1.0`
    Float {
        value: f64,
    },
    /// `({expr})`
    Grouping {
        value: Box<Spanned<Expression>>,
    },
    /// `5 as i32`
    Cast {
        value: Box<Spanned<Expression>>,
        ty: Box<Spanned<TypeExpression>>,
    },
    /// `-{expr}`
    Unary {
        operator: Spanned<UnaryOp>,
        operand: Box<Spanned<Expression>>,
    },
    /// `{expr} + {expr}`
    Binary {
        left: Box<Spanned<Expression>>,
        operator: Spanned<BinaryOp>,
        right: Box<Spanned<Expression>>,
    },
    /// `{expr}::<...>(...)`
    Call {
        callee: Box<Spanned<Expression>>,
        arguments: Box<[Separated<Spanned<Expression>>]>,
    },
    /// `{expr}::<Type1, Type2>` — turbofish type application. Always the callee
    /// of a `Call`.
    TypeApplication {
        callee: Box<Spanned<Expression>>,
        args: Box<[Spanned<TypeExpression>]>,
    },
    /// `{expr}.{expr}`
    ObjectAccess {
        object: Box<Spanned<Expression>>,
        member: Spanned<SymbolU32>,
    },
    /// `{expr}.*`
    Deref {
        pointer: Box<Spanned<Expression>>,
    },
    /// `return {expr}`
    Return {
        value: Option<Box<Spanned<Expression>>>,
    },
    /// `{ ... }`
    Block {
        statements: Box<[Separated<Spanned<Statement>>]>,
    },
    /// `{identifier}: { ... }`
    Label {
        label: Spanned<SymbolU32>,
        block: Box<Spanned<Expression>>,
    },
    /// `break (:{label})? {expr}?`
    Break {
        label: Option<Spanned<SymbolU32>>,
        value: Option<Box<Spanned<Expression>>>,
    },
    /// `if {expr} { ... }`
    IfElse {
        condition: Box<Spanned<Expression>>,
        then_block: Box<Spanned<Expression>>,
        else_block: Option<Box<Spanned<Expression>>>,
    },
    /// `continue (:{label})?`
    Continue {
        label: Option<Spanned<SymbolU32>>,
    },
    /// `loop { ... }`
    Loop {
        block: Box<Spanned<Expression>>,
    },
    /// `unreachable`
    Unreachable,
    /// "hello world"
    String {
        symbol: SymbolU32,
    },
    /// 'a'
    Char {
        symbol: SymbolU32,
    },
    /// A `::` separated path: `foo`, `module::Point`, `module::Point::<i32>`.
    /// Replaces bare `Identifier`, `NamespaceAccess`, and path-typed
    /// `TypeApplication` as the canonical representation for named references.
    Path(Path),
    /// `Name::{ field: expr }` or `module::Name::<T>::{ field: expr }`
    StructInit {
        path: Box<Path>,
        fields: Box<[Separated<Spanned<StructInitField>>]>,
    },
    /// `(a, b, c)` or `()`
    Tuple {
        elements: Box<[Spanned<Expression>]>,
    },
    /// `@name<T, U>(args)`
    IntrinsicCall {
        name: Spanned<SymbolU32>,
        type_args: Box<[Spanned<TypeExpression>]>,
        arguments: Box<[Separated<Spanned<Expression>>]>,
    },
    /// `[a, b, c]`
    ArrayList {
        elements: Box<[Spanned<Expression>]>,
    },
    /// `[value; count]`
    ArrayRepeat {
        value: Box<Spanned<Expression>>,
        count: Box<Spanned<Expression>>,
    },
    /// `expr[index]`
    Index {
        object: Box<Spanned<Expression>>,
        index: Box<Spanned<Expression>>,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructInitField {
    pub name: Spanned<SymbolU32>,
    /// `None` means shorthand: `{ field }` is equivalent to `{ field: field }`
    pub value: Option<Box<Spanned<Expression>>>,
}

/// One segment of a `::` path, with optional turbofish type args.
/// `Point` → `PathSegment { ident: "Point", type_args: [] }`
/// `Point::<i32>` → `PathSegment { ident: "Point", type_args: [i32] }`
#[cfg_attr(test, derive(serde::Serialize))]
pub struct PathSegment {
    pub ident: Spanned<SymbolU32>,
    /// Turbofish args (`::<T, U>`). Empty when no type args are provided.
    pub type_args: Box<[Spanned<TypeExpression>]>,
}

/// A `::` separated path, e.g. `module::Point::<i32>`.
/// Used as the name in struct-init expressions and can represent any
/// qualified identifier regardless of nesting depth.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Path {
    pub segments: Box<[PathSegment]>,
    pub span: TextSpan,
}

impl Expression {
    pub fn is_block_like(&self) -> bool {
        match self {
            Expression::Block { .. }
            | Expression::IfElse { .. }
            | Expression::Loop { .. }
            | Expression::Label { .. }
            | Expression::StructInit { .. } => true,
            _ => false,
        }
    }
}

/// A generic type parameter declaration: `T` or `T: Bound1 + Bound2`.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypeParam {
    pub name: Spanned<SymbolU32>,
    /// Trait bounds. Empty = unconstrained.
    pub bounds: Box<[Spanned<TypeExpression>]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionTypeParam {
    pub name: Option<Spanned<SymbolU32>>,
    pub ty: Box<Spanned<TypeExpression>>,
}

/// A single argument in `Name<...>` — either a positional type or an
/// associated-type binding.
#[cfg_attr(test, derive(serde::Serialize))]
pub enum GenericArg {
    /// `i32` — positional type argument.
    Type(Spanned<TypeExpression>),
    /// `Size = u32` — associated-type binding.
    Binding {
        name: Spanned<SymbolU32>,
        ty: Spanned<TypeExpression>,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum TypeExpression {
    /// `i32`, `module::Type`, `module::Wrapper::<T>` — a flat path of segments.
    Path(Path),
    /// `fn(i32, i32) -> i32`
    Function {
        params: Box<[Separated<Spanned<FunctionTypeParam>>]>,
        result: Option<Box<Spanned<TypeExpression>>>,
    },
    /// `*mut u8`
    Pointer {
        mutability: Option<TextSpan>,
        inner: Box<Spanned<TypeExpression>>,
    },
    /// `[]mut u8`
    Slice {
        mutability: Option<TextSpan>,
        inner: Box<Spanned<TypeExpression>>,
    },
    /// `[5]mut u8`
    Array {
        size: Spanned<usize>,
        mutability: Option<TextSpan>,
        inner: Box<Spanned<TypeExpression>>,
    },
    /// `(T, U, V)` or `()`
    Tuple {
        elements: Box<[Spanned<TypeExpression>]>,
    },
    /// `heap::*mut u8`, `heap::[]i32` — memory-tagged pointer/slice/array.
    /// The memory is always a single-segment path naming the memory
    /// declaration; the inner is a Pointer, Slice, or Array type.
    MemoryTagged {
        memory: Box<Path>,
        inner: Box<Spanned<TypeExpression>>,
    },
    /// `Point<i32>` or `Memory<Size = u32>` — generic type application.
    GenericApplication {
        name: Spanned<SymbolU32>,
        args: Box<[Separated<Spanned<GenericArg>>]>,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct PatternField {
    pub name: Spanned<SymbolU32>,
    /// `None` = shorthand `{ x }`, same as `{ x: x }`
    pub pattern: Option<Spanned<Pattern>>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Pattern {
    /// `_`
    Wildcard,
    /// `[mut] name`
    Binding {
        mut_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
    },
    /// `(pat, pat, ...)`
    Tuple {
        elements: Box<[Separated<Spanned<Pattern>>]>,
    },
    /// `Name { field, other: pat, ... }`
    Struct {
        name: Spanned<SymbolU32>,
        fields: Box<[Separated<Spanned<PatternField>>]>,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Statement {
    /// `{expr}`
    Expression(Box<Spanned<Expression>>),
    /// `local <pattern> [: type] = {expr}`
    LocalDefinition {
        pattern: Spanned<Pattern>,
        ty: Option<Box<Spanned<TypeExpression>>>,
        value: Box<Spanned<Expression>>,
    },
}

impl Statement {
    pub fn is_block_like(&self) -> bool {
        match self {
            Statement::Expression(expr) => expr.inner.is_block_like(),
            Statement::LocalDefinition { .. } => false,
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionParam {
    pub mut_span: Option<TextSpan>,
    pub name: Spanned<SymbolU32>,
    pub ty: Option<Box<Spanned<TypeExpression>>>,
}

/// The payload of an attribute, following the meta item grammar.
#[cfg_attr(test, derive(serde::Serialize))]
pub enum AttributeValue {
    /// `#[inline]` — name only, no payload.
    Word,
    /// `#[lang = "memory"]` — name plus a string literal (stored raw, including quotes).
    NameValue(Spanned<SymbolU32>),
}

/// A single attribute on an item, e.g. `#[inline]` or `#[lang = "memory"]`.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Attribute {
    pub name: Spanned<SymbolU32>,
    pub value: AttributeValue,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImplItem {
    Method {
        id: DefId,
        pub_span: Option<TextSpan>,
        attributes: Box<[Attribute]>,
        signature: FunctionSignature,
        block: Box<Spanned<Expression>>,
    },
    Const {
        id: DefId,
        name: Spanned<SymbolU32>,
        ty: Option<Box<Spanned<TypeExpression>>>,
        value: Box<Spanned<Expression>>,
    },
    /// An associated type definition: `type Name = ConcreteType;`
    AssociatedType {
        id: DefId,
        name: Spanned<SymbolU32>,
        ty: Box<Spanned<TypeExpression>>,
    },
}

impl ImplItem {
    pub fn is_block_like(&self) -> bool {
        match self {
            ImplItem::Method { .. } => true,
            ImplItem::Const { .. } | ImplItem::AssociatedType { .. } => false,
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum TraitItem {
    /// A method with an optional default body.
    Function {
        id: DefId,
        pub_span: Option<TextSpan>,
        attributes: Box<[Attribute]>,
        signature: FunctionSignature,
        /// `None` = abstract (must be provided by impl); `Some` = default
        /// implementation.
        body: Option<Box<Spanned<Expression>>>,
    },
    /// An associated constant declaration (type only, no value — value comes
    /// from impl).
    Const {
        id: DefId,
        name: Spanned<SymbolU32>,
        ty: Box<Spanned<TypeExpression>>,
    },
    /// An associated type declaration: `type Name;` or `type Name: Bound1 +
    /// Bound2;` The concrete type is provided by each impl; bounds
    /// constrain what is allowed.
    AssociatedType {
        id: DefId,
        name: Spanned<SymbolU32>,
        /// Trait bounds the concrete type must satisfy. Empty = unconstrained.
        bounds: Box<[Spanned<TypeExpression>]>,
    },
}

impl TraitItem {
    pub fn is_block_like(&self) -> bool {
        match self {
            TraitItem::Function { body, .. } => body.is_some(),
            TraitItem::Const { .. } | TraitItem::AssociatedType { .. } => false,
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSignature {
    pub name: Spanned<SymbolU32>,
    /// Generic type parameters `<T, U: Bound>`. Empty = monomorphic.
    pub type_params: Box<[TypeParam]>,
    pub params: Box<[Separated<Spanned<FunctionParam>>]>,
    pub result: Option<Box<Spanned<TypeExpression>>>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ExportEntry {
    pub name: Spanned<SymbolU32>,
    pub alias: Option<Spanned<SymbolU32>>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImportDeclaration {
    Function {
        id: DefId,
        signature: FunctionSignature,
    },
    Global {
        id: DefId,
        mut_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        ty: Box<Spanned<TypeExpression>>,
    },
    Memory {
        id: DefId,
        name: Spanned<SymbolU32>,
        kind: Box<Spanned<TypeExpression>>,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct MemoryConfig {
    pub min: Option<Spanned<u32>>,
    pub max: Option<Spanned<u32>>,
}

/// Intermediate type used only during parsing of a `MemoryConfig` block.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct MemoryConfigField {
    pub name: Spanned<SymbolU32>,
    pub value: Spanned<u32>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportEntry {
    pub external_name: Option<Spanned<SymbolU32>>,
    pub declaration: ImportDeclaration,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct EnumVariant {
    pub name: Spanned<SymbolU32>,
    pub value: Option<Box<Spanned<Expression>>>,
}

#[cfg_attr(test, derive(serde::Serialize))]
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct DefId(u32);

impl DefId {
    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct DefIdGenerator {
    next_id: u32,
}

impl DefIdGenerator {
    #[inline]
    pub fn new() -> Self {
        DefIdGenerator { next_id: 0 }
    }

    #[inline]
    pub fn generate(&mut self) -> DefId {
        let id = self.next_id;
        self.next_id += 1;
        DefId(id)
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Item {
    Function {
        id: DefId,
        pub_span: Option<TextSpan>,
        attributes: Box<[Attribute]>,
        signature: FunctionSignature,
        block: Box<Spanned<Expression>>,
    },
    FunctionDeclaration {
        id: DefId,
        pub_span: Option<TextSpan>,
        attributes: Box<[Attribute]>,
        signature: FunctionSignature,
    },
    Global {
        id: DefId,
        pub_span: Option<TextSpan>,
        mut_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        ty: Option<Box<Spanned<TypeExpression>>>,
        value: Box<Spanned<Expression>>,
    },
    Export {
        entries: Box<[Separated<Spanned<ExportEntry>>]>,
    },
    Import {
        module: Spanned<SymbolU32>,
        alias: Option<Spanned<SymbolU32>>,
        entries: Box<[Separated<Spanned<ImportEntry>>]>,
    },
    Enum {
        id: DefId,
        pub_span: Option<TextSpan>,
        repr: Option<Box<Spanned<TypeExpression>>>,
        name: Spanned<SymbolU32>,
        variants: Box<[Separated<Spanned<EnumVariant>>]>,
    },
    Impl {
        target: Box<Spanned<TypeExpression>>,
        items: Box<[Separated<Spanned<ImplItem>>]>,
    },
    /// `impl Trait for Type { ... }`
    ImplTrait {
        id: DefId,
        trait_name: Box<Spanned<TypeExpression>>,
        target: Box<Spanned<TypeExpression>>,
        items: Box<[Separated<Spanned<ImplItem>>]>,
    },
    Struct {
        id: DefId,
        pub_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        type_params: Box<[TypeParam]>,
        fields: Box<[Separated<Spanned<StructField>>]>,
    },
    Memory {
        id: DefId,
        name: Spanned<SymbolU32>,
        kind: Box<Spanned<TypeExpression>>,
        config: Option<MemoryConfig>,
    },
    Const {
        id: DefId,
        pub_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        ty: Option<Box<Spanned<TypeExpression>>>,
        value: Box<Spanned<Expression>>,
    },
    Module {
        pub_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        items: Box<[Separated<Spanned<Item>>]>,
    },
    ModuleDeclaration {
        pub_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
    },
    Trait {
        id: DefId,
        pub_span: Option<TextSpan>,
        attributes: Box<[Attribute]>,
        name: Spanned<SymbolU32>,
        /// Supertrait bounds: `trait X: Y + Z { ... }`.  Empty = no bounds.
        supertraits: Box<[Spanned<TypeExpression>]>,
        items: Box<[Separated<Spanned<TraitItem>>]>,
    },
    /// `fn @name<T>(params) -> Ret;` — compiler-provided intrinsic, no body.
    IntrinsicFunction {
        id: DefId,
        signature: FunctionSignature,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructField {
    pub pub_span: Option<TextSpan>,
    pub name: Spanned<SymbolU32>,
    pub ty: Box<Spanned<TypeExpression>>,
}

impl Item {
    pub fn is_block_like(&self) -> bool {
        match self {
            Item::Global { .. }
            | Item::Const { .. }
            | Item::Memory { .. }
            | Item::FunctionDeclaration { .. }
            | Item::IntrinsicFunction { .. }
            | Item::ModuleDeclaration { .. } => false,
            Item::Function { .. }
            | Item::Export { .. }
            | Item::Import { .. }
            | Item::Enum { .. }
            | Item::Impl { .. }
            | Item::ImplTrait { .. }
            | Item::Struct { .. }
            | Item::Module { .. }
            | Item::Trait { .. } => true,
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct AST {
    pub file_id: FileId,
    pub diagnostics: Vec<Diagnostic<FileId>>,
    pub items: Vec<Separated<Spanned<Item>>>,
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum BindingPower {
    Default,
    Assignment,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Comparison,
    BitwiseShift,
    Additive,
    Multiplicative,
    Unary,
    Cast,
    Postfix,
    Member,
    Primary,
}

impl From<BinaryOp> for BindingPower {
    fn from(operator: BinaryOp) -> Self {
        match operator {
            BinaryOp::Assign => BindingPower::Assignment,
            BinaryOp::AddAssign => BindingPower::Assignment,
            BinaryOp::SubAssign => BindingPower::Assignment,
            BinaryOp::MulAssign => BindingPower::Assignment,
            BinaryOp::DivAssign => BindingPower::Assignment,
            BinaryOp::RemAssign => BindingPower::Assignment,

            BinaryOp::Or => BindingPower::LogicalOr,
            BinaryOp::And => BindingPower::LogicalAnd,

            BinaryOp::BitOr => BindingPower::BitwiseOr,
            BinaryOp::BitXor => BindingPower::BitwiseXor,
            BinaryOp::BitAnd => BindingPower::BitwiseAnd,

            BinaryOp::Eq => BindingPower::Comparison,
            BinaryOp::NotEq => BindingPower::Comparison,
            BinaryOp::Less => BindingPower::Comparison,
            BinaryOp::LessEq => BindingPower::Comparison,
            BinaryOp::Greater => BindingPower::Comparison,
            BinaryOp::GreaterEq => BindingPower::Comparison,

            BinaryOp::LeftShift => BindingPower::BitwiseShift,
            BinaryOp::RightShift => BindingPower::BitwiseShift,

            BinaryOp::Add => BindingPower::Additive,
            BinaryOp::Sub => BindingPower::Additive,

            BinaryOp::Mul => BindingPower::Multiplicative,
            BinaryOp::Div => BindingPower::Multiplicative,
            BinaryOp::Rem => BindingPower::Multiplicative,
        }
    }
}

#[derive(Clone, Copy)]
pub enum Keyword {
    Export,
    Import,
    Local,
    Global,
    Mut,
    Enum,
    Fn,
    Loop,
    Break,
    Continue,
    Return,
    If,
    Else,
    As,
    Unreachable,
    Impl,
    SelfKw,
    Struct,
    Pub,
    Memory,
    Const,
    Module,
    Trait,
    For,
    SelfType,
    Type,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(text: &str) -> Result<Self, Self::Error> {
        match text {
            "local" => Ok(Keyword::Local),
            "export" => Ok(Keyword::Export),
            "import" => Ok(Keyword::Import),
            "global" => Ok(Keyword::Global),
            "mut" => Ok(Keyword::Mut),
            "return" => Ok(Keyword::Return),
            "fn" => Ok(Keyword::Fn),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "enum" => Ok(Keyword::Enum),
            "loop" => Ok(Keyword::Loop),
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "as" => Ok(Keyword::As),
            "unreachable" => Ok(Keyword::Unreachable),
            "impl" => Ok(Keyword::Impl),
            "self" => Ok(Keyword::SelfKw),
            "struct" => Ok(Keyword::Struct),
            "pub" => Ok(Keyword::Pub),
            "memory" => Ok(Keyword::Memory),
            "const" => Ok(Keyword::Const),
            "module" => Ok(Keyword::Module),
            "trait" => Ok(Keyword::Trait),
            "for" => Ok(Keyword::For),
            "Self" => Ok(Keyword::SelfType),
            "type" => Ok(Keyword::Type),
            _ => Err(()),
        }
    }
}

pub type StringInterner =
    string_interner::StringInterner<string_interner::backend::StringBackend<SymbolU32>>;

pub struct Parser<'input> {
    source: &'input str,
    lexer: Lexer<'input>,
    interner: &'input mut StringInterner,
    ast: AST,
    id_generator: &'input mut DefIdGenerator,
}

struct SeparatedGroup<T> {
    item_handler: fn(parser: &mut Parser) -> Result<Spanned<T>, ()>,
    open_token: Token,
    close_token: Token,
    separator_token: Token,
    /// Optional callback to determine if a missing separator should emit a
    /// diagnostic. If None, always emits a diagnostic. If Some, calls the
    /// function with the item.
    should_warn_missing_separator: Option<fn(&T) -> bool>,
}

impl<T> SeparatedGroup<T> {
    fn parse(self, parser: &mut Parser) -> Result<Spanned<Box<[Separated<Spanned<T>>]>>, ()> {
        let open_span = parser.next_expect(self.open_token)?.span;
        let mut items: Vec<Separated<Spanned<T>>> = Vec::new();

        let close_span = loop {
            let token = parser.lexer.peek();
            if token.inner == self.close_token {
                break parser.lexer.next().span;
            }

            if token.inner == Token::Eof {
                let expected_close_span = match items.last() {
                    Some(last) => TextSpan::new(last.inner.span.end, last.inner.span.end),
                    None => TextSpan::new(open_span.end, open_span.end),
                };
                parser.ast.diagnostics.push(report_unclosed_delimiter(
                    UnclosedDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        close_token: self.close_token,
                        open_span,
                        expected_close_span,
                    },
                ));
                break expected_close_span;
            }

            let item = match self.parse_with_recovery(parser) {
                Some(item) => item,
                None => continue,
            };

            let next_token = parser.lexer.peek();
            if next_token.inner == self.separator_token {
                let separator_span = parser.lexer.next().span;
                items.push(Separated {
                    inner: item,
                    separator: Some(separator_span),
                });
                continue;
            }
            if next_token.inner == self.close_token {
                items.push(Separated {
                    inner: item,
                    separator: None,
                });
                break parser.lexer.next().span;
            }

            if next_token.inner == Token::Eof {
                let eof_span = TextSpan::new(item.span.end, item.span.end);
                parser.ast.diagnostics.push(report_unclosed_delimiter(
                    UnclosedDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        close_token: self.close_token,
                        open_span,
                        expected_close_span: eof_span,
                    },
                ));
                items.push(Separated {
                    inner: item,
                    separator: None,
                });
                break eof_span;
            }

            let should_warn = self
                .should_warn_missing_separator
                .map(|f| f(&item.inner))
                .unwrap_or(true);

            if should_warn {
                parser.ast.diagnostics.push(report_missing_separator(
                    parser.ast.file_id,
                    TextSpan::new(item.span.end, item.span.end),
                    self.separator_token,
                ));
            }
            items.push(Separated {
                inner: item,
                separator: None,
            });
        };

        Ok(Spanned {
            inner: items.into_boxed_slice(),
            span: TextSpan::new(open_span.start, close_span.end),
        })
    }

    fn parse_with_recovery(&self, parser: &mut Parser) -> Option<Spanned<T>> {
        loop {
            match (self.item_handler)(parser) {
                Ok(item) => return Some(item),
                Err(_) => loop {
                    let token = parser.lexer.peek();
                    match token.inner {
                        t if t == self.separator_token => {
                            parser.lexer.next();
                            return None;
                        }
                        t if t == self.close_token || t == Token::Eof => {
                            return None;
                        }
                        _ => {
                            parser.lexer.next();
                        }
                    }
                },
            }
        }
    }
}

impl<'ctx> Parser<'ctx> {
    pub fn parse(
        file_id: FileId,
        files: &'ctx vfs::Files,
        interner: &'ctx mut StringInterner,
        id_generator: &'ctx mut DefIdGenerator,
    ) -> AST {
        let source = &files.get(file_id).unwrap().source;
        let mut parser = Self {
            source,
            lexer: Lexer::new(source),
            interner,
            ast: AST {
                file_id,
                diagnostics: Vec::new(),
                items: Vec::new(),
            },
            id_generator,
        };

        loop {
            let item_attrs = match Parser::parse_attributes(&mut parser) {
                Ok(attrs) => attrs,
                Err(_) => continue,
            };

            let start_token = parser.lexer.peek();
            if start_token.inner == Token::Eof {
                break;
            }
            if start_token.inner == Token::SemiColon {
                // TODO: report unnecessary semicolon
                parser.lexer.next();
                continue;
            }

            let item_handler = match parser.get_item_handler(start_token.clone()) {
                Ok(handler) => handler,
                Err(_) => match parser.recover_from_invalid_item(start_token) {
                    Some(handler) => handler,
                    None => break,
                },
            };

            match item_handler(&mut parser) {
                Ok(mut item) => {
                    if let Item::Function {
                        ref mut attributes, ..
                    }
                    | Item::FunctionDeclaration {
                        ref mut attributes, ..
                    }
                    | Item::Trait {
                        ref mut attributes, ..
                    } = item.inner
                    {
                        *attributes = item_attrs;
                    }
                    let separator_span = if !item.inner.is_block_like() {
                        let token = parser.lexer.peek();
                        match token.inner {
                            Token::SemiColon => Some(parser.lexer.next().span),
                            _ => {
                                // TODO: report missing semicolon
                                None
                            }
                        }
                    } else {
                        None
                    };

                    parser.ast.items.push(Separated {
                        inner: item,
                        separator: separator_span,
                    });
                }
                Err(_) => continue,
            }
        }

        parser.ast
    }

    fn recover_from_invalid_item(
        &mut self,
        start_token: Spanned<Token>,
    ) -> Option<fn(parser: &mut Parser) -> Result<Spanned<Item>, ()>> {
        let mut end_token = None;
        let handler = loop {
            let token = self.lexer.peek();
            if token.inner == Token::Eof {
                break None;
            }

            match self.get_item_handler(token.clone()) {
                Ok(handler) => break Some(handler),
                Err(_) => {
                    end_token = Some(self.lexer.next());
                }
            }
        };

        self.ast.diagnostics.push(report_invalid_item(
            self.ast.file_id,
            match end_token {
                Some(end_token) => TextSpan::new(start_token.span.start, end_token.span.end),
                None => start_token.span,
            },
        ));

        handler
    }

    fn get_item_handler(
        &mut self,
        token: Spanned<Token>,
    ) -> Result<fn(parser: &mut Parser) -> Result<Spanned<Item>, ()>, ()> {
        let keyword = match token.inner {
            Token::Identifier => Keyword::try_from(token.span.extract_str(self.source)).ok(),
            _ => None,
        };

        match keyword {
            Some(Keyword::Fn) => Ok(Parser::parse_function_definition_item),
            Some(Keyword::Global) => Ok(Parser::parse_global_definition_item),
            Some(Keyword::Export) => Ok(Parser::parse_export_block),
            Some(Keyword::Import) => Ok(Parser::parse_import_block),
            Some(Keyword::Enum) => Ok(Parser::parse_enum_item),
            Some(Keyword::Impl) => Ok(Parser::parse_impl_item),
            Some(Keyword::Struct) => Ok(Parser::parse_struct_item),
            Some(Keyword::Memory) => Ok(Parser::parse_memory_item),
            Some(Keyword::Const) => Ok(Parser::parse_const_item),
            Some(Keyword::Module) => Ok(Parser::parse_module_item),
            Some(Keyword::Pub) => Ok(Parser::parse_pub_item),
            Some(Keyword::Trait) => Ok(Parser::parse_trait_item),
            _ => return Err(()),
        }
    }

    #[inline]
    fn next_expect(&mut self, expected_token: Token) -> Result<Spanned<Token>, ()> {
        let token = self.lexer.next();
        if token.inner == expected_token {
            Ok(token)
        } else {
            self.ast.diagnostics.push(report_unexpected_token(
                self.ast.file_id,
                token,
                expected_token,
            ));
            Err(())
        }
    }

    #[inline]
    fn peek_expect(&mut self, expected_token: Token) -> Result<Spanned<Token>, ()> {
        let token = self.lexer.peek();
        if token.inner == expected_token {
            Ok(token)
        } else {
            self.ast.diagnostics.push(report_unexpected_token(
                self.ast.file_id,
                token,
                expected_token,
            ));
            Err(())
        }
    }

    fn intern_identifier(&mut self, span: TextSpan) -> SymbolU32 {
        let text = span.extract_str(self.source);
        match Keyword::try_from(text) {
            Ok(_) => {
                self.ast
                    .diagnostics
                    .push(report_reserved_identifier(self.ast.file_id, span));
            }
            Err(_) => {}
        }

        self.interner.get_or_intern(text)
    }

    fn parse_attributes(parser: &mut Parser) -> Result<Box<[Attribute]>, ()> {
        let mut attrs = Vec::new();
        loop {
            if parser.lexer.peek().inner != Token::Hash {
                break;
            }
            parser.lexer.next();
            let open_bracket = parser.next_expect(Token::OpenBracket)?;
            let name_token = parser.lexer.peek();
            if name_token.inner != Token::Identifier {
                parser.ast.diagnostics.push(report_invalid_attribute(
                    parser.ast.file_id,
                    name_token.span,
                ));
                return Err(());
            }
            let name_span = parser.lexer.next().span;
            let name_symbol = parser
                .interner
                .get_or_intern(name_span.extract_str(parser.source));

            let value = if parser.lexer.next_if(Token::Eq).is_some() {
                let str_token = parser.next_expect(Token::String)?;
                let raw = str_token.span.extract_str(parser.source);
                let sym = parser.interner.get_or_intern(raw);
                AttributeValue::NameValue(Spanned {
                    inner: sym,
                    span: str_token.span,
                })
            } else {
                AttributeValue::Word
            };

            let close_token = parser.lexer.peek();
            if close_token.inner != Token::CloseBracket {
                parser.ast.diagnostics.push(report_unclosed_delimiter(
                    UnclosedDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        open_span: open_bracket.span,
                        close_token: Token::CloseBracket,
                        expected_close_span: close_token.span,
                    },
                ));
                return Err(());
            }
            parser.lexer.next();
            attrs.push(Attribute {
                name: Spanned {
                    inner: name_symbol,
                    span: name_span,
                },
                value,
            });
        }
        Ok(attrs.into_boxed_slice())
    }

    fn parse_function_param_item(parser: &mut Parser) -> Result<Spanned<FunctionParam>, ()> {
        let token = parser.peek_expect(Token::Identifier)?;
        let mut_span = match Keyword::try_from(token.span.extract_str(parser.source)) {
            Ok(Keyword::Mut) => Some(parser.lexer.next().span),
            _ => None,
        };

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let text = name_span.extract_str(parser.source);
        match Keyword::try_from(text) {
            Ok(Keyword::SelfKw) | Err(_) => {}
            Ok(_) => {
                parser
                    .ast
                    .diagnostics
                    .push(report_reserved_identifier(parser.ast.file_id, name_span));
            }
        }

        let name = Spanned {
            inner: parser.interner.get_or_intern(text),
            span: name_span,
        };

        let colon = parser.lexer.next_if(Token::Colon);
        let (ty, span) = match colon {
            Some(_) => {
                let ty = parser.parse_type_expression()?;
                let span = TextSpan::new(name_span.start, ty.span.end);
                (Some(Box::new(ty)), span)
            }
            None => (None, name_span),
        };

        Ok(Spanned {
            inner: FunctionParam { mut_span, name, ty },
            span,
        })
    }

    fn parse_function_signature(&mut self) -> Result<Spanned<FunctionSignature>, ()> {
        let fn_span = self.lexer.next();
        let name_token = self.lexer.peek();
        let name_span = match name_token.inner {
            Token::Identifier | Token::AtIdent => self.lexer.next().span,
            _ => {
                return Err(self.ast.diagnostics.push(report_unexpected_token(
                    self.ast.file_id,
                    name_token,
                    Token::Identifier,
                )));
            }
        };
        let name_symbol = self
            .interner
            .get_or_intern(name_span.extract_str(self.source));
        let name = Spanned {
            inner: name_symbol,
            span: name_span,
        };

        let type_params = if self.lexer.peek().inner == Token::LeftArrow {
            self.parse_type_params()?
        } else {
            Box::new([])
        };

        let params = SeparatedGroup {
            open_token: Token::OpenParen,
            close_token: Token::CloseParen,
            separator_token: Token::Comma,
            item_handler: Parser::parse_function_param_item,
            should_warn_missing_separator: None,
        }
        .parse(self)?;

        let result = self
            .lexer
            .next_if(Token::MinusRightArrow)
            .ok_or(())
            .and_then(|_| Ok(Some(Box::new(Parser::parse_type_expression(self)?))))
            .unwrap_or_else(|_| None);

        let span = TextSpan::new(
            fn_span.span.start,
            match &result {
                Some(result) => result.span.end,
                None => params.span.end,
            },
        );

        Ok(Spanned {
            inner: FunctionSignature {
                name,
                type_params,
                params: params.inner,
                result,
            },
            span,
        })
    }

    /// Parse `<T, U: Bound1 + Bound2>` — generic type parameter declarations.
    fn parse_type_params(&mut self) -> Result<Box<[TypeParam]>, ()> {
        let open_span = self.next_expect(Token::LeftArrow)?.span;
        let mut params: Vec<TypeParam> = Vec::new();

        loop {
            let peeked = self.lexer.peek();
            if peeked.inner == Token::RightArrow {
                self.lexer.next();
                break;
            }
            if peeked.inner == Token::Eof {
                self.ast
                    .diagnostics
                    .push(report_unclosed_delimiter(UnclosedDelimiterDiagnostic {
                        file_id: self.ast.file_id,
                        open_span,
                        close_token: Token::RightArrow,
                        expected_close_span: peeked.span,
                    }));
                break;
            }

            let name_span = self.next_expect(Token::Identifier)?.span;
            let name_symbol = self.intern_identifier(name_span);
            let name = Spanned {
                inner: name_symbol,
                span: name_span,
            };

            let bounds: Box<[Spanned<TypeExpression>]> =
                if self.lexer.next_if(Token::Colon).is_some() {
                    let mut bounds = vec![self.parse_type_expression()?];
                    while self.lexer.next_if(Token::Plus).is_some() {
                        bounds.push(self.parse_type_expression()?);
                    }
                    bounds.into_boxed_slice()
                } else {
                    Box::new([])
                };

            params.push(TypeParam { name, bounds });

            if self.lexer.peek().inner == Token::Comma {
                self.lexer.next();
            }
        }

        Ok(params.into_boxed_slice())
    }

    /// Parse `::<Type1, Type2>` turbofish arguments (the `<...>` part after
    /// `::` is already consumed by the caller). Returns the args and the
    /// closing `>` span.
    fn parse_type_args(&mut self) -> Result<(Box<[Spanned<TypeExpression>]>, TextSpan), ()> {
        let open_span = self.next_expect(Token::LeftArrow)?.span;
        let mut args: Vec<Spanned<TypeExpression>> = Vec::new();

        let close_span = loop {
            let peeked = self.lexer.peek();
            if peeked.inner == Token::RightArrow {
                break self.lexer.next().span;
            }
            if peeked.inner == Token::Eof {
                self.ast
                    .diagnostics
                    .push(report_unclosed_delimiter(UnclosedDelimiterDiagnostic {
                        file_id: self.ast.file_id,
                        open_span,
                        close_token: Token::RightArrow,
                        expected_close_span: peeked.span,
                    }));
                break peeked.span;
            }

            args.push(self.parse_type_expression()?);

            if self.lexer.peek().inner == Token::Comma {
                self.lexer.next();
            }
        };

        Ok((args.into_boxed_slice(), close_span))
    }

    fn parse_function_definition_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let signature = Parser::parse_function_signature(parser)?;

        let is_intrinsic = signature
            .inner
            .name
            .span
            .extract_str(parser.source)
            .starts_with('@');

        if is_intrinsic {
            return Ok(Spanned {
                span: signature.span,
                inner: Item::IntrinsicFunction {
                    id: parser.id_generator.generate(),
                    signature: signature.inner,
                },
            });
        }

        let block = Parser::parse_block_expression(parser).ok().map(Box::new);
        let span = TextSpan::new(
            signature.span.start,
            match &block {
                Some(block) => block.span.end,
                None => signature.span.end,
            },
        );
        Ok(Spanned {
            inner: match block {
                Some(block) => Item::Function {
                    pub_span: None,
                    attributes: Box::new([]),
                    signature: signature.inner,
                    block,
                    id: parser.id_generator.generate(),
                },
                None => Item::FunctionDeclaration {
                    pub_span: None,
                    attributes: Box::new([]),
                    signature: signature.inner,
                    id: parser.id_generator.generate(),
                },
            },
            span,
        })
    }

    fn parse_mut_span(&mut self) -> Option<TextSpan> {
        let token = self.lexer.peek();
        match token.inner {
            Token::Identifier
                if matches!(
                    Keyword::try_from(token.span.extract_str(self.source)),
                    Ok(Keyword::Mut)
                ) =>
            {
                Some(self.lexer.next().span)
            }
            _ => None,
        }
    }

    fn parse_type_expression(&mut self) -> Result<Spanned<TypeExpression>, ()> {
        self.parse_type_atom()
    }

    fn parse_type_atom(&mut self) -> Result<Spanned<TypeExpression>, ()> {
        let token = self.lexer.peek();
        match token.inner {
            Token::Star => {
                let star_span = self.lexer.next().span;
                let mutability = self.parse_mut_span();
                let inner = self.parse_type_expression()?;
                let span = TextSpan::new(star_span.start, inner.span.end);
                Ok(Spanned {
                    inner: TypeExpression::Pointer {
                        mutability,
                        inner: Box::new(inner),
                    },
                    span,
                })
            }
            Token::OpenBracket => self.parse_slice_or_array_type_expression(),
            Token::Identifier => {
                let (first_tok, first_sym) =
                    match Keyword::try_from(token.span.extract_str(self.source)) {
                        Ok(Keyword::Fn) => return Parser::parse_function_type_expression(self),
                        Ok(Keyword::SelfType) => {
                            // `Self` is valid in type paths for `Self::AssocType`, etc.
                            let tok = self.lexer.next();
                            let sym = self.interner.get_or_intern("Self");
                            (tok, sym)
                        }
                        _ => {
                            let tok = self.lexer.next();
                            let sym = self.intern_identifier(tok.span);
                            (tok, sym)
                        }
                    };
                let first_span = first_tok.span;

                // `Type<T>` — direct `<` without `::` → GenericApplication (unchanged).
                if self.lexer.peek().inner == Token::LeftArrow {
                    let args = SeparatedGroup {
                        open_token: Token::LeftArrow,
                        close_token: Token::RightArrow,
                        separator_token: Token::Comma,
                        should_warn_missing_separator: None,
                        item_handler: |parser: &mut Parser| -> Result<Spanned<GenericArg>, ()> {
                            let ty = parser.parse_type_expression()?;
                            // A plain single-segment path followed by `=` is a
                            // named associated-type binding (`Size = u32`).
                            if let TypeExpression::Path(ref p) = ty.inner {
                                if p.segments.len() == 1 && p.segments[0].type_args.is_empty() {
                                    let symbol = p.segments[0].ident.inner;
                                    if parser.lexer.next_if(Token::Eq).is_some() {
                                        let name = Spanned {
                                            inner: symbol,
                                            span: ty.span,
                                        };
                                        let val = parser.parse_type_expression()?;
                                        let span = TextSpan::new(ty.span.start, val.span.end);
                                        return Ok(Spanned {
                                            inner: GenericArg::Binding { name, ty: val },
                                            span,
                                        });
                                    }
                                }
                            }
                            let span = ty.span;
                            Ok(Spanned {
                                inner: GenericArg::Type(ty),
                                span,
                            })
                        },
                    }
                    .parse(self)?;
                    let span = TextSpan::new(first_span.start, args.span.end);
                    return Ok(Spanned {
                        inner: TypeExpression::GenericApplication {
                            name: Spanned {
                                inner: first_sym,
                                span: first_span,
                            },
                            args: args.inner,
                        },
                        span,
                    });
                }

                // Greedy path: consume `::ident` and `::<T>` eagerly.
                // When `::` is consumed but the next token is a non-path atom
                // (e.g., `*`, `[`), this is a memory-tagged type (`heap::*T`).
                let mut segments: Vec<PathSegment> = vec![PathSegment {
                    ident: Spanned {
                        inner: first_sym,
                        span: first_span,
                    },
                    type_args: Box::new([]),
                }];
                let mut path_end = first_span.end;

                loop {
                    if self.lexer.peek().inner != Token::ColonColon {
                        break;
                    }
                    self.lexer.next(); // consume `::`
                    match self.lexer.peek().inner {
                        Token::LeftArrow => {
                            let (type_args, close_span) = self.parse_type_args()?;
                            path_end = close_span.end;
                            segments.last_mut().unwrap().type_args = type_args;
                            break;
                        }
                        Token::Identifier => {
                            let seg_tok = self.lexer.next();
                            let seg_sym = self.intern_identifier(seg_tok.span);
                            path_end = seg_tok.span.end;
                            segments.push(PathSegment {
                                ident: Spanned {
                                    inner: seg_sym,
                                    span: seg_tok.span,
                                },
                                type_args: Box::new([]),
                            });
                        }
                        _ => {
                            // `::` consumed; non-identifier follows — memory-tagged type.
                            let path_span = TextSpan::new(first_span.start, path_end);
                            let path = Path {
                                segments: segments.into_boxed_slice(),
                                span: path_span,
                            };
                            let inner = self.parse_type_atom()?;
                            let span = TextSpan::new(first_span.start, inner.span.end);
                            return Ok(Spanned {
                                inner: TypeExpression::MemoryTagged {
                                    memory: Box::new(path),
                                    inner: Box::new(inner),
                                },
                                span,
                            });
                        }
                    }
                }

                let path_span = TextSpan::new(first_span.start, path_end);
                let path = Path {
                    segments: segments.into_boxed_slice(),
                    span: path_span,
                };
                Ok(Spanned {
                    inner: TypeExpression::Path(path),
                    span: path_span,
                })
            }
            Token::OpenParen => self.parse_tuple_or_paren_type_expression(),
            _ => {
                let token = self.lexer.next();
                self.ast.diagnostics.push(report_unexpected_token(
                    self.ast.file_id,
                    token,
                    Token::Identifier,
                ));
                Err(())
            }
        }
    }

    fn parse_slice_or_array_type_expression(&mut self) -> Result<Spanned<TypeExpression>, ()> {
        let open_span = self.lexer.next().span;
        let next = self.lexer.peek();
        match next.inner {
            Token::CloseBracket => {
                let _close = self.lexer.next();
                let mutability = self.parse_mut_span();
                let inner = self.parse_type_expression()?;
                let span = TextSpan::new(open_span.start, inner.span.end);
                Ok(Spanned {
                    inner: TypeExpression::Slice {
                        mutability,
                        inner: Box::new(inner),
                    },
                    span,
                })
            }
            Token::Int => {
                let size_token = self.lexer.next();
                let size_value = size_token
                    .span
                    .extract_str(self.source)
                    .parse::<usize>()
                    .map_err(|_| {
                        self.ast.diagnostics.push(report_invalid_integer_literal(
                            self.ast.file_id,
                            size_token.span,
                        ));
                    })?;
                let size = Spanned {
                    inner: size_value,
                    span: size_token.span,
                };
                self.next_expect(Token::CloseBracket)?;
                let mutability = self.parse_mut_span();
                let inner = self.parse_type_expression()?;
                let span = TextSpan::new(open_span.start, inner.span.end);
                Ok(Spanned {
                    inner: TypeExpression::Array {
                        size,
                        mutability,
                        inner: Box::new(inner),
                    },
                    span,
                })
            }
            _ => {
                self.ast.diagnostics.push(report_unexpected_token(
                    self.ast.file_id,
                    next,
                    Token::CloseBracket,
                ));
                Err(())
            }
        }
    }

    fn parse_tuple_or_paren_type_expression(&mut self) -> Result<Spanned<TypeExpression>, ()> {
        let grouped = SeparatedGroup {
            open_token: Token::OpenParen,
            close_token: Token::CloseParen,
            separator_token: Token::Comma,
            item_handler: |parser| parser.parse_type_expression(),
            should_warn_missing_separator: None,
        }
        .parse(self)?;

        let span = grouped.span;
        let mut elements = Vec::from(grouped.inner);

        if elements.len() == 1 && elements[0].separator.is_none() {
            let single = elements.remove(0);
            return Ok(Spanned {
                inner: single.inner.inner,
                span,
            });
        }

        let types: Box<[Spanned<TypeExpression>]> = elements.into_iter().map(|s| s.inner).collect();
        Ok(Spanned {
            inner: TypeExpression::Tuple { elements: types },
            span,
        })
    }

    fn parse_function_type_param(parser: &mut Parser) -> Result<Spanned<FunctionTypeParam>, ()> {
        let ty = parser.parse_type_expression()?;
        // A single-segment path followed by `:` is a named parameter (`self: T`).
        let single_ident = match &ty.inner {
            TypeExpression::Path(p)
                if p.segments.len() == 1 && p.segments[0].type_args.is_empty() =>
            {
                Some(p.segments[0].ident.clone())
            }
            _ => None,
        };
        let (name, ty) = if let Some(name_ident) = single_ident {
            if parser.lexer.next_if(Token::Colon).is_some() {
                let new_ty = parser.parse_type_expression()?;
                (Some(name_ident), Box::new(new_ty))
            } else {
                (None, Box::new(ty))
            }
        } else {
            (None, Box::new(ty))
        };

        let span = TextSpan::new(
            name.clone().map(|n| n.span).unwrap_or(ty.span).start,
            ty.span.end,
        );
        Ok(Spanned {
            inner: FunctionTypeParam { name, ty },
            span,
        })
    }

    fn parse_function_type_expression(&mut self) -> Result<Spanned<TypeExpression>, ()> {
        let func_keyword = self.lexer.next();
        let params = SeparatedGroup {
            open_token: Token::OpenParen,
            close_token: Token::CloseParen,
            separator_token: Token::Comma,
            item_handler: Parser::parse_function_type_param,
            should_warn_missing_separator: None,
        }
        .parse(self)?;

        if let Some(_) = self.lexer.next_if(Token::MinusRightArrow) {
            let result = Box::new(Parser::parse_type_expression(self)?);
            let span = TextSpan::new(func_keyword.span.start, result.span.end);
            return Ok(Spanned {
                inner: TypeExpression::Function {
                    params: params.inner,
                    result: Some(result),
                },
                span,
            });
        } else {
            let span = TextSpan::new(func_keyword.span.start, params.span.end);
            return Ok(Spanned {
                inner: TypeExpression::Function {
                    params: params.inner,
                    result: None,
                },
                span,
            });
        };
    }

    fn nud_lookup(
        &mut self,
        token: Spanned<Token>,
    ) -> Option<(
        fn(parser: &mut Parser) -> Result<Spanned<Expression>, ()>,
        BindingPower,
    )> {
        match token.inner {
            Token::Identifier => match Keyword::try_from(token.span.extract_str(self.source)) {
                Ok(Keyword::Return) => {
                    Some((Parser::parse_return_expression, BindingPower::Primary))
                }
                Ok(Keyword::If) => Some((Parser::parse_if_else_expression, BindingPower::Primary)),
                Ok(Keyword::Loop) => Some((Parser::parse_loop_expression, BindingPower::Primary)),
                Ok(Keyword::Break) => Some((Parser::parse_break_expression, BindingPower::Primary)),
                Ok(Keyword::Continue) => {
                    Some((Parser::parse_continue_expression, BindingPower::Primary))
                }
                Ok(Keyword::Unreachable) => {
                    Some((Parser::parse_unreachable_expression, BindingPower::Primary))
                }
                _ => Some((Parser::parse_identifier_expression, BindingPower::Primary)),
            },
            Token::Int => Some((Parser::parse_int_expression, BindingPower::Primary)),
            Token::Float => Some((Parser::parse_float_expression, BindingPower::Primary)),
            Token::OpenBrace => Some((Parser::parse_block_expression, BindingPower::Primary)),
            Token::OpenParen => Some((Parser::parse_grouping_expression, BindingPower::Default)),
            Token::Minus | Token::Bang | Token::Caret => {
                Some((Parser::parse_unary_expression, BindingPower::Unary))
            }
            Token::String => Some((Parser::parse_string_expression, BindingPower::Primary)),
            Token::Char => Some((Parser::parse_char_expression, BindingPower::Primary)),
            Token::AtIdent => Some((
                Parser::parse_intrinsic_call_expression,
                BindingPower::Primary,
            )),
            Token::OpenBracket => Some((Parser::parse_array_expression, BindingPower::Primary)),
            _ => None,
        }
    }

    fn led_lookup(
        &mut self,
        token: Spanned<Token>,
    ) -> Option<(
        fn(
            parser: &mut Parser,
            left: Spanned<Expression>,
            bp: BindingPower,
        ) -> Result<Spanned<Expression>, ()>,
        BindingPower,
    )> {
        match token.inner {
            Token::Plus | Token::Minus => {
                Some((Parser::parse_binary_expression, BindingPower::Additive))
            }
            Token::Star | Token::Slash | Token::Percent => Some((
                Parser::parse_binary_expression,
                BindingPower::Multiplicative,
            )),
            Token::Eq | Token::PlusEq | Token::MinusEq | Token::StarEq | Token::PercentEq => {
                Some((Parser::parse_binary_expression, BindingPower::Assignment))
            }
            Token::DoubleVbar => Some((Parser::parse_binary_expression, BindingPower::LogicalOr)),
            Token::DoubleAmper => Some((Parser::parse_binary_expression, BindingPower::LogicalAnd)),
            Token::Vbar => Some((Parser::parse_binary_expression, BindingPower::BitwiseOr)),
            Token::Caret => Some((Parser::parse_binary_expression, BindingPower::BitwiseXor)),
            Token::Amper => Some((Parser::parse_binary_expression, BindingPower::BitwiseAnd)),
            Token::EqEq
            | Token::BangEq
            | Token::LeftArrowEq
            | Token::LeftArrow
            | Token::RightArrow
            | Token::RightArrowEq => {
                Some((Parser::parse_binary_expression, BindingPower::Comparison))
            }
            Token::DoubleLeftArrow | Token::DoubleRightArrow => {
                Some((Parser::parse_binary_expression, BindingPower::BitwiseShift))
            }
            Token::OpenParen => Some((Parser::parse_call_expression, BindingPower::Postfix)),
            Token::ColonColon => Some((
                Parser::parse_type_application_expression,
                BindingPower::Member,
            )),
            Token::Identifier => match Keyword::try_from(token.span.extract_str(self.source)) {
                Ok(Keyword::As) => Some((Parser::parse_cast_expression, BindingPower::Cast)),
                _ => None,
            },
            Token::Colon => Some((Parser::parse_labelled_expression, BindingPower::Primary)),
            Token::Dot => Some((Parser::parse_object_access_expression, BindingPower::Member)),
            Token::OpenBracket => Some((Parser::parse_index_expression, BindingPower::Postfix)),
            _ => None,
        }
    }

    fn parse_expression(&mut self, limit_bp: BindingPower) -> Result<Spanned<Expression>, ()> {
        let token = self.lexer.peek();
        let nud_handler = match self.nud_lookup(token.clone()) {
            Some((nud_handler, _)) => nud_handler,
            None => return Err(()),
        };
        let mut left = nud_handler(self)?;

        loop {
            let token = self.lexer.peek();

            let (led_handler, operator_bp) = match self.led_lookup(token) {
                Some((_, bp)) if bp <= limit_bp => break,
                Some((handler, bp)) => (handler, bp),
                None => break,
            };

            left = led_handler(self, left, operator_bp)?;
        }
        Ok(left)
    }

    fn parse_identifier_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let token = parser.lexer.next();
        let text = token.span.extract_str(parser.source);

        match Keyword::try_from(text) {
            Ok(Keyword::SelfKw) | Ok(Keyword::SelfType) | Err(_) => {}
            Ok(_) => {
                parser
                    .ast
                    .diagnostics
                    .push(report_reserved_identifier(parser.ast.file_id, token.span));
            }
        }

        let symbol = parser.interner.get_or_intern(text);
        let start = token.span;

        // Start the path with the first segment (no type args yet).
        let mut segments: Vec<PathSegment> = vec![PathSegment {
            ident: Spanned {
                inner: symbol,
                span: token.span,
            },
            type_args: Box::new([]),
        }];

        loop {
            if parser.lexer.peek().inner != Token::ColonColon {
                break;
            }
            parser.lexer.next(); // consume `::`

            match parser.lexer.peek().inner {
                // `::<T, U>` — turbofish type args for the last segment
                Token::LeftArrow => {
                    let (type_args, _close) = parser.parse_type_args()?;
                    segments.last_mut().unwrap().type_args = type_args;
                    // Loop continues: next `::` starts a new segment or `::{ }`
                    // triggers struct init.
                }
                // `::{ ... }` — struct init body
                Token::OpenBrace => {
                    let path_span = TextSpan::new(start.start, parser.lexer.peek().span.start);
                    let path = Path {
                        segments: segments.into(),
                        span: path_span,
                    };
                    return Parser::parse_struct_init_expression(parser, path);
                }
                // `::ident` — next path segment
                Token::Identifier => {
                    let seg_token = parser.lexer.next();
                    let seg_text = seg_token.span.extract_str(parser.source);
                    let seg_symbol = parser.interner.get_or_intern(seg_text);
                    segments.push(PathSegment {
                        ident: Spanned {
                            inner: seg_symbol,
                            span: seg_token.span,
                        },
                        type_args: Box::new([]),
                    });
                }
                // Anything else after `::` is malformed
                _ => {
                    let bad = parser.lexer.peek();
                    parser
                        .ast
                        .diagnostics
                        .push(report_invalid_namespace(parser.ast.file_id, bad.span));
                    return Err(());
                }
            }
        }

        let end = segments.last().unwrap().ident.span;
        let span = TextSpan::new(start.start, end.end);
        Ok(Spanned {
            inner: Expression::Path(Path {
                segments: segments.into(),
                span,
            }),
            span,
        })
    }

    fn parse_struct_init_expression(
        parser: &mut Parser,
        path: Path,
    ) -> Result<Spanned<Expression>, ()> {
        let path_start = path.span.start;
        let fields = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::Comma,
            item_handler: |parser: &mut Parser| -> Result<Spanned<StructInitField>, ()> {
                let name_token = parser.next_expect(Token::Identifier)?;
                let name_symbol = parser.intern_identifier(name_token.span);

                let (value, span) = if parser.lexer.next_if(Token::Colon).is_some() {
                    let expr = parser.parse_expression(BindingPower::Default)?;
                    let span = TextSpan::new(name_token.span.start, expr.span.end);
                    (Some(Box::new(expr)), span)
                } else {
                    // Shorthand: `{ field }` — value is the local with the same name
                    (None, name_token.span)
                };

                Ok(Spanned {
                    inner: StructInitField {
                        name: Spanned {
                            inner: name_symbol,
                            span: name_token.span,
                        },
                        value,
                    },
                    span,
                })
            },
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = TextSpan::new(path_start, fields.span.end);
        Ok(Spanned {
            inner: Expression::StructInit {
                path: Box::new(path),
                fields: fields.inner,
            },
            span,
        })
    }

    fn parse_object_access_expression(
        parser: &mut Parser,
        object: Spanned<Expression>,
        _: BindingPower,
    ) -> Result<Spanned<Expression>, ()> {
        _ = parser.lexer.next(); // consume the dot
        let token = parser.lexer.next();
        let span = TextSpan::new(object.span.start, token.span.end);
        match token.inner {
            Token::Star => Ok(Spanned {
                inner: Expression::Deref {
                    pointer: Box::new(object),
                },
                span,
            }),
            Token::Identifier => {
                let member_symbol = parser.intern_identifier(token.span);
                Ok(Spanned {
                    inner: Expression::ObjectAccess {
                        object: Box::new(object),
                        member: Spanned {
                            inner: member_symbol,
                            span: token.span,
                        },
                    },
                    span,
                })
            }
            _ => {
                parser.ast.diagnostics.push(report_unexpected_token(
                    parser.ast.file_id,
                    token,
                    Token::Identifier,
                ));
                Err(())
            }
        }
    }

    fn parse_binary_expression(
        parser: &mut Parser,
        left: Spanned<Expression>,
        bp: BindingPower,
    ) -> Result<Spanned<Expression>, ()> {
        let operator_token = parser.lexer.next();
        let operator = match BinaryOp::try_from(operator_token.inner) {
            Ok(operator) => operator,
            Err(_) => unreachable!(),
        };

        let right = match parser.parse_expression(bp) {
            Ok(expr) => expr,
            Err(_) => {
                parser
                    .ast
                    .diagnostics
                    .push(report_incomplete_binary_expression(
                        parser.ast.file_id,
                        operator_token.span,
                    ));
                return Err(());
            }
        };

        if bp == BindingPower::Comparison {
            match &left.inner {
                Expression::Binary {
                    operator: left_operator,
                    ..
                } if BindingPower::from(left_operator.inner) == BindingPower::Comparison => {
                    parser.ast.diagnostics.push(report_chained_comparisons(
                        parser.ast.file_id,
                        left_operator.span,
                        operator_token.span,
                    ));
                }
                _ => {}
            }
            match &right.inner {
                Expression::Binary {
                    operator: right_operator,
                    ..
                } if BindingPower::from(right_operator.inner) == BindingPower::Comparison => {
                    parser.ast.diagnostics.push(report_chained_comparisons(
                        parser.ast.file_id,
                        operator_token.span,
                        right_operator.span,
                    ));
                }
                _ => {}
            }
        }

        let span = TextSpan::new(left.span.start, right.span.end);
        Ok(Spanned {
            inner: Expression::Binary {
                left: Box::new(left),
                right: Box::new(right),
                operator: Spanned {
                    inner: operator,
                    span: operator_token.span,
                },
            },
            span,
        })
    }

    fn parse_string_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let token = parser.lexer.next();
        let string_content = token.span.extract_str(parser.source);
        let symbol = parser.interner.get_or_intern(string_content);

        Ok(Spanned {
            inner: Expression::String { symbol },
            span: token.span,
        })
    }

    fn parse_char_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let token = parser.lexer.next();
        let raw = token.span.extract_str(parser.source);
        let symbol = parser.interner.get_or_intern(raw);

        Ok(Spanned {
            inner: Expression::Char { symbol },
            span: token.span,
        })
    }

    fn parse_int_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        fn parse_integer_literal(s: &str) -> Option<i64> {
            let s = s.replace('_', "");
            if let Some(rest) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
                i64::from_str_radix(rest, 2).ok()
            } else if let Some(rest) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
                i64::from_str_radix(rest, 16).ok()
            } else {
                s.parse::<i64>().ok()
            }
        }
        let token = parser.lexer.next();
        let value = match token.inner {
            Token::Int => parse_integer_literal(token.span.extract_str(parser.source)),
            _ => unreachable!(),
        };

        let value = match value {
            Some(value) => value,
            None => {
                parser.ast.diagnostics.push(report_invalid_integer_literal(
                    parser.ast.file_id,
                    token.span,
                ));

                0
            }
        };

        Ok(Spanned {
            inner: Expression::Int { value },
            span: token.span,
        })
    }

    fn parse_float_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let token = parser.lexer.next();
        let value = match token.inner {
            Token::Float => token.span.extract_str(parser.source).parse::<f64>().ok(),
            _ => unreachable!(),
        };

        let value = match value {
            Some(value) => value,
            None => {
                parser
                    .ast
                    .diagnostics
                    .push(report_invalid_float_literal(parser.ast.file_id, token.span));

                0.0
            }
        };

        Ok(Spanned {
            inner: Expression::Float { value },
            span: token.span,
        })
    }

    fn parse_return_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let return_span = parser.lexer.next().span;
        match parser.parse_expression(BindingPower::Default) {
            Ok(expr) => {
                let span = TextSpan::new(return_span.start, expr.span.end);
                Ok(Spanned {
                    inner: Expression::Return {
                        value: Some(Box::new(expr)),
                    },
                    span,
                })
            }
            Err(_) => Ok(Spanned {
                inner: Expression::Return { value: None },
                span: return_span,
            }),
        }
    }

    fn parse_break_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let break_keyword = parser.lexer.next();
        let label = match parser.lexer.next_if(Token::Colon) {
            Some(_) => {
                let label_span = parser.next_expect(Token::Identifier)?.span;
                let label_symbol = parser.intern_identifier(label_span);

                Some(Spanned {
                    inner: label_symbol,
                    span: label_span,
                })
            }
            None => None,
        };
        let value = parser.parse_expression(BindingPower::Default).ok();

        let span = match (label.clone(), &value) {
            (_, Some(value)) => TextSpan::new(break_keyword.span.start, value.span.end),
            (Some(label), None) => TextSpan::new(break_keyword.span.start, label.span.end),
            (None, None) => break_keyword.span,
        };
        Ok(Spanned {
            inner: Expression::Break {
                label,
                value: value.map(Box::new),
            },
            span,
        })
    }

    fn parse_continue_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let continue_keyword = parser.lexer.next();
        let label = match parser.lexer.next_if(Token::Colon) {
            Some(_) => {
                let label_span = parser.next_expect(Token::Identifier)?.span;
                let label_symbol = parser.intern_identifier(label_span);

                Some(Spanned {
                    inner: label_symbol,
                    span: label_span,
                })
            }
            None => None,
        };

        let span = match label.clone() {
            Some(label) => TextSpan::new(continue_keyword.span.start, label.span.end),
            None => continue_keyword.span,
        };
        Ok(Spanned {
            inner: Expression::Continue { label },
            span,
        })
    }

    fn parse_grouping_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let grouped = SeparatedGroup {
            open_token: Token::OpenParen,
            close_token: Token::CloseParen,
            separator_token: Token::Comma,
            item_handler: |parser| parser.parse_expression(BindingPower::Default),
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = grouped.span;
        let mut elements = Vec::from(grouped.inner);

        // (expr) with no trailing comma — grouping, not a tuple
        if elements.len() == 1 && elements[0].separator.is_none() {
            let single = elements.remove(0);
            return Ok(Spanned {
                inner: Expression::Grouping {
                    value: Box::new(single.inner),
                },
                span,
            });
        }

        // () or (expr,) or (a, b, ...) — tuple
        let exprs: Box<[Spanned<Expression>]> = elements.into_iter().map(|s| s.inner).collect();
        Ok(Spanned {
            inner: Expression::Tuple { elements: exprs },
            span,
        })
    }

    fn parse_cast_expression(
        parser: &mut Parser,
        value: Spanned<Expression>,
        _: BindingPower,
    ) -> Result<Spanned<Expression>, ()> {
        _ = parser.lexer.next();
        let ty = parser.parse_type_expression()?;

        let span = TextSpan::new(value.span.start, ty.span.end);
        Ok(Spanned {
            inner: Expression::Cast {
                value: Box::new(value),
                ty: Box::new(ty),
            },
            span,
        })
    }

    fn parse_unary_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let operator_token = parser.lexer.next();
        let operator = match UnaryOp::try_from(operator_token.inner) {
            Ok(operator) => operator,
            Err(_) => unreachable!(),
        };

        let operand = match parser.parse_expression(BindingPower::Unary) {
            Ok(operand) => operand,
            Err(_) => {
                parser
                    .ast
                    .diagnostics
                    .push(report_incomplete_unary_expression(
                        parser.ast.file_id,
                        operator_token.span,
                    ));
                return Err(());
            }
        };

        let span = TextSpan::new(operator_token.span.start, operand.span.end);
        Ok(Spanned {
            inner: Expression::Unary {
                operator: Spanned {
                    inner: operator,
                    span: operator_token.span,
                },
                operand: Box::new(operand),
            },
            span,
        })
    }

    fn parse_labelled_expression(
        parser: &mut Parser,
        label_expr: Spanned<Expression>,
        _: BindingPower,
    ) -> Result<Spanned<Expression>, ()> {
        let label = match label_expr.inner {
            Expression::Path(path)
                if path.segments.len() == 1 && path.segments[0].type_args.is_empty() =>
            {
                path.segments.into_vec().remove(0).ident
            }
            _ => unreachable!(),
        };
        let colon_token = parser.next_expect(Token::Colon)?;

        let block = parser.parse_expression(BindingPower::Default)?;
        match block.inner {
            Expression::Block { .. } | Expression::IfElse { .. } | Expression::Loop { .. } => {}
            _ => {
                let mut diag = Diagnostic::error()
                    .with_code(DiagnosticCode::IncompleteExpression.code())
                    .with_message("expected a block-like expression after a label")
                    .with_label(Label::primary(parser.ast.file_id, block.span));
                match &block.inner {
                    Expression::Path(_) | Expression::Call { .. } => {
                        diag = diag.with_label(
                            Label::secondary(parser.ast.file_id, colon_token.span)
                                .with_message("use `::` here for namespace access"),
                        );
                    }
                    _ => {}
                }

                parser.ast.diagnostics.push(diag);
                return Err(());
            }
        }

        let span = TextSpan::new(label.span.start, block.span.end);
        Ok(Spanned {
            inner: Expression::Label {
                label,
                block: Box::new(block),
            },
            span,
        })
    }

    fn parse_block_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let statements = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::SemiColon,
            item_handler: |parser| {
                let token = parser.lexer.peek();
                let keyword = match token.inner {
                    Token::Identifier => {
                        Keyword::try_from(token.span.extract_str(parser.source)).ok()
                    }
                    _ => None,
                };
                match keyword {
                    Some(Keyword::Local) => Parser::parse_local_definition_statement(parser),
                    _ => {
                        let expr = parser.parse_expression(BindingPower::Default)?;
                        let span = expr.span;
                        Ok(Spanned {
                            inner: Statement::Expression(Box::new(expr)),
                            span,
                        })
                    }
                }
            },
            should_warn_missing_separator: Some(|stmt| !Statement::is_block_like(stmt)),
        }
        .parse(parser)?;

        Ok(Spanned {
            inner: Expression::Block {
                statements: statements.inner,
            },
            span: statements.span,
        })
    }

    fn parse_loop_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let loop_keyword = parser.lexer.next();
        let block = Parser::parse_block_expression(parser)?;

        let span = TextSpan::new(loop_keyword.span.start, block.span.end);
        Ok(Spanned {
            inner: Expression::Loop {
                block: Box::new(block),
            },
            span,
        })
    }

    fn parse_if_else_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let if_keyword = parser.lexer.next();
        let condition = parser.parse_expression(BindingPower::Default)?;
        let then_block = Parser::parse_block_expression(parser)?;
        let else_keyword = Keyword::try_from(parser.lexer.peek().span.extract_str(parser.source));

        match else_keyword {
            Ok(Keyword::Else) => {
                let _ = parser.lexer.next();
                let else_block = Parser::parse_block_expression(parser)?;
                let span = TextSpan::new(if_keyword.span.start, else_block.span.end);
                Ok(Spanned {
                    inner: Expression::IfElse {
                        condition: Box::new(condition),
                        then_block: Box::new(then_block),
                        else_block: Some(Box::new(else_block)),
                    },
                    span,
                })
            }
            _ => {
                let span = TextSpan::new(if_keyword.span.start, then_block.span.end);
                Ok(Spanned {
                    inner: Expression::IfElse {
                        condition: Box::new(condition),
                        then_block: Box::new(then_block),
                        else_block: None,
                    },
                    span,
                })
            }
        }
    }

    /// Handles `expr::<T, U>` turbofish when `expr` is NOT a bare identifier
    /// (e.g. `obj.method::<T>()`).  Bare-identifier paths are already fully
    /// consumed by `parse_identifier_expression`, so this led only fires on
    /// non-path left-hand sides.
    fn parse_type_application_expression(
        parser: &mut Parser,
        left: Spanned<Expression>,
        _: BindingPower,
    ) -> Result<Spanned<Expression>, ()> {
        _ = parser.lexer.next(); // consume `::`

        if parser.lexer.peek().inner == Token::LeftArrow {
            let (args, close_span) = parser.parse_type_args()?;
            let span = TextSpan::new(left.span.start, close_span.end);
            return Ok(Spanned {
                inner: Expression::TypeApplication {
                    callee: Box::new(left),
                    args,
                },
                span,
            });
        }

        let bad = parser.lexer.peek();
        parser
            .ast
            .diagnostics
            .push(report_invalid_namespace(parser.ast.file_id, bad.span));
        Err(())
    }

    fn parse_intrinsic_call_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let token = parser.lexer.next(); // consume @ident
        let name = Spanned {
            inner: parser
                .interner
                .get_or_intern(token.span.extract_str(parser.source)),
            span: token.span,
        };

        let type_args = if parser.lexer.peek().inner == Token::LeftArrow {
            let (args, _) = parser.parse_type_args()?;
            args
        } else {
            Box::new([])
        };

        let arguments = SeparatedGroup {
            open_token: Token::OpenParen,
            close_token: Token::CloseParen,
            separator_token: Token::Comma,
            item_handler: |parser| parser.parse_expression(BindingPower::Default),
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = TextSpan::new(token.span.start, arguments.span.end);
        Ok(Spanned {
            inner: Expression::IntrinsicCall {
                name,
                type_args,
                arguments: arguments.inner,
            },
            span,
        })
    }

    fn parse_call_expression(
        parser: &mut Parser,
        callee: Spanned<Expression>,
        _: BindingPower,
    ) -> Result<Spanned<Expression>, ()> {
        let arguments = SeparatedGroup {
            open_token: Token::OpenParen,
            close_token: Token::CloseParen,
            separator_token: Token::Comma,
            item_handler: |parser| parser.parse_expression(BindingPower::Default),
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = TextSpan::new(callee.span.start, arguments.span.end);
        Ok(Spanned {
            inner: Expression::Call {
                callee: Box::new(callee),
                arguments: arguments.inner,
            },
            span,
        })
    }

    fn parse_array_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let open_span = parser.next_expect(Token::OpenBracket)?.span;

        if let Some(close) = parser.lexer.next_if(Token::CloseBracket) {
            let span = TextSpan::new(open_span.start, close.span.end);
            return Ok(Spanned {
                inner: Expression::ArrayList {
                    elements: Box::new([]),
                },
                span,
            });
        }

        let first = parser.parse_expression(BindingPower::Default)?;

        if parser.lexer.next_if(Token::SemiColon).is_some() {
            let count = parser.parse_expression(BindingPower::Default)?;
            let close_span = parser.next_expect(Token::CloseBracket)?.span;
            let span = TextSpan::new(open_span.start, close_span.end);
            return Ok(Spanned {
                inner: Expression::ArrayRepeat {
                    value: Box::new(first),
                    count: Box::new(count),
                },
                span,
            });
        }

        let mut elements = vec![first];
        while parser.lexer.next_if(Token::Comma).is_some() {
            if parser.lexer.peek().inner == Token::CloseBracket {
                break;
            }
            elements.push(parser.parse_expression(BindingPower::Default)?);
        }

        let close_span = parser.next_expect(Token::CloseBracket)?.span;
        let span = TextSpan::new(open_span.start, close_span.end);
        Ok(Spanned {
            inner: Expression::ArrayList {
                elements: elements.into_boxed_slice(),
            },
            span,
        })
    }

    fn parse_index_expression(
        parser: &mut Parser,
        object: Spanned<Expression>,
        _: BindingPower,
    ) -> Result<Spanned<Expression>, ()> {
        _ = parser.lexer.next(); // consume `[`
        let index = parser.parse_expression(BindingPower::Default)?;
        let close_span = parser.next_expect(Token::CloseBracket)?.span;
        let span = TextSpan::new(object.span.start, close_span.end);
        Ok(Spanned {
            inner: Expression::Index {
                object: Box::new(object),
                index: Box::new(index),
            },
            span,
        })
    }

    fn parse_pattern_field(parser: &mut Parser) -> Result<Spanned<PatternField>, ()> {
        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name = Spanned {
            inner: parser.intern_identifier(name_span),
            span: name_span,
        };
        let pattern = if parser.lexer.next_if(Token::Colon).is_some() {
            Some(Parser::parse_pattern(parser)?)
        } else {
            None
        };
        let span = match &pattern {
            Some(p) => TextSpan::new(name_span.start, p.span.end),
            None => name_span,
        };
        Ok(Spanned {
            inner: PatternField { name, pattern },
            span,
        })
    }

    fn parse_pattern(parser: &mut Parser) -> Result<Spanned<Pattern>, ()> {
        let token = parser.lexer.peek();
        match token.inner {
            Token::OpenParen => {
                let elements = SeparatedGroup {
                    open_token: Token::OpenParen,
                    close_token: Token::CloseParen,
                    separator_token: Token::Comma,
                    should_warn_missing_separator: None,
                    item_handler: Parser::parse_pattern,
                }
                .parse(parser)?;
                Ok(Spanned {
                    inner: Pattern::Tuple {
                        elements: elements.inner,
                    },
                    span: elements.span,
                })
            }
            Token::Identifier => {
                let text = token.span.extract_str(parser.source);
                if text == "_" {
                    let span = parser.lexer.next().span;
                    return Ok(Spanned {
                        inner: Pattern::Wildcard,
                        span,
                    });
                }
                if let Ok(Keyword::Mut) = Keyword::try_from(text) {
                    let mut_token = parser.lexer.next();
                    let name_span = parser.next_expect(Token::Identifier)?.span;
                    let name = Spanned {
                        inner: parser.intern_identifier(name_span),
                        span: name_span,
                    };
                    let span = TextSpan::new(mut_token.span.start, name_span.end);
                    return Ok(Spanned {
                        inner: Pattern::Binding {
                            mut_span: Some(mut_token.span),
                            name,
                        },
                        span,
                    });
                }
                let name_token = parser.lexer.next();
                let name = Spanned {
                    inner: parser.intern_identifier(name_token.span),
                    span: name_token.span,
                };
                if parser.lexer.peek().inner == Token::OpenBrace {
                    let fields = SeparatedGroup {
                        open_token: Token::OpenBrace,
                        close_token: Token::CloseBrace,
                        separator_token: Token::Comma,
                        should_warn_missing_separator: None,
                        item_handler: Parser::parse_pattern_field,
                    }
                    .parse(parser)?;
                    let span = TextSpan::new(name_token.span.start, fields.span.end);
                    Ok(Spanned {
                        inner: Pattern::Struct {
                            name,
                            fields: fields.inner,
                        },
                        span,
                    })
                } else {
                    Ok(Spanned {
                        inner: Pattern::Binding {
                            mut_span: None,
                            name,
                        },
                        span: name_token.span,
                    })
                }
            }
            _ => {
                let token = parser.lexer.next();
                parser.ast.diagnostics.push(report_unexpected_token(
                    parser.ast.file_id,
                    token,
                    Token::Identifier,
                ));
                Err(())
            }
        }
    }

    fn parse_local_definition_statement(parser: &mut Parser) -> Result<Spanned<Statement>, ()> {
        let local_keyword = parser.lexer.next();

        let pattern = Parser::parse_pattern(parser)?;

        let ty = match parser.lexer.next_if(Token::Colon) {
            Some(_) => Some(Box::new(parser.parse_type_expression()?)),
            None => None,
        };

        let value = parser
            .lexer
            .next_if(Token::Eq)
            .ok_or(())
            .and_then(|_| parser.parse_expression(BindingPower::Default))
            .map_err(|_| {
                let token = parser.lexer.peek();
                parser
                    .ast
                    .diagnostics
                    .push(report_missing_local_initializer(
                        parser.ast.file_id,
                        token.span,
                    ));
            })?;

        let span = TextSpan::new(local_keyword.span.start, value.span.end);
        Ok(Spanned {
            inner: Statement::LocalDefinition {
                pattern,
                ty,
                value: Box::new(value),
            },
            span,
        })
    }

    fn parse_global_definition_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let global_keyword = parser.lexer.next();
        let token = parser.lexer.peek();
        let mut_span = match Keyword::try_from(token.span.extract_str(parser.source)) {
            Ok(Keyword::Mut) => {
                let mut_token = parser.lexer.next();
                Some(mut_token.span)
            }
            _ => None,
        };

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name = Spanned {
            inner: parser.intern_identifier(name_span),
            span: name_span,
        };

        let ty = match parser.lexer.next_if(Token::Colon) {
            Some(_) => Some(Box::new(parser.parse_type_expression()?)),
            None => None,
        };

        let value = parser
            .lexer
            .next_if(Token::Eq)
            .ok_or(())
            .and_then(|_| parser.parse_expression(BindingPower::Default))
            .map_err(|_| {
                let token = parser.lexer.peek();
                parser
                    .ast
                    .diagnostics
                    .push(report_missing_global_initializer(
                        parser.ast.file_id,
                        token.span,
                    ));
            })?;

        let span = TextSpan::new(global_keyword.span.start, value.span.end);
        Ok(Spanned {
            inner: Item::Global {
                pub_span: None,
                mut_span,
                name,
                ty,
                value: Box::new(value),
                id: parser.id_generator.generate(),
            },
            span,
        })
    }

    fn parse_const_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let const_span = parser.lexer.next().span;
        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name_symbol = parser.intern_identifier(name_span);

        let ty = if parser.lexer.next_if(Token::Colon).is_some() {
            Some(Box::new(parser.parse_type_expression()?))
        } else {
            None
        };

        let _ = parser.next_expect(Token::Eq)?;
        let value = parser.parse_expression(BindingPower::Default)?;

        let span = TextSpan::new(const_span.start, value.span.end);
        Ok(Spanned {
            inner: Item::Const {
                id: parser.id_generator.generate(),
                pub_span: None,
                name: Spanned {
                    inner: name_symbol,
                    span: name_span,
                },
                ty,
                value: Box::new(value),
            },
            span,
        })
    }

    fn parse_unreachable_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let unreachable_keyword = parser.lexer.next();
        Ok(Spanned {
            inner: Expression::Unreachable,
            span: unreachable_keyword.span,
        })
    }

    fn parse_export_block(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let export_keyword = parser.lexer.next();

        let entries = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::Comma,
            item_handler: |parser: &mut Parser| -> Result<Spanned<ExportEntry>, ()> {
                // Parse: name or name as "alias"
                let name_token = parser.next_expect(Token::Identifier)?;
                let name_symbol = parser.intern_identifier(name_token.span);

                // Check for optional "as" alias
                let alias = if let Token::Identifier = parser.lexer.peek().inner {
                    let potential_as = parser.lexer.peek();
                    if let Ok(Keyword::As) =
                        Keyword::try_from(potential_as.span.extract_str(parser.source))
                    {
                        _ = parser.lexer.next(); // consume "as"

                        let alias_token = parser.next_expect(Token::String)?;
                        let alias_symbol = parser.intern_identifier(alias_token.span);

                        Some(Spanned {
                            inner: alias_symbol,
                            span: alias_token.span,
                        })
                    } else {
                        None
                    }
                } else {
                    None
                };

                let span = match &alias {
                    Some(alias) => TextSpan::new(name_token.span.start, alias.span.end),
                    None => name_token.span,
                };

                Ok(Spanned {
                    inner: ExportEntry {
                        name: Spanned {
                            inner: name_symbol,
                            span: name_token.span,
                        },
                        alias: alias.clone(),
                    },
                    span,
                })
            },
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = TextSpan::new(export_keyword.span.start, entries.span.end);

        Ok(Spanned {
            inner: Item::Export {
                entries: entries.inner,
            },
            span,
        })
    }

    fn parse_enum_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let enum_span = parser.lexer.next().span;

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name_symbol = parser.intern_identifier(name_span);

        let repr = if parser.lexer.next_if(Token::Colon).is_some() {
            Some(Box::new(parser.parse_type_expression()?))
        } else {
            None
        };

        let variants = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::Comma,
            item_handler: |parser: &mut Parser| -> Result<Spanned<EnumVariant>, ()> {
                let name_token = parser.next_expect(Token::Identifier)?;
                let name_symbol = parser.intern_identifier(name_token.span);

                let (value, span) = if let Some(_) = parser.lexer.next_if(Token::Eq) {
                    let expr = parser.parse_expression(BindingPower::Default)?;
                    let span = TextSpan::new(name_token.span.start, expr.span.end);
                    (Some(Box::new(expr)), span)
                } else {
                    (None, name_token.span)
                };

                Ok(Spanned {
                    inner: EnumVariant {
                        name: Spanned {
                            inner: name_symbol,
                            span: name_token.span,
                        },
                        value,
                    },
                    span,
                })
            },
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = TextSpan::new(enum_span.start, variants.span.end);

        Ok(Spanned {
            inner: Item::Enum {
                id: parser.id_generator.generate(),
                pub_span: None,
                repr,
                name: Spanned {
                    inner: name_symbol,
                    span: name_span,
                },
                variants: variants.inner,
            },
            span,
        })
    }

    fn parse_impl_member(parser: &mut Parser) -> Result<Spanned<ImplItem>, ()> {
        let attrs = Parser::parse_attributes(parser)?;

        let token = parser.lexer.peek();
        let keyword = match token.inner {
            Token::Identifier => Keyword::try_from(token.span.extract_str(parser.source)).ok(),
            _ => None,
        };

        let pub_span = if matches!(keyword, Some(Keyword::Pub)) {
            parser.lexer.next();
            let next = parser.lexer.peek();
            Some(next.span) // will be consumed below by Keyword::Fn branch
        } else {
            None
        };

        let keyword = if pub_span.is_some() {
            match parser.lexer.peek().inner {
                Token::Identifier => {
                    Keyword::try_from(parser.lexer.peek().span.extract_str(parser.source)).ok()
                }
                _ => None,
            }
        } else {
            keyword
        };

        match keyword {
            Some(Keyword::Type) => {
                let type_span = parser.lexer.next().span;
                let name_span = parser.next_expect(Token::Identifier)?.span;
                let name_symbol = parser.intern_identifier(name_span);
                parser.next_expect(Token::Eq)?;
                let ty = parser.parse_type_expression()?;
                let span = TextSpan::new(type_span.start, ty.span.end);
                Ok(Spanned {
                    inner: ImplItem::AssociatedType {
                        id: parser.id_generator.generate(),
                        name: Spanned {
                            inner: name_symbol,
                            span: name_span,
                        },
                        ty: Box::new(Spanned {
                            inner: ty.inner,
                            span: ty.span,
                        }),
                    },
                    span,
                })
            }
            Some(Keyword::Const) => {
                let const_span = parser.lexer.next().span;
                let name_span = parser.next_expect(Token::Identifier)?.span;
                let name_symbol = parser.intern_identifier(name_span);

                let ty = if parser.lexer.next_if(Token::Colon).is_some() {
                    Some(Box::new(parser.parse_type_expression()?))
                } else {
                    None
                };

                let _ = parser.next_expect(Token::Eq)?;
                let value = parser.parse_expression(BindingPower::Default)?;
                let span = TextSpan::new(const_span.start, value.span.end);
                Ok(Spanned {
                    inner: ImplItem::Const {
                        id: parser.id_generator.generate(),
                        name: Spanned {
                            inner: name_symbol,
                            span: name_span,
                        },
                        ty,
                        value: Box::new(value),
                    },
                    span,
                })
            }
            Some(Keyword::Fn) => {
                let fn_span = parser.lexer.next().span;
                let name_span = parser.next_expect(Token::Identifier)?.span;
                let name_symbol = parser.intern_identifier(name_span);
                let type_params = if parser.lexer.peek().inner == Token::LeftArrow {
                    parser.parse_type_params()?
                } else {
                    Box::new([])
                };
                let params = SeparatedGroup {
                    open_token: Token::OpenParen,
                    close_token: Token::CloseParen,
                    separator_token: Token::Comma,
                    item_handler: Parser::parse_function_param_item,
                    should_warn_missing_separator: None,
                }
                .parse(parser)?;

                let result = parser
                    .lexer
                    .next_if(Token::MinusRightArrow)
                    .ok_or(())
                    .and_then(|_| Ok(Some(Box::new(Parser::parse_type_expression(parser)?))))
                    .unwrap_or_else(|_| None);
                let block = Parser::parse_block_expression(parser)?;
                let method_span = TextSpan::new(fn_span.start, block.span.end);
                Ok(Spanned {
                    inner: ImplItem::Method {
                        id: parser.id_generator.generate(),
                        pub_span,
                        attributes: attrs,
                        signature: FunctionSignature {
                            name: Spanned {
                                inner: name_symbol,
                                span: name_span,
                            },
                            type_params,
                            params: params.inner,
                            result,
                        },
                        block: Box::new(block),
                    },
                    span: method_span,
                })
            }
            _ => {
                let token = parser.lexer.next();
                parser.ast.diagnostics.push(report_unexpected_token(
                    parser.ast.file_id,
                    token,
                    Token::Identifier,
                ));
                Err(())
            }
        }
    }

    fn parse_impl_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let impl_span = parser.lexer.next().span;
        let first_ty = Box::new(parser.parse_type_expression()?);

        let peeked = parser.lexer.peek();
        let target = if peeked.inner == Token::Identifier
            && matches!(
                Keyword::try_from(peeked.span.extract_str(parser.source)),
                Ok(Keyword::For)
            ) {
            parser.lexer.next(); // consume `for`
            Some(Box::new(parser.parse_type_expression()?))
        } else {
            None
        };

        let items = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::SemiColon,
            item_handler: Parser::parse_impl_member,
            should_warn_missing_separator: Some(|item: &ImplItem| !item.is_block_like()),
        }
        .parse(parser)?;

        let span = TextSpan::new(impl_span.start, items.span.end);
        match target {
            Some(target) => Ok(Spanned {
                inner: Item::ImplTrait {
                    id: parser.id_generator.generate(),
                    items: items.inner,
                    target,
                    trait_name: first_ty,
                },
                span,
            }),
            None => Ok(Spanned {
                inner: Item::Impl {
                    items: items.inner,
                    target: first_ty,
                },
                span,
            }),
        }
    }

    fn parse_trait_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let trait_span = parser.lexer.next().span;

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name = Spanned {
            inner: parser.intern_identifier(name_span),
            span: name_span,
        };

        let supertraits: Box<[Spanned<TypeExpression>]> =
            if parser.lexer.next_if(Token::Colon).is_some() {
                let mut bounds = vec![parser.parse_type_expression()?];
                while parser.lexer.next_if(Token::Plus).is_some() {
                    bounds.push(parser.parse_type_expression()?);
                }
                bounds.into_boxed_slice()
            } else {
                Box::new([])
            };

        let items = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::SemiColon,
            item_handler: |parser: &mut Parser| -> Result<Spanned<TraitItem>, ()> {
                let attrs = Parser::parse_attributes(parser)?;

                let token = parser.lexer.peek();
                let keyword = match token.inner {
                    Token::Identifier => {
                        Keyword::try_from(token.span.extract_str(parser.source)).ok()
                    }
                    _ => None,
                };

                let pub_span = if matches!(keyword, Some(Keyword::Pub)) {
                    parser.lexer.next();
                    Some(parser.lexer.peek().span)
                } else {
                    None
                };

                let keyword = if pub_span.is_some() {
                    match parser.lexer.peek().inner {
                        Token::Identifier => {
                            Keyword::try_from(parser.lexer.peek().span.extract_str(parser.source))
                                .ok()
                        }
                        _ => None,
                    }
                } else {
                    keyword
                };

                match keyword {
                    Some(Keyword::Type) => {
                        let type_span = parser.lexer.next().span;
                        let name_span = parser.next_expect(Token::Identifier)?.span;
                        let name_symbol = parser.intern_identifier(name_span);
                        // Optional `: Bound1 + Bound2 + ...`
                        let bounds: Box<[Spanned<TypeExpression>]> =
                            if parser.lexer.next_if(Token::Colon).is_some() {
                                let mut list = vec![parser.parse_type_expression()?];
                                while parser.lexer.next_if(Token::Plus).is_some() {
                                    list.push(parser.parse_type_expression()?);
                                }
                                list.into_boxed_slice()
                            } else {
                                Box::new([])
                            };
                        let span = TextSpan::new(
                            type_span.start,
                            bounds.last().map(|b| b.span).unwrap_or(name_span).end,
                        );
                        Ok(Spanned {
                            inner: TraitItem::AssociatedType {
                                id: parser.id_generator.generate(),
                                name: Spanned {
                                    inner: name_symbol,
                                    span: name_span,
                                },
                                bounds,
                            },
                            span,
                        })
                    }
                    Some(Keyword::Const) => {
                        let const_span = parser.lexer.next().span;
                        let name_span = parser.next_expect(Token::Identifier)?.span;
                        let name_symbol = parser.intern_identifier(name_span);
                        parser.next_expect(Token::Colon)?;
                        let ty = parser.parse_type_expression()?;
                        let span = TextSpan::new(const_span.start, ty.span.end);
                        Ok(Spanned {
                            inner: TraitItem::Const {
                                id: parser.id_generator.generate(),
                                name: Spanned {
                                    inner: name_symbol,
                                    span: name_span,
                                },
                                ty: Box::new(ty),
                            },
                            span,
                        })
                    }
                    Some(Keyword::Fn) => {
                        let signature = parser.parse_function_signature()?;
                        let body = if parser.lexer.peek().inner == Token::OpenBrace {
                            Some(Box::new(Parser::parse_block_expression(parser)?))
                        } else {
                            None
                        };
                        let span = TextSpan::new(
                            signature.span.start,
                            match &body {
                                Some(body) => body.span.end,
                                None => signature.span.end,
                            },
                        );

                        Ok(Spanned {
                            inner: TraitItem::Function {
                                id: parser.id_generator.generate(),
                                pub_span,
                                attributes: attrs,
                                signature: signature.inner,
                                body,
                            },
                            span,
                        })
                    }
                    _ => {
                        let token = parser.lexer.next();
                        parser.ast.diagnostics.push(report_unexpected_token(
                            parser.ast.file_id,
                            token,
                            Token::Identifier,
                        ));
                        Err(())
                    }
                }
            },
            should_warn_missing_separator: Some(TraitItem::is_block_like),
        }
        .parse(parser)?;

        let span = TextSpan::new(trait_span.start, items.span.end);
        Ok(Spanned {
            inner: Item::Trait {
                id: parser.id_generator.generate(),
                pub_span: None,
                attributes: Box::new([]),
                name,
                supertraits,
                items: items.inner,
            },
            span,
        })
    }

    fn parse_struct_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let struct_span = parser.lexer.next().span;

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name_symbol = parser.intern_identifier(name_span);

        let type_params = if parser.lexer.peek().inner == Token::LeftArrow {
            parser.parse_type_params()?
        } else {
            Box::new([])
        };

        let fields = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::Comma,
            item_handler: |parser: &mut Parser| -> Result<Spanned<StructField>, ()> {
                let peek = parser.lexer.peek();
                let pub_span = match Keyword::try_from(peek.span.extract_str(parser.source)) {
                    Ok(Keyword::Pub) => Some(parser.lexer.next().span),
                    _ => None,
                };

                let name_token = parser.next_expect(Token::Identifier)?;
                let name_symbol = parser.intern_identifier(name_token.span);

                parser.next_expect(Token::Colon)?;

                let ty = parser.parse_type_expression()?;
                let span = TextSpan::new(pub_span.unwrap_or(name_token.span).start, ty.span.end);

                Ok(Spanned {
                    inner: StructField {
                        pub_span,
                        name: Spanned {
                            inner: name_symbol,
                            span: name_token.span,
                        },
                        ty: Box::new(ty),
                    },
                    span,
                })
            },
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = TextSpan::new(struct_span.start, fields.span.end);
        Ok(Spanned {
            inner: Item::Struct {
                id: parser.id_generator.generate(),
                pub_span: None,
                name: Spanned {
                    inner: name_symbol,
                    span: name_span,
                },
                type_params,
                fields: fields.inner,
            },
            span,
        })
    }

    fn parse_pub_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let pub_span = parser.lexer.next().span; // consume `pub`
        let next = parser.lexer.peek();
        let keyword = match next.inner {
            Token::Identifier => Keyword::try_from(next.span.extract_str(parser.source)).ok(),
            _ => None,
        };
        match keyword {
            Some(Keyword::Fn) => {
                let mut item = Parser::parse_function_definition_item(parser)?;
                if let Item::Function {
                    pub_span: ref mut ps,
                    ..
                } = item.inner
                {
                    *ps = Some(pub_span);
                }
                Ok(item)
            }
            Some(Keyword::Struct) => {
                let mut item = Parser::parse_struct_item(parser)?;
                if let Item::Struct {
                    pub_span: ref mut ps,
                    ..
                } = item.inner
                {
                    *ps = Some(pub_span);
                }
                Ok(item)
            }
            Some(Keyword::Module) => {
                let mut item = Parser::parse_module_item(parser)?;
                match &mut item.inner {
                    Item::Module { pub_span: ps, .. }
                    | Item::ModuleDeclaration { pub_span: ps, .. } => {
                        *ps = Some(pub_span);
                    }
                    _ => unreachable!(),
                }
                Ok(item)
            }
            Some(Keyword::Trait) => {
                let mut item = Parser::parse_trait_item(parser)?;
                if let Item::Trait {
                    pub_span: ref mut ps,
                    ..
                } = item.inner
                {
                    *ps = Some(pub_span);
                }
                Ok(item)
            }
            Some(Keyword::Global) => {
                let mut item = Parser::parse_global_definition_item(parser)?;
                if let Item::Global {
                    pub_span: ref mut ps,
                    ..
                } = item.inner
                {
                    *ps = Some(pub_span);
                }
                Ok(item)
            }
            Some(Keyword::Enum) => {
                let mut item = Parser::parse_enum_item(parser)?;
                if let Item::Enum {
                    pub_span: ref mut ps,
                    ..
                } = item.inner
                {
                    *ps = Some(pub_span);
                }
                Ok(item)
            }
            Some(Keyword::Const) => {
                let mut item = Parser::parse_const_item(parser)?;
                if let Item::Const {
                    pub_span: ref mut ps,
                    ..
                } = item.inner
                {
                    *ps = Some(pub_span);
                }
                Ok(item)
            }
            _ => {
                parser
                    .ast
                    .diagnostics
                    .push(report_invalid_item(parser.ast.file_id, pub_span));
                Err(())
            }
        }
    }

    fn parse_memory_config_field(parser: &mut Parser) -> Result<Spanned<MemoryConfigField>, ()> {
        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name = Spanned {
            inner: parser.intern_identifier(name_span),
            span: name_span,
        };
        parser.next_expect(Token::Colon)?;
        let value_token = parser.lexer.next();
        let value = match value_token.inner {
            Token::Int => {
                let raw = value_token
                    .span
                    .extract_str(parser.source)
                    .parse::<i64>()
                    .unwrap_or(0);
                if raw < 0 {
                    parser.ast.diagnostics.push(
                        codespan_reporting::diagnostic::Diagnostic::error()
                            .with_message("memory page count must be a non-negative integer")
                            .with_label(codespan_reporting::diagnostic::Label::primary(
                                parser.ast.file_id,
                                value_token.span,
                            )),
                    );
                    return Err(());
                }
                if raw > u32::MAX as i64 {
                    parser.ast.diagnostics.push(
                        codespan_reporting::diagnostic::Diagnostic::error()
                            .with_message("memory page count exceeds maximum (4294967295)")
                            .with_label(codespan_reporting::diagnostic::Label::primary(
                                parser.ast.file_id,
                                value_token.span,
                            )),
                    );
                    return Err(());
                }
                Spanned {
                    inner: raw as u32,
                    span: value_token.span,
                }
            }
            _ => {
                parser.ast.diagnostics.push(report_unexpected_token(
                    parser.ast.file_id,
                    value_token,
                    Token::Int,
                ));
                return Err(());
            }
        };
        let span = TextSpan::new(name_span.start, value.span.end);
        Ok(Spanned {
            inner: MemoryConfigField { name, value },
            span,
        })
    }

    fn parse_memory_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let memory_span = parser.lexer.next().span;

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name = Spanned {
            inner: parser.intern_identifier(name_span),
            span: name_span,
        };

        parser.next_expect(Token::Colon)?;
        let kind = parser.parse_type_expression()?;

        let config = if parser.lexer.peek().inner == Token::OpenBrace {
            let fields = SeparatedGroup {
                open_token: Token::OpenBrace,
                close_token: Token::CloseBrace,
                separator_token: Token::Comma,
                should_warn_missing_separator: None,
                item_handler: Parser::parse_memory_config_field,
            }
            .parse(parser)?;

            let mut min: Option<Spanned<u32>> = None;
            let mut max: Option<Spanned<u32>> = None;

            for field in fields.inner.iter() {
                let field_name = parser
                    .interner
                    .resolve(field.inner.inner.name.inner)
                    .unwrap_or("");
                match field_name {
                    "min" => {
                        if min.is_some() {
                            parser.ast.diagnostics.push(
                                codespan_reporting::diagnostic::Diagnostic::error()
                                    .with_message("duplicate memory property `min`")
                                    .with_label(codespan_reporting::diagnostic::Label::primary(
                                        parser.ast.file_id,
                                        field.inner.inner.name.span,
                                    )),
                            );
                        } else {
                            let v = &field.inner.inner.value;
                            min = Some(Spanned {
                                inner: v.inner,
                                span: TextSpan::new(v.span.start, v.span.end),
                            });
                        }
                    }
                    "max" => {
                        if max.is_some() {
                            parser.ast.diagnostics.push(
                                codespan_reporting::diagnostic::Diagnostic::error()
                                    .with_message("duplicate memory property `max`")
                                    .with_label(codespan_reporting::diagnostic::Label::primary(
                                        parser.ast.file_id,
                                        field.inner.inner.name.span,
                                    )),
                            );
                        } else {
                            let v = &field.inner.inner.value;
                            max = Some(Spanned {
                                inner: v.inner,
                                span: TextSpan::new(v.span.start, v.span.end),
                            });
                        }
                    }
                    _ => {
                        parser.ast.diagnostics.push(
                            codespan_reporting::diagnostic::Diagnostic::error()
                                .with_message(format!(
                                    "unknown memory property `{field_name}`, expected `min` or `max`"
                                ))
                                .with_label(codespan_reporting::diagnostic::Label::primary(
                                    parser.ast.file_id,
                                    field.inner.inner.name.span,
                                )),
                        );
                    }
                }
            }

            Some(MemoryConfig { min, max })
        } else {
            None
        };

        let span = TextSpan::new(memory_span.start, kind.span.end);
        Ok(Spanned {
            inner: Item::Memory {
                name,
                kind: Box::new(kind),
                config,
                id: parser.id_generator.generate(),
            },
            span,
        })
    }

    fn parse_global_import_declaration(&mut self) -> Result<Spanned<ImportDeclaration>, ()> {
        let keyword_span = self.lexer.next().span;
        let token = self.lexer.peek();
        let mut_span = match Keyword::try_from(token.span.extract_str(self.source)) {
            Ok(Keyword::Mut) => Some(self.lexer.next().span),
            _ => None,
        };

        let name_span = self.next_expect(Token::Identifier)?.span;
        let name_symbol = self.intern_identifier(name_span);
        let name = Spanned {
            inner: name_symbol,
            span: name_span,
        };

        let _ = self.next_expect(Token::Colon)?;
        let type_expr = self.parse_type_expression()?;
        let span = TextSpan::new(keyword_span.start, type_expr.span.end);

        Ok(Spanned {
            inner: ImportDeclaration::Global {
                mut_span,
                name,
                ty: Box::new(type_expr),
                id: self.id_generator.generate(),
            },
            span,
        })
    }

    fn parse_memory_import_declaration(&mut self) -> Result<Spanned<ImportDeclaration>, ()> {
        let keyword_span = self.lexer.next().span;
        let name_span = self.next_expect(Token::Identifier)?.span;
        let name = Spanned {
            inner: self.intern_identifier(name_span),
            span: name_span,
        };
        let _ = self.next_expect(Token::Colon)?;
        let kind = self.parse_type_expression()?;
        let span = TextSpan::new(keyword_span.start, kind.span.end);
        Ok(Spanned {
            inner: ImportDeclaration::Memory {
                name,
                kind: Box::new(kind),
                id: self.id_generator.generate(),
            },
            span,
        })
    }

    fn parse_import_entry(parser: &mut Parser) -> Result<Spanned<ImportEntry>, ()> {
        let token = parser.lexer.peek();
        let keyword = match token.inner {
            Token::Identifier => Keyword::try_from(token.span.extract_str(parser.source)).ok(),
            _ => None,
        };
        match keyword {
            Some(Keyword::Fn) => parser.parse_function_signature().map(|signature| Spanned {
                inner: ImportEntry {
                    external_name: None,
                    declaration: ImportDeclaration::Function {
                        signature: signature.inner,
                        id: parser.id_generator.generate(),
                    },
                },
                span: signature.span,
            }),
            Some(Keyword::Global) => parser
                .parse_global_import_declaration()
                .map(|decl| Spanned {
                    inner: ImportEntry {
                        external_name: None,
                        declaration: decl.inner,
                    },
                    span: decl.span,
                }),
            Some(Keyword::Memory) => parser
                .parse_memory_import_declaration()
                .map(|decl| Spanned {
                    inner: ImportEntry {
                        external_name: None,
                        declaration: decl.inner,
                    },
                    span: decl.span,
                }),
            _ => {
                parser
                    .ast
                    .diagnostics
                    .push(report_invalid_item(parser.ast.file_id, token.span));
                Err(())
            }
        }
    }

    fn parse_module_body_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let item_attrs = Parser::parse_attributes(parser).unwrap_or_default();
        let token = parser.lexer.peek();
        match parser.get_item_handler(token.clone()) {
            Ok(handler) => {
                let mut item = handler(parser)?;
                if let Item::Function {
                    ref mut attributes, ..
                }
                | Item::FunctionDeclaration {
                    ref mut attributes, ..
                } = item.inner
                {
                    *attributes = item_attrs;
                }
                Ok(item)
            }
            Err(()) => {
                parser
                    .ast
                    .diagnostics
                    .push(report_invalid_item(parser.ast.file_id, token.span));
                Err(())
            }
        }
    }

    fn parse_module_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let module_span = parser.lexer.next().span; // consume `module`

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name_symbol = parser.intern_identifier(name_span);

        if parser.lexer.peek().inner == Token::OpenBrace {
            let items = SeparatedGroup {
                open_token: Token::OpenBrace,
                close_token: Token::CloseBrace,
                separator_token: Token::SemiColon,
                item_handler: Parser::parse_module_body_item,
                should_warn_missing_separator: None,
            }
            .parse(parser)?;

            let span = TextSpan::new(module_span.start, items.span.end);
            Ok(Spanned {
                inner: Item::Module {
                    pub_span: None,
                    name: Spanned {
                        inner: name_symbol,
                        span: name_span,
                    },
                    items: items.inner,
                },
                span,
            })
        } else {
            Ok(Spanned {
                inner: Item::ModuleDeclaration {
                    pub_span: None,
                    name: Spanned {
                        inner: name_symbol,
                        span: name_span,
                    },
                },
                span: TextSpan::new(module_span.start, name_span.end),
            })
        }
    }

    fn parse_import_block(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let import_keyword = parser.lexer.next();

        let module_token = parser.next_expect(Token::String)?;
        let module_symbol = parser.intern_identifier(module_token.span);
        let module = Spanned {
            inner: module_symbol,
            span: module_token.span,
        };

        let alias = if let Token::Identifier = parser.lexer.peek().inner {
            let potential_as = parser.lexer.peek();
            if let Ok(Keyword::As) = Keyword::try_from(potential_as.span.extract_str(parser.source))
            {
                _ = parser.lexer.next(); // consume "as"

                let alias_token = parser.next_expect(Token::Identifier)?;
                let alias_symbol = parser.intern_identifier(alias_token.span);

                Some(Spanned {
                    inner: alias_symbol,
                    span: alias_token.span,
                })
            } else {
                None
            }
        } else {
            None
        };

        let entries = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::SemiColon,
            item_handler: Parser::parse_import_entry,
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = TextSpan::new(import_keyword.span.start, entries.span.end);

        Ok(Spanned {
            inner: Item::Import {
                module,
                alias,
                entries: entries.inner,
            },
            span,
        })
    }
}
