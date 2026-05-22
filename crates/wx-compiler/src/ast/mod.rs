use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files;
use string_interner::symbol::SymbolU32;

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

    #[inline]
    pub fn end_position(&self) -> TextSpan {
        TextSpan::new(self.end, self.end)
    }

    #[inline]
    pub fn start_position(&self) -> TextSpan {
        TextSpan::new(self.start, self.start)
    }
}

impl Into<core::ops::Range<usize>> for TextSpan {
    fn into(self) -> core::ops::Range<usize> {
        self.start as usize..self.end as usize
    }
}

#[derive(Clone)]
pub struct File {
    pub name: String,
    pub source: String,
    line_starts: Vec<usize>,
}

impl File {
    fn line_start(&self, line_index: usize) -> Result<usize, files::Error> {
        match line_index.cmp(&self.line_starts.len()) {
            core::cmp::Ordering::Less => Ok(*self
                .line_starts
                .get(line_index)
                .expect("failed despite previous check")),
            core::cmp::Ordering::Equal => Ok(self.source.len()),
            core::cmp::Ordering::Greater => Err(files::Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, serde::Serialize)]
pub struct FileId(u32);

pub struct Files {
    files: Vec<File>,
}

impl Files {
    pub fn new() -> Files {
        Files { files: Vec::new() }
    }

    pub fn add(&mut self, name: String, source: String) -> Option<FileId> {
        let file_id = FileId(u32::try_from(self.files.len()).ok()?);
        let line_starts = files::line_starts(&source).collect();

        self.files.push(File {
            name,
            line_starts,
            source,
        });

        Some(file_id)
    }

    pub fn get(&self, file_id: FileId) -> Result<&File, files::Error> {
        self.files
            .get(file_id.0 as usize)
            .ok_or(files::Error::FileMissing)
    }

    pub fn update(&mut self, file_id: FileId, source: String) {
        if let Some(file) = self.files.get_mut(file_id.0 as usize) {
            file.line_starts = files::line_starts(&source).collect();
            file.source = source;
        }
    }
}

impl<'files> files::Files<'files> for Files {
    type FileId = FileId;
    type Name = &'files str;
    type Source = &'files str;

    fn name(&'files self, file_id: FileId) -> Result<Self::Name, files::Error> {
        Ok(self.get(file_id)?.name.as_ref())
    }

    fn source(&'files self, file_id: FileId) -> Result<Self::Source, files::Error> {
        Ok(&self.get(file_id)?.source)
    }

    fn line_index(&self, file_id: FileId, byte_index: usize) -> Result<usize, files::Error> {
        self.get(file_id)?
            .line_starts
            .binary_search(&byte_index)
            .or_else(|next_line| Ok(next_line - 1))
    }

    fn line_range(
        &self,
        file_id: FileId,
        line_index: usize,
    ) -> Result<core::ops::Range<usize>, files::Error> {
        let file = self.get(file_id)?;
        let line_start = file.line_start(line_index)?;
        let next_line_start = file.line_start(line_index + 1)?;

        Ok(line_start..next_line_start)
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
    }
}

pub struct UnexpectedTokenDiagnostic {
    pub file_id: FileId,
    pub received: Spanned<Token>,
    pub expected: Token,
}

impl UnexpectedTokenDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::UnexpectedToken.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message(format!(
                "expected `{}`, found `{}`",
                self.expected, self.received.inner
            ))
            .with_label(
                Label::primary(self.file_id, self.received.span)
                    .with_message(format!("expected `{}`", self.expected)),
            )
    }
}

pub struct UnknownTokenDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnknownTokenDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::UnknownToken.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("unknown token")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct MissingSeparatorDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
    pub separator: Token,
}

impl MissingSeparatorDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(DiagnosticCode::MissingSeparator.code())
            .with_message("missing separator")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message(format!("consider adding `{}` here", self.separator)),
            )
    }
}

pub struct UnclosedDelimiterDiagnostic {
    pub file_id: FileId,
    pub open_span: TextSpan,
    pub close_token: Token,
    pub expected_close_span: TextSpan,
}

impl UnclosedDelimiterDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::UnclosedDelimiter.code())
            .with_message("unclosed delimiter")
            .with_label(
                Label::primary(self.file_id, self.expected_close_span)
                    .with_message(format!("consider adding `{}` here", self.close_token)),
            )
            .with_label(
                Label::secondary(self.file_id, self.open_span).with_message("unclosed delimiter"),
            )
    }
}

pub struct InvalidIntegerLiteralDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl InvalidIntegerLiteralDiagnostic {
    const CODE: &'static str = DiagnosticCode::InvalidLiteral.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("invalid integer literal")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct InvalidFloatLiteralDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl InvalidFloatLiteralDiagnostic {
    const CODE: &'static str = DiagnosticCode::InvalidLiteral.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("invalid float literal")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct IncompleteBinaryExpressionDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl IncompleteBinaryExpressionDiagnostic {
    const CODE: &'static str = DiagnosticCode::IncompleteExpression.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("incomplete binary expression")
            .with_label(Label::primary(self.file_id, self.span))
            .with_label(
                Label::secondary(self.file_id, self.span)
                    .with_message("consider adding a right-hand side operand"),
            )
    }
}

pub struct IncompleteUnaryExpressionDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl IncompleteUnaryExpressionDiagnostic {
    const CODE: &'static str = DiagnosticCode::IncompleteExpression.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("incomplete unary expression")
            .with_label(Label::primary(self.file_id, self.span))
            .with_label(
                Label::secondary(self.file_id, self.span)
                    .with_message("consider adding an operand"),
            )
    }
}

pub struct ChainedComparisonsDiagnostic {
    pub file_id: FileId,
    pub first_operator_span: TextSpan,
    pub second_operator_span: TextSpan,
}

impl ChainedComparisonsDiagnostic {
    const CODE: &'static str = DiagnosticCode::ChainedComparison.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("comparison operators cannot be chained")
            .with_label(Label::primary(self.file_id, self.first_operator_span))
            .with_label(Label::primary(self.file_id, self.second_operator_span))
            .with_note("consider using logical operator like `&&` or `||` to split the comparisons or use parentheses to group them")
    }
}

pub struct ReservedIdentifierDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl ReservedIdentifierDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(DiagnosticCode::ReservedIdentifier.code())
            .with_message("cannot use keyword as identifier")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct InvalidNamespaceDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl InvalidNamespaceDiagnostic {
    const CODE: &'static str = "invalid-namespace";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("invalid namespace")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("namespace must be a valid identifier"),
            )
    }
}

pub struct InvalidItemDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl InvalidItemDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::InvalidItem.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("invalid item")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct MissingLocalInitializerDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingLocalInitializerDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::MissingInitializer.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("missing initial value for local variable")
            .with_note("example syntax: local x: i32 = 0")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct MissingGlobalInitializerDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingGlobalInitializerDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::MissingInitializer.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("missing initial value for global variable")
            .with_note("example syntax: global x: i32 = 0")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct InvalidAttributeDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl InvalidAttributeDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::InvalidAttribute.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("invalid attribute")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("expected attribute name here"),
            )
    }
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
                        Some(span) => Some(TextSpan::merge(span, token.span)),
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
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF) {
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
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF) {
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
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF) {
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
        let mut peeker = self.chars.clone();
        while let Some(char) = peeker.next() {
            match char {
                'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
                    _ = self.chars.next();
                }
                _ => break,
            }
        }

        Token::Identifier
    }

    fn consume_whitespace(&mut self) -> Token {
        let mut peeker = self.chars.clone();
        while let Some(ch) = peeker.next() {
            if ch.is_whitespace() {
                _ = self.chars.next();
            } else {
                break;
            }
        }
        Token::Whitespace
    }

    fn consume_number(&mut self) -> Token {
        // Check for a base prefix immediately after the leading digit.
        // At this point `advance()` has consumed the first digit character.
        let first_remaining = self.chars.clone().next();
        match first_remaining {
            Some('b' | 'B') => {
                _ = self.chars.next(); // consume 'b'
                let mut peeker = self.chars.clone();
                while let Some(c) = peeker.next() {
                    match c {
                        '0' | '1' | '_' => {
                            _ = self.chars.next();
                        }
                        _ => break,
                    }
                }
                return Token::Int;
            }
            Some('x' | 'X') => {
                _ = self.chars.next(); // consume 'x'
                let mut peeker = self.chars.clone();
                while let Some(c) = peeker.next() {
                    match c {
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

        // Decimal integer or float; underscores are allowed as separators.
        let mut seen_dot = false;
        let mut peeker = self.chars.clone();
        while let Some(char) = peeker.next() {
            match char {
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
                    // Escape sequence - skip next character
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
                    // Escape sequence - skip next character
                    _ = self.chars.next();
                }
                '\'' | EOF => return Token::Char,
                _ => (),
            }
        }
    }

    fn consume_slash(&mut self) -> Token {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF) {
            '/' => {
                _ = self.chars.next();
            }
            '=' => {
                _ = self.chars.next();
                return Token::SlashEq;
            }
            _ => return Token::Slash,
        }

        while let Some(char) = peeker.next() {
            match char {
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
    pub fn as_str(&self) -> &str {
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

impl Clone for Spanned<Token> {
    fn clone(&self) -> Self {
        Spanned {
            inner: self.inner,
            span: self.span,
        }
    }
}

impl Clone for Spanned<SymbolU32> {
    fn clone(&self) -> Self {
        Spanned {
            inner: self.inner,
            span: self.span,
        }
    }
}

impl Clone for Spanned<BinaryOp> {
    fn clone(&self) -> Self {
        Spanned {
            inner: self.inner,
            span: self.span,
        }
    }
}

impl Clone for Spanned<UnaryOp> {
    fn clone(&self) -> Self {
        Spanned {
            inner: self.inner,
            span: self.span,
        }
    }
}

impl Clone for Spanned<u32> {
    fn clone(&self) -> Self {
        Spanned {
            inner: self.inner,
            span: self.span,
        }
    }
}

/// Represents content enclosed by matching delimiter tokens.
///
/// Used for parenthesized expressions `(expr)`, function parameter lists `(a,
/// b, c)`, code blocks `{ stmt; stmt; }`, and array literals `[1, 2, 3]`. The
/// `open` and `close` fields store the spans of the opening and closing
/// delimiters respectively.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Grouped<T> {
    pub open: TextSpan,
    pub inner: T,
    pub close: TextSpan,
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
    /// `x`
    Identifier {
        symbol: SymbolU32,
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
    /// `{expr}::{expr}`
    NamespaceAccess {
        namespace: Box<Spanned<TypeExpression>>,
        member: Spanned<SymbolU32>,
    },
    /// `{expr}.{expr}`
    ObjectAccess {
        object: Box<Spanned<Expression>>,
        member: Spanned<SymbolU32>,
    },
    TupleFieldAccess {
        object: Box<Spanned<Expression>>,
        field: Spanned<u32>,
    },
    /// `return {expr}`
    Return {
        value: Option<Box<Spanned<Expression>>>,
    },
    /// `{ ... }`
    Block {
        statements: Grouped<Box<[Separated<Spanned<Statement>>]>>,
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
    /// `Ident { field: expr, field }`
    StructInit {
        name: Spanned<SymbolU32>,
        fields: Grouped<Box<[Separated<Spanned<StructInitField>>]>>,
    },
    /// `(a, b, c)` or `()`
    Tuple {
        elements: Box<[Spanned<Expression>]>,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructInitField {
    pub name: Spanned<SymbolU32>,
    /// `None` means shorthand: `{ field }` is equivalent to `{ field: field }`
    pub value: Option<Box<Spanned<Expression>>>,
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

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionTypeParam {
    pub name: Option<Spanned<SymbolU32>>,
    pub ty: Box<Spanned<TypeExpression>>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum TypeExpression {
    /// `i32`
    Identifier { symbol: SymbolU32 },
    /// `fn(i32, i32) -> i32`
    Function {
        params: Grouped<Box<[Separated<Spanned<FunctionTypeParam>>]>>,
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
    /// `impl Trait`
    ImplTrait { name: Spanned<SymbolU32> },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Statement {
    /// `{expr}`
    Expression(Box<Spanned<Expression>>),
    /// `local (mut)? {identifier}(: {type})? = {expr}`
    LocalDefinition {
        mut_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
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

/// A single `#[name]` attribute on a function or method.
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Attribute {
    pub name: Spanned<SymbolU32>,
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
}

impl ImplItem {
    pub fn is_block_like(&self) -> bool {
        match self {
            ImplItem::Method { .. } => true,
            ImplItem::Const { .. } => false,
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
}

impl TraitItem {
    pub fn is_block_like(&self) -> bool {
        match self {
            TraitItem::Function { body, .. } => body.is_some(),
            TraitItem::Const { .. } => false,
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSignature {
    pub name: Spanned<SymbolU32>,
    pub params: Grouped<Box<[Separated<Spanned<FunctionParam>>]>>,
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
    pub fn as_u32(self) -> u32 {
        self.0
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
        mut_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        ty: Option<Box<Spanned<TypeExpression>>>,
        value: Box<Spanned<Expression>>,
    },
    Export {
        entries: Grouped<Box<[Separated<Spanned<ExportEntry>>]>>,
    },
    Import {
        module: Spanned<SymbolU32>,
        alias: Option<Spanned<SymbolU32>>,
        entries: Grouped<Box<[Separated<Spanned<ImportEntry>>]>>,
    },
    Enum {
        id: DefId,
        repr: Option<Box<Spanned<TypeExpression>>>,
        name: Spanned<SymbolU32>,
        variants: Grouped<Box<[Separated<Spanned<EnumVariant>>]>>,
    },
    Impl {
        target: Box<Spanned<TypeExpression>>,
        items: Grouped<Box<[Separated<Spanned<ImplItem>>]>>,
    },
    /// `impl Trait for Type { ... }`
    ImplTrait {
        trait_name: Box<Spanned<TypeExpression>>,
        target: Box<Spanned<TypeExpression>>,
        items: Grouped<Box<[Separated<Spanned<ImplItem>>]>>,
    },
    Struct {
        id: DefId,
        pub_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        fields: Grouped<Box<[Separated<Spanned<StructField>>]>>,
    },
    Memory {
        id: DefId,
        name: Spanned<SymbolU32>,
        kind: Box<Spanned<TypeExpression>>,
    },
    Const {
        id: DefId,
        name: Spanned<SymbolU32>,
        ty: Option<Box<Spanned<TypeExpression>>>,
        value: Box<Spanned<Expression>>,
    },
    Module {
        pub_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        items: Grouped<Box<[Separated<Spanned<Item>>]>>,
    },
    Trait {
        pub_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        items: Grouped<Box<[Separated<Spanned<TraitItem>>]>>,
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
            | Item::FunctionDeclaration { .. } => false,
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
    Call,
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
    next_def_id: DefId,
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
    fn parse(self, parser: &mut Parser) -> Result<Grouped<Box<[Separated<Spanned<T>>]>>, ()> {
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
                parser.ast.diagnostics.push(
                    UnclosedDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        close_token: self.close_token,
                        open_span,
                        expected_close_span,
                    }
                    .report(),
                );
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
                parser.ast.diagnostics.push(
                    UnclosedDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        close_token: self.close_token,
                        open_span,
                        expected_close_span: eof_span,
                    }
                    .report(),
                );
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
                parser.ast.diagnostics.push(
                    MissingSeparatorDiagnostic {
                        file_id: parser.ast.file_id,
                        span: TextSpan::new(item.span.end, item.span.end),
                        separator: self.separator_token,
                    }
                    .report(),
                );
            }
            items.push(Separated {
                inner: item,
                separator: None,
            });
        };

        Ok(Grouped {
            open: open_span,
            inner: items.into_boxed_slice(),
            close: close_span,
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

impl<'input> Parser<'input> {
    pub fn parse(
        file_id: FileId,
        source: &'input str,
        interner: &'input mut StringInterner,
    ) -> AST {
        let mut parser = Self {
            source,
            lexer: Lexer::new(source),
            interner,
            ast: AST {
                file_id,
                diagnostics: Vec::new(),
                items: Vec::new(),
            },
            next_def_id: DefId(0),
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

        self.ast.diagnostics.push(
            InvalidItemDiagnostic {
                file_id: self.ast.file_id,
                span: match end_token {
                    Some(end_token) => TextSpan::merge(start_token.span, end_token.span),
                    None => start_token.span,
                },
            }
            .report(),
        );

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
            self.ast.diagnostics.push(
                UnexpectedTokenDiagnostic {
                    file_id: self.ast.file_id,
                    received: token,
                    expected: expected_token,
                }
                .report(),
            );
            Err(())
        }
    }

    #[inline]
    fn peek_expect(&mut self, expected_token: Token) -> Result<Spanned<Token>, ()> {
        let token = self.lexer.peek();
        if token.inner == expected_token {
            Ok(token)
        } else {
            self.ast.diagnostics.push(
                UnexpectedTokenDiagnostic {
                    file_id: self.ast.file_id,
                    received: token,
                    expected: expected_token,
                }
                .report(),
            );
            Err(())
        }
    }

    fn intern_identifier(&mut self, span: TextSpan) -> SymbolU32 {
        let text = span.extract_str(self.source);
        match Keyword::try_from(text) {
            Ok(_) => {
                self.ast.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: self.ast.file_id,
                        span,
                    }
                    .report(),
                );
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
                parser.ast.diagnostics.push(
                    InvalidAttributeDiagnostic {
                        file_id: parser.ast.file_id,
                        span: name_token.span,
                    }
                    .report(),
                );
                return Err(());
            }
            let name_span = parser.lexer.next().span;
            let name_symbol = parser
                .interner
                .get_or_intern(name_span.extract_str(parser.source));
            let close_token = parser.lexer.peek();
            if close_token.inner != Token::CloseBracket {
                parser.ast.diagnostics.push(
                    UnclosedDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        open_span: open_bracket.span,
                        close_token: Token::CloseBracket,
                        expected_close_span: close_token.span,
                    }
                    .report(),
                );
                return Err(());
            }
            parser.lexer.next();
            attrs.push(Attribute {
                name: Spanned {
                    inner: name_symbol,
                    span: name_span,
                },
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
                parser.ast.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: parser.ast.file_id,
                        span: name_span,
                    }
                    .report(),
                );
            }
        }

        let name = Spanned {
            inner: parser.interner.get_or_intern(text),
            span: name_span,
        };

        let colon = parser.lexer.next_if(Token::Colon);
        let (ty, span) = match colon {
            Some(colon) => {
                let token = parser.lexer.peek();
                match token.inner {
                    Token::Identifier => {
                        let ty = parser.parse_type_expression()?;
                        let span = TextSpan::merge(name_span, ty.span);
                        (Some(Box::new(ty)), span)
                    }
                    _ => (None, TextSpan::merge(name_span, colon.span)),
                }
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
        let name_span = self.next_expect(Token::Identifier)?.span;
        let name_symbol = self.intern_identifier(name_span);
        let name = Spanned {
            inner: name_symbol,
            span: name_span,
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

        let span = TextSpan::merge(
            fn_span.span,
            match &result {
                Some(result) => result.span,
                None => params.close,
            },
        );

        Ok(Spanned {
            inner: FunctionSignature {
                name,
                params,
                result,
            },
            span,
        })
    }

    #[inline]
    fn get_id(&mut self) -> DefId {
        let id = self.next_def_id;
        self.next_def_id.0 += 1;
        id
    }

    fn parse_function_definition_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let signature = Parser::parse_function_signature(parser)?;
        let block = Parser::parse_block_expression(parser).ok().map(Box::new);
        let span = TextSpan::merge(
            signature.span,
            match &block {
                Some(block) => block.span,
                None => signature.span,
            },
        );
        Ok(Spanned {
            inner: match block {
                Some(block) => Item::Function {
                    pub_span: None,
                    attributes: Box::new([]),
                    signature: signature.inner,
                    block,
                    id: parser.get_id(),
                },
                None => Item::FunctionDeclaration {
                    pub_span: None,
                    attributes: Box::new([]),
                    signature: signature.inner,
                    id: parser.get_id(),
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
        let token = self.lexer.peek();
        match token.inner {
            Token::Star => {
                let star_span = self.lexer.next().span;
                let mutability = self.parse_mut_span();
                let inner = self.parse_type_expression()?;
                let span = TextSpan::merge(star_span, inner.span);
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
                match Keyword::try_from(token.span.extract_str(self.source)) {
                    Ok(Keyword::Fn) => return Parser::parse_function_type_expression(self),
                    Ok(Keyword::Impl) => {
                        let impl_span = self.lexer.next().span;
                        let name_token = self.next_expect(Token::Identifier)?;
                        let name_symbol = self.intern_identifier(name_token.span);
                        let span = TextSpan::merge(impl_span, name_token.span);
                        return Ok(Spanned {
                            inner: TypeExpression::ImplTrait {
                                name: Spanned {
                                    inner: name_symbol,
                                    span: name_token.span,
                                },
                            },
                            span,
                        });
                    }
                    _ => {}
                }
                let token = self.lexer.next();
                Ok(Spanned {
                    inner: TypeExpression::Identifier {
                        symbol: self.intern_identifier(token.span),
                    },
                    span: token.span,
                })
            }
            Token::OpenParen => self.parse_tuple_or_paren_type_expression(),
            _ => {
                let token = self.lexer.next();
                self.ast.diagnostics.push(
                    UnexpectedTokenDiagnostic {
                        file_id: self.ast.file_id,
                        received: token,
                        expected: Token::Identifier,
                    }
                    .report(),
                );
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
                let span = TextSpan::merge(open_span, inner.span);
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
                        self.ast.diagnostics.push(
                            InvalidIntegerLiteralDiagnostic {
                                file_id: self.ast.file_id,
                                span: size_token.span,
                            }
                            .report(),
                        );
                    })?;
                let size = Spanned {
                    inner: size_value,
                    span: size_token.span,
                };
                self.next_expect(Token::CloseBracket)?;
                let mutability = self.parse_mut_span();
                let inner = self.parse_type_expression()?;
                let span = TextSpan::merge(open_span, inner.span);
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
                self.ast.diagnostics.push(
                    UnexpectedTokenDiagnostic {
                        file_id: self.ast.file_id,
                        received: next,
                        expected: Token::CloseBracket,
                    }
                    .report(),
                );
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

        let span = TextSpan::merge(grouped.open, grouped.close);
        let mut elements = Vec::from(grouped.inner);

        // (T) with no trailing comma — parenthesized type, unwrap
        if elements.len() == 1 && elements[0].separator.is_none() {
            let single = elements.remove(0);
            return Ok(Spanned {
                inner: single.inner.inner,
                span,
            });
        }

        // () or (T,) or (T, U, ...) — tuple type
        let types: Box<[Spanned<TypeExpression>]> = elements.into_iter().map(|s| s.inner).collect();
        Ok(Spanned {
            inner: TypeExpression::Tuple { elements: types },
            span,
        })
    }

    fn parse_function_type_param(parser: &mut Parser) -> Result<Spanned<FunctionTypeParam>, ()> {
        let ty = parser.parse_type_expression()?;
        let (name, ty) = match ty.inner {
            TypeExpression::Identifier { symbol } => {
                if let Some(_) = parser.lexer.next_if(Token::Colon) {
                    let name_span = ty.span;
                    let name_symbol = symbol;
                    let ty = parser.parse_type_expression()?;
                    (
                        Some(Spanned {
                            inner: name_symbol,
                            span: name_span,
                        }),
                        Box::new(ty),
                    )
                } else {
                    (None, Box::new(ty))
                }
            }
            _ => (None, Box::new(ty)),
        };

        let span = TextSpan::merge(name.clone().map(|n| n.span).unwrap_or(ty.span), ty.span);
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
            let span = TextSpan::merge(func_keyword.span, result.span);
            return Ok(Spanned {
                inner: TypeExpression::Function {
                    params,
                    result: Some(result),
                },
                span,
            });
        } else {
            let span = TextSpan::merge(func_keyword.span, params.close);
            return Ok(Spanned {
                inner: TypeExpression::Function {
                    params,
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
            Token::OpenParen => Some((Parser::parse_call_expression, BindingPower::Call)),
            Token::ColonColon => Some((
                Parser::parse_namespace_access_expression,
                BindingPower::Member,
            )),
            Token::Identifier => match Keyword::try_from(token.span.extract_str(self.source)) {
                Ok(Keyword::As) => Some((Parser::parse_cast_expression, BindingPower::Cast)),
                _ => None,
            },
            Token::Colon => Some((Parser::parse_labelled_expression, BindingPower::Primary)),
            Token::Dot => Some((Parser::parse_object_access_expression, BindingPower::Member)),
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
            Ok(Keyword::SelfKw) | Err(_) => {}
            Ok(_) => {
                parser.ast.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
            }
        }
        let symbol = parser.interner.get_or_intern(text);

        Ok(Spanned {
            inner: Expression::Identifier { symbol },
            span: token.span,
        })
    }

    fn parse_struct_init_expression(
        parser: &mut Parser,
        name: Spanned<SymbolU32>,
    ) -> Result<Spanned<Expression>, ()> {
        let fields = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::Comma,
            item_handler: |parser: &mut Parser| -> Result<Spanned<StructInitField>, ()> {
                let name_token = parser.next_expect(Token::Identifier)?;
                let name_symbol = parser.intern_identifier(name_token.span);

                let (value, span) = if parser.lexer.next_if(Token::Colon).is_some() {
                    let expr = parser.parse_expression(BindingPower::Default)?;
                    let span = TextSpan::merge(name_token.span, expr.span);
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

        let span = TextSpan::merge(name.span, fields.close);
        Ok(Spanned {
            inner: Expression::StructInit { name, fields },
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
        let span = TextSpan::merge(object.span, token.span);
        match token.inner {
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
            Token::Int => {
                let index = token
                    .span
                    .extract_str(parser.source)
                    .parse::<u32>()
                    .unwrap();
                Ok(Spanned {
                    inner: Expression::TupleFieldAccess {
                        object: Box::new(object),
                        field: Spanned {
                            inner: index,
                            span: token.span,
                        },
                    },
                    span,
                })
            }
            _ => {
                parser.ast.diagnostics.push(
                    UnexpectedTokenDiagnostic {
                        file_id: parser.ast.file_id,
                        received: token,
                        expected: Token::Identifier,
                    }
                    .report(),
                );
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
                parser.ast.diagnostics.push(
                    IncompleteBinaryExpressionDiagnostic {
                        file_id: parser.ast.file_id,
                        span: operator_token.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        if bp == BindingPower::Comparison {
            match &left.inner {
                Expression::Binary {
                    operator: left_operator,
                    ..
                } if BindingPower::from(left_operator.inner) == BindingPower::Comparison => {
                    parser.ast.diagnostics.push(
                        ChainedComparisonsDiagnostic {
                            file_id: parser.ast.file_id,
                            first_operator_span: left_operator.span,
                            second_operator_span: operator_token.span,
                        }
                        .report(),
                    );
                }
                _ => {}
            }
            match &right.inner {
                Expression::Binary {
                    operator: right_operator,
                    ..
                } if BindingPower::from(right_operator.inner) == BindingPower::Comparison => {
                    parser.ast.diagnostics.push(
                        ChainedComparisonsDiagnostic {
                            file_id: parser.ast.file_id,
                            first_operator_span: operator_token.span,
                            second_operator_span: right_operator.span,
                        }
                        .report(),
                    );
                }
                _ => {}
            }
        }

        let span = TextSpan::merge(left.span, right.span);
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
                parser.ast.diagnostics.push(
                    InvalidIntegerLiteralDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );

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
                parser.ast.diagnostics.push(
                    InvalidIntegerLiteralDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );

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
                let span = TextSpan::merge(return_span, expr.span);
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
            (_, Some(value)) => TextSpan::merge(break_keyword.span, value.span),
            (Some(label), None) => TextSpan::merge(break_keyword.span, label.span),
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
            Some(label) => TextSpan::merge(continue_keyword.span, label.span),
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

        let span = TextSpan::merge(grouped.open, grouped.close);
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

        let span = TextSpan::merge(value.span, ty.span);
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
                parser.ast.diagnostics.push(
                    IncompleteUnaryExpressionDiagnostic {
                        file_id: parser.ast.file_id,
                        span: operator_token.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        let span = TextSpan::merge(operator_token.span, operand.span);
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
            Expression::Identifier { symbol } => Spanned {
                inner: symbol,
                span: label_expr.span,
            },
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
                    Expression::Identifier { .. } | Expression::Call { .. } => {
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

        let span = TextSpan::merge(label.span, block.span);
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

        let span = TextSpan::merge(statements.open, statements.close);
        Ok(Spanned {
            inner: Expression::Block { statements },
            span,
        })
    }

    fn parse_loop_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let loop_keyword = parser.lexer.next();
        let block = Parser::parse_block_expression(parser)?;

        let span = TextSpan::merge(loop_keyword.span, block.span);
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
        let maybe_keyword = Keyword::try_from(parser.lexer.peek().span.extract_str(parser.source));

        match maybe_keyword {
            Ok(Keyword::Else) => {
                let _ = parser.lexer.next();
                let else_block = Parser::parse_block_expression(parser)?;
                let span = TextSpan::merge(if_keyword.span, else_block.span);
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
                let span = TextSpan::merge(if_keyword.span, then_block.span);
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

    fn parse_namespace_access_expression(
        parser: &mut Parser,
        left: Spanned<Expression>,
        _: BindingPower,
    ) -> Result<Spanned<Expression>, ()> {
        let symbol = match left.inner {
            Expression::Identifier { symbol } => symbol,
            _ => {
                parser.ast.diagnostics.push(
                    InvalidNamespaceDiagnostic {
                        file_id: parser.ast.file_id,
                        span: left.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        // Consume the :: token
        _ = parser.lexer.next();

        // `Ident::{ ... }` — struct initializer
        if parser.lexer.peek().inner == Token::OpenBrace {
            let name = Spanned {
                inner: symbol,
                span: left.span,
            };
            return Parser::parse_struct_init_expression(parser, name);
        }

        // Normal namespace access: `Ident::member`
        let member_token = parser.next_expect(Token::Identifier)?;
        let member_symbol = parser.intern_identifier(member_token.span);

        let span = TextSpan::merge(left.span, member_token.span);
        Ok(Spanned {
            inner: Expression::NamespaceAccess {
                namespace: Box::new(Spanned {
                    inner: TypeExpression::Identifier { symbol },
                    span: left.span,
                }),
                member: Spanned {
                    inner: member_symbol,
                    span: member_token.span,
                },
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

        let span = TextSpan::merge(callee.span, arguments.close);
        Ok(Spanned {
            inner: Expression::Call {
                callee: Box::new(callee),
                arguments: arguments.inner,
            },
            span,
        })
    }

    fn parse_local_definition_statement(parser: &mut Parser) -> Result<Spanned<Statement>, ()> {
        let local_keyword = parser.lexer.next();
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
            Some(_) => {
                let ty = parser.parse_type_expression()?;
                Some(Box::new(ty))
            }
            None => None,
        };

        let value = parser
            .lexer
            .next_if(Token::Eq)
            .ok_or(())
            .and_then(|_| {
                let expr = parser.parse_expression(BindingPower::Default)?;
                Ok(expr)
            })
            .map_err(|_| {
                let token = parser.lexer.peek();
                parser.ast.diagnostics.push(
                    MissingLocalInitializerDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
            })?;

        let span = TextSpan::merge(local_keyword.span, value.span);
        Ok(Spanned {
            inner: Statement::LocalDefinition {
                mut_span,
                name,
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
                parser.ast.diagnostics.push(
                    MissingGlobalInitializerDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
            })?;

        let span = TextSpan::merge(global_keyword.span, value.span);
        Ok(Spanned {
            inner: Item::Global {
                mut_span,
                name,
                ty,
                value: Box::new(value),
                id: parser.get_id(),
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

        let span = TextSpan::merge(const_span, value.span);
        Ok(Spanned {
            inner: Item::Const {
                id: parser.get_id(),
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
                    Some(alias) => TextSpan::merge(name_token.span, alias.span),
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

        let span = TextSpan::merge(export_keyword.span, entries.close);

        Ok(Spanned {
            inner: Item::Export { entries },
            span,
        })
    }

    fn parse_enum_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let enum_span = parser.lexer.next().span;

        let repr = if let Some(open_arrow) = parser.lexer.next_if(Token::LeftArrow) {
            let type_expr = parser.parse_type_expression()?;
            if parser.lexer.next_if(Token::RightArrow).is_none() {
                parser.ast.diagnostics.push(
                    UnclosedDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        open_span: open_arrow.span,
                        close_token: Token::RightArrow,
                        expected_close_span: type_expr.span.end_position(),
                    }
                    .report(),
                );
            }
            Some(Box::new(type_expr))
        } else {
            None
        };

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name_symbol = parser.intern_identifier(name_span);

        let variants = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::Comma,
            item_handler: |parser: &mut Parser| -> Result<Spanned<EnumVariant>, ()> {
                let name_token = parser.next_expect(Token::Identifier)?;
                let name_symbol = parser.intern_identifier(name_token.span);

                let (value, span) = if let Some(_) = parser.lexer.next_if(Token::Eq) {
                    let expr = parser.parse_expression(BindingPower::Default)?;
                    let span = TextSpan::merge(name_token.span, expr.span);
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

        let span = TextSpan::merge(enum_span, variants.close);

        Ok(Spanned {
            inner: Item::Enum {
                id: parser.get_id(),
                repr,
                name: Spanned {
                    inner: name_symbol,
                    span: name_span,
                },
                variants,
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
                let span = TextSpan::merge(const_span, value.span);
                Ok(Spanned {
                    inner: ImplItem::Const {
                        id: parser.get_id(),
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
                let method_span = TextSpan::merge(fn_span, block.span);
                Ok(Spanned {
                    inner: ImplItem::Method {
                        id: parser.get_id(),
                        pub_span,
                        attributes: attrs,
                        signature: FunctionSignature {
                            name: Spanned {
                                inner: name_symbol,
                                span: name_span,
                            },
                            params,
                            result,
                        },
                        block: Box::new(block),
                    },
                    span: method_span,
                })
            }
            _ => {
                let token = parser.lexer.next();
                parser.ast.diagnostics.push(
                    UnexpectedTokenDiagnostic {
                        file_id: parser.ast.file_id,
                        received: token,
                        expected: Token::Identifier,
                    }
                    .report(),
                );
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

        let span = TextSpan::merge(impl_span, items.close);
        match target {
            Some(target) => Ok(Spanned {
                inner: Item::ImplTrait {
                    items,
                    target,
                    trait_name: first_ty,
                },
                span,
            }),
            None => Ok(Spanned {
                inner: Item::Impl {
                    items,
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
                    Some(Keyword::Const) => {
                        let const_span = parser.lexer.next().span;
                        let name_span = parser.next_expect(Token::Identifier)?.span;
                        let name_symbol = parser.intern_identifier(name_span);
                        parser.next_expect(Token::Colon)?;
                        let ty = parser.parse_type_expression()?;
                        let span = TextSpan::merge(const_span, ty.span);
                        Ok(Spanned {
                            inner: TraitItem::Const {
                                id: parser.get_id(),
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
                        let span = TextSpan::merge(
                            signature.span,
                            match &body {
                                Some(body) => body.span,
                                None => signature.span,
                            },
                        );

                        Ok(Spanned {
                            inner: TraitItem::Function {
                                id: parser.get_id(),
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
                        parser.ast.diagnostics.push(
                            UnexpectedTokenDiagnostic {
                                file_id: parser.ast.file_id,
                                received: token,
                                expected: Token::Identifier,
                            }
                            .report(),
                        );
                        Err(())
                    }
                }
            },
            should_warn_missing_separator: Some(TraitItem::is_block_like),
        }
        .parse(parser)?;

        let span = TextSpan::merge(trait_span, items.close);
        Ok(Spanned {
            inner: Item::Trait {
                pub_span: None,
                name,
                items,
            },
            span,
        })
    }

    fn parse_struct_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let struct_span = parser.lexer.next().span;

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name_symbol = parser.intern_identifier(name_span);

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
                let span = TextSpan::merge(pub_span.unwrap_or(name_token.span), ty.span);

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

        let span = TextSpan::merge(struct_span, fields.close);
        Ok(Spanned {
            inner: Item::Struct {
                id: parser.get_id(),
                pub_span: None,
                name: Spanned {
                    inner: name_symbol,
                    span: name_span,
                },
                fields,
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
                if let Item::Module {
                    pub_span: ref mut ps,
                    ..
                } = item.inner
                {
                    *ps = Some(pub_span);
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
            _ => {
                parser.ast.diagnostics.push(
                    InvalidItemDiagnostic {
                        file_id: parser.ast.file_id,
                        span: pub_span,
                    }
                    .report(),
                );
                Err(())
            }
        }
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

        let span = TextSpan::merge(memory_span, kind.span);
        Ok(Spanned {
            inner: Item::Memory {
                name,
                kind: Box::new(kind),
                id: parser.get_id(),
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
        let span = TextSpan::merge(keyword_span, type_expr.span);

        Ok(Spanned {
            inner: ImportDeclaration::Global {
                mut_span,
                name,
                ty: Box::new(type_expr),
                id: self.get_id(),
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
        let span = TextSpan::merge(keyword_span, kind.span);
        Ok(Spanned {
            inner: ImportDeclaration::Memory {
                name,
                kind: Box::new(kind),
                id: self.get_id(),
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
                        id: parser.get_id(),
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
                parser.ast.diagnostics.push(
                    InvalidItemDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
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
                parser.ast.diagnostics.push(
                    InvalidItemDiagnostic {
                        file_id: parser.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
                Err(())
            }
        }
    }

    fn parse_module_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let module_span = parser.lexer.next().span; // consume `module`

        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name_symbol = parser.intern_identifier(name_span);

        let items = SeparatedGroup {
            open_token: Token::OpenBrace,
            close_token: Token::CloseBrace,
            separator_token: Token::SemiColon,
            item_handler: Parser::parse_module_body_item,
            should_warn_missing_separator: None,
        }
        .parse(parser)?;

        let span = TextSpan::merge(module_span, items.close);
        Ok(Spanned {
            inner: Item::Module {
                pub_span: None,
                name: Spanned {
                    inner: name_symbol,
                    span: name_span,
                },
                items,
            },
            span,
        })
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

        let span = TextSpan::merge(import_keyword.span, entries.close);

        Ok(Spanned {
            inner: Item::Import {
                module,
                alias,
                entries,
            },
            span,
        })
    }
}

#[cfg(test)]
mod tests;
