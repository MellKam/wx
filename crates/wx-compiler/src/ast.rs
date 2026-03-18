use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files;
use string_interner::symbol::SymbolU32;

#[derive(Debug, Copy, Clone, PartialEq)]
#[cfg_attr(test, derive(serde::Serialize))]
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

// impl std::fmt::Display for TextSpan {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "[{start}, {end})", start = self.start, end = self.end)
//     }
// }

impl Into<core::ops::Range<usize>> for TextSpan {
    fn into(self) -> core::ops::Range<usize> {
        self.start as usize..self.end as usize
    }
}

#[derive(Debug, Clone)]
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

#[derive(Copy, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(Debug, serde::Serialize))]
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
#[cfg_attr(test, derive(Debug, serde::Serialize))]
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
        MissingTypeAnnotation => "E0011",
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

pub struct MissingReturnTypeDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingReturnTypeDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::MissingTypeAnnotation.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("missing return type annotation")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("expected colon `:` followed by type"),
            )
    }
}

pub struct MissingParamTypeAnnotationDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingParamTypeAnnotationDiagnostic {
    const CODE: &'static str = DiagnosticCode::MissingTypeAnnotation.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("missing type annotation for function parameter")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("expected colon `:` followed by type"),
            )
    }
}

pub struct MissingEnumTypeAnnotationDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingEnumTypeAnnotationDiagnostic {
    const CODE: &'static str = DiagnosticCode::MissingTypeAnnotation.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("missing type annotation for enum")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("expected colon `:` followed by type"),
            )
    }
}

pub struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    offset: usize,
    peeked: Option<Spanned<Token>>,
}

const EOF: char = '\0';

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.chars(),
            offset: 0,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Spanned<Token> {
        match &self.peeked {
            Some(token) => return token.clone(),
            None => {
                let token = self.next();
                self.peeked = Some(token.clone());
                return token;
            }
        }
    }

    pub fn next_if(&mut self, expected: Token) -> Option<Spanned<Token>> {
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

    pub fn next(&mut self) -> Spanned<Token> {
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
            '&' => self.consume_and_check('&', Token::AmperAmper, Token::Amper),
            '|' => self.consume_and_check('|', Token::VbarVbar, Token::Vbar),
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
                return Token::Arrow;
            }
            _ => return Token::Minus,
        }
    }

    fn consume_open_angle(&mut self) -> Token {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF) {
            '=' => {
                _ = self.chars.next();
                return Token::LessEq;
            }
            '<' => {
                _ = self.chars.next();
                return Token::LeftShift;
            }
            _ => return Token::Less,
        }
    }

    fn consume_close_angle(&mut self) -> Token {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF) {
            '=' => {
                _ = self.chars.next();
                return Token::GreaterEq;
            }
            '>' => {
                _ = self.chars.next();
                return Token::RightShift;
            }
            _ => return Token::Greater,
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
            Token::Less => Ok(BinaryOp::Less),
            Token::LessEq => Ok(BinaryOp::LessEq),
            Token::Greater => Ok(BinaryOp::Greater),
            Token::GreaterEq => Ok(BinaryOp::GreaterEq),
            // Logical
            Token::AmperAmper => Ok(BinaryOp::And),
            Token::VbarVbar => Ok(BinaryOp::Or),
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
            Token::LeftShift => Ok(BinaryOp::LeftShift),
            Token::RightShift => Ok(BinaryOp::RightShift),
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

/// Represents a value prefixed by a token, where the token provides context or
/// annotation.
///
/// Used for type annotations (`: i32`) and return types (`-> i32`) where a
/// special token precedes the actual value. The `separator` field stores the
/// prefix token's span.
#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub struct Annotated<T> {
    pub prefix: TextSpan,
    pub inner: T,
}

/// Represents content enclosed by matching delimiter tokens.
///
/// Used for parenthesized expressions `(expr)`, function parameter lists `(a,
/// b, c)`, code blocks `{ stmt; stmt; }`, and array literals `[1, 2, 3]`. The
/// `open` and `close` fields store the spans of the opening and closing
/// delimiters respectively.
#[cfg_attr(test, derive(Debug, serde::Serialize))]
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
#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub struct Separated<T> {
    pub inner: T,
    pub separator: Option<TextSpan>,
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
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
        value: Grouped<Box<Spanned<Expression>>>,
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
    /// `{expr}()`
    Call {
        callee: Box<Spanned<Expression>>,
        arguments: Grouped<Box<[Separated<Spanned<Expression>>]>>,
    },
    /// `{expr}::{expr}`
    Namespace {
        namespace: Box<TypeExpression>,
        member: Spanned<SymbolU32>,
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
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub enum TypeExpression {
    Error,
    /// `i32`
    Identifier {
        symbol: SymbolU32,
    },
    /// `fn(i32, i32) -> i32`
    Function {
        params: Grouped<Box<[Separated<Spanned<TypeExpression>>]>>,
        result: Annotated<Box<Spanned<TypeExpression>>>,
    },
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub enum Statement {
    /// `{expr}`
    Expression(Box<Spanned<Expression>>),
    /// `local (mut)? {identifier}(: {type})? = {expr}`
    LocalDefinition {
        mut_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        type_annotation: Option<Annotated<Box<Spanned<TypeExpression>>>>,
        value: Box<Spanned<Expression>>,
    },
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub struct FunctionParam {
    pub mut_span: Option<TextSpan>,
    pub name: Spanned<SymbolU32>,
    pub type_annotation: Annotated<Box<Spanned<TypeExpression>>>,
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub struct FunctionSignature {
    pub fn_span: TextSpan,
    pub name: Spanned<SymbolU32>,
    pub params: Grouped<Box<[Separated<Spanned<FunctionParam>>]>>,
    pub result: Annotated<Box<Spanned<TypeExpression>>>,
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub enum Item {
    FunctionDefinition {
        signature: FunctionSignature,
        block: Box<Spanned<Expression>>,
    },
    GlobalDefinition {
        mut_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        type_annotation: Option<Annotated<Box<Spanned<TypeExpression>>>>,
        value: Box<Spanned<Expression>>,
    },
    ExportModifier {
        alias: Option<Spanned<SymbolU32>>,
        item: Box<Spanned<Item>>,
    },
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
pub struct AST {
    pub file_id: FileId,
    pub diagnostics: Vec<Diagnostic<FileId>>,
    pub items: Vec<Spanned<Item>>,
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
    // Member,
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
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(text: &str) -> Result<Self, Self::Error> {
        match text {
            "export" => Ok(Keyword::Export),
            "local" => Ok(Keyword::Local),
            "global" => Ok(Keyword::Global),
            "mut" => Ok(Keyword::Mut),
            "enum" => Ok(Keyword::Enum),
            "fn" => Ok(Keyword::Fn),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "loop" => Ok(Keyword::Loop),
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "return" => Ok(Keyword::Return),
            "as" => Ok(Keyword::As),
            "unreachable" => Ok(Keyword::Unreachable),
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
}

struct SeparatedGroup<T> {
    item_handler: fn(parser: &mut Parser) -> Result<Spanned<T>, ()>,
    open_token: Token,
    close_token: Token,
    separator_token: Token,
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

            let item = match self.parse_item_with_recovery(parser) {
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

            parser.ast.diagnostics.push(
                MissingSeparatorDiagnostic {
                    file_id: parser.ast.file_id,
                    span: TextSpan::new(item.span.end, item.span.end),
                    separator: self.separator_token,
                }
                .report(),
            );
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

    fn parse_item_with_recovery(&self, parser: &mut Parser) -> Option<Spanned<T>> {
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
        };

        loop {
            let start_token = parser.lexer.peek();
            if start_token.inner == Token::Eof {
                break;
            }

            let item_handler = match parser.get_item_handler(start_token.clone()) {
                Ok(handler) => handler,
                Err(_) => match parser.recover_from_invalid_item(start_token) {
                    Some(handler) => handler,
                    None => break,
                },
            };

            match item_handler(&mut parser) {
                Ok(item) => {
                    parser.ast.items.push(item);
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
            Some(Keyword::Export) => Ok(Parser::parse_export_modifier_item),
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

    #[inline]
    fn intern_identifier(&mut self, span: TextSpan) -> Result<SymbolU32, ()> {
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

                Err(())
            }
            Err(_) => Ok(self.interner.get_or_intern(text)),
        }
    }

    fn parse_function_param_item(parser: &mut Parser) -> Result<Spanned<FunctionParam>, ()> {
        let token = parser.peek_expect(Token::Identifier)?;
        let mut_span = match Keyword::try_from(token.span.extract_str(parser.source)) {
            Ok(Keyword::Mut) => Some(parser.lexer.next().span),
            _ => None,
        };

        let name_span = parser.next_expect(Token::Identifier)?.span;
        match Keyword::try_from(name_span.extract_str(parser.source)) {
            Ok(_) => {
                parser.ast.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: parser.ast.file_id,
                        span: name_span,
                    }
                    .report(),
                );
            }
            _ => {}
        }

        let colon = parser.lexer.next_if(Token::Colon);
        let type_annotation = match colon {
            Some(colon) => Annotated {
                prefix: colon.span,
                inner: Box::new(parser.parse_type_expression()?),
            },
            None => {
                let name_end = name_span.end_position();
                parser.ast.diagnostics.push(
                    MissingParamTypeAnnotationDiagnostic {
                        file_id: parser.ast.file_id,
                        span: name_end,
                    }
                    .report(),
                );

                Annotated {
                    prefix: name_end,
                    inner: Box::new(Spanned {
                        inner: TypeExpression::Error,
                        span: name_end,
                    }),
                }
            }
        };

        let span = TextSpan::merge(name_span, type_annotation.inner.span);
        Ok(Spanned {
            inner: FunctionParam {
                mut_span,
                name: Spanned {
                    inner: parser.intern_identifier(name_span)?,
                    span: name_span,
                },
                type_annotation,
            },
            span,
        })
    }

    fn parse_function_definition_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let fn_span = parser.lexer.next();
        let name_span = parser.next_expect(Token::Identifier)?.span;
        let name_symbol = parser.intern_identifier(name_span)?;
        let name = Spanned {
            inner: name_symbol,
            span: name_span,
        };

        let params = SeparatedGroup {
            open_token: Token::OpenParen,
            close_token: Token::CloseParen,
            separator_token: Token::Comma,
            item_handler: Parser::parse_function_param_item,
        }
        .parse(parser)?;

        let result = parser
            .lexer
            .next_if(Token::Arrow)
            .ok_or(())
            .and_then(|colon| {
                Ok(Annotated {
                    prefix: colon.span,
                    inner: Box::new(Parser::parse_type_expression(parser)?),
                })
            })
            .unwrap_or_else(|_| {
                let params_end = params.close.end_position();
                parser.ast.diagnostics.push(
                    MissingReturnTypeDiagnostic {
                        file_id: parser.ast.file_id,
                        span: params_end,
                    }
                    .report(),
                );

                Annotated {
                    prefix: params_end,
                    inner: Box::new(Spanned {
                        inner: TypeExpression::Error,
                        span: params_end,
                    }),
                }
            });

        let block = Parser::parse_block_expression(parser)?;
        let span = TextSpan::merge(fn_span.span, block.span);
        Ok(Spanned {
            inner: Item::FunctionDefinition {
                signature: FunctionSignature {
                    fn_span: fn_span.span,
                    name,
                    params,
                    result,
                },
                block: Box::new(block),
            },
            span,
        })
    }

    fn parse_type_expression(&mut self) -> Result<Spanned<TypeExpression>, ()> {
        let token = self.peek_expect(Token::Identifier)?;
        match Keyword::try_from(token.span.extract_str(self.source)) {
            Ok(Keyword::Fn) => Parser::parse_function_type_expression(self),
            Ok(_) => {
                self.ast.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: self.ast.file_id,
                        span: token.span,
                    }
                    .report(),
                );
                Ok(Spanned {
                    inner: TypeExpression::Identifier {
                        symbol: self.intern_identifier(token.span)?,
                    },
                    span: token.span,
                })
            }
            Err(_) => {
                let token = self.lexer.next();
                Ok(Spanned {
                    inner: TypeExpression::Identifier {
                        symbol: self.intern_identifier(token.span)?,
                    },
                    span: token.span,
                })
            }
        }
    }

    fn parse_function_type_expression(&mut self) -> Result<Spanned<TypeExpression>, ()> {
        let func_keyword = self.lexer.next();
        let params = SeparatedGroup {
            open_token: Token::OpenParen,
            close_token: Token::CloseParen,
            separator_token: Token::Comma,
            item_handler: |parser| parser.parse_type_expression(),
        }
        .parse(self)?;

        let arrow_span = self.next_expect(Token::Arrow)?.span;
        let result = Annotated {
            prefix: arrow_span,
            inner: Box::new(Parser::parse_type_expression(self)?),
        };

        let span = TextSpan::merge(func_keyword.span, result.inner.span);
        Ok(Spanned {
            inner: TypeExpression::Function { params, result },
            span,
        })
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
            // TokenKind::String { .. } => Some((parse_string_expression, BindingPower::Primary)),
            // TokenKind::Char { .. } => Some((parse_string_expression, BindingPower::Primary)),
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
            Token::VbarVbar => Some((Parser::parse_binary_expression, BindingPower::LogicalOr)),
            Token::AmperAmper => Some((Parser::parse_binary_expression, BindingPower::LogicalAnd)),
            Token::Vbar => Some((Parser::parse_binary_expression, BindingPower::BitwiseOr)),
            Token::Caret => Some((Parser::parse_binary_expression, BindingPower::BitwiseXor)),
            Token::Amper => Some((Parser::parse_binary_expression, BindingPower::BitwiseAnd)),
            Token::EqEq
            | Token::BangEq
            | Token::LessEq
            | Token::Less
            | Token::Greater
            | Token::GreaterEq => Some((Parser::parse_binary_expression, BindingPower::Comparison)),
            Token::LeftShift | Token::RightShift => {
                Some((Parser::parse_binary_expression, BindingPower::BitwiseShift))
            }
            Token::OpenParen => Some((Parser::parse_call_expression, BindingPower::Call)),
            // Token::ColonColon => Some((
            //     Parser::parse_namespace_member_expression,
            //     BindingPower::Member,
            // )),
            Token::Identifier => match Keyword::try_from(token.span.extract_str(self.source)) {
                Ok(Keyword::As) => Some((Parser::parse_cast_expression, BindingPower::Cast)),
                _ => None,
            },
            Token::Colon => Some((Parser::parse_labelled_expression, BindingPower::Primary)),
            // TokenKind::Dot => Some((parse_member_expression, BindingPower::Member)),
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
        let symbol = parser.intern_identifier(token.span)?;

        Ok(Spanned {
            inner: Expression::Identifier { symbol },
            span: token.span,
        })
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

    fn parse_int_expression(parser: &mut Parser) -> Result<Spanned<Expression>, ()> {
        let token = parser.lexer.next();
        let value = match token.inner {
            Token::Int => token.span.extract_str(parser.source).parse::<i64>().ok(),
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
                let label_symbol = parser.intern_identifier(label_span)?;

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
                let label_symbol = parser.intern_identifier(label_span)?;

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
        let open = parser.lexer.next().span;
        let value = parser.parse_expression(BindingPower::Default)?;

        match parser.lexer.peek().inner {
            Token::CloseParen => {
                let close = parser.lexer.next().span;

                Ok(Spanned {
                    inner: Expression::Grouping {
                        value: Grouped {
                            open,
                            inner: Box::new(value),
                            close,
                        },
                    },
                    span: TextSpan::merge(open, close),
                })
            }
            _ => {
                parser.ast.diagnostics.push(
                    UnclosedDelimiterDiagnostic {
                        file_id: parser.ast.file_id,
                        open_span: open,
                        close_token: Token::CloseParen,
                        expected_close_span: value.span.end_position(),
                    }
                    .report(),
                );

                let span = TextSpan::merge(open, value.span);
                Ok(Spanned {
                    inner: Expression::Grouping {
                        value: Grouped {
                            open,
                            inner: Box::new(value),
                            close: open.end_position(),
                        },
                    },
                    span,
                })
            }
        }
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
        _ = parser.next_expect(Token::Colon)?;

        let block = parser.parse_expression(BindingPower::Default)?;
        match block.inner {
            Expression::Block { .. } | Expression::IfElse { .. } | Expression::Loop { .. } => {}
            _ => unreachable!("expected a block expression after label"),
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
        }
        .parse(parser)?;

        let span = TextSpan::merge(callee.span, arguments.close);
        Ok(Spanned {
            inner: Expression::Call {
                callee: Box::new(callee),
                arguments,
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
        match Keyword::try_from(name_span.extract_str(parser.source)) {
            Ok(_) => {
                parser.ast.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: parser.ast.file_id,
                        span: name_span,
                    }
                    .report(),
                );
            }
            _ => {}
        }
        let name = Spanned {
            inner: parser.intern_identifier(name_span)?,
            span: name_span,
        };

        let type_annotation = parser
            .lexer
            .next_if(Token::Colon)
            .ok_or(())
            .and_then(|colon| {
                let ty = parser.parse_type_expression()?;
                Ok(Annotated {
                    prefix: colon.span,
                    inner: Box::new(ty),
                })
            })
            .ok(); // todo: recover from invalid type

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
                type_annotation,
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
        match Keyword::try_from(name_span.extract_str(parser.source)) {
            Ok(_) => {
                parser.ast.diagnostics.push(
                    ReservedIdentifierDiagnostic {
                        file_id: parser.ast.file_id,
                        span: name_span,
                    }
                    .report(),
                );
            }
            _ => {}
        }
        let name = Spanned {
            inner: parser.intern_identifier(name_span)?,
            span: name_span,
        };

        let type_annotation = match parser.lexer.peek().inner {
            Token::Colon => {
                let colon_span = parser.lexer.next().span;
                let type_expr = parser.parse_type_expression()?;
                Some(Annotated {
                    prefix: colon_span,
                    inner: Box::new(type_expr),
                })
            }
            _ => None,
        };

        let _ = parser.next_expect(Token::Eq)?;
        let value = parser.parse_expression(BindingPower::Default)?;

        let span = TextSpan::merge(global_keyword.span, value.span);
        Ok(Spanned {
            inner: Item::GlobalDefinition {
                mut_span,
                name,
                type_annotation,
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

    fn parse_export_modifier_item(parser: &mut Parser) -> Result<Spanned<Item>, ()> {
        let export_keyword = parser.lexer.next();
        let next_token = parser.lexer.peek();
        let keyword = Keyword::try_from(next_token.span.extract_str(parser.source));
        let item = match keyword {
            Ok(Keyword::Fn) => Parser::parse_function_definition_item(parser)?,
            Ok(Keyword::Global) => Parser::parse_global_definition_item(parser)?,
            _ => {
                parser.ast.diagnostics.push(
                    InvalidItemDiagnostic {
                        file_id: parser.ast.file_id,
                        span: export_keyword.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        let span = TextSpan::merge(export_keyword.span, item.span);
        Ok(Spanned {
            inner: Item::ExportModifier {
                alias: None,
                item: Box::new(item),
            },
            span,
        })
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;

    #[allow(unused)]
    struct TestCase {
        interner: StringInterner,
        files: Files,
        ast: AST,
    }

    impl<'case> TestCase {
        fn new(source: &str) -> Self {
            let mut interner = StringInterner::new();
            let mut files = Files::new();
            let file_id = files
                .add("main.wx".to_string(), source.to_string())
                .unwrap();
            let ast = Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

            TestCase {
                interner,
                files,
                ast,
            }
        }
    }

    #[test]
    fn test_parse_simple_addition() {
        let case = TestCase::new(indoc! {"fn add(a:i32,b:i32)->i32{a+b}"});
        insta::assert_yaml_snapshot!(case.ast);
    }
}
