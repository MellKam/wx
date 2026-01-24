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

    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start as usize..self.end as usize]
    }
}

impl std::fmt::Display for TextSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{start}, {end})", start = self.start, end = self.end)
    }
}

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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum TokenKind {
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Token {
    pub kind: TokenKind,
    pub span: TextSpan,
}

pub struct Lexer<'a> {
    chars: std::str::Chars<'a>,
    offset: usize,
    peeked: Option<Token>,
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

    pub fn next(&mut self) -> Token {
        if let Some(token) = self.peeked.take() {
            return token;
        }

        let mut unknown_span: Option<TextSpan> = None;
        loop {
            let token = self.advance();
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

    fn advance(&mut self) -> Token {
        let start = self.chars.as_str().len();
        let kind = match self.chars.next().unwrap_or(EOF) {
            // Patterns are ordered by frequency of occurrence in typical source code
            // Most Frequent
            ' ' | '\t' | '\n' | '\r' => self.consume_whitespace(),
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
            '/' => self.consume_slash(),

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
        let span = TextSpan::new(self.offset as u32, (self.offset + length) as u32);
        self.offset += length;

        Token { kind, span }
    }

    fn consume_and_check(
        &mut self,
        expect: char,
        token: TokenKind,
        fallback: TokenKind,
    ) -> TokenKind {
        match self.chars.clone().next().unwrap_or(EOF) {
            ch if ch == expect => {
                _ = self.chars.next();
                return token;
            }
            _ => return fallback,
        }
    }

    fn consume_dash(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF) {
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
        match peeker.next().unwrap_or(EOF) {
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
        match peeker.next().unwrap_or(EOF) {
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
        loop {
            match self.chars.next().unwrap_or(EOF) {
                '\\' => {
                    // Escape sequence - skip next character
                    _ = self.chars.next();
                }
                '"' | EOF => return TokenKind::String,
                _ => (),
            }
        }
    }

    fn consume_char(&mut self) -> TokenKind {
        loop {
            match self.chars.next().unwrap_or(EOF) {
                '\\' => {
                    // Escape sequence - skip next character
                    _ = self.chars.next();
                }
                '\'' | EOF => return TokenKind::Char,
                _ => (),
            }
        }
    }

    fn consume_slash(&mut self) -> TokenKind {
        let mut peeker = self.chars.clone();
        match peeker.next().unwrap_or(EOF) {
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
                '\n' | EOF => break,
                _ => {
                    _ = self.chars.next();
                }
            }
        }

        TokenKind::Comment
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(token: TokenKind) -> Result<Self, Self::Error> {
        match token {
            // Arithmetic
            TokenKind::Plus => Ok(BinaryOp::Add),
            TokenKind::Minus => Ok(BinaryOp::Sub),
            TokenKind::Star => Ok(BinaryOp::Mul),
            TokenKind::Slash => Ok(BinaryOp::Div),
            TokenKind::Percent => Ok(BinaryOp::Rem),
            // Relational
            TokenKind::EqEq => Ok(BinaryOp::Eq),
            TokenKind::BangEq => Ok(BinaryOp::NotEq),
            TokenKind::Less => Ok(BinaryOp::Less),
            TokenKind::LessEq => Ok(BinaryOp::LessEq),
            TokenKind::Greater => Ok(BinaryOp::Greater),
            TokenKind::GreaterEq => Ok(BinaryOp::GreaterEq),
            // Logical
            TokenKind::AmperAmper => Ok(BinaryOp::And),
            TokenKind::VbarVbar => Ok(BinaryOp::Or),
            // Assignment
            TokenKind::Eq => Ok(BinaryOp::Assign),
            TokenKind::PlusEq => Ok(BinaryOp::AddAssign),
            TokenKind::MinusEq => Ok(BinaryOp::SubAssign),
            TokenKind::StarEq => Ok(BinaryOp::MulAssign),
            TokenKind::SlashEq => Ok(BinaryOp::DivAssign),
            TokenKind::PercentEq => Ok(BinaryOp::RemAssign),
            // Bitwise
            TokenKind::Amper => Ok(BinaryOp::BitAnd),
            TokenKind::Vbar => Ok(BinaryOp::BitOr),
            TokenKind::Caret => Ok(BinaryOp::BitXor),
            TokenKind::LeftShift => Ok(BinaryOp::LeftShift),
            TokenKind::RightShift => Ok(BinaryOp::RightShift),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    InvertSign,
    Not,
    BitNot,
}

impl TryFrom<TokenKind> for UnaryOp {
    type Error = ();

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::Minus => Ok(UnaryOp::InvertSign),
            TokenKind::Bang => Ok(UnaryOp::Not),
            TokenKind::Caret => Ok(UnaryOp::BitNot),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
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
        };
        write!(f, "{}", symbol)
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            UnaryOp::InvertSign => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "^",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: TextSpan,
}

#[derive(Debug)]
pub struct Separated<T> {
    pub inner: T,
    pub separator: Option<TextSpan>,
}

#[derive(Debug)]
pub struct Grouped<T> {
    pub open: TextSpan,
    pub inner: T,
    pub close: TextSpan,
}

#[derive(Debug)]
pub enum Expression {
    /// `1`
    Int { value: i64 },
    /// `1.0`
    Float { value: f64 },
    /// `({expr})`
    Grouping { value: Grouped<Box<Expression>> },
    /// `x`
    Identifier { symbol: SymbolU32 },
    /// `5 as i32`
    Cast {
        value: Box<Expression>,
        ty: Box<TypeExpression>,
    },
    /// `-{expr}`
    Unary {
        operator: UnaryOp,
        operand: Box<Expression>,
    },
    /// `{expr} + {expr}`
    Binary {
        left: Box<Expression>,
        operator: BinaryOp,
        right: Box<Expression>,
    },
    /// `{expr}()`
    Call {
        callee: Box<Expression>,
        arguments: Grouped<Box<[Separated<Expression>]>>,
    },
    /// `{expr}::{expr}`
    Namespace {
        namespace: Box<TypeExpression>,
        member: Spanned<SymbolU32>,
    },
    /// `return {expr}`
    Return { value: Option<Box<Expression>> },
    /// `{ ... }`
    Block(Grouped<Box<[Separated<Statement>]>>),
    /// `{identifier}: { ... }`
    Label {
        label: Spanned<SymbolU32>,
        block: Box<Expression>,
    },
    /// `break (:{label})? {expr}?`
    Break {
        label: Option<Spanned<SymbolU32>>,
        value: Option<Box<Expression>>,
    },
    /// `if {expr} { ... }`
    IfElse {
        condition: Box<Expression>,
        then_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
    /// `continue (:{label})?`
    Continue { label: Option<Spanned<SymbolU32>> },
    /// `loop { ... }`
    Loop { block: Box<Expression> },
    /// `unreachable`
    Unreachable,
}

#[derive(Debug)]
pub enum TypeExpression {
    /// `i32`
    Identifier { symbol: SymbolU32 },
    /// `func(i32, i32) -> i32`
    Function {
        params: Grouped<Box<[Separated<Spanned<TypeExpression>>]>>,
        result: Separated<Box<Spanned<TypeExpression>>>,
    },
}

#[derive(Debug)]
pub enum Statement {
    /// `{expr}`
    Expression { expr: Box<Expression> },
    /// `local (mut)? {identifier}(: {type})? = {expr}`
    LocalDefinition {
        local_span: TextSpan,
        mut_span: Option<TextSpan>,
        name: Spanned<SymbolU32>,
        annotation: Option<Separated<Box<TypeExpression>>>,
        eq_span: TextSpan,
        value: Box<Expression>,
    },
}
