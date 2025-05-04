use codespan::{ByteIndex, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::files::FileId;

#[derive(Debug)]
pub enum DiagnosticKind {
    UnknownToken(UnknownTokenDiagnostic),
    UnexpectedEof(UnexpectedEofDiagnostic),
    MissingStatementDelimiter(MissingStatementDelimiterDiagnostic),
    InvalidStatement(InvalidStatementDiagnostic),
    MisssingClosingParen(MissingClosingParenDiagnostic),
    InvalidIntegerLiteral(InvalidIntegerLiteralDiagnostic),
    MissingFunctionBody(MissingFunctionBodyDiagnostic),
    ReservedIdentifier(ReservedIdentifierDiagnostic),
}

impl DiagnosticKind {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        use DiagnosticKind::*;
        match self {
            UnknownToken(diagnostic) => diagnostic.to_diagnostic(),
            UnexpectedEof(diagnostic) => diagnostic.to_diagnostic(),
            MissingStatementDelimiter(diagnostic) => diagnostic.to_diagnostic(),
            InvalidStatement(diagnostic) => diagnostic.to_diagnostic(),
            MisssingClosingParen(diagnostic) => diagnostic.to_diagnostic(),
            InvalidIntegerLiteral(diagnostic) => diagnostic.to_diagnostic(),
            MissingFunctionBody(diagnostic) => diagnostic.to_diagnostic(),
            ReservedIdentifier(diagnostic) => diagnostic.to_diagnostic(),
        }
    }
}

#[derive(Debug)]
pub struct UnknownTokenDiagnostic {
    pub file_id: FileId,
    pub span: Span,
}

impl UnknownTokenDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("unknown token")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

impl Into<DiagnosticKind> for UnknownTokenDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::UnknownToken(self)
    }
}

#[derive(Debug)]
pub struct UnexpectedEofDiagnostic {
    pub file_id: FileId,
    pub span: Span,
}

impl UnexpectedEofDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("unexpected end of file")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

impl Into<DiagnosticKind> for UnexpectedEofDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::UnexpectedEof(self)
    }
}

#[derive(Debug)]
pub struct MissingStatementDelimiterDiagnostic {
    pub file_id: FileId,
    pub position: ByteIndex,
}

impl MissingStatementDelimiterDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("missing statement delimiter")
            .with_label(
                Label::primary(
                    self.file_id,
                    self.position.to_usize()..self.position.to_usize(),
                )
                .with_message("consider adding a semicolon `;`"),
            )
    }
}

impl Into<DiagnosticKind> for MissingStatementDelimiterDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::MissingStatementDelimiter(self)
    }
}

#[derive(Debug)]
pub struct InvalidStatementDiagnostic {
    pub file_id: FileId,
    pub span: Span,
}

impl InvalidStatementDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("invalid statement")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

impl Into<DiagnosticKind> for InvalidStatementDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::InvalidStatement(self)
    }
}

#[derive(Debug)]
pub struct MissingClosingParenDiagnostic {
    pub file_id: FileId,
    pub opening_paren: Span,
    pub expected_closing_paren_position: ByteIndex,
}

impl MissingClosingParenDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("missing closing parenthesis")
            .with_label(
                Label::primary(
                    self.file_id,
                    self.opening_paren.start().to_usize()..self.opening_paren.end().to_usize(),
                )
                .with_message("consider adding a closing parenthesis `)`"),
            )
            .with_label(
                Label::secondary(
                    self.file_id,
                    self.expected_closing_paren_position.to_usize()
                        ..self.expected_closing_paren_position.to_usize(),
                )
                .with_message("missing closing parenthesis"),
            )
    }
}

impl Into<DiagnosticKind> for MissingClosingParenDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::MisssingClosingParen(self)
    }
}

#[derive(Debug)]
pub struct InvalidIntegerLiteralDiagnostic {
    pub file_id: FileId,
    pub span: Span,
}

impl InvalidIntegerLiteralDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("invalid integer literal")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

impl Into<DiagnosticKind> for InvalidIntegerLiteralDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::InvalidIntegerLiteral(self)
    }
}

#[derive(Debug)]
pub struct MissingFunctionBodyDiagnostic {
    pub file_id: FileId,
    pub span: Span,
}

impl MissingFunctionBodyDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("missing function body, expected opening brace `{`")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

impl Into<DiagnosticKind> for MissingFunctionBodyDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::MissingFunctionBody(self)
    }
}

#[derive(Debug)]
pub struct ReservedIdentifierDiagnostic {
    pub file_id: FileId,
    pub span: Span,
}

impl ReservedIdentifierDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("reserved identifier")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

impl Into<DiagnosticKind> for ReservedIdentifierDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::ReservedIdentifier(self)
    }
}
