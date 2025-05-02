use codespan::{ByteIndex, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub trait Report {
    fn report(&self) -> Diagnostic<()>;
}

pub enum DiagnosticKind {
    UnknownToken(UnknownTokenDiagnostic),
    UnexpectedEof(UnexpectedEofDiagnostic),
    MissingStatementDelimiter(MissingStatementDelimiterDiagnostic),
    InvalidStatement(InvalidStatementDiagnostic),
    MisssingClosingParen(MissingClosingParenDiagnostic),
    InvalidIntegerLiteral(InvalidIntegerLiteralDiagnostic),
    MissingFunctionBody(MissingFunctionBodyDiagnostic),
}

impl Report for DiagnosticKind {
    fn report(&self) -> Diagnostic<()> {
        match self {
            DiagnosticKind::UnknownToken(diagnostic) => diagnostic.report(),
            DiagnosticKind::UnexpectedEof(diagnostic) => diagnostic.report(),
            DiagnosticKind::MissingStatementDelimiter(diagnostic) => diagnostic.report(),
            DiagnosticKind::InvalidStatement(diagnostic) => diagnostic.report(),
            DiagnosticKind::MisssingClosingParen(diagnostic) => diagnostic.report(),
            DiagnosticKind::InvalidIntegerLiteral(diagnostic) => diagnostic.report(),
            DiagnosticKind::MissingFunctionBody(diagnostic) => diagnostic.report(),
        }
    }
}

pub struct UnknownTokenDiagnostic {
    pub span: Span,
}

impl Report for UnknownTokenDiagnostic {
    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("unknown token")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<DiagnosticKind> for UnknownTokenDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::UnknownToken(self)
    }
}

pub struct UnexpectedEofDiagnostic {
    pub span: Span,
}

impl Report for UnexpectedEofDiagnostic {
    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("unexpected end of file")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<DiagnosticKind> for UnexpectedEofDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::UnexpectedEof(self)
    }
}

pub struct MissingStatementDelimiterDiagnostic {
    pub position: ByteIndex,
}

impl Report for MissingStatementDelimiterDiagnostic {
    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("missing statement delimiter")
            .with_label(
                Label::primary((), self.position.to_usize()..self.position.to_usize())
                    .with_message("consider adding a semicolon `;`"),
            )
    }
}

impl Into<DiagnosticKind> for MissingStatementDelimiterDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::MissingStatementDelimiter(self)
    }
}

pub struct InvalidStatementDiagnostic {
    pub span: Span,
}

impl Report for InvalidStatementDiagnostic {
    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("invalid statement")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<DiagnosticKind> for InvalidStatementDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::InvalidStatement(self)
    }
}

pub struct MissingClosingParenDiagnostic {
    pub opening_paren: Span,
    pub expected_closing_paren_position: ByteIndex,
}

impl Report for MissingClosingParenDiagnostic {
    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("missing closing parenthesis")
            .with_label(
                Label::primary(
                    (),
                    self.opening_paren.start().to_usize()..self.opening_paren.end().to_usize(),
                )
                .with_message("consider adding a closing parenthesis `)`"),
            )
            .with_label(
                Label::secondary(
                    (),
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

pub struct InvalidIntegerLiteralDiagnostic {
    pub span: Span,
}

impl Report for InvalidIntegerLiteralDiagnostic {
    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("invalid integer literal")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<DiagnosticKind> for InvalidIntegerLiteralDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::InvalidIntegerLiteral(self)
    }
}

pub struct MissingFunctionBodyDiagnostic {
    pub span: Span,
}

impl Report for MissingFunctionBodyDiagnostic {
    fn report(&self) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("missing function body, expected opening brace `{`")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<DiagnosticKind> for MissingFunctionBodyDiagnostic {
    fn into(self) -> DiagnosticKind {
        DiagnosticKind::MissingFunctionBody(self)
    }
}
