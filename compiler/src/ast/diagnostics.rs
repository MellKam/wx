use codespan::{ByteIndex, Span};
use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, Label, Severity};

pub trait Report {
    const SEVERITY: Severity;
    fn report(&self) -> CodespanDiagnostic<()>;
}

pub enum Diagnostic {
    UnknownToken(UnknownTokenDiagnostic),
    UnexpectedEof(UnexpectedEofDiagnostic),
    MissingStatementDelimiter(MissingStatementDelimiterDiagnostic),
    InvalidStatement(InvalidStatementDiagnostic),
    MisssingClosingParen(MissingClosingParenDiagnostic),
    InvalidIntegerLiteral(InvalidIntegerLiteralDiagnostic),
    MissingFunctionBody(MissingFunctionBodyDiagnostic),
}

impl Report for Diagnostic {
    const SEVERITY: Severity = Severity::Error;

    fn report(&self) -> CodespanDiagnostic<()> {
        match self {
            Diagnostic::UnknownToken(diagnostic) => diagnostic.report(),
            Diagnostic::UnexpectedEof(diagnostic) => diagnostic.report(),
            Diagnostic::MissingStatementDelimiter(diagnostic) => diagnostic.report(),
            Diagnostic::InvalidStatement(diagnostic) => diagnostic.report(),
            Diagnostic::MisssingClosingParen(diagnostic) => diagnostic.report(),
            Diagnostic::InvalidIntegerLiteral(diagnostic) => diagnostic.report(),
            Diagnostic::MissingFunctionBody(diagnostic) => diagnostic.report(),
        }
    }
}

pub struct UnknownTokenDiagnostic {
    pub span: Span,
}

impl Report for UnknownTokenDiagnostic {
    const SEVERITY: Severity = Severity::Error;

    fn report(&self) -> CodespanDiagnostic<()> {
        CodespanDiagnostic::new(Self::SEVERITY)
            .with_message("unknown token")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<Diagnostic> for UnknownTokenDiagnostic {
    fn into(self) -> Diagnostic {
        Diagnostic::UnknownToken(self)
    }
}

pub struct UnexpectedEofDiagnostic {
    pub span: Span,
}

impl Report for UnexpectedEofDiagnostic {
    const SEVERITY: Severity = Severity::Error;

    fn report(&self) -> CodespanDiagnostic<()> {
        CodespanDiagnostic::new(Self::SEVERITY)
            .with_message("unexpected end of file")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<Diagnostic> for UnexpectedEofDiagnostic {
    fn into(self) -> Diagnostic {
        Diagnostic::UnexpectedEof(self)
    }
}

pub struct MissingStatementDelimiterDiagnostic {
    pub position: ByteIndex,
}

impl Report for MissingStatementDelimiterDiagnostic {
    const SEVERITY: Severity = Severity::Error;

    fn report(&self) -> CodespanDiagnostic<()> {
        CodespanDiagnostic::new(Self::SEVERITY)
            .with_message("missing statement delimiter")
            .with_label(
                Label::primary((), self.position.to_usize()..self.position.to_usize())
                    .with_message("consider adding a semicolon `;`"),
            )
    }
}

impl Into<Diagnostic> for MissingStatementDelimiterDiagnostic {
    fn into(self) -> Diagnostic {
        Diagnostic::MissingStatementDelimiter(self)
    }
}

pub struct InvalidStatementDiagnostic {
    pub span: Span,
}

impl Report for InvalidStatementDiagnostic {
    const SEVERITY: Severity = Severity::Error;

    fn report(&self) -> CodespanDiagnostic<()> {
        CodespanDiagnostic::new(Self::SEVERITY)
            .with_message("invalid statement")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<Diagnostic> for InvalidStatementDiagnostic {
    fn into(self) -> Diagnostic {
        Diagnostic::InvalidStatement(self)
    }
}

pub struct MissingClosingParenDiagnostic {
    pub opening_paren: Span,
    pub expected_closing_paren_position: ByteIndex,
}

impl Report for MissingClosingParenDiagnostic {
    const SEVERITY: Severity = Severity::Error;

    fn report(&self) -> CodespanDiagnostic<()> {
        CodespanDiagnostic::new(Self::SEVERITY)
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

impl Into<Diagnostic> for MissingClosingParenDiagnostic {
    fn into(self) -> Diagnostic {
        Diagnostic::MisssingClosingParen(self)
    }
}

pub struct InvalidIntegerLiteralDiagnostic {
    pub span: Span,
}

impl Report for InvalidIntegerLiteralDiagnostic {
    const SEVERITY: Severity = Severity::Error;

    fn report(&self) -> CodespanDiagnostic<()> {
        CodespanDiagnostic::new(Self::SEVERITY)
            .with_message("invalid integer literal")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<Diagnostic> for InvalidIntegerLiteralDiagnostic {
    fn into(self) -> Diagnostic {
        Diagnostic::InvalidIntegerLiteral(self)
    }
}

pub struct MissingFunctionBodyDiagnostic {
    pub span: Span,
}

impl Report for MissingFunctionBodyDiagnostic {
    const SEVERITY: Severity = Severity::Error;

    fn report(&self) -> CodespanDiagnostic<()> {
        CodespanDiagnostic::new(Self::SEVERITY)
            .with_message("missing function body, expected opening brace `{`")
            .with_label(Label::primary((), self.span))
    }
}

impl Into<Diagnostic> for MissingFunctionBodyDiagnostic {
    fn into(self) -> Diagnostic {
        Diagnostic::MissingFunctionBody(self)
    }
}
