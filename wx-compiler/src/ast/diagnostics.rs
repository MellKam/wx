use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::lexer::{Token, TokenKind};
use crate::files::FileId;
use crate::span::TextSpan;

pub struct UnknownTokenDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnknownTokenDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("unknown token")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct UnexpectedEofDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnexpectedEofDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("unexpected end of file")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct MissingStatementDelimiterDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingStatementDelimiterDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("missing statement delimiter")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("consider adding a semicolon `;` at the end of the statement"),
            )
    }
}

pub struct MissingClosingParenDiagnostic {
    pub file_id: FileId,
    pub opening_paren: TextSpan,
    pub expr_span: TextSpan,
}

impl MissingClosingParenDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("missing closing parenthesis")
            .with_label(
                Label::primary(self.file_id, self.opening_paren)
                    .with_message("consider adding a closing parenthesis `)`"),
            )
            .with_label(Label::secondary(self.file_id, self.expr_span))
    }
}

pub struct InvalidIntegerLiteralDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl InvalidIntegerLiteralDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("invalid integer literal")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct IncompleteBinaryExpressionDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl IncompleteBinaryExpressionDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("incomplete binary expression")
            .with_label(Label::primary(self.file_id, self.span))
            .with_label(
                Label::secondary(self.file_id, self.span)
                    .with_message("consider adding a right-hand side operand"),
            )
    }
}

pub struct MissingUnaryOperandDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingUnaryOperandDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("missing operand")
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
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("comparison operators cannot be chained")
            .with_label(Label::primary(self.file_id, self.first_operator_span))
            .with_label(Label::primary(self.file_id, self.second_operator_span))
            .with_note("consider using logical operator like `&&` or `||` to split the comparisons or use parentheses to group them")
    }
}

pub struct UnexpectedTokenDiagnostic {
    pub file_id: FileId,
    pub received: Token,
    pub expected_kind: TokenKind,
}

impl UnexpectedTokenDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!(
                "unexpected token, expected `{}` but found `{}`",
                self.expected_kind, self.received.kind
            ))
            .with_label(Label::primary(self.file_id, self.received.span))
            .with_label(
                Label::secondary(self.file_id, self.received.span)
                    .with_message(format!("expected `{}`", self.expected_kind)),
            )
    }
}

pub struct ReservedIdentifierDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl ReservedIdentifierDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("can't use keyword as identifier")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct InvalidNamespaceDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl InvalidNamespaceDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("invalid namespace")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("namespace must be a valid identifier"),
            )
    }
}
