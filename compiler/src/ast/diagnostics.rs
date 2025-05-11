use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::lexer::{Token, TokenKind};
use crate::files::FileId;
use crate::span::{ByteIndex, Span};

#[derive(Debug, Clone)]
pub enum DiagnosticContext {
    UnknownToken {
        file_id: FileId,
        span: Span,
    },
    UnexpectedEof {
        file_id: FileId,
        span: Span,
    },
    MissingStatementDelimiter {
        file_id: FileId,
        position: ByteIndex,
    },
    InvalidStatement {
        file_id: FileId,
        span: Span,
    },
    MissingClosingParen {
        file_id: FileId,
        opening_paren: Span,
        expr_span: Span,
    },
    InvalidIntegerLiteral {
        file_id: FileId,
        span: Span,
    },
    MissingFunctionBody {
        file_id: FileId,
        span: Span,
    },
    ReservedIdentifier {
        file_id: FileId,
        span: Span,
    },
    UnexpectedToken {
        file_id: FileId,
        received: Token,
        expected_kind: TokenKind,
    },
    IncompleteBinaryExpression {
        file_id: FileId,
        span: Span,
    },
    MissingUnaryOperand {
        file_id: FileId,
        span: Span,
    },
    InvalidNamespace {
        file_id: FileId,
        span: Span,
    },
}

impl DiagnosticContext {
    pub fn to_diagnostic(self) -> Diagnostic<FileId> {
        use DiagnosticContext::*;
        match self {
            UnknownToken { file_id, span } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_label(Label::primary(file_id, span)),
            UnexpectedEof { file_id, span } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_label(Label::primary(file_id, span)),
            MissingStatementDelimiter { file_id, position } => Diagnostic::error()
                .with_message("missing statement delimiter")
                .with_label(
                    Label::primary(file_id, position.to_usize()..position.to_usize())
                        .with_message("consider adding a semicolon `;`"),
                ),
            InvalidStatement { file_id, span } => Diagnostic::error()
                .with_message("invalid statement")
                .with_label(Label::primary(file_id, span)),
            MissingClosingParen {
                file_id,
                opening_paren,
                ..
            } => Diagnostic::error()
                .with_message("missing closing parenthesis")
                .with_label(
                    Label::primary(file_id, opening_paren)
                        .with_message("consider adding a closing parenthesis `)`"),
                ),
            InvalidIntegerLiteral { file_id, span } => Diagnostic::error()
                .with_message("invalid integer literal")
                .with_label(Label::primary(file_id, span)),
            MissingFunctionBody { file_id, span } => Diagnostic::error()
                .with_message("missing function body, expected opening brace `{`")
                .with_label(Label::primary(file_id, span)),
            ReservedIdentifier { file_id, span } => Diagnostic::error()
                .with_message("reserved identifier")
                .with_label(Label::primary(file_id, span)),
            UnexpectedToken {
                file_id,
                expected_kind,
                received,
            } => Diagnostic::error()
                .with_message(format!(
                    "unexpected token, expected `{}` but found `{}`",
                    expected_kind, received.kind
                ))
                .with_label(Label::primary(file_id, received.span))
                .with_label(
                    Label::secondary(file_id, received.span)
                        .with_message(format!("expected `{}`", expected_kind)),
                ),
            IncompleteBinaryExpression { file_id, span } => Diagnostic::error()
                .with_message("incomplete binary expression")
                .with_label(Label::primary(file_id, span))
                .with_label(
                    Label::secondary(file_id, span)
                        .with_message("consider adding a right-hand side operand"),
                ),
            MissingUnaryOperand { file_id, span } => Diagnostic::error()
                .with_message("missing operand")
                .with_label(Label::primary(file_id, span))
                .with_label(
                    Label::secondary(file_id, span).with_message("consider adding an operand"),
                ),
            InvalidNamespace { file_id, span } => Diagnostic::error()
                .with_message("invalid namespace")
                .with_label(
                    Label::primary(file_id, span)
                        .with_message("namespace must be a valid identifier"),
                ),
        }
    }
}
