use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::lexer::{Token, TokenKind};
use crate::files::FileId;
use crate::span::{ByteIndex, TextSpan};

#[derive(Debug, Clone)]
pub enum DiagnosticContext {
    UnknownToken {
        file_id: FileId,
        span: TextSpan,
    },
    UnexpectedEof {
        file_id: FileId,
        span: TextSpan,
    },
    MissingStatementDelimiter {
        file_id: FileId,
        position: ByteIndex,
    },
    InvalidStatement {
        file_id: FileId,
        span: TextSpan,
    },
    MissingClosingParen {
        file_id: FileId,
        opening_paren: TextSpan,
        expr_span: TextSpan,
    },
    InvalidIntegerLiteral {
        file_id: FileId,
        span: TextSpan,
    },
    MissingFunctionBody {
        file_id: FileId,
        span: TextSpan,
    },
    ReservedIdentifier {
        file_id: FileId,
        span: TextSpan,
    },
    UnexpectedToken {
        file_id: FileId,
        received: Token,
        expected_kind: TokenKind,
    },
    IncompleteBinaryExpression {
        file_id: FileId,
        span: TextSpan,
    },
    MissingUnaryOperand {
        file_id: FileId,
        span: TextSpan,
    },
    InvalidNamespace {
        file_id: FileId,
        span: TextSpan,
    },
    ChainedComparisons {
        file_id: FileId,
        first_operator_span: TextSpan,
        second_operator_span: TextSpan,
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
            ChainedComparisons {
                file_id,
                first_operator_span,
                second_operator_span,
            } => Diagnostic::error()
                .with_message("comparison operators cannot be chained")
                .with_label(Label::primary(file_id, first_operator_span))
                .with_label(Label::primary(file_id, second_operator_span)).with_note("consider using logical operator like `&&` or `||` to split the comparisons or use parentheses to group them"),
        }
    }
}
