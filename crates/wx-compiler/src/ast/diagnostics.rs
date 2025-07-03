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
    const CODE: &'static str = "unexpected-eof";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("unexpected end of file")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct MissingStatementDelimiterDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingStatementDelimiterDiagnostic {
    const CODE: &'static str = "missing-statement-delimiter";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(Self::CODE)
            .with_message("expected semicolon")
            .with_label(
                Label::primary(self.file_id, self.span).with_message("consider adding `;` here"),
            )
    }
}

pub struct MissingClosingParenDiagnostic {
    pub file_id: FileId,
    pub opening_paren: TextSpan,
    pub expr_span: TextSpan,
}

impl MissingClosingParenDiagnostic {
    const CODE: &'static str = "missing-closing-paren";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
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
    const CODE: &'static str = "invalid-integer-literal";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("invalid integer literal")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct IncompleteBinaryExpressionDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl IncompleteBinaryExpressionDiagnostic {
    const CODE: &'static str = "incomplete-binary-expression";

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

pub struct MissingUnaryOperandDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingUnaryOperandDiagnostic {
    const CODE: &'static str = "missing-unary-operand";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
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
    const CODE: &'static str = "chained-comparisons";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
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
    const CODE: &'static str = "unexpected-token";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message(format!(
                "expected `{}` but found `{}`",
                self.expected_kind, self.received.kind
            ))
            .with_label(
                Label::primary(self.file_id, self.received.span)
                    .with_message(format!("expected `{}`", self.expected_kind)),
            )
    }
}

pub struct ReservedIdentifierDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl ReservedIdentifierDiagnostic {
    const CODE: &'static str = "reserved-identifier";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("can't use keyword as identifier")
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
    const CODE: &'static str = "invalid-item";

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
    const CODE: &'static str = "missing-initializer";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("expected initial value for local variable")
            .with_note("example syntax: local x: i32 = 0;")
            .with_label(Label::primary(self.file_id, self.span))
    }
}
