use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::lexer::{Token, TokenKind};
use crate::files::FileId;
use crate::span::TextSpan;

pub enum DiagnosticCode {
    UnknownToken,
    UnexpectedToken,
    MissingSeparator,
    UnclosedGrouping,
    InvalidLiteral,
    IncompleteExpression,
    ChainedComparison,
    ReservedIdentifier,
    InvalidItem,
    MissingInitializer,
    MissingTypeAnnotation,
}

impl DiagnosticCode {
    const fn code(self) -> &'static str {
        match self {
            DiagnosticCode::UnknownToken => "D0001",
            DiagnosticCode::UnexpectedToken => "E0002",
            DiagnosticCode::MissingSeparator => "E0003",
            DiagnosticCode::UnclosedGrouping => "E0004",
            DiagnosticCode::InvalidLiteral => "E0005",
            DiagnosticCode::IncompleteExpression => "E0006",
            DiagnosticCode::ChainedComparison => "E0007",
            DiagnosticCode::ReservedIdentifier => "E0008",
            DiagnosticCode::InvalidItem => "E0009",
            DiagnosticCode::MissingInitializer => "E0010",
            DiagnosticCode::MissingTypeAnnotation => "E0011",
        }
    }
}

pub struct UnexpectedTokenDiagnostic {
    pub file_id: FileId,
    pub received: Token,
    pub expected_kind: TokenKind,
}

impl UnexpectedTokenDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::UnexpectedToken.code();

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
    pub delimiter: TokenKind,
}

impl MissingSeparatorDiagnostic {
    pub const CODE: &'static str = DiagnosticCode::MissingSeparator.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(Self::CODE)
            .with_message("missing separator")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message(format!("consider adding `{}` here", self.delimiter)),
            )
    }
}

pub struct UnclosedGroupingDiagnotic {
    pub file_id: FileId,
    pub open_span: TextSpan,
    pub close_token: TokenKind,
    pub expected_close_span: TextSpan,
}

impl UnclosedGroupingDiagnotic {
    pub const CODE: &'static str = DiagnosticCode::UnclosedGrouping.code();

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("unclosed grouping")
            .with_label(
                Label::primary(self.file_id, self.expected_close_span)
                    .with_message(format!("consider adding `{}` here", self.close_token)),
            )
            .with_label(Label::secondary(self.file_id, self.open_span).with_message("opened here"))
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
    const CODE: &'static str = DiagnosticCode::ReservedIdentifier.code();

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
