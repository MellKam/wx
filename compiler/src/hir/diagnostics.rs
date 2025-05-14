use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::EnumIndex;
use crate::ast::{self, BinaryOperator};
use crate::files::FileId;
use crate::hir;
use crate::span::Span;

#[derive(Debug, Clone)]
pub enum DiagnosticContext {
    UnknownEnumVariant {
        file_id: FileId,
        enum_index: EnumIndex,
        span: Span,
    },
    UnknownType {
        file_id: FileId,
        span: Span,
    },
    BinaryExpressionMistmatch {
        file_id: FileId,
        left: Span,
        left_type: hir::Type,
        operator: BinaryOperator,
        right: Span,
        right_type: hir::Type,
    },
    InvalidEnumRepresentation {
        file_id: FileId,
        enum_index: EnumIndex,
        type_: hir::Type,
        span: Span,
    },
    InvalidEnumValue {
        file_id: FileId,
        enum_index: EnumIndex,
        span: Span,
    },
    UndeclaredIdentifier {
        file_id: FileId,
        span: Span,
    },
    NonCallableIdentifier {
        file_id: FileId,
        span: Span,
    },
    TypeAnnotationRequired {
        file_id: FileId,
        span: Span,
    },
    UnusedExpressionValue {
        file_id: FileId,
        span: Span,
    },
    TypeMistmatch {
        file_id: FileId,
        expected: hir::Type,
        actual: Option<hir::Type>,
        span: Span,
    },
    LiteralOutOfRange {
        file_id: FileId,
        primitive: hir::PrimitiveType,
        value: i64,
        span: Span,
    },
}

impl ast::BinaryOperator {
    pub fn get_error_message(self, left: hir::Type, right: hir::Type) -> String {
        match self {
            BinaryOperator::Add => format!("cannot add `{}` to `{}`", left, right),
            BinaryOperator::Subtract => format!("cannot subtract `{}` from `{}`", left, right),
            BinaryOperator::Assign => format!("cannot assign `{}` to `{}`", left, right),
            BinaryOperator::Multiply => format!("cannot multiply `{}` by `{}`", left, right),
            BinaryOperator::Divide => format!("cannot divide `{}` by `{}`", left, right),
            BinaryOperator::Remainder => format!(
                "cannot calculate the remainder of `{}` by `{}`",
                left, right
            ),
            BinaryOperator::Eq
            | BinaryOperator::NotEq
            | BinaryOperator::Less
            | BinaryOperator::LessEq
            | BinaryOperator::Greater
            | BinaryOperator::GreaterEq => {
                format!("cannot compare `{}` to `{}`", left, right)
            }
        }
    }
}

impl DiagnosticContext {
    pub fn to_diagnostic(self) -> Diagnostic<FileId> {
        use DiagnosticContext::*;
        match self {
            UnknownEnumVariant {
                file_id,
                enum_index,
                span,
            } => Diagnostic::error()
                .with_message("unknown enum variant")
                .with_label(Label::primary(file_id, span)),
            BinaryExpressionMistmatch {
                file_id,
                left,
                left_type,
                operator,
                right,
                right_type,
            } => {
                let message = operator.get_error_message(left_type, right_type);
                Diagnostic::error()
                    .with_message(message)
                    .with_label(Label::primary(file_id, left))
                    .with_label(Label::primary(file_id, right))
            }
            UnknownType { file_id, span } => Diagnostic::error()
                .with_message("unknown type")
                .with_label(Label::primary(file_id, span)),
            InvalidEnumRepresentation {
                file_id,
                enum_index,
                type_,
                span,
            } => Diagnostic::error()
                .with_message(format!(
                    "can't represent enum with `{}`: expected i32 or i64",
                    type_
                ))
                .with_label(Label::primary(file_id, span)),
            InvalidEnumValue {
                file_id,
                enum_index,
                span,
            } => Diagnostic::error()
                .with_message("invalid enum variant value")
                .with_label(Label::primary(file_id, span).with_message("expected integer literal")),
            UndeclaredIdentifier { file_id, span } => Diagnostic::error()
                .with_message("undeclared identifier")
                .with_label(Label::primary(file_id, span)),
            NonCallableIdentifier { file_id, span } => Diagnostic::error()
                .with_message("non-callable identifier")
                .with_label(Label::primary(file_id, span)),
            TypeAnnotationRequired { file_id, span } => Diagnostic::error()
                .with_message("type annotation required")
                .with_label(Label::primary(file_id, span)),
            UnusedExpressionValue { file_id, span } => Diagnostic::error()
                .with_message("unused expression value")
                .with_label(Label::primary(file_id, span).with_message("value never used"))
                .with_note(
                    "if you don't need the value, consider dropping it with assignment to `_`",
                ),
            TypeMistmatch {
                file_id,
                span,
                actual,
                expected,
            } => Diagnostic::error()
                .with_message("type mismatch")
                .with_label(Label::primary(file_id, span).with_message(format!(
                    "expected `{}`, found `{}`",
                    expected,
                    actual.unwrap_or(hir::Type::Unknown)
                ))),
            LiteralOutOfRange {
                file_id,
                primitive,
                value,
                span,
            } => Diagnostic::error()
                .with_message(format!(
                    "literal `{}` out of range for `{}`",
                    value, primitive
                ))
                .with_label(Label::primary(file_id, span)),
        }
    }
}
