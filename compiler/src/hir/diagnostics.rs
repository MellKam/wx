use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::EnumIndex;
use crate::ast::{self, BinaryOp};
use crate::files::FileId;
use crate::hir;
use crate::span::TextSpan;

#[derive(Debug, Clone)]
pub enum DiagnosticContext {
    UnknownEnumVariant {
        file_id: FileId,
        enum_index: EnumIndex,
        span: TextSpan,
    },
    UnknownType {
        file_id: FileId,
        span: TextSpan,
    },
    BinaryExpressionMistmatch {
        file_id: FileId,
        left: TextSpan,
        left_type: hir::Type,
        operator: BinaryOp,
        right: TextSpan,
        right_type: hir::Type,
    },
    InvalidEnumRepresentation {
        file_id: FileId,
        type_: hir::Type,
        span: TextSpan,
    },
    InvalidEnumValue {
        file_id: FileId,
        variant_index: usize,
    },
    UndeclaredIdentifier {
        file_id: FileId,
        span: TextSpan,
    },
    NonCallableIdentifier {
        file_id: FileId,
        span: TextSpan,
    },
    TypeAnnotationRequired {
        file_id: FileId,
        span: TextSpan,
    },
    UnusedExpressionValue {
        file_id: FileId,
        span: TextSpan,
    },
    TypeMistmatch {
        file_id: FileId,
        expected: hir::Type,
        actual: hir::Type,
        span: TextSpan,
    },
    LiteralOutOfRange {
        file_id: FileId,
        primitive: hir::PrimitiveType,
        value: i64,
        span: TextSpan,
    },
    ComparisonTypeAnnotationRequired {
        file_id: FileId,
        left: TextSpan,
        right: TextSpan,
    },
}

impl ast::BinaryOp {
    pub fn get_error_message(self, left: hir::Type, right: hir::Type) -> String {
        match self {
            BinaryOp::Add => format!("cannot add `{}` to `{}`", left, right),
            BinaryOp::Sub => format!("cannot subtract `{}` from `{}`", left, right),
            BinaryOp::Assign => format!("cannot assign `{}` to `{}`", left, right),
            BinaryOp::Mul => format!("cannot multiply `{}` by `{}`", left, right),
            BinaryOp::Div => format!("cannot divide `{}` by `{}`", left, right),
            BinaryOp::Rem => format!(
                "cannot calculate the remainder of `{}` by `{}`",
                left, right
            ),
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => {
                format!("cannot compare `{}` to `{}`", left, right)
            }
            _ => {
                format!("cannot perform operation on `{}` and `{}`", left, right)
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
                variant_index,
            } => Diagnostic::error().with_message("invalid enum variant value"),
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
                    actual
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
            ComparisonTypeAnnotationRequired { file_id, left, right } => Diagnostic::error().with_message("type annotation required for at least one of binary operands of comparison expression").with_label(Label::primary(file_id, left)).with_label(Label::primary(file_id, right))
        }
    }
}
