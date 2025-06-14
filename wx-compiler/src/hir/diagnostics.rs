use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::EnumIndex;
use crate::ast::BinaryOp;
use crate::files::FileId;
use crate::hir;
use crate::span::TextSpan;

pub struct UnknownEnumVariantDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnknownEnumVariantDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("unknown enum variant")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct UnknownTypeDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnknownTypeDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("unknown type")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct BinaryExpressionMistmatchDiagnostic {
    pub file_id: FileId,
    pub left_span: TextSpan,
    pub left_type: hir::Type,
    pub operator: BinaryOp,
    pub right_span: TextSpan,
    pub right_type: hir::Type,
}

impl BinaryExpressionMistmatchDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        let message = match self.operator {
            BinaryOp::Add => format!("cannot add `{}` to `{}`", self.left_type, self.right_type),
            BinaryOp::Sub => format!(
                "cannot subtract `{}` from `{}`",
                self.left_type, self.right_type
            ),
            BinaryOp::Assign => format!(
                "cannot assign `{}` to `{}`",
                self.left_type, self.right_type
            ),
            BinaryOp::Mul => format!(
                "cannot multiply `{}` by `{}`",
                self.left_type, self.right_type
            ),
            BinaryOp::Div => format!(
                "cannot divide `{}` by `{}`",
                self.left_type, self.right_type
            ),
            BinaryOp::Rem => format!(
                "cannot calculate the remainder of `{}` by `{}`",
                self.left_type, self.right_type
            ),
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => {
                format!(
                    "cannot compare `{}` to `{}`",
                    self.left_type, self.right_type
                )
            }
            BinaryOp::MulAssign => {
                format!(
                    "cannot multiply-assign `{}` to `{}`",
                    self.right_type, self.left_type
                )
            }
            BinaryOp::DivAssign => {
                format!(
                    "cannot divide-assign `{}` by `{}`",
                    self.right_type, self.left_type
                )
            }
            BinaryOp::RemAssign => {
                format!(
                    "cannot remainder-assign `{}` by `{}`",
                    self.right_type, self.left_type
                )
            }
            BinaryOp::AddAssign => {
                format!(
                    "cannot add-assign `{}` to `{}`",
                    self.right_type, self.left_type
                )
            }
            BinaryOp::SubAssign => {
                format!(
                    "cannot subtract-assign `{}` from `{}`",
                    self.right_type, self.left_type
                )
            }
            _ => {
                format!(
                    "cannot perform operation on `{}` and `{}`",
                    self.left_type, self.right_type
                )
            }
        };

        Diagnostic::error()
            .with_message(message)
            .with_label(
                Label::secondary(self.file_id, self.left_span)
                    .with_message(format!("`{}`", self.left_type)),
            )
            .with_label(
                Label::primary(self.file_id, self.right_span)
                    .with_message(format!("`{}`", self.right_type)),
            )
    }
}

pub struct InvalidEnumRepresentationDiagnostic {
    pub file_id: FileId,
    pub ty: hir::Type,
    pub span: TextSpan,
}

impl InvalidEnumRepresentationDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!(
                "can't represent enum with `{}`: expected i32 or i64",
                self.ty
            ))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct InvalidEnumValueDiagnostic {
    pub file_id: FileId,
    pub enum_index: EnumIndex,
    pub span: TextSpan,
}

impl InvalidEnumValueDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("invalid enum variant value")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct UndeclaredIdentifierDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UndeclaredIdentifierDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("undeclared identifier")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct NonCallableIdentifierDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl NonCallableIdentifierDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("non-callable identifier")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct TypeAnnotationRequiredDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl TypeAnnotationRequiredDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("type annotation required")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct UnusedValueDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnusedValueDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("value must be used")
            .with_label(Label::primary(self.file_id, self.span).with_message("value never used"))
            .with_note("if you don't need the value, consider dropping it with assignment to `_`")
    }
}

pub struct TypeMistmatchDiagnostic {
    pub file_id: FileId,
    pub expected: hir::Type,
    pub actual: hir::Type,
    pub span: TextSpan,
}

impl TypeMistmatchDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("type mismatch")
            .with_label(
                Label::primary(self.file_id, self.span).with_message(format!(
                    "expected `{}`, found `{}`",
                    self.expected, self.actual
                )),
            )
    }
}

pub struct IntegerLiteralOutOfRangeDiagnostic {
    pub file_id: FileId,
    pub primitive: hir::PrimitiveType,
    pub value: i64,
    pub span: TextSpan,
}

impl IntegerLiteralOutOfRangeDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!(
                "literal `{}` out of range for `{}`",
                self.value, self.primitive
            ))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct ComparisonTypeAnnotationRequiredDiagnostic {
    pub file_id: FileId,
    pub left: TextSpan,
    pub right: TextSpan,
}

impl ComparisonTypeAnnotationRequiredDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("type annotation required")
            .with_label(Label::primary(self.file_id, self.left))
            .with_label(Label::primary(self.file_id, self.right))
            .with_note("at least one side of the comparison must have a known type")
    }
}

pub struct UnreachableCodeDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnreachableCodeDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_message("unreachable code")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("this code will never be executed"),
            )
    }
}

pub struct UnableToCoerceDiagnostic {
    pub file_id: FileId,
    pub target_type: hir::Type,
    pub span: TextSpan,
}

impl UnableToCoerceDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!("unable to coerce to type `{}`", self.target_type))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct OperatorCannotBeAppliedDiagnostic {
    pub file_id: FileId,
    pub operator: BinaryOp,
    pub to_type: hir::Type,
    pub span: TextSpan,
}

impl OperatorCannotBeAppliedDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!(
                "operator `{}` cannot be applied to type `{}`",
                self.operator, self.to_type
            ))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct CannotMutateImmutableDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl CannotMutateImmutableDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("cannot mutate immutable variable")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct UndeclaredLabelDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UndeclaredLabelDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("undeclared label")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct BreakOutsideOfLoopDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl BreakOutsideOfLoopDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("`break` outside of loop")
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("`break` can only be used inside loops or labeled blocks")
    }
}
