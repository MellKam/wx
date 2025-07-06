use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::EnumIndex;
use crate::ast::BinaryOp;
use crate::files::FileId;
use crate::hir;
use crate::hir::global::GlobalContext;
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
    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        let left = global.display_type(self.left_type);
        let right = global.display_type(self.right_type);

        let message = match self.operator {
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
            BinaryOp::MulAssign => {
                format!("cannot multiply-assign `{}` to `{}`", right, left)
            }
            BinaryOp::DivAssign => {
                format!("cannot divide-assign `{}` by `{}`", right, left)
            }
            BinaryOp::RemAssign => {
                format!("cannot remainder-assign `{}` by `{}`", right, left)
            }
            BinaryOp::AddAssign => {
                format!("cannot add-assign `{}` to `{}`", right, left)
            }
            BinaryOp::SubAssign => {
                format!("cannot subtract-assign `{}` from `{}`", right, left)
            }
            _ => {
                format!("cannot perform operation on `{}` and `{}`", left, right)
            }
        };

        Diagnostic::error()
            .with_message(message)
            .with_label(
                Label::secondary(self.file_id, self.left_span).with_message(format!("`{}`", left)),
            )
            .with_label(
                Label::primary(self.file_id, self.right_span).with_message(format!("`{}`", right)),
            )
    }
}

pub struct InvalidEnumTypeDiagnostic {
    pub file_id: FileId,
    pub ty: hir::Type,
    pub span: TextSpan,
}

impl InvalidEnumTypeDiagnostic {
    const CODE: &'static str = "invalid-enum-type";

    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!(
                "cannot represent enum with type `{}`",
                global.display_type(self.ty)
            ))
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("enum can only be represented with `i32`, `i64`, `f32`, or `f64`")
    }
}

pub struct InvalidGlobalTypeDiagnostic {
    pub file_id: FileId,
    pub ty: hir::Type,
    pub span: TextSpan,
}

impl InvalidGlobalTypeDiagnostic {
    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!(
                "cannot define global with type `{}`",
                global.display_type(self.ty)
            ))
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("global can only be represented with `i32`, `i64`, `f32`, or `f64`")
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
    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("type mismatch")
            .with_label(
                Label::primary(self.file_id, self.span).with_message(format!(
                    "expected `{}`, found `{}`",
                    global.display_type(self.expected),
                    global.display_type(self.actual)
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

pub struct FloatLiteralOutOfRangeDiagnostic {
    pub file_id: FileId,
    pub primitive: hir::PrimitiveType,
    pub value: f64,
    pub span: TextSpan,
}

impl FloatLiteralOutOfRangeDiagnostic {
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
    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!(
                "unable to coerce to type `{}`",
                global.display_type(self.target_type)
            ))
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
    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(format!(
                "operator `{}` cannot be applied to type `{}`",
                self.operator,
                global.display_type(self.to_type)
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
