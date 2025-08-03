use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::EnumIndex;
use crate::ast::{BinOpKind, BinaryOp, UnaryOp};
use crate::files::FileId;
use crate::hir::global::GlobalContext;
use crate::hir::{self, TypeWithSpan};
use crate::span::TextSpan;

pub enum DiagnosticCode {
    Unknown,
}

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
            .with_message("cannot find type `gew` in this scope")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct BinaryExpressionMistmatchDiagnostic {
    pub file_id: FileId,
    pub left: TypeWithSpan,
    pub operator: BinaryOp,
    pub right: TypeWithSpan,
}

impl BinaryExpressionMistmatchDiagnostic {
    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        let left_type = global.display_type(self.left.ty);
        let right_type = global.display_type(self.right.ty);

        let message = match self.operator.kind {
            BinOpKind::Add => format!("cannot add `{}` to `{}`", left_type, right_type),
            BinOpKind::Sub => format!("cannot subtract `{}` from `{}`", left_type, right_type),
            BinOpKind::Assign => format!("cannot assign `{}` to `{}`", left_type, right_type),
            BinOpKind::Mul => format!("cannot multiply `{}` by `{}`", left_type, right_type),
            BinOpKind::Div => format!("cannot divide `{}` by `{}`", left_type, right_type),
            BinOpKind::Rem => format!(
                "cannot calculate the remainder of `{}` by `{}`",
                left_type, right_type
            ),
            BinOpKind::Eq
            | BinOpKind::NotEq
            | BinOpKind::Less
            | BinOpKind::LessEq
            | BinOpKind::Greater
            | BinOpKind::GreaterEq => {
                format!("cannot compare `{}` to `{}`", left_type, right_type)
            }
            BinOpKind::MulAssign => {
                format!("cannot multiply-assign `{}` to `{}`", right_type, left_type)
            }
            BinOpKind::DivAssign => {
                format!("cannot divide-assign `{}` by `{}`", right_type, left_type)
            }
            BinOpKind::RemAssign => {
                format!(
                    "cannot remainder-assign `{}` by `{}`",
                    right_type, left_type
                )
            }
            BinOpKind::AddAssign => {
                format!("cannot add-assign `{}` to `{}`", right_type, left_type)
            }
            BinOpKind::SubAssign => {
                format!(
                    "cannot subtract-assign `{}` from `{}`",
                    right_type, left_type
                )
            }
            _ => {
                format!(
                    "cannot perform operation on `{}` and `{}`",
                    left_type, right_type
                )
            }
        };

        Diagnostic::error()
            .with_message(message)
            .with_label(
                Label::secondary(self.file_id, self.left.span)
                    .with_message(format!("`{}`", left_type)),
            )
            .with_label(
                Label::primary(self.file_id, self.right.span)
                    .with_message(format!("`{}`", right_type)),
            )
    }
}

pub struct InvalidEnumTypeDiagnostic {
    pub file_id: FileId,
    pub ty: hir::Type,
    pub span: TextSpan,
}

impl InvalidEnumTypeDiagnostic {
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

pub struct CannotCallExpressionDiagnostic {
    pub file_id: FileId,
    pub ty: hir::Type,
    pub span: TextSpan,
}

impl CannotCallExpressionDiagnostic {
    pub const CODE: &'static str = "E2009";

    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("call expression requires function")
            .with_label(
                Label::primary(self.file_id, self.span).with_message(format!(
                    "expected function, found `{}`",
                    global.display_type(self.ty)
                )),
            )
    }
}

pub struct TypeAnnotationRequiredDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl TypeAnnotationRequiredDiagnostic {
    pub const CODE: &'static str = "E2008";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("type annotation required")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct UnusedValueDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnusedValueDiagnostic {
    pub const CODE: &'static str = "E2009";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
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
    pub const CODE: &'static str = "E2008";

    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
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
    pub const CODE: &'static str = "E2007";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
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
    pub const CODE: &'static str = "E2006";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(Self::CODE)
            .with_message("unreachable code")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("this code will never be executed"),
            )
    }
}

pub struct UnreachableExpressionDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnreachableExpressionDiagnostic {
    pub const CODE: &'static str = "E2006";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(Self::CODE)
            .with_message("unreachable expression")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("this expression will never be evaluated"),
            )
    }
}

pub struct UnableToCoerceDiagnostic {
    pub file_id: FileId,
    pub target_type: hir::Type,
    pub span: TextSpan,
}

impl UnableToCoerceDiagnostic {
    pub const CODE: &'static str = "E2005";

    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message(format!(
                "unable to coerce to type `{}`",
                global.display_type(self.target_type)
            ))
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct BinaryOperatorCannotBeAppliedDiagnostic {
    pub file_id: FileId,
    pub operator: BinaryOp,
    pub operand: TypeWithSpan,
}

impl BinaryOperatorCannotBeAppliedDiagnostic {
    pub const CODE: &'static str = "E2004";

    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message(format!(
                "operator `{}` cannot be applied to type `{}`",
                self.operator.kind,
                global.display_type(self.operand.ty)
            ))
            .with_label(Label::primary(self.file_id, self.operand.span))
            .with_label(Label::secondary(self.file_id, self.operator.span))
    }
}

pub struct UnaryOperatorCannotBeAppliedDiagnostic {
    pub file_id: FileId,
    pub operator: UnaryOp,
    pub operand: TypeWithSpan,
}

impl UnaryOperatorCannotBeAppliedDiagnostic {
    pub const CODE: &'static str = "E2004";

    pub fn report(self, global: &GlobalContext) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message(format!(
                "operator `{}` cannot be applied to type `{}`",
                self.operator.kind,
                global.display_type(self.operand.ty)
            ))
            .with_label(Label::primary(self.file_id, self.operand.span))
            .with_label(Label::secondary(self.file_id, self.operator.span))
    }
}

pub struct CannotMutateImmutableDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl CannotMutateImmutableDiagnostic {
    pub const CODE: &'static str = "E2003";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("cannot mutate immutable variable")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct UndeclaredLabelDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UndeclaredLabelDiagnostic {
    pub const CODE: &'static str = "E2000";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("undeclared label")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct BreakOutsideOfLoopDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl BreakOutsideOfLoopDiagnostic {
    pub const CODE: &'static str = "E2001";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("`break` outside of loop")
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("`break` is only allowed inside loops or labeled blocks")
    }
}

pub struct InvalidAssignmentTargetDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl InvalidAssignmentTargetDiagnostic {
    pub const CODE: &'static str = "E2002";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_code(Self::CODE)
            .with_message("invalid assignment target")
            .with_label(
                Label::primary(self.file_id, self.span)
                    .with_message("cannot assign to this expression"),
            )
            .with_note("assignment only allowed to a variable or `_`")
    }
}

pub struct UnusedVariableDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnusedVariableDiagnostic {
    pub const CODE: &'static str = "E2009";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(Self::CODE)
            .with_message("unused variable")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct UnnecessaryMutabilityDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl UnnecessaryMutabilityDiagnostic {
    pub const CODE: &'static str = "E2010";

    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::warning()
            .with_code(Self::CODE)
            .with_message("unnecessary mutability")
            .with_label(Label::primary(self.file_id, self.span))
    }
}

pub struct MissingElseClauseDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl MissingElseClauseDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message("`if` may be missing an `else` clause")
            .with_label(Label::primary(self.file_id, self.span))
            .with_note("`if` expressions without `else` evaluate to `()`")
            .with_note("consider adding an `else` block that evaluates to the expected type")
    }
}

pub struct ArgumentCountMismatchDiagnostic {
    pub file_id: FileId,
    pub expected_count: u32,
    pub actual_count: u32,
    pub span: TextSpan,
}

impl ArgumentCountMismatchDiagnostic {
    pub fn report(self) -> Diagnostic<FileId> {
        let diagnostic: Diagnostic<FileId> =
            Diagnostic::error().with_message(match self.expected_count {
                1 => format!("expected 1 argument, found {}", self.actual_count),
                _ => format!(
                    "expected {} arguments, found {}",
                    self.expected_count, self.actual_count
                ),
            });

        if self.actual_count > self.expected_count {
            diagnostic.with_label(Label::primary(self.file_id, self.span).with_message(
                match self.actual_count - self.expected_count {
                    1 => "remove the extra argument",
                    _ => "remove the extra arguments",
                },
            ))
        } else {
            diagnostic.with_label(Label::primary(self.file_id, self.span).with_message(
                match self.expected_count - self.actual_count {
                    1 => "add the missing argument",
                    _ => "add the missing arguments",
                },
            ))
        }
    }
}
