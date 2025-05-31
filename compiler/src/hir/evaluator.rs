use std::ops::{Shl, Shr};

use crate::{ast, hir};

pub struct Evaluator {}

impl Evaluator {
    pub fn evaluate_untyped_binary_expr(
        operator: ast::BinaryOp,
        left: &hir::Expression,
        right: &hir::Expression,
    ) -> Result<hir::Expression, ()> {
        use ast::BinaryOp;
        match operator {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                Evaluator::evaluate_untyped_arithmetic_expr(operator, left, right)
            }
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => {
                Evaluator::evaluate_untyped_comparison_expr(operator, left, right)
            }
            BinaryOp::And | BinaryOp::Or => {
                Evaluator::evaluate_untyped_logical_expr(operator, left, right)
            }
            BinaryOp::BitOr
            | BinaryOp::BitAnd
            | BinaryOp::BitXor
            | BinaryOp::LeftShift
            | BinaryOp::RightShift => {
                Evaluator::evaluate_untyped_bitwise_expr(operator, left, right)
            }
            _ => Err(()),
        }
    }

    fn evaluate_untyped_arithmetic_expr(
        operator: ast::BinaryOp,
        left: &hir::Expression,
        right: &hir::Expression,
    ) -> Result<hir::Expression, ()> {
        use ast::BinaryOp;
        use hir::{ExprKind, Expression};

        match (&left.kind, &right.kind) {
            (&ExprKind::Int(left), &ExprKind::Int(right)) => match operator {
                BinaryOp::Add => {
                    let value = left.checked_add(right).ok_or(())?;
                    // TODO: Handle overflow
                    Ok(Expression {
                        kind: ExprKind::Int(value),
                        ty: None,
                    })
                }
                BinaryOp::Sub => {
                    let value = left.checked_sub(right).ok_or(())?;
                    Ok(Expression {
                        kind: ExprKind::Int(value),
                        ty: None,
                    })
                }
                BinaryOp::Mul => {
                    let value = left.checked_mul(right).ok_or(())?;
                    Ok(Expression {
                        kind: ExprKind::Int(value),
                        ty: None,
                    })
                }
                BinaryOp::Div => {
                    let value = left.checked_div(right).ok_or(())?;
                    Ok(Expression {
                        kind: ExprKind::Int(value),
                        ty: None,
                    })
                }
                BinaryOp::Rem => {
                    let value = left.checked_rem(right).ok_or(())?;
                    Ok(Expression {
                        kind: ExprKind::Int(value),
                        ty: None,
                    })
                }
                _ => unreachable!(),
            },
            _ => Err(()),
        }
    }

    fn evaluate_untyped_logical_expr(
        operator: ast::BinaryOp,
        left: &hir::Expression,
        right: &hir::Expression,
    ) -> Result<hir::Expression, ()> {
        use ast::BinaryOp;
        use hir::{ExprKind, Expression};

        match (&left.kind, &right.kind) {
            (&ExprKind::Bool(left), &ExprKind::Bool(right)) => match operator {
                BinaryOp::And => Ok(Expression {
                    kind: ExprKind::Bool(left && right),
                    ty: Some(hir::Type::Bool),
                }),
                BinaryOp::Or => Ok(Expression {
                    kind: ExprKind::Bool(left || right),
                    ty: Some(hir::Type::Bool),
                }),
                _ => unreachable!(),
            },
            _ => Err(()),
        }
    }

    fn evaluate_untyped_comparison_expr(
        operator: ast::BinaryOp,
        left: &hir::Expression,
        right: &hir::Expression,
    ) -> Result<hir::Expression, ()> {
        use ast::BinaryOp;
        use hir::{ExprKind, Expression};

        match (&left.kind, &right.kind) {
            (&ExprKind::Int(left), &ExprKind::Int(right)) => match operator {
                BinaryOp::Eq => Ok(Expression {
                    kind: ExprKind::Bool(left == right),
                    ty: Some(hir::Type::Bool),
                }),
                BinaryOp::NotEq => Ok(Expression {
                    kind: ExprKind::Bool(left != right),
                    ty: Some(hir::Type::Bool),
                }),
                BinaryOp::Less => Ok(Expression {
                    kind: ExprKind::Bool(left < right),
                    ty: Some(hir::Type::Bool),
                }),
                BinaryOp::LessEq => Ok(Expression {
                    kind: ExprKind::Bool(left <= right),
                    ty: Some(hir::Type::Bool),
                }),
                BinaryOp::Greater => Ok(Expression {
                    kind: ExprKind::Bool(left > right),
                    ty: Some(hir::Type::Bool),
                }),
                BinaryOp::GreaterEq => Ok(Expression {
                    kind: ExprKind::Bool(left >= right),
                    ty: Some(hir::Type::Bool),
                }),
                _ => unreachable!(),
            },
            (&ExprKind::Bool(left), &ExprKind::Bool(right)) => match operator {
                BinaryOp::Eq => Ok(Expression {
                    kind: ExprKind::Bool(left == right),
                    ty: Some(hir::Type::Bool),
                }),
                BinaryOp::NotEq => Ok(Expression {
                    kind: ExprKind::Bool(left != right),
                    ty: Some(hir::Type::Bool),
                }),
                _ => unreachable!(),
            },
            _ => Err(()),
        }
    }

    pub fn evaluate_untyped_bitwise_expr(
        operator: ast::BinaryOp,
        left: &hir::Expression,
        right: &hir::Expression,
    ) -> Result<hir::Expression, ()> {
        use ast::BinaryOp;
        use hir::{ExprKind, Expression};

        match (&left.kind, &right.kind) {
            (&ExprKind::Int(left), &ExprKind::Int(right)) => match operator {
                BinaryOp::BitAnd => Ok(Expression {
                    kind: ExprKind::Int(left & right),
                    ty: None,
                }),
                BinaryOp::BitOr => Ok(Expression {
                    kind: ExprKind::Int(left | right),
                    ty: None,
                }),
                BinaryOp::BitXor => Ok(Expression {
                    kind: ExprKind::Int(left ^ right),
                    ty: None,
                }),
                BinaryOp::LeftShift => Ok(Expression {
                    kind: ExprKind::Int(left.shl(right as u32)),
                    ty: None,
                }),
                BinaryOp::RightShift => Ok(Expression {
                    kind: ExprKind::Int(left.shr(right as u32)),
                    ty: None,
                }),
                _ => unreachable!(),
            },
            _ => Err(()),
        }
    }
}
