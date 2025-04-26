use core::panic;

use crate::hir::{self, RuntimeType};
use crate::{ast, mir};

pub struct MIRBuilder {
    mir: mir::MIR,
}

impl From<hir::RuntimeType> for mir::Type {
    fn from(ty: hir::RuntimeType) -> Self {
        match ty {
            hir::RuntimeType::Unit => mir::Type::Unit,
            hir::RuntimeType::I32 => mir::Type::I32,
            hir::RuntimeType::I64 => mir::Type::I64,
        }
    }
}

impl MIRBuilder {
    pub fn build(hir: &hir::HIR) -> mir::MIR {
        mir::MIR {
            functions: hir
                .functions
                .iter()
                .map(|func| MIRBuilder::build_function(func))
                .collect(),
        }
    }

    fn build_function(function: &hir::Function) -> mir::Function {
        mir::Function {
            locals: function
                .locals
                .iter()
                .map(|local| mir::Local {
                    index: mir::LocalIndex(local.index.0),
                    ty: mir::Type::from(local.ty),
                })
                .collect(),
            output: vec![mir::Type::from(function.result)],
            body: function
                .body
                .iter()
                .map(|stmt| MIRBuilder::build_expression_from_statement(stmt))
                .collect(),
        }
    }

    fn build_expression_from_statement(stmt: &hir::Statement) -> mir::Expression {
        use hir::Statement::*;
        match stmt {
            Local { index, ty, expr } => {
                let local_index = mir::LocalIndex(index.0);
                let kind = mir::ExprKind::Assign {
                    index: local_index,
                    value: Box::new(MIRBuilder::build_expression(expr, *ty)),
                };
                mir::Expression {
                    kind,
                    ty: mir::Type::from(*ty),
                }
            }
            Expr { ty, expr } => MIRBuilder::build_expression(expr, *ty),
            Return { ty, expr } => mir::Expression {
                kind: mir::ExprKind::Return {
                    value: Box::new(MIRBuilder::build_expression(expr, *ty)),
                },
                ty: mir::Type::from(*ty),
            },
        }
    }

    fn build_expression(expr: &hir::Expression, expected_type: RuntimeType) -> mir::Expression {
        match expr.ty {
            hir::Type::Comptime(hir::ComptimeType::Int) => {
                let value = MIRBuilder::build_comptime_int_expression(&expr.kind);
                return mir::Expression {
                    kind: mir::ExprKind::Int { value },
                    ty: mir::Type::from(expected_type),
                };
            }
            _ => {}
        }

        match expr.kind.clone() {
            hir::ExprKind::Binary { operator, lhs, rhs } => {
                MIRBuilder::build_runtime_binary_expression(operator, &lhs, &rhs, expected_type)
            }
            hir::ExprKind::Unary { operator, operand } => {
                MIRBuilder::build_runtime_unary_expression(operator, &operand, expected_type)
            }
            hir::ExprKind::Local(index) => {
                let local_index = mir::LocalIndex(index.0);
                let kind = mir::ExprKind::Local { index: local_index };
                mir::Expression {
                    kind,
                    ty: mir::Type::from(expected_type),
                }
            }
            hir::ExprKind::Int(_) => unreachable!(),
        }
    }

    fn build_runtime_unary_expression(
        operator: ast::UnaryOperator,
        operand: &hir::Expression,
        expected_type: hir::RuntimeType,
    ) -> mir::Expression {
        let operand = MIRBuilder::build_expression(operand, expected_type);

        let kind = match operator {
            ast::UnaryOperator::Invert => mir::ExprKind::Sub {
                left: Box::new(mir::Expression {
                    kind: mir::ExprKind::Int { value: 0 },
                    ty: mir::Type::from(expected_type),
                }),
                right: Box::new(operand),
            },
        };

        mir::Expression {
            kind,
            ty: mir::Type::from(expected_type),
        }
    }

    fn build_runtime_binary_expression(
        operator: ast::BinaryOperator,
        lhs: &hir::Expression,
        rhs: &hir::Expression,
        expected_type: hir::RuntimeType,
    ) -> mir::Expression {
        let left = MIRBuilder::build_expression(lhs, expected_type);
        let right = MIRBuilder::build_expression(rhs, expected_type);

        let kind = match operator {
            ast::BinaryOperator::Add => mir::ExprKind::Add {
                left: Box::new(left),
                right: Box::new(right),
            },
            ast::BinaryOperator::Subtract => mir::ExprKind::Sub {
                left: Box::new(left),
                right: Box::new(right),
            },
            ast::BinaryOperator::Multiply => mir::ExprKind::Mul {
                left: Box::new(left),
                right: Box::new(right),
            },
            _ => todo!("unimplemented operator"),
        };

        mir::Expression {
            kind,
            ty: mir::Type::from(expected_type),
        }
    }

    fn build_comptime_int_expression(expr: &hir::ExprKind) -> i64 {
        match expr {
            hir::ExprKind::Int(value) => *value,
            hir::ExprKind::Unary { operator, operand } => {
                let value = MIRBuilder::build_comptime_int_expression(&operand.kind);
                match operator {
                    ast::UnaryOperator::Invert => !value,
                }
            }
            hir::ExprKind::Binary { operator, lhs, rhs } => {
                let left = MIRBuilder::build_comptime_int_expression(&lhs.kind);
                let right = MIRBuilder::build_comptime_int_expression(&rhs.kind);
                match operator {
                    ast::BinaryOperator::Add => left + right,
                    ast::BinaryOperator::Subtract => left - right,
                    ast::BinaryOperator::Multiply => left * right,
                    ast::BinaryOperator::Divide => left / right,
                    ast::BinaryOperator::Remainder => left % right,
                    _ => todo!("unimplemented operator"),
                }
            }
            _ => panic!("probably unreachable"),
        }
    }
}
