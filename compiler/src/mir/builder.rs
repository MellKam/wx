use core::panic;

use crate::{ast, hir, mir};

pub struct MIRBuilder {
    mir: mir::MIR,
}

impl From<hir::Type> for mir::Type {
    fn from(ty: hir::Type) -> Self {
        match ty {
            hir::Type::I32 => mir::Type::I32,
            hir::Type::I64 => mir::Type::I64,
            hir::Type::Unit => mir::Type::Unit,
            hir::Type::Never => mir::Type::Never,
            _ => panic!("HIR type not supported in MIR"),
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
            name: function.name,
            param_count: function.signature.params.len(),
            locals: function
                .locals
                .iter()
                .map(|local| mir::Local {
                    name: local.name,
                    ty: mir::Type::from(local.ty),
                })
                .collect(),
            result: mir::Type::from(function.signature.result),
            body: function
                .body
                .iter()
                .filter_map(|stmt| MIRBuilder::build_expression_from_statement(&function, stmt))
                .collect(),
        }
    }

    fn build_expression_from_statement(
        func: &hir::Function,
        stmt: &hir::Statement,
    ) -> Option<mir::Expression> {
        use hir::Statement::*;
        match stmt {
            Local { index, expr } => {
                let local = func
                    .locals
                    .get(*index as usize)
                    .expect("local index out of bounds");
                let ty = mir::Type::from(local.ty);
                let value = MIRBuilder::build_expression(expr, ty);
                match value.kind {
                    mir::ExprKind::Int { value: num } if num == 0 => return None,
                    _ => {
                        return Some(mir::Expression {
                            kind: mir::ExprKind::Assign {
                                index: *index,
                                value: Box::new(value),
                            },
                            ty,
                        });
                    }
                }
            }
            Expr { expr } => {
                let ty = mir::Type::from(expr.ty);
                let value = MIRBuilder::build_expression(expr, ty);
                Some(value)
            }
            Return { expr } => Some(mir::Expression {
                kind: mir::ExprKind::Return {
                    value: Box::new(MIRBuilder::build_expression(
                        expr,
                        mir::Type::from(func.signature.result),
                    )),
                },
                ty: mir::Type::Never,
            }),
            Assign { index, expr } => {
                let local = func
                    .locals
                    .get(*index as usize)
                    .expect("local index out of bounds");
                let ty = mir::Type::from(local.ty);
                let value = MIRBuilder::build_expression(expr, ty);
                Some(mir::Expression {
                    kind: mir::ExprKind::Assign {
                        index: *index,
                        value: Box::new(value),
                    },
                    ty,
                })
            }
        }
    }

    fn build_expression(expr: &hir::Expression, expected_type: mir::Type) -> mir::Expression {
        match expr.ty {
            hir::Type::ComptimeInt => {
                let value = MIRBuilder::build_comptime_int_expression(&expr.kind);
                return mir::Expression {
                    kind: mir::ExprKind::Int { value },
                    ty: expected_type,
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
            hir::ExprKind::Local(index) => mir::Expression {
                kind: mir::ExprKind::Local { index },
                ty: mir::Type::from(expected_type),
            },
            hir::ExprKind::Int(_) => unreachable!(),
        }
    }

    fn build_runtime_unary_expression(
        operator: ast::UnaryOperator,
        operand: &hir::Expression,
        expected_type: mir::Type,
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
        expected_type: mir::Type,
    ) -> mir::Expression {
        match operator {
            ast::BinaryOperator::Add => {
                let left = MIRBuilder::build_expression(lhs, expected_type);
                let right = MIRBuilder::build_expression(rhs, expected_type);

                mir::Expression {
                    kind: mir::ExprKind::Add {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: mir::Type::from(expected_type),
                }
            }
            ast::BinaryOperator::Subtract => {
                let left = MIRBuilder::build_expression(lhs, expected_type);
                let right = MIRBuilder::build_expression(rhs, expected_type);

                mir::Expression {
                    kind: mir::ExprKind::Sub {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: mir::Type::from(expected_type),
                }
            }
            ast::BinaryOperator::Multiply => {
                let left = MIRBuilder::build_expression(lhs, expected_type);
                let right = MIRBuilder::build_expression(rhs, expected_type);

                mir::Expression {
                    kind: mir::ExprKind::Mul {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: mir::Type::from(expected_type),
                }
            }
            _ => todo!("unimplemented operator"),
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
