use core::panic;

use crate::{ast, hir, mir};

pub struct MIRBuilder<'a> {
    hir: &'a hir::HIR,
}

impl From<hir::PrimitiveType> for mir::Type {
    fn from(ty: hir::PrimitiveType) -> Self {
        match ty {
            hir::PrimitiveType::I32 => mir::Type::I32,
            hir::PrimitiveType::I64 => mir::Type::I64,
            hir::PrimitiveType::Unit => mir::Type::Unit,
            hir::PrimitiveType::Never => mir::Type::Never,
        }
    }
}

impl<'a> MIRBuilder<'a> {
    pub fn build(hir: &hir::HIR) -> mir::MIR {
        let builder = MIRBuilder { hir };
        mir::MIR {
            functions: hir
                .functions
                .iter()
                .map(|func| builder.build_function(func))
                .collect(),
        }
    }

    fn to_mir_function_type(&self, ty: hir::FunctionType) -> mir::FunctionType {
        mir::FunctionType {
            param_count: ty.params.len(),
            params_results: ty
                .params
                .into_iter()
                .map(|ty| self.to_mir_type(ty))
                .chain(std::iter::once(self.to_mir_type(ty.result)))
                .collect(),
        }
    }

    fn to_mir_type(&self, ty: hir::Type) -> mir::Type {
        match ty {
            hir::Type::Primitive(ty) => mir::Type::from(ty),
            hir::Type::Function(index) => mir::Type::Function(index),
            hir::Type::Enum(enum_index) => {
                let enum_ = &self.hir.enums[enum_index as usize];
                mir::Type::from(enum_.ty.clone())
            }
            hir::Type::Comptime(_) => panic!("comptime types are not supported in MIR"),
        }
    }

    fn build_function(&self, function: &hir::Function) -> mir::Function {
        mir::Function {
            export: function.export,
            name: function.name,
            ty: self.to_mir_function_type(function.ty.clone()),
            block: mir::Block {
                locals: function
                    .block
                    .locals
                    .iter()
                    .map(|local| mir::Local {
                        name: local.name,
                        ty: self.to_mir_type(local.ty),
                    })
                    .collect(),
                expressions: function
                    .block
                    .statements
                    .iter()
                    .filter_map(|stmt| self.build_expression_from_statement(function, stmt))
                    .chain(function.block.result.as_ref().map(|expr| {
                        self.build_expression(expr, &self.to_mir_type(function.ty.result))
                    }))
                    .collect(),
                ty: self.to_mir_type(function.ty.result),
            },
        }
    }

    fn build_expression_from_statement(
        &self,
        func: &hir::Function,
        stmt: &hir::Statement,
    ) -> Option<mir::Expression> {
        use hir::Statement::*;
        match stmt {
            Decl { index, expr } => {
                let local = func.block.locals.get(*index as usize)?;
                let ty = self.to_mir_type(local.ty);
                let value = self.build_expression(expr, &ty);
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
                let ty = self.to_mir_type(expr.ty);
                let value = self.build_expression(expr, &ty);
                Some(value)
            }
            Return { expr } => Some(mir::Expression {
                kind: mir::ExprKind::Return {
                    value: Box::new(self.build_expression(expr, &self.to_mir_type(func.ty.result))),
                },
                ty: mir::Type::Never,
            }),
        }
    }

    fn build_expression(
        &self,
        expr: &hir::Expression,
        expected_type: &mir::Type,
    ) -> mir::Expression {
        match expr.ty {
            hir::Type::Comptime(hir::ComptimeType::Int) => {
                let value = MIRBuilder::build_comptime_int_expression(&expr.kind);
                return mir::Expression {
                    kind: mir::ExprKind::Int { value },
                    ty: expected_type.clone(),
                };
            }
            _ => {}
        }

        match expr.kind.clone() {
            hir::ExprKind::Binary { operator, lhs, rhs } => {
                self.build_runtime_binary_expression(operator, &lhs, &rhs, expected_type)
            }
            hir::ExprKind::Unary { operator, operand } => {
                self.build_runtime_unary_expression(operator, &operand, expected_type)
            }
            hir::ExprKind::Local(index) => mir::Expression {
                kind: mir::ExprKind::Local { index },
                ty: mir::Type::from(expected_type.clone()),
            },
            hir::ExprKind::Function(index) => mir::Expression {
                kind: mir::ExprKind::Function { index },
                ty: mir::Type::from(expected_type.clone()),
            },
            hir::ExprKind::Call { callee, arguments } => {
                let args = arguments
                    .into_iter()
                    .map(|arg| self.build_expression(&arg, expected_type))
                    .collect();

                mir::Expression {
                    kind: mir::ExprKind::Call {
                        callee: match callee.kind {
                            hir::ExprKind::Function(index) => index,
                            _ => panic!("expected function"),
                        },
                        arguments: args,
                    },
                    ty: expected_type.clone(),
                }
            }
            hir::ExprKind::EnumVariant {
                enum_index,
                variant_index,
            } => {
                let enum_ = &self.hir.enums[enum_index as usize];
                let variant = &enum_.variants[variant_index as usize];
                let ty = mir::Type::from(enum_.ty.clone());

                mir::Expression {
                    kind: mir::ExprKind::Int {
                        value: variant.value,
                    },
                    ty,
                }
            }
            hir::ExprKind::Int(_) => unreachable!(),
            hir::ExprKind::Placeholder => unreachable!(),
        }
    }

    fn build_runtime_unary_expression(
        &self,
        operator: ast::UnaryOperator,
        operand: &hir::Expression,
        expected_type: &mir::Type,
    ) -> mir::Expression {
        let operand = self.build_expression(operand, expected_type);

        let kind = match operator {
            ast::UnaryOperator::Invert => mir::ExprKind::Sub {
                left: Box::new(mir::Expression {
                    kind: mir::ExprKind::Int { value: 0 },
                    ty: mir::Type::from(expected_type.clone()),
                }),
                right: Box::new(operand),
            },
        };

        mir::Expression {
            kind,
            ty: mir::Type::from(expected_type.clone()),
        }
    }

    fn build_runtime_binary_expression(
        &self,
        operator: ast::BinaryOperator,
        lhs: &hir::Expression,
        rhs: &hir::Expression,
        expected_type: &mir::Type,
    ) -> mir::Expression {
        match operator {
            ast::BinaryOperator::Add => {
                let left = self.build_expression(lhs, expected_type);
                let right = self.build_expression(rhs, expected_type);

                mir::Expression {
                    kind: mir::ExprKind::Add {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: mir::Type::from(expected_type.clone()),
                }
            }
            ast::BinaryOperator::Subtract => {
                let left = self.build_expression(lhs, expected_type);
                let right = self.build_expression(rhs, expected_type);

                mir::Expression {
                    kind: mir::ExprKind::Sub {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: mir::Type::from(expected_type.clone()),
                }
            }
            ast::BinaryOperator::Multiply => {
                let left = self.build_expression(lhs, expected_type);
                let right = self.build_expression(rhs, expected_type);

                mir::Expression {
                    kind: mir::ExprKind::Mul {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: mir::Type::from(expected_type.clone()),
                }
            }
            ast::BinaryOperator::Assign => {
                let ty = self.to_mir_type(lhs.ty);
                let value = self.build_expression(rhs, &ty);

                match lhs.kind {
                    hir::ExprKind::Local(index) => mir::Expression {
                        kind: mir::ExprKind::Assign {
                            index,
                            value: Box::new(value),
                        },
                        ty,
                    },
                    hir::ExprKind::Placeholder => mir::Expression {
                        kind: mir::ExprKind::Drop {
                            value: Box::new(value),
                        },
                        ty,
                    },
                    _ => panic!("assignment only allowed on local mutable variables"),
                }
            }
            ast::BinaryOperator::Eq => {
                let left = self.build_expression(lhs, expected_type);
                let right = self.build_expression(rhs, expected_type);

                mir::Expression {
                    kind: mir::ExprKind::Equal {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: mir::Type::from(expected_type.clone()),
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
                    ast::BinaryOperator::Eq => {
                        if left == right {
                            1
                        } else {
                            0
                        }
                    }
                    _ => todo!("unimplemented operator"),
                }
            }
            _ => panic!("probably unreachable"),
        }
    }
}
