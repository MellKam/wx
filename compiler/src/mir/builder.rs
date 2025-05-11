use core::panic;

use crate::{ast, hir, mir};

pub struct Builder<'a> {
    hir: &'a hir::HIR,
}

impl From<hir::PrimitiveType> for mir::Type {
    fn from(ty: hir::PrimitiveType) -> Self {
        match ty {
            hir::PrimitiveType::I32 => mir::Type::I32,
            hir::PrimitiveType::I64 => mir::Type::I64,
        }
    }
}

impl<'a> Builder<'a> {
    pub fn build(hir: &hir::HIR) -> mir::MIR {
        let builder = Builder { hir };
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
            hir::Type::Unit => mir::Type::Unit,
            hir::Type::Never => mir::Type::Never,
            hir::Type::Unknown => panic!("unknown type"),
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
                    .expressions
                    .iter()
                    .map(|expr| self.build_expression(expr))
                    .collect(),
                ty: self.to_mir_type(function.ty.result),
            },
        }
    }

    fn build_expression(&self, expr: &hir::Expression) -> mir::Expression {
        let ty = self.to_mir_type(expr.ty.unwrap());
        match &expr.kind {
            hir::ExprKind::Binary { operator, lhs, rhs } => {
                self.build_binary_expression(*operator, &lhs, &rhs, ty)
            }
            hir::ExprKind::Unary { operator, operand } => {
                self.build_unary_expression(*operator, &operand, ty)
            }
            hir::ExprKind::Local(index) => mir::Expression {
                kind: mir::ExprKind::Local { index: *index },
                ty,
            },
            hir::ExprKind::Function(index) => mir::Expression {
                kind: mir::ExprKind::Function { index: *index },
                ty,
            },
            hir::ExprKind::Call { callee, arguments } => {
                let args = arguments
                    .into_iter()
                    .map(|arg| self.build_expression(&arg))
                    .collect();

                mir::Expression {
                    kind: mir::ExprKind::Call {
                        callee: match callee.kind {
                            hir::ExprKind::Function(index) => index,
                            _ => panic!("expected function"),
                        },
                        arguments: args,
                    },
                    ty,
                }
            }
            hir::ExprKind::EnumVariant {
                enum_index,
                variant_index,
            } => {
                let enum_ = &self.hir.enums[*enum_index as usize];
                let variant = &enum_.variants[*variant_index as usize];

                mir::Expression {
                    kind: mir::ExprKind::Int {
                        value: variant.value,
                    },
                    ty,
                }
            }
            hir::ExprKind::LocalDeclaration { index, expr } => match expr.kind {
                hir::ExprKind::Int(value) if value == 0 => {
                    return mir::Expression {
                        kind: mir::ExprKind::Noop,
                        ty: mir::Type::Unit,
                    };
                }
                _ => {
                    return mir::Expression {
                        kind: mir::ExprKind::Assign {
                            index: *index,
                            value: Box::new(self.build_expression(expr)),
                        },
                        ty,
                    };
                }
            },
            hir::ExprKind::Return(expr) => mir::Expression {
                kind: mir::ExprKind::Return {
                    value: Box::new(self.build_expression(expr)),
                },
                ty: mir::Type::Never,
            },
            hir::ExprKind::Int(value) => mir::Expression {
                kind: mir::ExprKind::Int { value: *value },
                ty,
            },
            hir::ExprKind::Placeholder => unreachable!(),
            hir::ExprKind::IfElse { .. } => unimplemented!(),
        }
    }

    fn build_unary_expression(
        &self,
        operator: ast::UnaryOperator,
        operand: &hir::Expression,
        ty: mir::Type,
    ) -> mir::Expression {
        let kind = match operator {
            ast::UnaryOperator::Invert => mir::ExprKind::Sub {
                left: Box::new(mir::Expression {
                    kind: mir::ExprKind::Int { value: 0 },
                    ty: ty.clone(),
                }),
                right: Box::new(self.build_expression(operand)),
            },
        };

        mir::Expression { kind, ty }
    }

    fn build_binary_expression(
        &self,
        operator: ast::BinaryOperator,
        lhs: &hir::Expression,
        rhs: &hir::Expression,
        ty: mir::Type,
    ) -> mir::Expression {
        match operator {
            ast::BinaryOperator::Add => mir::Expression {
                kind: mir::ExprKind::Add {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            ast::BinaryOperator::Subtract => mir::Expression {
                kind: mir::ExprKind::Sub {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            ast::BinaryOperator::Multiply => mir::Expression {
                kind: mir::ExprKind::Mul {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            ast::BinaryOperator::Assign => {
                let value = self.build_expression(rhs);

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
            ast::BinaryOperator::Eq => mir::Expression {
                kind: mir::ExprKind::Equal {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            _ => todo!("unimplemented operator"),
        }
    }
}
