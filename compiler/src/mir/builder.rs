use core::panic;

use crate::hir::{self, ScopeIndex};
use crate::{ast, mir};

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
            hir::Type::Function(index) => mir::Type::Function(index.0),
            hir::Type::Enum(enum_index) => {
                let enum_ = &self.hir.enums[enum_index.0 as usize];
                mir::Type::from(enum_.ty.clone())
            }
            hir::Type::Unit => mir::Type::Unit,
            hir::Type::Never => mir::Type::Never,
            hir::Type::Unknown => panic!("unknown type"),
        }
    }

    fn build_function(&self, function: &hir::Function) -> mir::Function {
        let block = self.build_block_expression(
            ScopeIndex(0),
            &function.expressions,
            match &function.result {
                Some(result) => Some(result),
                None => None,
            },
            self.to_mir_type(function.ty.result),
        );

        mir::Function {
            export: function.export,
            name: function.name,
            scopes: function
                .scopes
                .scopes
                .iter()
                .map(|scope| mir::LocalScope {
                    parent_scope: match scope.parent_scope {
                        Some(index) => Some(mir::ScopeIndex(index.0)),
                        None => None,
                    },
                    locals: scope
                        .locals
                        .iter()
                        .map(|local| mir::Local {
                            name: local.name,
                            ty: self.to_mir_type(local.ty),
                            mutability: local.mutability,
                        })
                        .collect(),
                })
                .collect(),
            ty: self.to_mir_function_type(function.ty.clone()),
            block,
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
            hir::ExprKind::Local {
                local_index,
                scope_index,
            } => mir::Expression {
                kind: mir::ExprKind::Local {
                    local_index: local_index.0,
                    scope_index: mir::ScopeIndex(scope_index.0),
                },
                ty,
            },
            hir::ExprKind::Function(index) => mir::Expression {
                kind: mir::ExprKind::Function { index: index.0 },
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
                            hir::ExprKind::Function(index) => index.0,
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
                let enum_ = &self.hir.enums[enum_index.0 as usize];
                let variant = &enum_.variants[variant_index.0 as usize];

                mir::Expression {
                    kind: mir::ExprKind::Int {
                        value: variant.value,
                    },
                    ty,
                }
            }
            hir::ExprKind::LocalDeclaration {
                local_index,
                scope_index,
                expr,
            } => match expr.kind {
                hir::ExprKind::Int(value) if value == 0 => {
                    return mir::Expression {
                        kind: mir::ExprKind::Noop,
                        ty: mir::Type::Unit,
                    };
                }
                _ => {
                    return mir::Expression {
                        kind: mir::ExprKind::Assign {
                            local_index: local_index.0,
                            scope_index: mir::ScopeIndex(scope_index.0),
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
            hir::ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => self.build_block_expression(
                *scope_index,
                expressions,
                match result {
                    Some(result) => Some(&result),
                    None => None,
                },
                ty,
            ),
            hir::ExprKind::IfElse {
                condition,
                else_block,
                then_block,
            } => {
                let condition = self.build_expression(condition);
                let then_block = self.build_expression(then_block);
                let else_block = match else_block {
                    Some(else_block) => Some(self.build_expression(else_block)),
                    None => None,
                };

                mir::Expression {
                    kind: mir::ExprKind::IfElse {
                        condition: Box::new(condition),
                        then_block: Box::new(then_block),
                        else_block: else_block.map(|block| Box::new(block)),
                    },
                    ty,
                }
            }
        }
    }

    fn build_block_expression(
        &self,
        scope_index: hir::ScopeIndex,
        expressions: &[hir::Expression],
        result: Option<&hir::Expression>,
        ty: mir::Type,
    ) -> mir::Expression {
        let expressions: Box<_> = expressions
            .iter()
            .map(|expr| self.build_expression(expr))
            .chain(match result {
                Some(result) => {
                    let expr = self.build_expression(result);

                    // 0 is the root scope of the function
                    match scope_index.0 == 0 {
                        true => Some(mir::Expression {
                            kind: mir::ExprKind::Return {
                                value: Box::new(expr),
                            },
                            ty: mir::Type::Never,
                        }),
                        false => Some(mir::Expression {
                            kind: mir::ExprKind::Break {
                                scope_index: mir::ScopeIndex(scope_index.0),
                                value: Some(Box::new(expr)),
                            },
                            ty: mir::Type::Never,
                        }),
                    }
                }
                None => None,
            })
            .collect();

        mir::Expression {
            kind: mir::ExprKind::Block {
                scope_index: mir::ScopeIndex(scope_index.0),
                expressions,
            },
            ty,
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
                    hir::ExprKind::Local {
                        local_index,
                        scope_index,
                    } => mir::Expression {
                        kind: mir::ExprKind::Assign {
                            local_index: local_index.0,
                            scope_index: mir::ScopeIndex(scope_index.0),
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
