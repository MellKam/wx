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
            hir::PrimitiveType::F32 => mir::Type::F32,
            hir::PrimitiveType::F64 => mir::Type::F64,
            hir::PrimitiveType::U32 => mir::Type::U32,
            hir::PrimitiveType::U64 => mir::Type::U64,
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
            globals: hir
                .globals
                .iter()
                .map(|global| mir::Global {
                    name: global.name.symbol,
                    ty: builder.to_mir_type(global.ty.clone()),
                    mutability: match global.mutability {
                        Some(_) => mir::Mutability::Mutable,
                        None => mir::Mutability::Const,
                    },
                    value: builder.build_expression(&global.value),
                })
                .collect(),
            exports: hir.exports.clone(),
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
            hir::Type::Bool => mir::Type::Bool,
            hir::Type::Enum(enum_index) => {
                let enum_ = &self.hir.enums[enum_index.0 as usize];
                mir::Type::from(enum_.ty)
            }
            hir::Type::Unit => mir::Type::Unit,
            hir::Type::Never => mir::Type::Never,
            hir::Type::Unknown => panic!("unknown type cannot be converted to MIR"),
        }
    }

    fn build_function(&self, function: &hir::Function) -> mir::Function {
        let block = match &function.block.kind {
            hir::ExprKind::Block {
                expressions,
                result,
                ..
            } => self.build_block_expression(
                ScopeIndex(0),
                expressions,
                match result {
                    Some(result) => Some(result),
                    None => None,
                },
                self.to_mir_type(function.ty.result),
            ),
            _ => unreachable!(),
        };

        mir::Function {
            name: function.name.symbol,
            frame: function
                .stack
                .scopes
                .iter()
                .map(|scope| mir::BlockScope {
                    kind: match scope.kind {
                        hir::BlockKind::Block => mir::BlockKind::Block,
                        hir::BlockKind::Loop => mir::BlockKind::Loop,
                    },
                    parent: match scope.parent {
                        Some(index) => Some(mir::ScopeIndex(index.0)),
                        None => None,
                    },
                    locals: scope
                        .locals
                        .iter()
                        .map(|local| mir::Local {
                            name: local.name.symbol,
                            ty: self.to_mir_type(local.ty),
                            mutability: match local.mutability {
                                Some(_) => mir::Mutability::Mutable,
                                None => mir::Mutability::Const,
                            },
                        })
                        .collect(),
                    result: self.to_mir_type(scope.inferred_type.expect("must be typed")),
                })
                .collect(),
            ty: self.to_mir_function_type(function.ty.clone()),
            block,
        }
    }

    fn build_expression(&self, expr: &hir::Expression) -> mir::Expression {
        let ty = self.to_mir_type(match expr.ty {
            Some(ty) => ty,
            None => panic!("expression type must be defined {expr:?}"),
        });
        match &expr.kind {
            hir::ExprKind::Binary {
                operator,
                left,
                right,
            } => self.build_binary_expression(operator.kind, &left, &right, ty),
            hir::ExprKind::Unary { operator, operand } => {
                self.build_unary_expression(operator.kind, &operand, ty)
            }
            hir::ExprKind::Bool(value) => mir::Expression {
                kind: mir::ExprKind::Bool { value: *value },
                ty,
            },
            hir::ExprKind::Global { global_index } => mir::Expression {
                kind: mir::ExprKind::Global {
                    global_index: global_index.0,
                },
                ty,
            },
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
            hir::ExprKind::Call { callee, arguments } => mir::Expression {
                kind: mir::ExprKind::Call {
                    callee: Box::new(self.build_expression(callee)),
                    arguments: arguments
                        .into_iter()
                        .map(|arg| self.build_expression(&arg))
                        .collect(),
                },
                ty,
            },
            hir::ExprKind::EnumVariant {
                enum_index,
                variant_index,
            } => {
                let enum_ = &self.hir.enums[enum_index.0 as usize];
                let variant = &enum_.variants[variant_index.0 as usize];

                mir::Expression {
                    kind: match variant.value.kind {
                        hir::ExprKind::Int(value) => mir::ExprKind::Int { value },
                        hir::ExprKind::Float(value) => mir::ExprKind::Float { value },
                        _ => unreachable!(),
                    },
                    ty,
                }
            }
            hir::ExprKind::LocalDeclaration {
                local_index,
                scope_index,
                expr,
                ..
            } => match expr.kind {
                hir::ExprKind::Int(value) if value == 0 => {
                    return mir::Expression {
                        kind: mir::ExprKind::Noop,
                        ty: mir::Type::Unit,
                    };
                }
                hir::ExprKind::Bool(value) if value == false => {
                    return mir::Expression {
                        kind: mir::ExprKind::Noop,
                        ty: mir::Type::Unit,
                    };
                }
                _ => {
                    return mir::Expression {
                        kind: mir::ExprKind::LocalSet {
                            local_index: local_index.0,
                            scope_index: mir::ScopeIndex(scope_index.0),
                            value: Box::new(self.build_expression(expr)),
                        },
                        ty,
                    };
                }
            },
            hir::ExprKind::Return { value } => mir::Expression {
                kind: mir::ExprKind::Return {
                    value: match value {
                        Some(value) => Some(Box::new(self.build_expression(value))),
                        None => None,
                    },
                },
                ty: mir::Type::Never,
            },
            hir::ExprKind::Int(value) => mir::Expression {
                kind: mir::ExprKind::Int { value: *value },
                ty,
            },
            hir::ExprKind::Float(value) => mir::Expression {
                kind: mir::ExprKind::Float { value: *value },
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
            hir::ExprKind::Break { scope_index, value } => mir::Expression {
                kind: mir::ExprKind::Break {
                    scope_index: mir::ScopeIndex(scope_index.0),
                    value: match value {
                        Some(value) => Some(Box::new(self.build_expression(value))),
                        None => None,
                    },
                },
                ty,
            },
            hir::ExprKind::Continue { scope_index } => mir::Expression {
                kind: mir::ExprKind::Continue {
                    scope_index: mir::ScopeIndex(scope_index.0),
                },
                ty: mir::Type::Never,
            },
            hir::ExprKind::Unreachable => mir::Expression {
                kind: mir::ExprKind::Unreachable,
                ty: mir::Type::Never,
            },
            hir::ExprKind::Loop { block, scope_index } => {
                let block = match &block.kind {
                    hir::ExprKind::Block {
                        expressions,
                        result,
                        ..
                    } => self.build_block_expression(
                        *scope_index,
                        expressions,
                        match result {
                            Some(result) => Some(result),
                            None => None,
                        },
                        mir::Type::Never,
                    ),
                    _ => unreachable!(),
                };

                mir::Expression {
                    kind: mir::ExprKind::Loop {
                        scope_index: mir::ScopeIndex(scope_index.0),
                        block: Box::new(block),
                    },
                    ty: mir::Type::Never,
                }
            }
            hir::ExprKind::Error => panic!("invalid HIR"),
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
            .chain(result.map(|result| self.build_expression(result)))
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
        operator: ast::UnOpKind,
        operand: &hir::Expression,
        ty: mir::Type,
    ) -> mir::Expression {
        let kind = match operator {
            ast::UnOpKind::InvertSign => mir::ExprKind::Neg {
                value: Box::new(self.build_expression(operand)),
            },
            ast::UnOpKind::Not => mir::ExprKind::Eqz {
                value: Box::new(self.build_expression(operand)),
            },
            ast::UnOpKind::BitNot => mir::ExprKind::BitNot {
                value: Box::new(self.build_expression(operand)),
            },
        };

        mir::Expression { kind, ty }
    }

    fn build_binary_expression(
        &self,
        operator: ast::BinOpKind,
        lhs: &hir::Expression,
        rhs: &hir::Expression,
        ty: mir::Type,
    ) -> mir::Expression {
        use crate::ast::BinOpKind;
        match operator {
            BinOpKind::Add => mir::Expression {
                kind: mir::ExprKind::Add {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Sub => mir::Expression {
                kind: mir::ExprKind::Sub {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Mul => mir::Expression {
                kind: mir::ExprKind::Mul {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Div => mir::Expression {
                kind: mir::ExprKind::Div {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Rem => mir::Expression {
                kind: mir::ExprKind::Rem {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Less => mir::Expression {
                kind: mir::ExprKind::Less {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::LessEq => mir::Expression {
                kind: mir::ExprKind::LessEq {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Greater => mir::Expression {
                kind: mir::ExprKind::Greater {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::GreaterEq => mir::Expression {
                kind: mir::ExprKind::GreaterEq {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Assign => {
                let value = self.build_expression(rhs);
                match lhs.kind {
                    hir::ExprKind::Local {
                        local_index,
                        scope_index,
                    } => mir::Expression {
                        kind: mir::ExprKind::LocalSet {
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
                    hir::ExprKind::Global { global_index } => mir::Expression {
                        kind: mir::ExprKind::GlobalSet {
                            global_index: global_index.0,
                            value: Box::new(value),
                        },
                        ty,
                    },
                    _ => unreachable!(),
                }
            }
            BinOpKind::AddAssign => match lhs.kind {
                hir::ExprKind::Local {
                    local_index,
                    scope_index,
                } => {
                    let left = self.build_expression(lhs);
                    let sum_type = left.ty.clone();
                    let sum = mir::Expression {
                        kind: mir::ExprKind::Add {
                            left: Box::new(left),
                            right: Box::new(self.build_expression(rhs)),
                        },
                        ty: sum_type,
                    };

                    mir::Expression {
                        kind: mir::ExprKind::LocalSet {
                            local_index: local_index.0,
                            scope_index: mir::ScopeIndex(scope_index.0),
                            value: Box::new(sum),
                        },
                        ty,
                    }
                }
                _ => unreachable!("add assignment only allowed on local mutable variables"),
            },
            BinOpKind::SubAssign => match lhs.kind {
                hir::ExprKind::Local {
                    local_index,
                    scope_index,
                } => {
                    let left = self.build_expression(lhs);
                    let value_ty = left.ty.clone();
                    let value = mir::Expression {
                        kind: mir::ExprKind::Sub {
                            left: Box::new(self.build_expression(lhs)),
                            right: Box::new(self.build_expression(rhs)),
                        },
                        ty: value_ty,
                    };

                    mir::Expression {
                        kind: mir::ExprKind::LocalSet {
                            local_index: local_index.0,
                            scope_index: mir::ScopeIndex(scope_index.0),
                            value: Box::new(value),
                        },
                        ty,
                    }
                }
                _ => unreachable!("sub assignment only allowed on local mutable variables"),
            },
            BinOpKind::MulAssign => match lhs.kind {
                hir::ExprKind::Local {
                    local_index,
                    scope_index,
                } => {
                    let left = self.build_expression(lhs);
                    let value_type = left.ty.clone();
                    let value = mir::Expression {
                        kind: mir::ExprKind::Mul {
                            left: Box::new(self.build_expression(lhs)),
                            right: Box::new(self.build_expression(rhs)),
                        },
                        ty: value_type,
                    };

                    mir::Expression {
                        kind: mir::ExprKind::LocalSet {
                            local_index: local_index.0,
                            scope_index: mir::ScopeIndex(scope_index.0),
                            value: Box::new(value),
                        },
                        ty,
                    }
                }
                _ => unreachable!("mul assignment only allowed on local mutable variables"),
            },
            BinOpKind::DivAssign => match lhs.kind {
                hir::ExprKind::Local {
                    local_index,
                    scope_index,
                } => {
                    let left = self.build_expression(lhs);
                    let value_type = left.ty.clone();
                    let value = mir::Expression {
                        kind: mir::ExprKind::Div {
                            left: Box::new(self.build_expression(lhs)),
                            right: Box::new(self.build_expression(rhs)),
                        },
                        ty: value_type,
                    };

                    mir::Expression {
                        kind: mir::ExprKind::LocalSet {
                            local_index: local_index.0,
                            scope_index: mir::ScopeIndex(scope_index.0),
                            value: Box::new(value),
                        },
                        ty,
                    }
                }
                _ => unreachable!("div assignment only allowed on local mutable variables"),
            },
            BinOpKind::RemAssign => match lhs.kind {
                hir::ExprKind::Local {
                    local_index,
                    scope_index,
                } => {
                    let left = self.build_expression(lhs);
                    let value_type = left.ty.clone();
                    let value = mir::Expression {
                        kind: mir::ExprKind::Rem {
                            left: Box::new(self.build_expression(lhs)),
                            right: Box::new(self.build_expression(rhs)),
                        },
                        ty: value_type,
                    };

                    mir::Expression {
                        kind: mir::ExprKind::LocalSet {
                            local_index: local_index.0,
                            scope_index: mir::ScopeIndex(scope_index.0),
                            value: Box::new(value),
                        },
                        ty,
                    }
                }
                _ => unreachable!("rem assignment only allowed on local mutable variables"),
            },
            BinOpKind::BitAnd => mir::Expression {
                kind: mir::ExprKind::BitAnd {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::BitOr => mir::Expression {
                kind: mir::ExprKind::BitOr {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::BitXor => mir::Expression {
                kind: mir::ExprKind::BitXor {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::LeftShift => mir::Expression {
                kind: mir::ExprKind::LeftShift {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::RightShift => mir::Expression {
                kind: mir::ExprKind::RightShift {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Eq => mir::Expression {
                kind: mir::ExprKind::Eq {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::NotEq => mir::Expression {
                kind: mir::ExprKind::NotEq {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::And => mir::Expression {
                kind: mir::ExprKind::And {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
            BinOpKind::Or => mir::Expression {
                kind: mir::ExprKind::Or {
                    left: Box::new(self.build_expression(lhs)),
                    right: Box::new(self.build_expression(rhs)),
                },
                ty,
            },
        }
    }
}
