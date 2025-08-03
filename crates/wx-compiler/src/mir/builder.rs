use core::panic;
use std::cell::RefCell;
use std::rc::Rc;

use crate::{ast, hir, mir};

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

struct ScopeWithLocals {
    scope: Rc<RefCell<mir::BlockScope>>,
    locals: Vec<Rc<RefCell<mir::Local>>>,
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
            globals: Vec::new(),
            // globals: hir
            //     .globals
            //     .iter()
            //     .map(|global| mir::Global {
            //         name: global.name.symbol,
            //         ty: builder.to_mir_type(global.ty.clone()),
            //         mutability: match global.mutability {
            //             Some(_) => mir::Mutability::Mutable,
            //             None => mir::Mutability::Const,
            //         },
            //         value: builder.build_expression(&global.value),
            //     })
            //     .collect(),
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

    fn build_function(&self, func: &hir::Function) -> mir::Function {
        let scopes = self.create_linked_scopes(&func.stack);
        let block = self.build_block_expression(&scopes, &func.block);

        mir::Function {
            symbol: func.name.symbol,
            ty: self.to_mir_function_type(func.ty.clone()),
            block,
        }
    }

    fn build_expression(
        &self,
        scopes: &Vec<ScopeWithLocals>,
        expr: &hir::Expression,
    ) -> mir::Expression {
        let ty = self.to_mir_type(expr.ty.unwrap());
        match &expr.kind {
            hir::ExprKind::Int(value) => mir::Expression {
                kind: mir::ExprKind::Int { value: *value },
                ty,
            },
            hir::ExprKind::Float(value) => mir::Expression {
                kind: mir::ExprKind::Float { value: *value },
                ty,
            },
            hir::ExprKind::Bool(value) => mir::Expression {
                kind: mir::ExprKind::Bool { value: *value },
                ty,
            },
            hir::ExprKind::Binary { .. } => self.build_binary_expression(scopes, expr),
            hir::ExprKind::Unary { .. } => self.build_unary_expression(scopes, expr),
            hir::ExprKind::Local {
                local_index,
                scope_index,
            } => {
                let scope = &scopes[scope_index.0 as usize];
                mir::Expression {
                    kind: mir::ExprKind::Local {
                        local: Rc::downgrade(&scope.locals[local_index.0 as usize]),
                        scope: Rc::downgrade(&scope.scope),
                    },
                    ty,
                }
            }
            hir::ExprKind::Global { global_index } => mir::Expression {
                kind: mir::ExprKind::Global {
                    global_index: global_index.0,
                },
                ty,
            },
            hir::ExprKind::Function(index) => mir::Expression {
                kind: mir::ExprKind::Function { index: index.0 },
                ty,
            },
            hir::ExprKind::Call { callee, arguments } => mir::Expression {
                kind: mir::ExprKind::Call {
                    callee: Box::new(self.build_expression(scopes, callee)),
                    arguments: arguments
                        .into_iter()
                        .map(|arg| self.build_expression(scopes, &arg))
                        .collect(),
                },
                ty,
            },
            hir::ExprKind::LocalDefinition {
                local_index,
                scope_index,
                expr,
            } => {
                let scope = &scopes[scope_index.0 as usize];

                mir::Expression {
                    kind: mir::ExprKind::LocalSet {
                        local: Rc::downgrade(&scope.locals[local_index.0 as usize]),
                        scope: Rc::downgrade(&scope.scope),
                        value: Box::new(self.build_expression(scopes, expr)),
                    },
                    ty,
                }
            }
            hir::ExprKind::Return { value } => mir::Expression {
                kind: mir::ExprKind::Return {
                    value: match value {
                        Some(value) => Some(Box::new(self.build_expression(scopes, value))),
                        None => None,
                    },
                },
                ty: mir::Type::Never,
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
            hir::ExprKind::Placeholder => unreachable!(),
            hir::ExprKind::Block { .. } => self.build_block_expression(scopes, expr),
            hir::ExprKind::IfElse {
                condition,
                else_block,
                then_block,
            } => {
                let condition = self.build_expression(scopes, condition);
                let then_block = self.build_expression(scopes, then_block);
                let else_block = else_block
                    .as_ref()
                    .map(|expr| self.build_expression(scopes, expr));

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
                    scope: Rc::downgrade(&scopes[scope_index.0 as usize].scope),
                    value: match value {
                        Some(value) => Some(Box::new(self.build_expression(scopes, value))),
                        None => None,
                    },
                },
                ty,
            },
            hir::ExprKind::Continue { scope_index } => mir::Expression {
                kind: mir::ExprKind::Continue {
                    scope: Rc::downgrade(&scopes[scope_index.0 as usize].scope),
                },
                ty: mir::Type::Never,
            },
            hir::ExprKind::Unreachable => mir::Expression {
                kind: mir::ExprKind::Unreachable,
                ty: mir::Type::Never,
            },
            hir::ExprKind::Loop { block, .. } => {
                let block = self.build_block_expression(scopes, block);

                mir::Expression {
                    kind: mir::ExprKind::Loop {
                        block: Box::new(block),
                    },
                    ty,
                }
            }
            hir::ExprKind::Error => panic!("invalid HIR"),
        }
    }

    fn create_linked_locals(
        &self,
        start_index: u32,
        hir_locals: &[hir::Local],
    ) -> Vec<Rc<RefCell<mir::Local>>> {
        let mut mir_locals = Vec::with_capacity(hir_locals.len());
        let mut prev: Option<Rc<RefCell<mir::Local>>> = None;

        for (index, hir_local) in hir_locals.iter().enumerate() {
            let mir_local = Rc::new(RefCell::new(mir::Local {
                index: start_index + index as u32,
                symbol: hir_local.name.symbol,
                ty: self.to_mir_type(hir_local.ty),
                mutability: match hir_local.mutability {
                    Some(_) => mir::Mutability::Mutable,
                    None => mir::Mutability::Const,
                },
                prev: prev.as_ref().map(Rc::downgrade),
                next: None,
            }));

            if let Some(prev) = &prev {
                prev.borrow_mut().next = Some(mir_local.clone());
            }
            prev = Some(mir_local.clone());
            mir_locals.push(mir_local);
        }

        mir_locals
    }

    fn create_linked_scopes(&self, frame: &hir::StackFrame) -> Vec<ScopeWithLocals> {
        let mut scopes: Vec<ScopeWithLocals> = Vec::with_capacity(frame.scopes.len());
        let mut start_index = 0;

        for (index, scope) in frame.scopes.iter().enumerate() {
            let mir_scope = Rc::new(RefCell::new(mir::BlockScope {
                index: index as u32,
                kind: match scope.kind {
                    hir::BlockKind::Block => mir::BlockKind::Block,
                    hir::BlockKind::Loop => mir::BlockKind::Loop,
                },
                parent: match scope.parent {
                    Some(index) => scopes
                        .get(index.0 as usize)
                        .map(|scope| Rc::downgrade(&scope.scope)),
                    None => None,
                },
                children: Vec::new(),
                locals: None,
                result: self.to_mir_type(scope.inferred_type.unwrap()),
            }));

            if let Some(index) = scope.parent {
                let parent = &scopes.get(index.0 as usize).unwrap().scope;
                parent.borrow_mut().children.push(Rc::downgrade(&mir_scope));
            }

            let locals = self.create_linked_locals(start_index, &scope.locals);
            start_index += locals.len() as u32;
            mir_scope.borrow_mut().locals = locals.first().map(Rc::clone);

            scopes.push(ScopeWithLocals {
                scope: mir_scope,
                locals,
            });
        }
        scopes
    }

    fn build_block_expression(
        &self,
        scopes: &Vec<ScopeWithLocals>,
        expr: &hir::Expression,
    ) -> mir::Expression {
        match &expr.kind {
            hir::ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => {
                let expressions: Box<_> = expressions
                    .iter()
                    .map(|expr| self.build_expression(scopes, expr))
                    .chain(
                        result
                            .as_ref()
                            .map(|result| self.build_expression(scopes, result)),
                    )
                    .collect();

                mir::Expression {
                    kind: mir::ExprKind::Block {
                        scope: scopes[scope_index.0 as usize].scope.clone(),
                        expressions,
                    },
                    ty: self.to_mir_type(expr.ty.unwrap()),
                }
            }
            _ => unreachable!(),
        }
    }

    fn build_unary_expression(
        &self,
        scopes: &Vec<ScopeWithLocals>,
        expr: &hir::Expression,
    ) -> mir::Expression {
        let (operator, operand) = match &expr.kind {
            hir::ExprKind::Unary { operator, operand } => (operator.clone(), operand),
            _ => unreachable!(),
        };
        let ty = self.to_mir_type(expr.ty.unwrap());
        let value = Box::new(self.build_expression(scopes, operand));

        mir::Expression {
            kind: match operator.kind {
                ast::UnOpKind::InvertSign => mir::ExprKind::Neg { value },
                ast::UnOpKind::Not => mir::ExprKind::Eqz { value },
                ast::UnOpKind::BitNot => mir::ExprKind::BitNot { value },
            },
            ty,
        }
    }

    fn build_binary_expression(
        &self,
        scopes: &Vec<ScopeWithLocals>,
        expr: &hir::Expression,
    ) -> mir::Expression {
        let (operator, left, right) = match &expr.kind {
            hir::ExprKind::Binary {
                operator,
                left,
                right,
            } => (operator, left, right),
            _ => unreachable!(),
        };
        let ty = self.to_mir_type(expr.ty.unwrap());

        use crate::ast::BinOpKind;
        match operator.kind {
            BinOpKind::Assign => {
                let value = self.build_expression(scopes, &right);
                match left.kind {
                    hir::ExprKind::Local {
                        local_index,
                        scope_index,
                    } => {
                        let scope = &scopes[scope_index.0 as usize];
                        mir::Expression {
                            kind: mir::ExprKind::LocalSet {
                                local: Rc::downgrade(&scope.locals[local_index.0 as usize]),
                                scope: Rc::downgrade(&scope.scope),
                                value: Box::new(value),
                            },
                            ty,
                        }
                    }
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
            BinOpKind::AddAssign
            | BinOpKind::SubAssign
            | BinOpKind::DivAssign
            | BinOpKind::MulAssign
            | BinOpKind::RemAssign => {
                let hir_left = &left;
                let left = Box::new(self.build_expression(scopes, &left));
                let right = Box::new(self.build_expression(scopes, &right));
                let value_type = left.ty;
                let value = match operator.kind {
                    BinOpKind::AddAssign => mir::Expression {
                        kind: mir::ExprKind::Add { left, right },
                        ty: value_type,
                    },
                    BinOpKind::SubAssign => mir::Expression {
                        kind: mir::ExprKind::Sub { left, right },
                        ty: value_type,
                    },
                    BinOpKind::MulAssign => mir::Expression {
                        kind: mir::ExprKind::Mul { left, right },
                        ty: value_type,
                    },
                    BinOpKind::DivAssign => mir::Expression {
                        kind: mir::ExprKind::Div { left, right },
                        ty: value_type,
                    },
                    BinOpKind::RemAssign => mir::Expression {
                        kind: mir::ExprKind::Rem { left, right },
                        ty: value_type,
                    },
                    _ => unreachable!(),
                };

                match &hir_left.kind {
                    hir::ExprKind::Local {
                        local_index,
                        scope_index,
                    } => {
                        let scope = &scopes[scope_index.0 as usize];
                        mir::Expression {
                            kind: mir::ExprKind::LocalSet {
                                local: Rc::downgrade(&scope.locals[local_index.0 as usize]),
                                scope: Rc::downgrade(&scope.scope),
                                value: Box::new(value),
                            },
                            ty,
                        }
                    }
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
            BinOpKind::And => {
                let left = Box::new(self.build_expression(scopes, &left));
                let right = Box::new(self.build_expression(scopes, &right));

                mir::Expression {
                    kind: mir::ExprKind::IfElse {
                        condition: left,
                        then_block: right,
                        else_block: Some(Box::new(mir::Expression {
                            kind: mir::ExprKind::Bool { value: false },
                            ty,
                        })),
                    },
                    ty,
                }
            }
            BinOpKind::Or => {
                let left = Box::new(self.build_expression(scopes, &left));
                let right = Box::new(self.build_expression(scopes, &right));

                mir::Expression {
                    kind: mir::ExprKind::IfElse {
                        condition: left,
                        then_block: Box::new(mir::Expression {
                            kind: mir::ExprKind::Bool { value: true },
                            ty,
                        }),
                        else_block: Some(right),
                    },
                    ty,
                }
            }
            BinOpKind::BitAnd
            | BinOpKind::BitOr
            | BinOpKind::BitXor
            | BinOpKind::LeftShift
            | BinOpKind::RightShift
            | BinOpKind::Eq
            | BinOpKind::NotEq
            | BinOpKind::Add
            | BinOpKind::Sub
            | BinOpKind::Mul
            | BinOpKind::Div
            | BinOpKind::Rem
            | BinOpKind::Greater
            | BinOpKind::GreaterEq
            | BinOpKind::Less
            | BinOpKind::LessEq => {
                let left = Box::new(self.build_expression(scopes, &left));
                let right = Box::new(self.build_expression(scopes, &right));

                let kind = match operator.kind {
                    BinOpKind::BitAnd => mir::ExprKind::BitAnd { left, right },
                    BinOpKind::BitOr => mir::ExprKind::BitOr { left, right },
                    BinOpKind::BitXor => mir::ExprKind::BitXor { left, right },
                    BinOpKind::LeftShift => mir::ExprKind::LeftShift { left, right },
                    BinOpKind::RightShift => mir::ExprKind::RightShift { left, right },
                    BinOpKind::Eq => mir::ExprKind::Eq { left, right },
                    BinOpKind::NotEq => mir::ExprKind::NotEq { left, right },
                    BinOpKind::And => mir::ExprKind::And { left, right },
                    BinOpKind::Sub => mir::ExprKind::Sub { left, right },
                    BinOpKind::Mul => mir::ExprKind::Mul { left, right },
                    BinOpKind::Div => mir::ExprKind::Div { left, right },
                    BinOpKind::Rem => mir::ExprKind::Rem { left, right },
                    BinOpKind::Greater => mir::ExprKind::Greater { left, right },
                    BinOpKind::GreaterEq => mir::ExprKind::GreaterEq { left, right },
                    BinOpKind::Less => mir::ExprKind::Less { left, right },
                    BinOpKind::LessEq => mir::ExprKind::LessEq { left, right },
                    _ => unreachable!(),
                };

                mir::Expression { kind, ty }
            }
        }
    }
}
