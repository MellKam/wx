use std::collections::HashMap;

use crate::wasm::{self, TableIndex};
use crate::{hir, mir};

pub struct Builder {
    expressions: Vec<wasm::Expression>,
    table: Vec<wasm::FuncIndex>,
}

impl TryFrom<mir::Type> for wasm::ValueType {
    type Error = ();

    fn try_from(value: mir::Type) -> Result<Self, Self::Error> {
        match value {
            mir::Type::Bool => Ok(wasm::ValueType::I32),
            mir::Type::I32 => Ok(wasm::ValueType::I32),
            mir::Type::I64 => Ok(wasm::ValueType::I64),
            mir::Type::F32 => Ok(wasm::ValueType::F32),
            mir::Type::F64 => Ok(wasm::ValueType::F64),
            mir::Type::U32 => Ok(wasm::ValueType::I32),
            mir::Type::U64 => Ok(wasm::ValueType::I64),
            mir::Type::Function(_) => Ok(wasm::ValueType::I32),
            _ => Err(()),
        }
    }
}

impl From<mir::Type> for wasm::BlockResult {
    fn from(value: mir::Type) -> Self {
        wasm::ValueType::try_from(value)
            .map(wasm::BlockResult::SingleValue)
            .unwrap_or(wasm::BlockResult::Empty)
    }
}

#[derive(Debug)]
struct FunctionContext<'mir> {
    locals: Box<[wasm::Local]>,
    scope_offsets: Box<[usize]>,
    scopes: &'mir Vec<mir::BlockScope>,
    scope_index: mir::ScopeIndex,
}

impl FunctionContext<'_> {
    fn get_flat_index(
        &self,
        scope_index: mir::ScopeIndex,
        local_index: mir::LocalIndex,
    ) -> wasm::LocalIndex {
        let scope_offset = self.scope_offsets[scope_index.0 as usize];
        wasm::LocalIndex(scope_offset as u32 + local_index)
    }

    pub fn get_break_depth(&self, target_scope: mir::ScopeIndex) -> Option<u32> {
        let mut index = self.scope_index;
        let mut depth = 0;

        loop {
            let scope = &self.scopes[index.0 as usize];
            depth += match scope.kind {
                mir::BlockKind::Loop => depth + 2,
                mir::BlockKind::Block => depth + 1,
            };

            if index.0 == target_scope.0 {
                return Some(depth - 1);
            }

            match scope.parent {
                Some(scope_index) => {
                    index = scope_index;
                }
                None => return None,
            }
        }
    }

    pub fn get_continue_depth(&self, target_scope: mir::ScopeIndex) -> Option<u32> {
        let mut index = self.scope_index;
        let mut depth = 0;

        loop {
            if index.0 == target_scope.0 {
                return Some(depth);
            }

            let scope = self.scopes.get(index.0 as usize).unwrap();
            match scope.parent {
                Some(scope_index) => {
                    index = scope_index;
                    depth += match scope.kind {
                        mir::BlockKind::Loop => 2, // 1 for the loop, 1 for the block
                        mir::BlockKind::Block => 1,
                    };
                }
                None => return None,
            }
        }
    }
}

impl From<mir::FunctionType> for wasm::FunctionType {
    fn from(ty: mir::FunctionType) -> Self {
        wasm::FunctionType {
            param_count: ty.param_count,
            param_results: ty
                .params_results
                .iter()
                .map(|ty| wasm::ValueType::try_from(ty.clone()).unwrap())
                .collect(),
        }
    }
}

impl Builder {
    pub fn build<'a>(mir: &mir::MIR) -> Result<wasm::Module, ()> {
        let mut builder = Builder {
            expressions: Vec::new(),
            table: Vec::new(),
        };

        let mut types = HashMap::<wasm::FunctionType, wasm::TypeIndex>::new();
        let mut function_signatures = Vec::<wasm::TypeIndex>::with_capacity(mir.functions.len());
        let exports: Box<_> = mir
            .exports
            .iter()
            .map(|item| match item {
                hir::ExportItem::Global { global_index } => {
                    let global = mir.globals.get(global_index.0 as usize).unwrap();

                    wasm::ExportItem::Global {
                        name: global.name,
                        global_index: wasm::GlobalIndex(global_index.0),
                    }
                }
                hir::ExportItem::Function { func_index } => {
                    let func = mir.functions.get(func_index.0 as usize).unwrap();

                    wasm::ExportItem::Function {
                        func_index: wasm::FuncIndex(func_index.0),
                        name: func.name,
                    }
                }
            })
            .collect();
        let mut functions = Vec::<wasm::FunctionBody>::with_capacity(mir.functions.len());

        for func in mir.functions.iter() {
            let ty = wasm::FunctionType::from(func.ty.clone());
            let next_type_index = wasm::TypeIndex(types.len() as u32);
            let type_index = types.entry(ty.clone()).or_insert(next_type_index).clone();
            function_signatures.push(type_index);

            let expressions = match &func.block.kind {
                mir::ExprKind::Block { expressions, .. } => expressions,
                _ => unreachable!(),
            };

            let scope_offsets: Box<_> = func
                .frame
                .iter()
                .scan(0, |offset, scope| {
                    let current = *offset;
                    *offset += scope.locals.len();
                    Some(current)
                })
                .collect();

            let flat_locals: Box<_> = func
                .frame
                .iter()
                .flat_map(|scope| {
                    scope.locals.iter().map(|local| wasm::Local {
                        name: local.name,
                        ty: wasm::ValueType::try_from(local.ty.clone()).unwrap(),
                    })
                })
                .collect();

            let mut ctx = FunctionContext {
                locals: flat_locals,
                scope_offsets,
                scope_index: mir::ScopeIndex(0),
                scopes: &func.frame,
            };

            let expressions = expressions
                .iter()
                .map(|expr| builder.build_expression(&mut ctx, expr))
                .collect::<Result<Box<_>, ()>>()?;

            functions.push(wasm::FunctionBody {
                name: func.name,
                locals: ctx.locals,
                expressions,
            });
        }

        Ok(wasm::Module {
            globals: wasm::GlobalSection {
                globals: mir
                    .globals
                    .iter()
                    .map(|global| wasm::Global {
                        name: global.name,
                        ty: wasm::ValueType::try_from(global.ty.clone()).unwrap(),
                        mutability: global.mutability == hir::Mutability::Mutable,
                        value: builder.build_global_expr(global),
                    })
                    .collect::<Box<_>>(),
            },
            tables: wasm::TableSection {
                tables: match builder.table.len() {
                    0 => Box::new([]),
                    _ => Box::new([wasm::TableType {
                        ty: wasm::RefType::FuncRef,
                        limits: wasm::ResizableLimits::Initial(builder.table.len() as u32),
                    }]),
                },
            },
            elements: wasm::ElementSection {
                segments: match builder.table.len() {
                    0 => Box::new([]),
                    _ => Box::new([wasm::ElementSegment {
                        table_index: wasm::TableIndex(0),
                        offset: 0,
                        indices: builder.table.into_boxed_slice(),
                    }]),
                },
            },
            types: wasm::TypeSection {
                signatures: {
                    let mut sorted_types: Vec<_> = types.into_iter().collect();
                    sorted_types.sort_by_key(|&(_, index)| index);
                    sorted_types
                        .into_iter()
                        .map(|(ty, _)| ty)
                        .collect::<Box<_>>()
                },
            },
            functions: wasm::FunctionSection {
                types: function_signatures.into_boxed_slice(),
            },
            exports: wasm::ExportSection { items: exports },
            code: wasm::CodeSection {
                expressions: builder.expressions.into_boxed_slice(),
                functions: functions.into_boxed_slice(),
            },
        })
    }

    fn build_global_expr(&self, global: &mir::Global) -> wasm::Expression {
        match global.value.kind {
            mir::ExprKind::Int { value } => match global.ty {
                mir::Type::I32 => wasm::Expression::I32Const {
                    value: value as i32,
                },
                mir::Type::I64 => wasm::Expression::I64Const { value },
                _ => unreachable!(),
            },
            mir::ExprKind::Float { value } => match global.ty {
                mir::Type::F32 => wasm::Expression::F32Const {
                    value: value as f32,
                },
                mir::Type::F64 => wasm::Expression::F64Const { value },
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn push_expr(&mut self, expr: wasm::Expression) -> wasm::ExprIndex {
        let index = self.expressions.len() as u32;
        self.expressions.push(expr);
        wasm::ExprIndex(index)
    }

    fn build_expression<'mir, 'wasm>(
        &mut self,
        ctx: &mut FunctionContext<'mir>,
        expr: &mir::Expression,
    ) -> Result<wasm::ExprIndex, ()> {
        match &expr.kind {
            mir::ExprKind::Noop => Ok(self.push_expr(wasm::Expression::Nop)),
            mir::ExprKind::Function { index } => {
                let table_index = self.table.len() as i32;
                self.table.push(wasm::FuncIndex(*index));
                let expr = wasm::Expression::I32Const { value: table_index };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Bool { value } => {
                let expr = wasm::Expression::I32Const {
                    value: if *value { 1 } else { 0 },
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Call { callee, arguments } => {
                let arguments = arguments
                    .iter()
                    .map(|arg| self.build_expression(ctx, arg))
                    .collect::<Result<Box<_>, _>>()?;

                let expr = match callee.kind {
                    mir::ExprKind::Function { index } => wasm::Expression::Call {
                        function: wasm::FuncIndex(index),
                        arguments,
                    },
                    _ => {
                        let type_index = match callee.ty {
                            mir::Type::Function(type_index) => wasm::TypeIndex(type_index),
                            _ => unreachable!("callee must be a function type"),
                        };
                        let function = self.build_expression(ctx, callee)?;

                        wasm::Expression::CallIndirect {
                            expr: function,
                            table_index: TableIndex(0),
                            type_index,
                            arguments,
                        }
                    }
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Int { value } => {
                let expr = match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => wasm::Expression::I32Const {
                        value: *value as i32,
                    },
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Const { value: *value },
                    _ => return Err(()),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Float { value } => {
                let expr = match expr.ty {
                    mir::Type::F32 => wasm::Expression::F32Const {
                        value: *value as f32,
                    },
                    mir::Type::F64 => wasm::Expression::F64Const { value: *value },
                    _ => return Err(()),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Add { left, right } => {
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => wasm::Expression::I32Add { left, right },
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Add { left, right },
                    mir::Type::F32 => wasm::Expression::F32Add { left, right },
                    mir::Type::F64 => wasm::Expression::F64Add { left, right },
                    mir::Type::Bool
                    | mir::Type::Function(_)
                    | mir::Type::Unit
                    | mir::Type::Never => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Mul { left, right } => {
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => wasm::Expression::I32Mul { left, right },
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Mul { left, right },
                    mir::Type::F32 => wasm::Expression::F32Mul { left, right },
                    mir::Type::F64 => wasm::Expression::F64Mul { left, right },
                    mir::Type::Bool
                    | mir::Type::Function(_)
                    | mir::Type::Unit
                    | mir::Type::Never => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Div { left, right } => {
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match expr.ty {
                    mir::Type::I32 => wasm::Expression::I32DivS { left, right },
                    mir::Type::I64 => wasm::Expression::I64DivS { left, right },
                    mir::Type::F32 => wasm::Expression::F32Div { left, right },
                    mir::Type::F64 => wasm::Expression::F64Div { left, right },
                    mir::Type::U32 => wasm::Expression::I32DivU { left, right },
                    mir::Type::U64 => wasm::Expression::I64DivU { left, right },
                    mir::Type::Bool
                    | mir::Type::Unit
                    | mir::Type::Never
                    | mir::Type::Function(_) => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Rem { left, right } => {
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match expr.ty {
                    mir::Type::I32 => wasm::Expression::I32RemS { left, right },
                    mir::Type::I64 => wasm::Expression::I64RemS { left, right },
                    mir::Type::F32 => {
                        let div = self.push_expr(wasm::Expression::F32Div { left, right });
                        let trunc = self.push_expr(wasm::Expression::F32Trunc { value: div });
                        let mul = self.push_expr(wasm::Expression::F32Mul { left: trunc, right });
                        wasm::Expression::F32Sub { left, right: mul }
                    }
                    mir::Type::F64 => {
                        let div = self.push_expr(wasm::Expression::F64Div { left, right });
                        let trunc = self.push_expr(wasm::Expression::F64Trunc { value: div });
                        let mul = self.push_expr(wasm::Expression::F64Mul { left: trunc, right });
                        wasm::Expression::F64Sub { left, right: mul }
                    }
                    mir::Type::U32 => wasm::Expression::I32RemU { left, right },
                    mir::Type::U64 => wasm::Expression::I64RemU { left, right },
                    _ => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::NotEq { left, right } => {
                let left_ty = left.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;

                let expr = match left_ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 | mir::Type::Function(_) => {
                        wasm::Expression::I32Ne { left, right }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Ne { left, right },
                    mir::Type::F32 => wasm::Expression::F32Ne { left, right },
                    mir::Type::F64 => wasm::Expression::F64Ne { left, right },
                    mir::Type::Unit | mir::Type::Never => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::And { left, right } => {
                let left_ty = left.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match left_ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => {
                        wasm::Expression::I32And { left, right }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64And { left, right },
                    _ => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Or { left, right } => {
                let left_ty = left.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match left_ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => {
                        wasm::Expression::I32Or { left, right }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Or { left, right },
                    _ => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Local {
                local_index,
                scope_index,
            } => {
                let expr = wasm::Expression::LocalGet {
                    local_index: ctx.get_flat_index(*scope_index, *local_index),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::LocalSet {
                local_index,
                scope_index,
                value,
            } => {
                let expr = wasm::Expression::LocalSet {
                    local_index: ctx.get_flat_index(*scope_index, *local_index),
                    value: self.build_expression(ctx, &value)?,
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Global { global_index } => {
                let expr = wasm::Expression::GlobalGet {
                    global_index: wasm::GlobalIndex(*global_index),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::GlobalSet {
                global_index,
                value,
            } => {
                let expr = wasm::Expression::GlobalSet {
                    global: wasm::GlobalIndex(*global_index),
                    value: self.build_expression(ctx, &value)?,
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Return { value } => {
                let expr = wasm::Expression::Return {
                    value: match value {
                        Some(value) => Some(self.build_expression(ctx, value)?),
                        None => None,
                    },
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Sub { left, right } => {
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => wasm::Expression::I32Sub { left, right },
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Sub { left, right },
                    mir::Type::F32 => wasm::Expression::F32Sub { left, right },
                    mir::Type::F64 => wasm::Expression::F64Sub { left, right },
                    mir::Type::Bool
                    | mir::Type::Never
                    | mir::Type::Unit
                    | mir::Type::Function(_) => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Drop { value } => match value.ty {
                mir::Type::Never | mir::Type::Unit => Err(()),
                _ => {
                    let expr = wasm::Expression::Drop {
                        value: self.build_expression(ctx, &value)?,
                    };
                    Ok(self.push_expr(expr))
                }
            },
            mir::ExprKind::Eq { left, right } => {
                let ty = left.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match ty {
                    mir::Type::I32 | mir::Type::U32 | mir::Type::Bool | mir::Type::Function(_) => {
                        wasm::Expression::I32Eq { left, right }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Eq { left, right },
                    mir::Type::F32 => wasm::Expression::F32Eq { left, right },
                    mir::Type::F64 => wasm::Expression::F64Eq { left, right },
                    mir::Type::Never | mir::Type::Unit => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Eqz { value } => {
                let value_ty = value.ty;
                let value = self.build_expression(ctx, &value)?;
                let expr = match value_ty {
                    mir::Type::I32 | mir::Type::U32 | mir::Type::Bool => {
                        wasm::Expression::I32Eqz { value }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Eqz { value },
                    mir::Type::F32 => wasm::Expression::F32Eq {
                        left: value,
                        right: self.push_expr(wasm::Expression::F32Const { value: 0.0 }),
                    },
                    mir::Type::F64 => wasm::Expression::F64Eq {
                        left: value,
                        right: self.push_expr(wasm::Expression::F64Const { value: 0.0 }),
                    },
                    mir::Type::Function(_) | mir::Type::Never | mir::Type::Unit => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Block {
                expressions,
                scope_index,
            } => {
                let expr = wasm::Expression::Block {
                    expressions: expressions
                        .iter()
                        .map(|expr| {
                            ctx.scope_index = mir::ScopeIndex(scope_index.0);
                            self.build_expression(ctx, expr)
                        })
                        .collect::<Result<_, _>>()?,
                    result: match wasm::ValueType::try_from(expr.ty) {
                        Ok(ty) => wasm::BlockResult::SingleValue(ty),
                        _ => wasm::BlockResult::Empty,
                    },
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Loop { scope_index, block } => {
                let expressions = match &block.kind {
                    mir::ExprKind::Block { expressions, .. } => expressions,
                    _ => unreachable!(),
                };

                let continue_expr = self.push_expr(wasm::Expression::Break {
                    depth: 0, // Loop itself
                    value: None,
                });

                let loop_expr = wasm::Expression::Loop {
                    expressions: expressions
                        .iter()
                        .map(|expr| {
                            ctx.scope_index = mir::ScopeIndex(scope_index.0);
                            self.build_expression(ctx, expr)
                        })
                        .chain(std::iter::once(Ok(continue_expr)))
                        .collect::<Result<_, _>>()?,
                    result: wasm::BlockResult::Empty,
                };

                let scope = &ctx.scopes[scope_index.0 as usize];
                let block_expr = wasm::Expression::Block {
                    expressions: Box::new([
                        self.push_expr(loop_expr),
                        self.push_expr(wasm::Expression::Unreachable),
                    ]),
                    result: match wasm::ValueType::try_from(scope.result) {
                        Ok(ty) => wasm::BlockResult::SingleValue(ty),
                        _ => wasm::BlockResult::Empty,
                    },
                };

                Ok(self.push_expr(block_expr))
            }
            mir::ExprKind::Continue { scope_index } => {
                let expr = wasm::Expression::Break {
                    depth: ctx.get_continue_depth(*scope_index).unwrap(),
                    value: None,
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Break { value, scope_index } => {
                let expr = wasm::Expression::Break {
                    depth: ctx.get_break_depth(*scope_index).unwrap(),
                    value: match value {
                        Some(value) => Some(self.build_expression(ctx, value)?),
                        None => None,
                    },
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Unreachable => Ok(self.push_expr(wasm::Expression::Unreachable)),
            mir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.build_expression(ctx, &condition)?;
                let then_branch = self.build_expression(ctx, &then_block)?;
                let else_branch = match else_block {
                    Some(else_block) => Some(self.build_expression(ctx, else_block)?),
                    None => None,
                };
                let expr = wasm::Expression::IfElse {
                    condition,
                    result: wasm::BlockResult::from(expr.ty),
                    then_branch,
                    else_branch,
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::BitAnd { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) | (mir::Type::U32, mir::Type::U32) => {
                        wasm::Expression::I32And { left, right }
                    }
                    (mir::Type::I64, mir::Type::I64) | (mir::Type::U64, mir::Type::U64) => {
                        wasm::Expression::I64And { left, right }
                    }
                    _ => return Err(()),
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::BitOr { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) | (mir::Type::U32, mir::Type::U32) => {
                        wasm::Expression::I32Or { left, right }
                    }
                    (mir::Type::I64, mir::Type::I64) | (mir::Type::U64, mir::Type::U64) => {
                        wasm::Expression::I64Or { left, right }
                    }
                    _ => return Err(()),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::BitXor { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) | (mir::Type::U32, mir::Type::U32) => {
                        wasm::Expression::I32Xor { left, right }
                    }
                    (mir::Type::I64, mir::Type::I64) | (mir::Type::U64, mir::Type::U64) => {
                        wasm::Expression::I64Xor { left, right }
                    }
                    _ => return Err(()),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::BitNot { value } => {
                let left = self.build_expression(ctx, &value)?;
                let expr = match value.ty {
                    mir::Type::I32 | mir::Type::U32 => wasm::Expression::I32Xor {
                        left,
                        right: self.push_expr(wasm::Expression::I32Const { value: -1 }),
                    },
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Xor {
                        left,
                        right: self.push_expr(wasm::Expression::I64Const { value: -1 }),
                    },
                    _ => return Err(()),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::LeftShift { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) | (mir::Type::U32, mir::Type::U32) => {
                        wasm::Expression::I32Shl { left, right }
                    }
                    (mir::Type::I64, mir::Type::I64) => wasm::Expression::I64Shl { left, right },
                    _ => return Err(()),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::RightShift { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) => wasm::Expression::I32ShrS { left, right },
                    (mir::Type::I64, mir::Type::I64) => wasm::Expression::I64ShrS { left, right },
                    (mir::Type::U32, mir::Type::U32) => wasm::Expression::I32ShrU { left, right },
                    (mir::Type::U64, mir::Type::U64) => wasm::Expression::I64ShrU { left, right },
                    _ => return Err(()),
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Less { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) => wasm::Expression::I32LtS { left, right },
                    (mir::Type::I64, mir::Type::I64) => wasm::Expression::I64LtS { left, right },
                    (mir::Type::F32, mir::Type::F32) => wasm::Expression::F32Lt { left, right },
                    (mir::Type::F64, mir::Type::F64) => wasm::Expression::F64Lt { left, right },
                    (mir::Type::U32, mir::Type::U32) => wasm::Expression::I32LtU { left, right },
                    (mir::Type::U64, mir::Type::U64) => wasm::Expression::I64LtU { left, right },
                    _ => {
                        return Err(());
                    }
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::LessEq { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) => wasm::Expression::I32LeS { left, right },
                    (mir::Type::I64, mir::Type::I64) => wasm::Expression::I64LeS { left, right },
                    (mir::Type::F32, mir::Type::F32) => wasm::Expression::F32Le { left, right },
                    (mir::Type::F64, mir::Type::F64) => wasm::Expression::F64Le { left, right },
                    (mir::Type::U32, mir::Type::U32) => wasm::Expression::I32LeU { left, right },
                    (mir::Type::U64, mir::Type::U64) => wasm::Expression::I64LeU { left, right },
                    _ => {
                        return Err(());
                    }
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Greater { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) => wasm::Expression::I32GtS { left, right },
                    (mir::Type::I64, mir::Type::I64) => wasm::Expression::I64GtS { left, right },
                    (mir::Type::F32, mir::Type::F32) => wasm::Expression::F32Gt { left, right },
                    (mir::Type::F64, mir::Type::F64) => wasm::Expression::F64Gt { left, right },
                    (mir::Type::U32, mir::Type::U32) => wasm::Expression::I32GtU { left, right },
                    (mir::Type::U64, mir::Type::U64) => wasm::Expression::I64GtU { left, right },
                    _ => {
                        return Err(());
                    }
                };
                Ok(self.push_expr(expr))
            }
            mir::ExprKind::GreaterEq { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) => wasm::Expression::I32GeS { left, right },
                    (mir::Type::I64, mir::Type::I64) => wasm::Expression::I64GeS { left, right },
                    (mir::Type::F32, mir::Type::F32) => wasm::Expression::F32Ge { left, right },
                    (mir::Type::F64, mir::Type::F64) => wasm::Expression::F64Ge { left, right },
                    (mir::Type::U32, mir::Type::U32) => wasm::Expression::I32GeU { left, right },
                    (mir::Type::U64, mir::Type::U64) => wasm::Expression::I64GeU { left, right },
                    _ => {
                        return Err(());
                    }
                };

                Ok(self.push_expr(expr))
            }
            mir::ExprKind::Neg { value } => {
                let value = self.build_expression(ctx, &value)?;
                let expr = match expr.ty {
                    mir::Type::I32 => wasm::Expression::I32Sub {
                        left: self.push_expr(wasm::Expression::I32Const { value: 0 }),
                        right: value,
                    },
                    mir::Type::I64 => wasm::Expression::I64Sub {
                        left: self.push_expr(wasm::Expression::I64Const { value: 0 }),
                        right: value,
                    },
                    mir::Type::F32 => wasm::Expression::F32Neg { value },
                    mir::Type::F64 => wasm::Expression::F64Neg { value },
                    mir::Type::U32
                    | mir::Type::U64
                    | mir::Type::Bool
                    | mir::Type::Function(_)
                    | mir::Type::Unit
                    | mir::Type::Never => {
                        return Err(());
                    }
                };

                Ok(self.push_expr(expr))
            }
        }
    }
}
