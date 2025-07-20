use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
struct FunctionContext {
    pub locals: Vec<wasm::Local>,
    pub local_indices: HashMap<u32, wasm::LocalIndex>,
    pub scope: Rc<RefCell<mir::BlockScope>>,
}

impl FunctionContext {
    fn new(root_scope: Rc<RefCell<mir::BlockScope>>) -> Self {
        let mut ctx = Self {
            locals: Vec::new(),
            local_indices: HashMap::new(),
            scope: root_scope.clone(),
        };

        ctx.populate_locals(root_scope);

        ctx
    }

    fn populate_locals(&mut self, scope: Rc<RefCell<mir::BlockScope>>) {
        let mut current_local = scope.borrow().locals.clone();
        loop {
            let local = match current_local {
                Some(local) => local,
                None => break,
            };

            let local_index = wasm::LocalIndex(self.locals.len() as u32);
            self.locals.push(wasm::Local {
                name: local.borrow().symbol,
                ty: wasm::ValueType::try_from(local.borrow().ty).unwrap(),
            });
            self.local_indices.insert(local.borrow().index, local_index);

            current_local = local.borrow().next.clone();
        }

        for child_scope in scope.borrow().children.iter() {
            self.populate_locals(child_scope.upgrade().unwrap());
        }
    }

    pub fn get_break_depth(&self, target_scope: Rc<RefCell<mir::BlockScope>>) -> Option<u32> {
        let mut scope = self.scope.clone();
        let mut depth = 0;

        loop {
            depth += match scope.borrow().kind {
                mir::BlockKind::Loop => depth + 2,
                mir::BlockKind::Block => depth + 1,
            };

            if scope.borrow().index == target_scope.borrow().index {
                return Some(depth - 1);
            }

            let parent = scope.borrow().parent.clone()?;
            scope = parent.upgrade().unwrap();
        }
    }

    pub fn get_continue_depth(&self, target_scope: Rc<RefCell<mir::BlockScope>>) -> Option<u32> {
        let mut scope = self.scope.clone();
        let mut depth = 0;

        loop {
            if scope.borrow().index == target_scope.borrow().index {
                return Some(depth);
            }

            let parent = scope.borrow().parent.clone()?;
            scope = parent.upgrade().unwrap();
            depth += match scope.borrow().kind {
                mir::BlockKind::Loop => 2, // 1 for the loop, 1 for the block
                mir::BlockKind::Block => 1,
            };
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
                        name: func.symbol,
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

            let (expressions, scope) = match &func.block.kind {
                mir::ExprKind::Block { expressions, scope } => (expressions, scope),
                _ => unreachable!(),
            };

            let mut ctx = FunctionContext::new(scope.clone());
            let expressions = expressions
                .iter()
                .map(|expr| builder.build_expression(&mut ctx, expr))
                .collect::<Result<Box<_>, ()>>()?;

            functions.push(wasm::FunctionBody {
                name: func.symbol,
                locals: ctx.locals.into_boxed_slice(),
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
                        mutability: global.mutability == mir::Mutability::Mutable,
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

    fn build_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &mir::Expression,
    ) -> Result<wasm::Expression, ()> {
        match &expr.kind {
            mir::ExprKind::Noop => Ok(wasm::Expression::Nop),
            mir::ExprKind::Function { index } => {
                let table_index = self.table.len() as i32;
                self.table.push(wasm::FuncIndex(*index));
                let expr = wasm::Expression::I32Const { value: table_index };
                Ok(expr)
            }
            mir::ExprKind::Bool { value } => {
                let expr = wasm::Expression::I32Const {
                    value: if *value { 1 } else { 0 },
                };
                Ok(expr)
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
                            expr: Box::new(function),
                            table_index: TableIndex(0),
                            type_index,
                            arguments,
                        }
                    }
                };
                Ok(expr)
            }
            mir::ExprKind::Int { value } => {
                let expr = match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => wasm::Expression::I32Const {
                        value: *value as i32,
                    },
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Const { value: *value },
                    _ => return Err(()),
                };
                Ok(expr)
            }
            mir::ExprKind::Float { value } => {
                let expr = match expr.ty {
                    mir::Type::F32 => wasm::Expression::F32Const {
                        value: *value as f32,
                    },
                    mir::Type::F64 => wasm::Expression::F64Const { value: *value },
                    _ => return Err(()),
                };
                Ok(expr)
            }
            mir::ExprKind::Add { left, right } => {
                let left = self.build_expression(ctx, &left)?;
                let right = self.build_expression(ctx, &right)?;
                let expr = match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => wasm::Expression::I32Add {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Add {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    mir::Type::F32 => wasm::Expression::F32Add {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    mir::Type::F64 => wasm::Expression::F64Add {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    mir::Type::Bool
                    | mir::Type::Function(_)
                    | mir::Type::Unit
                    | mir::Type::Never => return Err(()),
                };

                Ok(expr)
            }
            mir::ExprKind::Mul { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
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

                Ok(expr)
            }
            mir::ExprKind::Div { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
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

                Ok(expr)
            }
            mir::ExprKind::Rem { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match expr.ty {
                    mir::Type::I32 => wasm::Expression::I32RemS { left, right },
                    mir::Type::I64 => wasm::Expression::I64RemS { left, right },
                    mir::Type::F32 => {
                        let div = Box::new(wasm::Expression::F32Div {
                            left: left.clone(),
                            right: right.clone(),
                        });
                        let trunc = Box::new(wasm::Expression::F32Trunc { value: div });
                        let mul = Box::new(wasm::Expression::F32Mul { left: trunc, right });
                        wasm::Expression::F32Sub { left, right: mul }
                    }
                    mir::Type::F64 => {
                        let div = Box::new(wasm::Expression::F64Div {
                            left: left.clone(),
                            right: right.clone(),
                        });
                        let trunc = Box::new(wasm::Expression::F64Trunc { value: div });
                        let mul = Box::new(wasm::Expression::F64Mul { left: trunc, right });
                        wasm::Expression::F64Sub { left, right: mul }
                    }
                    mir::Type::U32 => wasm::Expression::I32RemU { left, right },
                    mir::Type::U64 => wasm::Expression::I64RemU { left, right },
                    _ => return Err(()),
                };

                Ok(expr)
            }
            mir::ExprKind::NotEq { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                let expr = match left_ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 | mir::Type::Function(_) => {
                        wasm::Expression::I32Ne { left, right }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Ne { left, right },
                    mir::Type::F32 => wasm::Expression::F32Ne { left, right },
                    mir::Type::F64 => wasm::Expression::F64Ne { left, right },
                    mir::Type::Unit | mir::Type::Never => return Err(()),
                };

                Ok(expr)
            }
            mir::ExprKind::And { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match left_ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => {
                        wasm::Expression::I32And { left, right }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64And { left, right },
                    _ => return Err(()),
                };

                Ok(expr)
            }
            mir::ExprKind::Or { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match left_ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => {
                        wasm::Expression::I32Or { left, right }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Or { left, right },
                    _ => return Err(()),
                };

                Ok(expr)
            }
            mir::ExprKind::Local { local, .. } => {
                let local_index = ctx
                    .local_indices
                    .get(&local.upgrade().unwrap().borrow().index)
                    .copied()
                    .unwrap();

                Ok(wasm::Expression::LocalGet { local_index })
            }
            mir::ExprKind::LocalSet { local, value, .. } => {
                let local_index = ctx
                    .local_indices
                    .get(&local.upgrade().unwrap().borrow().index)
                    .copied()
                    .unwrap();

                Ok(wasm::Expression::LocalSet {
                    local_index,
                    value: Box::new(self.build_expression(ctx, &value)?),
                })
            }
            mir::ExprKind::Global { global_index } => Ok(wasm::Expression::GlobalGet {
                global_index: wasm::GlobalIndex(*global_index),
            }),
            mir::ExprKind::GlobalSet {
                global_index,
                value,
            } => Ok(wasm::Expression::GlobalSet {
                global: wasm::GlobalIndex(*global_index),
                value: Box::new(self.build_expression(ctx, &value)?),
            }),
            mir::ExprKind::Return { value } => Ok(wasm::Expression::Return {
                value: match value {
                    Some(value) => Some(Box::new(self.build_expression(ctx, value)?)),
                    None => None,
                },
            }),
            mir::ExprKind::Sub { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
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

                Ok(expr)
            }
            mir::ExprKind::Drop { value } => match value.ty {
                mir::Type::Never | mir::Type::Unit => Err(()),
                _ => Ok(wasm::Expression::Drop {
                    value: Box::new(self.build_expression(ctx, &value)?),
                }),
            },
            mir::ExprKind::Eq { left, right } => {
                let ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match ty {
                    mir::Type::I32 | mir::Type::U32 | mir::Type::Bool | mir::Type::Function(_) => {
                        wasm::Expression::I32Eq { left, right }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Eq { left, right },
                    mir::Type::F32 => wasm::Expression::F32Eq { left, right },
                    mir::Type::F64 => wasm::Expression::F64Eq { left, right },
                    mir::Type::Never | mir::Type::Unit => return Err(()),
                };

                Ok(expr)
            }
            mir::ExprKind::Eqz { value } => {
                let value_ty = value.ty;
                let value = Box::new(self.build_expression(ctx, &value)?);
                let expr = match value_ty {
                    mir::Type::I32 | mir::Type::U32 | mir::Type::Bool => {
                        wasm::Expression::I32Eqz { value }
                    }
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Eqz { value },
                    mir::Type::F32 => wasm::Expression::F32Eq {
                        left: value,
                        right: Box::new(wasm::Expression::F32Const { value: 0.0 }),
                    },
                    mir::Type::F64 => wasm::Expression::F64Eq {
                        left: value,
                        right: Box::new(wasm::Expression::F64Const { value: 0.0 }),
                    },
                    mir::Type::Function(_) | mir::Type::Never | mir::Type::Unit => return Err(()),
                };

                Ok(expr)
            }
            mir::ExprKind::Block { expressions, scope } => {
                let expr = wasm::Expression::Block {
                    expressions: expressions
                        .iter()
                        .map(|expr| {
                            ctx.scope = scope.clone();
                            self.build_expression(ctx, expr)
                        })
                        .collect::<Result<_, _>>()?,
                    result: match wasm::ValueType::try_from(expr.ty) {
                        Ok(ty) => wasm::BlockResult::SingleValue(ty),
                        _ => wasm::BlockResult::Empty,
                    },
                };
                Ok(expr)
            }
            mir::ExprKind::Loop { block } => {
                let (expressions, scope) = match &block.kind {
                    mir::ExprKind::Block { expressions, scope } => (expressions, scope),
                    _ => unreachable!(),
                };

                let loop_expr = wasm::Expression::Loop {
                    expressions: expressions
                        .iter()
                        .map(|expr| {
                            ctx.scope = scope.clone();
                            self.build_expression(ctx, expr)
                        })
                        .chain(std::iter::once(Ok(wasm::Expression::Break {
                            depth: 0, // Loop itself
                            value: None,
                        })))
                        .collect::<Result<_, _>>()?,
                    result: wasm::BlockResult::Empty,
                };

                let block_expr = wasm::Expression::Block {
                    expressions: Box::new([loop_expr, wasm::Expression::Unreachable]),
                    result: match wasm::ValueType::try_from(scope.borrow().result) {
                        Ok(ty) => wasm::BlockResult::SingleValue(ty),
                        _ => wasm::BlockResult::Empty,
                    },
                };

                Ok(block_expr)
            }
            mir::ExprKind::Continue { scope } => {
                let expr = wasm::Expression::Break {
                    depth: ctx.get_continue_depth(scope.upgrade().unwrap()).unwrap(),
                    value: None,
                };
                Ok(expr)
            }
            mir::ExprKind::Break { value, scope } => {
                let expr = wasm::Expression::Break {
                    depth: ctx.get_break_depth(scope.upgrade().unwrap()).unwrap(),
                    value: match value {
                        Some(value) => Some(Box::new(self.build_expression(ctx, value)?)),
                        None => None,
                    },
                };
                Ok(expr)
            }
            mir::ExprKind::Unreachable => Ok(wasm::Expression::Unreachable),
            mir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let condition = Box::new(self.build_expression(ctx, &condition)?);
                let then_branch = Box::new(self.build_expression(ctx, &then_block)?);
                let else_branch = match else_block {
                    Some(else_block) => Some(Box::new(self.build_expression(ctx, else_block)?)),
                    None => None,
                };
                let expr = wasm::Expression::IfElse {
                    condition,
                    result: wasm::BlockResult::from(expr.ty),
                    then_branch,
                    else_branch,
                };

                Ok(expr)
            }
            mir::ExprKind::BitAnd { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) | (mir::Type::U32, mir::Type::U32) => {
                        wasm::Expression::I32And { left, right }
                    }
                    (mir::Type::I64, mir::Type::I64) | (mir::Type::U64, mir::Type::U64) => {
                        wasm::Expression::I64And { left, right }
                    }
                    _ => return Err(()),
                };

                Ok(expr)
            }
            mir::ExprKind::BitOr { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) | (mir::Type::U32, mir::Type::U32) => {
                        wasm::Expression::I32Or { left, right }
                    }
                    (mir::Type::I64, mir::Type::I64) | (mir::Type::U64, mir::Type::U64) => {
                        wasm::Expression::I64Or { left, right }
                    }
                    _ => return Err(()),
                };
                Ok(expr)
            }
            mir::ExprKind::BitXor { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) | (mir::Type::U32, mir::Type::U32) => {
                        wasm::Expression::I32Xor { left, right }
                    }
                    (mir::Type::I64, mir::Type::I64) | (mir::Type::U64, mir::Type::U64) => {
                        wasm::Expression::I64Xor { left, right }
                    }
                    _ => return Err(()),
                };
                Ok(expr)
            }
            mir::ExprKind::BitNot { value } => {
                let left = Box::new(self.build_expression(ctx, &value)?);
                let expr = match value.ty {
                    mir::Type::I32 | mir::Type::U32 => wasm::Expression::I32Xor {
                        left,
                        right: Box::new(wasm::Expression::I32Const { value: -1 }),
                    },
                    mir::Type::I64 | mir::Type::U64 => wasm::Expression::I64Xor {
                        left,
                        right: Box::new(wasm::Expression::I64Const { value: -1 }),
                    },
                    _ => return Err(()),
                };
                Ok(expr)
            }
            mir::ExprKind::LeftShift { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) | (mir::Type::U32, mir::Type::U32) => {
                        wasm::Expression::I32Shl { left, right }
                    }
                    (mir::Type::I64, mir::Type::I64) => wasm::Expression::I64Shl { left, right },
                    _ => return Err(()),
                };
                Ok(expr)
            }
            mir::ExprKind::RightShift { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
                let expr = match (left_ty, right_ty) {
                    (mir::Type::I32, mir::Type::I32) => wasm::Expression::I32ShrS { left, right },
                    (mir::Type::I64, mir::Type::I64) => wasm::Expression::I64ShrS { left, right },
                    (mir::Type::U32, mir::Type::U32) => wasm::Expression::I32ShrU { left, right },
                    (mir::Type::U64, mir::Type::U64) => wasm::Expression::I64ShrU { left, right },
                    _ => return Err(()),
                };
                Ok(expr)
            }
            mir::ExprKind::Less { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
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
                Ok(expr)
            }
            mir::ExprKind::LessEq { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
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
                Ok(expr)
            }
            mir::ExprKind::Greater { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
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
                Ok(expr)
            }
            mir::ExprKind::GreaterEq { left, right } => {
                let left_ty = left.ty;
                let right_ty = right.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);
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

                Ok(expr)
            }
            mir::ExprKind::Neg { value } => {
                let value = Box::new(self.build_expression(ctx, &value)?);
                let expr = match expr.ty {
                    mir::Type::I32 => wasm::Expression::I32Sub {
                        left: Box::new(wasm::Expression::I32Const { value: 0 }),
                        right: value,
                    },
                    mir::Type::I64 => wasm::Expression::I64Sub {
                        left: Box::new(wasm::Expression::I64Const { value: 0 }),
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

                Ok(expr)
            }
        }
    }
}
