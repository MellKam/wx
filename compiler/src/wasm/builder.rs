use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;

use crate::{mir, wasm};

pub struct Builder {
    expressions: Vec<wasm::Expression>,
}

impl TryFrom<mir::Type> for wasm::ValueType {
    type Error = ();

    fn try_from(value: mir::Type) -> Result<Self, Self::Error> {
        match value {
            mir::Type::Bool => Ok(wasm::ValueType::I32),
            mir::Type::I32 => Ok(wasm::ValueType::I32),
            mir::Type::I64 => Ok(wasm::ValueType::I64),
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
struct FunctionContext<'a> {
    locals: Box<[wasm::Local<'a>]>,
    scope_offsets: Box<[usize]>,
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
    pub fn build<'a>(
        mir: &mir::MIR,
        interner: &'a StringInterner<StringBackend>,
    ) -> wasm::Module<'a> {
        let mut builder = Builder {
            expressions: Vec::new(),
        };

        let mut types = HashMap::<wasm::FunctionType, wasm::TypeIndex>::new();
        let mut function_signatures = Vec::<wasm::TypeIndex>::with_capacity(mir.functions.len());
        let mut exports = Vec::<wasm::ExportItem>::new();
        let mut functions = Vec::<wasm::FunctionBody>::with_capacity(mir.functions.len());

        for (func_index, func) in mir.functions.iter().enumerate() {
            let ty = wasm::FunctionType::from(func.ty.clone());
            let next_type_index = wasm::TypeIndex(types.len() as u32);
            let type_index = types.entry(ty).or_insert(next_type_index).clone();
            function_signatures.push(type_index);
            match func.export {
                true => {
                    exports.push(wasm::ExportItem::Function(wasm::FunctionExport {
                        name: interner.resolve(func.name).unwrap(),
                        index: wasm::FunctionIndex(func_index as u32),
                    }));
                }
                false => {}
            }

            let func_expressions = match &func.block.kind {
                mir::ExprKind::Block { expressions, .. } => expressions,
                _ => unreachable!(),
            };

            let scope_offsets: Box<_> = func
                .scopes
                .iter()
                .scan(0, |offset, scope| {
                    let current = *offset;
                    *offset += scope.locals.len();
                    Some(current)
                })
                .collect();

            let flat_locals: Box<_> = func
                .scopes
                .iter()
                .flat_map(|scope| {
                    scope.locals.iter().map(|local| wasm::Local {
                        name: interner.resolve(local.name).unwrap(),
                        ty: wasm::ValueType::try_from(local.ty.clone()).unwrap(),
                    })
                })
                .collect();

            let mut ctx = FunctionContext {
                locals: flat_locals,
                scope_offsets,
            };

            let func_expressions = func_expressions
                .iter()
                .map(|expr| builder.build_expression(&mut ctx, expr))
                .collect();

            functions.push(wasm::FunctionBody {
                name: interner.resolve(func.name).unwrap(),
                locals: ctx.locals,
                expressions: func_expressions,
            });
        }

        wasm::Module {
            types: wasm::TypeSection {
                signatures: types.into_iter().map(|(ty, _)| ty).collect::<Box<_>>(),
            },
            functions: wasm::FunctionSection {
                functions: function_signatures.into_boxed_slice(),
            },
            exports: wasm::ExportSection {
                items: exports.into_boxed_slice(),
            },
            code: wasm::CodeSection {
                expressions: builder.expressions.into_boxed_slice(),
                functions: functions.into_boxed_slice(),
            },
        }
    }

    fn push_expr(&mut self, expr: wasm::Expression) -> wasm::ExprIndex {
        let index = self.expressions.len() as u32;
        self.expressions.push(expr);
        wasm::ExprIndex(index)
    }

    fn build_expression<'a>(
        &mut self,
        ctx: &FunctionContext<'a>,
        expr: &mir::Expression,
    ) -> wasm::ExprIndex {
        match &expr.kind {
            mir::ExprKind::Noop => self.push_expr(wasm::Expression::Nop),
            mir::ExprKind::Function { index } => self.push_expr(wasm::Expression::I32Const {
                value: *index as i32,
            }),
            mir::ExprKind::Bool { value } => self.push_expr(wasm::Expression::I32Const {
                value: if *value { 1 } else { 0 },
            }),
            mir::ExprKind::Call { callee, arguments } => {
                let expr = wasm::Expression::Call {
                    function: wasm::FunctionIndex(*callee),
                    arguments: arguments
                        .iter()
                        .map(|arg| self.build_expression(ctx, arg))
                        .collect(),
                };
                self.push_expr(expr)
            }
            mir::ExprKind::Int { value } => {
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Const {
                        value: *value as i32,
                    },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Const { value: *value },
                    _ => {
                        panic!("unsupported type for int expression {:?}", expr.ty)
                    }
                })
            }
            mir::ExprKind::Add { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Add { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Add { left, right },
                    ty => panic!("unsupported type for add operation {:?}", ty),
                })
            }
            mir::ExprKind::Mul { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Mul { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Mul { left, right },
                    ty => panic!("unsupported type for mul operation {:?}", ty),
                })
            }
            mir::ExprKind::Div { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32DivS { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64DivS { left, right },
                    ty => panic!("unsupported type for div operation {:?}", ty),
                })
            }
            mir::ExprKind::Rem { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32RemS { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64RemS { left, right },
                    ty => panic!("unsupported type for rem operation {:?}", ty),
                })
            }
            mir::ExprKind::NotEq { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);

                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Ne { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Ne { left, right },
                    ty => panic!("unsupported type for not equal operation {:?}", ty),
                })
            }
            mir::ExprKind::And { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32And { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64And { left, right },
                    ty => panic!("unsupported type for and operation {:?}", ty),
                })
            }
            mir::ExprKind::Or { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Or { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Or { left, right },
                    ty => panic!("unsupported type for or operation {:?}", ty),
                })
            }
            mir::ExprKind::Local {
                local_index,
                scope_index,
            } => {
                let local = ctx.get_flat_index(*scope_index, *local_index);
                self.push_expr(wasm::Expression::LocalGet { local })
            }
            mir::ExprKind::Assign {
                local_index,
                scope_index,
                value,
            } => {
                let value = self.build_expression(ctx, &value);
                let local = ctx.get_flat_index(*scope_index, *local_index);
                self.push_expr(wasm::Expression::LocalSet { local, value })
            }
            mir::ExprKind::Return { value } => {
                let value = match value {
                    Some(value) => Some(self.build_expression(ctx, value)),
                    None => None,
                };
                self.push_expr(wasm::Expression::Return { value })
            }
            mir::ExprKind::Sub { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Sub { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Sub { left, right },
                    ty => panic!("unsupported type for sub operation {:?}", ty),
                })
            }
            mir::ExprKind::Drop { value } => {
                let value = self.build_expression(ctx, &value);
                match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32 | wasm::ValueType::I64) => {
                        self.push_expr(wasm::Expression::Drop { value })
                    }
                    // TODO: fix drop of empty types
                    _ => unreachable!(),
                }
            }
            mir::ExprKind::Eq { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                // TODO: handle eqz case
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Eq { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Eq { left, right },
                    ty => panic!("unsupported type for equal operation {:?}", ty),
                })
            }
            mir::ExprKind::Eqz { value } => {
                let value = self.build_expression(ctx, &value);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Eqz { value },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Eqz { value },
                    ty => panic!("unsupported type for eqz operation {:?}", ty),
                })
            }
            mir::ExprKind::Block { expressions, .. } => {
                let expr = wasm::Expression::Block {
                    expressions: expressions
                        .iter()
                        .map(|expr| self.build_expression(ctx, expr))
                        .collect(),
                    result: match wasm::ValueType::try_from(expr.ty.clone()) {
                        Ok(wasm::ValueType::I32) => {
                            wasm::BlockResult::SingleValue(wasm::ValueType::I32)
                        }
                        Ok(wasm::ValueType::I64) => {
                            wasm::BlockResult::SingleValue(wasm::ValueType::I64)
                        }
                        _ => wasm::BlockResult::Empty,
                    },
                };
                self.push_expr(expr)
            }
            mir::ExprKind::Break { value, .. } => {
                let value = match value {
                    Some(value) => Some(self.build_expression(ctx, value)),
                    None => None,
                };
                // hardcoded zero for now, fix this later
                self.push_expr(wasm::Expression::Break { depth: 0, value })
            }
            mir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.build_expression(ctx, &condition);
                let then_branch = self.build_expression(ctx, &then_block);
                let else_branch = else_block
                    .as_ref()
                    .map(|else_block| self.build_expression(ctx, else_block));
                self.push_expr(wasm::Expression::IfElse {
                    condition,
                    result: wasm::BlockResult::from(expr.ty.clone()),
                    then_branch,
                    else_branch,
                })
            }
            mir::ExprKind::BitAnd { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32And { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64And { left, right },
                    ty => panic!("unsupported type for bit and operation {:?}", ty),
                })
            }
            mir::ExprKind::BitOr { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Or { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Or { left, right },
                    ty => panic!("unsupported type for bit or operation {:?}", ty),
                })
            }
            mir::ExprKind::BitXor { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Xor { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Xor { left, right },
                    ty => panic!("unsupported type for bit xor operation {:?}", ty),
                })
            }
            mir::ExprKind::BitNot { value } => {
                let left = self.build_expression(ctx, &value);
                let right = self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Const { value: -1 },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Const { value: -1 },
                    ty => panic!("unsupported type for bit not operation {:?}", ty),
                });
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Xor { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Xor { left, right },
                    ty => panic!("unsupported type for bit not operation {:?}", ty),
                })
            }
            mir::ExprKind::LeftShift { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32Shl { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64Shl { left, right },
                    ty => panic!("unsupported type for left shift operation {:?}", ty),
                })
            }
            mir::ExprKind::RightShift { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32ShrS { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64ShrS { left, right },
                    ty => panic!("unsupported type for right shift operation {:?}", ty),
                })
            }
            mir::ExprKind::Less { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32LtS { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64LtS { left, right },
                    ty => panic!("unsupported type for less operation {:?}", ty),
                })
            }
            mir::ExprKind::LessEq { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32LeS { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64LeS { left, right },
                    ty => panic!("unsupported type for less equal operation {:?}", ty),
                })
            }
            mir::ExprKind::Greater { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32GtS { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64GtS { left, right },
                    ty => panic!("unsupported type for greater operation {:?}", ty),
                })
            }
            mir::ExprKind::GreaterEq { left, right } => {
                let left = self.build_expression(ctx, &left);
                let right = self.build_expression(ctx, &right);
                self.push_expr(match wasm::ValueType::try_from(expr.ty.clone()) {
                    Ok(wasm::ValueType::I32) => wasm::Expression::I32GeS { left, right },
                    Ok(wasm::ValueType::I64) => wasm::Expression::I64GeS { left, right },
                    ty => panic!("unsupported type for greater equal operation {:?}", ty),
                })
            }
        }
    }
}
