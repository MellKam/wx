use string_interner::StringInterner;
use string_interner::backend::StringBackend;

use crate::{mir, wasm};

pub struct Builder<'a> {
    interner: &'a StringInterner<StringBackend>,
}

impl TryFrom<mir::Type> for wasm::ValueType {
    type Error = ();

    fn try_from(value: mir::Type) -> Result<Self, Self::Error> {
        match value {
            mir::Type::I32 => Ok(wasm::ValueType::I32),
            mir::Type::I64 => Ok(wasm::ValueType::I64),
            mir::Type::Function(_) => Ok(wasm::ValueType::I32),
            _ => Err(()),
        }
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

impl<'a> Builder<'a> {
    pub fn build(mir: &mir::MIR, interner: &'a StringInterner<StringBackend>) -> wasm::Module<'a> {
        let builder = Builder { interner };
        let mut functions = Vec::new();
        for function in &mir.functions {
            let expressions = match &function.block.kind {
                mir::ExprKind::Block { expressions, .. } => expressions,
                _ => panic!("expected block expression"),
            };

            let scope_offsets: Box<_> = function
                .scopes
                .iter()
                .scan(0, |offset, scope| {
                    let current = *offset;
                    *offset += scope.locals.len();
                    Some(current)
                })
                .collect();

            let flat_locals: Box<_> = function
                .scopes
                .iter()
                .flat_map(|scope| {
                    scope.locals.iter().map(|local| wasm::Local {
                        name: interner.resolve(local.name).unwrap(),
                        ty: wasm::ValueType::try_from(local.ty.clone()).unwrap(),
                    })
                })
                .collect();

            let ctx = FunctionContext {
                locals: flat_locals,
                scope_offsets,
            };

            let mut instructions = Vec::new();
            for expr in expressions {
                builder.build_expression(&ctx, &mut instructions, expr);
            }

            functions.push(wasm::Function {
                export: function.export,
                name: interner.resolve(function.name).unwrap(),
                locals: ctx.locals,
                ty: wasm::FunctionType::from(function.ty.clone()),
                instructions,
            });
        }
        wasm::Module { functions }
    }

    fn build_expression(
        &self,
        ctx: &FunctionContext<'a>,
        body: &mut Vec<wasm::Instruction>,
        expr: &mir::Expression,
    ) {
        match &expr.kind {
            mir::ExprKind::Noop => {}
            mir::ExprKind::Function { index } => {
                body.push(wasm::Instruction::I32Const {
                    value: *index as i32,
                });
            }
            mir::ExprKind::Call { callee, arguments } => {
                for arg in arguments {
                    self.build_expression(ctx, body, arg);
                }
                body.push(wasm::Instruction::Call { index: *callee });
            }
            mir::ExprKind::Int { value } => {
                let instruction = match expr.ty {
                    mir::Type::I32 => wasm::Instruction::I32Const {
                        value: value.clone() as i32,
                    },
                    mir::Type::I64 => wasm::Instruction::I64Const {
                        value: value.clone(),
                    },
                    _ => {
                        panic!("unsupported type for int expression {:?}", expr.ty)
                    }
                };
                body.push(instruction);
            }
            mir::ExprKind::Add { left, right } => {
                self.build_expression(ctx, body, &left);
                self.build_expression(ctx, body, &right);
                match &expr.ty {
                    mir::Type::I32 => {
                        body.push(wasm::Instruction::I32Add);
                    }
                    mir::Type::I64 => {
                        body.push(wasm::Instruction::I64Add);
                    }
                    ty => panic!("unsupported type for add operation {:?}", ty),
                }
            }
            mir::ExprKind::Local {
                local_index,
                scope_index,
            } => {
                let index = ctx.get_flat_index(*scope_index, *local_index);
                body.push(wasm::Instruction::LocalGet { index });
            }
            mir::ExprKind::Mul { left, right } => {
                self.build_expression(ctx, body, &left);
                self.build_expression(ctx, body, &right);
                match expr.ty {
                    mir::Type::I32 => {
                        body.push(wasm::Instruction::I32Mul);
                    }
                    mir::Type::I64 => {
                        body.push(wasm::Instruction::I64Mul);
                    }
                    _ => unreachable!(),
                }
            }
            mir::ExprKind::Assign {
                local_index,
                scope_index,
                value,
            } => {
                self.build_expression(ctx, body, &value);
                let index = ctx.get_flat_index(*scope_index, *local_index);
                body.push(wasm::Instruction::LocalSet { index });
            }
            mir::ExprKind::Return { value } => {
                self.build_expression(ctx, body, &value);
                body.push(wasm::Instruction::Return);
            }
            mir::ExprKind::Sub { left, right } => {
                self.build_expression(ctx, body, &left);
                self.build_expression(ctx, body, &right);
                match expr.ty {
                    mir::Type::I32 => {
                        body.push(wasm::Instruction::I32Sub);
                    }
                    mir::Type::I64 => {
                        body.push(wasm::Instruction::I64Sub);
                    }
                    _ => unreachable!(),
                }
            }
            mir::ExprKind::Drop { value } => {
                self.build_expression(ctx, body, &value);
                match value.ty {
                    mir::Type::I32 | mir::Type::I64 => {
                        body.push(wasm::Instruction::Drop);
                    }
                    _ => unreachable!(),
                }
            }
            mir::ExprKind::Equal { left, right } => {
                self.build_expression(ctx, body, &left);
                self.build_expression(ctx, body, &right);
                match expr.ty {
                    mir::Type::I32 => {
                        body.push(wasm::Instruction::I32Eq);
                    }
                    mir::Type::I64 => {
                        body.push(wasm::Instruction::I64Eq);
                    }
                    _ => unreachable!(),
                }
            }
            mir::ExprKind::Block { expressions, .. } => {
                body.push(wasm::Instruction::Block {
                    ty: match expr.ty {
                        mir::Type::I32 => Some(wasm::ValueType::I32),
                        mir::Type::I64 => Some(wasm::ValueType::I64),
                        _ => None,
                    },
                });
                for expr in expressions {
                    self.build_expression(ctx, body, &expr);
                }
                body.push(wasm::Instruction::End);
            }
            mir::ExprKind::Break { value, .. } => {
                if let Some(value) = value {
                    self.build_expression(ctx, body, &value);
                }
                // hardcoded zero for now, fix this later
                body.push(wasm::Instruction::Br { block_index: 0 });
            }
        }
    }
}
