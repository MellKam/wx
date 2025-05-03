use string_interner::StringInterner;
use string_interner::backend::StringBackend;

use super::Instruction;
use crate::{mir, wasm};

pub struct WASMBuilder {}

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

impl WASMBuilder {
    pub fn build<'a>(
        mir: &mir::MIR,
        interner: &'a StringInterner<StringBackend>,
    ) -> wasm::Module<'a> {
        let mut functions = Vec::new();
        for function in &mir.functions {
            let mut instructions = Vec::new();
            for expr in &function.body {
                WASMBuilder::build_expression(&mut instructions, expr)
            }
            functions.push(wasm::Function {
                export: function.export,
                name: interner.resolve(function.name).unwrap(),
                locals: function
                    .locals
                    .iter()
                    .map(|local| wasm::Local {
                        name: interner.resolve(local.name).unwrap(),
                        ty: wasm::ValueType::try_from(local.ty.clone()).unwrap(),
                    })
                    .collect(),
                param_count: function.param_count as u32,
                result: match function.result {
                    mir::Type::I32 => Some(wasm::ValueType::I32),
                    mir::Type::I64 => Some(wasm::ValueType::I64),
                    mir::Type::Unit => None,
                    _ => unreachable!(),
                },
                instructions,
            });
        }
        wasm::Module { functions }
    }

    fn build_expression(body: &mut Vec<Instruction>, expr: &mir::Expression) {
        match &expr.kind {
            mir::ExprKind::Function { index } => {
                body.push(wasm::Instruction::I32Const {
                    value: *index as i32,
                });
            }
            mir::ExprKind::Call { callee, arguments } => {
                for arg in arguments {
                    WASMBuilder::build_expression(body, arg);
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
                WASMBuilder::build_expression(body, &left);
                WASMBuilder::build_expression(body, &right);
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
            mir::ExprKind::Local { index } => {
                body.push(wasm::Instruction::LocalGet { index: *index });
            }
            mir::ExprKind::Mul { left, right } => {
                WASMBuilder::build_expression(body, &left);
                WASMBuilder::build_expression(body, &right);
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
            mir::ExprKind::Assign { index, value } => {
                WASMBuilder::build_expression(body, &value);
                body.push(wasm::Instruction::LocalSet { index: *index });
            }
            mir::ExprKind::Return { value } => {
                WASMBuilder::build_expression(body, &value);
                body.push(wasm::Instruction::Return);
            }
            mir::ExprKind::Sub { left, right } => {
                WASMBuilder::build_expression(body, &left);
                WASMBuilder::build_expression(body, &right);
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
                WASMBuilder::build_expression(body, &value);
                match value.ty {
                    mir::Type::I32 | mir::Type::I64 => {
                        body.push(wasm::Instruction::Drop);
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}
