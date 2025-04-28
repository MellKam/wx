use string_interner::StringInterner;
use string_interner::backend::StringBackend;

use super::Instruction;
use crate::{mir, wasm};

pub struct WASMBuilder<'a> {
    wasm: wasm::Module<'a>,
}

impl TryFrom<mir::Type> for wasm::ValueType {
    type Error = ();

    fn try_from(value: mir::Type) -> Result<Self, Self::Error> {
        match value {
            mir::Type::I32 => Ok(wasm::ValueType::I32),
            mir::Type::I64 => Ok(wasm::ValueType::I64),
            _ => Err(()),
        }
    }
}

impl<'a> WASMBuilder<'a> {
    pub fn build(mir: &mir::MIR, interner: &'a StringInterner<StringBackend>) -> wasm::Module<'a> {
        let mut functions = Vec::new();
        for function in &mir.functions {
            let mut instructions = Vec::new();
            for expr in &function.body {
                WASMBuilder::build_expression(&mut instructions, expr)
            }
            functions.push(wasm::Function {
                name: interner.resolve(function.name).unwrap(),
                locals: function
                    .locals
                    .as_slice()
                    .get(function.param_count as usize..)
                    .unwrap_or(&[])
                    .iter()
                    .map(|local| wasm::ValueType::try_from(*local).unwrap())
                    .collect(),
                param_count: function.param_count,
                signature: function
                    .locals
                    .as_slice()
                    .get(0..function.param_count as usize)
                    .unwrap_or(&[])
                    .iter()
                    .map(|local| wasm::ValueType::try_from(*local).unwrap())
                    .chain(
                        function
                            .output
                            .iter()
                            .map(|ty| wasm::ValueType::try_from(*ty).unwrap())
                            .clone(),
                    )
                    .collect(),
                instructions,
            });
        }
        wasm::Module { functions }
    }

    fn build_expression(body: &mut Vec<Instruction>, expr: &mir::Expression) {
        match &expr.kind {
            mir::ExprKind::Int { value } => {
                let instruction = match expr.ty {
                    mir::Type::I32 => wasm::Instruction::I32Const {
                        value: value.clone() as i32,
                    },
                    mir::Type::I64 => wasm::Instruction::I64Const {
                        value: value.clone(),
                    },
                    _ => unreachable!(),
                };
                body.push(instruction);
            }
            mir::ExprKind::Add { left, right } => {
                WASMBuilder::build_expression(body, &left);
                WASMBuilder::build_expression(body, &right);
                match expr.ty {
                    mir::Type::I32 => {
                        body.push(wasm::Instruction::I32Add);
                    }
                    mir::Type::I64 => {
                        body.push(wasm::Instruction::I64Add);
                    }
                    _ => unreachable!(),
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
            mir::ExprKind::Negate { operand } => {
                match expr.ty {
                    mir::Type::I32 => {
                        body.push(wasm::Instruction::I32Const { value: 0 });
                    }
                    mir::Type::I64 => {
                        body.push(wasm::Instruction::I64Const { value: 0 });
                    }
                    _ => unreachable!(),
                }
                WASMBuilder::build_expression(body, &operand);
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
        }
    }
}
