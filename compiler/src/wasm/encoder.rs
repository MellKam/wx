use std::collections::HashMap;

use leb128fmt;

use crate::wasm;

#[repr(u8)]
enum SectionId {
    Custom = 0,
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
}

trait Encode {
    fn encode(&self, sink: &mut Vec<u8>);
}

trait EncodeWithContext {
    fn encode_with_context(&self, sink: &mut Vec<u8>, module: &wasm::Module);
}

impl Encode for i32 {
    fn encode(&self, sink: &mut Vec<u8>) {
        let (value, pos) = leb128fmt::encode_s32(*self).unwrap();
        sink.extend_from_slice(&value[..pos]);
    }
}

impl Encode for i64 {
    fn encode(&self, sink: &mut Vec<u8>) {
        let (value, pos) = leb128fmt::encode_s64(*self).unwrap();
        sink.extend_from_slice(&value[..pos]);
    }
}

impl Encode for u32 {
    fn encode(&self, sink: &mut Vec<u8>) {
        let (value, pos) = leb128fmt::encode_u32(*self).unwrap();
        sink.extend_from_slice(&value[..pos]);
    }
}

impl Encode for wasm::ValueType {
    fn encode(&self, sink: &mut Vec<u8>) {
        let opcode = match self {
            wasm::ValueType::I32 => 0x7F,
            wasm::ValueType::I64 => 0x7E,
        };

        sink.push(opcode);
    }
}

impl Encode for wasm::BlockResult {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            wasm::BlockResult::Empty => {
                sink.push(0x40);
            }
            wasm::BlockResult::SingleValue(ty) => {
                ty.encode(sink);
            }
        }
    }
}

#[repr(u8)]
enum Instruction {
    Nop = 0x01,
    Block = 0x02,
    If = 0x04,
    Else = 0x05,
    End = 0x0B,
    Br = 0x0C,
    Return = 0x0F,
    Call = 0x10,
    Drop = 0x1A,
    LocalGet = 0x20,
    LocalSet = 0x21,
    I32Const = 0x41,
    I64Const = 0x42,
    I32Eq = 0x46,
    I32Add = 0x6A,
    I32Sub = 0x6B,
    I32Mul = 0x6C,
    I64Add = 0x7C,
    I64Sub = 0x7D,
    I64Mul = 0x7E,
    I64Eq = 0x86,
}

impl EncodeWithContext for wasm::Expression {
    fn encode_with_context(&self, sink: &mut Vec<u8>, module: &wasm::Module) {
        match self {
            wasm::Expression::Nop => {
                sink.push(Instruction::Nop as u8);
            }
            wasm::Expression::LocalGet { local } => {
                sink.push(Instruction::LocalGet as u8);
                local.0.encode(sink);
            }
            wasm::Expression::LocalSet { local, value } => {
                module.get_expr(*value).encode_with_context(sink, module);
                sink.push(Instruction::LocalSet as u8);
                local.0.encode(sink);
            }
            wasm::Expression::Return { value } => {
                match value {
                    Some(value) => {
                        module.get_expr(*value).encode_with_context(sink, module);
                    }
                    None => {}
                };
                sink.push(Instruction::Return as u8);
            }
            wasm::Expression::I32Add { left, right } => {
                module.get_expr(*left).encode_with_context(sink, module);
                module.get_expr(*right).encode_with_context(sink, module);
                sink.push(Instruction::I32Add as u8);
            }
            wasm::Expression::I32Sub { left, right } => {
                module.get_expr(*left).encode_with_context(sink, module);
                module.get_expr(*right).encode_with_context(sink, module);
                sink.push(Instruction::I32Sub as u8);
            }
            wasm::Expression::I32Mul { left, right } => {
                module.get_expr(*left).encode_with_context(sink, module);
                module.get_expr(*right).encode_with_context(sink, module);
                sink.push(Instruction::I32Mul as u8);
            }
            wasm::Expression::Block {
                expressions,
                result,
            } => {
                sink.push(Instruction::Block as u8);
                result.encode(sink);
                for expr_index in expressions.iter() {
                    module
                        .get_expr(*expr_index)
                        .encode_with_context(sink, module);
                }
                sink.push(Instruction::End as u8);
            }
            wasm::Expression::Break { depth, value } => {
                match value {
                    Some(value) => {
                        module.get_expr(*value).encode_with_context(sink, module);
                    }
                    None => {}
                };
                sink.push(Instruction::Br as u8);
                depth.encode(sink);
            }
            wasm::Expression::Call {
                function,
                arguments,
            } => {
                for arg in arguments.iter() {
                    module.get_expr(*arg).encode_with_context(sink, module);
                }
                sink.push(Instruction::Call as u8);
                function.0.encode(sink);
            }
            wasm::Expression::I32Const { value } => {
                sink.push(Instruction::I32Const as u8);
                value.encode(sink);
            }
            wasm::Expression::I64Const { value } => {
                sink.push(Instruction::I64Const as u8);
                value.encode(sink);
            }
            wasm::Expression::I32Eq { left, right } => {
                module.get_expr(*left).encode_with_context(sink, module);
                module.get_expr(*right).encode_with_context(sink, module);
                sink.push(Instruction::I32Eq as u8);
            }
            wasm::Expression::IfElse {
                condition,
                result,
                then_branch,
                else_branch,
            } => {
                module
                    .get_expr(*condition)
                    .encode_with_context(sink, module);
                sink.push(Instruction::If as u8);
                result.encode(sink);
                module
                    .get_expr(*then_branch)
                    .encode_with_context(sink, module);
                match else_branch {
                    Some(else_branch) => {
                        sink.push(Instruction::Else as u8);
                        module
                            .get_expr(*else_branch)
                            .encode_with_context(sink, module);
                    }
                    None => {}
                }
                sink.push(Instruction::End as u8);
            }
            wasm::Expression::Drop { value } => {
                module.get_expr(*value).encode_with_context(sink, module);
                sink.push(Instruction::Drop as u8);
            }
            wasm::Expression::I64Add { left, right } => {
                module.get_expr(*left).encode_with_context(sink, module);
                module.get_expr(*right).encode_with_context(sink, module);
                sink.push(Instruction::I64Add as u8);
            }
            wasm::Expression::I64Sub { left, right } => {
                module.get_expr(*left).encode_with_context(sink, module);
                module.get_expr(*right).encode_with_context(sink, module);
                sink.push(Instruction::I64Sub as u8);
            }
            wasm::Expression::I64Mul { left, right } => {
                module.get_expr(*left).encode_with_context(sink, module);
                module.get_expr(*right).encode_with_context(sink, module);
                sink.push(Instruction::I64Mul as u8);
            }
            wasm::Expression::I64Eq { left, right } => {
                module.get_expr(*left).encode_with_context(sink, module);
                module.get_expr(*right).encode_with_context(sink, module);
                sink.push(Instruction::I64Eq as u8);
            }
        }
    }
}

impl Encode for wasm::FunctionType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(0x60); // Function type

        let param_count = self.param_count as u32;
        param_count.encode(sink);
        for param in self.params().iter() {
            param.encode(sink);
        }

        let result_count = self.results().len() as u32;
        result_count.encode(sink);
        for result in self.results().iter() {
            result.encode(sink);
        }
    }
}

impl Encode for wasm::TypeSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Type as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let size = self.signatures.len() as u32;
        size.encode(&mut section_sink);
        for signature in &self.signatures {
            signature.encode(&mut section_sink);
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl Encode for wasm::FunctionSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Function as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let size = self.functions.len() as u32;
        size.encode(&mut section_sink);
        for type_index in &self.functions {
            type_index.0.encode(&mut section_sink);
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

#[repr(u8)]
enum ExportKind {
    Function = 0x00,
    // Table = 0x01,
    // Memory = 0x02,
    // Global = 0x03,
}

impl Encode for wasm::FunctionExport<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        let name_len = self.name.len() as u32;
        name_len.encode(sink);
        sink.extend_from_slice(self.name.as_bytes());
        sink.push(ExportKind::Function as u8);
        self.index.0.encode(sink);
    }
}

impl Encode for wasm::ExportSection<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Export as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let export_count = self.items.len() as u32;
        export_count.encode(&mut section_sink);
        for item in &self.items {
            match item {
                wasm::ExportItem::Function(func_export) => {
                    func_export.encode(&mut section_sink);
                }
            }
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl<'a> wasm::FunctionBody<'a> {
    fn encode(&self, sink: &mut Vec<u8>, module: &wasm::Module) {
        let mut body_content: Vec<u8> = Vec::new();

        let mut grouped_locals: HashMap<wasm::ValueType, u32> = HashMap::new();
        for local in self.locals.iter() {
            *grouped_locals.entry(local.ty).or_insert(0) += 1;
        }

        let local_groups_count = grouped_locals.len() as u32;
        local_groups_count.encode(&mut body_content);
        for (local_type, count) in grouped_locals {
            count.encode(&mut body_content);
            local_type.encode(&mut body_content);
        }

        for expr_id in self.expressions.iter() {
            module
                .get_expr(*expr_id)
                .encode_with_context(&mut body_content, module);
        }
        body_content.push(Instruction::End as u8);

        let body_size = body_content.len() as u32;
        body_size.encode(sink);
        sink.extend_from_slice(&body_content);
    }
}

impl<'a> wasm::CodeSection<'a> {
    fn encode(&self, sink: &mut Vec<u8>, module: &wasm::Module) {
        sink.push(SectionId::Code as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let function_count = self.functions.len() as u32;
        function_count.encode(&mut section_sink);
        for function in &self.functions {
            function.encode(&mut section_sink, module);
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

pub struct Encoder {}

impl Encoder {
    pub fn encode(module: &wasm::Module) -> Vec<u8> {
        let mut sink = [
            0x00, 0x61, 0x73, 0x6D, // Magic
            0x01, 0x00, 0x00, 0x00, // Version
        ]
        .to_vec();

        module.types.encode(&mut sink);
        module.functions.encode(&mut sink);
        module.exports.encode(&mut sink);
        module.code.encode(&mut sink, module);

        sink
    }
}
