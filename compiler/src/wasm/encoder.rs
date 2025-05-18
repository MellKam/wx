use std::collections::HashMap;

use leb128fmt;

use crate::wasm;

pub trait Encode {
    fn encode(&self, sink: &mut Vec<u8>);
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

impl Encode for wasm::Instruction {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            wasm::Instruction::LocalGet { index } => {
                sink.push(0x20);
                index.0.encode(sink);
            }
            wasm::Instruction::LocalSet { index } => {
                sink.push(0x21);
                index.0.encode(sink);
            }
            wasm::Instruction::Return => {
                sink.push(0x0F);
            }
            wasm::Instruction::Block { ty } => {
                sink.push(0x02);
                if let Some(ty) = ty {
                    ty.encode(sink);
                } else {
                    sink.push(0x40); // Empty block type
                }
            }
            wasm::Instruction::Br { block_index } => {
                sink.push(0x0C);
                block_index.encode(sink);
            }
            wasm::Instruction::End => {
                sink.push(0x0B);
            }
            wasm::Instruction::Call { index } => {
                sink.push(0x10);
                index.encode(sink);
            }
            wasm::Instruction::Drop => {
                sink.push(0x1A);
            }
            wasm::Instruction::I32Const { value } => {
                sink.push(0x41);
                value.encode(sink);
            }
            wasm::Instruction::I64Const { value } => {
                sink.push(0x42);
                value.encode(sink);
            }
            wasm::Instruction::I32Add => {
                sink.push(0x6A);
            }
            wasm::Instruction::I32Sub => {
                sink.push(0x6B);
            }
            wasm::Instruction::I32Mul => {
                sink.push(0x6C);
            }
            wasm::Instruction::I32DivS => {
                sink.push(0x6D);
            }
            wasm::Instruction::I32DivU => {
                sink.push(0x6E);
            }
            wasm::Instruction::I32RemS => {
                sink.push(0x6F);
            }
            wasm::Instruction::I32RemU => {
                sink.push(0x70);
            }
            wasm::Instruction::I32And => {
                sink.push(0x71);
            }
            wasm::Instruction::I32Eq => {
                sink.push(0x46);
            }
            wasm::Instruction::I32Eqz => {
                sink.push(0x45);
            }
            wasm::Instruction::I32Or => {
                sink.push(0x72);
            }
            wasm::Instruction::I64Add => {
                sink.push(0x7C);
            }
            wasm::Instruction::I64Sub => {
                sink.push(0x7D);
            }
            wasm::Instruction::I64Mul => {
                sink.push(0x7E);
            }
            wasm::Instruction::I64DivS => {
                sink.push(0x7F);
            }
            wasm::Instruction::I64DivU => {
                sink.push(0x80);
            }
            wasm::Instruction::I64RemS => {
                sink.push(0x81);
            }
            wasm::Instruction::I64RemU => {
                sink.push(0x82);
            }
            wasm::Instruction::I64And => {
                sink.push(0x83);
            }
            wasm::Instruction::I64Or => {
                sink.push(0x84);
            }
            wasm::Instruction::I64Eq => {
                sink.push(0x86);
            }
            wasm::Instruction::I64Eqz => {
                sink.push(0x87);
            }
        }
    }
}

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

struct TypeSection {
    signatures: Vec<wasm::FunctionType>,
}

impl Encode for wasm::FunctionType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(0x60); // Function type

        let param_count = self.params().len() as u32;
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

impl Encode for TypeSection {
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

struct FunctionSection {
    function_indexes: Vec<u32>,
}

impl Encode for FunctionSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Function as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let size = self.function_indexes.len() as u32;
        size.encode(&mut section_sink);
        for index in &self.function_indexes {
            index.encode(&mut section_sink);
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

struct ExportFunction<'a> {
    name: &'a str,
    index: u32,
}

struct ExportSection<'a> {
    functions: Vec<ExportFunction<'a>>,
}

#[repr(u8)]
enum ExportKind {
    Function = 0,
    Table = 1,
    Memory = 2,
    Global = 3,
}

impl Encode for ExportFunction<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        let name_len = self.name.len() as u32;
        name_len.encode(sink);
        sink.extend_from_slice(self.name.as_bytes());
        sink.push(ExportKind::Function as u8);
        self.index.encode(sink);
    }
}

impl Encode for ExportSection<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Export as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let export_count = self.functions.len() as u32;
        export_count.encode(&mut section_sink);
        for function in &self.functions {
            function.encode(&mut section_sink);
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

struct FunctionBody<'a> {
    locals: &'a [wasm::Local<'a>],
    instructions: &'a [wasm::Instruction],
}

impl<'a> Encode for FunctionBody<'a> {
    fn encode(&self, sink: &mut Vec<u8>) {
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

        for instruction in self.instructions.iter() {
            instruction.encode(&mut body_content);
        }
        wasm::Instruction::End.encode(&mut body_content);

        let body_size = body_content.len() as u32;
        body_size.encode(sink);
        sink.extend_from_slice(&body_content);
    }
}

struct CodeSection<'a> {
    functions: Vec<FunctionBody<'a>>,
}

impl<'a> Encode for CodeSection<'a> {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Code as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let function_count = self.functions.len() as u32;
        function_count.encode(&mut section_sink);
        for function in &self.functions {
            function.encode(&mut section_sink);
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
        TypeSection {
            signatures: module
                .functions
                .iter()
                .map(|function| function.ty.clone())
                .collect(),
        }
        .encode(&mut sink);

        FunctionSection {
            function_indexes: module
                .functions
                .iter()
                .enumerate()
                .map(|(index, _)| index as u32)
                .collect(),
        }
        .encode(&mut sink);

        ExportSection {
            functions: module
                .functions
                .iter()
                .enumerate()
                .filter(|(_, function)| function.export)
                .map(|(index, function)| ExportFunction {
                    name: function.name,
                    index: index as u32,
                })
                .collect(),
        }
        .encode(&mut sink);

        CodeSection {
            functions: module
                .functions
                .iter()
                .map(|function| FunctionBody {
                    locals: function.locals.as_ref(),
                    instructions: function.instructions.as_ref(),
                })
                .collect(),
        }
        .encode(&mut sink);

        sink
    }
}
