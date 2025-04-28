use crate::wasm;

pub trait Encode {
    fn encode(&self, sink: &mut String);
}

impl Encode for i32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for i64 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for u32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for wasm::ValueType {
    fn encode(&self, sink: &mut String) {
        match self {
            wasm::ValueType::I32 => sink.push_str("i32"),
            wasm::ValueType::I64 => sink.push_str("i64"),
        }
    }
}

impl Encode for wasm::Instruction {
    fn encode(&self, sink: &mut String) {
        match self {
            wasm::Instruction::LocalGet { index } => {
                sink.push_str("local.get ");
                index.encode(sink);
            }
            wasm::Instruction::LocalSet { index } => {
                sink.push_str("local.set ");
                index.encode(sink);
            }
            wasm::Instruction::Return => {
                sink.push_str("return");
            }
            wasm::Instruction::I32Const { value } => {
                sink.push_str("i32.const ");
                value.encode(sink);
            }
            wasm::Instruction::I64Const { value } => {
                sink.push_str("i64.const ");
                value.encode(sink);
            }
            wasm::Instruction::I32Add => {
                sink.push_str("i32.add");
            }
            wasm::Instruction::I32Sub => {
                sink.push_str("i32.sub");
            }
            wasm::Instruction::I32Mul => {
                sink.push_str("i32.mul");
            }
            wasm::Instruction::I32DivS => {
                sink.push_str("i32.div_s");
            }
            wasm::Instruction::I32DivU => {
                sink.push_str("i32.div_u");
            }
            wasm::Instruction::I32RemS => {
                sink.push_str("i32.rem_s");
            }
            wasm::Instruction::I32RemU => {
                sink.push_str("i32.rem_u");
            }
            wasm::Instruction::I32And => {
                sink.push_str("i32.and");
            }
            wasm::Instruction::I32Or => {
                sink.push_str("i32.or");
            }
            wasm::Instruction::I64Add => {
                sink.push_str("i64.add");
            }
            wasm::Instruction::I64Sub => {
                sink.push_str("i64.sub");
            }
            wasm::Instruction::I64Mul => {
                sink.push_str("i64.mul");
            }
            wasm::Instruction::I64DivS => {
                sink.push_str("i64.div_s");
            }
            wasm::Instruction::I64DivU => {
                sink.push_str("i64.div_u");
            }
            wasm::Instruction::I64RemS => {
                sink.push_str("i64.rem_s");
            }
            wasm::Instruction::I64RemU => {
                sink.push_str("i64.rem_u");
            }
            wasm::Instruction::I64And => {
                sink.push_str("i64.and");
            }
            wasm::Instruction::I64Or => {
                sink.push_str("i64.or");
            }
        }
    }
}

impl Encode for wasm::Function<'_> {
    fn encode(&self, sink: &mut String) {
        sink.push_str("(func");
        sink.push_str(format!("(export\"{}\")", self.name).as_str());

        match self.param_count {
            0 => {}
            _ => {
                sink.push_str("(param ");
                for (index, param) in self
                    .signature
                    .get(0..self.param_count as usize)
                    .unwrap()
                    .iter()
                    .enumerate()
                {
                    param.encode(sink);
                    match index {
                        index if index as u32 + 1 == self.param_count => {}
                        _ => sink.push_str(" "),
                    }
                }
                sink.push_str(")");
            }
        }

        match self.signature.len() as u32 - self.param_count {
            0 => {}
            _ => {
                sink.push_str("(result ");
                for (index, result) in self
                    .signature
                    .get(self.param_count as usize..)
                    .unwrap()
                    .iter()
                    .enumerate()
                {
                    result.encode(sink);
                    match index {
                        index
                            if index as u32 + 1
                                == self.signature.len() as u32 - self.param_count => {}
                        _ => sink.push_str(" "),
                    }
                }
                sink.push_str(")");
            }
        }

        match self.locals.len() {
            0 => {}
            _ => {
                sink.push_str("(local ");
                for local in &self.locals {
                    local.encode(sink);
                    sink.push_str(" ");
                }
                sink.push_str(")");
            }
        }

        for instruction in &self.instructions {
            sink.push_str("(");
            instruction.encode(sink);
            sink.push_str(")");
        }
        sink.push_str(")");
    }
}

impl wasm::Module<'_> {
    pub fn encode_wat(&self) -> String {
        let mut sink = String::new();
        sink.push_str("(module");
        for function in &self.functions {
            function.encode(&mut sink);
        }
        sink.push_str(")");
        sink
    }
}
