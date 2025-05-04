use crate::wasm;

pub trait Encode {
    fn encode(&self, sink: &mut String);
}

pub trait EncodeWithContext {
    fn encode_with_context(&self, sink: &mut String, module: &wasm::Module, func_index: usize);
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

impl EncodeWithContext for wasm::Instruction {
    fn encode_with_context(&self, sink: &mut String, module: &wasm::Module, func_index: usize) {
        match self {
            wasm::Instruction::LocalGet { index } => {
                sink.push_str("local.get $");
                let local = module
                    .functions
                    .get(func_index)
                    .unwrap()
                    .locals
                    .get(*index as usize)
                    .unwrap();
                sink.push_str(local.name);
            }
            wasm::Instruction::LocalSet { index } => {
                sink.push_str("local.set $");
                let local = module
                    .functions
                    .get(func_index)
                    .unwrap()
                    .locals
                    .get(*index as usize)
                    .unwrap();
                sink.push_str(local.name);
            }
            wasm::Instruction::Return => {
                sink.push_str("return");
            }
            wasm::Instruction::Drop => {
                sink.push_str("drop");
            }
            wasm::Instruction::Call { index } => {
                sink.push_str("call $");
                let func = module.functions.get(*index as usize).unwrap();
                sink.push_str(func.name);
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
            wasm::Instruction::I32Eq => {
                sink.push_str("i32.eq");
            }
            wasm::Instruction::I32Eqz => {
                sink.push_str("i32.eqz");
            }
            wasm::Instruction::I64Eq => {
                sink.push_str("i64.eq");
            }
            wasm::Instruction::I64Eqz => {
                sink.push_str("i64.eqz");
            }
        }
    }
}

impl EncodeWithContext for wasm::Function<'_> {
    fn encode_with_context(&self, sink: &mut String, module: &wasm::Module, func_index: usize) {
        sink.push_str(format!("(func ${}", self.name).as_str());
        match self.export {
            true => {
                sink.push_str(format!("(export\"{}\")", self.name).as_str());
            }
            false => {}
        }

        for param in self.params().iter() {
            sink.push_str("(param $");
            sink.push_str(param.name);
            sink.push_str(" ");
            param.ty.encode(sink);
            sink.push_str(")");
        }

        for result in self.ty.results() {
            sink.push_str("(result ");
            result.encode(sink);
            sink.push_str(")");
        }

        for local in self.locals_without_params() {
            sink.push_str("(local $");
            sink.push_str(local.name);
            sink.push_str(" ");
            local.ty.encode(sink);
            sink.push_str(")");
        }

        for instruction in &self.instructions {
            sink.push_str("(");
            instruction.encode_with_context(sink, module, func_index);
            sink.push_str(")");
        }
        sink.push_str(")");
    }
}

impl wasm::Module<'_> {
    pub fn encode_wat(&self) -> String {
        let mut sink = String::new();
        sink.push_str("(module");
        for (index, func) in self.functions.iter().enumerate() {
            func.encode_with_context(&mut sink, self, index);
        }
        sink.push_str(")");
        sink
    }
}
