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

#[repr(u8)]
enum Instruction {
    Unreachable = 0x00,
    Nop = 0x01,
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    // Try = 0x06,
    // Catch = 0x07,
    // Throw = 0x08,
    // Rethrow = 0x09,
    // ThrowRef = 0x0A,
    End = 0x0B,
    Br = 0x0C,
    BrIf = 0x0D,
    BrTable = 0x0E,
    Return = 0x0F,
    Call = 0x10,
    CallIndirect = 0x11,
    // ReturnCall = 0x12,
    // ReturnCallIndirect = 0x13,
    // CallRef = 0x14,
    // ReturnCallRef = 0x15,
    // 0x16 (reserved)
    // 0x17 (reserved)
    // Delegate = 0x18,
    // CatchAll = 0x19,
    Drop = 0x1A,
    Select = 0x1B,
    // SelectT = 0x1C,
    // 0x1D (reserved)
    // 0x1E (reserved)
    // TryTable = 0x1F,
    LocalGet = 0x20,
    LocalSet = 0x21,
    LocalTee = 0x22,
    GlobalGet = 0x23,
    GlobalSet = 0x24,
    // TableGet = 0x25,
    // TableSet = 0x26,
    // 0x27 (reserved)
    // - Load instructions
    I32Load = 0x28,
    I64Load = 0x29,
    F32Load = 0x2A,
    F64Load = 0x2B,
    I32Load8S = 0x2C,
    I32Load8U = 0x2D,
    I32Load16S = 0x2E,
    I32Load16U = 0x2F,
    I64Load8S = 0x30,
    I64Load8U = 0x31,
    I64Load16S = 0x32,
    I64Load16U = 0x33,
    I64Load32S = 0x34,
    I64Load32U = 0x35,
    // - Store instructions
    I32Store = 0x36,
    I64Store = 0x37,
    F32Store = 0x38,
    F64Store = 0x39,
    I32Store8 = 0x3A,
    I32Store16 = 0x3B,
    I64Store8 = 0x3C,
    I64Store16 = 0x3D,
    I64Store32 = 0x3E,
    // - Memory instructions
    MemorySize = 0x3F,
    MemoryGrow = 0x40,
    // - Constant instructions
    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,
    // I32 logical and comparison
    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4A,
    I32GtU = 0x4B,
    I32LeS = 0x4C,
    I32LeU = 0x4D,
    I32GeS = 0x4E,
    I32GeU = 0x4F,
    // I64 logical and comparison
    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64LeU = 0x58,
    I64GeS = 0x59,
    I64GeU = 0x5A,
    // F32 comparison
    F32Eq = 0x5B,
    F32Ne = 0x5C,
    F32Lt = 0x5D,
    F32Gt = 0x5E,
    F32Le = 0x5F,
    F32Ge = 0x60,
    // F64 comparison
    F64Eq = 0x61,
    F64Ne = 0x62,
    F64Lt = 0x63,
    F64Gt = 0x64,
    F64Le = 0x65,
    F64Ge = 0x66,
    // - I32 arithmetic and bitwise operations
    I32Clz = 0x67,
    I32Ctz = 0x68,
    I32Popcnt = 0x69,
    I32Add = 0x6A,
    I32Sub = 0x6B,
    I32Mul = 0x6C,
    I32DivS = 0x6D,
    I32DivU = 0x6E,
    I32RemS = 0x6F,
    I32RemU = 0x70,
    I32And = 0x71,
    I32Or = 0x72,
    I32Xor = 0x73,
    I32Shl = 0x74,
    I32ShrS = 0x75,
    I32ShrU = 0x76,
    I32Rotl = 0x77,
    I32Rotr = 0x78,
    // - I64 arithmetic and bitwise operations
    I64Clz = 0x79,
    I64Ctz = 0x7A,
    I64Popcnt = 0x7B,
    I64Add = 0x7C,
    I64Sub = 0x7D,
    I64Mul = 0x7E,
    I64DivS = 0x7F,
    I64DivU = 0x80,
    I64RemS = 0x81,
    I64RemU = 0x82,
    I64And = 0x83,
    I64Or = 0x84,
    I64Xor = 0x85,
    I64Shl = 0x86,
    I64ShrS = 0x87,
    I64ShrU = 0x88,
    I64Rotl = 0x89,
    I64Rotr = 0x8A,
    // - F32 arithmetic operations
    F32Abs = 0x8B,
    F32Neg = 0x8C,
    F32Ceil = 0x8D,
    F32Floor = 0x8E,
    F32Trunc = 0x8F,
    F32Nearest = 0x90,
    F32Sqrt = 0x91,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    F32Min = 0x96,
    F32Max = 0x97,
    F32Copysign = 0x98,
    // - F64 arithmetic operations
    F64Abs = 0x99,
    F64Neg = 0x9A,
    F64Ceil = 0x9B,
    F64Floor = 0x9C,
    F64Trunc = 0x9D,
    F64Nearest = 0x9E,
    F64Sqrt = 0x9F,
    F64Add = 0xA0,
    F64Sub = 0xA1,
    F64Mul = 0xA2,
    F64Div = 0xA3,
    F64Min = 0xA4,
    F64Max = 0xA5,
    F64Copysign = 0xA6,
    // - Conversion instructions
    I32WrapI64 = 0xA7,
    I32TruncF32S = 0xA8,
    I32TruncF32U = 0xA9,
    I32TruncF64S = 0xAA,
    I32TruncF64U = 0xAB,
    I64ExtendI32S = 0xAC,
    I64ExtendI32U = 0xAD,
    I64TruncF32S = 0xAE,
    I64TruncF32U = 0xAF,
    I64TruncF64S = 0xB0,
    I64TruncF64U = 0xB1,
    F32ConvertI32S = 0xB2,
    F32ConvertI32U = 0xB3,
    F32ConvertI64S = 0xB4,
    F32ConvertI64U = 0xB5,
    F32DemoteF64 = 0xB6,
    F64ConvertI32S = 0xB7,
    F64ConvertI32U = 0xB8,
    F64ConvertI64S = 0xB9,
    F64ConvertI64U = 0xBA,
    F64PromoteF32 = 0xBB,
    I32ReinterpretF32 = 0xBC,
    I64ReinterpretF64 = 0xBD,
    F32ReinterpretI32 = 0xBE,
    F64ReinterpretI64 = 0xBF,
}

trait Encode {
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

impl Encode for f32 {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend_from_slice(&self.to_le_bytes());
    }
}

impl Encode for f64 {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend_from_slice(&self.to_le_bytes());
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
            wasm::ValueType::F32 => 0x7D,
            wasm::ValueType::F64 => 0x7C,
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

#[derive(Clone)]
struct EncodeContext<'a> {
    module: &'a wasm::Module<'a>,
    func_index: wasm::FuncIndex,
}

impl EncodeContext<'_> {
    fn encode_expr(&self, sink: &mut Vec<u8>, expr_index: wasm::ExprIndex) {
        self.module
            .get_expr(expr_index)
            .encode_with_context(sink, self.clone());
    }
}

trait EncodeWithContext {
    fn encode_with_context(&self, sink: &mut Vec<u8>, ctx: EncodeContext);
}

impl EncodeWithContext for wasm::Expression {
    fn encode_with_context(&self, sink: &mut Vec<u8>, ctx: EncodeContext) {
        use wasm::Expression;
        match self {
            Expression::Nop => {
                // sink.push(Instruction::Nop as u8);
            }
            Expression::LocalGet { local } => {
                sink.push(Instruction::LocalGet as u8);
                local.0.encode(sink);
            }
            Expression::LocalSet { local, value } => {
                ctx.encode_expr(sink, *value);
                sink.push(Instruction::LocalSet as u8);
                local.0.encode(sink);
            }
            Expression::GlobalGet { global } => {
                sink.push(Instruction::GlobalGet as u8);
                global.0.encode(sink);
            }
            Expression::GlobalSet { global, value } => {
                ctx.encode_expr(sink, *value);
                sink.push(Instruction::GlobalSet as u8);
                global.0.encode(sink);
            }
            Expression::Return { value } => {
                match value {
                    Some(value) => {
                        ctx.encode_expr(sink, *value);
                    }
                    None => {}
                };
                sink.push(Instruction::Return as u8);
            }
            Expression::I32Add { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32Add as u8);
            }
            Expression::I32Sub { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32Sub as u8);
            }
            Expression::I32Mul { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32Mul as u8);
            }
            Expression::Block {
                expressions,
                result,
            } => {
                sink.push(Instruction::Block as u8);
                result.encode(sink);
                for expr_index in expressions.iter() {
                    ctx.encode_expr(sink, *expr_index);
                }
                sink.push(Instruction::End as u8);
            }
            Expression::Loop {
                expressions,
                result,
            } => {
                sink.push(Instruction::Loop as u8);
                result.encode(sink);
                for expr_index in expressions.iter() {
                    ctx.encode_expr(sink, *expr_index);
                }
                sink.push(Instruction::End as u8);
            }
            Expression::Unreachable => {
                sink.push(Instruction::Unreachable as u8);
            }
            Expression::Break { depth, value } => {
                match value {
                    Some(value) => {
                        ctx.encode_expr(sink, *value);
                    }
                    None => {}
                };
                sink.push(Instruction::Br as u8);
                depth.encode(sink);
            }
            Expression::Call {
                function,
                arguments,
            } => {
                for arg in arguments.iter().copied() {
                    ctx.encode_expr(sink, arg);
                }
                sink.push(Instruction::Call as u8);
                function.0.encode(sink);
            }
            Expression::CallIndirect {
                expr,
                type_index,
                arguments,
                table_index,
            } => {
                for arg in arguments.iter().copied() {
                    ctx.encode_expr(sink, arg);
                }
                ctx.encode_expr(sink, *expr);

                sink.push(Instruction::CallIndirect as u8);
                type_index.0.encode(sink);
                table_index.0.encode(sink);
            }
            Expression::I32Const { value } => {
                sink.push(Instruction::I32Const as u8);
                value.encode(sink);
            }
            Expression::F32Const { value } => {
                sink.push(Instruction::F32Const as u8);
                value.encode(sink);
            }
            Expression::I64Const { value } => {
                sink.push(Instruction::I64Const as u8);
                value.encode(sink);
            }
            Expression::F64Const { value } => {
                sink.push(Instruction::F64Const as u8);
                value.encode(sink);
            }
            Expression::I32Eq { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32Eq as u8);
            }
            Expression::I32Eqz { value } => {
                ctx.encode_expr(sink, *value);
                sink.push(Instruction::I32Eqz as u8);
            }
            Expression::I32Ne { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32Ne as u8);
            }
            Expression::I32And { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32And as u8);
            }
            Expression::I32Or { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32Or as u8);
            }
            Expression::IfElse {
                condition,
                result,
                then_branch,
                else_branch,
            } => {
                ctx.encode_expr(sink, *condition);
                sink.push(Instruction::If as u8);
                result.encode(sink);
                ctx.encode_expr(sink, *then_branch);
                match else_branch {
                    Some(else_branch) => {
                        sink.push(Instruction::Else as u8);
                        ctx.encode_expr(sink, *else_branch);
                    }
                    None => {}
                }
                sink.push(Instruction::End as u8);
            }
            Expression::Drop { value } => {
                ctx.encode_expr(sink, *value);
                sink.push(Instruction::Drop as u8);
            }
            Expression::I64Add { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64Add as u8);
            }
            Expression::I64Sub { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64Sub as u8);
            }
            Expression::I64Mul { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64Mul as u8);
            }
            Expression::I64Eq { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64Eq as u8);
            }
            Expression::I64Eqz { value } => {
                ctx.encode_expr(sink, *value);
                sink.push(Instruction::I64Eqz as u8);
            }
            Expression::I64Ne { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64Ne as u8);
            }
            Expression::I64And { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64And as u8);
            }
            Expression::I64Or { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64Or as u8);
            }
            Expression::I32DivS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32DivS as u8);
            }
            Expression::I32GeS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32GeS as u8);
            }
            Expression::I32ShrS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32ShrS as u8);
            }
            Expression::I32LtS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32LtS as u8);
            }
            Expression::I32GtS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32GtS as u8);
            }
            Expression::I32LeS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32LeS as u8);
            }
            Expression::I32RemS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32RemS as u8);
            }
            Expression::I64DivS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64DivS as u8);
            }
            Expression::I64GeS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64GeS as u8);
            }
            Expression::I64ShrS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64ShrS as u8);
            }
            Expression::I64LtS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64LtS as u8);
            }
            Expression::I64GtS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64GtS as u8);
            }
            Expression::I64LeS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64LeS as u8);
            }
            Expression::I64RemS { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64RemS as u8);
            }
            Expression::I32Xor { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32Xor as u8);
            }
            Expression::I64Xor { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64Xor as u8);
            }
            Expression::I32Shl { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I32Shl as u8);
            }
            Expression::I64Shl { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::I64Shl as u8);
            }
            Expression::F32Add { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Add as u8);
            }
            Expression::F32Sub { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Sub as u8);
            }
            Expression::F32Mul { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Mul as u8);
            }
            Expression::F64Add { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Add as u8);
            }
            Expression::F64Sub { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Sub as u8);
            }
            Expression::F64Mul { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Mul as u8);
            }
            Expression::F32Eq { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Eq as u8);
            }
            Expression::F32Ne { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Ne as u8);
            }
            Expression::F64Eq { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Eq as u8);
            }
            Expression::F64Ne { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Ne as u8);
            }
            Expression::F32Lt { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Lt as u8);
            }
            Expression::F32Gt { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Gt as u8);
            }
            Expression::F32Le { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Le as u8);
            }
            Expression::F32Ge { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Ge as u8);
            }
            Expression::F64Lt { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Lt as u8);
            }
            Expression::F64Gt { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Gt as u8);
            }
            Expression::F64Le { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Le as u8);
            }
            Expression::F64Ge { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Ge as u8);
            }
            Expression::F32Div { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F32Div as u8);
            }
            Expression::F64Div { left, right } => {
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push(Instruction::F64Div as u8);
            }
            Expression::F32Neg { value } => {
                ctx.encode_expr(sink, *value);
                sink.push(Instruction::F32Neg as u8);
            }
            Expression::F64Neg { value } => {
                ctx.encode_expr(sink, *value);
                sink.push(Instruction::F64Neg as u8);
            }
        }
    }
}

impl Encode for wasm::FunctionType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(0x60); // Function type

        let param_count = self.param_count as u32;
        param_count.encode(sink);
        for param in self.params() {
            param.encode(sink);
        }

        let result_count = (self.param_results.len() - self.param_count) as u32;
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
        (self.signatures.len() as u32).encode(&mut section_sink);
        for signature in &self.signatures {
            signature.encode(&mut section_sink);
        }

        (section_sink.len() as u32).encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl Encode for wasm::FunctionSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Function as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let size = self.types.len() as u32;
        size.encode(&mut section_sink);
        for type_index in &self.types {
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
    Global = 0x03,
}

impl Encode for wasm::ExportItem<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            wasm::ExportItem::Function { name, func_index } => {
                let name_len = name.len() as u32;
                name_len.encode(sink);
                sink.extend_from_slice(name.as_bytes());
                sink.push(ExportKind::Function as u8);
                func_index.0.encode(sink);
            }
            wasm::ExportItem::Global { name, global_index } => {
                let name_len = name.len() as u32;
                name_len.encode(sink);
                sink.extend_from_slice(name.as_bytes());
                sink.push(ExportKind::Global as u8);
                global_index.0.encode(sink);
            }
        }
    }
}

impl Encode for wasm::ExportSection<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Export as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let export_count = self.items.len() as u32;
        export_count.encode(&mut section_sink);
        for item in &self.items {
            item.encode(&mut section_sink);
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl Encode for wasm::Global<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.ty.encode(sink);
        sink.push(if self.mutability { 0x01 } else { 0x00 });

        use wasm::Expression;
        match self.value {
            Expression::I32Const { value } => {
                sink.push(Instruction::I32Const as u8);
                value.encode(sink);
            }
            Expression::F32Const { value } => {
                sink.push(Instruction::F32Const as u8);
                value.encode(sink);
            }
            Expression::I64Const { value } => {
                sink.push(Instruction::I64Const as u8);
                value.encode(sink);
            }
            Expression::F64Const { value } => {
                sink.push(Instruction::F64Const as u8);
                value.encode(sink);
            }
            _ => unreachable!(),
        }
        sink.push(Instruction::End as u8);
    }
}

impl Encode for wasm::GlobalSection<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Global as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let global_count = self.globals.len() as u32;
        global_count.encode(&mut section_sink);
        for global in &self.globals {
            global.encode(&mut section_sink);
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl<'a> EncodeWithContext for wasm::FunctionBody<'a> {
    fn encode_with_context(&self, sink: &mut Vec<u8>, ctx: EncodeContext) {
        let mut body_content: Vec<u8> = Vec::new();

        let func_type = ctx.module.get_type(
            *ctx.module
                .functions
                .types
                .get(ctx.func_index.0 as usize)
                .unwrap(),
        );

        let mut grouped_locals = Vec::<(wasm::ValueType, u32)>::new();
        for local in self.locals.iter().skip(func_type.param_count) {
            match grouped_locals.last_mut() {
                Some((last_ty, count)) if *last_ty == local.ty => {
                    *count += 1;
                }
                _ => {
                    grouped_locals.push((local.ty, 1));
                }
            }
        }

        (grouped_locals.len() as u32).encode(&mut body_content);
        for (group_type, count) in grouped_locals {
            count.encode(&mut body_content);
            group_type.encode(&mut body_content);
        }

        for expr_index in self.expressions.iter() {
            ctx.encode_expr(&mut body_content, *expr_index);
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
        for (index, func) in self.functions.iter().enumerate() {
            func.encode_with_context(
                &mut section_sink,
                EncodeContext {
                    module,
                    func_index: wasm::FuncIndex(index as u32),
                },
            );
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl Encode for wasm::RefType {
    fn encode(&self, sink: &mut Vec<u8>) {
        let ref_type = match self {
            wasm::RefType::FuncRef => 0x70,
            wasm::RefType::ExternRef => 0x6F,
        };
        sink.push(ref_type);
    }
}

impl Encode for wasm::ResizableLimits {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            wasm::ResizableLimits::Initial(initial) => {
                sink.push(0x00);
                initial.encode(sink);
            }
            wasm::ResizableLimits::InitialAndMax { initial, maximum } => {
                sink.push(0x01);
                initial.encode(sink);
                maximum.encode(sink);
            }
        }
    }
}

impl Encode for wasm::TableType {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.ty.encode(sink);
        self.limits.encode(sink);
    }
}

impl Encode for wasm::TableSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Table as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let table_count = self.tables.len() as u32;
        table_count.encode(&mut section_sink);
        for table in &self.tables {
            table.encode(&mut section_sink);
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl Encode for wasm::ElementSegment {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.table_index.0.encode(sink);
        sink.push(Instruction::I32Const as u8);
        self.offset.encode(sink);
        sink.push(Instruction::End as u8);

        let indicies_count = self.indices.len() as u32;
        indicies_count.encode(sink);
        for index in self.indices.iter().copied() {
            index.0.encode(sink);
        }
    }
}

impl Encode for wasm::ElementSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Element as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let segment_count = self.segments.len() as u32;
        segment_count.encode(&mut section_sink);
        for segment in &self.segments {
            segment.encode(&mut section_sink);
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
        match module.tables.tables.len() {
            0 => {}
            _ => module.tables.encode(&mut sink),
        }
        module.globals.encode(&mut sink);
        module.exports.encode(&mut sink);
        match module.elements.segments.len() {
            0 => {}
            _ => module.elements.encode(&mut sink),
        }
        module.code.encode(&mut sink, module);

        sink
    }
}
