pub mod builder;
pub mod encoder;
pub mod wat;

#[derive(Debug, Clone)]
pub enum Instruction {
    LocalGet { index: u32 },
    LocalSet { index: u32 },
    Return,

    I32Const { value: i32 },
    I64Const { value: i64 },

    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,

    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum ValueType {
    I32,
    I64,
}

#[derive(Debug, Clone)]
pub struct Function<'a> {
    name: &'a str,
    param_count: u32,
    signature: Vec<ValueType>,
    locals: Vec<ValueType>,
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Module<'a> {
    functions: Vec<Function<'a>>,
}
