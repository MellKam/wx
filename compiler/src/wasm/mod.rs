pub mod builder;
pub mod encoder;
pub mod wat;

pub use builder::*;
pub use encoder::*;

#[derive(Debug, Clone)]
pub enum Instruction {
    LocalGet { index: LocalIndex },
    LocalSet { index: LocalIndex },
    Return,
    Block { ty: Option<ValueType> },
    Br { block_index: u32 },
    End,
    Drop,
    Call { index: u32 },

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
    I32Eq,
    I32Eqz,

    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Eq,
    I64Eqz,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum ValueType {
    I32,
    I64,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub param_count: usize,
    pub param_results: Box<[ValueType]>,
}

impl FunctionType {
    pub fn params(&self) -> &[ValueType] {
        self.param_results.get(..self.param_count).unwrap_or(&[])
    }

    pub fn results(&self) -> &[ValueType] {
        self.param_results.get(self.param_count..).unwrap_or(&[])
    }
}

#[derive(Debug, Clone)]
pub struct Local<'a> {
    name: &'a str,
    ty: ValueType,
}

#[derive(Debug, Clone, Copy)]
pub struct LocalIndex(pub u32);

#[derive(Debug, Clone)]
pub struct Function<'a> {
    export: bool,
    name: &'a str,
    ty: FunctionType,
    locals: Box<[Local<'a>]>,
    instructions: Vec<Instruction>,
}

impl Function<'_> {
    pub fn params(&self) -> &[Local] {
        self.locals
            .get(0..self.ty.param_count as usize)
            .unwrap_or(&[])
    }

    pub fn locals_without_params(&self) -> &[Local] {
        self.locals
            .get(self.ty.param_count as usize..)
            .unwrap_or(&[])
    }
}

#[derive(Debug)]
pub struct Module<'a> {
    functions: Vec<Function<'a>>,
}
