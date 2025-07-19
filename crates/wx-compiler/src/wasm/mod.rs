pub mod builder;
pub mod encoder;
pub mod wat;

pub use builder::*;
pub use encoder::*;
use serde::Serialize;
use string_interner::symbol::SymbolU32;

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Serialize)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, Serialize)]
pub enum BlockResult {
    Empty,
    SingleValue(ValueType),
    // TODO: MultiValue
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct LocalIndex(pub u32);

#[derive(Debug, Clone, Serialize)]
pub struct Local {
    name: SymbolU32,
    ty: ValueType,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct FuncIndex(pub u32);

#[derive(Debug, Clone, Copy, Serialize)]
pub struct GlobalIndex(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
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

#[derive(Debug, Clone, Serialize)]
pub enum Expression {
    Nop,
    I32Const {
        value: i32,
    },
    I64Const {
        value: i64,
    },
    F32Const {
        value: f32,
    },
    F64Const {
        value: f64,
    },
    LocalGet {
        local_index: LocalIndex,
    },
    LocalSet {
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    GlobalGet {
        global_index: GlobalIndex,
    },
    GlobalSet {
        global: GlobalIndex,
        value: Box<Expression>,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    Block {
        expressions: Box<[Expression]>,
        result: BlockResult,
    },
    Break {
        depth: u32,
        value: Option<Box<Expression>>,
    },
    Unreachable,
    Loop {
        expressions: Box<[Expression]>,
        result: BlockResult,
    },
    IfElse {
        condition: Box<Expression>,
        result: BlockResult,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    Drop {
        value: Box<Expression>,
    },
    Call {
        function: FuncIndex,
        arguments: Box<[Expression]>,
    },
    CallIndirect {
        expr: Box<Expression>,
        table_index: TableIndex,
        type_index: TypeIndex,
        arguments: Box<[Expression]>,
    },
    I32Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32Mul {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32DivS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32DivU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32RemS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32RemU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32Ne {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32And {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32Or {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32Xor {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32Eqz {
        value: Box<Expression>,
    },
    I32Shl {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32ShrS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32ShrU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32LtS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32LtU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32GtS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32GtU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32LeS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32LeU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32GeS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I32GeU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64Mul {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64DivS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64DivU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64RemS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64RemU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64Eqz {
        value: Box<Expression>,
    },
    I64Ne {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64And {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64Or {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64Xor {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64Shl {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64ShrS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64ShrU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64LtS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64LtU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64GtS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64GtU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64LeS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64LeU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64GeS {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    I64GeU {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Mul {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Mul {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Ne {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Ne {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Lt {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Lt {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Gt {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Gt {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Le {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Le {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Ge {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Ge {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Div {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F64Div {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    F32Neg {
        value: Box<Expression>,
    },
    F64Neg {
        value: Box<Expression>,
    },
    F32Trunc {
        value: Box<Expression>,
    },
    F64Trunc {
        value: Box<Expression>,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct TypeSection {
    signatures: Box<[FunctionType]>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Serialize)]
pub struct TypeIndex(pub u32);

#[derive(Debug, Clone, Copy, Serialize)]
pub struct TableIndex(pub u32);

#[derive(Debug, Clone, Serialize)]
pub struct FunctionSection {
    types: Box<[TypeIndex]>,
}

#[derive(Debug, Clone, Serialize)]
pub enum ExportItem {
    Function {
        name: SymbolU32,
        func_index: FuncIndex,
    },
    Global {
        name: SymbolU32,
        global_index: GlobalIndex,
    },
    // Table,
    // Memory,
}

#[derive(Debug, Clone, Serialize)]
pub struct ExportSection {
    items: Box<[ExportItem]>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionBody {
    name: SymbolU32,
    locals: Box<[Local]>,
    expressions: Box<[Expression]>,
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeSection {
    expressions: Box<[Expression]>,
    functions: Box<[FunctionBody]>,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

#[derive(Debug, Clone, Serialize)]
pub enum ResizableLimits {
    Initial(u32),
    InitialAndMax { initial: u32, maximum: u32 },
}

#[derive(Debug, Clone, Serialize)]
pub struct TableType {
    ty: RefType,
    limits: ResizableLimits,
}

#[derive(Debug, Clone, Serialize)]
pub struct TableSection {
    tables: Box<[TableType]>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ElementSegment {
    table_index: TableIndex,
    offset: u32,
    indices: Box<[FuncIndex]>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ElementSection {
    segments: Box<[ElementSegment]>,
}

#[derive(Debug, Clone, Serialize)]
pub struct GlobalSection {
    globals: Box<[Global]>,
}

#[derive(Debug, Clone, Serialize)]
struct Global {
    name: SymbolU32,
    ty: ValueType,
    mutability: bool,
    value: Expression,
}

#[derive(Debug, Clone, Serialize)]
pub struct Module {
    types: TypeSection,
    globals: GlobalSection,
    tables: TableSection,
    elements: ElementSection,
    functions: FunctionSection,
    exports: ExportSection,
    code: CodeSection,
}
