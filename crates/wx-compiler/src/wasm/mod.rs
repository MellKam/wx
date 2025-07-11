pub mod builder;
pub mod encoder;
pub mod wat;

pub use builder::*;
pub use encoder::*;

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone)]
pub enum BlockResult {
    Empty,
    SingleValue(ValueType),
    // TODO: MultiValue
}

#[derive(Debug, Clone, Copy)]
pub struct LocalIndex(pub u32);

#[derive(Debug, Clone)]
pub struct Local<'a> {
    name: &'a str,
    ty: ValueType,
}

#[derive(Debug, Clone, Copy)]
pub struct FuncIndex(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct GlobalIndex(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy)]
pub struct ExprIndex(pub u32);

#[derive(Debug, Clone)]
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
        value: ExprIndex,
    },
    GlobalGet {
        global_index: GlobalIndex,
    },
    GlobalSet {
        global: GlobalIndex,
        value: ExprIndex,
    },
    Return {
        value: Option<ExprIndex>,
    },
    Block {
        expressions: Box<[ExprIndex]>,
        result: BlockResult,
    },
    Break {
        depth: u32,
        value: Option<ExprIndex>,
    },
    Unreachable,
    Loop {
        expressions: Box<[ExprIndex]>,
        result: BlockResult,
    },
    IfElse {
        condition: ExprIndex,
        result: BlockResult,
        then_branch: ExprIndex,
        else_branch: Option<ExprIndex>,
    },
    Drop {
        value: ExprIndex,
    },
    Call {
        function: FuncIndex,
        arguments: Box<[ExprIndex]>,
    },
    CallIndirect {
        expr: ExprIndex,
        table_index: TableIndex,
        type_index: TypeIndex,
        arguments: Box<[ExprIndex]>,
    },
    I32Add {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32Sub {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32Mul {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32DivS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32DivU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32RemS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32RemU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32Eq {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32Ne {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32And {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32Or {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32Xor {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32Eqz {
        value: ExprIndex,
    },
    I32Shl {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32ShrS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32ShrU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32LtS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32LtU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32GtS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32GtU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32LeS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32LeU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32GeS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I32GeU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64Add {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64Sub {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64Mul {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64DivS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64DivU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64RemS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64RemU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64Eq {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64Eqz {
        value: ExprIndex,
    },
    I64Ne {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64And {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64Or {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64Xor {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64Shl {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64ShrS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64ShrU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64LtS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64LtU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64GtS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64GtU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64LeS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64LeU {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64GeS {
        left: ExprIndex,
        right: ExprIndex,
    },
    I64GeU {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Add {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Sub {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Mul {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Add {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Sub {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Mul {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Eq {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Eq {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Ne {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Ne {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Lt {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Lt {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Gt {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Gt {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Le {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Le {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Ge {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Ge {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Div {
        left: ExprIndex,
        right: ExprIndex,
    },
    F64Div {
        left: ExprIndex,
        right: ExprIndex,
    },
    F32Neg {
        value: ExprIndex,
    },
    F64Neg {
        value: ExprIndex,
    },
    F32Trunc {
        value: ExprIndex,
    },
    F64Trunc {
        value: ExprIndex,
    },
}

#[derive(Debug, Clone)]
pub struct TypeSection {
    signatures: Box<[FunctionType]>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct TypeIndex(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct TableIndex(pub u32);

#[derive(Debug, Clone)]
pub struct FunctionSection {
    types: Box<[TypeIndex]>,
}

#[derive(Debug, Clone)]
pub enum ExportItem<'a> {
    Function {
        name: &'a str,
        func_index: FuncIndex,
    },
    Global {
        name: &'a str,
        global_index: GlobalIndex,
    },
    // Table,
    // Memory,
}

#[derive(Debug, Clone)]
pub struct ExportSection<'a> {
    items: Box<[ExportItem<'a>]>,
}

#[derive(Debug, Clone)]
pub struct FunctionBody<'a> {
    name: &'a str,
    locals: Box<[Local<'a>]>,
    expressions: Box<[ExprIndex]>,
}

#[derive(Debug, Clone)]
pub struct CodeSection<'a> {
    expressions: Box<[Expression]>,
    functions: Box<[FunctionBody<'a>]>,
}

#[derive(Debug, Clone, Copy)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

#[derive(Debug, Clone)]
pub enum ResizableLimits {
    Initial(u32),
    InitialAndMax { initial: u32, maximum: u32 },
}

#[derive(Debug, Clone)]
pub struct TableType {
    ty: RefType,
    limits: ResizableLimits,
}

#[derive(Debug, Clone)]
pub struct TableSection {
    tables: Box<[TableType]>,
}

#[derive(Debug, Clone)]
pub struct ElementSegment {
    table_index: TableIndex,
    offset: u32,
    indices: Box<[FuncIndex]>,
}

#[derive(Debug, Clone)]
pub struct ElementSection {
    segments: Box<[ElementSegment]>,
}

#[derive(Debug, Clone)]
pub struct GlobalSection<'a> {
    globals: Box<[Global<'a>]>,
}

#[derive(Debug, Clone)]
struct Global<'a> {
    name: &'a str,
    ty: ValueType,
    mutability: bool,
    value: Expression,
}

#[derive(Debug, Clone)]
pub struct Module<'a> {
    types: TypeSection,
    globals: GlobalSection<'a>,
    tables: TableSection,
    elements: ElementSection,
    functions: FunctionSection,
    exports: ExportSection<'a>,
    code: CodeSection<'a>,
}

impl Module<'_> {
    pub fn get_expr(&self, index: ExprIndex) -> &Expression {
        self.code.expressions.get(index.0 as usize).unwrap()
    }

    pub fn get_function(&self, index: FuncIndex) -> &FunctionBody {
        self.code.functions.get(index.0 as usize).unwrap()
    }

    pub fn get_type(&self, index: TypeIndex) -> &FunctionType {
        self.types.signatures.get(index.0 as usize).unwrap()
    }
}
