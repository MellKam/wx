use std::collections::HashMap;

use leb128fmt;

use crate::{ast, mir};

#[derive(Clone, Copy, PartialEq, Hash, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum BlockResult {
    Empty,
    SingleValue(ValueType),
    MultiValue(SignatureIndex),
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct LocalIndex(pub u32);

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Local {
    ty: ValueType,
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct FuncIndex(pub u32);

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct GlobalIndex(pub u32);

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    pub param_count: usize,
    pub param_results: Box<[ValueType]>,
}

impl FunctionSignature {
    pub fn params(&self) -> &[ValueType] {
        self.param_results.get(..self.param_count).unwrap_or(&[])
    }

    pub fn results(&self) -> &[ValueType] {
        self.param_results.get(self.param_count..).unwrap_or(&[])
    }
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
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
    },
    GlobalGet {
        global_index: GlobalIndex,
    },
    GlobalSet {
        global: GlobalIndex,
    },
    Return,
    Block {
        expressions: Box<[Expression]>,
        result: BlockResult,
    },
    Break {
        depth: u32,
    },
    Unreachable,
    Loop {
        expressions: Box<[Expression]>,
        result: BlockResult,
    },
    IfElse {
        result: BlockResult,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    Drop,
    Call {
        function: FuncIndex,
    },
    CallIndirect {
        table_index: TableIndex,
        type_index: SignatureIndex,
    },
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32Eq,
    I32Ne,
    I32And,
    I32Or,
    I32Xor,
    I32Eqz,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64Eq,
    I64Eqz,
    I64Ne,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
    F32Add,
    F32Sub,
    F32Mul,
    F64Add,
    F64Sub,
    F64Mul,
    F32Eq,
    F64Eq,
    F32Ne,
    F64Ne,
    F32Lt,
    F64Lt,
    F32Gt,
    F64Gt,
    F32Le,
    F64Le,
    F32Ge,
    F64Ge,
    F32Div,
    F64Div,
    F32Neg,
    F64Neg,
    F32Trunc,
    F64Trunc,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypeSection {
    signatures: Box<[FunctionSignature]>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, serde::Serialize)]
pub struct SignatureIndex(pub u32);

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct TableIndex(pub u32);

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSection {
    types: Box<[SignatureIndex]>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum ExportItem {
    Function {
        name: String,
        func_index: FuncIndex,
    },
    Global {
        name: String,
        global_index: GlobalIndex,
    },
    // Table,
    Memory {
        name: String,
        memory_index: u32,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ExportSection {
    items: Box<[ExportItem]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionBody {
    locals: Box<[Local]>,
    expressions: Box<[u8]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct CodeSection {
    functions: Box<[FunctionBody]>,
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum ResizableLimits {
    Initial(u32),
    InitialAndMax { initial: u32, maximum: u32 },
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct TableType {
    ty: RefType,
    limits: ResizableLimits,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TableSection {
    tables: Box<[TableType]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ElementSegment {
    table_index: TableIndex,
    offset: u32,
    indices: Box<[FuncIndex]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ElementSection {
    segments: Box<[ElementSegment]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct GlobalSection {
    globals: Box<[Global]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Debug, Clone)]
pub enum Mutability {
    Immutable,
    Mutable,
}

#[cfg_attr(test, derive(serde::Serialize))]
struct Global {
    ty: ValueType,
    mutability: Mutability,
    value: Expression,
}

#[derive(Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImportDesc {
    Function {
        signature_index: SignatureIndex,
    },
    Global {
        ty: ValueType,
        mutability: Mutability,
    },
    Memory {
        memory_type: MemoryType,
    },
}

#[derive(Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDesc,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportSection {
    imports: Box<[Import]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct WasmModule {
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    tables: TableSection,
    memory: MemorySection,
    globals: GlobalSection,
    exports: ExportSection,
    elements: ElementSection,
    code: CodeSection,
    data: DataSection,
}

pub struct Builder {
    table: Vec<FuncIndex>,
    string_pool: StringPool,
    /// Maps every function DefId (imported or defined) to its wasm function
    /// index. Imported functions occupy indices 0..import_func_count;
    /// defined functions follow.
    func_wasm_index: HashMap<ast::DefId, u32>,
    /// Maps every global DefId (imported or defined) to its wasm global index.
    /// Imported globals occupy indices 0..import_global_count; defined globals
    /// follow.
    global_wasm_index: HashMap<ast::DefId, u32>,
    /// Deduplicated function-type entries for the wasm type section.
    /// Shared between function signatures and multi-value block types.
    signatures: HashMap<FunctionSignature, SignatureIndex>,
}

impl TryFrom<mir::Type> for ValueType {
    type Error = ();

    fn try_from(value: mir::Type) -> Result<Self, Self::Error> {
        match value {
            mir::Type::Bool => Ok(ValueType::I32),
            mir::Type::I32 => Ok(ValueType::I32),
            mir::Type::I64 => Ok(ValueType::I64),
            mir::Type::F32 => Ok(ValueType::F32),
            mir::Type::F64 => Ok(ValueType::F64),
            mir::Type::U32 => Ok(ValueType::I32),
            mir::Type::U64 => Ok(ValueType::I64),
            mir::Type::Pointer => Ok(ValueType::I32),
            mir::Type::Function { .. } => Ok(ValueType::I32),
            _ => unreachable!(),
        }
    }
}

struct StringPool {
    // symbol → (memory_offset, byte_length)
    strings: Vec<(u32, u32)>,
    bytes: Vec<u8>,
}

impl StringPool {
    fn new() -> Self {
        StringPool {
            strings: Vec::new(),
            bytes: Vec::new(),
        }
    }

    fn add(&mut self, s: &str) -> (u32, u32) {
        let offset = self.bytes.len() as u32;
        let len = s.len() as u32;
        self.bytes.extend_from_slice(s.as_bytes());
        self.strings.push((offset, len));
        (offset, len)
    }
}

impl Builder {
    /// Recursively expand a MIR type into its flat wasm `ValueType`s.
    /// Unit/Never produce zero slots; Aggregate recurses into its fields.
    fn flatten_type(ty: mir::Type, aggregates: &[mir::Aggregate]) -> Vec<ValueType> {
        match ty {
            mir::Type::Unit | mir::Type::Never => vec![],
            mir::Type::Aggregate { aggregate_index } => aggregates[aggregate_index as usize]
                .values
                .iter()
                .flat_map(|&f| Self::flatten_type(f, aggregates))
                .collect(),
            t => vec![ValueType::try_from(t).unwrap()],
        }
    }

    /// Intern a function signature built from a MIR signature + the aggregate
    /// pool, correctly flattening any aggregate params/results into
    /// individual wasm types.
    fn register_signature(
        &mut self,
        sig: &mir::FunctionSignature,
        aggregates: &[mir::Aggregate],
    ) -> SignatureIndex {
        let mut param_results = Vec::new();
        for &param in sig.params() {
            param_results.extend(Self::flatten_type(param, aggregates));
        }
        let param_count = param_results.len();
        param_results.extend(Self::flatten_type(sig.result(), aggregates));
        let signature = FunctionSignature {
            param_count,
            param_results: param_results.into_boxed_slice(),
        };
        let next = SignatureIndex(self.signatures.len() as u32);
        *self.signatures.entry(signature).or_insert(next)
    }

    pub fn build<'interner>(
        mir: &mir::MIR,
        interner: &'interner ast::StringInterner,
    ) -> Result<WasmModule, ()> {
        let mut builder = Builder {
            table: Vec::new(),
            string_pool: StringPool::new(),
            func_wasm_index: HashMap::new(),
            global_wasm_index: HashMap::new(),
            signatures: HashMap::new(),
        };

        for symbol in mir.strings.iter().copied() {
            let s = interner.resolve(symbol).unwrap();
            builder.string_pool.add(s);
        }
        let required_pages = if builder.string_pool.bytes.is_empty() {
            0
        } else {
            (builder.string_pool.bytes.len() as u32)
                .div_ceil(65536)
                .max(1)
        };

        // Process imports: build the wasm Import list and assign wasm indices.
        // Imports come first in the wasm binary, so their indices are assigned here.
        let mut imports = Vec::<Import>::new();
        let mut next_func_idx: u32 = 0;
        let mut next_global_idx: u32 = 0;
        for import_module in &mir.imports {
            for item in &import_module.items {
                match item {
                    mir::ImportModuleItem::Function {
                        name,
                        id,
                        signature_index,
                    } => {
                        let sig_index = builder.register_signature(
                            &mir.signatures[*signature_index as usize],
                            &mir.aggregates,
                        );
                        imports.push(Import {
                            module: import_module.name.clone(),
                            name: interner.resolve(*name).unwrap().to_string(),
                            desc: ImportDesc::Function {
                                signature_index: sig_index,
                            },
                        });
                        builder.func_wasm_index.insert(*id, next_func_idx);
                        next_func_idx += 1;
                    }
                    mir::ImportModuleItem::Global { name, id } => {
                        // TODO: get actual type from the MIR once global types are tracked
                        imports.push(Import {
                            module: import_module.name.clone(),
                            name: interner.resolve(*name).unwrap().to_string(),
                            desc: ImportDesc::Global {
                                ty: ValueType::I32,
                                mutability: Mutability::Immutable,
                            },
                        });
                        builder.global_wasm_index.insert(*id, next_global_idx);
                        next_global_idx += 1;
                    }
                    mir::ImportModuleItem::Memory { name, .. } => {
                        imports.push(Import {
                            module: import_module.name.clone(),
                            name: interner.resolve(*name).unwrap().to_string(),
                            desc: ImportDesc::Memory {
                                memory_type: MemoryType::Unbounded { initial: 0 },
                            },
                        });
                    }
                }
            }
        }
        // Defined functions follow all imports; defined globals follow all imported
        // globals.
        for func in &mir.functions {
            builder.func_wasm_index.insert(func.id, next_func_idx);
            next_func_idx += 1;
        }
        for global in &mir.globals {
            builder.global_wasm_index.insert(global.id, next_global_idx);
            next_global_idx += 1;
        }

        let mut function_signatures = Vec::<SignatureIndex>::with_capacity(mir.functions.len());
        let exports: Box<_> = mir
            .exports
            .iter()
            .map(|item| match item {
                mir::ExportItem::Global { id, name } => ExportItem::Global {
                    name: interner.resolve(*name).unwrap().to_string(),
                    global_index: GlobalIndex(builder.global_wasm_index[id]),
                },
                mir::ExportItem::Function { id, name } => ExportItem::Function {
                    name: interner.resolve(*name).unwrap().to_string(),
                    func_index: FuncIndex(builder.func_wasm_index[id]),
                },
                mir::ExportItem::Memory { memory_index, name } => ExportItem::Memory {
                    memory_index: *memory_index,
                    name: interner.resolve(*name).unwrap().to_string(),
                },
            })
            .chain(
                if required_pages > 0
                    && !mir
                        .exports
                        .iter()
                        .any(|e| matches!(e, mir::ExportItem::Memory { .. }))
                {
                    Some(ExportItem::Memory {
                        memory_index: 0,
                        name: "memory".to_string(),
                    })
                } else {
                    None
                },
            )
            .collect();

        let mut functions = Vec::<FunctionBody>::with_capacity(mir.functions.len());
        for func in mir.functions.iter() {
            let signature_index = builder.register_signature(
                &mir.signatures[func.signature_index as usize],
                &mir.aggregates,
            );
            function_signatures.push(signature_index);

            let opt_func = crate::opt::builder::Builder::build(mir, func);
            let scheduled = crate::opt::scheduler::Scheduler::schedule(&opt_func, mir);
            let body = builder.encode_scheduled(&scheduled, mir);

            functions.push(body);
        }

        let globals = GlobalSection {
            globals: mir
                .globals
                .iter()
                .map(|global| Global {
                    ty: ValueType::try_from(global.ty).unwrap(),
                    mutability: match global.mutability {
                        mir::Mutability::Mutable => Mutability::Mutable,
                        mir::Mutability::Immutable => Mutability::Immutable,
                    },
                    value: builder.build_global_expr(global),
                })
                .collect::<Box<_>>(),
        };

        Ok(WasmModule {
            types: TypeSection {
                signatures: {
                    let mut sorted_types: Vec<_> = builder.signatures.drain().collect();
                    sorted_types.sort_by_key(|&(_, index)| index);
                    sorted_types
                        .into_iter()
                        .map(|(ty, _)| ty)
                        .collect::<Box<_>>()
                },
            },
            imports: ImportSection {
                imports: imports.into_boxed_slice(),
            },
            data: DataSection {
                segments: if builder.string_pool.bytes.is_empty() {
                    Box::new([])
                } else {
                    Box::new([DataSegment {
                        memory_index: 0,
                        offset: 0,
                        bytes: builder.string_pool.bytes.into_boxed_slice(),
                    }])
                },
            },
            memory: MemorySection {
                // Emit one entry per defined memory from the source.  The first defined
                // memory (wasm index = number of imported memories) must be large enough
                // to hold the static data section (string pool), so its initial page count
                // is at least `required_pages`.  All memories are emitted as unbounded so
                // that `memory.grow` can succeed at runtime.
                memories: {
                    let imported_memory_count = mir
                        .imports
                        .iter()
                        .flat_map(|m| m.items.iter())
                        .filter(|item| matches!(item, mir::ImportModuleItem::Memory { .. }))
                        .count() as u32;
                    if mir.memories.is_empty() {
                        // No declared memories: emit a synthetic one only if string data
                        // needs a home.
                        if required_pages == 0 {
                            Box::new([])
                        } else {
                            Box::new([MemoryType::Unbounded {
                                initial: required_pages,
                            }])
                        }
                    } else {
                        mir.memories
                            .iter()
                            .enumerate()
                            .map(|(i, _kind)| {
                                // The first defined memory (lowest wasm index after imports)
                                // needs at least `required_pages` to cover static data.
                                let initial = if i as u32 == 0 && imported_memory_count == 0 {
                                    required_pages
                                } else {
                                    0
                                };
                                MemoryType::Unbounded { initial }
                            })
                            .collect()
                    }
                },
            },
            globals,
            tables: TableSection {
                tables: match builder.table.len() {
                    0 => Box::new([]),
                    _ => Box::new([TableType {
                        ty: RefType::FuncRef,
                        limits: ResizableLimits::Initial(builder.table.len() as u32),
                    }]),
                },
            },
            elements: ElementSection {
                segments: match builder.table.len() {
                    0 => Box::new([]),
                    _ => Box::new([ElementSegment {
                        table_index: TableIndex(0),
                        offset: 0,
                        indices: builder.table.into_boxed_slice(),
                    }]),
                },
            },
            functions: FunctionSection {
                types: function_signatures.into_boxed_slice(),
            },
            exports: ExportSection { items: exports },
            code: CodeSection {
                functions: functions.into_boxed_slice(),
            },
        })
    }

    /// Encode a [`scheduler::ScheduledFunction`] into a [`FunctionBody`].
    ///
    /// All index resolution (function WASM indices, global WASM indices, string
    /// byte offsets, data section end) is performed here using the maps and
    /// pools already built on `self`.
    fn encode_scheduled(
        &mut self,
        scheduled: &crate::opt::scheduler::ScheduledFunction,
        mir: &mir::MIR,
    ) -> FunctionBody {
        let locals: Box<[Local]> = scheduled
            .locals
            .iter()
            .map(|l| Local {
                ty: ValueType::from(l.ty),
            })
            .collect();

        let mut sink = Vec::new();
        for instr in scheduled.body.iter().cloned() {
            self.encode_scheduled_instr(instr, mir, &mut sink);
        }

        FunctionBody {
            locals,
            expressions: sink.into_boxed_slice(),
        }
    }

    fn encode_scheduled_instr(
        &mut self,
        instr: crate::opt::scheduler::Instruction,
        mir: &mir::MIR,
        sink: &mut Vec<u8>,
    ) {
        use crate::opt::scheduler::Instruction as SI;
        match instr {
            SI::I32Const(v) => {
                sink.push(Instruction::I32Const as u8);
                v.encode(sink);
            }
            SI::I64Const(v) => {
                sink.push(Instruction::I64Const as u8);
                v.encode(sink);
            }
            SI::F32Const(v) => {
                sink.push(Instruction::F32Const as u8);
                v.encode(sink);
            }
            SI::F64Const(v) => {
                sink.push(Instruction::F64Const as u8);
                v.encode(sink);
            }
            SI::LocalGet(i) => {
                sink.push(Instruction::LocalGet as u8);
                i.encode(sink);
            }
            SI::LocalSet(i) => {
                sink.push(Instruction::LocalSet as u8);
                i.encode(sink);
            }
            SI::LocalTee(i) => {
                sink.push(Instruction::LocalTee as u8);
                i.encode(sink);
            }
            SI::GlobalGet(id) => {
                let wasm_idx = self.global_wasm_index[&id];
                sink.push(Instruction::GlobalGet as u8);
                wasm_idx.encode(sink);
            }
            SI::GlobalSet(id) => {
                let wasm_idx = self.global_wasm_index[&id];
                sink.push(Instruction::GlobalSet as u8);
                wasm_idx.encode(sink);
            }
            SI::I32Add => sink.push(Instruction::I32Add as u8),
            SI::I32Sub => sink.push(Instruction::I32Sub as u8),
            SI::I32Mul => sink.push(Instruction::I32Mul as u8),
            SI::I32DivS => sink.push(Instruction::I32DivS as u8),
            SI::I32RemS => sink.push(Instruction::I32RemS as u8),
            SI::I32And => sink.push(Instruction::I32And as u8),
            SI::I32Or => sink.push(Instruction::I32Or as u8),
            SI::I32Xor => sink.push(Instruction::I32Xor as u8),
            SI::I32Shl => sink.push(Instruction::I32Shl as u8),
            SI::I32ShrS => sink.push(Instruction::I32ShrS as u8),
            SI::I32ShrU => sink.push(Instruction::I32ShrU as u8),
            SI::I32Eqz => sink.push(Instruction::I32Eqz as u8),
            SI::I32Eq => sink.push(Instruction::I32Eq as u8),
            SI::I32Ne => sink.push(Instruction::I32Ne as u8),
            SI::I32LtS => sink.push(Instruction::I32LtS as u8),
            SI::I32LtU => sink.push(Instruction::I32LtU as u8),
            SI::I32LeS => sink.push(Instruction::I32LeS as u8),
            SI::I32LeU => sink.push(Instruction::I32LeU as u8),
            SI::I32GtS => sink.push(Instruction::I32GtS as u8),
            SI::I32GtU => sink.push(Instruction::I32GtU as u8),
            SI::I32GeS => sink.push(Instruction::I32GeS as u8),
            SI::I32GeU => sink.push(Instruction::I32GeU as u8),
            SI::I32Clz => sink.push(Instruction::I32Clz as u8),
            SI::I32Ctz => sink.push(Instruction::I32Ctz as u8),
            SI::I64Add => sink.push(Instruction::I64Add as u8),
            SI::I64Sub => sink.push(Instruction::I64Sub as u8),
            SI::I64Mul => sink.push(Instruction::I64Mul as u8),
            SI::I64DivS => sink.push(Instruction::I64DivS as u8),
            SI::I64RemS => sink.push(Instruction::I64RemS as u8),
            SI::I64And => sink.push(Instruction::I64And as u8),
            SI::I64Or => sink.push(Instruction::I64Or as u8),
            SI::I64Xor => sink.push(Instruction::I64Xor as u8),
            SI::I64Shl => sink.push(Instruction::I64Shl as u8),
            SI::I64ShrS => sink.push(Instruction::I64ShrS as u8),
            SI::I64ShrU => sink.push(Instruction::I64ShrU as u8),
            SI::I64Eqz => sink.push(Instruction::I64Eqz as u8),
            SI::I64Eq => sink.push(Instruction::I64Eq as u8),
            SI::I64Ne => sink.push(Instruction::I64Ne as u8),
            SI::I64LtS => sink.push(Instruction::I64LtS as u8),
            SI::I64LtU => sink.push(Instruction::I64LtU as u8),
            SI::I64LeS => sink.push(Instruction::I64LeS as u8),
            SI::I64LeU => sink.push(Instruction::I64LeU as u8),
            SI::I64GtS => sink.push(Instruction::I64GtS as u8),
            SI::I64GtU => sink.push(Instruction::I64GtU as u8),
            SI::I64GeS => sink.push(Instruction::I64GeS as u8),
            SI::I64GeU => sink.push(Instruction::I64GeU as u8),
            SI::F32Add => sink.push(Instruction::F32Add as u8),
            SI::F32Sub => sink.push(Instruction::F32Sub as u8),
            SI::F32Mul => sink.push(Instruction::F32Mul as u8),
            SI::F32Div => sink.push(Instruction::F32Div as u8),
            SI::F32Neg => sink.push(Instruction::F32Neg as u8),
            SI::F64Add => sink.push(Instruction::F64Add as u8),
            SI::F64Sub => sink.push(Instruction::F64Sub as u8),
            SI::F64Mul => sink.push(Instruction::F64Mul as u8),
            SI::F64Div => sink.push(Instruction::F64Div as u8),
            SI::F64Neg => sink.push(Instruction::F64Neg as u8),
            SI::F32Eq => sink.push(Instruction::F32Eq as u8),
            SI::F32Ne => sink.push(Instruction::F32Ne as u8),
            SI::F32Lt => sink.push(Instruction::F32Lt as u8),
            SI::F32Le => sink.push(Instruction::F32Le as u8),
            SI::F32Gt => sink.push(Instruction::F32Gt as u8),
            SI::F32Ge => sink.push(Instruction::F32Ge as u8),
            SI::F64Eq => sink.push(Instruction::F64Eq as u8),
            SI::F64Ne => sink.push(Instruction::F64Ne as u8),
            SI::F64Lt => sink.push(Instruction::F64Lt as u8),
            SI::F64Le => sink.push(Instruction::F64Le as u8),
            SI::F64Gt => sink.push(Instruction::F64Gt as u8),
            SI::F64Ge => sink.push(Instruction::F64Ge as u8),
            SI::Block { ty } => {
                sink.push(Instruction::Block as u8);
                Self::encode_block_type(ty, sink);
            }
            SI::Loop { ty } => {
                sink.push(Instruction::Loop as u8);
                Self::encode_block_type(ty, sink);
            }
            SI::If { ty } => {
                sink.push(Instruction::If as u8);
                Self::encode_block_type(ty, sink);
            }
            SI::Else => sink.push(Instruction::Else as u8),
            SI::End => sink.push(Instruction::End as u8),
            SI::Br(depth) => {
                sink.push(Instruction::Br as u8);
                depth.encode(sink);
            }
            SI::BrIf(depth) => {
                sink.push(Instruction::BrIf as u8);
                depth.encode(sink);
            }
            SI::Return => sink.push(Instruction::Return as u8),
            SI::Unreachable => sink.push(Instruction::Unreachable as u8),
            SI::Drop => sink.push(Instruction::Drop as u8),
            SI::Call(id) => {
                let wasm_idx = self.func_wasm_index[&id];
                sink.push(Instruction::Call as u8);
                wasm_idx.encode(sink);
            }
            SI::CallIndirectSym { mir_sig_index } => {
                let type_index = self
                    .register_signature(&mir.signatures[mir_sig_index as usize], &mir.aggregates);
                sink.push(Instruction::CallIndirect as u8);
                type_index.0.encode(sink);
                0u32.encode(sink); // table index 0 (single function table)
            }
            SI::MemorySize(idx) => {
                sink.push(Instruction::MemorySize as u8);
                idx.encode(sink);
            }
            SI::MemoryGrow(idx) => {
                sink.push(Instruction::MemoryGrow as u8);
                idx.encode(sink);
            }
            SI::Nop => sink.push(Instruction::Nop as u8),
            SI::FunctionPointer(id) => {
                let wasm_idx = self.func_wasm_index[&id];
                let table_idx = self.table.len() as i32;
                self.table.push(FuncIndex(wasm_idx));
                sink.push(Instruction::I32Const as u8);
                table_idx.encode(sink);
            }
            SI::StringPointer(pool_idx) => {
                let (byte_offset, _) = self.string_pool.strings[pool_idx as usize];
                sink.push(Instruction::I32Const as u8);
                (byte_offset as i32).encode(sink);
            }
            SI::DataSectionEnd { .. } => {
                // All static string data lives in memory 0 at offset 0; the end
                // of the data section is the total pool byte length.
                let offset = self.string_pool.bytes.len() as i32;
                sink.push(Instruction::I32Const as u8);
                offset.encode(sink);
            }
        }
    }

    fn encode_block_type(ty: crate::opt::scheduler::BlockType, sink: &mut Vec<u8>) {
        use crate::opt::scheduler::BlockType;
        match ty {
            BlockType::Empty => sink.push(0x40),
            BlockType::Value(vt) => ValueType::from(vt).encode(sink),
        }
    }

    fn build_global_expr(&self, global: &mir::Global) -> Expression {
        match global.value.kind {
            mir::ExprKind::Int { value } => match global.ty {
                mir::Type::I32 | mir::Type::U32 => Expression::I32Const {
                    value: value as i32,
                },
                mir::Type::I64 | mir::Type::U64 => Expression::I64Const { value },
                _ => unreachable!(),
            },
            mir::ExprKind::Float { value } => match global.ty {
                mir::Type::F32 => Expression::F32Const {
                    value: value as f32,
                },
                mir::Type::F64 => Expression::F64Const { value },
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

#[allow(unused)]
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

#[allow(unused)]
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

impl Encode for ValueType {
    fn encode(&self, sink: &mut Vec<u8>) {
        let opcode = match self {
            ValueType::I32 => 0x7F,
            ValueType::I64 => 0x7E,
            ValueType::F32 => 0x7D,
            ValueType::F64 => 0x7C,
        };

        sink.push(opcode);
    }
}

impl Encode for BlockResult {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            BlockResult::Empty => sink.push(0x40),
            BlockResult::SingleValue(ty) => ty.encode(sink),
            // Multi-value block types are encoded as a type-section index (s33).
            // Type indices are always small positive integers, so s33 == u32 LEB128.
            BlockResult::MultiValue(idx) => idx.0.encode(sink),
        }
    }
}

impl Encode for FunctionSignature {
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

impl Encode for TypeSection {
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

impl Encode for ImportSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Import as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        (self.imports.len() as u32).encode(&mut section_sink);
        for import in &self.imports {
            // Module name
            (import.module.len() as u32).encode(&mut section_sink);
            section_sink.extend_from_slice(import.module.as_bytes());

            // Import name
            (import.name.len() as u32).encode(&mut section_sink);
            section_sink.extend_from_slice(import.name.as_bytes());

            // Import description
            match &import.desc {
                ImportDesc::Function { signature_index } => {
                    section_sink.push(0x00); // Function import kind
                    signature_index.0.encode(&mut section_sink);
                }
                ImportDesc::Global { ty, mutability } => {
                    section_sink.push(0x03); // Global import kind
                    ty.encode(&mut section_sink);
                    match mutability {
                        Mutability::Immutable => section_sink.push(0x00),
                        Mutability::Mutable => section_sink.push(0x01),
                    }
                }
                ImportDesc::Memory { memory_type } => {
                    section_sink.push(0x02); // Memory import kind
                    memory_type.encode(&mut section_sink);
                }
            }
        }

        (section_sink.len() as u32).encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl Encode for FunctionSection {
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
    _Table = 0x01,
    Memory = 0x02,
    Global = 0x03,
}

impl Encode for ExportItem {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self.clone() {
            ExportItem::Function { name, func_index } => {
                (name.len() as u32).encode(sink);
                sink.extend_from_slice(name.as_bytes());
                sink.push(ExportKind::Function as u8);
                func_index.0.encode(sink);
            }
            ExportItem::Global { name, global_index } => {
                (name.len() as u32).encode(sink);
                sink.extend_from_slice(name.as_bytes());
                sink.push(ExportKind::Global as u8);
                global_index.0.encode(sink);
            }
            ExportItem::Memory { name, memory_index } => {
                (name.len() as u32).encode(sink);
                sink.extend_from_slice(name.as_bytes());
                sink.push(ExportKind::Memory as u8);
                memory_index.encode(sink);
            }
        }
    }
}

impl Encode for ExportSection {
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

impl Encode for Global {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.ty.encode(sink);
        sink.push(match self.mutability {
            Mutability::Immutable => 0x00,
            Mutability::Mutable => 0x01,
        });

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

impl Encode for GlobalSection {
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

impl FunctionBody {
    fn encode(&self, sink: &mut Vec<u8>, module: &WasmModule, func_index: FuncIndex) {
        let mut body_content: Vec<u8> = Vec::new();

        let type_index = module.functions.types[func_index.0 as usize];
        let func_type = module.types.signatures[type_index.0 as usize].clone();

        let mut grouped_locals = Vec::<(ValueType, u32)>::new();
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

        body_content.extend_from_slice(&module.code.functions[func_index.0 as usize].expressions);
        body_content.push(Instruction::End as u8);

        let body_size = body_content.len() as u32;
        body_size.encode(sink);
        sink.extend_from_slice(&body_content);
    }
}

trait ContextEncode {
    fn encode(&self, sink: &mut Vec<u8>, module: &WasmModule);
}

impl ContextEncode for CodeSection {
    fn encode(&self, sink: &mut Vec<u8>, module: &WasmModule) {
        sink.push(SectionId::Code as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let function_count = self.functions.len() as u32;
        function_count.encode(&mut section_sink);
        for (index, func) in self.functions.iter().enumerate() {
            func.encode(&mut section_sink, module, FuncIndex(index as u32));
        }

        let section_size = section_sink.len() as u32;
        section_size.encode(sink);
        sink.extend_from_slice(&section_sink);
    }
}

impl Encode for RefType {
    fn encode(&self, sink: &mut Vec<u8>) {
        let ref_type = match self {
            RefType::FuncRef => 0x70,
            RefType::ExternRef => 0x6F,
        };
        sink.push(ref_type);
    }
}

impl Encode for ResizableLimits {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ResizableLimits::Initial(initial) => {
                sink.push(0x00);
                initial.encode(sink);
            }
            ResizableLimits::InitialAndMax { initial, maximum } => {
                sink.push(0x01);
                initial.encode(sink);
                maximum.encode(sink);
            }
        }
    }
}

impl Encode for TableType {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.ty.encode(sink);
        self.limits.encode(sink);
    }
}

impl Encode for TableSection {
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

impl Encode for ElementSegment {
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

impl Encode for ElementSection {
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

#[cfg_attr(test, derive(serde::Serialize))]
pub struct DataSection {
    pub segments: Box<[DataSegment]>,
}

impl Encode for DataSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Data as u8);

        let mut content = Vec::new();
        (self.segments.len() as u32).encode(&mut content);
        for segment in self.segments.iter() {
            segment.encode(&mut content);
        }

        (content.len() as u32).encode(sink);
        sink.extend_from_slice(&content);
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct DataSegment {
    pub memory_index: u32,
    pub offset: u32, // i32.const offset expr in wasm binary
    pub bytes: Box<[u8]>,
}

impl Encode for DataSegment {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.memory_index.encode(sink);
        // offset is encoded as i32.const offset followed by end opcode
        sink.push(Instruction::I32Const as u8);
        self.offset.encode(sink);
        sink.push(Instruction::End as u8);
        (self.bytes.len() as u32).encode(sink);
        sink.extend_from_slice(&self.bytes);
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct MemorySection {
    pub memories: Box<[MemoryType]>,
}

#[derive(Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum MemoryType {
    /// Only initial size specified (can grow up to 2^16 pages)
    Unbounded { initial: u32 },
    /// Both min and max specified
    Bounded { initial: u32, max: u32 },
}

impl Encode for MemorySection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(SectionId::Memory as u8);

        let mut content = Vec::new();
        (self.memories.len() as u32).encode(&mut content);
        for memory in self.memories.iter() {
            memory.encode(&mut content);
        }

        (content.len() as u32).encode(sink);
        sink.extend_from_slice(&content);
    }
}

impl Encode for MemoryType {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            MemoryType::Unbounded { initial } => {
                sink.push(0x00);
                initial.encode(sink);
            }
            MemoryType::Bounded { initial, max } => {
                sink.push(0x01);
                initial.encode(sink);
                max.encode(sink);
            }
        }
    }
}

impl WasmModule {
    pub fn encode(&self) -> Vec<u8> {
        let mut sink = [
            0x00, 0x61, 0x73, 0x6D, // Magic
            0x01, 0x00, 0x00, 0x00, // Version
        ]
        .to_vec();

        self.types.encode(&mut sink);
        match self.imports.imports.len() {
            0 => {}
            _ => self.imports.encode(&mut sink),
        }
        match self.functions.types.len() {
            0 => {}
            _ => self.functions.encode(&mut sink),
        }
        match self.tables.tables.len() {
            0 => {}
            _ => self.tables.encode(&mut sink),
        }
        match self.memory.memories.len() {
            0 => {}
            _ => self.memory.encode(&mut sink),
        }
        match self.globals.globals.len() {
            0 => {}
            _ => self.globals.encode(&mut sink),
        }
        match self.exports.items.len() {
            0 => {}
            _ => self.exports.encode(&mut sink),
        }
        match self.elements.segments.len() {
            0 => {}
            _ => self.elements.encode(&mut sink),
        }
        self.code.encode(&mut sink, self);
        match self.data.segments.len() {
            0 => {}
            _ => self.data.encode(&mut sink),
        }

        sink
    }
}

#[cfg(test)]
mod tests;
