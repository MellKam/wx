use std::collections::HashMap;

use leb128fmt;

use crate::{ast, mir, tir};

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
    // TODO: MultiValue
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

impl From<mir::FunctionSignature> for FunctionSignature {
    fn from(ty: mir::FunctionSignature) -> Self {
        let param_count = ty.params().len();

        // Filter out Unit return types (functions with no return value in WASM)
        let param_results: Box<[ValueType]> = ty
            .items
            .into_iter()
            .filter_map(|ty| {
                if matches!(ty, mir::Type::Unit) {
                    None
                } else {
                    Some(ValueType::try_from(ty).unwrap())
                }
            })
            .collect();

        FunctionSignature {
            param_count,
            param_results,
        }
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
    // Memory,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ExportSection {
    items: Box<[ExportItem]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionBody {
    locals: Box<[Local]>,
    expressions: Box<[Expression]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct CodeSection {
    expressions: Box<[Expression]>,
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

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImportDesc {
    Function {
        signature_index: SignatureIndex,
    },
    Global {
        ty: ValueType,
        mutability: Mutability,
    },
}

#[derive(Debug, Clone)]
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

pub struct Builder<'mir, 'interner> {
    expressions: Vec<Expression>,
    table: Vec<FuncIndex>,
    mir: &'mir mir::MIR,
    string_pool: StringPool,
    interner: &'interner ast::StringInterner,
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
            mir::Type::Function { .. } => Ok(ValueType::I32),
            _ => Err(()),
        }
    }
}

impl From<mir::Type> for BlockResult {
    fn from(value: mir::Type) -> Self {
        ValueType::try_from(value)
            .map(BlockResult::SingleValue)
            .unwrap_or(BlockResult::Empty)
    }
}

struct FunctionContext<'mir> {
    local_offsets: Box<[usize]>,
    locals: Box<[Local]>,
    scope_offsets: Box<[usize]>,
    scopes: &'mir Vec<mir::BlockScope>,
    scope_index: mir::ScopeIndex,
}

impl FunctionContext<'_> {
    fn get_flat_index(
        &self,
        scope_index: tir::ScopeIndex,
        local_index: tir::LocalIndex,
    ) -> LocalIndex {
        let scope_offset = self.scope_offsets[scope_index as usize];
        let local_offset = self.local_offsets[scope_offset + local_index as usize];
        LocalIndex(local_offset as u32)
    }

    pub fn get_break_depth(&self, target_scope: tir::ScopeIndex) -> Option<u32> {
        let mut index = self.scope_index;
        let mut depth = 0;

        loop {
            let scope = &self.scopes[index as usize];
            depth += match scope.kind {
                tir::BlockKind::Loop => depth + 2,
                tir::BlockKind::Block => depth + 1,
            };

            if index == target_scope {
                return Some(depth - 1);
            }

            match scope.parent {
                Some(scope_index) => {
                    index = scope_index;
                }
                None => return None,
            }
        }
    }

    pub fn get_continue_depth(&self, target_scope: tir::ScopeIndex) -> Option<u32> {
        let mut index = self.scope_index;
        let mut depth = 0;

        loop {
            if index == target_scope {
                return Some(depth);
            }

            let scope = self.scopes.get(index as usize).unwrap();
            match scope.parent {
                Some(scope_index) => {
                    index = scope_index;
                    depth += match scope.kind {
                        tir::BlockKind::Loop => 2, // 1 for the loop, 1 for the block
                        tir::BlockKind::Block => 1,
                    };
                }
                None => return None,
            }
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

impl<'mir, 'interner> Builder<'mir, 'interner> {
    pub fn build<'a>(
        mir: &mir::MIR,
        interner: &'interner ast::StringInterner,
    ) -> Result<WasmModule, ()> {
        let mut builder = Builder {
            mir,
            interner,
            expressions: Vec::new(),
            table: Vec::new(),
            string_pool: StringPool::new(),
        };

        let mut signatures = HashMap::<FunctionSignature, SignatureIndex>::new();

        // Process imports first to build signatures for imported functions
        let mut imports = Vec::<Import>::new();
        for import_module in &mir.imports {
            for item in &import_module.items {
                match item {
                    mir::ImportModuleItem::Function {
                        name,
                        func_index: _,
                        signature_index,
                    } => {
                        // Get the signature from MIR and ensure it's in the signatures map
                        let signature = FunctionSignature::from(
                            mir.signatures[*signature_index as usize].clone(),
                        );
                        let next_signature_index = SignatureIndex(signatures.len() as u32);
                        let sig_index = signatures
                            .entry(signature)
                            .or_insert(next_signature_index)
                            .clone();

                        imports.push(Import {
                            module: import_module.name.clone(),
                            name: name.clone(),
                            desc: ImportDesc::Function {
                                signature_index: sig_index,
                            },
                        });
                    }
                    mir::ImportModuleItem::Global {
                        name,
                        global_index: _,
                    } => {
                        // For now, assume imported globals are i32 and immutable
                        // TODO: Get actual type from the MIR
                        imports.push(Import {
                            module: import_module.name.clone(),
                            name: name.clone(),
                            desc: ImportDesc::Global {
                                ty: ValueType::I32,
                                mutability: Mutability::Immutable,
                            },
                        });
                    }
                }
            }
        }

        let mut function_signatures = Vec::<SignatureIndex>::with_capacity(mir.functions.len());
        let exports: Box<_> = mir
            .exports
            .iter()
            .map(|item| match item {
                mir::ExportItem::Global { global_index, name } => ExportItem::Global {
                    name: name.clone(),
                    global_index: GlobalIndex(*global_index),
                },
                mir::ExportItem::Function { func_index, name } => ExportItem::Function {
                    name: name.clone(),
                    // MIR func_index already accounts for imports (absolute index in function
                    // space)
                    func_index: FuncIndex(*func_index),
                },
            })
            .collect();

        let mut functions = Vec::<FunctionBody>::with_capacity(mir.functions.len());
        for func in mir.functions.iter() {
            let signature =
                FunctionSignature::from(mir.signatures[func.signature_index as usize].clone());
            let next_signature_index = SignatureIndex(signatures.len() as u32);
            let signature_index = signatures
                .entry(signature)
                .or_insert(next_signature_index)
                .clone();
            function_signatures.push(signature_index);

            let expressions = match &func.block.kind {
                mir::ExprKind::Block { expressions, .. } => expressions,
                _ => unreachable!(),
            };

            let scope_offsets: Box<_> = func
                .scopes
                .iter()
                .scan(0, |offset, scope| {
                    let current = *offset;
                    *offset += scope.locals.len();
                    Some(current)
                })
                .collect();

            let min_capacity = scope_offsets.last().copied().unwrap_or(0);
            let mut locals = Vec::with_capacity(min_capacity);
            let mut local_offsets = Vec::with_capacity(min_capacity + 1);
            local_offsets.push(0); // First local starts at index 0

            for scope in &func.scopes {
                for local in scope.locals.iter().cloned() {
                    match local.ty {
                        mir::Type::Tuple { tuple_index } => {
                            let tuple = &mir.tuples[tuple_index as usize];
                            local_offsets
                                .push(local_offsets.last().copied().unwrap() + tuple.fields.len());
                            for field in tuple.fields.iter().copied() {
                                locals.push(Local {
                                    ty: ValueType::try_from(field).unwrap(),
                                });
                            }
                        }
                        _ => {
                            local_offsets.push(local_offsets.last().copied().unwrap() + 1);
                            locals.push(Local {
                                ty: ValueType::try_from(local.ty).unwrap(),
                            });
                        }
                    }
                }
            }

            let mut ctx = FunctionContext {
                locals: locals.into_boxed_slice(),
                scope_offsets,
                local_offsets: local_offsets.into_boxed_slice(),
                scope_index: 0 as tir::ScopeIndex,
                scopes: &func.scopes,
            };

            let mut sink = Vec::new();
            for expr in expressions {
                builder.build_expression(&mut ctx, expr, &mut sink)?;
            }

            functions.push(FunctionBody {
                locals: ctx.locals,
                expressions: sink.into_boxed_slice(),
            });
        }

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
                    let mut sorted_types: Vec<_> = signatures.into_iter().collect();
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
                memories: if required_pages == 0 {
                    Box::new([])
                } else {
                    Box::new([MemoryType::Bounded {
                        initial: required_pages,
                        max: required_pages,
                    }])
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
                expressions: builder.expressions.into_boxed_slice(),
                functions: functions.into_boxed_slice(),
            },
        })
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

    fn build_expression<'wasm>(
        &mut self,
        ctx: &mut FunctionContext<'mir>,
        expr: &mir::Expression,
        sink: &mut Vec<Expression>,
    ) -> Result<(), ()> {
        match &expr.kind {
            mir::ExprKind::Function { func_index } => {
                let table_index = self.table.len() as i32;
                self.table.push(FuncIndex(*func_index));
                sink.push(Expression::I32Const { value: table_index });
                Ok(())
            }
            mir::ExprKind::Bool { value } => Ok(sink.push(Expression::I32Const {
                value: if *value { 1 } else { 0 },
            })),
            mir::ExprKind::Call { callee, arguments } => {
                for argument in arguments {
                    self.build_expression(ctx, argument, sink)?;
                }

                match callee.kind {
                    mir::ExprKind::Function { func_index } => sink.push(Expression::Call {
                        function: FuncIndex(func_index),
                    }),
                    _ => {
                        let type_index = match callee.ty {
                            mir::Type::Function { signature_index } => {
                                SignatureIndex(signature_index)
                            }
                            _ => unreachable!("callee must be a function type"),
                        };
                        self.build_expression(ctx, callee, sink)?;
                        sink.push(Expression::CallIndirect {
                            table_index: TableIndex(0),
                            type_index,
                        });
                    }
                };
                Ok(())
            }
            mir::ExprKind::Int { value } => Ok(sink.push(match expr.ty {
                mir::Type::I32 | mir::Type::U32 => Expression::I32Const {
                    value: *value as i32,
                },
                mir::Type::I64 | mir::Type::U64 => Expression::I64Const { value: *value },
                _ => return Err(()),
            })),
            mir::ExprKind::Float { value } => Ok(sink.push(match expr.ty {
                mir::Type::F32 => Expression::F32Const {
                    value: *value as f32,
                },
                mir::Type::F64 => Expression::F64Const { value: *value },
                _ => return Err(()),
            })),
            mir::ExprKind::LocalGet {
                local_index,
                scope_index,
            } => Ok(sink.push(Expression::LocalGet {
                local_index: ctx.get_flat_index(*scope_index, *local_index),
            })),
            mir::ExprKind::LocalSet {
                local_index,
                scope_index,
                value,
            } => match value.ty {
                mir::Type::Tuple { .. } => match &value.kind {
                    mir::ExprKind::String {
                        scope_index,
                        local_index,
                        string_index,
                    } => {
                        let tuple_start_index = ctx.get_flat_index(*scope_index, *local_index).0;
                        let (offset, len) = self.string_pool.strings[*string_index as usize];

                        sink.push(Expression::I32Const {
                            value: offset as i32,
                        });
                        sink.push(Expression::LocalSet {
                            local_index: LocalIndex(tuple_start_index),
                        });
                        sink.push(Expression::I32Const { value: len as i32 });
                        sink.push(Expression::LocalSet {
                            local_index: LocalIndex(tuple_start_index + 1),
                        });
                        Ok(())
                    }
                    _ => unreachable!(),
                },
                _ => {
                    self.build_expression(ctx, &value, sink)?;
                    sink.push(Expression::LocalSet {
                        local_index: ctx.get_flat_index(*scope_index, *local_index),
                    });
                    Ok(())
                }
            },
            mir::ExprKind::Global { global_index } => Ok(sink.push(Expression::GlobalGet {
                global_index: GlobalIndex(*global_index),
            })),
            mir::ExprKind::GlobalSet {
                global_index,
                value,
            } => {
                self.build_expression(ctx, value, sink)?;
                sink.push(Expression::GlobalSet {
                    global: GlobalIndex(*global_index),
                });
                Ok(())
            }
            mir::ExprKind::Return { value } => {
                match value {
                    Some(value) => self.build_expression(ctx, value, sink)?,
                    None => {}
                };
                Ok(sink.push(Expression::Return))
            }
            mir::ExprKind::Drop { value } => {
                self.build_expression(ctx, value, sink)?;
                match value.ty {
                    mir::Type::Never | mir::Type::Unit => Err(()),
                    _ => Ok(sink.push(Expression::Drop)),
                }
            }
            mir::ExprKind::Block {
                expressions,
                scope_index,
            } => {
                let mut body = Vec::new();
                for expression in expressions {
                    ctx.scope_index = *scope_index;
                    self.build_expression(ctx, expression, &mut body)?;
                }
                Ok(sink.push(Expression::Block {
                    expressions: body.into_boxed_slice(),
                    result: match ValueType::try_from(expr.ty) {
                        Ok(ty) => BlockResult::SingleValue(ty),
                        _ => BlockResult::Empty,
                    },
                }))
            }
            mir::ExprKind::Loop { scope_index, block } => {
                let expressions = match &block.kind {
                    mir::ExprKind::Block { expressions, .. } => expressions,
                    _ => unreachable!(),
                };

                let mut body = Vec::new();
                for expression in expressions {
                    ctx.scope_index = *scope_index;
                    self.build_expression(ctx, expression, &mut body)?;
                }
                body.push(Expression::Break {
                    depth: 0, // Loop itself
                });

                let scope = &ctx.scopes[*scope_index as usize];
                Ok(sink.push(Expression::Block {
                    expressions: Box::new([
                        Expression::Loop {
                            expressions: body.into_boxed_slice(),
                            result: BlockResult::Empty,
                        },
                        Expression::Unreachable,
                    ]),
                    result: match ValueType::try_from(scope.result) {
                        Ok(ty) => BlockResult::SingleValue(ty),
                        _ => BlockResult::Empty,
                    },
                }))
            }
            mir::ExprKind::Continue { scope_index } => Ok(sink.push(Expression::Break {
                depth: ctx.get_continue_depth(*scope_index).unwrap(),
            })),
            mir::ExprKind::Break { value, scope_index } => {
                match value {
                    Some(value) => self.build_expression(ctx, value, sink)?,
                    None => {}
                }
                Ok(sink.push(Expression::Break {
                    depth: ctx.get_break_depth(*scope_index).unwrap(),
                }))
            }
            mir::ExprKind::Unreachable => Ok(sink.push(Expression::Unreachable)),
            mir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                self.build_expression(ctx, condition, sink)?;

                let then_branch = {
                    let mut body = Vec::with_capacity(1);
                    self.build_expression(ctx, then_block, &mut body)?;
                    Box::new(body.remove(0))
                };
                let else_branch = match else_block {
                    Some(else_block) => {
                        let mut body = Vec::with_capacity(1);
                        self.build_expression(ctx, else_block, &mut body)?;
                        Some(Box::new(body.remove(0)))
                    }
                    None => None,
                };

                Ok(sink.push(Expression::IfElse {
                    result: BlockResult::from(expr.ty),
                    then_branch,
                    else_branch,
                }))
            }
            mir::ExprKind::Neg { value } => Ok(match expr.ty {
                mir::Type::I32 => {
                    sink.push(Expression::I32Const { value: 0 });
                    self.build_expression(ctx, value, sink)?;
                    sink.push(Expression::I32Sub);
                }
                mir::Type::I64 => {
                    sink.push(Expression::I64Const { value: 0 });
                    self.build_expression(ctx, value, sink)?;
                    sink.push(Expression::I64Sub);
                }
                mir::Type::F32 => {
                    self.build_expression(ctx, value, sink)?;
                    sink.push(Expression::F32Neg);
                }
                mir::Type::F64 => {
                    self.build_expression(ctx, value, sink)?;
                    sink.push(Expression::F64Neg);
                }
                _ => return Err(()),
            }),
            mir::ExprKind::Add { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Expression::I32Add,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Add,
                    mir::Type::F32 => Expression::F32Add,
                    mir::Type::F64 => Expression::F64Add,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::Sub { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Expression::I32Sub,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Sub,
                    mir::Type::F32 => Expression::F32Sub,
                    mir::Type::F64 => Expression::F64Sub,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::Mul { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Expression::I32Mul,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Mul,
                    mir::Type::F32 => Expression::F32Mul,
                    mir::Type::F64 => Expression::F64Mul,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::Div { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 => Expression::I32DivS,
                    mir::Type::U32 => Expression::I32DivU,
                    mir::Type::I64 => Expression::I64DivS,
                    mir::Type::U64 => Expression::I64DivU,
                    mir::Type::F32 => Expression::F32Div,
                    mir::Type::F64 => Expression::F64Div,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::Rem { left, right } => {
                match expr.ty {
                    mir::Type::I32 => {
                        self.build_expression(ctx, left, sink)?;
                        self.build_expression(ctx, right, sink)?;
                        Ok(sink.push(Expression::I32RemS))
                    }
                    mir::Type::U32 => {
                        self.build_expression(ctx, left, sink)?;
                        self.build_expression(ctx, right, sink)?;
                        Ok(sink.push(Expression::I32RemU))
                    }
                    mir::Type::I64 => {
                        self.build_expression(ctx, left, sink)?;
                        self.build_expression(ctx, right, sink)?;
                        Ok(sink.push(Expression::I64RemS))
                    }
                    mir::Type::U64 => {
                        self.build_expression(ctx, left, sink)?;
                        self.build_expression(ctx, right, sink)?;
                        Ok(sink.push(Expression::I64RemU))
                    }
                    mir::Type::F32 => {
                        // left - (trunc(left / right) * right)
                        self.build_expression(ctx, left, sink)?; // left
                        self.build_expression(ctx, left, sink)?; // left (for div)
                        self.build_expression(ctx, right, sink)?; // right (for div)
                        sink.push(Expression::F32Div); // left / right
                        sink.push(Expression::F32Trunc); // trunc(left / right)
                        self.build_expression(ctx, right, sink)?; // right (for mul)
                        sink.push(Expression::F32Mul); // trunc(left / right) * right
                        sink.push(Expression::F32Sub); // left - (trunc(left / right) * right)
                        Ok(())
                    }
                    mir::Type::F64 => {
                        self.build_expression(ctx, left, sink)?;
                        self.build_expression(ctx, left, sink)?;
                        self.build_expression(ctx, right, sink)?;
                        sink.push(Expression::F64Div);
                        sink.push(Expression::F64Trunc);
                        self.build_expression(ctx, right, sink)?;
                        sink.push(Expression::F64Mul);
                        sink.push(Expression::F64Sub);
                        Ok(())
                    }
                    _ => return Err(()),
                }
            }
            mir::ExprKind::Eq { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match left.ty {
                    mir::Type::Bool
                    | mir::Type::I32
                    | mir::Type::U32
                    | mir::Type::Function { .. } => Expression::I32Eq,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Eq,
                    mir::Type::F32 => Expression::F32Eq,
                    mir::Type::F64 => Expression::F64Eq,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::NotEq { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match left.ty {
                    mir::Type::Bool
                    | mir::Type::I32
                    | mir::Type::U32
                    | mir::Type::Function { .. } => Expression::I32Ne,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Ne,
                    mir::Type::F32 => Expression::F32Ne,
                    mir::Type::F64 => Expression::F64Ne,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::And { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match left.ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => Expression::I32And,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64And,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::Or { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match left.ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => Expression::I32Or,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Or,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::BitAnd { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Expression::I32And,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64And,
                    _ => return Err(()),
                }))
            }

            mir::ExprKind::BitOr { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Expression::I32Or,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Or,
                    _ => return Err(()),
                }))
            }

            mir::ExprKind::BitXor { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Expression::I32Xor,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Xor,
                    _ => return Err(()),
                }))
            }

            mir::ExprKind::LeftShift { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Expression::I32Shl,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Shl,
                    _ => return Err(()),
                }))
            }

            mir::ExprKind::RightShift { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match expr.ty {
                    mir::Type::I32 => Expression::I32ShrS,
                    mir::Type::U32 => Expression::I32ShrU,
                    mir::Type::I64 => Expression::I64ShrS,
                    mir::Type::U64 => Expression::I64ShrU,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::Less { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match left.ty {
                    mir::Type::I32 => Expression::I32LtS,
                    mir::Type::U32 => Expression::I32LtU,
                    mir::Type::I64 => Expression::I64LtS,
                    mir::Type::U64 => Expression::I64LtU,
                    mir::Type::F32 => Expression::F32Lt,
                    mir::Type::F64 => Expression::F64Lt,
                    _ => return Err(()),
                }))
            }

            mir::ExprKind::LessEq { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match left.ty {
                    mir::Type::I32 => Expression::I32LeS,
                    mir::Type::U32 => Expression::I32LeU,
                    mir::Type::I64 => Expression::I64LeS,
                    mir::Type::U64 => Expression::I64LeU,
                    mir::Type::F32 => Expression::F32Le,
                    mir::Type::F64 => Expression::F64Le,
                    _ => return Err(()),
                }))
            }

            mir::ExprKind::Greater { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match left.ty {
                    mir::Type::I32 => Expression::I32GtS,
                    mir::Type::U32 => Expression::I32GtU,
                    mir::Type::I64 => Expression::I64GtS,
                    mir::Type::U64 => Expression::I64GtU,
                    mir::Type::F32 => Expression::F32Gt,
                    mir::Type::F64 => Expression::F64Gt,
                    _ => return Err(()),
                }))
            }

            mir::ExprKind::GreaterEq { left, right } => {
                self.build_expression(ctx, left, sink)?;
                self.build_expression(ctx, right, sink)?;
                Ok(sink.push(match left.ty {
                    mir::Type::I32 => Expression::I32GeS,
                    mir::Type::U32 => Expression::I32GeU,
                    mir::Type::I64 => Expression::I64GeS,
                    mir::Type::U64 => Expression::I64GeU,
                    mir::Type::F32 => Expression::F32Ge,
                    mir::Type::F64 => Expression::F64Ge,
                    _ => return Err(()),
                }))
            }
            mir::ExprKind::BitNot { value } => {
                self.build_expression(ctx, value, sink)?;
                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => {
                        sink.push(Expression::I32Const { value: !0 });
                        sink.push(Expression::I32Xor);
                    }
                    mir::Type::I64 | mir::Type::U64 => {
                        sink.push(Expression::I64Const { value: !0 });
                        sink.push(Expression::I64Xor);
                    }
                    _ => return Err(()),
                }
                Ok(())
            }

            mir::ExprKind::Eqz { value } => {
                self.build_expression(ctx, value, sink)?;
                sink.push(match expr.ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => Expression::I32Eqz,
                    mir::Type::I64 | mir::Type::U64 => Expression::I64Eqz,
                    _ => return Err(()),
                });
                Ok(())
            }
            mir::ExprKind::Noop => Ok(()),
            mir::ExprKind::String {
                scope_index,
                local_index,
                ..
            } => {
                let start_tuple_index = ctx.get_flat_index(*scope_index, *local_index).0;

                sink.push(Expression::LocalGet {
                    local_index: LocalIndex(start_tuple_index),
                });
                sink.push(Expression::LocalGet {
                    local_index: LocalIndex(start_tuple_index + 1),
                });
                Ok(())
            }
            mir::ExprKind::LocalTupleGet {
                scope_index,
                local_index,
                field_index,
            } => {
                let local_index = ctx.get_flat_index(*scope_index, *local_index).0 + *field_index;
                Ok(sink.push(Expression::LocalGet {
                    local_index: LocalIndex(local_index),
                }))
            }
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
            BlockResult::Empty => {
                sink.push(0x40);
            }
            BlockResult::SingleValue(ty) => {
                ty.encode(sink);
            }
        }
    }
}

trait ContextEncode {
    fn encode(&self, sink: &mut Vec<u8>, module: &WasmModule);
}

impl ContextEncode for Expression {
    fn encode(&self, sink: &mut Vec<u8>, module: &WasmModule) {
        match self {
            Expression::Nop => {
                // sink.push(Instruction::Nop as u8);
            }
            Expression::LocalGet { local_index: local } => {
                sink.push(Instruction::LocalGet as u8);
                local.0.encode(sink);
            }
            Expression::LocalSet { local_index: local } => {
                sink.push(Instruction::LocalSet as u8);
                local.0.encode(sink);
            }
            Expression::GlobalGet {
                global_index: global,
            } => {
                sink.push(Instruction::GlobalGet as u8);
                global.0.encode(sink);
            }
            Expression::GlobalSet { global } => {
                sink.push(Instruction::GlobalSet as u8);
                global.0.encode(sink);
            }
            Expression::Return => {
                sink.push(Instruction::Return as u8);
            }
            Expression::I32Add => {
                sink.push(Instruction::I32Add as u8);
            }
            Expression::I32Sub => {
                sink.push(Instruction::I32Sub as u8);
            }
            Expression::I32Mul => {
                sink.push(Instruction::I32Mul as u8);
            }
            Expression::Block {
                expressions,
                result,
            } => {
                sink.push(Instruction::Block as u8);
                result.encode(sink);
                for expr in expressions.iter() {
                    expr.encode(sink, module);
                }
                sink.push(Instruction::End as u8);
            }
            Expression::Loop {
                expressions,
                result,
            } => {
                sink.push(Instruction::Loop as u8);
                result.encode(sink);
                for expr in expressions.iter() {
                    expr.encode(sink, module);
                }
                sink.push(Instruction::End as u8);
            }
            Expression::Unreachable => {
                sink.push(Instruction::Unreachable as u8);
            }
            Expression::Break { depth } => {
                sink.push(Instruction::Br as u8);
                depth.encode(sink);
            }
            Expression::Call { function } => {
                sink.push(Instruction::Call as u8);
                function.0.encode(sink);
            }
            Expression::CallIndirect {
                type_index,
                table_index,
            } => {
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
            Expression::I32Eq => {
                sink.push(Instruction::I32Eq as u8);
            }
            Expression::I32Eqz => {
                sink.push(Instruction::I32Eqz as u8);
            }
            Expression::I32Ne => {
                sink.push(Instruction::I32Ne as u8);
            }
            Expression::I32And => {
                sink.push(Instruction::I32And as u8);
            }
            Expression::I32Or => {
                sink.push(Instruction::I32Or as u8);
            }
            Expression::IfElse {
                result,
                then_branch,
                else_branch,
            } => {
                sink.push(Instruction::If as u8);
                result.encode(sink);
                then_branch.encode(sink, module);
                match else_branch {
                    Some(else_branch) => {
                        sink.push(Instruction::Else as u8);
                        else_branch.encode(sink, module);
                    }
                    None => {}
                }
                sink.push(Instruction::End as u8);
            }
            Expression::Drop => {
                sink.push(Instruction::Drop as u8);
            }
            Expression::I64Add => {
                sink.push(Instruction::I64Add as u8);
            }
            Expression::I64Sub => {
                sink.push(Instruction::I64Sub as u8);
            }
            Expression::I64Mul => {
                sink.push(Instruction::I64Mul as u8);
            }
            Expression::I64Eq => {
                sink.push(Instruction::I64Eq as u8);
            }
            Expression::I64Eqz => {
                sink.push(Instruction::I64Eqz as u8);
            }
            Expression::I64Ne => {
                sink.push(Instruction::I64Ne as u8);
            }
            Expression::I64And => {
                sink.push(Instruction::I64And as u8);
            }
            Expression::I64Or => {
                sink.push(Instruction::I64Or as u8);
            }
            Expression::I32DivS => {
                sink.push(Instruction::I32DivS as u8);
            }
            Expression::I32GeS => {
                sink.push(Instruction::I32GeS as u8);
            }
            Expression::I32ShrS => {
                sink.push(Instruction::I32ShrS as u8);
            }
            Expression::I32LtS => {
                sink.push(Instruction::I32LtS as u8);
            }
            Expression::I32GtS => {
                sink.push(Instruction::I32GtS as u8);
            }
            Expression::I32LeS => {
                sink.push(Instruction::I32LeS as u8);
            }
            Expression::I32RemS => {
                sink.push(Instruction::I32RemS as u8);
            }
            Expression::I64DivS => {
                sink.push(Instruction::I64DivS as u8);
            }
            Expression::I64GeS => {
                sink.push(Instruction::I64GeS as u8);
            }
            Expression::I64ShrS => {
                sink.push(Instruction::I64ShrS as u8);
            }
            Expression::I64LtS => {
                sink.push(Instruction::I64LtS as u8);
            }
            Expression::I64GtS => {
                sink.push(Instruction::I64GtS as u8);
            }
            Expression::I64LeS => {
                sink.push(Instruction::I64LeS as u8);
            }
            Expression::I64RemS => {
                sink.push(Instruction::I64RemS as u8);
            }
            Expression::I32Xor => {
                sink.push(Instruction::I32Xor as u8);
            }
            Expression::I64Xor => {
                sink.push(Instruction::I64Xor as u8);
            }
            Expression::I32Shl => {
                sink.push(Instruction::I32Shl as u8);
            }
            Expression::I64Shl => {
                sink.push(Instruction::I64Shl as u8);
            }
            Expression::F32Add => {
                sink.push(Instruction::F32Add as u8);
            }
            Expression::F32Sub => {
                sink.push(Instruction::F32Sub as u8);
            }
            Expression::F32Mul => {
                sink.push(Instruction::F32Mul as u8);
            }
            Expression::F64Add => {
                sink.push(Instruction::F64Add as u8);
            }
            Expression::F64Sub => {
                sink.push(Instruction::F64Sub as u8);
            }
            Expression::F64Mul => {
                sink.push(Instruction::F64Mul as u8);
            }
            Expression::F32Eq => {
                sink.push(Instruction::F32Eq as u8);
            }
            Expression::F32Ne => {
                sink.push(Instruction::F32Ne as u8);
            }
            Expression::F64Eq => {
                sink.push(Instruction::F64Eq as u8);
            }
            Expression::F64Ne => {
                sink.push(Instruction::F64Ne as u8);
            }
            Expression::F32Lt => {
                sink.push(Instruction::F32Lt as u8);
            }
            Expression::F32Gt => {
                sink.push(Instruction::F32Gt as u8);
            }
            Expression::F32Le => {
                sink.push(Instruction::F32Le as u8);
            }
            Expression::F32Ge => {
                sink.push(Instruction::F32Ge as u8);
            }
            Expression::F64Lt => {
                sink.push(Instruction::F64Lt as u8);
            }
            Expression::F64Gt => {
                sink.push(Instruction::F64Gt as u8);
            }
            Expression::F64Le => {
                sink.push(Instruction::F64Le as u8);
            }
            Expression::F64Ge => {
                sink.push(Instruction::F64Ge as u8);
            }
            Expression::F32Div => {
                sink.push(Instruction::F32Div as u8);
            }
            Expression::F64Div => {
                sink.push(Instruction::F64Div as u8);
            }
            Expression::F32Neg => {
                sink.push(Instruction::F32Neg as u8);
            }
            Expression::F64Neg => {
                sink.push(Instruction::F64Neg as u8);
            }
            Expression::I32DivU => {
                sink.push(Instruction::I32DivU as u8);
            }
            Expression::I32GeU => {
                sink.push(Instruction::I32GeU as u8);
            }
            Expression::I32ShrU => {
                sink.push(Instruction::I32ShrU as u8);
            }
            Expression::I32LtU => {
                sink.push(Instruction::I32LtU as u8);
            }
            Expression::I32GtU => {
                sink.push(Instruction::I32GtU as u8);
            }
            Expression::I32LeU => {
                sink.push(Instruction::I32LeU as u8);
            }
            Expression::I32RemU => {
                sink.push(Instruction::I32RemU as u8);
            }
            Expression::I64DivU => {
                sink.push(Instruction::I64DivU as u8);
            }
            Expression::I64GeU => {
                sink.push(Instruction::I64GeU as u8);
            }
            Expression::I64ShrU => {
                sink.push(Instruction::I64ShrU as u8);
            }
            Expression::I64LtU => {
                sink.push(Instruction::I64LtU as u8);
            }
            Expression::I64GtU => {
                sink.push(Instruction::I64GtU as u8);
            }
            Expression::I64LeU => {
                sink.push(Instruction::I64LeU as u8);
            }
            Expression::I64RemU => {
                sink.push(Instruction::I64RemU as u8);
            }
            Expression::F32Trunc => {
                sink.push(Instruction::F32Trunc as u8);
            }
            Expression::F64Trunc => {
                sink.push(Instruction::F64Trunc as u8);
            }
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
    // Table = 0x01,
    // Memory = 0x02,
    Global = 0x03,
}

impl ContextEncode for ExportItem {
    fn encode(&self, sink: &mut Vec<u8>, _module: &WasmModule) {
        match self.clone() {
            ExportItem::Function { name, func_index } => {
                let name_len = name.len() as u32;
                name_len.encode(sink);
                sink.extend_from_slice(name.as_bytes());
                sink.push(ExportKind::Function as u8);
                func_index.0.encode(sink);
            }
            ExportItem::Global { name, global_index } => {
                let name_len = name.len() as u32;
                name_len.encode(sink);
                sink.extend_from_slice(name.as_bytes());
                sink.push(ExportKind::Global as u8);
                global_index.0.encode(sink);
            }
        }
    }
}

impl ContextEncode for ExportSection {
    fn encode(&self, sink: &mut Vec<u8>, module: &WasmModule) {
        sink.push(SectionId::Export as u8);

        let mut section_sink: Vec<u8> = Vec::new();
        let export_count = self.items.len() as u32;
        export_count.encode(&mut section_sink);
        for item in &self.items {
            item.encode(&mut section_sink, module);
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

        for expr in self.expressions.iter() {
            expr.encode(&mut body_content, module);
        }
        body_content.push(Instruction::End as u8);

        let body_size = body_content.len() as u32;
        body_size.encode(sink);
        sink.extend_from_slice(&body_content);
    }
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
        self.functions.encode(&mut sink);
        match self.tables.tables.len() {
            0 => {}
            _ => self.tables.encode(&mut sink),
        }
        self.globals.encode(&mut sink);
        self.exports.encode(&mut sink, self);
        match self.elements.segments.len() {
            0 => {}
            _ => self.elements.encode(&mut sink),
        }
        self.code.encode(&mut sink, self);

        sink
    }
}

#[cfg(test)]
mod tests {
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
    use codespan_reporting::term::{self};
    use indoc::indoc;

    use super::*;
    use crate::{ast, mir, tir};

    #[allow(unused)]
    struct TestCase {
        interner: ast::StringInterner,
        files: ast::Files,
        ast: ast::AST,
        tir: tir::TIR,
        mir: mir::MIR,
        wasm: WasmModule,
        bytecode: Vec<u8>,
    }

    impl<'case> TestCase {
        fn new(source: &str) -> Self {
            let mut interner = ast::StringInterner::new();
            let mut files = ast::Files::new();
            let file_id = files
                .add("main.wx".to_string(), source.to_string())
                .unwrap();
            let ast =
                ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
            if ast.diagnostics.len() > 0 {
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                for diagnostic in ast.diagnostics.iter() {
                    term::emit_to_io_write(&mut writer.lock(), &config, &files, diagnostic)
                        .unwrap();
                }
                std::process::exit(1);
            }
            let tir = tir::TIR::build(&ast, &mut interner);
            if tir.diagnostics.len() > 0 {
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                for diagnostic in tir.diagnostics.iter() {
                    term::emit_to_io_write(&mut writer.lock(), &config, &files, diagnostic)
                        .unwrap();
                }
                std::process::exit(1);
            }
            let mir = mir::MIR::build(&tir, &interner);
            let wasm = Builder::build(&mir, &interner).unwrap();
            let bytecode = wasm.encode();

            TestCase {
                interner,
                files,
                ast,
                tir,
                mir,
                wasm,
                bytecode,
            }
        }
    }

    #[test]
    fn test_parse_simple_addition() {
        let case = TestCase::new(indoc! {"
            fn add(mut a: i32, b: i32) -> i32 { a += b; a }

            export { add }
        "});
        // insta::assert_yaml_snapshot!(case.bytecode);

        // Execute the wasm bytecode using wasmtime to verify it works
        let engine = wasmtime::Engine::default();
        let module =
            wasmtime::Module::new(&engine, &case.bytecode).expect("Failed to create module");
        let mut store = wasmtime::Store::new(&engine, ());
        let instance =
            wasmtime::Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

        let add = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "add")
            .expect("Failed to get add function");

        // Test: 5 + 3 = 8
        let result = add
            .call(&mut store, (5, 3))
            .expect("Failed to call add function");
        assert_eq!(result, 8, "add(5, 3) should return 8");

        // Test: 10 + 20 = 30
        let result = add
            .call(&mut store, (10, 20))
            .expect("Failed to call add function");
        assert_eq!(result, 30, "add(10, 20) should return 30");

        // Test: -5 + 3 = -2
        let result = add
            .call(&mut store, (-5, 3))
            .expect("Failed to call add function");
        assert_eq!(result, -2, "add(-5, 3) should return -2");
    }

    #[test]
    fn test_arithmetic_operations() {
        let case = TestCase::new(indoc! {"
            fn sub(a: i32, b: i32) -> i32 { a - b }
            fn mul(a: i32, b: i32) -> i32 { a * b }
            fn div(a: i32, b: i32) -> i32 { a / b }
            fn rem(a: i32, b: i32) -> i32 { a % b }

            export {
                sub,
                mul,
                div,
                rem
            }
        "});

        println!("{}", wasmprinter::print_bytes(&case.bytecode).unwrap());

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let sub = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "sub")
            .unwrap();
        assert_eq!(sub.call(&mut store, (10, 3)).unwrap(), 7);
        assert_eq!(sub.call(&mut store, (5, 10)).unwrap(), -5);

        let mul = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "mul")
            .unwrap();
        assert_eq!(mul.call(&mut store, (6, 7)).unwrap(), 42);
        assert_eq!(mul.call(&mut store, (-3, 4)).unwrap(), -12);

        let div = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "div")
            .unwrap();
        assert_eq!(div.call(&mut store, (20, 4)).unwrap(), 5);
        assert_eq!(div.call(&mut store, (15, 4)).unwrap(), 3);

        let rem = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "rem")
            .unwrap();
        assert_eq!(rem.call(&mut store, (10, 3)).unwrap(), 1);
        assert_eq!(rem.call(&mut store, (20, 7)).unwrap(), 6);
    }

    #[test]
    fn test_comparison_operations() {
        let case = TestCase::new(indoc! {"
            fn lt(a: i32, b: i32) -> i32 { 
                if a < b { 1 } else { 0 }
            }
            fn gt(a: i32, b: i32) -> i32 { 
                if a > b { 1 } else { 0 }
            }
            fn eq(a: i32, b: i32) -> i32 { 
                if a == b { 1 } else { 0 }
            }
            fn ne(a: i32, b: i32) -> i32 { 
                if a != b { 1 } else { 0 }
            }

            export {
                lt,
                gt,
                eq,
                ne
            }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let lt = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "lt")
            .unwrap();
        assert_eq!(lt.call(&mut store, (5, 10)).unwrap(), 1);
        assert_eq!(lt.call(&mut store, (10, 5)).unwrap(), 0);

        let gt = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "gt")
            .unwrap();
        assert_eq!(gt.call(&mut store, (10, 5)).unwrap(), 1);
        assert_eq!(gt.call(&mut store, (5, 10)).unwrap(), 0);

        let eq = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "eq")
            .unwrap();
        assert_eq!(eq.call(&mut store, (5, 5)).unwrap(), 1);
        assert_eq!(eq.call(&mut store, (5, 10)).unwrap(), 0);

        let ne = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "ne")
            .unwrap();
        assert_eq!(ne.call(&mut store, (5, 10)).unwrap(), 1);
        assert_eq!(ne.call(&mut store, (5, 5)).unwrap(), 0);
    }

    #[test]
    fn test_conditional_expression() {
        let case = TestCase::new(indoc! {"
            fn max(a: i32, b: i32) -> i32 {
                if a > b { a } else { b }
            }
            fn abs(a: i32) -> i32 {
                if a < 0 { -a } else { a }
            }

            export {
                max,
                abs
            }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let max = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "max")
            .unwrap();
        assert_eq!(max.call(&mut store, (5, 10)).unwrap(), 10);
        assert_eq!(max.call(&mut store, (10, 5)).unwrap(), 10);
        assert_eq!(max.call(&mut store, (7, 7)).unwrap(), 7);

        let abs = instance
            .get_typed_func::<i32, i32>(&mut store, "abs")
            .unwrap();
        assert_eq!(abs.call(&mut store, 5).unwrap(), 5);
        assert_eq!(abs.call(&mut store, -5).unwrap(), 5);
        assert_eq!(abs.call(&mut store, 0).unwrap(), 0);
    }

    #[test]
    fn test_loops() {
        let case = TestCase::new(indoc! {"
            fn factorial(n: i32) -> i32 {
                local mut result: i32 = 1;
                local mut i: i32 = 1;
                loop {
                    if i > n { break result };
                    result *= i;
                    i += 1;
                }
            }
            fn sum_to_n(n: i32) -> i32 {
                local mut sum: i32 = 0;
                local mut i: i32 = 1;
                loop {
                    if i > n { break };
                    sum += i;
                    i += 1;
                };
                sum
            }

            export {
                factorial,
                sum_to_n
            }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let factorial = instance
            .get_typed_func::<i32, i32>(&mut store, "factorial")
            .unwrap();
        assert_eq!(factorial.call(&mut store, 5).unwrap(), 120);
        assert_eq!(factorial.call(&mut store, 6).unwrap(), 720);
        assert_eq!(factorial.call(&mut store, 0).unwrap(), 1);

        let sum_to_n = instance
            .get_typed_func::<i32, i32>(&mut store, "sum_to_n")
            .unwrap();
        assert_eq!(sum_to_n.call(&mut store, 10).unwrap(), 55);
        assert_eq!(sum_to_n.call(&mut store, 100).unwrap(), 5050);
    }

    #[test]
    fn test_i64_operations() {
        let case = TestCase::new(indoc! {"
            fn add64(a: i64, b: i64) -> i64 { a + b }
            fn mul64(a: i64, b: i64) -> i64 { a * b }

            export {
                add64,
                mul64
            }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let add64 = instance
            .get_typed_func::<(i64, i64), i64>(&mut store, "add64")
            .unwrap();
        assert_eq!(
            add64.call(&mut store, (1000000000, 2000000000)).unwrap(),
            3000000000
        );
        assert_eq!(add64.call(&mut store, (-500, 1000)).unwrap(), 500);

        let mul64 = instance
            .get_typed_func::<(i64, i64), i64>(&mut store, "mul64")
            .unwrap();
        assert_eq!(
            mul64.call(&mut store, (1000000, 1000000)).unwrap(),
            1000000000000
        );
    }

    #[test]
    fn test_f32_operations() {
        let case = TestCase::new(indoc! {"
            fn add_f32(a: f32, b: f32) -> f32 { a + b }
            fn mul_f32(a: f32, b: f32) -> f32 { a * b }
            fn div_f32(a: f32, b: f32) -> f32 { a / b }

            export {
                add_f32,
                mul_f32,
                div_f32
            }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let add_f32 = instance
            .get_typed_func::<(f32, f32), f32>(&mut store, "add_f32")
            .unwrap();
        assert!((add_f32.call(&mut store, (1.5, 2.5)).unwrap() - 4.0).abs() < 0.001);

        let mul_f32 = instance
            .get_typed_func::<(f32, f32), f32>(&mut store, "mul_f32")
            .unwrap();
        assert!((mul_f32.call(&mut store, (2.5, 4.0)).unwrap() - 10.0).abs() < 0.001);

        let div_f32 = instance
            .get_typed_func::<(f32, f32), f32>(&mut store, "div_f32")
            .unwrap();
        assert!((div_f32.call(&mut store, (10.0, 4.0)).unwrap() - 2.5).abs() < 0.001);
    }

    #[test]
    fn test_f64_operations() {
        let case = TestCase::new(indoc! {"
            fn add_f64(a: f64, b: f64) -> f64 { a + b }
            fn sub_f64(a: f64, b: f64) -> f64 { a - b }

            export {
                add_f64,
                sub_f64
            }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let add_f64 = instance
            .get_typed_func::<(f64, f64), f64>(&mut store, "add_f64")
            .unwrap();
        assert!((add_f64.call(&mut store, (1.5, 2.5)).unwrap() - 4.0).abs() < 0.0001);

        let sub_f64 = instance
            .get_typed_func::<(f64, f64), f64>(&mut store, "sub_f64")
            .unwrap();
        assert!((sub_f64.call(&mut store, (10.5, 3.5)).unwrap() - 7.0).abs() < 0.0001);
    }

    #[test]
    fn test_bitwise_operations() {
        let case = TestCase::new(indoc! {"
            fn bit_and(a: i32, b: i32) -> i32 { a & b }
            fn bit_or(a: i32, b: i32) -> i32 { a | b }
            fn bit_xor(a: i32, b: i32) -> i32 { a ^ b }
            fn left_shift(a: i32, b: i32) -> i32 { a << b }
            fn right_shift(a: i32, b: i32) -> i32 { a >> b }

            export {
                bit_and,
                bit_or,
                bit_xor,
                left_shift,
                right_shift
            }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let bit_and = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "bit_and")
            .unwrap();
        assert_eq!(bit_and.call(&mut store, (0b1100, 0b1010)).unwrap(), 0b1000);

        let bit_or = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "bit_or")
            .unwrap();
        assert_eq!(bit_or.call(&mut store, (0b1100, 0b1010)).unwrap(), 0b1110);

        let bit_xor = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "bit_xor")
            .unwrap();
        assert_eq!(bit_xor.call(&mut store, (0b1100, 0b1010)).unwrap(), 0b0110);

        let left_shift = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "left_shift")
            .unwrap();
        assert_eq!(left_shift.call(&mut store, (5, 2)).unwrap(), 20);

        let right_shift = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "right_shift")
            .unwrap();
        assert_eq!(right_shift.call(&mut store, (20, 2)).unwrap(), 5);
    }

    #[test]
    fn test_logical_operations() {
        let case = TestCase::new(indoc! {"
            fn and(a: i32, b: i32) -> i32 { ((a != 0) && (b != 0)) as i32 }
            fn or(a: i32, b: i32) -> i32 { ((a != 0) || (b != 0)) as i32 }

            export { and, or }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let and = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "and")
            .unwrap();
        assert_eq!(and.call(&mut store, (1, 1)).unwrap(), 1);
        assert_eq!(and.call(&mut store, (1, 0)).unwrap(), 0);
        assert_eq!(and.call(&mut store, (0, 1)).unwrap(), 0);
        assert_eq!(and.call(&mut store, (0, 0)).unwrap(), 0);

        let or = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "or")
            .unwrap();
        assert_eq!(or.call(&mut store, (1, 1)).unwrap(), 1);
        assert_eq!(or.call(&mut store, (1, 0)).unwrap(), 1);
        assert_eq!(or.call(&mut store, (0, 1)).unwrap(), 1);
        assert_eq!(or.call(&mut store, (0, 0)).unwrap(), 0);
    }

    #[test]
    fn test_global_variables() {
        let case = TestCase::new(indoc! {"
            global mut global_counter: i32 = 0
            
            fn increment() -> i32 {
                global_counter += 1;
                global_counter
            }
            
            fn get_counter() -> i32 {
                global_counter
            }

            export {
                increment,
                get_counter
            }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let increment = instance
            .get_typed_func::<(), i32>(&mut store, "increment")
            .unwrap();
        let get_counter = instance
            .get_typed_func::<(), i32>(&mut store, "get_counter")
            .unwrap();

        assert_eq!(get_counter.call(&mut store, ()).unwrap(), 0);
        assert_eq!(increment.call(&mut store, ()).unwrap(), 1);
        assert_eq!(increment.call(&mut store, ()).unwrap(), 2);
        assert_eq!(get_counter.call(&mut store, ()).unwrap(), 2);
        assert_eq!(increment.call(&mut store, ()).unwrap(), 3);
        assert_eq!(get_counter.call(&mut store, ()).unwrap(), 3);
    }

    #[test]
    fn test_fibonacci() {
        let case = TestCase::new(indoc! {"
            fn fibonacci(n: i32) -> i32 {
                if n <= 1 { return n };
                local mut a: i32 = 0;
                local mut b: i32 = 1;
                local mut i: i32 = 2;
                loop {
                    if i > n { break };
                    local temp = a + b;
                    a = b;
                    b = temp;
                    i += 1;
                };
                b
            }

            export { fibonacci }
        "});

        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut store = wasmtime::Store::new(&engine, ());
        let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

        let fibonacci = instance
            .get_typed_func::<i32, i32>(&mut store, "fibonacci")
            .unwrap();
        assert_eq!(fibonacci.call(&mut store, 0).unwrap(), 0);
        assert_eq!(fibonacci.call(&mut store, 1).unwrap(), 1);
        assert_eq!(fibonacci.call(&mut store, 2).unwrap(), 1);
        assert_eq!(fibonacci.call(&mut store, 3).unwrap(), 2);
        assert_eq!(fibonacci.call(&mut store, 4).unwrap(), 3);
        assert_eq!(fibonacci.call(&mut store, 5).unwrap(), 5);
        assert_eq!(fibonacci.call(&mut store, 10).unwrap(), 55);
    }

    #[test]
    fn test_imports() {
        let case = TestCase::new(indoc! {"
            import \"env\" as env {
                fn print_i32(value: i32) -> unit;
            }

            fn main() -> unit {
                env::print_i32(42);
            }

            export { main }
        "});

        // The compiled module should have the import section
        // insta::assert_yaml_snapshot!(case.wasm);

        // Execute the wasm bytecode using wasmtime to verify it works
        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
        let mut linker = wasmtime::Linker::new(&engine);

        // Provide the imported print_i32 function
        linker
            .func_wrap("env", "print_i32", |value: i32| {
                println!("print_i32 called with: {}", value);
            })
            .unwrap();

        let mut store = wasmtime::Store::new(&engine, ());
        let instance = linker.instantiate(&mut store, &module).unwrap();

        let main = instance
            .get_typed_func::<(), ()>(&mut store, "main")
            .unwrap();

        // Call main, which should call print_i32(42)
        main.call(&mut store, ()).unwrap();
    }
}
