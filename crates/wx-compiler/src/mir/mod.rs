/// the role of MIR is to desugar the syntax like x += 1 into x = x + 1 and
/// lower the concepts like enums into primitive constants, convert labels from
/// symbols in interner into numeric indices
use std::collections::HashMap;

use string_interner::symbol::SymbolU32;

use crate::{ast, tir};

pub type LocalIndex = u32;
pub type ScopeIndex = u32;
pub type GlobalIndex = u32;
pub type SignatureIndex = u32;
pub type FunctionIndex = u32;
pub type AggregateIndex = u32;
pub type StringIndex = u32;

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Debug, Clone)]
pub enum ExprKind {
    Noop,
    Bool {
        value: bool,
    },
    Function {
        func_index: FunctionIndex,
    },
    Int {
        value: i64,
    },
    Float {
        value: f64,
    },
    LocalGet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    LocalSet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    StringIndex {
        string_index: StringIndex,
    },
    Aggregate {
        values: Box<[Expression]>,
    },
    AggregateGet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value_index: u32,
    },
    Global {
        global_index: GlobalIndex,
    },
    GlobalSet {
        global_index: GlobalIndex,
        value: Box<Expression>,
    },
    Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Mul {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Div {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Rem {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    And {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Or {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    Drop {
        value: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Eqz {
        value: Box<Expression>,
    },
    NotEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Block {
        scope_index: ScopeIndex,
        expressions: Box<[Expression]>,
    },
    Break {
        scope_index: ScopeIndex,
        value: Option<Box<Expression>>,
    },
    Continue {
        scope_index: ScopeIndex,
    },
    Unreachable,
    IfElse {
        condition: Box<Expression>,
        then_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
    BitAnd {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitOr {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitXor {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitNot {
        value: Box<Expression>,
    },
    LeftShift {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    RightShift {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Less {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    LessEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Greater {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    GreaterEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Loop {
        scope_index: ScopeIndex,
        block: Box<Expression>,
    },
    Neg {
        value: Box<Expression>,
    },
    /// `i32.const <data_section_end>` — byte offset of the first writable memory region.
    MemoryOffset {
        memory_index: u32,
    },
    /// `memory.size <memory_index>` — current size of a linear memory in pages.
    MemorySize {
        memory_index: u32,
    },
    /// `memory.grow <memory_index>` — grow linear memory by N pages; pushes old size or -1.
    MemoryGrow {
        memory_index: u32,
        delta: Box<Expression>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    U32,
    U64,
    U8,
    I8,
    U16,
    I16,
    Unit,
    Never,
    Bool,
    Pointer,
    Aggregate { aggregate_index: AggregateIndex },
    Function { signature_index: SignatureIndex },
}

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Aggregate {
    pub values: Box<[Type]>,
}

impl std::hash::Hash for Aggregate {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for field in self.values.iter() {
            field.hash(state);
        }
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct MIR {
    pub functions: Vec<Function>,
    pub signatures: Vec<FunctionSignature>,
    pub globals: Vec<Global>,
    pub exports: Vec<ExportItem>,
    pub imports: Vec<ImportModule>,
    pub memories: Vec<tir::MemoryKind>,
    pub aggregates: Box<[Aggregate]>,
    pub strings: Box<[SymbolU32]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportModule {
    pub name: String,
    pub items: Vec<ImportModuleItem>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImportModuleItem {
    Function {
        name: SymbolU32,
        func_index: FunctionIndex,
        signature_index: SignatureIndex,
    },
    Global {
        name: SymbolU32,
        global_index: GlobalIndex,
    },
    Memory {
        name: SymbolU32,
        memory_index: u32,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ExportItem {
    Function {
        func_index: FunctionIndex,
        name: SymbolU32,
    },
    Global {
        global_index: GlobalIndex,
        name: SymbolU32,
    },
    Memory {
        memory_index: u32,
        name: SymbolU32,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, Copy)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone)]
pub struct Local {
    pub ty: Type,
    pub mutability: Mutability,
}

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone)]
pub struct BlockScope {
    pub kind: tir::BlockKind,
    pub parent: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub result: Type,
}

#[derive(Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSignature {
    pub items: Box<[Type]>,
    pub params_count: usize,
}

impl FunctionSignature {
    pub fn params(&self) -> &[Type] {
        &self.items[..self.params_count]
    }

    pub fn result(&self) -> Type {
        self.items[self.params_count]
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone)]
pub struct Function {
    pub signature_index: SignatureIndex,
    pub scopes: Vec<BlockScope>,
    pub block: Expression,
    pub inline: bool,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
    pub ty: Type,
    pub mutability: Mutability,
    pub value: Expression,
}

struct AggregatePool {
    lookup: HashMap<Aggregate, AggregateIndex>,
    aggregates: Vec<Aggregate>,
}

impl AggregatePool {
    fn new() -> Self {
        AggregatePool {
            lookup: HashMap::new(),
            aggregates: Vec::new(),
        }
    }

    fn add(&mut self, aggregate: Aggregate) -> AggregateIndex {
        if let Some(&index) = self.lookup.get(&aggregate) {
            index
        } else {
            let index = self.lookup.len() as AggregateIndex;
            self.lookup.insert(aggregate.clone(), index);
            self.aggregates.push(aggregate);
            index
        }
    }

    #[allow(dead_code)]
    fn get(&self, index: AggregateIndex) -> Option<&Aggregate> {
        self.aggregates.get(index as usize)
    }
}

fn build_index_remap<T>(
    import_count: u32,
    items: &[T],
    get_source: fn(&T) -> tir::ItemSource,
) -> Box<[u32]> {
    build_index_remap_filtered(import_count, items, get_source, |_| true)
}

fn build_index_remap_filtered<T>(
    import_count: u32,
    items: &[T],
    get_source: fn(&T) -> tir::ItemSource,
    is_live: impl Fn(usize) -> bool,
) -> Box<[u32]> {
    let mut tir_to_mir = vec![u32::MAX; items.len()];
    let mut next_imported = 0u32;
    let mut next_defined = import_count;

    for (tir_index, item) in items.iter().enumerate() {
        tir_to_mir[tir_index] = match get_source(item) {
            tir::ItemSource::Imported => {
                let i = next_imported;
                next_imported += 1;
                i
            }
            tir::ItemSource::Defined if is_live(tir_index) => {
                let i = next_defined;
                next_defined += 1;
                i
            }
            // Dead defined items get no MIR slot; sentinel value is never accessed.
            tir::ItemSource::Defined => u32::MAX,
            // Built-in items (compiler intrinsics) get no MIR slot.
            tir::ItemSource::Intrinsic => u32::MAX,
        };
    }

    tir_to_mir.into_boxed_slice()
}

/// A function is live if it has at least one access recorded in TIR.
/// TIR pushes an access whenever a function is called, referenced, or exported,
/// so `accesses.is_empty()` reliably identifies dead code.
fn is_function_live(decl: &tir::DeclaredFunction) -> bool {
    !decl.accesses.is_empty()
}

impl MIR {
    pub fn build(tir: &tir::TIR, interner: &ast::StringInterner) -> MIR {
        let import_count = tir
            .declared_functions
            .iter()
            .filter(|f| f.source == tir::ItemSource::Imported)
            .count() as u32;
        let func_index_remap = build_index_remap_filtered(
            import_count,
            &tir.declared_functions,
            |f| f.source,
            |tir_index| is_function_live(&tir.declared_functions[tir_index]),
        );
        let global_index_remap = build_index_remap(
            (tir.declared_globals.len() - tir.defined_globals.len()) as u32,
            &tir.declared_globals,
            |g| g.source,
        );

        // TODO: check why are there dublicated signatures after building MIR
        // Signatures live in the type pool as Type::Function variants.
        // Build the remap first so lower_function/lower_type_index can use it.
        let sig_type_indices: Vec<u32> = {
            let mut entries: Vec<u32> = tir
                .type_pool
                .iter()
                .enumerate()
                .filter(|(_, ty)| matches!(ty, tir::Type::Function { .. }))
                .map(|(i, _)| i as u32)
                .collect();
            entries.sort_unstable();
            entries
        };
        let sig_index_remap: HashMap<u32, u32> = sig_type_indices
            .iter()
            .enumerate()
            .map(|(pos, &type_idx)| (type_idx, pos as u32))
            .collect();

        let mut builder = Builder {
            tir,
            interner,
            string_pool: StringPool::new(),
            aggregate_pool: AggregatePool::new(),
            func_index_remap,
            global_index_remap,
            sig_index_remap,
            current_substitutions: Box::new([]),
        };

        // Only lower functions whose body was successfully built (body compilation
        // can fail if TIR encountered errors, leaving them in declared_functions
        // Only lower live functions (dead code elimination via TIR access tracking).
        // Also skip functions whose body failed to compile (present in
        // declared_functions as Defined but absent from defined_functions).
        let functions = tir
            .declared_functions
            .iter()
            .enumerate()
            .filter(|(_, decl)| decl.source == tir::ItemSource::Defined && is_function_live(decl))
            .filter_map(
                |(tir_index, _)| match tir.defined_functions.get(&(tir_index as u32))? {
                    tir::DefinedFunction::Concrete(func) => {
                        let mut mir_func = builder.lower_function(func);
                        mir_func.inline = func.is_inline();
                        Some(mir_func)
                    }
                    tir::DefinedFunction::Template {
                        template_func,
                        substitutions,
                    } => {
                        let template_func = *template_func;
                        builder.current_substitutions = substitutions.clone();
                        let func = match tir.defined_functions.get(&template_func)? {
                            tir::DefinedFunction::Concrete(f) => f,
                            _ => return None,
                        };
                        let mut mir_func = builder.lower_function(func);
                        mir_func.inline = true;
                        builder.current_substitutions = Box::new([]);
                        Some(mir_func)
                    }
                },
            )
            .collect();

        let globals = tir
            .declared_globals
            .iter()
            .enumerate()
            .filter(|(_, decl)| decl.source == tir::ItemSource::Defined)
            .map(|(tir_index, _)| {
                let global = tir.defined_globals.get(&(tir_index as u32)).unwrap();
                builder.lower_global(global)
            })
            .collect();

        let signatures = sig_type_indices
            .iter()
            .map(|&type_idx| match tir.type_pool[type_idx as usize].clone() {
                tir::Type::Function {
                    items,
                    params_count,
                } => {
                    let lowered_items = items
                        .iter()
                        .map(|&idx| builder.lower_type_index(idx))
                        .collect();
                    FunctionSignature {
                        items: lowered_items,
                        params_count,
                    }
                }
                _ => unreachable!(),
            })
            .collect();

        let mut mir = MIR {
            functions,
            globals,
            strings: builder.string_pool.strings.into_boxed_slice(),
            signatures,
            aggregates: builder.aggregate_pool.aggregates.into_boxed_slice(),
            imports: tir
                .import_modules
                .iter()
                .map(|module| ImportModule {
                    name: interner
                        .resolve(module.external_name.inner)
                        .unwrap()
                        .to_string(),
                    items: module
                        .lookup
                        .iter()
                        .map(|(symbol, value)| match value {
                            tir::ImportValue::Function { func_index } => {
                                let tir_sig_index =
                                    tir.declared_functions[*func_index as usize].signature_index;
                                let signature_index = builder.sig_index_remap[&tir_sig_index];
                                ImportModuleItem::Function {
                                    name: *symbol,
                                    func_index: builder.func_index_remap[*func_index as usize],
                                    signature_index,
                                }
                            }
                            tir::ImportValue::Global { global_index } => ImportModuleItem::Global {
                                name: *symbol,
                                global_index: builder.global_index_remap[*global_index as usize],
                            },
                            tir::ImportValue::Memory { memory_index } => ImportModuleItem::Memory {
                                name: *symbol,
                                memory_index: *memory_index,
                            },
                        })
                        .collect(),
                })
                .collect(),
            memories: tir
                .declared_memories
                .iter()
                .filter(|m| m.source == tir::ItemSource::Defined)
                .map(|m| m.kind)
                .collect(),
            exports: {
                let mut exports: Vec<ExportItem> = tir
                    .exports
                    .values()
                    .map(|export| match export {
                        tir::ExportItem::Function {
                            func_index,
                            external_name,
                            internal_name,
                        } => ExportItem::Function {
                            func_index: builder.func_index_remap[*func_index as usize],
                            name: external_name
                                .clone()
                                .map(|n| n.inner)
                                .unwrap_or(internal_name.inner),
                        },
                        tir::ExportItem::Global {
                            global_index,
                            external_name,
                            internal_name,
                        } => ExportItem::Global {
                            global_index: builder.global_index_remap[*global_index as usize],
                            name: external_name
                                .clone()
                                .map(|n| n.inner)
                                .unwrap_or(internal_name.inner),
                        },
                        tir::ExportItem::Memory {
                            memory_index,
                            external_name,
                            internal_name,
                        } => ExportItem::Memory {
                            memory_index: *memory_index,
                            name: external_name
                                .clone()
                                .map(|n| n.inner)
                                .unwrap_or(internal_name.inner),
                        },
                    })
                    .collect();
                exports.sort_by_key(|e| match e {
                    ExportItem::Function { name, .. } => *name,
                    ExportItem::Global { name, .. } => *name,
                    ExportItem::Memory { name, .. } => *name,
                });
                exports
            },
        };

        run_inlining_pass(&mut mir);
        mir
    }
}

struct StringPool {
    lookup: HashMap<SymbolU32, StringIndex>,
    strings: Vec<SymbolU32>,
}

impl StringPool {
    fn new() -> Self {
        StringPool {
            lookup: HashMap::new(),
            strings: Vec::new(),
        }
    }

    fn add(&mut self, symbol: SymbolU32) -> StringIndex {
        if let Some(&index) = self.lookup.get(&symbol) {
            index
        } else {
            let index = self.strings.len() as StringIndex;
            self.lookup.insert(symbol, index);
            self.strings.push(symbol);
            index
        }
    }
}

struct Builder<'tir> {
    tir: &'tir tir::TIR,
    interner: &'tir ast::StringInterner,
    string_pool: StringPool,
    aggregate_pool: AggregatePool,
    func_index_remap: Box<[u32]>,
    global_index_remap: Box<[u32]>,
    sig_index_remap: HashMap<tir::TypeIndex, SignatureIndex>,
    current_substitutions: Box<[(tir::TypeIndex, tir::TypeIndex)]>,
}

struct FunctionContext {
    frame: Vec<BlockScope>,
    current_scope_index: usize,
}

impl<'tir> Builder<'tir> {
    fn lower_type_index(&mut self, type_idx: tir::TypeIndex) -> Type {
        match type_idx {
            idx if idx == tir::Type::ERROR_IDX => unreachable!(),
            idx if idx == tir::Type::UNIT_IDX => return Type::Unit,
            idx if idx == tir::Type::NEVER_IDX => return Type::Never,
            idx if idx == tir::Type::UNKNOWN_IDX => unreachable!(),
            idx if idx == tir::Type::I8_IDX => return Type::I8,
            idx if idx == tir::Type::U8_IDX => return Type::U8,
            idx if idx == tir::Type::I16_IDX => return Type::I16,
            idx if idx == tir::Type::U16_IDX => return Type::U16,
            idx if idx == tir::Type::I32_IDX => return Type::I32,
            idx if idx == tir::Type::U32_IDX => return Type::U32,
            idx if idx == tir::Type::I64_IDX => return Type::I64,
            idx if idx == tir::Type::U64_IDX => return Type::U64,
            idx if idx == tir::Type::F32_IDX => return Type::F32,
            idx if idx == tir::Type::F64_IDX => return Type::F64,
            idx if idx == tir::Type::BOOL_IDX => return Type::Bool,
            idx if idx == tir::Type::CHAR_IDX => return Type::U32,
            _ => {}
        };
        let type_idx = self
            .current_substitutions
            .iter()
            .find(|(from, _)| *from == type_idx)
            .map(|(_, to)| *to)
            .unwrap_or(type_idx);
        match self.tir.type_pool[type_idx as usize].clone() {
            tir::Type::Function { .. } => {
                let sig_idx = self.sig_index_remap[&type_idx];
                Type::Function {
                    signature_index: sig_idx,
                }
            }
            tir::Type::Memory { .. } | tir::Type::Trait { .. } => Type::Unit,
            tir::Type::Struct { struct_index } => {
                let fields: Box<[Type]> = self.tir.structs[struct_index as usize]
                    .fields
                    .iter()
                    .map(|f| self.lower_type_index(f.ty.inner))
                    .collect();
                let aggregate_index = self.aggregate_pool.add(Aggregate { values: fields });
                Type::Aggregate { aggregate_index }
            }
            tir::Type::Tuple { elements } => {
                let fields: Box<[Type]> = elements
                    .iter()
                    .map(|&elem_idx| self.lower_type_index(elem_idx))
                    .collect();
                let aggregate_index = self.aggregate_pool.add(Aggregate { values: fields });
                Type::Aggregate { aggregate_index }
            }
            _ => unimplemented!(),
        }
    }

    fn memory_index_from_arg(&self, type_idx: tir::TypeIndex) -> u32 {
        let resolved = self
            .current_substitutions
            .iter()
            .find(|(from, _)| *from == type_idx)
            .map(|(_, to)| *to)
            .unwrap_or(type_idx);
        match self.tir.type_pool[resolved as usize] {
            tir::Type::Memory { memory_index, .. } => memory_index,
            _ => unreachable!("intrinsic argument is not a Memory type"),
        }
    }

    fn lower_function(&mut self, func: &tir::Function) -> Function {
        let frame = func
            .stack
            .scopes
            .iter()
            .map(|scope| {
                let result_type_idx = scope.inferred_type.unwrap_or(tir::Type::UNIT_IDX);
                let locals = scope
                    .locals
                    .iter()
                    .map(|tir_local| Local {
                        ty: self.lower_type_index(tir_local.ty),
                        mutability: if tir_local.mut_span.is_some() {
                            Mutability::Mutable
                        } else {
                            Mutability::Immutable
                        },
                    })
                    .collect();
                BlockScope {
                    kind: scope.kind,
                    parent: scope.parent,
                    locals,
                    result: self.lower_type_index(result_type_idx),
                }
            })
            .collect();

        let mut ctx = FunctionContext {
            current_scope_index: 0,
            frame,
        };

        let mut top_sink = Vec::new();
        let block = self.lower_expression(&mut ctx, &func.block, &mut top_sink);

        Function {
            signature_index: self.sig_index_remap[&func.signature_index],
            scopes: ctx.frame,
            block,
            inline: false,
        }
    }

    fn lower_global(&mut self, global: &tir::Global) -> Global {
        let mut dummy_ctx = FunctionContext {
            frame: Vec::new(),
            current_scope_index: 0,
        };

        Global {
            ty: self.lower_type_index(global.ty.inner),
            mutability: if global.mut_span.is_some() {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            value: self.lower_expression(&mut dummy_ctx, &global.value.inner, &mut Vec::new()),
        }
    }

    fn lower_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &tir::Expression,
        sink: &mut Vec<Expression>,
    ) -> Expression {
        use crate::ast::{BinaryOp, UnaryOp};

        match &expr.kind {
            tir::ExprKind::Error | tir::ExprKind::Placeholder | tir::ExprKind::Memory { .. } => {
                unreachable!("memory handle should only appear as object of a member access")
            }
            tir::ExprKind::Unreachable => Expression {
                kind: ExprKind::Unreachable,
                ty: Type::Never,
            },
            tir::ExprKind::Int { value } => Expression {
                kind: ExprKind::Int { value: *value },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Float { value } => Expression {
                kind: ExprKind::Float { value: *value },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Bool { value } => Expression {
                kind: ExprKind::Bool { value: *value },
                ty: Type::Bool,
            },
            tir::ExprKind::Global { global_index } => Expression {
                kind: ExprKind::Global {
                    global_index: self.global_index_remap[*global_index as usize],
                },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Local {
                scope_index,
                local_index,
            } => Expression {
                kind: ExprKind::LocalGet {
                    scope_index: *scope_index,
                    local_index: *local_index,
                },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Function { func_index } => Expression {
                kind: ExprKind::Function {
                    func_index: self.func_index_remap[*func_index as usize],
                },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Char { value } => Expression {
                kind: ExprKind::Int {
                    value: *value as i64,
                },
                ty: Type::U32,
            },
            tir::ExprKind::String { symbol } => {
                let string_index = self.string_pool.add(*symbol);
                let len = self.interner.resolve(*symbol).unwrap().len() as i64;
                let ty = self.lower_type_index(expr.ty);
                Expression {
                    kind: ExprKind::Aggregate {
                        values: Box::new([
                            Expression {
                                kind: ExprKind::StringIndex { string_index },
                                ty: Type::Pointer,
                            },
                            Expression {
                                kind: ExprKind::Int { value: len },
                                ty: Type::U32,
                            },
                        ]),
                    },
                    ty,
                }
            }
            tir::ExprKind::Return { value } => Expression {
                kind: ExprKind::Return {
                    value: value
                        .as_ref()
                        .map(|v| Box::new(self.lower_expression(func_ctx, v, sink))),
                },
                ty: Type::Never,
            },
            tir::ExprKind::EnumVariant {
                enum_index,
                variant_index,
            } => {
                let enum_ = &self.tir.enums[*enum_index as usize];
                let variant = &enum_.variants[*variant_index as usize];
                self.lower_expression(func_ctx, &variant.value, sink)
            }
            tir::ExprKind::Call { callee, arguments } => {
                // Detect calls to intrinsic functions (e.g. wasm::memory32_grow).
                // TIR resolves module::fn directly to ExprKind::Function { func_index }.
                if let tir::ExprKind::Function { func_index } = &callee.kind {
                    let decl = &self.tir.declared_functions[*func_index as usize];
                    if decl.source == tir::ItemSource::Intrinsic {
                        // `interner` and `tir` live for `'tir`, so these refs outlive &mut self borrows below.
                        let intrinsic_name = self.interner.resolve(decl.name.inner).unwrap();
                        let result_ty = self.lower_type_index(expr.ty);
                        return match intrinsic_name {
                            "memory32_size" => {
                                let memory_index =
                                    self.memory_index_from_arg(arguments[0].ty);
                                Expression {
                                    kind: ExprKind::MemorySize { memory_index },
                                    ty: result_ty,
                                }
                            }
                            "memory32_grow" => {
                                let memory_index =
                                    self.memory_index_from_arg(arguments[0].ty);
                                let delta = Box::new(
                                    self.lower_expression(func_ctx, &arguments[1], sink),
                                );
                                Expression {
                                    kind: ExprKind::MemoryGrow { memory_index, delta },
                                    ty: result_ty,
                                }
                            }
                            name => unreachable!("unknown intrinsic: {name}"),
                        };
                    }
                }

                let callee = Box::new(self.lower_expression(func_ctx, callee, sink));
                let arguments = arguments
                    .iter()
                    .map(|arg| self.lower_expression(func_ctx, arg, sink))
                    .collect();
                Expression {
                    kind: ExprKind::Call { callee, arguments },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::MethodCall {
                object,
                arguments,
                func_index,
            } => {
                if let tir::Type::Memory { .. } = &self.tir.type_pool[object.ty as usize] {
                    let memory_index = match &object.kind {
                        tir::ExprKind::Memory { memory_index } => *memory_index,
                        _ => unreachable!("memory method call on non-memory expression"),
                    };
                    let method_name = self
                        .interner
                        .resolve(self.tir.declared_functions[*func_index as usize].name.inner)
                        .unwrap_or("");
                    let result_ty = self.lower_type_index(expr.ty);
                    return match method_name {
                        "size" => Expression {
                            kind: ExprKind::MemorySize { memory_index },
                            ty: result_ty,
                        },
                        "grow" => {
                            let delta =
                                Box::new(self.lower_expression(func_ctx, &arguments[0], sink));
                            Expression {
                                kind: ExprKind::MemoryGrow {
                                    memory_index,
                                    delta,
                                },
                                ty: result_ty,
                            }
                        }
                        _ => unreachable!("unknown memory method: {}", method_name),
                    };
                }

                let callee = Box::new(Expression {
                    kind: ExprKind::Function {
                        func_index: self.func_index_remap[*func_index as usize],
                    },
                    ty: self.lower_type_index(expr.ty),
                });
                let object = self.lower_expression(func_ctx, object, sink);
                let arguments: Box<_> = std::iter::once(object)
                    .chain(
                        arguments
                            .iter()
                            .map(|arg| self.lower_expression(func_ctx, arg, sink)),
                    )
                    .collect();
                Expression {
                    kind: ExprKind::Call { callee, arguments },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::NamespaceAccess { ty, member, .. } => {
                match &self.tir.type_pool[ty.inner as usize] {
                    tir::Type::Enum { enum_index } => {
                        let enum_index = *enum_index;
                        let enum_ = &self.tir.enums[enum_index as usize];
                        let variant_index = *enum_
                            .lookup
                            .get(&member.inner)
                            .expect("unknown enum variant");
                        let variant = &enum_.variants[variant_index as usize];
                        self.lower_expression(func_ctx, &variant.value, sink)
                    }
                    tir::Type::ImportModule { module_index } => {
                        let module_index = *module_index;
                        let module = &self.tir.import_modules[module_index as usize];
                        match module
                            .lookup
                            .get(&member.inner)
                            .expect("unknown import member")
                        {
                            tir::ImportValue::Function { func_index } => Expression {
                                kind: ExprKind::Function {
                                    func_index: self.func_index_remap[*func_index as usize],
                                },
                                ty: self.lower_type_index(expr.ty),
                            },
                            tir::ImportValue::Global { global_index } => Expression {
                                kind: ExprKind::Global {
                                    global_index: *global_index,
                                },
                                ty: self.lower_type_index(expr.ty),
                            },
                            tir::ImportValue::Memory { .. } => {
                                // TIR resolves imported memory namespace accesses to ExprKind::Memory,
                                // not ExprKind::NamespaceAccess, so this arm is never reached.
                                unreachable!("import module NamespaceAccess for Memory")
                            }
                        }
                    }
                    tir::Type::Memory { memory_index, .. } => {
                        let memory_index = *memory_index;
                        let member_name = self.interner.resolve(member.inner).unwrap();
                        let result_ty = self.lower_type_index(expr.ty);
                        match member_name {
                            "OFFSET" => Expression {
                                kind: ExprKind::MemoryOffset { memory_index },
                                ty: result_ty,
                            },
                            "MEMORY_INDEX" => Expression {
                                kind: ExprKind::Int {
                                    value: memory_index as i64,
                                },
                                ty: result_ty,
                            },
                            _ => unreachable!("unknown memory namespace member: {}", member_name),
                        }
                    }
                    _ => {
                        let entry = self
                            .tir
                            .impl_members
                            .get(&ty.inner)
                            .and_then(|m| m.get(&member.inner))
                            .expect("unresolved impl member in MIR lowering");
                        match entry {
                            tir::ImplEntry::AssociatedConst { .. } => {
                                let member_name = self.interner.resolve(member.inner).unwrap_or("");
                                let layout = tir::compute_layout(
                                    self.tir.type_pool.as_slice(),
                                    &self.tir.structs,
                                    ty.inner,
                                    tir::PointerSize::P32,
                                );
                                let value = match member_name {
                                    "SIZE" => layout.size as i64,
                                    "ALIGN" => layout.align as i64,
                                    _ => todo!("unknown associated const: {}", member_name),
                                };
                                Expression {
                                    kind: ExprKind::Int { value },
                                    ty: self.lower_type_index(expr.ty),
                                }
                            }
                            _ => todo!("non-const impl member in namespace access"),
                        }
                    }
                }
            }
            tir::ExprKind::ObjectAccess { object, member } => {
                // Memory member access — OFFSET constant; grow/size appear only as call callees.
                if let tir::Type::Memory { .. } = &self.tir.type_pool[object.ty as usize] {
                    let member_name = self.interner.resolve(member.inner).unwrap_or("");
                    let memory_index = match &object.kind {
                        tir::ExprKind::Memory { memory_index } => *memory_index,
                        _ => unreachable!("memory member access on non-memory expression"),
                    };
                    return match member_name {
                        "OFFSET" => Expression {
                            kind: ExprKind::MemoryOffset { memory_index },
                            ty: self.lower_type_index(expr.ty),
                        },
                        _ => unreachable!(
                            "unexpected memory member in non-call position: {}",
                            member_name
                        ),
                    };
                }

                let struct_index = match &self.tir.type_pool[object.ty as usize] {
                    tir::Type::Struct { struct_index } => *struct_index,
                    _ => unreachable!("ObjectAccess on non-struct type"),
                };
                let field_index =
                    self.tir.structs[struct_index as usize].lookup[&member.inner] as u32;
                let field_ty = self.lower_type_index(expr.ty);

                match &object.kind {
                    tir::ExprKind::Local {
                        scope_index,
                        local_index,
                    } => Expression {
                        kind: ExprKind::AggregateGet {
                            scope_index: *scope_index,
                            local_index: *local_index,
                            value_index: field_index,
                        },
                        ty: field_ty,
                    },
                    _ => {
                        let object_ty = self.lower_type_index(object.ty);
                        let object_lowered = self.lower_expression(func_ctx, object, sink);

                        let temp_idx = func_ctx.frame[0].locals.len() as u32;
                        func_ctx.frame[0].locals.push(Local {
                            ty: object_ty,
                            mutability: Mutability::Immutable,
                        });

                        sink.push(Expression {
                            kind: ExprKind::LocalSet {
                                scope_index: 0,
                                local_index: temp_idx,
                                value: Box::new(object_lowered),
                            },
                            ty: Type::Unit,
                        });

                        Expression {
                            kind: ExprKind::AggregateGet {
                                scope_index: 0,
                                local_index: temp_idx,
                                value_index: field_index,
                            },
                            ty: field_ty,
                        }
                    }
                }
            }
            tir::ExprKind::StructInit { fields, .. } => {
                let mir_fields: Box<[Expression]> = fields
                    .iter()
                    .map(|f| self.lower_expression(func_ctx, f, sink))
                    .collect();
                Expression {
                    kind: ExprKind::Aggregate { values: mir_fields },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let condition = Box::new(self.lower_expression(func_ctx, condition, sink));
                let then_block = Box::new(self.lower_expression(func_ctx, then_block, sink));
                let else_block = else_block
                    .as_ref()
                    .map(|e| Box::new(self.lower_expression(func_ctx, e, sink)));
                Expression {
                    kind: ExprKind::IfElse {
                        condition,
                        then_block,
                        else_block,
                    },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::Break { scope_index, value } => Expression {
                kind: ExprKind::Break {
                    scope_index: *scope_index,
                    value: value
                        .as_ref()
                        .map(|v| Box::new(self.lower_expression(func_ctx, v, sink))),
                },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Continue { scope_index } => Expression {
                kind: ExprKind::Continue {
                    scope_index: *scope_index,
                },
                ty: Type::Never,
            },
            tir::ExprKind::Loop { scope_index, block } => Expression {
                kind: ExprKind::Loop {
                    scope_index: *scope_index,
                    block: Box::new(self.lower_expression(func_ctx, block, sink)),
                },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => {
                func_ctx.current_scope_index = *scope_index as usize;
                let mut inner_sink: Vec<Expression> = Vec::new();

                for e in expressions.iter() {
                    let lowered = self.lower_expression(func_ctx, e, &mut inner_sink);
                    inner_sink.push(lowered);
                }
                if let Some(result) = result {
                    let lowered = self.lower_expression(func_ctx, result, &mut inner_sink);
                    inner_sink.push(lowered);
                }

                Expression {
                    kind: ExprKind::Block {
                        scope_index: *scope_index,
                        expressions: inner_sink.into_boxed_slice(),
                    },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::LocalDeclaration {
                scope_index,
                local_index,
                value,
                ..
            } => Expression {
                kind: ExprKind::LocalSet {
                    scope_index: *scope_index,
                    local_index: *local_index,
                    value: Box::new(self.lower_expression(func_ctx, value, sink)),
                },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Unary { operator, operand } => {
                let operand = Box::new(self.lower_expression(func_ctx, operand, sink));
                Expression {
                    kind: match operator.inner {
                        UnaryOp::InvertSign => ExprKind::Neg { value: operand },
                        UnaryOp::Not => ExprKind::Eqz { value: operand },
                        UnaryOp::BitNot => ExprKind::BitNot { value: operand },
                    },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::Binary {
                operator,
                left,
                right,
            } => {
                use BinaryOp::*;

                let kind = match operator.inner {
                    Assign => self.lower_assignment(func_ctx, left, right, sink),
                    AddAssign | SubAssign | MulAssign | DivAssign | RemAssign => {
                        self.lower_compound_assignment(func_ctx, operator.inner, left, right, sink)
                    }
                    Add => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Add { left, right }
                    }
                    Sub => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Sub { left, right }
                    }
                    Mul => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Mul { left, right }
                    }
                    Div => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Div { left, right }
                    }
                    Rem => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Rem { left, right }
                    }
                    Eq => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Eq { left, right }
                    }
                    NotEq => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::NotEq { left, right }
                    }
                    Less => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Less { left, right }
                    }
                    LessEq => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::LessEq { left, right }
                    }
                    Greater => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Greater { left, right }
                    }
                    GreaterEq => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::GreaterEq { left, right }
                    }
                    And => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::And { left, right }
                    }
                    Or => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::Or { left, right }
                    }
                    BitAnd => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::BitAnd { left, right }
                    }
                    BitOr => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::BitOr { left, right }
                    }
                    BitXor => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::BitXor { left, right }
                    }
                    LeftShift => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::LeftShift { left, right }
                    }
                    RightShift => {
                        let left = Box::new(self.lower_expression(func_ctx, left, sink));
                        let right = Box::new(self.lower_expression(func_ctx, right, sink));
                        ExprKind::RightShift { left, right }
                    }
                };

                Expression {
                    kind,
                    ty: self.lower_type_index(expr.ty),
                }
            }
        }
    }

    fn lower_assignment(
        &mut self,
        func_ctx: &mut FunctionContext,
        left: &tir::Expression,
        right: &tir::Expression,
        sink: &mut Vec<Expression>,
    ) -> ExprKind {
        match &left.kind {
            tir::ExprKind::Local {
                scope_index,
                local_index,
            } => ExprKind::LocalSet {
                scope_index: *scope_index,
                local_index: *local_index,
                value: Box::new(self.lower_expression(func_ctx, right, sink)),
            },
            tir::ExprKind::Global { global_index } => ExprKind::GlobalSet {
                global_index: self.global_index_remap[*global_index as usize],
                value: Box::new(self.lower_expression(func_ctx, right, sink)),
            },
            _ => unreachable!(),
        }
    }

    fn lower_compound_assignment(
        &mut self,
        func_ctx: &mut FunctionContext,
        op: crate::ast::BinaryOp,
        left: &tir::Expression,
        right: &tir::Expression,
        sink: &mut Vec<Expression>,
    ) -> ExprKind {
        use crate::ast::BinaryOp::*;

        // Desugar x += y to x = x + y
        let binary_op = match op {
            AddAssign => Add,
            SubAssign => Sub,
            MulAssign => Mul,
            DivAssign => Div,
            RemAssign => Rem,
            _ => unreachable!(),
        };

        // Create the binary operation: x + y
        let binary_expr_kind = match binary_op {
            Add => ExprKind::Add {
                left: Box::new(self.lower_expression(func_ctx, left, sink)),
                right: Box::new(self.lower_expression(func_ctx, right, sink)),
            },
            Sub => ExprKind::Sub {
                left: Box::new(self.lower_expression(func_ctx, left, sink)),
                right: Box::new(self.lower_expression(func_ctx, right, sink)),
            },
            Mul => ExprKind::Mul {
                left: Box::new(self.lower_expression(func_ctx, left, sink)),
                right: Box::new(self.lower_expression(func_ctx, right, sink)),
            },
            Div => ExprKind::Div {
                left: Box::new(self.lower_expression(func_ctx, left, sink)),
                right: Box::new(self.lower_expression(func_ctx, right, sink)),
            },
            Rem => ExprKind::Rem {
                left: Box::new(self.lower_expression(func_ctx, left, sink)),
                right: Box::new(self.lower_expression(func_ctx, right, sink)),
            },
            _ => unreachable!(),
        };

        let binary_expr = Expression {
            kind: binary_expr_kind,
            ty: self.lower_type_index(left.ty),
        };

        // Now assign the result back to left: x = (x + y)
        match &left.kind {
            tir::ExprKind::Local {
                scope_index,
                local_index,
            } => ExprKind::LocalSet {
                scope_index: *scope_index,
                local_index: *local_index,
                value: Box::new(binary_expr),
            },
            tir::ExprKind::Global { global_index } => ExprKind::GlobalSet {
                global_index: self.global_index_remap[*global_index as usize],
                value: Box::new(binary_expr),
            },
            _ => unreachable!(),
        }
    }
}

// ---------------------------------------------------------------------------
// Inlining pass
// ---------------------------------------------------------------------------

/// Deep-clones `expr`, offsetting every scope index by `scope_offset` and
/// rewriting `Return { value }` into `Break { scope_index: wrapper_scope, value }`.
fn rewrite_body(
    expr: Expression,
    scope_offset: ScopeIndex,
    wrapper_scope: ScopeIndex,
) -> Expression {
    let rw = |e: Expression| rewrite_body(e, scope_offset, wrapper_scope);
    let rw_box = |e: Box<Expression>| Box::new(rw(*e));
    let rw_opt = |e: Option<Box<Expression>>| e.map(|b| rw_box(b));
    let rw_args = |es: Box<[Expression]>| es.into_vec().into_iter().map(rw).collect::<Box<[_]>>();

    let ty = expr.ty;
    let kind = match expr.kind {
        // Scope-indexed variants — offset the index and recurse into children.
        ExprKind::LocalGet {
            scope_index,
            local_index,
        } => ExprKind::LocalGet {
            scope_index: scope_index + scope_offset,
            local_index,
        },
        ExprKind::AggregateGet {
            scope_index,
            local_index,
            value_index: field_index,
        } => ExprKind::AggregateGet {
            scope_index: scope_index + scope_offset,
            local_index,
            value_index: field_index,
        },
        ExprKind::LocalSet {
            scope_index,
            local_index,
            value,
        } => ExprKind::LocalSet {
            scope_index: scope_index + scope_offset,
            local_index,
            value: rw_box(value),
        },
        ExprKind::Block {
            scope_index,
            expressions,
        } => ExprKind::Block {
            scope_index: scope_index + scope_offset,
            expressions: rw_args(expressions),
        },
        ExprKind::Break { scope_index, value } => ExprKind::Break {
            scope_index: scope_index + scope_offset,
            value: rw_opt(value),
        },
        ExprKind::Continue { scope_index } => ExprKind::Continue {
            scope_index: scope_index + scope_offset,
        },
        ExprKind::Loop { scope_index, block } => ExprKind::Loop {
            scope_index: scope_index + scope_offset,
            block: rw_box(block),
        },
        // Return becomes a Break out of the wrapper scope.
        ExprKind::Return { value } => ExprKind::Break {
            scope_index: wrapper_scope,
            value: rw_opt(value),
        },
        // Non-scope variants — recurse into children only.
        ExprKind::Drop { value } => ExprKind::Drop {
            value: rw_box(value),
        },
        ExprKind::Neg { value } => ExprKind::Neg {
            value: rw_box(value),
        },
        ExprKind::BitNot { value } => ExprKind::BitNot {
            value: rw_box(value),
        },
        ExprKind::Eqz { value } => ExprKind::Eqz {
            value: rw_box(value),
        },
        ExprKind::GlobalSet {
            global_index,
            value,
        } => ExprKind::GlobalSet {
            global_index,
            value: rw_box(value),
        },
        ExprKind::Aggregate { values: fields } => ExprKind::Aggregate {
            values: rw_args(fields),
        },
        ExprKind::Call { callee, arguments } => ExprKind::Call {
            callee: rw_box(callee),
            arguments: rw_args(arguments),
        },
        ExprKind::IfElse {
            condition,
            then_block,
            else_block,
        } => ExprKind::IfElse {
            condition: rw_box(condition),
            then_block: rw_box(then_block),
            else_block: else_block.map(rw_box),
        },
        ExprKind::Add { left, right } => ExprKind::Add {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::Sub { left, right } => ExprKind::Sub {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::Mul { left, right } => ExprKind::Mul {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::Div { left, right } => ExprKind::Div {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::Rem { left, right } => ExprKind::Rem {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::And { left, right } => ExprKind::And {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::Or { left, right } => ExprKind::Or {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::Eq { left, right } => ExprKind::Eq {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::NotEq { left, right } => ExprKind::NotEq {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::Less { left, right } => ExprKind::Less {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::LessEq { left, right } => ExprKind::LessEq {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::Greater { left, right } => ExprKind::Greater {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::GreaterEq { left, right } => ExprKind::GreaterEq {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::BitAnd { left, right } => ExprKind::BitAnd {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::BitOr { left, right } => ExprKind::BitOr {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::BitXor { left, right } => ExprKind::BitXor {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::LeftShift { left, right } => ExprKind::LeftShift {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::RightShift { left, right } => ExprKind::RightShift {
            left: rw_box(left),
            right: rw_box(right),
        },
        ExprKind::MemoryGrow {
            memory_index,
            delta,
        } => ExprKind::MemoryGrow {
            memory_index,
            delta: rw_box(delta),
        },
        // Leaf variants — nothing to rewrite.
        k @ (ExprKind::Noop
        | ExprKind::Bool { .. }
        | ExprKind::Function { .. }
        | ExprKind::Int { .. }
        | ExprKind::Float { .. }
        | ExprKind::StringIndex { .. }
        | ExprKind::Global { .. }
        | ExprKind::Unreachable
        | ExprKind::MemoryOffset { .. }
        | ExprKind::MemorySize { .. }) => k,
    };
    Expression { kind, ty }
}

/// Substitutes a direct call at the call site with the callee's body inlined
/// into the caller. Appends the required scopes to `caller_scopes`.
fn inline_call(
    callee: Function,
    arguments: Box<[Expression]>,
    caller_scopes: &mut Vec<BlockScope>,
) -> Expression {
    let result_ty = callee.block.ty;

    // The wrapper scope is the break-target for all rewritten `Return` nodes.
    let wrapper_scope = caller_scopes.len() as ScopeIndex;
    caller_scopes.push(BlockScope {
        kind: tir::BlockKind::Block,
        parent: None,
        locals: vec![],
        result: result_ty,
    });

    // Callee's scopes follow the wrapper.  Offset their parent pointers.
    let body_scope_offset = caller_scopes.len() as ScopeIndex;
    for scope in callee.scopes {
        caller_scopes.push(BlockScope {
            parent: scope.parent.map(|p| p + body_scope_offset),
            ..scope
        });
    }

    // Store each argument into the corresponding param local in the callee's
    // root scope (now living at body_scope_offset).
    let mut exprs: Vec<Expression> = arguments
        .into_vec()
        .into_iter()
        .enumerate()
        .map(|(i, arg)| Expression {
            ty: Type::Unit,
            kind: ExprKind::LocalSet {
                scope_index: body_scope_offset,
                local_index: i as LocalIndex,
                value: Box::new(arg),
            },
        })
        .collect();

    exprs.push(rewrite_body(callee.block, body_scope_offset, wrapper_scope));

    Expression {
        ty: result_ty,
        kind: ExprKind::Block {
            scope_index: wrapper_scope,
            expressions: exprs.into_boxed_slice(),
        },
    }
}

/// Walks `expr` in-place (post-order) and replaces every direct call to an
/// `#[inline]` function with its inlined body.
fn inline_expr(expr: &mut Expression, caller_scopes: &mut Vec<BlockScope>, functions: &[Function]) {
    // Recurse into all children first.
    match &mut expr.kind {
        ExprKind::LocalSet { value, .. }
        | ExprKind::GlobalSet { value, .. }
        | ExprKind::Drop { value }
        | ExprKind::Neg { value }
        | ExprKind::BitNot { value }
        | ExprKind::Eqz { value } => inline_expr(value, caller_scopes, functions),

        ExprKind::Aggregate { values: fields } => {
            for e in fields.iter_mut() {
                inline_expr(e, caller_scopes, functions);
            }
        }
        ExprKind::Block { expressions, .. } => {
            for e in expressions.iter_mut() {
                inline_expr(e, caller_scopes, functions);
            }
        }
        ExprKind::Loop { block, .. } => inline_expr(block, caller_scopes, functions),

        ExprKind::Break { value, .. } | ExprKind::Return { value } => {
            if let Some(v) = value {
                inline_expr(v, caller_scopes, functions);
            }
        }
        ExprKind::IfElse {
            condition,
            then_block,
            else_block,
        } => {
            inline_expr(condition, caller_scopes, functions);
            inline_expr(then_block, caller_scopes, functions);
            if let Some(e) = else_block {
                inline_expr(e, caller_scopes, functions);
            }
        }
        ExprKind::Call { callee, arguments } => {
            inline_expr(callee, caller_scopes, functions);
            for a in arguments.iter_mut() {
                inline_expr(a, caller_scopes, functions);
            }
        }
        ExprKind::Add { left, right }
        | ExprKind::Sub { left, right }
        | ExprKind::Mul { left, right }
        | ExprKind::Div { left, right }
        | ExprKind::Rem { left, right }
        | ExprKind::And { left, right }
        | ExprKind::Or { left, right }
        | ExprKind::Eq { left, right }
        | ExprKind::NotEq { left, right }
        | ExprKind::Less { left, right }
        | ExprKind::LessEq { left, right }
        | ExprKind::Greater { left, right }
        | ExprKind::GreaterEq { left, right }
        | ExprKind::BitAnd { left, right }
        | ExprKind::BitOr { left, right }
        | ExprKind::BitXor { left, right }
        | ExprKind::LeftShift { left, right }
        | ExprKind::RightShift { left, right } => {
            inline_expr(left, caller_scopes, functions);
            inline_expr(right, caller_scopes, functions);
        }
        ExprKind::MemoryGrow { delta, .. } => inline_expr(delta, caller_scopes, functions),
        // Leaf variants — nothing to recurse into.
        ExprKind::Noop
        | ExprKind::Bool { .. }
        | ExprKind::Function { .. }
        | ExprKind::Int { .. }
        | ExprKind::Float { .. }
        | ExprKind::StringIndex { .. }
        | ExprKind::Global { .. }
        | ExprKind::Unreachable
        | ExprKind::LocalGet { .. }
        | ExprKind::AggregateGet { .. }
        | ExprKind::Continue { .. }
        | ExprKind::MemoryOffset { .. }
        | ExprKind::MemorySize { .. } => {}
    }

    // After children are processed, check if this node is an inlinable call.
    let func_index = match &expr.kind {
        ExprKind::Call { callee, .. } => match callee.kind {
            ExprKind::Function { func_index } => func_index,
            _ => return,
        },
        _ => return,
    };
    let f = func_index as usize;
    if f >= functions.len() || !functions[f].inline {
        return;
    }

    // Extract the arguments, then replace the expression with the inlined body.
    let arguments = match std::mem::replace(&mut expr.kind, ExprKind::Noop) {
        ExprKind::Call { arguments, .. } => arguments,
        _ => unreachable!(),
    };
    *expr = inline_call(functions[f].clone(), arguments, caller_scopes);
}

/// Runs one shallow inlining pass over all function bodies in `mir`.
/// Every direct call to a function marked `#[inline]` is replaced with that
/// function's body (one level deep — calls within the inlined body are not
/// further expanded).
pub fn run_inlining_pass(mir: &mut MIR) {
    let placeholder = Function {
        signature_index: 0,
        scopes: vec![],
        block: Expression {
            kind: ExprKind::Noop,
            ty: Type::Unit,
        },
        inline: false,
    };
    for i in 0..mir.functions.len() {
        // Swap the function out so we can borrow the rest of `mir.functions`
        // immutably for reading callee bodies while mutating this function.
        let mut func = std::mem::replace(&mut mir.functions[i], placeholder.clone());
        inline_expr(&mut func.block, &mut func.scopes, &mir.functions);
        mir.functions[i] = func;
    }
}

#[cfg(test)]
mod tests;
