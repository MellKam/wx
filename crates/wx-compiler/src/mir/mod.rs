/// the role of MIR is to desugar the syntax like x += 1 into x = x + 1 and
/// lower the concepts like enums into primitive constants, convert labels from
/// symbols in interner into numeric indices
use std::collections::{HashMap, HashSet, VecDeque};

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
        id: ast::DefId,
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
        id: ast::DefId,
    },
    GlobalSet {
        id: ast::DefId,
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
    #[cfg_attr(test, serde(skip))]
    pub inline_functions: HashSet<ast::DefId>,
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
        id: ast::DefId,
        signature_index: SignatureIndex,
    },
    Global {
        name: SymbolU32,
        id: ast::DefId,
    },
    Memory {
        name: SymbolU32,
        memory_index: u32,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ExportItem {
    Function { id: ast::DefId, name: SymbolU32 },
    Global { id: ast::DefId, name: SymbolU32 },
    Memory { memory_index: u32, name: SymbolU32 },
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
    pub id: ast::DefId,
    pub signature_index: SignatureIndex,
    pub scopes: Vec<BlockScope>,
    pub block: Expression,
    /// DefIds of functions that call this function, derived from TIR accesses.
    /// Used to build the call graph without re-walking MIR expressions.
    #[cfg_attr(test, serde(skip))]
    pub callers: Box<[ast::DefId]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
    pub id: ast::DefId,
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

/// Memory layout of a type: size in bytes and required alignment in bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Layout {
    pub size: u32,
    pub align: u32,
}

impl Layout {
    fn ptr(ptr_size: PointerSize) -> Layout {
        let n = ptr_size as u32;
        Layout { size: n, align: n }
    }

    fn pad_to_align(self) -> Self {
        Layout {
            size: (self.size + self.align - 1) & !(self.align - 1),
            align: self.align,
        }
    }
}

#[repr(u32)]
#[derive(Clone, Copy)]
pub enum PointerSize {
    Memory32 = 4,
    Memory64 = 8,
}

/// Compute the memory layout of a type.
///
/// Fields of structs and tuples are sorted by alignment descending before
/// computing padding, giving optimal (minimal) struct sizes.
///
/// Panics on `Error`, `Unknown`, `ImportModule`, `Enum`, or non-value types.
pub fn compute_layout(
    types: &[tir::Type],
    structs: &[tir::Struct],
    idx: tir::TypeIndex,
    ptr_size: PointerSize,
) -> Layout {
    match &types[idx as usize] {
        tir::Type::Unit | tir::Type::Never => Layout { size: 0, align: 1 },
        tir::Type::I32 | tir::Type::U32 | tir::Type::F32 | tir::Type::Char => {
            Layout { size: 4, align: 4 }
        }
        tir::Type::U8 | tir::Type::I8 => Layout { size: 1, align: 1 },
        tir::Type::U16 | tir::Type::I16 => Layout { size: 2, align: 2 },
        tir::Type::Bool => Layout { size: 1, align: 1 },
        tir::Type::I64 | tir::Type::U64 | tir::Type::F64 => Layout { size: 8, align: 8 },
        tir::Type::Pointer { .. } => Layout::ptr(ptr_size),
        tir::Type::Slice { .. } => {
            let p = ptr_size as u32;
            Layout {
                size: p * 2,
                align: p,
            }
        }
        tir::Type::Array { of, size, .. } => {
            let elem = compute_layout(types, structs, *of, ptr_size).pad_to_align();
            Layout {
                size: elem.size * size,
                align: elem.align,
            }
        }
        tir::Type::Tuple { elements } => {
            let mut indexed: Vec<(tir::TypeIndex, u32)> = elements
                .iter()
                .map(|&ty| (ty, compute_layout(types, structs, ty, ptr_size).align))
                .collect();
            indexed.sort_by(|a, b| b.1.cmp(&a.1));
            let sorted: Vec<tir::TypeIndex> = indexed.into_iter().map(|(ty, _)| ty).collect();
            aggregate_layout(types, structs, &sorted, ptr_size)
        }
        tir::Type::Struct { struct_index } => {
            let si = *struct_index as usize;
            let mut indexed: Vec<(tir::TypeIndex, u32)> = structs[si]
                .fields
                .iter()
                .map(|f| {
                    let align = compute_layout(types, structs, f.ty.inner, ptr_size).align;
                    (f.ty.inner, align)
                })
                .collect();
            indexed.sort_by(|a, b| b.1.cmp(&a.1));
            let sorted: Vec<tir::TypeIndex> = indexed.into_iter().map(|(ty, _)| ty).collect();
            aggregate_layout(types, structs, &sorted, ptr_size)
        }
        tir::Type::Function { .. } => Layout::ptr(ptr_size),
        tir::Type::Error
        | tir::Type::Unknown
        | tir::Type::ImportModule { .. }
        | tir::Type::Module { .. }
        | tir::Type::Enum { .. }
        | tir::Type::Memory { .. }
        | tir::Type::Trait { .. }
        | tir::Type::TypeParam { .. } => panic!("compute_layout called on non-value type"),
    }
}

/// C-style layout for an ordered sequence of field types.
fn aggregate_layout(
    types: &[tir::Type],
    structs: &[tir::Struct],
    fields: &[tir::TypeIndex],
    ptr_size: PointerSize,
) -> Layout {
    let mut size: u32 = 0;
    let mut align: u32 = 1;
    for &field_idx in fields {
        let field = compute_layout(types, structs, field_idx, ptr_size);
        size = (size + field.align - 1) & !(field.align - 1);
        size += field.size;
        align = align.max(field.align);
    }
    size = (size + align - 1) & !(align - 1);
    Layout { size, align }
}

/// Describes the permutation between an aggregate type's declaration order and
/// its physical (alignment-sorted, padding-minimised) storage order.
struct AggregateLayout {
    /// `decl_to_phys[decl_index]` = physical slot index.
    decl_to_phys: Box<[u32]>,
    /// `phys_to_decl[phys_index]` = declaration index.
    phys_to_decl: Box<[u32]>,
}

/// Compute the `AggregateLayout` for a struct or tuple type.
///
/// Fields/elements are sorted by alignment descending (stable, so equal-
/// alignment fields keep their declaration order) to minimise padding.
fn compute_field_order(
    type_index: tir::TypeIndex,
    types: &[tir::Type],
    structs: &[tir::Struct],
) -> AggregateLayout {
    let decl_types: Box<[tir::TypeIndex]> = match &types[type_index as usize] {
        tir::Type::Tuple { elements } => elements.clone(),
        tir::Type::Struct { struct_index } => structs[*struct_index as usize]
            .fields
            .iter()
            .map(|f| f.ty.inner)
            .collect(),
        _ => unreachable!("compute_layout called on non-aggregate type"),
    };

    let mut indexed: Vec<(u32, u32)> = decl_types
        .iter()
        .enumerate()
        .map(|(decl, &ty)| {
            let align = compute_layout(types, structs, ty, PointerSize::Memory32).align;
            (decl as u32, align)
        })
        .collect();
    indexed.sort_by(|a, b| b.1.cmp(&a.1));

    let phys_to_decl: Box<[u32]> = indexed.iter().map(|(decl, _)| *decl).collect();
    let mut decl_to_phys = vec![0u32; decl_types.len()];
    for (phys, &decl) in phys_to_decl.iter().enumerate() {
        decl_to_phys[decl as usize] = phys as u32;
    }

    AggregateLayout {
        decl_to_phys: decl_to_phys.into_boxed_slice(),
        phys_to_decl,
    }
}

struct LayoutCache {
    map: HashMap<tir::TypeIndex, AggregateLayout>,
}

impl LayoutCache {
    fn new() -> Self {
        LayoutCache {
            map: HashMap::new(),
        }
    }

    fn get_or_compute(
        &mut self,
        type_index: tir::TypeIndex,
        types: &[tir::Type],
        structs: &[tir::Struct],
    ) -> &AggregateLayout {
        self.map
            .entry(type_index)
            .or_insert_with(|| compute_field_order(type_index, types, structs))
    }
}

impl MIR {
    pub fn build(tir: &tir::TIR, interner: &ast::StringInterner) -> MIR {
        // Memory index remap: each memory's DefId → its position in tir.memories
        // which equals the WebAssembly linear-memory index.
        let memory_id_remap: HashMap<ast::DefId, u32> = tir
            .memories
            .iter()
            .enumerate()
            .map(|(i, m)| (m.id, i as u32))
            .collect();

        // Signatures live in the type pool as Type::Function entries.
        let sig_type_indices: Vec<u32> = tir
            .type_pool
            .iter()
            .enumerate()
            .filter(|(_, ty)| matches!(ty, tir::Type::Function { .. }))
            .map(|(i, _)| i as u32)
            .collect();
        let sig_index_remap: HashMap<u32, u32> = sig_type_indices
            .iter()
            .enumerate()
            .map(|(pos, &type_idx)| (type_idx, pos as u32))
            .collect();

        let max_def_id = tir
            .functions
            .iter()
            .map(|f| f.id.as_u32())
            .max()
            .unwrap_or(0);

        let mut builder = Builder {
            tir,
            interner,
            string_pool: StringPool::new(),
            aggregate_pool: AggregatePool::new(),
            layout_cache: LayoutCache::new(),
            memory_id_remap,
            sig_index_remap,
            current_substitutions: Box::new([]),
            mono_registry: MonoRegistry::new(max_def_id + 1),
        };
        let _ = max_def_id;

        // MIR functions: live defined (Internal) monomorphic functions only.
        // Generic functions (type_params non-empty) are lowered on demand by the
        // mono pass below. Wasm index ordering (imports first) is codegen's responsibility.
        let mut functions: Vec<Function> = Vec::new();
        let mut inline_functions: HashSet<ast::DefId> = HashSet::new();
        for func in &tir.functions {
            if func.source == tir::ItemSource::Internal
                && func.body.is_some()
                && !func.accesses.is_empty()
                && func.origin != tir::FunctionOrigin::Trait
                && func.type_params.is_empty()
            {
                if func
                    .attributes
                    .iter()
                    .any(|a| *a == tir::FunctionAttribute::Inline)
                {
                    inline_functions.insert(func.id);
                }
                functions.push(builder.lower_function(func));
            }
        }

        // Monomorphization: drain the registry worklist populated by lower_expression
        // when it encountered calls to generic functions. Each iteration may add new
        // entries (generic-calls-generic), so we loop until the worklist is exhausted.
        let base_sig_count = builder.sig_index_remap.len() as u32;
        let mut mono_sigs: Vec<FunctionSignature> = Vec::new();
        let mut work_cursor = 0;
        loop {
            let current_len = builder.mono_registry.worklist.len();
            if work_cursor >= current_len {
                break;
            }
            let pending = builder.mono_registry.worklist[work_cursor..current_len].to_vec();
            work_cursor = current_len;

            for (orig_id, subst, mono_id) in pending {
                let tir_idx = tir.function_index_lookup[&orig_id];
                let tir_func = &tir.functions[tir_idx as usize];

                builder.current_substitutions = subst;

                // Compute concrete signature while substitutions are active.
                let tir_sig = match &tir.type_pool[tir_func.signature_index as usize] {
                    tir::Type::Function { signature } => signature.clone(),
                    _ => unreachable!(),
                };
                let concrete_sig = FunctionSignature {
                    items: tir_sig
                        .params()
                        .iter()
                        .chain(std::iter::once(&tir_sig.result()))
                        .map(|&ty| builder.lower_type_index(ty))
                        .collect(),
                    params_count: tir_sig.params().len(),
                };
                let concrete_sig_index = base_sig_count + mono_sigs.len() as u32;
                mono_sigs.push(concrete_sig);

                let mut mir_func = builder.lower_function(tir_func);
                mir_func.id = mono_id;
                mir_func.signature_index = concrete_sig_index;

                builder.current_substitutions = Box::new([]);
                functions.push(mir_func);
            }
        }

        let globals: Vec<Global> = tir
            .globals
            .iter()
            .filter(|g| g.source == tir::ItemSource::Internal && g.value.is_some())
            .map(|g| builder.lower_global(g))
            .collect();

        // Build base signatures. Generic (TypeParam-containing) function types get
        // a dummy entry so sig_index_remap stays valid; mono instances use their
        // own concrete entries appended below.
        let mut signatures: Vec<FunctionSignature> = sig_type_indices
            .iter()
            .map(|&type_idx| match &tir.type_pool[type_idx as usize] {
                tir::Type::Function { signature } => {
                    let is_generic =
                        signature.params().iter().any(|&p| {
                            matches!(tir.type_pool[p as usize], tir::Type::TypeParam { .. })
                        }) || matches!(
                            tir.type_pool[signature.result() as usize],
                            tir::Type::TypeParam { .. }
                        );
                    if is_generic {
                        FunctionSignature {
                            items: Box::new([]),
                            params_count: 0,
                        }
                    } else {
                        FunctionSignature {
                            items: signature
                                .params()
                                .iter()
                                .chain(std::iter::once(&signature.result()))
                                .map(|&idx| builder.lower_type_index(idx))
                                .collect(),
                            params_count: signature.params().len(),
                        }
                    }
                }
                _ => unreachable!(),
            })
            .collect();
        signatures.extend(mono_sigs);

        let mut mir = MIR {
            functions,
            inline_functions,
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
                            tir::ImportValue::Function { id } => {
                                let tir_idx = tir.function_index_lookup[id];
                                let tir_func = &tir.functions[tir_idx as usize];
                                let signature_index =
                                    builder.sig_index_remap[&tir_func.signature_index];
                                ImportModuleItem::Function {
                                    name: *symbol,
                                    id: *id,
                                    signature_index,
                                }
                            }
                            tir::ImportValue::Global { id } => ImportModuleItem::Global {
                                name: *symbol,
                                id: *id,
                            },
                            tir::ImportValue::Memory { id } => ImportModuleItem::Memory {
                                name: *symbol,
                                memory_index: builder.memory_id_remap[id],
                            },
                        })
                        .collect(),
                })
                .collect(),
            memories: tir
                .memories
                .iter()
                .filter(|m| m.source == tir::ItemSource::Internal)
                .map(|m| m.kind)
                .collect(),
            exports: {
                let mut exports: Vec<ExportItem> = tir
                    .exports
                    .values()
                    .map(|export| match export {
                        tir::ExportItem::Function {
                            id,
                            external_name,
                            internal_name,
                        } => ExportItem::Function {
                            id: *id,
                            name: external_name
                                .clone()
                                .map(|n| n.inner)
                                .unwrap_or(internal_name.inner),
                        },
                        tir::ExportItem::Global {
                            id,
                            external_name,
                            internal_name,
                        } => ExportItem::Global {
                            id: *id,
                            name: external_name
                                .clone()
                                .map(|n| n.inner)
                                .unwrap_or(internal_name.inner),
                        },
                        tir::ExportItem::Memory {
                            id,
                            external_name,
                            internal_name,
                        } => ExportItem::Memory {
                            memory_index: builder.memory_id_remap[id],
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

/// Tracks which generic function instantiations are needed, assigning each
/// unique `(original_def_id, type_args)` pair a fresh synthetic `DefId`.
struct MonoRegistry {
    map: HashMap<(ast::DefId, Box<[tir::TypeIndex]>), ast::DefId>,
    /// Stable insertion-order worklist; grows as generic-calls-generic paths
    /// are encountered during lowering.
    worklist: Vec<(ast::DefId, Box<[tir::TypeIndex]>, ast::DefId)>,
    next_id: u32,
}

impl MonoRegistry {
    fn new(start_id: u32) -> Self {
        Self {
            map: HashMap::new(),
            worklist: Vec::new(),
            next_id: start_id,
        }
    }

    fn get_or_insert(
        &mut self,
        orig_id: ast::DefId,
        type_args: Box<[tir::TypeIndex]>,
    ) -> ast::DefId {
        if let Some(&id) = self.map.get(&(orig_id, type_args.clone())) {
            return id;
        }
        let id = ast::DefId::new(self.next_id);
        self.next_id += 1;
        self.map.insert((orig_id, type_args.clone()), id);
        self.worklist.push((orig_id, type_args, id));
        id
    }
}

struct Builder<'tir> {
    tir: &'tir tir::TIR,
    interner: &'tir ast::StringInterner,
    string_pool: StringPool,
    aggregate_pool: AggregatePool,
    layout_cache: LayoutCache,
    /// DefId → index in tir.memories (= WebAssembly memory index).
    memory_id_remap: HashMap<ast::DefId, u32>,
    sig_index_remap: HashMap<tir::TypeIndex, SignatureIndex>,
    /// Concrete type substitution for the current generic instantiation.
    /// Indexed by `param_index`: `current_substitutions[i]` is the concrete
    /// `TypeIndex` for `TypeParam { param_index: i }`.
    current_substitutions: Box<[tir::TypeIndex]>,
    mono_registry: MonoRegistry,
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

        match self.tir.type_pool[type_idx as usize].clone() {
            tir::Type::TypeParam { param_index } => {
                let concrete = self.current_substitutions[param_index as usize];
                self.lower_type_index(concrete)
            }
            tir::Type::Function { .. } => {
                let sig_idx = self.sig_index_remap[&type_idx];
                Type::Function {
                    signature_index: sig_idx,
                }
            }
            tir::Type::Memory { .. } | tir::Type::Trait { .. } => Type::Unit,
            tir::Type::Struct { struct_index } => {
                let si = struct_index as usize;
                let decl_types: Vec<tir::TypeIndex> = self.tir.structs[si]
                    .fields
                    .iter()
                    .map(|f| f.ty.inner)
                    .collect();
                let phys_to_decl = self
                    .layout_cache
                    .get_or_compute(type_idx, &self.tir.type_pool, &self.tir.structs)
                    .phys_to_decl
                    .to_vec();
                let fields: Box<[Type]> = phys_to_decl
                    .iter()
                    .map(|&decl| self.lower_type_index(decl_types[decl as usize]))
                    .collect();
                let aggregate_index = self.aggregate_pool.add(Aggregate { values: fields });
                Type::Aggregate { aggregate_index }
            }
            tir::Type::Tuple { elements } => {
                let elements = elements.to_vec();
                let phys_to_decl = self
                    .layout_cache
                    .get_or_compute(type_idx, &self.tir.type_pool, &self.tir.structs)
                    .phys_to_decl
                    .to_vec();

                let fields: Box<[Type]> = phys_to_decl
                    .iter()
                    .map(|&decl| self.lower_type_index(elements[decl as usize]))
                    .collect();
                let aggregate_index = self.aggregate_pool.add(Aggregate { values: fields });
                Type::Aggregate { aggregate_index }
            }
            other => unimplemented!("lower_type_index: unhandled TIR type {:?}", other),
        }
    }

    fn memory_index_from_arg(&self, type_idx: tir::TypeIndex) -> u32 {
        match self.tir.type_pool[type_idx as usize] {
            tir::Type::Memory { id, .. } => self.memory_id_remap[&id],
            _ => unreachable!("intrinsic argument is not a Memory type"),
        }
    }

    fn lower_function(&mut self, func: &tir::Function) -> Function {
        let body = func
            .body
            .as_ref()
            .expect("lower_function called on bodyless function");
        let frame = body
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
        let block = self.lower_expression(&mut ctx, &body.block, &mut top_sink);

        let callers: Box<[ast::DefId]> = func
            .accesses
            .iter()
            .filter_map(|a| a.caller)
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();

        Function {
            id: func.id,
            signature_index: self.sig_index_remap[&func.signature_index],
            scopes: ctx.frame,
            block,
            callers,
        }
    }

    fn lower_global(&mut self, global: &tir::Global) -> Global {
        let mut dummy_ctx = FunctionContext {
            frame: Vec::new(),
            current_scope_index: 0,
        };
        let value_expr = global
            .value
            .as_ref()
            .expect("lower_global called on global without value");

        Global {
            id: global.id,
            ty: self.lower_type_index(global.ty.inner),
            mutability: if global.mut_span.is_some() {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            value: self.lower_expression(&mut dummy_ctx, &value_expr.inner, &mut Vec::new()),
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
            tir::ExprKind::Global { id } => Expression {
                kind: ExprKind::Global { id: *id },
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
            tir::ExprKind::Function { id } => Expression {
                kind: ExprKind::Function { id: *id },
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
            tir::ExprKind::GenericCall { id, type_args, arguments } => {
                let mono_id = self.mono_registry.get_or_insert(*id, type_args.clone());
                let lowered_args: Box<[_]> = arguments
                    .iter()
                    .map(|arg| self.lower_expression(func_ctx, arg, sink))
                    .collect();
                Expression {
                    kind: ExprKind::Call {
                        callee: Box::new(Expression {
                            kind: ExprKind::Function { id: mono_id },
                            ty: Type::Unit,
                        }),
                        arguments: lowered_args,
                    },
                    ty: self.lower_type_index(expr.ty),
                }
            }

            tir::ExprKind::GenericMethodCall {
                id,
                type_args,
                object,
                arguments,
            } => {
                let tir_idx = self.tir.function_index_lookup[id];
                let tir_func = &self.tir.functions[tir_idx as usize];

                // Resolve any TypeParam entries in type_args through active substitutions.
                let resolved: Box<[tir::TypeIndex]> = type_args
                    .iter()
                    .map(|&ty| match &self.tir.type_pool[ty as usize] {
                        tir::Type::TypeParam { param_index } => self
                            .current_substitutions
                            .get(*param_index as usize)
                            .copied()
                            .unwrap_or(ty),
                        _ => ty,
                    })
                    .collect();

                let target_id = if tir_func.body.is_some() {
                    // Default impl: monomorphize with resolved type_args.
                    self.mono_registry.get_or_insert(*id, resolved)
                } else {
                    // Abstract method called inside a default body: the concrete Self
                    // type is now known, so dispatch directly to the impl.
                    let concrete_self = resolved[0];
                    let method_name = tir_func.name.inner;
                    let impl_func_idx = self
                        .tir
                        .impl_members
                        .get(&concrete_self)
                        .and_then(|m| m.get(&method_name))
                        .map(|entry| match entry {
                            tir::ImplEntry::Method(idx) | tir::ImplEntry::AssociatedFn(idx) => {
                                *idx
                            }
                            _ => unreachable!("impl entry for method is not a function"),
                        })
                        .expect("no impl found for abstract trait method");
                    self.tir.functions[impl_func_idx as usize].id
                };

                let lowered_object = self.lower_expression(func_ctx, object, sink);
                let lowered_args: Box<[_]> = std::iter::once(lowered_object)
                    .chain(
                        arguments
                            .iter()
                            .map(|arg| self.lower_expression(func_ctx, arg, sink)),
                    )
                    .collect();
                Expression {
                    kind: ExprKind::Call {
                        callee: Box::new(Expression {
                            kind: ExprKind::Function { id: target_id },
                            ty: Type::Unit,
                        }),
                        arguments: lowered_args,
                    },
                    ty: self.lower_type_index(expr.ty),
                }
            }

            tir::ExprKind::Call { callee, arguments } => {
                // Detect calls to intrinsic functions (e.g. wasm::memory32_grow).
                // TIR resolves module::fn directly to ExprKind::Function { id }.
                if let tir::ExprKind::Function { id } = &callee.kind {
                    let tir_idx = self.tir.function_index_lookup[id];
                    let tir_func = &self.tir.functions[tir_idx as usize];
                    if tir_func
                        .attributes
                        .iter()
                        .any(|a| *a == tir::FunctionAttribute::Intrinsic)
                    {
                        let intrinsic_name = self.interner.resolve(tir_func.name.inner).unwrap();
                        let result_ty = self.lower_type_index(expr.ty);
                        return match intrinsic_name {
                            "memory32_size" => {
                                let memory_index = self.memory_index_from_arg(arguments[0].ty);
                                Expression {
                                    kind: ExprKind::MemorySize { memory_index },
                                    ty: result_ty,
                                }
                            }
                            "memory32_grow" => {
                                let memory_index = self.memory_index_from_arg(arguments[0].ty);
                                let delta =
                                    Box::new(self.lower_expression(func_ctx, &arguments[1], sink));
                                Expression {
                                    kind: ExprKind::MemoryGrow {
                                        memory_index,
                                        delta,
                                    },
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
                id,
            } => {
                if let tir::Type::Memory { .. } = &self.tir.type_pool[object.ty as usize] {
                    let memory_index = match &object.kind {
                        tir::ExprKind::Memory { id } => self.memory_id_remap[id],
                        _ => unreachable!("memory method call on non-memory expression"),
                    };
                    let tir_idx = self.tir.function_index_lookup[id];
                    let method_name = self
                        .interner
                        .resolve(self.tir.functions[tir_idx as usize].name.inner)
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

                let tir_idx = self.tir.function_index_lookup[id];
                let tir_func = &self.tir.functions[tir_idx as usize];
                let callee = Box::new(Expression {
                    kind: ExprKind::Function { id: tir_func.id },
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
                            tir::ImportValue::Function { id } => Expression {
                                kind: ExprKind::Function { id: *id },
                                ty: self.lower_type_index(expr.ty),
                            },
                            tir::ImportValue::Global { id } => Expression {
                                kind: ExprKind::Global { id: *id },
                                ty: self.lower_type_index(expr.ty),
                            },
                            tir::ImportValue::Memory { .. } => {
                                // TIR resolves imported memory namespace accesses to ExprKind::Memory,
                                // not ExprKind::NamespaceAccess, so this arm is never reached.
                                unreachable!("import module NamespaceAccess for Memory")
                            }
                        }
                    }
                    tir::Type::Memory { id, .. } => {
                        let memory_index = self.memory_id_remap[id];
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
                                let member_name = self.interner.resolve(member.inner).unwrap();
                                let layout = compute_layout(
                                    &self.tir.type_pool,
                                    &self.tir.structs,
                                    ty.inner,
                                    PointerSize::Memory32,
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
                        tir::ExprKind::Memory { id } => self.memory_id_remap[id],
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
                let decl_index = self.tir.structs[struct_index as usize].lookup[&member.inner];
                let phys_index = self
                    .layout_cache
                    .get_or_compute(object.ty, &self.tir.type_pool, &self.tir.structs)
                    .decl_to_phys[decl_index];
                let field_ty = self.lower_type_index(expr.ty);

                match &object.kind {
                    tir::ExprKind::Local {
                        scope_index,
                        local_index,
                    } => Expression {
                        kind: ExprKind::AggregateGet {
                            scope_index: *scope_index,
                            local_index: *local_index,
                            value_index: phys_index,
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
                                value_index: phys_index,
                            },
                            ty: field_ty,
                        }
                    }
                }
            }
            tir::ExprKind::StructInit { fields, .. } => {
                // TIR stores fields in declaration order; permute to physical order.
                let decl_to_phys = self
                    .layout_cache
                    .get_or_compute(expr.ty, &self.tir.type_pool, &self.tir.structs)
                    .decl_to_phys
                    .to_vec();
                let mut phys_slots: Vec<Option<Expression>> =
                    (0..fields.len()).map(|_| None).collect();
                for (decl, field) in fields.iter().enumerate() {
                    let phys = decl_to_phys[decl] as usize;
                    phys_slots[phys] = Some(self.lower_expression(func_ctx, field, sink));
                }
                let mir_fields: Box<[Expression]> =
                    phys_slots.into_iter().map(|e| e.unwrap()).collect();
                Expression {
                    kind: ExprKind::Aggregate { values: mir_fields },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::TupleInit { elements } => {
                // Same reordering as StructInit: declaration order → physical (alignment-sorted) order.
                let decl_to_phys = self
                    .layout_cache
                    .get_or_compute(expr.ty, &self.tir.type_pool, &self.tir.structs)
                    .decl_to_phys
                    .to_vec();
                let mut phys_slots: Vec<Option<Expression>> =
                    (0..elements.len()).map(|_| None).collect();
                for (decl, elem) in elements.iter().enumerate() {
                    let phys = decl_to_phys[decl] as usize;
                    phys_slots[phys] = Some(self.lower_expression(func_ctx, elem, sink));
                }
                let mir_elems: Box<[Expression]> =
                    phys_slots.into_iter().map(|e| e.unwrap()).collect();
                Expression {
                    kind: ExprKind::Aggregate { values: mir_elems },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::TupleFieldAccess {
                object,
                field_index,
            } => {
                let phys_index = self
                    .layout_cache
                    .get_or_compute(object.ty, &self.tir.type_pool, &self.tir.structs)
                    .decl_to_phys[*field_index as usize];
                let field_ty = self.lower_type_index(expr.ty);

                match &object.kind {
                    tir::ExprKind::Local {
                        scope_index,
                        local_index,
                    } => Expression {
                        kind: ExprKind::AggregateGet {
                            scope_index: *scope_index,
                            local_index: *local_index,
                            value_index: phys_index,
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
                                value_index: phys_index,
                            },
                            ty: field_ty,
                        }
                    }
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
            tir::ExprKind::Global { id } => ExprKind::GlobalSet {
                id: *id,
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
            tir::ExprKind::Global { id } => ExprKind::GlobalSet {
                id: *id,
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
        ExprKind::GlobalSet { id, value } => ExprKind::GlobalSet {
            id,
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
    callee: &Function,
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
    for scope in callee.scopes.iter().cloned() {
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

    exprs.push(rewrite_body(
        callee.block.clone(),
        body_scope_offset,
        wrapper_scope,
    ));

    Expression {
        ty: result_ty,
        kind: ExprKind::Block {
            scope_index: wrapper_scope,
            expressions: exprs.into_boxed_slice(),
        },
    }
}

/// Walks `expr` in-place (post-order) and replaces every direct call to
/// `inline_id` with `inline_body` inlined at the call site.
fn inline_expr(
    expr: &mut Expression,
    caller_scopes: &mut Vec<BlockScope>,
    inline_id: ast::DefId,
    inline_body: &Function,
) {
    // Recurse into all children first.
    match &mut expr.kind {
        ExprKind::LocalSet { value, .. }
        | ExprKind::GlobalSet { value, .. }
        | ExprKind::Drop { value }
        | ExprKind::Neg { value }
        | ExprKind::BitNot { value }
        | ExprKind::Eqz { value } => inline_expr(value, caller_scopes, inline_id, inline_body),

        ExprKind::Aggregate { values: fields } => {
            for e in fields.iter_mut() {
                inline_expr(e, caller_scopes, inline_id, inline_body);
            }
        }
        ExprKind::Block { expressions, .. } => {
            for e in expressions.iter_mut() {
                inline_expr(e, caller_scopes, inline_id, inline_body);
            }
        }
        ExprKind::Loop { block, .. } => inline_expr(block, caller_scopes, inline_id, inline_body),

        ExprKind::Break { value, .. } | ExprKind::Return { value } => {
            if let Some(v) = value {
                inline_expr(v, caller_scopes, inline_id, inline_body);
            }
        }
        ExprKind::IfElse {
            condition,
            then_block,
            else_block,
        } => {
            inline_expr(condition, caller_scopes, inline_id, inline_body);
            inline_expr(then_block, caller_scopes, inline_id, inline_body);
            if let Some(e) = else_block {
                inline_expr(e, caller_scopes, inline_id, inline_body);
            }
        }
        ExprKind::Call { callee, arguments } => {
            inline_expr(callee, caller_scopes, inline_id, inline_body);
            for a in arguments.iter_mut() {
                inline_expr(a, caller_scopes, inline_id, inline_body);
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
            inline_expr(left, caller_scopes, inline_id, inline_body);
            inline_expr(right, caller_scopes, inline_id, inline_body);
        }
        ExprKind::MemoryGrow { delta, .. } => {
            inline_expr(delta, caller_scopes, inline_id, inline_body)
        }
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

    // After children are processed, check if this node is a call to inline_id.
    let id = match &expr.kind {
        ExprKind::Call { callee, .. } => match &callee.kind {
            ExprKind::Function { id } => *id,
            _ => return,
        },
        _ => return,
    };
    if id != inline_id {
        return;
    }

    let arguments = match std::mem::replace(&mut expr.kind, ExprKind::Noop) {
        ExprKind::Call { arguments, .. } => arguments,
        _ => unreachable!(),
    };
    *expr = inline_call(inline_body, arguments, caller_scopes);
}

/// Directed call graph over MIR function `DefId`s.
///
/// Built from the `callers` field on each `Function` (derived from TIR
/// accesses during lowering) — no expression-tree walk required.
struct CallGraph {
    /// `callees[A]` = functions that A calls.
    callees: HashMap<ast::DefId, HashSet<ast::DefId>>,
    /// `callers[A]` = functions that call A.
    callers: HashMap<ast::DefId, HashSet<ast::DefId>>,
}

impl CallGraph {
    fn build(functions: &[Function]) -> Self {
        let mut callees: HashMap<ast::DefId, HashSet<ast::DefId>> =
            HashMap::with_capacity(functions.len());
        let mut callers: HashMap<ast::DefId, HashSet<ast::DefId>> =
            HashMap::with_capacity(functions.len());
        for f in functions {
            callees.insert(f.id, HashSet::new());
            callers.insert(f.id, HashSet::new());
        }

        for func in functions {
            for &caller_id in func.callers.iter() {
                if let Some(caller_callees) = callees.get_mut(&caller_id) {
                    caller_callees.insert(func.id);
                    callers.get_mut(&func.id).unwrap().insert(caller_id);
                }
            }
        }

        CallGraph { callees, callers }
    }
}

/// Inlines all `#[inline]` functions in topological order, then removes
/// unreachable functions via dead code elimination from export roots.
pub fn run_inlining_pass(mir: &mut MIR) {
    let mut graph = CallGraph::build(&mir.functions);

    // DefId → index in mir.functions for O(1) mutation during inlining.
    let func_idx: HashMap<ast::DefId, usize> = mir
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| (f.id, i))
        .collect();

    // Kahn's algorithm on the inline subgraph:
    // in-degree = number of inline callees not yet processed.
    let mut inline_callee_count: HashMap<ast::DefId, usize> = mir
        .inline_functions
        .iter()
        .map(|&id| {
            let count = graph.callees[&id]
                .iter()
                .filter(|c| mir.inline_functions.contains(c))
                .count();
            (id, count)
        })
        .collect();

    let mut queue: VecDeque<ast::DefId> = inline_callee_count
        .iter()
        .filter(|(_, n)| **n == 0)
        .map(|(&id, _)| id)
        .collect();

    while let Some(f_id) = queue.pop_front() {
        // f's body is clean: all of its inline callees were processed first.
        // Clone once here; inline_call will clone scopes+block again per call site.
        let f_body = mir.functions[func_idx[&f_id]].clone();

        let caller_ids: Vec<ast::DefId> = graph.callers[&f_id].iter().copied().collect();
        for caller_id in caller_ids {
            let ci = func_idx[&caller_id];
            let caller_func = &mut mir.functions[ci];
            inline_expr(
                &mut caller_func.block,
                &mut caller_func.scopes,
                f_id,
                &f_body,
            );

            // Update graph: remove caller → f, propagate f's callees to caller.
            graph.callees.get_mut(&caller_id).unwrap().remove(&f_id);
            graph.callers.get_mut(&f_id).unwrap().remove(&caller_id);
            let f_callees: Vec<ast::DefId> = graph.callees[&f_id].iter().copied().collect();
            for callee_id in f_callees {
                graph.callees.get_mut(&caller_id).unwrap().insert(callee_id);
                graph.callers.get_mut(&callee_id).unwrap().insert(caller_id);
            }

            // If caller is also inline, one of its pending inline callees is done.
            if let Some(count) = inline_callee_count.get_mut(&caller_id) {
                *count -= 1;
                if *count == 0 {
                    queue.push_back(caller_id);
                }
            }
        }
        // graph.callers[f_id] is now empty — f is dead.
    }

    // Dead code elimination: BFS from exported function roots.
    let mut live: HashSet<ast::DefId> = mir
        .exports
        .iter()
        .filter_map(|e| match e {
            ExportItem::Function { id, .. } => Some(*id),
            _ => None,
        })
        .collect();
    let mut dce_queue: VecDeque<ast::DefId> = live.iter().copied().collect();
    while let Some(id) = dce_queue.pop_front() {
        for &callee_id in graph.callees.get(&id).into_iter().flatten() {
            if live.insert(callee_id) {
                dce_queue.push_back(callee_id);
            }
        }
    }
    mir.functions.retain(|f| live.contains(&f.id));
}

#[cfg(test)]
mod tests;
