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
pub type TupleIndex = u32;
pub type StringIndex = u32;

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Debug)]
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
    LocalTupleGet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        field_index: u32,
    },
    /// Get a field from an arbitrary struct/tuple expression.
    TupleFieldGet {
        object: Box<Expression>,
        field_index: u32,
    },
    /// Construct a struct/tuple value from its fields (in declaration order).
    StructCreate {
        fields: Box<[Expression]>,
    },
    LocalSet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    String {
        string_index: StringIndex,
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
    StringPointer,
    Tuple { tuple_index: TupleIndex },
    Function { signature_index: SignatureIndex },
}

#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Debug)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Tuple {
    pub fields: Box<[Type]>,
}

impl std::hash::Hash for Tuple {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for field in self.fields.iter() {
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
    pub tuples: Box<[Tuple]>,
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
pub struct Function {
    pub signature_index: SignatureIndex,
    pub scopes: Vec<BlockScope>,
    pub block: Expression,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
    pub ty: Type,
    pub mutability: Mutability,
    pub value: Expression,
}

struct TuplePool {
    lookup: HashMap<Tuple, TupleIndex>,
    tuples: Vec<Tuple>,
}

impl TuplePool {
    const STRING_TUPLE_INDEX: TupleIndex = 0;

    fn new() -> Self {
        let mut lookup = HashMap::new();
        let string_tuple = Tuple {
            fields: Box::new([Type::StringPointer, Type::U32]),
        };
        lookup.insert(string_tuple.clone(), 0);

        TuplePool {
            lookup,
            tuples: vec![string_tuple],
        }
    }

    fn add(&mut self, tuple: Tuple) -> TupleIndex {
        if let Some(&index) = self.lookup.get(&tuple) {
            index
        } else {
            let index = self.lookup.len() as TupleIndex;
            self.lookup.insert(tuple.clone(), index);
            self.tuples.push(tuple);
            index
        }
    }

    #[allow(dead_code)]
    fn get(&self, index: TupleIndex) -> Option<&Tuple> {
        self.tuples.get(index as usize)
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
            string_pool: StringPool::new(),
            tuple_pool: TuplePool::new(),
            func_index_remap,
            global_index_remap,
            sig_index_remap,
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
            .filter_map(|(tir_index, _)| {
                tir.defined_functions
                    .get(&(tir_index as u32))
                    .map(|func| builder.lower_function(func))
            })
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

        MIR {
            functions,
            globals,
            strings: builder.string_pool.strings.into_boxed_slice(),
            signatures,
            tuples: builder.tuple_pool.tuples.into_boxed_slice(),
            imports: tir
                .namespaces
                .iter()
                .filter_map(|namespace| match namespace {
                    tir::Namespace::ImportModule(module) => Some(ImportModule {
                        name: interner
                            .resolve(module.external_name.inner)
                            .unwrap()
                            .to_string(),
                        items: module
                            .lookup
                            .iter()
                            .map(|(symbol, value)| match value {
                                tir::ImportValue::Function { func_index } => {
                                    let signature_index = tir.declared_functions
                                        [*func_index as usize]
                                        .signature_index;
                                    ImportModuleItem::Function {
                                        name: *symbol,
                                        func_index: builder.func_index_remap[*func_index as usize],
                                        signature_index,
                                    }
                                }
                                tir::ImportValue::Global { global_index } => {
                                    ImportModuleItem::Global {
                                        name: *symbol,
                                        global_index: builder.global_index_remap
                                            [*global_index as usize],
                                    }
                                }
                            })
                            .collect(),
                    }),
                    _ => None,
                })
                .collect(),
            exports: tir
                .exports
                .iter()
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
                })
                .collect(),
        }
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
    string_pool: StringPool,
    tuple_pool: TuplePool,
    func_index_remap: Box<[u32]>,
    global_index_remap: Box<[u32]>,
    /// Maps a TypeIndex (of a Function type in the type pool) to its MIR SignatureIndex.
    sig_index_remap: HashMap<u32, u32>,
}

struct FunctionContext {
    frame: Vec<BlockScope>,
    current_scope_index: usize,
}

impl<'tir> Builder<'tir> {
    /// Lower a `TypeIndex` (index into `tir.type_pool`) to a MIR `Type`.
    /// This is the primary entry point; all call sites should use this.
    fn lower_type_index(&mut self, type_idx: tir::TypeIndex) -> Type {
        match self.tir.type_pool[type_idx as usize].clone() {
            tir::Type::I32 => Type::I32,
            tir::Type::I64 => Type::I64,
            tir::Type::F32 => Type::F32,
            tir::Type::F64 => Type::F64,
            tir::Type::U32 => Type::U32,
            tir::Type::U64 => Type::U64,
            tir::Type::U8 => Type::U8,
            tir::Type::I8 => Type::I8,
            tir::Type::U16 => Type::U16,
            tir::Type::I16 => Type::I16,
            tir::Type::Unit => Type::Unit,
            tir::Type::Never => Type::Never,
            tir::Type::Bool => Type::Bool,
            tir::Type::Char => Type::U32,
            tir::Type::Struct { struct_index } => {
                let field_type_indices: Box<[tir::TypeIndex]> = self.tir.structs
                    [struct_index as usize]
                    .fields
                    .iter()
                    .map(|f| f.ty.inner)
                    .collect();
                let fields: Box<[Type]> = field_type_indices
                    .iter()
                    .map(|&idx| self.lower_type_index(idx))
                    .collect();
                let tuple = Tuple { fields };
                let tuple_index = self.tuple_pool.add(tuple);
                Type::Tuple { tuple_index }
            }
            tir::Type::Function { .. } => {
                let sig_idx = self.sig_index_remap[&type_idx];
                Type::Function {
                    signature_index: sig_idx,
                }
            }
            // Types not yet supported in MIR — map to I32 as a placeholder
            tir::Type::Unknown
            | tir::Type::Error
            | tir::Type::Namespace { .. }
            | tir::Type::Pointer { .. }
            | tir::Type::Array { .. }
            | tir::Type::Slice { .. }
            | tir::Type::Tuple { .. } => Type::I32,
        }
    }

    fn lower_function(&mut self, func: &tir::Function) -> Function {
        let frame = func
            .stack
            .scopes
            .iter()
            .map(|scope| {
                let local_type_indices: Box<[(tir::TypeIndex, Option<ast::TextSpan>)]> =
                    scope.locals.iter().map(|l| (l.ty, l.mut_span)).collect();
                let result_type_idx = scope.inferred_type.unwrap_or(tir::UNIT_IDX);
                let locals = local_type_indices
                    .iter()
                    .map(|&(ty_idx, mut_span)| Local {
                        ty: self.lower_type_index(ty_idx),
                        mutability: if mut_span.is_some() {
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

        let block = self.lower_expression(&mut ctx, &func.block);

        Function {
            signature_index: self.sig_index_remap[&func.signature_index],
            scopes: ctx.frame,
            block,
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
            value: self.lower_expression(&mut dummy_ctx, &global.value.inner),
        }
    }

    fn lower_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &tir::Expression,
    ) -> Expression {
        use crate::ast::{BinaryOp, UnaryOp};

        match &expr.kind {
            tir::ExprKind::Error | tir::ExprKind::Placeholder => unreachable!(),
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
                Expression {
                    kind: ExprKind::String { string_index },
                    ty: Type::Tuple {
                        tuple_index: TuplePool::STRING_TUPLE_INDEX,
                    },
                }
            }
            tir::ExprKind::Return { value } => Expression {
                kind: ExprKind::Return {
                    value: value
                        .as_ref()
                        .map(|v| Box::new(self.lower_expression(func_ctx, v))),
                },
                ty: Type::Never,
            },
            tir::ExprKind::EnumVariant {
                namespace_index,
                variant_index,
            } => match &self.tir.namespaces[*namespace_index as usize] {
                tir::Namespace::Enum(enum_) => {
                    let variant = &enum_.variants[*variant_index as usize];
                    self.lower_expression(func_ctx, &variant.value)
                }
                _ => unreachable!(),
            },
            tir::ExprKind::Call { callee, arguments } => {
                let callee = Box::new(self.lower_expression(func_ctx, callee));
                let arguments = arguments
                    .iter()
                    .map(|arg| self.lower_expression(func_ctx, arg))
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
                let callee = Box::new(Expression {
                    kind: ExprKind::Function {
                        func_index: self.func_index_remap[*func_index as usize],
                    },
                    ty: self.lower_type_index(expr.ty),
                });
                let object = Box::new(self.lower_expression(func_ctx, object));
                let arguments: Box<_> = std::iter::once(*object)
                    .chain(
                        arguments
                            .iter()
                            .map(|arg| self.lower_expression(func_ctx, arg)),
                    )
                    .collect();

                Expression {
                    kind: ExprKind::Call { callee, arguments },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::NamespaceAccess { ty, member, .. } => {
                match &self.tir.type_pool[ty.inner as usize] {
                    tir::Type::Namespace { namespace_index } => {
                        let namespace_index = *namespace_index;
                        match &self.tir.namespaces[namespace_index as usize] {
                            tir::Namespace::Enum(enum_) => {
                                let variant_index = *enum_
                                    .lookup
                                    .get(&member.inner)
                                    .expect("unknown enum variant");
                                let variant = &enum_.variants[variant_index as usize];
                                self.lower_expression(func_ctx, &variant.value)
                            }
                            tir::Namespace::ImportModule(module) => {
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
                                }
                            }
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
                            tir::ImplEntry::AssociatedConst { value, .. } => Expression {
                                kind: ExprKind::Int { value: *value },
                                ty: self.lower_type_index(expr.ty),
                            },
                            _ => todo!("non-const impl member in namespace access"),
                        }
                    }
                }
            }
            tir::ExprKind::ObjectAccess { object, member } => {
                // Resolve the field index from the struct type.
                let struct_index = match &self.tir.type_pool[object.ty as usize] {
                    tir::Type::Struct { struct_index } => *struct_index,
                    _ => unreachable!("ObjectAccess on non-struct type"),
                };
                let field_index =
                    self.tir.structs[struct_index as usize].lookup[&member.inner] as u32;

                // Prefer LocalTupleGet when the object is a plain local — cheaper.
                match &object.kind {
                    tir::ExprKind::Local {
                        scope_index,
                        local_index,
                    } => Expression {
                        kind: ExprKind::LocalTupleGet {
                            scope_index: *scope_index,
                            local_index: *local_index,
                            field_index,
                        },
                        ty: self.lower_type_index(expr.ty),
                    },
                    _ => {
                        let object = Box::new(self.lower_expression(func_ctx, object));
                        Expression {
                            kind: ExprKind::TupleFieldGet {
                                object,
                                field_index,
                            },
                            ty: self.lower_type_index(expr.ty),
                        }
                    }
                }
            }
            tir::ExprKind::StructInit { fields, .. } => {
                let mir_fields: Box<[Expression]> = fields
                    .iter()
                    .map(|f| self.lower_expression(func_ctx, f))
                    .collect();
                Expression {
                    kind: ExprKind::StructCreate { fields: mir_fields },
                    ty: self.lower_type_index(expr.ty),
                }
            }
            tir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let condition = Box::new(self.lower_expression(func_ctx, condition));
                let then_block = Box::new(self.lower_expression(func_ctx, then_block));
                let else_block = else_block
                    .as_ref()
                    .map(|e| Box::new(self.lower_expression(func_ctx, e)));
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
                        .map(|v| Box::new(self.lower_expression(func_ctx, v))),
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
                    block: Box::new(self.lower_expression(func_ctx, block)),
                },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => {
                func_ctx.current_scope_index = *scope_index as usize;
                let mut lowered_exprs: Vec<Expression> = expressions
                    .iter()
                    .map(|e| self.lower_expression(func_ctx, e))
                    .collect();

                if let Some(result) = result {
                    lowered_exprs.push(self.lower_expression(func_ctx, result));
                }

                Expression {
                    kind: ExprKind::Block {
                        scope_index: *scope_index,
                        expressions: lowered_exprs.into_boxed_slice(),
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
                    value: Box::new(self.lower_expression(func_ctx, value)),
                },
                ty: self.lower_type_index(expr.ty),
            },
            tir::ExprKind::Unary { operator, operand } => {
                let operand = Box::new(self.lower_expression(func_ctx, operand));
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
                    // Handle compound assignments by desugaring
                    Assign => {
                        // left = right
                        // This requires left to be an assignable expression
                        self.lower_assignment(func_ctx, left, right)
                    }
                    AddAssign | SubAssign | MulAssign | DivAssign | RemAssign => {
                        // x += y becomes x = x + y
                        self.lower_compound_assignment(func_ctx, operator.inner, left, right)
                    }
                    // Regular binary operations
                    Add => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Add { left, right }
                    }
                    Sub => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Sub { left, right }
                    }
                    Mul => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Mul { left, right }
                    }
                    Div => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Div { left, right }
                    }
                    Rem => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Rem { left, right }
                    }
                    Eq => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Eq { left, right }
                    }
                    NotEq => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::NotEq { left, right }
                    }
                    Less => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Less { left, right }
                    }
                    LessEq => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::LessEq { left, right }
                    }
                    Greater => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Greater { left, right }
                    }
                    GreaterEq => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::GreaterEq { left, right }
                    }
                    And => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::And { left, right }
                    }
                    Or => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::Or { left, right }
                    }
                    BitAnd => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::BitAnd { left, right }
                    }
                    BitOr => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::BitOr { left, right }
                    }
                    BitXor => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::BitXor { left, right }
                    }
                    LeftShift => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
                        ExprKind::LeftShift { left, right }
                    }
                    RightShift => {
                        let left = Box::new(self.lower_expression(func_ctx, left));
                        let right = Box::new(self.lower_expression(func_ctx, right));
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
    ) -> ExprKind {
        match &left.kind {
            tir::ExprKind::Local {
                scope_index,
                local_index,
            } => ExprKind::LocalSet {
                scope_index: *scope_index,
                local_index: *local_index,
                value: Box::new(self.lower_expression(func_ctx, right)),
            },
            tir::ExprKind::Global { global_index } => ExprKind::GlobalSet {
                global_index: self.global_index_remap[*global_index as usize],
                value: Box::new(self.lower_expression(func_ctx, right)),
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
                left: Box::new(self.lower_expression(func_ctx, left)),
                right: Box::new(self.lower_expression(func_ctx, right)),
            },
            Sub => ExprKind::Sub {
                left: Box::new(self.lower_expression(func_ctx, left)),
                right: Box::new(self.lower_expression(func_ctx, right)),
            },
            Mul => ExprKind::Mul {
                left: Box::new(self.lower_expression(func_ctx, left)),
                right: Box::new(self.lower_expression(func_ctx, right)),
            },
            Div => ExprKind::Div {
                left: Box::new(self.lower_expression(func_ctx, left)),
                right: Box::new(self.lower_expression(func_ctx, right)),
            },
            Rem => ExprKind::Rem {
                left: Box::new(self.lower_expression(func_ctx, left)),
                right: Box::new(self.lower_expression(func_ctx, right)),
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

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{ast, tir};

    #[allow(unused)]
    struct TestCase {
        interner: ast::StringInterner,
        files: ast::Files,
        ast: ast::AST,
        tir: tir::TIR,
        mir: MIR,
    }

    impl<'case> TestCase {
        fn new(source: &str) -> Self {
            let mut interner = ast::StringInterner::new();
            let mut files = ast::Files::new();

            let std_source = include_str!("../../../std.wx");
            let std_file_id = files
                .add("std.wx".to_string(), std_source.to_string())
                .unwrap();
            let std_ast = ast::Parser::parse(
                std_file_id,
                &files.get(std_file_id).unwrap().source,
                &mut interner,
            );

            let file_id = files
                .add("main.wx".to_string(), source.to_string())
                .unwrap();
            let ast =
                ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
            let tir = tir::TIR::build(&[&std_ast, &ast], &mut interner);
            let mir = MIR::build(&tir, &interner);

            TestCase {
                interner,
                files,
                ast,
                tir,
                mir,
            }
        }
    }

    #[test]
    fn test_string_field_access_lowered_to_local_tuple_get() {
        let case = TestCase::new(indoc! {"
            import \"console\" {
                fn log(ptr: u32, len: u32);
            }

            fn main() {
                local str = \"test\";
                local length = str.len();
                console::log(0, length);
            }

            export { main }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }

    #[test]
    fn test_char_lowered_to_u32() {
        // char fields should appear as U32 in MIR locals and expressions
        let case = TestCase::new(indoc! {"
            fn identity(c: char) -> char {
                c
            }

            export { identity }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }

    #[test]
    fn test_struct_field_access_lowered_to_local_tuple_get() {
        // ObjectAccess on a local struct → LocalTupleGet
        let case = TestCase::new(indoc! {"
            struct Point {
                x: u32,
                y: u32,
            }

            fn get_x(p: Point) -> u32 {
                p.x
            }

            export { get_x }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }

    #[test]
    fn test_struct_init_lowered_to_struct_create() {
        // StructInit → StructCreate in MIR
        let case = TestCase::new(indoc! {"
            struct Point {
                x: u32,
                y: u32,
            }

            fn make_point(x: u32, y: u32) -> Point {
                Point::{ x: x, y: y }
            }

            export { make_point }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }

    #[test]
    fn test_struct_method_call() {
        // Method calls on structs should stay as Call with the function remapped
        let case = TestCase::new(indoc! {"
            fn to_upper(c: char) -> char {
                c.to_ascii_uppercase()
            }

            export { to_upper }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }

    #[test]
    fn test_global_struct_type() {
        // A global of struct type should be lowered to a Tuple type
        let case = TestCase::new(indoc! {"
            struct Vec2 {
                x: u32,
                y: u32,
            }

            fn get_origin() -> Vec2 {
                Vec2::{ x: 0, y: 0 }
            }

            export { get_origin }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }

    #[test]
    fn test_size_associated_const() {
        // SIZE/ALIGN associated consts (built-in impl) should be lowered to Int in MIR
        let case = TestCase::new(indoc! {"
            fn get_u32_size() -> u32 {
                u32::SIZE
            }

            export { get_u32_size }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }
}
