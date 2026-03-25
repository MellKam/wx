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
    Unit,
    Never,
    Bool,
    StringPointer,
    Tuple { tuple_index: TupleIndex },
    Function { signature_index: SignatureIndex },
}

#[cfg_attr(test, derive(serde::Serialize))]
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

// the role of MIR is to desugar the syntax like x += 1 into x = x + 1 and lower
// the concepts like enums into primitive constants, convert labels from symbols
// in interner into numeric indices

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

    fn get(&self, index: TupleIndex) -> Option<&Tuple> {
        self.tuples.get(index as usize)
    }
}

fn build_index_remap<T>(
    import_count: u32,
    items: &[T],
    get_source: fn(&T) -> tir::ItemSource,
) -> Box<[u32]> {
    let mut tir_to_mir = vec![0u32; items.len()];
    let mut next_imported = 0u32;
    let mut next_defined = import_count;

    for (tir_index, item) in items.iter().enumerate() {
        tir_to_mir[tir_index] = match get_source(item) {
            tir::ItemSource::Imported => {
                let i = next_imported;
                next_imported += 1;
                i
            }
            tir::ItemSource::Defined => {
                let i = next_defined;
                next_defined += 1;
                i
            }
        };
    }

    tir_to_mir.into_boxed_slice()
}

impl MIR {
    pub fn build(tir: &tir::TIR, interner: &ast::StringInterner) -> MIR {
        let func_index_remap = build_index_remap(
            (tir.declared_functions.len() - tir.defined_functions.len()) as u32,
            &tir.declared_functions,
            |f| f.source,
        );
        let global_index_remap = build_index_remap(
            (tir.declared_globals.len() - tir.defined_globals.len()) as u32,
            &tir.declared_globals,
            |g| g.source,
        );

        let mut builder = Builder {
            interner,
            tir,
            string_pool: StringPool::new(),
            tuple_pool: TuplePool::new(),
            func_index_remap,
            global_index_remap,
        };
        let functions = tir
            .declared_functions
            .iter()
            .enumerate()
            .filter(|(_, decl)| decl.source == tir::ItemSource::Defined)
            .map(|(tir_index, _)| {
                let func = tir.defined_functions.get(&(tir_index as u32)).unwrap();
                builder.lower_function(func)
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

        let signatures = tir
            .signatures
            .iter()
            .map(|signature| FunctionSignature {
                items: signature
                    .items
                    .iter()
                    .map(|&ty| builder.lower_type(ty))
                    .collect(),
                params_count: signature.params_count,
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
            let index = self.lookup.len() as StringIndex;
            self.lookup.insert(symbol, index);
            self.strings.push(symbol);
            index
        }
    }

    fn get(&self, index: StringIndex) -> Option<SymbolU32> {
        self.strings.get(index as usize).cloned()
    }
}

struct Builder<'tir, 'interner> {
    interner: &'interner ast::StringInterner,
    tir: &'tir tir::TIR,
    string_pool: StringPool,
    tuple_pool: TuplePool,
    func_index_remap: Box<[u32]>,
    global_index_remap: Box<[u32]>,
}

struct FunctionContext {
    frame: Vec<BlockScope>,
    current_scope_index: usize,
}

impl<'tir, 'interner> Builder<'tir, 'interner> {
    fn lower_type(&self, ty: tir::Type) -> Type {
        match ty {
            tir::Type::I32 => Type::I32,
            tir::Type::I64 => Type::I64,
            tir::Type::F32 => Type::F32,
            tir::Type::F64 => Type::F64,
            tir::Type::U32 => Type::U32,
            tir::Type::U64 => Type::U64,
            tir::Type::Unit => Type::Unit,
            tir::Type::Never => Type::Never,
            tir::Type::Bool => Type::Bool,
            tir::Type::String => Type::Tuple {
                tuple_index: TuplePool::STRING_TUPLE_INDEX,
            },
            tir::Type::Function { signature_index } => Type::Function { signature_index },
            // TODO: handle enums
            tir::Type::Unknown | tir::Type::Error | tir::Type::Namespace { .. } => {
                // These shouldn't appear in valid TIR, but handle gracefully
                Type::I32
            }
        }
    }

    fn lower_function(&mut self, func: &tir::Function) -> Function {
        let frame = func
            .stack
            .scopes
            .iter()
            .map(|scope| self.lower_block_scope(scope))
            .collect();

        let mut ctx = FunctionContext {
            current_scope_index: 0,
            frame,
        };

        let block = self.lower_expression(&mut ctx, &func.block);

        Function {
            signature_index: func.signature_index,
            scopes: ctx.frame,
            block,
        }
    }

    fn lower_block_scope(&self, scope: &tir::BlockScope) -> BlockScope {
        let locals = scope
            .locals
            .iter()
            .map(|local| Local {
                ty: self.lower_type(local.ty),
                mutability: if local.mut_span.is_some() {
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
            result: self.lower_type(scope.inferred_type.unwrap_or(tir::Type::Unit)),
        }
    }

    fn lower_global(&mut self, global: &tir::Global) -> Global {
        // Create a dummy context for the global expression.
        // Constant expressions like literals won't access the local frame.
        let mut dummy_ctx = FunctionContext {
            frame: Vec::new(),
            current_scope_index: 0,
        };

        Global {
            ty: self.lower_type(global.ty.inner),
            mutability: if global.mut_span.is_some() {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            // Now you can safely call lower_expression using the dummy context
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
                ty: self.lower_type(expr.ty),
            },
            tir::ExprKind::Float { value } => Expression {
                kind: ExprKind::Float { value: *value },
                ty: self.lower_type(expr.ty),
            },
            tir::ExprKind::Bool { value } => Expression {
                kind: ExprKind::Bool { value: *value },
                ty: Type::Bool,
            },
            tir::ExprKind::Global { global_index } => Expression {
                kind: ExprKind::Global {
                    global_index: self.global_index_remap[*global_index as usize],
                },
                ty: self.lower_type(expr.ty),
            },
            tir::ExprKind::Local {
                scope_index,
                local_index,
            } => Expression {
                kind: ExprKind::LocalGet {
                    scope_index: *scope_index,
                    local_index: *local_index,
                },
                ty: self.lower_type(expr.ty),
            },
            tir::ExprKind::Function { func_index } => Expression {
                kind: ExprKind::Function {
                    func_index: self.func_index_remap[*func_index as usize],
                },
                ty: self.lower_type(expr.ty),
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
                    ty: self.lower_type(expr.ty),
                }
            }
            tir::ExprKind::NamespaceAccess {
                namespace_index,
                member,
                ..
            } => {
                let namespace = &self.tir.namespaces[*namespace_index as usize];
                match namespace {
                    tir::Namespace::ImportModule(_) => self.lower_expression(func_ctx, member),
                    tir::Namespace::Enum(_) => self.lower_expression(func_ctx, member),
                }
            }
            tir::ExprKind::ObjectAccess { object, member } => {
                todo!();
                // let object = self.lower_expression(func_ctx, object);
                // match object.kind {
                //     ExprKind::String {
                //         scope_index,
                //         local_index,
                //         ..
                //     } => {
                //         let field_index = match
                // self.interner.resolve(member.inner).unwrap() {
                //             "ptr" => 0,
                //             "len" => 1,
                //             _ => unreachable!(),
                //         };

                //         Expression {
                //             kind: ExprKind::LocalTupleGet {
                //                 scope_index,
                //                 local_index,
                //                 field_index,
                //             },
                //             ty: match field_index {
                //                 0 => Type::StringPointer,
                //                 1 => Type::U32,
                //                 _ => unreachable!(),
                //             },
                //         }
                //     }
                //     _ => unreachable!(),
                // }
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
                    ty: self.lower_type(expr.ty),
                }
            }
            tir::ExprKind::Break { scope_index, value } => Expression {
                kind: ExprKind::Break {
                    scope_index: *scope_index,
                    value: value
                        .as_ref()
                        .map(|v| Box::new(self.lower_expression(func_ctx, v))),
                },
                ty: self.lower_type(expr.ty),
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
                ty: self.lower_type(expr.ty),
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
                    ty: self.lower_type(expr.ty),
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
                ty: self.lower_type(expr.ty),
            },
            tir::ExprKind::Unary { operator, operand } => {
                let operand = Box::new(self.lower_expression(func_ctx, operand));
                Expression {
                    kind: match operator.inner {
                        UnaryOp::InvertSign => ExprKind::Neg { value: operand },
                        UnaryOp::Not => ExprKind::Eqz { value: operand },
                        UnaryOp::BitNot => ExprKind::BitNot { value: operand },
                    },
                    ty: self.lower_type(expr.ty),
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
                    ty: self.lower_type(expr.ty),
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
            ty: self.lower_type(left.ty),
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
            let file_id = files
                .add("main.wx".to_string(), source.to_string())
                .unwrap();
            let ast =
                ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
            let tir = tir::TIR::build(&ast, &mut interner);
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
    fn test_parse_simple_addition() {
        let case = TestCase::new(indoc! {"
            import \"console\" {
                fn log(ptr: i32, len: i32) -> unit;
            }

            fn main() -> unit {
                console::log(\"hello world\");
            }

            export { main }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }
}
