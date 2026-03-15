use std::collections::HashMap;

use crate::{ast, tir};
use string_interner::symbol::SymbolU32;

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

#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypeSection {
    signatures: Box<[FunctionType]>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, serde::Serialize)]
pub struct TypeIndex(pub u32);

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct TableIndex(pub u32);

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSection {
    types: Box<[TypeIndex]>,
}

#[derive(Debug, Clone, serde::Serialize)]
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

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ExportSection {
    items: Box<[ExportItem]>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionBody {
    name: SymbolU32,
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
enum Mutability {
    Mutable,
    Immutable,
}

#[cfg_attr(test, derive(serde::Serialize))]
struct Global {
    name: SymbolU32,
    ty: ValueType,
    mutability: Mutability,
    value: Expression,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct WasmModule {
    types: TypeSection,
    globals: GlobalSection,
    tables: TableSection,
    elements: ElementSection,
    functions: FunctionSection,
    exports: ExportSection,
    code: CodeSection,
}

struct Builder {
    expressions: Vec<Expression>,
    table: Vec<FuncIndex>,
}

impl TryFrom<tir::Type> for ValueType {
    type Error = ();

    fn try_from(value: tir::Type) -> Result<Self, Self::Error> {
        match value {
            tir::Type::Bool => Ok(ValueType::I32),
            tir::Type::I32 => Ok(ValueType::I32),
            tir::Type::I64 => Ok(ValueType::I64),
            tir::Type::F32 => Ok(ValueType::F32),
            tir::Type::F64 => Ok(ValueType::F64),
            tir::Type::U32 => Ok(ValueType::I32),
            tir::Type::U64 => Ok(ValueType::I64),
            tir::Type::Function { .. } => Ok(ValueType::I32),
            _ => Err(()),
        }
    }
}

impl From<tir::Type> for BlockResult {
    fn from(value: tir::Type) -> Self {
        ValueType::try_from(value)
            .map(BlockResult::SingleValue)
            .unwrap_or(BlockResult::Empty)
    }
}

#[derive(Debug)]
struct FunctionContext<'mir> {
    locals: Box<[Local]>,
    scope_offsets: Box<[usize]>,
    scopes: &'mir Vec<tir::BlockScope>,
    scope_index: tir::ScopeIndex,
}

impl FunctionContext<'_> {
    fn get_flat_index(
        &self,
        scope_index: tir::ScopeIndex,
        local_index: tir::LocalIndex,
    ) -> LocalIndex {
        let scope_offset = self.scope_offsets[scope_index as usize];
        LocalIndex(scope_offset as u32 + local_index)
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

impl From<tir::FunctionSignature> for FunctionType {
    fn from(ty: tir::FunctionSignature) -> Self {
        FunctionType {
            param_count: ty.params().len(),
            param_results: ty
                .items
                .into_iter()
                .map(|ty| ValueType::try_from(ty).unwrap())
                .collect(),
        }
    }
}

impl Builder {
    pub fn build<'a>(tir: &tir::TIR) -> Result<WasmModule, ()> {
        let mut builder = Builder {
            expressions: Vec::new(),
            table: Vec::new(),
        };

        let mut types = HashMap::<FunctionType, TypeIndex>::new();
        let mut function_signatures = Vec::<TypeIndex>::with_capacity(tir.functions.len());
        let exports: Box<_> = tir
            .exports
            .iter()
            .map(|item| match item {
                tir::ExportItem::Global { global_index } => {
                    let global = tir.globals.get(*global_index as usize).unwrap();

                    ExportItem::Global {
                        name: global.name.inner,
                        global_index: GlobalIndex(*global_index),
                    }
                }
                tir::ExportItem::Function { func_index } => {
                    let func = tir.functions.get(*func_index as usize).unwrap();

                    ExportItem::Function {
                        name: func.name.inner,
                        func_index: FuncIndex(*func_index),
                    }
                }
            })
            .collect();
        let mut functions = Vec::<FunctionBody>::with_capacity(tir.functions.len());

        for func in tir.functions.iter() {
            let ty = FunctionType::from(
                tir.signatures
                    .get(func.signature_index as usize)
                    .cloned()
                    .unwrap(),
            );
            let next_type_index = TypeIndex(types.len() as u32);
            let type_index = types.entry(ty).or_insert(next_type_index).clone();
            function_signatures.push(type_index);

            let expressions = match &func.block.kind {
                tir::ExprKind::Block { expressions, .. } => expressions,
                _ => unreachable!(),
            };

            let scope_offsets: Box<_> = func
                .stack
                .scopes
                .iter()
                .scan(0, |offset, scope| {
                    let current = *offset;
                    *offset += scope.locals.len();
                    Some(current)
                })
                .collect();

            let flat_locals: Box<_> = func
                .stack
                .scopes
                .iter()
                .flat_map(|scope| {
                    scope.locals.iter().map(|local| Local {
                        ty: ValueType::try_from(local.ty).unwrap(),
                    })
                })
                .collect();

            let mut ctx = FunctionContext {
                locals: flat_locals,
                scope_offsets,
                scope_index: 0 as tir::ScopeIndex,
                scopes: &func.stack.scopes,
            };

            let expressions = expressions
                .iter()
                .map(|expr| builder.build_expression(&mut ctx, expr))
                .collect::<Result<Box<_>, ()>>()?;

            functions.push(FunctionBody {
                name: func.name.inner,
                locals: ctx.locals,
                expressions,
            });
        }

        Ok(WasmModule {
            globals: GlobalSection {
                globals: tir
                    .globals
                    .iter()
                    .map(|global| Global {
                        name: global.name.inner,
                        ty: ValueType::try_from(global.ty.inner).unwrap(),
                        mutability: match global.mut_span {
                            Some(_) => Mutability::Mutable,
                            None => Mutability::Immutable,
                        },
                        value: builder.build_global_expr(global),
                    })
                    .collect::<Box<_>>(),
            },
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
            types: TypeSection {
                signatures: {
                    let mut sorted_types: Vec<_> = types.into_iter().collect();
                    sorted_types.sort_by_key(|&(_, index)| index);
                    sorted_types
                        .into_iter()
                        .map(|(ty, _)| ty)
                        .collect::<Box<_>>()
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

    fn build_global_expr(&self, global: &tir::Global) -> Expression {
        match global.value.kind {
            tir::ExprKind::Int { value } => match global.ty.inner {
                tir::Type::I32 => Expression::I32Const {
                    value: value as i32,
                },
                tir::Type::I64 => Expression::I64Const { value },
                _ => unreachable!(),
            },
            tir::ExprKind::Float { value } => match global.ty.inner {
                tir::Type::F32 => Expression::F32Const {
                    value: value as f32,
                },
                tir::Type::F64 => Expression::F64Const { value },
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn build_expression<'tir, 'wasm>(
        &mut self,
        ctx: &mut FunctionContext<'tir>,
        expr: &tir::Expression,
    ) -> Result<Expression, ()> {
        match &expr.kind {
            tir::ExprKind::Function { func_index } => {
                let table_index = self.table.len() as i32;
                self.table.push(FuncIndex(*func_index));
                Ok(Expression::I32Const { value: table_index })
            }
            tir::ExprKind::Bool { value } => Ok(Expression::I32Const {
                value: if *value { 1 } else { 0 },
            }),
            tir::ExprKind::Call { callee, arguments } => {
                let arguments = arguments
                    .iter()
                    .map(|arg| self.build_expression(ctx, arg))
                    .collect::<Result<Box<_>, _>>()?;

                let expr = match callee.kind {
                    tir::ExprKind::Function { func_index } => Expression::Call {
                        function: FuncIndex(func_index),
                        arguments,
                    },
                    _ => {
                        let type_index = match callee.ty {
                            tir::Type::Function { signature_index } => TypeIndex(signature_index),
                            _ => unreachable!("callee must be a function type"),
                        };
                        let function = self.build_expression(ctx, callee)?;

                        Expression::CallIndirect {
                            expr: Box::new(function),
                            table_index: TableIndex(0),
                            type_index,
                            arguments,
                        }
                    }
                };
                Ok(expr)
            }
            tir::ExprKind::Int { value } => {
                let expr = match expr.ty {
                    tir::Type::I32 | tir::Type::U32 => Expression::I32Const {
                        value: *value as i32,
                    },
                    tir::Type::I64 | tir::Type::U64 => Expression::I64Const { value: *value },
                    _ => return Err(()),
                };
                Ok(expr)
            }
            tir::ExprKind::Float { value } => {
                let expr = match expr.ty {
                    tir::Type::F32 => Expression::F32Const {
                        value: *value as f32,
                    },
                    tir::Type::F64 => Expression::F64Const { value: *value },
                    _ => return Err(()),
                };
                Ok(expr)
            }

            tir::ExprKind::LocalGet {
                local_index,
                scope_index,
            } => Ok(wasm::Expression::LocalGet {
                local_index: ctx.get_flat_index(*scope_index, *local_index),
            }),
            tir::ExprKind::LocalSet {
                local_index,
                scope_index,
                value,
            } => Ok(wasm::Expression::LocalSet {
                local_index: ctx.get_flat_index(*scope_index, *local_index),
                value: Box::new(self.build_expression(ctx, &value)?),
            }),
            tir::ExprKind::Global { global_index } => Ok(wasm::Expression::GlobalGet {
                global_index: wasm::GlobalIndex(*global_index),
            }),
            tir::ExprKind::GlobalSet {
                global_index,
                value,
            } => Ok(wasm::Expression::GlobalSet {
                global: wasm::GlobalIndex(*global_index),
                value: Box::new(self.build_expression(ctx, &value)?),
            }),
            tir::ExprKind::Return { value } => Ok(wasm::Expression::Return {
                value: match value {
                    Some(value) => Some(Box::new(self.build_expression(ctx, value)?)),
                    None => None,
                },
            }),
            tir::ExprKind::Drop { value } => match value.ty {
                tir::Type::Never | tir::Type::Unit => Err(()),
                _ => Ok(wasm::Expression::Drop {
                    value: Box::new(self.build_expression(ctx, &value)?),
                }),
            },
            tir::ExprKind::Block {
                expressions,
                scope_index,
            } => {
                let expr = wasm::Expression::Block {
                    expressions: expressions
                        .iter()
                        .map(|expr| {
                            ctx.scope_index = tir::ScopeIndex(scope_index.0);
                            self.build_expression(ctx, expr)
                        })
                        .collect::<Result<_, _>>()?,
                    result: match wasm::ValueType::try_from(expr.ty) {
                        Ok(ty) => wasm::BlockResult::SingleValue(ty),
                        _ => wasm::BlockResult::Empty,
                    },
                };
                Ok(expr)
            }
            tir::ExprKind::Loop { scope_index, block } => {
                let expressions = match &block.kind {
                    tir::ExprKind::Block { expressions, .. } => expressions,
                    _ => unreachable!(),
                };

                let loop_expr = wasm::Expression::Loop {
                    expressions: expressions
                        .iter()
                        .map(|expr| {
                            ctx.scope_index = tir::ScopeIndex(scope_index.0);
                            self.build_expression(ctx, expr)
                        })
                        .chain(std::iter::once(Ok(wasm::Expression::Break {
                            depth: 0, // Loop itself
                            value: None,
                        })))
                        .collect::<Result<_, _>>()?,
                    result: wasm::BlockResult::Empty,
                };

                let scope = &ctx.scopes[scope_index.0 as usize];
                let block_expr = wasm::Expression::Block {
                    expressions: Box::new([loop_expr, wasm::Expression::Unreachable]),
                    result: match wasm::ValueType::try_from(scope.result) {
                        Ok(ty) => wasm::BlockResult::SingleValue(ty),
                        _ => wasm::BlockResult::Empty,
                    },
                };

                Ok(block_expr)
            }
            tir::ExprKind::Continue { scope_index } => {
                let expr = wasm::Expression::Break {
                    depth: ctx.get_continue_depth(*scope_index).unwrap(),
                    value: None,
                };
                Ok(expr)
            }
            tir::ExprKind::Break { value, scope_index } => {
                let expr = wasm::Expression::Break {
                    depth: ctx.get_break_depth(*scope_index).unwrap(),
                    value: match value {
                        Some(value) => Some(Box::new(self.build_expression(ctx, value)?)),
                        None => None,
                    },
                };
                Ok(expr)
            }
            tir::ExprKind::Unreachable => Ok(wasm::Expression::Unreachable),
            tir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let condition = Box::new(self.build_expression(ctx, &condition)?);
                let then_branch = Box::new(self.build_expression(ctx, &then_block)?);
                let else_branch = match else_block {
                    Some(else_block) => Some(Box::new(self.build_expression(ctx, else_block)?)),
                    None => None,
                };
                let expr = wasm::Expression::IfElse {
                    condition,
                    result: wasm::BlockResult::from(expr.ty),
                    then_branch,
                    else_branch,
                };

                Ok(expr)
            }

            tir::ExprKind::Neg { value } => {
                let value = Box::new(self.build_expression(ctx, &value)?);
                let expr = match expr.ty {
                    tir::Type::I32 => wasm::Expression::I32Sub {
                        left: Box::new(wasm::Expression::I32Const { value: 0 }),
                        right: value,
                    },
                    tir::Type::I64 => wasm::Expression::I64Sub {
                        left: Box::new(wasm::Expression::I64Const { value: 0 }),
                        right: value,
                    },
                    tir::Type::F32 => wasm::Expression::F32Neg { value },
                    tir::Type::F64 => wasm::Expression::F64Neg { value },
                    tir::Type::U32
                    | tir::Type::U64
                    | tir::Type::Bool
                    | tir::Type::Function(_)
                    | tir::Type::Unit
                    | tir::Type::Never => {
                        return Err(());
                    }
                };

                Ok(expr)
            }
        }
    }

    fn build_binary_expression<'tir>(
        &mut self,
        ctx: &mut FunctionContext<'tir>,
        expr: &tir::Expression,
    ) -> Result<Expression, ()> {
        let (operator, left, right) = match &expr.kind {
            tir::ExprKind::Binary {
                operator,
                left,
                right,
            } => (operator, left, right),
            _ => unreachable!(),
        };

        let left_ty = left.ty;
        let right_ty = right.ty;
        let left = Box::new(self.build_expression(ctx, &left)?);
        let right = Box::new(self.build_expression(ctx, &right)?);

        match operator.inner {
            ast::BinaryOp::Add => match expr.ty {
                tir::Type::I32 | tir::Type::U32 => Ok(Expression::I32Add { left, right }),
                tir::Type::I64 | tir::Type::U64 => Ok(Expression::I64Add { left, right }),
                tir::Type::F32 => Ok(Expression::F32Add { left, right }),
                tir::Type::F64 => Ok(Expression::F64Add { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Sub => match expr.ty {
                tir::Type::I32 | tir::Type::U32 => Ok(Expression::I32Sub { left, right }),
                tir::Type::I64 | tir::Type::U64 => Ok(Expression::I64Sub { left, right }),
                tir::Type::F32 => Ok(Expression::F32Sub { left, right }),
                tir::Type::F64 => Ok(Expression::F64Sub { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Mul => match expr.ty {
                tir::Type::I32 | tir::Type::U32 => Ok(Expression::I32Mul { left, right }),
                tir::Type::I64 | tir::Type::U64 => Ok(Expression::I64Mul { left, right }),
                tir::Type::F32 => Ok(Expression::F32Mul { left, right }),
                tir::Type::F64 => Ok(Expression::F64Mul { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Div => match expr.ty {
                tir::Type::I32 => Ok(Expression::I32DivS { left, right }),
                tir::Type::I64 => Ok(Expression::I64DivS { left, right }),
                tir::Type::F32 => Ok(Expression::F32Div { left, right }),
                tir::Type::F64 => Ok(Expression::F64Div { left, right }),
                tir::Type::U32 => Ok(Expression::I32DivU { left, right }),
                tir::Type::U64 => Ok(Expression::I64DivU { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Rem => match expr.ty {
                tir::Type::I32 => Ok(Expression::I32RemS { left, right }),
                tir::Type::I64 => Ok(Expression::I64RemS { left, right }),
                tir::Type::F32 => {
                    let trunc = Box::new(Expression::F32Trunc {
                        value: Box::new(Expression::F32Div {
                            left: left.clone(),
                            right: right.clone(),
                        }),
                    });
                    Ok(Expression::F32Sub {
                        left,
                        right: Box::new(Expression::F32Mul { left: trunc, right }),
                    })
                }
                tir::Type::F64 => {
                    let trunc = Box::new(Expression::F64Trunc {
                        value: Box::new(Expression::F64Div {
                            left: left.clone(),
                            right: right.clone(),
                        }),
                    });
                    Ok(Expression::F64Sub {
                        left,
                        right: Box::new(Expression::F64Mul { left: trunc, right }),
                    })
                }
                tir::Type::U32 => Ok(Expression::I32RemU { left, right }),
                tir::Type::U64 => Ok(Expression::I64RemU { left, right }),
                _ => return Err(()),
            },
            ast::BinaryOp::NotEq => match left_ty {
                tir::Type::Bool | tir::Type::I32 | tir::Type::U32 | tir::Type::Function { .. } => {
                    Ok(Expression::I32Ne { left, right })
                }
                tir::Type::I64 | tir::Type::U64 => Ok(Expression::I64Ne { left, right }),
                tir::Type::F32 => Ok(Expression::F32Ne { left, right }),
                tir::Type::F64 => Ok(Expression::F64Ne { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Eq => match left_ty {
                tir::Type::I32 | tir::Type::U32 | tir::Type::Bool | tir::Type::Function { .. } => {
                    Ok(Expression::I32Eq { left, right })
                }
                tir::Type::I64 | tir::Type::U64 => Ok(Expression::I64Eq { left, right }),
                tir::Type::F32 => Ok(Expression::F32Eq { left, right }),
                tir::Type::F64 => Ok(Expression::F64Eq { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::And => match left_ty {
                tir::Type::Bool | tir::Type::I32 | tir::Type::U32 => {
                    Ok(Expression::I32And { left, right })
                }
                tir::Type::I64 | tir::Type::U64 => Ok(Expression::I64And { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Or => match left_ty {
                tir::Type::Bool | tir::Type::I32 | tir::Type::U32 => {
                    Ok(Expression::I32Or { left, right })
                }
                tir::Type::I64 | tir::Type::U64 => Ok(Expression::I64Or { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::BitAnd => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) | (tir::Type::U32, tir::Type::U32) => {
                    Ok(Expression::I32And { left, right })
                }
                (tir::Type::I64, tir::Type::I64) | (tir::Type::U64, tir::Type::U64) => {
                    Ok(Expression::I64And { left, right })
                }
                _ => Err(()),
            },
            ast::BinaryOp::BitOr => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) | (tir::Type::U32, tir::Type::U32) => {
                    Ok(Expression::I32Or { left, right })
                }
                (tir::Type::I64, tir::Type::I64) | (tir::Type::U64, tir::Type::U64) => {
                    Ok(Expression::I64Or { left, right })
                }
                _ => Err(()),
            },
            ast::BinaryOp::BitXor => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) | (tir::Type::U32, tir::Type::U32) => {
                    Ok(Expression::I32Xor { left, right })
                }
                (tir::Type::I64, tir::Type::I64) | (tir::Type::U64, tir::Type::U64) => {
                    Ok(Expression::I64Xor { left, right })
                }
                _ => Err(()),
            },
            ast::BinaryOp::LeftShift => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) | (tir::Type::U32, tir::Type::U32) => {
                    Ok(Expression::I32Shl { left, right })
                }
                (tir::Type::I64, tir::Type::I64) => Ok(Expression::I64Shl { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::RightShift => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) => Ok(Expression::I32ShrS { left, right }),
                (tir::Type::I64, tir::Type::I64) => Ok(Expression::I64ShrS { left, right }),
                (tir::Type::U32, tir::Type::U32) => Ok(Expression::I32ShrU { left, right }),
                (tir::Type::U64, tir::Type::U64) => Ok(Expression::I64ShrU { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Less => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) => Ok(Expression::I32LtS { left, right }),
                (tir::Type::I64, tir::Type::I64) => Ok(Expression::I64LtS { left, right }),
                (tir::Type::F32, tir::Type::F32) => Ok(Expression::F32Lt { left, right }),
                (tir::Type::F64, tir::Type::F64) => Ok(Expression::F64Lt { left, right }),
                (tir::Type::U32, tir::Type::U32) => Ok(Expression::I32LtU { left, right }),
                (tir::Type::U64, tir::Type::U64) => Ok(Expression::I64LtU { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::LessEq => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) => Ok(Expression::I32LeS { left, right }),
                (tir::Type::I64, tir::Type::I64) => Ok(Expression::I64LeS { left, right }),
                (tir::Type::F32, tir::Type::F32) => Ok(Expression::F32Le { left, right }),
                (tir::Type::F64, tir::Type::F64) => Ok(Expression::F64Le { left, right }),
                (tir::Type::U32, tir::Type::U32) => Ok(Expression::I32LeU { left, right }),
                (tir::Type::U64, tir::Type::U64) => Ok(Expression::I64LeU { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Greater => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) => Ok(Expression::I32GtS { left, right }),
                (tir::Type::I64, tir::Type::I64) => Ok(Expression::I64GtS { left, right }),
                (tir::Type::F32, tir::Type::F32) => Ok(Expression::F32Gt { left, right }),
                (tir::Type::F64, tir::Type::F64) => Ok(Expression::F64Gt { left, right }),
                (tir::Type::U32, tir::Type::U32) => Ok(Expression::I32GtU { left, right }),
                (tir::Type::U64, tir::Type::U64) => Ok(Expression::I64GtU { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::GreaterEq => match (left_ty, right_ty) {
                (tir::Type::I32, tir::Type::I32) => Ok(Expression::I32GeS { left, right }),
                (tir::Type::I64, tir::Type::I64) => Ok(Expression::I64GeS { left, right }),
                (tir::Type::F32, tir::Type::F32) => Ok(Expression::F32Ge { left, right }),
                (tir::Type::F64, tir::Type::F64) => Ok(Expression::F64Ge { left, right }),
                (tir::Type::U32, tir::Type::U32) => Ok(Expression::I32GeU { left, right }),
                (tir::Type::U64, tir::Type::U64) => Ok(Expression::I64GeU { left, right }),
                _ => Err(()),
            },
            ast::BinaryOp::Assign => {}
            ast::BinaryOp::AddAssign => {}
            ast::BinaryOp::SubAssign => {}
            ast::BinaryOp::MulAssign => {}
            ast::BinaryOp::DivAssign => {}
            ast::BinaryOp::RemAssign => {}
        }
    }
}
