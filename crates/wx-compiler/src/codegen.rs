use crate::{mir, tir};
use leb128fmt;
use std::collections::HashMap;

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
        FunctionSignature {
            param_count: ty.params().len(),
            param_results: ty
                .items
                .into_iter()
                .map(|ty| ValueType::try_from(ty).unwrap())
                .collect(),
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
        type_index: SignatureIndex,
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
enum Mutability {
    Immutable,
    Mutable,
}

#[cfg_attr(test, derive(serde::Serialize))]
struct Global {
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

pub struct Builder {
    expressions: Vec<Expression>,
    table: Vec<FuncIndex>,
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

impl Builder {
    pub fn build<'a>(mir: &mir::MIR) -> Result<WasmModule, ()> {
        let mut builder = Builder {
            expressions: Vec::new(),
            table: Vec::new(),
        };

        let mut signatures = HashMap::<FunctionSignature, SignatureIndex>::new();
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

            let flat_locals: Box<_> = func
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
                scopes: &func.scopes,
            };

            let expressions = expressions
                .iter()
                .map(|expr| builder.build_expression(&mut ctx, expr))
                .collect::<Result<Box<_>, ()>>()?;

            functions.push(FunctionBody {
                locals: ctx.locals,
                expressions,
            });
        }

        Ok(WasmModule {
            globals: GlobalSection {
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
                    let mut sorted_types: Vec<_> = signatures.into_iter().collect();
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

    fn build_expression<'mir, 'wasm>(
        &mut self,
        ctx: &mut FunctionContext<'mir>,
        expr: &mir::Expression,
    ) -> Result<Expression, ()> {
        match &expr.kind {
            mir::ExprKind::Function { func_index } => {
                let table_index = self.table.len() as i32;
                self.table.push(FuncIndex(*func_index));
                Ok(Expression::I32Const { value: table_index })
            }
            mir::ExprKind::Bool { value } => Ok(Expression::I32Const {
                value: if *value { 1 } else { 0 },
            }),
            mir::ExprKind::Call { callee, arguments } => {
                let arguments = arguments
                    .iter()
                    .map(|arg| self.build_expression(ctx, arg))
                    .collect::<Result<Box<_>, _>>()?;

                let expr = match callee.kind {
                    mir::ExprKind::Function { func_index } => Expression::Call {
                        function: FuncIndex(func_index),
                        arguments,
                    },
                    _ => {
                        let type_index = match callee.ty {
                            mir::Type::Function { signature_index } => {
                                SignatureIndex(signature_index)
                            }
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
            mir::ExprKind::Int { value } => match expr.ty {
                mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32Const {
                    value: *value as i32,
                }),
                mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Const { value: *value }),
                _ => Err(()),
            },
            mir::ExprKind::Float { value } => match expr.ty {
                mir::Type::F32 => Ok(Expression::F32Const {
                    value: *value as f32,
                }),
                mir::Type::F64 => Ok(Expression::F64Const { value: *value }),
                _ => Err(()),
            },

            mir::ExprKind::LocalGet {
                local_index,
                scope_index,
            } => Ok(Expression::LocalGet {
                local_index: ctx.get_flat_index(*scope_index, *local_index),
            }),
            mir::ExprKind::LocalSet {
                local_index,
                scope_index,
                value,
            } => Ok(Expression::LocalSet {
                local_index: ctx.get_flat_index(*scope_index, *local_index),
                value: Box::new(self.build_expression(ctx, &value)?),
            }),
            mir::ExprKind::Global { global_index } => Ok(Expression::GlobalGet {
                global_index: GlobalIndex(*global_index),
            }),
            mir::ExprKind::GlobalSet {
                global_index,
                value,
            } => Ok(Expression::GlobalSet {
                global: GlobalIndex(*global_index),
                value: Box::new(self.build_expression(ctx, &value)?),
            }),
            mir::ExprKind::Return { value } => Ok(Expression::Return {
                value: match value {
                    Some(value) => Some(Box::new(self.build_expression(ctx, value)?)),
                    None => None,
                },
            }),
            mir::ExprKind::Drop { value } => match value.ty {
                mir::Type::Never | mir::Type::Unit => Err(()),
                _ => Ok(Expression::Drop {
                    value: Box::new(self.build_expression(ctx, &value)?),
                }),
            },
            mir::ExprKind::Block {
                expressions,
                scope_index,
            } => Ok(Expression::Block {
                expressions: expressions
                    .iter()
                    .map(|expr| {
                        ctx.scope_index = *scope_index;
                        self.build_expression(ctx, expr)
                    })
                    .collect::<Result<_, _>>()?,
                result: match ValueType::try_from(expr.ty) {
                    Ok(ty) => BlockResult::SingleValue(ty),
                    _ => BlockResult::Empty,
                },
            }),
            mir::ExprKind::Loop { scope_index, block } => {
                let expressions = match &block.kind {
                    mir::ExprKind::Block { expressions, .. } => expressions,
                    _ => unreachable!(),
                };

                let loop_expr = Expression::Loop {
                    expressions: expressions
                        .iter()
                        .map(|expr| {
                            ctx.scope_index = *scope_index;
                            self.build_expression(ctx, expr)
                        })
                        .chain(std::iter::once(Ok(Expression::Break {
                            depth: 0, // Loop itself
                            value: None,
                        })))
                        .collect::<Result<_, _>>()?,
                    result: BlockResult::Empty,
                };

                let scope = &ctx.scopes[*scope_index as usize];
                let block_expr = Expression::Block {
                    expressions: Box::new([loop_expr, Expression::Unreachable]),
                    result: match ValueType::try_from(scope.result) {
                        Ok(ty) => BlockResult::SingleValue(ty),
                        _ => BlockResult::Empty,
                    },
                };

                Ok(block_expr)
            }
            mir::ExprKind::Continue { scope_index } => Ok(Expression::Break {
                depth: ctx.get_continue_depth(*scope_index).unwrap(),
                value: None,
            }),
            mir::ExprKind::Break { value, scope_index } => Ok(Expression::Break {
                depth: ctx.get_break_depth(*scope_index).unwrap(),
                value: match value {
                    Some(value) => Some(Box::new(self.build_expression(ctx, value)?)),
                    None => None,
                },
            }),
            mir::ExprKind::Unreachable => Ok(Expression::Unreachable),
            mir::ExprKind::IfElse {
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
                let expr = Expression::IfElse {
                    condition,
                    result: BlockResult::from(expr.ty),
                    then_branch,
                    else_branch,
                };

                Ok(expr)
            }
            mir::ExprKind::Neg { value } => {
                let value = Box::new(self.build_expression(ctx, &value)?);
                match expr.ty {
                    mir::Type::I32 => Ok(Expression::I32Sub {
                        left: Box::new(Expression::I32Const { value: 0 }),
                        right: value,
                    }),
                    mir::Type::I64 => Ok(Expression::I64Sub {
                        left: Box::new(Expression::I64Const { value: 0 }),
                        right: value,
                    }),
                    mir::Type::F32 => Ok(Expression::F32Neg { value }),
                    mir::Type::F64 => Ok(Expression::F64Neg { value }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Add { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32Add { left, right }),
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Add { left, right }),
                    mir::Type::F32 => Ok(Expression::F32Add { left, right }),
                    mir::Type::F64 => Ok(Expression::F64Add { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Sub { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32Sub { left, right }),
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Sub { left, right }),
                    mir::Type::F32 => Ok(Expression::F32Sub { left, right }),
                    mir::Type::F64 => Ok(Expression::F64Sub { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Mul { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32Mul { left, right }),
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Mul { left, right }),
                    mir::Type::F32 => Ok(Expression::F32Mul { left, right }),
                    mir::Type::F64 => Ok(Expression::F64Mul { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Div { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 => Ok(Expression::I32DivS { left, right }),
                    mir::Type::U32 => Ok(Expression::I32DivU { left, right }),
                    mir::Type::I64 => Ok(Expression::I64DivS { left, right }),
                    mir::Type::U64 => Ok(Expression::I64DivU { left, right }),
                    mir::Type::F32 => Ok(Expression::F32Div { left, right }),
                    mir::Type::F64 => Ok(Expression::F64Div { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Rem { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 => Ok(Expression::I32RemS { left, right }),
                    mir::Type::U32 => Ok(Expression::I32RemU { left, right }),
                    mir::Type::I64 => Ok(Expression::I64RemS { left, right }),
                    mir::Type::U64 => Ok(Expression::I64RemU { left, right }),

                    mir::Type::F32 => {
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

                    mir::Type::F64 => {
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

                    _ => Err(()),
                }
            }
            mir::ExprKind::Eq { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match left_ty {
                    mir::Type::Bool
                    | mir::Type::I32
                    | mir::Type::U32
                    | mir::Type::Function { .. } => Ok(Expression::I32Eq { left, right }),

                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Eq { left, right }),

                    mir::Type::F32 => Ok(Expression::F32Eq { left, right }),

                    mir::Type::F64 => Ok(Expression::F64Eq { left, right }),

                    _ => Err(()),
                }
            }
            mir::ExprKind::NotEq { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match left_ty {
                    mir::Type::Bool
                    | mir::Type::I32
                    | mir::Type::U32
                    | mir::Type::Function { .. } => Ok(Expression::I32Ne { left, right }),

                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Ne { left, right }),

                    mir::Type::F32 => Ok(Expression::F32Ne { left, right }),

                    mir::Type::F64 => Ok(Expression::F64Ne { left, right }),

                    _ => Err(()),
                }
            }
            mir::ExprKind::And { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => {
                        Ok(Expression::I32And { left, right })
                    }

                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64And { left, right }),

                    _ => Err(()),
                }
            }
            mir::ExprKind::Or { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => {
                        Ok(Expression::I32Or { left, right })
                    }

                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Or { left, right }),

                    _ => Err(()),
                }
            }
            mir::ExprKind::BitAnd { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32And { left, right }),
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64And { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::BitOr { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32Or { left, right }),
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Or { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::BitXor { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32Xor { left, right }),
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Xor { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::LeftShift { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32Shl { left, right }),
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Shl { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::RightShift { left, right } => {
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match expr.ty {
                    mir::Type::I32 => Ok(Expression::I32ShrS { left, right }),
                    mir::Type::U32 => Ok(Expression::I32ShrU { left, right }),
                    mir::Type::I64 => Ok(Expression::I64ShrS { left, right }),
                    mir::Type::U64 => Ok(Expression::I64ShrU { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Less { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match left_ty {
                    mir::Type::I32 => Ok(Expression::I32LtS { left, right }),
                    mir::Type::U32 => Ok(Expression::I32LtU { left, right }),
                    mir::Type::I64 => Ok(Expression::I64LtS { left, right }),
                    mir::Type::U64 => Ok(Expression::I64LtU { left, right }),
                    mir::Type::F32 => Ok(Expression::F32Lt { left, right }),
                    mir::Type::F64 => Ok(Expression::F64Lt { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::LessEq { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match left_ty {
                    mir::Type::I32 => Ok(Expression::I32LeS { left, right }),
                    mir::Type::U32 => Ok(Expression::I32LeU { left, right }),
                    mir::Type::I64 => Ok(Expression::I64LeS { left, right }),
                    mir::Type::U64 => Ok(Expression::I64LeU { left, right }),
                    mir::Type::F32 => Ok(Expression::F32Le { left, right }),
                    mir::Type::F64 => Ok(Expression::F64Le { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Greater { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match left_ty {
                    mir::Type::I32 => Ok(Expression::I32GtS { left, right }),
                    mir::Type::U32 => Ok(Expression::I32GtU { left, right }),
                    mir::Type::I64 => Ok(Expression::I64GtS { left, right }),
                    mir::Type::U64 => Ok(Expression::I64GtU { left, right }),
                    mir::Type::F32 => Ok(Expression::F32Gt { left, right }),
                    mir::Type::F64 => Ok(Expression::F64Gt { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::GreaterEq { left, right } => {
                let left_ty = left.ty;
                let left = Box::new(self.build_expression(ctx, &left)?);
                let right = Box::new(self.build_expression(ctx, &right)?);

                match left_ty {
                    mir::Type::I32 => Ok(Expression::I32GeS { left, right }),
                    mir::Type::U32 => Ok(Expression::I32GeU { left, right }),
                    mir::Type::I64 => Ok(Expression::I64GeS { left, right }),
                    mir::Type::U64 => Ok(Expression::I64GeU { left, right }),
                    mir::Type::F32 => Ok(Expression::F32Ge { left, right }),
                    mir::Type::F64 => Ok(Expression::F64Ge { left, right }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::BitNot { value } => {
                let value = Box::new(self.build_expression(ctx, &value)?);

                match expr.ty {
                    mir::Type::I32 | mir::Type::U32 => Ok(Expression::I32Xor {
                        left: value,
                        right: Box::new(Expression::I32Const { value: !0 }),
                    }),
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Xor {
                        left: value,
                        right: Box::new(Expression::I64Const { value: !0 }),
                    }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Eqz { value } => {
                let value = Box::new(self.build_expression(ctx, &value)?);

                match expr.ty {
                    mir::Type::Bool | mir::Type::I32 | mir::Type::U32 => {
                        Ok(Expression::I32Eqz { value })
                    }
                    mir::Type::I64 | mir::Type::U64 => Ok(Expression::I64Eqz { value }),
                    _ => Err(()),
                }
            }
            mir::ExprKind::Noop => Ok(Expression::Nop),
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
            Expression::LocalSet {
                local_index: local,
                value,
            } => {
                value.encode(sink, module);
                sink.push(Instruction::LocalSet as u8);
                local.0.encode(sink);
            }
            Expression::GlobalGet {
                global_index: global,
            } => {
                sink.push(Instruction::GlobalGet as u8);
                global.0.encode(sink);
            }
            Expression::GlobalSet { global, value } => {
                value.encode(sink, module);
                sink.push(Instruction::GlobalSet as u8);
                global.0.encode(sink);
            }
            Expression::Return { value } => {
                match value {
                    Some(value) => {
                        value.encode(sink, module);
                    }
                    None => {}
                };
                sink.push(Instruction::Return as u8);
            }
            Expression::I32Add { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32Add as u8);
            }
            Expression::I32Sub { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32Sub as u8);
            }
            Expression::I32Mul { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
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
            Expression::Break { depth, value } => {
                if let Some(value) = value {
                    value.encode(sink, module);
                }
                sink.push(Instruction::Br as u8);
                depth.encode(sink);
            }
            Expression::Call {
                function,
                arguments,
            } => {
                for argument in arguments.iter() {
                    argument.encode(sink, module);
                }
                sink.push(Instruction::Call as u8);
                function.0.encode(sink);
            }
            Expression::CallIndirect {
                expr,
                type_index,
                arguments,
                table_index,
            } => {
                for argument in arguments.iter() {
                    argument.encode(sink, module);
                }
                expr.encode(sink, module);

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
            Expression::I32Eq { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32Eq as u8);
            }
            Expression::I32Eqz { value } => {
                value.encode(sink, module);
                sink.push(Instruction::I32Eqz as u8);
            }
            Expression::I32Ne { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32Ne as u8);
            }
            Expression::I32And { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32And as u8);
            }
            Expression::I32Or { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32Or as u8);
            }
            Expression::IfElse {
                condition,
                result,
                then_branch,
                else_branch,
            } => {
                condition.encode(sink, module);
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
            Expression::Drop { value } => {
                value.encode(sink, module);
                sink.push(Instruction::Drop as u8);
            }
            Expression::I64Add { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64Add as u8);
            }
            Expression::I64Sub { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64Sub as u8);
            }
            Expression::I64Mul { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64Mul as u8);
            }
            Expression::I64Eq { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64Eq as u8);
            }
            Expression::I64Eqz { value } => {
                value.encode(sink, module);
                sink.push(Instruction::I64Eqz as u8);
            }
            Expression::I64Ne { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64Ne as u8);
            }
            Expression::I64And { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64And as u8);
            }
            Expression::I64Or { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64Or as u8);
            }
            Expression::I32DivS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32DivS as u8);
            }
            Expression::I32GeS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32GeS as u8);
            }
            Expression::I32ShrS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32ShrS as u8);
            }
            Expression::I32LtS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32LtS as u8);
            }
            Expression::I32GtS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32GtS as u8);
            }
            Expression::I32LeS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32LeS as u8);
            }
            Expression::I32RemS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32RemS as u8);
            }
            Expression::I64DivS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64DivS as u8);
            }
            Expression::I64GeS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64GeS as u8);
            }
            Expression::I64ShrS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64ShrS as u8);
            }
            Expression::I64LtS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64LtS as u8);
            }
            Expression::I64GtS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64GtS as u8);
            }
            Expression::I64LeS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64LeS as u8);
            }
            Expression::I64RemS { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64RemS as u8);
            }
            Expression::I32Xor { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32Xor as u8);
            }
            Expression::I64Xor { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64Xor as u8);
            }
            Expression::I32Shl { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32Shl as u8);
            }
            Expression::I64Shl { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64Shl as u8);
            }
            Expression::F32Add { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Add as u8);
            }
            Expression::F32Sub { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Sub as u8);
            }
            Expression::F32Mul { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Mul as u8);
            }
            Expression::F64Add { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Add as u8);
            }
            Expression::F64Sub { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Sub as u8);
            }
            Expression::F64Mul { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Mul as u8);
            }
            Expression::F32Eq { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Eq as u8);
            }
            Expression::F32Ne { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Ne as u8);
            }
            Expression::F64Eq { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Eq as u8);
            }
            Expression::F64Ne { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Ne as u8);
            }
            Expression::F32Lt { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Lt as u8);
            }
            Expression::F32Gt { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Gt as u8);
            }
            Expression::F32Le { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Le as u8);
            }
            Expression::F32Ge { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Ge as u8);
            }
            Expression::F64Lt { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Lt as u8);
            }
            Expression::F64Gt { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Gt as u8);
            }
            Expression::F64Le { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Le as u8);
            }
            Expression::F64Ge { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Ge as u8);
            }
            Expression::F32Div { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F32Div as u8);
            }
            Expression::F64Div { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::F64Div as u8);
            }
            Expression::F32Neg { value } => {
                value.encode(sink, module);
                sink.push(Instruction::F32Neg as u8);
            }
            Expression::F64Neg { value } => {
                value.encode(sink, module);
                sink.push(Instruction::F64Neg as u8);
            }
            Expression::I32DivU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32DivU as u8);
            }
            Expression::I32GeU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32GeU as u8);
            }
            Expression::I32ShrU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32ShrU as u8);
            }
            Expression::I32LtU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32LtU as u8);
            }
            Expression::I32GtU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32GtU as u8);
            }
            Expression::I32LeU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32LeU as u8);
            }
            Expression::I32RemU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I32RemU as u8);
            }
            Expression::I64DivU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64DivU as u8);
            }
            Expression::I64GeU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64GeU as u8);
            }
            Expression::I64ShrU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64ShrU as u8);
            }
            Expression::I64LtU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64LtU as u8);
            }
            Expression::I64GtU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64GtU as u8);
            }
            Expression::I64LeU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64LeU as u8);
            }
            Expression::I64RemU { left, right } => {
                left.encode(sink, module);
                right.encode(sink, module);
                sink.push(Instruction::I64RemU as u8);
            }
            Expression::F32Trunc { value } => {
                value.encode(sink, module);
                sink.push(Instruction::F32Trunc as u8);
            }
            Expression::F64Trunc { value } => {
                value.encode(sink, module);
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

impl WasmModule {
    pub fn encode(&self) -> Vec<u8> {
        let mut sink = [
            0x00, 0x61, 0x73, 0x6D, // Magic
            0x01, 0x00, 0x00, 0x00, // Version
        ]
        .to_vec();

        self.types.encode(&mut sink);
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
    use std::os::unix::process;

    use codespan_reporting::term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    };
    use indoc::indoc;

    use crate::{ast, mir, tir};

    use super::*;

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
                    term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
                }
                std::process::exit(1);
            }
            let tir = tir::TIR::build(&ast, &mut interner);
            if tir.diagnostics.len() > 0 {
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                for diagnostic in tir.diagnostics.iter() {
                    term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
                }
                std::process::exit(1);
            }
            let mir = mir::MIR::build(&tir, &interner);
            let wasm = Builder::build(&mir).unwrap();
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
            export fn add(mut a: i32, b: i32) -> i32 { a += b; a }
        "});
        insta::assert_yaml_snapshot!(case.bytecode);

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
            export fn sub(a: i32, b: i32) -> i32 { a - b }
            export fn mul(a: i32, b: i32) -> i32 { a * b }
            export fn div(a: i32, b: i32) -> i32 { a / b }
            export fn rem(a: i32, b: i32) -> i32 { a % b }
        "});

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
            export fn lt(a: i32, b: i32) -> i32 { 
                if a < b { 1 } else { 0 }
            }
            export fn gt(a: i32, b: i32) -> i32 { 
                if a > b { 1 } else { 0 }
            }
            export fn eq(a: i32, b: i32) -> i32 { 
                if a == b { 1 } else { 0 }
            }
            export fn ne(a: i32, b: i32) -> i32 { 
                if a != b { 1 } else { 0 }
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
            export fn max(a: i32, b: i32) -> i32 {
                if a > b { a } else { b }
            }
            export fn abs(a: i32) -> i32 {
                if a < 0 { -a } else { a }
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
            export fn factorial(n: i32) -> i32 {
                local mut result: i32 = 1;
                local mut i: i32 = 1;
                loop {
                    if i > n { break result };
                    result *= i;
                    i += 1;
                }
            }
            export fn sum_to_n(n: i32) -> i32 {
                local mut sum: i32 = 0;
                local mut i: i32 = 1;
                loop {
                    if i > n { break };
                    sum += i;
                    i += 1;
                };
                sum
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
            export fn add64(a: i64, b: i64) -> i64 { a + b }
            export fn mul64(a: i64, b: i64) -> i64 { a * b }
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
            export fn add_f32(a: f32, b: f32) -> f32 { a + b }
            export fn mul_f32(a: f32, b: f32) -> f32 { a * b }
            export fn div_f32(a: f32, b: f32) -> f32 { a / b }
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
            export fn add_f64(a: f64, b: f64) -> f64 { a + b }
            export fn sub_f64(a: f64, b: f64) -> f64 { a - b }
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
            export fn bit_and(a: i32, b: i32) -> i32 { a & b }
            export fn bit_or(a: i32, b: i32) -> i32 { a | b }
            export fn bit_xor(a: i32, b: i32) -> i32 { a ^ b }
            export fn left_shift(a: i32, b: i32) -> i32 { a << b }
            export fn right_shift(a: i32, b: i32) -> i32 { a >> b }
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
            export fn and(a: i32, b: i32) -> i32 { ((a != 0) && (b != 0)) as i32 }
            export fn or(a: i32, b: i32) -> i32 { ((a != 0) || (b != 0)) as i32 }
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
            
            export fn increment() -> i32 {
                global_counter += 1;
                global_counter
            }
            
            export fn get_counter() -> i32 {
                global_counter
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
            export fn fibonacci(n: i32) -> i32 {
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
}
