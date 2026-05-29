use wx_compiler::ast::{DefId, TextSpan};
use wx_compiler::tir::{EnumVariantIndex, ExportItem, ExprKind, Expression, LocalIndex, ScopeIndex, TIR};
use wx_compiler::vfs::FileId;

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Function(DefId),
    Global(DefId),
    Const(DefId),
    Local { func_id: DefId, scope_idx: ScopeIndex, local_idx: LocalIndex },
    Param { func_id: DefId, param_idx: u32 },
    EnumVariant { enum_idx: u32, variant_idx: EnumVariantIndex },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SymbolUsage {
    Definition,
    Reference,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone)]
pub struct SpanInfo {
    pub file_id: FileId,
    pub span: TextSpan,
    pub kind: SymbolKind,
    pub usage: SymbolUsage,
}

pub struct SymbolIndex {
    entries: Vec<SpanInfo>,
}

impl SymbolIndex {
    fn new() -> Self {
        Self { entries: Vec::new() }
    }

    fn add(&mut self, info: SpanInfo) {
        self.entries.push(info);
    }

    fn build(&mut self) {
        self.entries.sort_by_key(|e| e.span.start);
    }

    pub fn find_at_position(&self, file_id: FileId, pos: u32) -> Option<&SpanInfo> {
        let upper = self.entries.partition_point(|e| e.span.start <= pos);
        self.entries[..upper]
            .iter()
            .rev()
            .filter(|e| e.file_id == file_id && e.span.end >= pos)
            .min_by_key(|e| e.span.end - e.span.start)
    }

    pub fn find_all_references(&self, kind: &SymbolKind) -> Vec<(FileId, TextSpan)> {
        self.entries
            .iter()
            .filter(|e| same_symbol(&e.kind, kind))
            .map(|e| (e.file_id, e.span))
            .collect()
    }

    pub fn find_definition(&self, kind: &SymbolKind) -> Option<(FileId, TextSpan)> {
        self.entries
            .iter()
            .find(|e| same_symbol(&e.kind, kind) && e.usage == SymbolUsage::Definition)
            .map(|e| (e.file_id, e.span))
    }
}

fn same_symbol(a: &SymbolKind, b: &SymbolKind) -> bool {
    match (a, b) {
        (SymbolKind::Function(a), SymbolKind::Function(b)) => a == b,
        (SymbolKind::Global(a), SymbolKind::Global(b)) => a == b,
        (SymbolKind::Const(a), SymbolKind::Const(b)) => a == b,
        (
            SymbolKind::Local { func_id: f1, scope_idx: s1, local_idx: l1 },
            SymbolKind::Local { func_id: f2, scope_idx: s2, local_idx: l2 },
        ) => f1 == f2 && s1 == s2 && l1 == l2,
        (
            SymbolKind::Param { func_id: f1, param_idx: p1 },
            SymbolKind::Param { func_id: f2, param_idx: p2 },
        ) => f1 == f2 && p1 == p2,
        (
            SymbolKind::EnumVariant { enum_idx: e1, variant_idx: v1 },
            SymbolKind::EnumVariant { enum_idx: e2, variant_idx: v2 },
        ) => e1 == e2 && v1 == v2,
        _ => false,
    }
}

pub fn build_symbol_index(tir: &TIR) -> SymbolIndex {
    let mut index = SymbolIndex::new();

    for global in &tir.globals {
        index.add(SpanInfo {
            file_id: global.file_id,
            span: global.name.span,
            kind: SymbolKind::Global(global.id),
            usage: SymbolUsage::Definition,
        });
        if let Some(value) = &global.value {
            index_expression(&mut index, global.file_id, None, &value.inner);
        }
    }

    for function in &tir.functions {
        let func_id = function.id;
        let file_id = function.file_id;

        index.add(SpanInfo {
            file_id,
            span: function.name.span,
            kind: SymbolKind::Function(func_id),
            usage: SymbolUsage::Definition,
        });

        for (param_idx, param) in function.params.iter().enumerate() {
            index.add(SpanInfo {
                file_id,
                span: param.name.span,
                kind: SymbolKind::Param { func_id, param_idx: param_idx as u32 },
                usage: SymbolUsage::Definition,
            });
        }

        if let Some(body) = &function.body {
            for (scope_idx, scope) in body.stack.scopes.iter().enumerate() {
                for (local_idx, local) in scope.locals.iter().enumerate() {
                    if scope_idx == 0 && local_idx < function.params.len() {
                        continue;
                    }
                    index.add(SpanInfo {
                        file_id,
                        span: local.name.span,
                        kind: SymbolKind::Local {
                            func_id,
                            scope_idx: scope_idx as ScopeIndex,
                            local_idx: local_idx as LocalIndex,
                        },
                        usage: SymbolUsage::Definition,
                    });
                }
            }
            index_expression(
                &mut index,
                file_id,
                Some(FunctionContext { func_id, num_params: function.params.len() as u32 }),
                &body.block,
            );
        }
    }

    for (enum_idx, enum_) in tir.enums.iter().enumerate() {
        for (variant_idx, variant) in enum_.variants.iter().enumerate() {
            index.add(SpanInfo {
                file_id: enum_.file_id,
                span: variant.name.span,
                kind: SymbolKind::EnumVariant {
                    enum_idx: enum_idx as u32,
                    variant_idx: variant_idx as EnumVariantIndex,
                },
                usage: SymbolUsage::Definition,
            });
        }
    }

    for constant in &tir.constants {
        if constant.value.is_some() {
            index.add(SpanInfo {
                file_id: constant.file_id,
                span: constant.name.span,
                kind: SymbolKind::Const(constant.id),
                usage: SymbolUsage::Definition,
            });
        }
    }

    for export in tir.exports.values() {
        match export {
            ExportItem::Function { internal_name, id, .. } => {
                if let Some(&fi) = tir.function_index_lookup.get(id) {
                    index.add(SpanInfo {
                        file_id: tir.functions[fi as usize].file_id,
                        span: internal_name.span,
                        kind: SymbolKind::Function(*id),
                        usage: SymbolUsage::Reference,
                    });
                }
            }
            ExportItem::Global { internal_name, id, .. } => {
                if let Some(&gi) = tir.global_index_lookup.get(id) {
                    index.add(SpanInfo {
                        file_id: tir.globals[gi as usize].file_id,
                        span: internal_name.span,
                        kind: SymbolKind::Global(*id),
                        usage: SymbolUsage::Reference,
                    });
                }
            }
            ExportItem::Memory { .. } => {}
        }
    }

    index.build();
    index
}

#[derive(Clone, Copy)]
struct FunctionContext {
    func_id: DefId,
    num_params: u32,
}

fn index_expression(
    index: &mut SymbolIndex,
    file_id: FileId,
    func_ctx: Option<FunctionContext>,
    expr: &Expression,
) {
    match &expr.kind {
        ExprKind::Global { id } => {
            index.add(SpanInfo {
                file_id,
                span: expr.span,
                kind: SymbolKind::Global(*id),
                usage: SymbolUsage::Reference,
            });
        }
        ExprKind::Function { id } => {
            index.add(SpanInfo {
                file_id,
                span: expr.span,
                kind: SymbolKind::Function(*id),
                usage: SymbolUsage::Reference,
            });
        }
        ExprKind::Const { id } => {
            index.add(SpanInfo {
                file_id,
                span: expr.span,
                kind: SymbolKind::Const(*id),
                usage: SymbolUsage::Reference,
            });
        }
        ExprKind::Local { scope_index, local_index } => {
            if let Some(ctx) = func_ctx {
                let kind = if *scope_index == 0 && *local_index < ctx.num_params {
                    SymbolKind::Param { func_id: ctx.func_id, param_idx: *local_index }
                } else {
                    SymbolKind::Local {
                        func_id: ctx.func_id,
                        scope_idx: *scope_index,
                        local_idx: *local_index,
                    }
                };
                index.add(SpanInfo { file_id, span: expr.span, kind, usage: SymbolUsage::Reference });
            }
        }
        ExprKind::EnumVariant { enum_index, variant_index } => {
            index.add(SpanInfo {
                file_id,
                span: expr.span,
                kind: SymbolKind::EnumVariant {
                    enum_idx: *enum_index,
                    variant_idx: *variant_index,
                },
                usage: SymbolUsage::Reference,
            });
        }
        ExprKind::NamespaceAccess { member, .. } => {
            index_expression(index, file_id, func_ctx, member);
        }
        ExprKind::GenericCall { id, arguments, .. } => {
            index.add(SpanInfo {
                file_id,
                span: expr.span,
                kind: SymbolKind::Function(*id),
                usage: SymbolUsage::Reference,
            });
            for arg in arguments.iter() {
                index_expression(index, file_id, func_ctx, arg);
            }
        }
        ExprKind::GenericMethodCall { id, object, arguments, .. } => {
            index.add(SpanInfo {
                file_id,
                span: expr.span,
                kind: SymbolKind::Function(*id),
                usage: SymbolUsage::Reference,
            });
            index_expression(index, file_id, func_ctx, object);
            for arg in arguments.iter() {
                index_expression(index, file_id, func_ctx, arg);
            }
        }
        ExprKind::MethodCall { id, object, arguments } => {
            index.add(SpanInfo {
                file_id,
                span: expr.span,
                kind: SymbolKind::Function(*id),
                usage: SymbolUsage::Reference,
            });
            index_expression(index, file_id, func_ctx, object);
            for arg in arguments.iter() {
                index_expression(index, file_id, func_ctx, arg);
            }
        }
        ExprKind::LocalDeclaration { value, .. } => {
            index_expression(index, file_id, func_ctx, value);
        }
        ExprKind::Call { callee, arguments } => {
            index_expression(index, file_id, func_ctx, callee);
            for arg in arguments.iter() {
                index_expression(index, file_id, func_ctx, arg);
            }
        }
        ExprKind::Unary { operand, .. } => {
            index_expression(index, file_id, func_ctx, operand);
        }
        ExprKind::Binary { left, right, .. } => {
            index_expression(index, file_id, func_ctx, left);
            index_expression(index, file_id, func_ctx, right);
        }
        ExprKind::Block { expressions, result, .. } => {
            for e in expressions.iter() {
                index_expression(index, file_id, func_ctx, e);
            }
            if let Some(r) = result {
                index_expression(index, file_id, func_ctx, r);
            }
        }
        ExprKind::IfElse { condition, then_block, else_block } => {
            index_expression(index, file_id, func_ctx, condition);
            index_expression(index, file_id, func_ctx, then_block);
            if let Some(e) = else_block {
                index_expression(index, file_id, func_ctx, e);
            }
        }
        ExprKind::Loop { block, .. } => {
            index_expression(index, file_id, func_ctx, block);
        }
        ExprKind::Break { value, .. } => {
            if let Some(v) = value {
                index_expression(index, file_id, func_ctx, v);
            }
        }
        ExprKind::Return { value } => {
            if let Some(v) = value {
                index_expression(index, file_id, func_ctx, v);
            }
        }
        ExprKind::ObjectAccess { object, .. } => {
            index_expression(index, file_id, func_ctx, object);
        }
        ExprKind::StructInit { fields, .. } => {
            for f in fields.iter() {
                index_expression(index, file_id, func_ctx, f);
            }
        }
        ExprKind::TupleInit { elements } => {
            for e in elements.iter() {
                index_expression(index, file_id, func_ctx, e);
            }
        }
        ExprKind::TupleFieldAccess { object, .. } => {
            index_expression(index, file_id, func_ctx, object);
        }
        ExprKind::Memory { .. }
        | ExprKind::String { .. }
        | ExprKind::Char { .. }
        | ExprKind::Int { .. }
        | ExprKind::Float { .. }
        | ExprKind::Bool { .. }
        | ExprKind::Continue { .. }
        | ExprKind::Unreachable
        | ExprKind::Placeholder
        | ExprKind::Error => {}
    }
}
