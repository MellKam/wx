use wx_compiler::ast::{DefId, TextSpan};
use wx_compiler::tir::{EnumVariantIndex, ExportItem, LocalIndex, ScopeIndex, TraitIndex, TIR};
use wx_compiler::vfs::FileId;

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Function(DefId),
    Global(DefId),
    Const(DefId),
    Enum(u32),
    Struct(u32),
    Module(u32),
    ImportModule(u32),
    Local { func_id: DefId, scope_idx: ScopeIndex, local_idx: LocalIndex },
    Param { func_id: DefId, param_idx: u32 },
    EnumVariant { enum_idx: u32, variant_idx: EnumVariantIndex },
    Label { func_id: DefId, scope_idx: ScopeIndex },
    Trait(TraitIndex),
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
        (SymbolKind::Enum(a), SymbolKind::Enum(b)) => a == b,
        (SymbolKind::Struct(a), SymbolKind::Struct(b)) => a == b,
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
        (
            SymbolKind::Label { func_id: f1, scope_idx: s1 },
            SymbolKind::Label { func_id: f2, scope_idx: s2 },
        ) => f1 == f2 && s1 == s2,
        (SymbolKind::Module(a), SymbolKind::Module(b)) => a == b,
        (SymbolKind::ImportModule(a), SymbolKind::ImportModule(b)) => a == b,
        (SymbolKind::Trait(a), SymbolKind::Trait(b)) => a == b,
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
        for access in &global.accesses {
            index.add(SpanInfo {
                file_id: access.file_id,
                span: access.span,
                kind: SymbolKind::Global(global.id),
                usage: SymbolUsage::Reference,
            });
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

        for access in &function.accesses {
            index.add(SpanInfo {
                file_id: access.file_id,
                span: access.span,
                kind: SymbolKind::Function(func_id),
                usage: SymbolUsage::Reference,
            });
        }

        let num_params = function.params.len();
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
                if let Some(label) = &scope.label {
                    let kind = SymbolKind::Label { func_id, scope_idx: scope_idx as ScopeIndex };
                    index.add(SpanInfo {
                        file_id,
                        span: label.span,
                        kind: kind.clone(),
                        usage: SymbolUsage::Definition,
                    });
                    for &access_span in &label.accesses {
                        index.add(SpanInfo {
                            file_id,
                            span: access_span,
                            kind: kind.clone(),
                            usage: SymbolUsage::Reference,
                        });
                    }
                }
                for (local_idx, local) in scope.locals.iter().enumerate() {
                    let is_param = scope_idx == 0 && local_idx < num_params;
                    let kind = if is_param {
                        SymbolKind::Param { func_id, param_idx: local_idx as u32 }
                    } else {
                        SymbolKind::Local {
                            func_id,
                            scope_idx: scope_idx as ScopeIndex,
                            local_idx: local_idx as LocalIndex,
                        }
                    };
                    if !is_param {
                        index.add(SpanInfo {
                            file_id,
                            span: local.name.span,
                            kind: kind.clone(),
                            usage: SymbolUsage::Definition,
                        });
                    }
                    for access in &local.accesses {
                        index.add(SpanInfo {
                            file_id,
                            span: access.span,
                            kind: kind.clone(),
                            usage: SymbolUsage::Reference,
                        });
                    }
                }
            }
        }
    }

    for (struct_idx, struct_) in tir.structs.iter().enumerate() {
        index.add(SpanInfo {
            file_id: struct_.file_id,
            span: struct_.name.span,
            kind: SymbolKind::Struct(struct_idx as u32),
            usage: SymbolUsage::Definition,
        });
        for access in &struct_.accesses {
            index.add(SpanInfo {
                file_id: access.file_id,
                span: access.span,
                kind: SymbolKind::Struct(struct_idx as u32),
                usage: SymbolUsage::Reference,
            });
        }
    }

    for (enum_idx, enum_) in tir.enums.iter().enumerate() {
        index.add(SpanInfo {
            file_id: enum_.file_id,
            span: enum_.name.span,
            kind: SymbolKind::Enum(enum_idx as u32),
            usage: SymbolUsage::Definition,
        });
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
            for access in &constant.accesses {
                index.add(SpanInfo {
                    file_id: access.file_id,
                    span: access.span,
                    kind: SymbolKind::Const(constant.id),
                    usage: SymbolUsage::Reference,
                });
            }
        }
    }

    for (decl_idx, decl) in tir.module_decls.iter().enumerate() {
        index.add(SpanInfo {
            file_id: decl.declaring_file_id,
            span: decl.name.span,
            kind: SymbolKind::Module(decl_idx as u32),
            usage: SymbolUsage::Definition,
        });
    }

    for (decl_idx, decl) in tir.import_decls.iter().enumerate() {
        let span = decl
            .internal_name
            .as_ref()
            .map(|n| n.span)
            .unwrap_or(decl.external_name.span);
        index.add(SpanInfo {
            file_id: decl.file_id,
            span,
            kind: SymbolKind::ImportModule(decl_idx as u32),
            usage: SymbolUsage::Definition,
        });
    }

    for (trait_idx, trait_) in tir.traits.iter().enumerate() {
        let kind = SymbolKind::Trait(trait_idx as TraitIndex);
        index.add(SpanInfo {
            file_id: trait_.file_id,
            span: trait_.name.span,
            kind: kind.clone(),
            usage: SymbolUsage::Definition,
        });
        for access in &trait_.accesses {
            index.add(SpanInfo {
                file_id: access.file_id,
                span: access.span,
                kind: kind.clone(),
                usage: SymbolUsage::Reference,
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
