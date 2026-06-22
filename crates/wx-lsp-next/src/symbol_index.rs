use string_interner::symbol::SymbolU32;
use wx_compiler::ast::{DefId, StringInterner, TextSpan};
use wx_compiler::tir::{
    EnumVariantIndex, ExportItem, FieldAccessKind, LocalIndex, ScopeIndex, SourceSpan, TIR,
    TraitIndex, TypeParamOwner,
};
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
    Local {
        func_id: DefId,
        scope_idx: ScopeIndex,
        local_idx: LocalIndex,
    },
    Param {
        func_id: DefId,
        param_idx: u32,
    },
    EnumVariant {
        enum_idx: u32,
        variant_idx: EnumVariantIndex,
    },
    Label {
        func_id: DefId,
        scope_idx: ScopeIndex,
    },
    Trait(TraitIndex),
    TypeParam {
        owner: TypeParamOwner,
        param_index: u32,
    },
    AssocType {
        trait_index: TraitIndex,
        assoc_name: SymbolU32,
    },
    StructField {
        struct_idx: u32,
        field_idx: u32,
    },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone)]
pub struct SpanInfo {
    pub source: SourceSpan,
    pub kind: SymbolKind,
}

pub struct SymbolIndex {
    pub definitions: Vec<SpanInfo>,
    pub references: Vec<SpanInfo>,
    /// Named module-level definitions sorted by string value for prefix search.
    /// Excludes scope-sensitive items (locals, params, type params, labels).
    pub defs_by_name: Vec<(SymbolU32, SpanInfo)>,
}

impl SymbolIndex {
    fn new() -> Self {
        Self {
            definitions: Vec::new(),
            references: Vec::new(),
            defs_by_name: Vec::new(),
        }
    }

    fn build(&mut self, interner: &StringInterner) {
        self.definitions.sort_by_key(|e| e.source.span.start);
        self.references.sort_by_key(|e| e.source.span.start);
        self.defs_by_name.sort_by(|(a, _), (b, _)| {
            interner
                .resolve(*a)
                .unwrap_or("")
                .cmp(interner.resolve(*b).unwrap_or(""))
        });
    }

    pub fn find_at_position(&self, file_id: FileId, pos: u32) -> Option<&SpanInfo> {
        let in_defs = find_narrowest(&self.definitions, file_id, pos);
        let in_refs = find_narrowest(&self.references, file_id, pos);
        match (in_defs, in_refs) {
            (Some(d), Some(r)) => {
                let d_len = d.source.span.end - d.source.span.start;
                let r_len = r.source.span.end - r.source.span.start;
                Some(if d_len <= r_len { d } else { r })
            }
            (Some(d), None) => Some(d),
            (None, Some(r)) => Some(r),
            (None, None) => None,
        }
    }
}

fn find_narrowest(entries: &[SpanInfo], file_id: FileId, pos: u32) -> Option<&SpanInfo> {
    let upper = entries.partition_point(|e| e.source.span.start <= pos);
    entries[..upper]
        .iter()
        .rev()
        .filter(|e| e.source.file_id == file_id && e.source.span.end >= pos)
        .min_by_key(|e| e.source.span.end - e.source.span.start)
}

pub fn build_symbol_index(tir: &TIR, interner: &StringInterner) -> SymbolIndex {
    let mut index = SymbolIndex::new();

    for global in &tir.globals {
        let info = SpanInfo {
            source: SourceSpan::new(global.file_id, global.name.span),
            kind: SymbolKind::Global(global.id),
        };
        index.defs_by_name.push((global.name.inner, info.clone()));
        index.definitions.push(info);
        for access in &global.accesses {
            index.references.push(SpanInfo {
                source: *access,
                kind: SymbolKind::Global(global.id),
            });
        }
    }

    for function in &tir.functions {
        let func_id = function.id;
        let file_id = function.file_id;

        let info = SpanInfo {
            source: SourceSpan::new(file_id, function.name.span),
            kind: SymbolKind::Function(func_id),
        };
        index.defs_by_name.push((function.name.inner, info.clone()));
        index.definitions.push(info);

        for access in &function.accesses {
            index.references.push(SpanInfo {
                source: SourceSpan::new(access.file_id, access.span),
                kind: SymbolKind::Function(func_id),
            });
        }

        for (param_index, tp) in function.type_params.iter().enumerate() {
            let kind = SymbolKind::TypeParam {
                owner: TypeParamOwner::Function(func_id),
                param_index: param_index as u32,
            };
            index.definitions.push(SpanInfo {
                source: SourceSpan::new(file_id, tp.name_span),
                kind: kind.clone(),
            });
            for access in &tp.accesses {
                index.references.push(SpanInfo {
                    source: SourceSpan::new(access.file_id, access.span),
                    kind: kind.clone(),
                });
            }
        }

        let num_params = function.params.len();
        for (param_idx, param) in function.params.iter().enumerate() {
            index.definitions.push(SpanInfo {
                source: SourceSpan::new(file_id, param.name.span),
                kind: SymbolKind::Param {
                    func_id,
                    param_idx: param_idx as u32,
                },
            });
        }

        if let Some(body) = &function.body {
            for (scope_idx, scope) in body.stack.scopes.iter().enumerate() {
                if let Some(label) = &scope.label {
                    let kind = SymbolKind::Label {
                        func_id,
                        scope_idx: scope_idx as ScopeIndex,
                    };
                    index.definitions.push(SpanInfo {
                        source: SourceSpan::new(file_id, label.span),
                        kind: kind.clone(),
                    });
                    for &access_span in &label.accesses {
                        index.references.push(SpanInfo {
                            source: SourceSpan::new(file_id, access_span),
                            kind: kind.clone(),
                        });
                    }
                }
                for (local_idx, local) in scope.locals.iter().enumerate() {
                    let is_param = scope_idx == 0 && local_idx < num_params;
                    let kind = if is_param {
                        SymbolKind::Param {
                            func_id,
                            param_idx: local_idx as u32,
                        }
                    } else {
                        SymbolKind::Local {
                            func_id,
                            scope_idx: scope_idx as ScopeIndex,
                            local_idx: local_idx as LocalIndex,
                        }
                    };
                    if !is_param {
                        index.definitions.push(SpanInfo {
                            source: SourceSpan::new(file_id, local.name.span),
                            kind: kind.clone(),
                        });
                    }
                    for access in &local.accesses {
                        index.references.push(SpanInfo {
                            source: SourceSpan::new(file_id, access.span),
                            kind: kind.clone(),
                        });
                    }
                }
            }
        }
    }

    for (struct_idx, struct_) in tir.structs.iter().enumerate() {
        let info = SpanInfo {
            source: SourceSpan::new(struct_.file_id, struct_.name.span),
            kind: SymbolKind::Struct(struct_idx as u32),
        };
        index.defs_by_name.push((struct_.name.inner, info.clone()));
        index.definitions.push(info);
        for access in &struct_.accesses {
            index.references.push(SpanInfo {
                source: *access,
                kind: SymbolKind::Struct(struct_idx as u32),
            });
        }
        for (param_index, tp) in struct_.type_params.iter().enumerate() {
            let kind = SymbolKind::TypeParam {
                owner: TypeParamOwner::Struct(struct_.id),
                param_index: param_index as u32,
            };
            index.definitions.push(SpanInfo {
                source: SourceSpan::new(struct_.file_id, tp.name_span),
                kind: kind.clone(),
            });
            for access in &tp.accesses {
                index.references.push(SpanInfo {
                    source: SourceSpan::new(access.file_id, access.span),
                    kind: kind.clone(),
                });
            }
        }

        for (field_idx, field) in struct_.fields.iter().enumerate() {
            let kind = SymbolKind::StructField {
                struct_idx: struct_idx as u32,
                field_idx: field_idx as u32,
            };
            index.definitions.push(SpanInfo {
                source: SourceSpan::new(struct_.file_id, field.name.span),
                kind: kind.clone(),
            });
            for access in &field.accesses {
                if matches!(access.kind, FieldAccessKind::Read | FieldAccessKind::Init) {
                    index.references.push(SpanInfo {
                        source: SourceSpan::new(access.file_id, access.span),
                        kind: kind.clone(),
                    });
                }
            }
        }
    }

    for (enum_idx, enum_) in tir.enums.iter().enumerate() {
        let info = SpanInfo {
            source: SourceSpan::new(enum_.file_id, enum_.name.span),
            kind: SymbolKind::Enum(enum_idx as u32),
        };
        index.defs_by_name.push((enum_.name.inner, info.clone()));
        index.definitions.push(info);
        for (variant_idx, variant) in enum_.variants.iter().enumerate() {
            let variant_kind = SymbolKind::EnumVariant {
                enum_idx: enum_idx as u32,
                variant_idx: variant_idx as EnumVariantIndex,
            };
            let variant_info = SpanInfo {
                source: SourceSpan::new(enum_.file_id, variant.name.span),
                kind: variant_kind.clone(),
            };
            index
                .defs_by_name
                .push((variant.name.inner, variant_info.clone()));
            index.definitions.push(variant_info);
            for access in &variant.accesses {
                index.references.push(SpanInfo {
                    source: *access,
                    kind: variant_kind.clone(),
                });
            }
        }
    }

    for constant in &tir.constants {
        if constant.value.is_some() {
            let info = SpanInfo {
                source: SourceSpan::new(constant.file_id, constant.name.span),
                kind: SymbolKind::Const(constant.id),
            };
            index.defs_by_name.push((constant.name.inner, info.clone()));
            index.definitions.push(info);
            for access in &constant.accesses {
                index.references.push(SpanInfo {
                    source: *access,
                    kind: SymbolKind::Const(constant.id),
                });
            }
        }
    }

    for (decl_idx, decl) in tir.module_decls.iter().enumerate() {
        let kind = SymbolKind::Module(decl_idx as u32);
        let def_source = match decl.own_file_id {
            Some(fid) => SourceSpan::new(fid, TextSpan::new(0, 0)),
            None => SourceSpan::new(decl.declaring_file_id, decl.name.span),
        };
        let info = SpanInfo {
            source: def_source,
            kind: kind.clone(),
        };
        index.defs_by_name.push((decl.name.inner, info.clone()));
        index.definitions.push(info);
        if decl.own_file_id.is_some() {
            index.references.push(SpanInfo {
                source: SourceSpan::new(decl.declaring_file_id, decl.name.span),
                kind: kind.clone(),
            });
        }
        for access in &decl.accesses {
            index.references.push(SpanInfo {
                source: *access,
                kind: kind.clone(),
            });
        }
    }

    for (decl_idx, decl) in tir.import_decls.iter().enumerate() {
        let kind = SymbolKind::ImportModule(decl_idx as u32);
        let (name_sym, span) = match &decl.internal_name {
            Some(n) => (n.inner, n.span),
            None => (decl.external_name.inner, decl.external_name.span),
        };
        let info = SpanInfo {
            source: SourceSpan::new(decl.file_id, span),
            kind: kind.clone(),
        };
        index.defs_by_name.push((name_sym, info.clone()));
        index.definitions.push(info);
        for access in &decl.accesses {
            index.references.push(SpanInfo {
                source: *access,
                kind: kind.clone(),
            });
        }
    }

    for (trait_idx, trait_) in tir.traits.iter().enumerate() {
        let trait_index = trait_idx as TraitIndex;
        let kind = SymbolKind::Trait(trait_index);
        let info = SpanInfo {
            source: SourceSpan::new(trait_.file_id, trait_.name.span),
            kind: kind.clone(),
        };
        index.defs_by_name.push((trait_.name.inner, info.clone()));
        index.definitions.push(info);
        for access in &trait_.accesses {
            index.references.push(SpanInfo {
                source: *access,
                kind: kind.clone(),
            });
        }

        for (assoc_name, at) in &trait_.assoc_types {
            let at_kind = SymbolKind::AssocType {
                trait_index,
                assoc_name: *assoc_name,
            };
            let at_info = SpanInfo {
                source: SourceSpan::new(trait_.file_id, at.name_span),
                kind: at_kind.clone(),
            };
            index.defs_by_name.push((*assoc_name, at_info.clone()));
            index.definitions.push(at_info);
            for access in &at.accesses {
                index.references.push(SpanInfo {
                    source: *access,
                    kind: at_kind.clone(),
                });
            }
        }
    }

    for export in tir.exports.values() {
        match export {
            ExportItem::Function {
                internal_name, id, ..
            } => {
                if let Some(&fi) = tir.function_index_lookup.get(id) {
                    index.references.push(SpanInfo {
                        source: SourceSpan::new(
                            tir.functions[fi as usize].file_id,
                            internal_name.span,
                        ),
                        kind: SymbolKind::Function(*id),
                    });
                }
            }
            ExportItem::Global {
                internal_name, id, ..
            } => {
                if let Some(&gi) = tir.global_index_lookup.get(id) {
                    index.references.push(SpanInfo {
                        source: SourceSpan::new(
                            tir.globals[gi as usize].file_id,
                            internal_name.span,
                        ),
                        kind: SymbolKind::Global(*id),
                    });
                }
            }
            ExportItem::Memory { .. } => {}
        }
    }

    index.build(interner);
    index
}
