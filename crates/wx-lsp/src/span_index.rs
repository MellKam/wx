#![allow(dead_code)]

use wx_compiler::ast::TextSpan;
use wx_compiler::tir::{
    EnumVariantIndex, ExprKind, Expression, FunctionIndex, GlobalIndex, LocalIndex, NamespaceIndex,
    ScopeIndex, TIR, Type,
};

/// The kind of symbol being referenced or defined
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, PartialEq)]
pub enum SymbolKind {
    LocalVariable {
        func_idx: FunctionIndex,
        scope_idx: ScopeIndex,
        local_idx: LocalIndex,
    },
    GlobalVariable {
        global_idx: GlobalIndex,
    },
    Function {
        func_idx: FunctionIndex,
    },
    EnumVariant {
        namespace_index: NamespaceIndex,
        variant_idx: EnumVariantIndex,
    },
    FunctionParam {
        func_idx: FunctionIndex,
        param_idx: u32,
    },
    Type {
        ty: Type,
    },
}

/// How the symbol is being used at this location
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SymbolUsage {
    /// Where the symbol is defined
    Definition,
    /// Where the symbol is used/referenced
    Reference,
    /// Type annotation usage
    TypeUsage,
}

/// Information about a symbol at a specific text span
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone)]
pub struct SpanInfo {
    pub span: TextSpan,
    pub kind: SymbolKind,
    pub usage: SymbolUsage,
}

/// Index for efficiently looking up symbols by text position
pub struct SpanIndex {
    /// Sorted by span.start for binary search
    entries: Vec<SpanInfo>,
}

impl SpanIndex {
    pub fn new() -> Self {
        SpanIndex {
            entries: Vec::new(),
        }
    }

    /// Add a new span entry to the index
    pub fn add(&mut self, info: SpanInfo) {
        self.entries.push(info);
    }

    /// Build/finalize the index by sorting entries
    /// Call this after all entries have been added
    pub fn build(&mut self) {
        self.entries.sort_by_key(|e| e.span.start);
    }

    /// Find the symbol at the given text position
    /// Returns the innermost/smallest span containing the position
    pub fn find_at_position(&self, pos: u32) -> Option<&SpanInfo> {
        // Find the smallest (most specific) span containing the position
        // Use <= for the end check to handle cursor positions at symbol boundaries
        self.entries
            .iter()
            .filter(|entry| entry.span.start <= pos && pos <= entry.span.end)
            .min_by_key(|entry| entry.span.end - entry.span.start)
    }

    /// Find all references to the same symbol
    pub fn find_all_references(&self, kind: &SymbolKind) -> Vec<TextSpan> {
        self.entries
            .iter()
            .filter(|entry| Self::same_symbol(&entry.kind, kind))
            .map(|entry| entry.span)
            .collect()
    }

    /// Find the definition of a symbol
    pub fn find_definition(&self, kind: &SymbolKind) -> Option<TextSpan> {
        self.entries
            .iter()
            .find(|entry| {
                Self::same_symbol(&entry.kind, kind) && entry.usage == SymbolUsage::Definition
            })
            .map(|entry| entry.span)
    }

    /// Check if two symbol kinds refer to the same symbol
    fn same_symbol(a: &SymbolKind, b: &SymbolKind) -> bool {
        match (a, b) {
            (
                SymbolKind::LocalVariable {
                    func_idx: f1,
                    scope_idx: s1,
                    local_idx: l1,
                },
                SymbolKind::LocalVariable {
                    func_idx: f2,
                    scope_idx: s2,
                    local_idx: l2,
                },
            ) => f1 == f2 && s1 == s2 && l1 == l2,
            (
                SymbolKind::GlobalVariable { global_idx: g1 },
                SymbolKind::GlobalVariable { global_idx: g2 },
            ) => g1 == g2,
            (SymbolKind::Function { func_idx: f1 }, SymbolKind::Function { func_idx: f2 }) => {
                f1 == f2
            }
            // (SymbolKind::EnumType { enum_idx: e1 }, SymbolKind::EnumType { enum_idx: e2 }) => {
            //     e1 == e2
            // }
            // (
            //     SymbolKind::EnumVariant {
            //         enum_idx: e1,
            //         variant_idx: v1,
            //     },
            //     SymbolKind::EnumVariant {
            //         enum_idx: e2,
            //         variant_idx: v2,
            //     },
            // ) => e1 == e2 && v1 == v2,
            (
                SymbolKind::FunctionParam {
                    func_idx: f1,
                    param_idx: p1,
                },
                SymbolKind::FunctionParam {
                    func_idx: f2,
                    param_idx: p2,
                },
            ) => f1 == f2 && p1 == p2,
            // Function parameters are also local variables in scope 0
            // FunctionParam at index N corresponds to LocalVariable at scope 0, index N
            (
                SymbolKind::FunctionParam {
                    func_idx: f1,
                    param_idx: p1,
                },
                SymbolKind::LocalVariable {
                    func_idx: f2,
                    scope_idx: 0,
                    local_idx: l2,
                },
            ) => f1 == f2 && *p1 == *l2,
            (
                SymbolKind::LocalVariable {
                    func_idx: f1,
                    scope_idx: 0,
                    local_idx: l1,
                },
                SymbolKind::FunctionParam {
                    func_idx: f2,
                    param_idx: p2,
                },
            ) => f1 == f2 && *l1 == *p2,
            (
                SymbolKind::Type {
                    ty:
                        Type::Namespace {
                            namespace_index: n1,
                        },
                },
                SymbolKind::Type {
                    ty:
                        Type::Namespace {
                            namespace_index: n2,
                        },
                },
            ) => n1 == n2,
            _ => false,
        }
    }

    /// Get all entries (for debugging)
    pub fn entries(&self) -> &[SpanInfo] {
        &self.entries
    }
}

/// Build a SpanIndex from a TIR
pub fn build_span_index(tir: &TIR) -> SpanIndex {
    let mut index = SpanIndex::new();

    // Index global variables
    for (global_idx, global) in tir.defined_globals.iter() {
        index.add(SpanInfo {
            span: global.name.span,
            kind: SymbolKind::GlobalVariable {
                global_idx: *global_idx,
            },
            usage: SymbolUsage::Definition,
        });

        // Index expressions in global initializer
        index_expression(&mut index, None, &global.value.inner);
    }

    // Index functions
    for (func_idx, func) in tir.defined_functions.iter() {
        // Index function name
        index.add(SpanInfo {
            span: func.name.span,
            kind: SymbolKind::Function {
                func_idx: *func_idx,
            },
            usage: SymbolUsage::Definition,
        });

        // Index function parameters
        for (param_idx, param) in func.params.iter().enumerate() {
            index.add(SpanInfo {
                span: param.name.span,
                kind: SymbolKind::FunctionParam {
                    func_idx: *func_idx,
                    param_idx: param_idx as u32,
                },
                usage: SymbolUsage::Definition,
            });
        }

        // Index local variables in all scopes
        for (scope_idx, scope) in func.stack.scopes.iter().enumerate() {
            for (local_idx, local) in scope.locals.iter().enumerate() {
                // Skip the first N locals in scope 0, where N = number of parameters
                // These are the parameters, already indexed above
                if scope_idx == 0 && local_idx < func.params.len() {
                    continue;
                }

                index.add(SpanInfo {
                    span: local.name.span,
                    kind: SymbolKind::LocalVariable {
                        func_idx: *func_idx,
                        scope_idx: scope_idx as u32,
                        local_idx: local_idx as u32,
                    },
                    usage: SymbolUsage::Definition,
                });
            }
        }

        // Index expressions in function body
        index_expression(&mut index, Some(*func_idx), &func.block);
    }

    // Index exports
    for export in tir.exports.iter() {
        index.add(match export {
            wx_compiler::tir::ExportItem::Function {
                internal_name,
                func_index,
                ..
            } => SpanInfo {
                span: internal_name.span,
                kind: SymbolKind::Function {
                    func_idx: *func_index,
                },
                usage: SymbolUsage::Reference,
            },
            wx_compiler::tir::ExportItem::Global {
                internal_name,
                global_index,
                ..
            } => SpanInfo {
                span: internal_name.span,
                kind: SymbolKind::GlobalVariable {
                    global_idx: *global_index,
                },
                usage: SymbolUsage::Reference,
            },
        });
    }

    for (namespace_index, namespace) in tir.namespaces.iter().enumerate() {
        match namespace {
            wx_compiler::tir::Namespace::ImportModule(module) => {
                for import_value in module.lookup.values() {
                    match import_value {
                        wx_compiler::tir::ImportValue::Function { func_index } => {
                            index.add(SpanInfo {
                                span: tir.declared_functions[*func_index as usize].name.span,
                                kind: SymbolKind::Function {
                                    func_idx: *func_index,
                                },
                                usage: SymbolUsage::Definition,
                            });
                        }
                        wx_compiler::tir::ImportValue::Global { global_index } => {
                            index.add(SpanInfo {
                                span: tir.declared_globals[*global_index as usize].name.span,
                                kind: SymbolKind::GlobalVariable {
                                    global_idx: *global_index,
                                },
                                usage: SymbolUsage::Definition,
                            });
                        }
                    }
                }
                index.add(SpanInfo {
                    span: module
                        .internal_name
                        .clone()
                        .map(|n| n.span)
                        .unwrap_or(module.external_name.span),
                    kind: SymbolKind::Type {
                        ty: Type::Namespace {
                            namespace_index: namespace_index as u32,
                        },
                    },
                    usage: SymbolUsage::Definition,
                });
            }
            wx_compiler::tir::Namespace::Enum(enum_) => {
                for (variant_index, variant) in enum_.variants.iter().enumerate() {
                    index.add(SpanInfo {
                        span: variant.name.span,
                        kind: SymbolKind::EnumVariant {
                            namespace_index: namespace_index as u32,
                            variant_idx: variant_index as u32,
                        },
                        usage: SymbolUsage::Definition,
                    });
                }

                index.add(SpanInfo {
                    span: enum_.name.span,
                    kind: SymbolKind::Type {
                        ty: Type::Namespace {
                            namespace_index: namespace_index as u32,
                        },
                    },
                    usage: SymbolUsage::Definition,
                });
            }
        }
    }

    // Sort entries for efficient lookup
    index.build();

    index
}

/// Recursively index all expressions
fn index_expression(index: &mut SpanIndex, func_idx: Option<FunctionIndex>, expr: &Expression) {
    match &expr.kind {
        ExprKind::Local {
            scope_index,
            local_index,
        } => {
            if let Some(func_idx) = func_idx {
                index.add(SpanInfo {
                    span: expr.span,
                    kind: SymbolKind::LocalVariable {
                        func_idx,
                        scope_idx: *scope_index,
                        local_idx: *local_index,
                    },
                    usage: SymbolUsage::Reference,
                });
            }
        }
        ExprKind::Global { global_index } => {
            index.add(SpanInfo {
                span: expr.span,
                kind: SymbolKind::GlobalVariable {
                    global_idx: *global_index,
                },
                usage: SymbolUsage::Reference,
            });
        }
        ExprKind::Function { func_index } => {
            index.add(SpanInfo {
                span: expr.span,
                kind: SymbolKind::Function {
                    func_idx: *func_index,
                },
                usage: SymbolUsage::Reference,
            });
        }
        ExprKind::EnumVariant {
            namespace_index,
            variant_index,
        } => {
            index.add(SpanInfo {
                span: expr.span,
                kind: SymbolKind::EnumVariant {
                    namespace_index: *namespace_index,
                    variant_idx: *variant_index,
                },
                usage: SymbolUsage::Reference,
            });
        }
        ExprKind::LocalDeclaration { value, .. } => {
            // The definition is already indexed when we process scopes
            // Just index the initializer expression
            index_expression(index, func_idx, value);
        }
        ExprKind::Unary { operand, .. } => {
            index_expression(index, func_idx, operand);
        }
        ExprKind::Binary { left, right, .. } => {
            index_expression(index, func_idx, left);
            index_expression(index, func_idx, right);
        }
        ExprKind::Call { callee, arguments } => {
            index_expression(index, func_idx, callee);
            for arg in arguments.iter() {
                index_expression(index, func_idx, arg);
            }
        }
        ExprKind::Block {
            expressions,
            result,
            ..
        } => {
            for expr in expressions.iter() {
                index_expression(index, func_idx, expr);
            }
            if let Some(result) = result {
                index_expression(index, func_idx, result);
            }
        }
        ExprKind::IfElse {
            condition,
            then_block,
            else_block,
        } => {
            index_expression(index, func_idx, condition);
            index_expression(index, func_idx, then_block);
            if let Some(else_block) = else_block {
                index_expression(index, func_idx, else_block);
            }
        }
        ExprKind::Loop { block, .. } => {
            index_expression(index, func_idx, block);
        }
        ExprKind::Break { value, .. } => {
            if let Some(value) = value {
                index_expression(index, func_idx, value);
            }
        }
        ExprKind::Return { value } => {
            if let Some(value) = value {
                index_expression(index, func_idx, value);
            }
        }
        ExprKind::NamespaceAccess {
            namespace_index,
            namespace_span,
            member,
        } => {
            index.add(SpanInfo {
                span: namespace_span.clone(),
                kind: SymbolKind::Type {
                    ty: Type::Namespace {
                        namespace_index: *namespace_index,
                    },
                },
                usage: SymbolUsage::Reference,
            });

            index_expression(index, func_idx, member);
        }
        ExprKind::Error
        | ExprKind::Placeholder
        | ExprKind::Unreachable
        | ExprKind::Int { .. }
        | ExprKind::Float { .. }
        | ExprKind::Bool { .. }
        | ExprKind::Continue { .. } => {
            // No symbols to index
        }
    }
}

#[cfg(test)]
mod tests {
    use string_interner::StringInterner;
    use wx_compiler::ast;

    use super::*;

    #[test]
    fn test_span_index_basic_lookup() {
        let source = "export fn add(a: i32, b: i32) -> i32 { a + b }";

        let mut interner = StringInterner::new();
        let mut files = ast::Files::new();
        let file_id = files
            .add("test.wx".to_string(), source.to_string())
            .unwrap();

        // Parse AST
        let ast = ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        // Build TIR
        let tir = TIR::build(&ast, &mut interner);

        // Build span index
        let span_index = build_span_index(&tir);

        // Should have indexed several items: function, parameters, references
        assert!(
            span_index.entries().len() > 0,
            "Should have indexed some symbols"
        );

        // Find the function definition (should be early in the source)
        // "export fn add(a: i32, b: i32) -> i32 { a + b }"
        //           ^10
        let position_in_add = 11; // should be within "add" function name (10..13)
        let result = span_index.find_at_position(position_in_add);

        assert!(
            result.is_some(),
            "Should find a symbol at position {}",
            position_in_add
        );

        if let Some(info) = result {
            // Should be the function definition
            assert!(
                matches!(info.kind, SymbolKind::Function { .. }),
                "Should find the function 'add' at this position"
            );
            assert_eq!(
                info.usage,
                SymbolUsage::Definition,
                "Should be a definition"
            );

            // Verify the span text matches
            let span_text = info.span.extract_str(source);
            assert_eq!(span_text, "add", "Should extract 'add' from the span");
        }

        // Test finding all references to the function
        if let Some(func_info) = span_index.entries().iter().find(|e| {
            matches!(e.kind, SymbolKind::Function { .. }) && e.usage == SymbolUsage::Definition
        }) {
            let references = span_index.find_all_references(&func_info.kind);
            // Should have the definition
            assert!(references.len() >= 1, "Should find at least the definition");
        }
    }
}
