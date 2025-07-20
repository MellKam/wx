use core::panic;

use codespan_reporting::diagnostic::Diagnostic;
use serde::Serialize;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use crate::ast::BinaryOp;
use crate::hir::diagnostics::*;
use crate::hir::global::{GlobalContext, GlobalValue};
use crate::hir::*;
use crate::span::TextSpan;
use crate::{ast, hir};

pub struct Builder<'ast, 'interner> {
    ast: &'ast ast::Ast,
    global: GlobalContext<'interner>,
    diagnostics: Vec<Diagnostic<FileId>>,
    hir: HIR,
}

#[derive(Debug, Serialize)]
pub struct BuilderResult {
    pub hir: HIR,
    pub diagnostics: Vec<Diagnostic<FileId>>,
}

enum BlockState<T> {
    Exhaustive(T),
    Incomplete(T),
}

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub lookup: HashMap<(ScopeIndex, SymbolU32), LocalIndex>,
    pub func_index: FuncIndex,
    pub scope_index: ScopeIndex,
    pub frame: StackFrame,
}

impl FunctionContext {
    pub fn push_local(&mut self, local: Local) -> LocalIndex {
        let name_symbol = local.name.symbol;
        let index = self.frame.push_local(self.scope_index, local);
        self.lookup.insert((self.scope_index, name_symbol), index);
        index
    }

    pub fn resolve_local(&self, symbol: SymbolU32) -> Option<(ScopeIndex, LocalIndex)> {
        let mut scope_index = self.scope_index;

        loop {
            if let Some(&value) = self.lookup.get(&(scope_index, symbol)) {
                return Some((scope_index, value));
            }

            scope_index = self.frame.scopes[scope_index.0 as usize].parent?;
        }
    }

    pub fn enter_block<T>(&mut self, block: BlockScope, handler: impl FnOnce(&mut Self) -> T) -> T {
        let parent_scope_index = self.scope_index;
        self.scope_index = ScopeIndex(self.frame.scopes.len() as u32);
        self.frame.scopes.push(block);

        let result = handler(self);

        self.scope_index = parent_scope_index;
        result
    }

    pub fn resolve_label(&self, symbol: SymbolU32) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.frame.scopes[scope_index.0 as usize];
            if scope.label == Some(symbol) {
                return Some(scope_index);
            }

            scope_index = match scope.parent {
                Some(parent) => parent,
                None => return None,
            };
        }
    }

    pub fn get_closest_loop_block(&self) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.frame.scopes[scope_index.0 as usize];
            match scope.kind {
                BlockKind::Loop => return Some(scope_index),
                _ => {}
            }

            scope_index = match scope.parent {
                Some(parent) => parent,
                None => return None,
            }
        }
    }
}

impl<'ast, 'interner> Builder<'ast, 'interner> {
    pub fn build(
        ast: &'ast ast::Ast,
        interner: &'interner mut StringInterner<StringBackend<SymbolU32>>,
    ) -> BuilderResult {
        let mut builder = Builder {
            ast,
            global: GlobalContext::new(interner),
            diagnostics: Vec::new(),
            hir: HIR::new(ast.file_id),
        };

        for item in builder.ast.items.iter() {
            _ = builder.define_item(item);
        }

        for item in builder.ast.items.iter() {
            _ = builder.build_item(item);
        }

        let Builder {
            mut hir,
            global,
            diagnostics,
            ..
        } = builder;
        hir.enums = global.enums;
        hir.globals = global.globals;
        hir.exports = global.exports;

        BuilderResult { hir, diagnostics }
    }

    fn build_item(&mut self, item: &ast::Item) -> Result<(), ()> {
        match &item.kind {
            ast::ItemKind::ExportModifier { item, .. } => self.build_item(item),
            ast::ItemKind::FunctionDefinition { .. } => {
                let func = self.build_function_definition(item)?;
                self.hir.functions.push(func);

                Ok(())
            }
            ast::ItemKind::EnumDefinition { .. } => Ok(()),
            ast::ItemKind::GlobalDefinition { .. } => Ok(()),
        }
    }

    fn define_item(&mut self, item: &ast::Item) -> Result<GlobalValue, ()> {
        use ast::ItemKind::*;
        match &item.kind {
            ExportModifier { item, .. } => {
                let global_value = self.define_item(item)?;
                let export_item = match global_value {
                    GlobalValue::Function { func_index } => ExportItem::Function { func_index },
                    GlobalValue::Global { global_index } => ExportItem::Global { global_index },
                    _ => unreachable!(),
                };
                self.global.exports.push(export_item);
                Ok(global_value)
            }
            FunctionDefinition { signature, .. } => {
                match self
                    .global
                    .symbol_lookup
                    .get(&(global::LookupCategory::Value, signature.name.symbol))
                    .cloned()
                {
                    Some(_) => todo!("value with this name already declared"),
                    None => {}
                };

                let func_type = self.build_function_type(signature)?;
                let type_index = self.global.get_or_insert_func_type(&func_type);

                let func_index = FuncIndex(self.global.functions.len() as u32);
                self.global.symbol_lookup.insert(
                    (global::LookupCategory::Value, signature.name.symbol),
                    GlobalValue::Function { func_index },
                );
                self.global.functions.push(type_index);
                Ok(GlobalValue::Function { func_index })
            }
            GlobalDefinition { name, .. } => {
                match self
                    .global
                    .symbol_lookup
                    .get(&(global::LookupCategory::Value, name.symbol))
                    .cloned()
                {
                    Some(_) => todo!("value with this name already declared"),
                    None => {}
                };

                let global_item = self.build_global_item(item)?;

                let global_index = GlobalIndex(self.global.globals.len() as u32);
                self.global.symbol_lookup.insert(
                    (global::LookupCategory::Value, global_item.name.symbol),
                    GlobalValue::Global { global_index },
                );
                self.global.globals.push(global_item);
                Ok(GlobalValue::Global { global_index })
            }
            EnumDefinition { name, .. } => {
                match self
                    .global
                    .symbol_lookup
                    .get(&(global::LookupCategory::Type, name.symbol))
                    .cloned()
                {
                    Some(_) => todo!("type with this name already declared"),
                    None => {}
                };
                let enum_item = self.build_enum_item(item)?;

                let enum_index = EnumIndex(self.global.enums.len() as u32);
                self.global.symbol_lookup.insert(
                    (global::LookupCategory::Type, enum_item.name.symbol),
                    GlobalValue::Enum { enum_index },
                );
                self.global.enums.push(enum_item);
                Ok(GlobalValue::Enum { enum_index })
            }
        }
    }

    fn build_function_type(
        &mut self,
        signature: &ast::FunctionSignature,
    ) -> Result<FunctionType, ()> {
        Ok(FunctionType {
            params: signature
                .params
                .inner
                .iter()
                .map(|param| self.resolve_type(&param.inner.annotation.ty))
                .collect::<Result<Box<_>, ()>>()?,
            result: self.resolve_type(&signature.result.ty)?,
        })
    }

    fn build_global_item(&mut self, item: &ast::Item) -> Result<Global, ()> {
        let (name, ty, value_expr, mutability) = match &item.kind {
            ast::ItemKind::GlobalDefinition {
                name,
                ty,
                value,
                mutable,
            } => (name.clone(), ty, value, mutable.clone()),
            _ => unreachable!(),
        };

        let ty = self.resolve_type(ty).unwrap();

        let mut expr = match value_expr.kind {
            ast::ExprKind::Int { value } => hir::Expression {
                kind: hir::ExprKind::Int(value),
                ty: None,
                span: value_expr.span,
            },
            ast::ExprKind::Float { value } => hir::Expression {
                kind: hir::ExprKind::Float(value),
                ty: None,
                span: value_expr.span,
            },
            _ => panic!("invalid global value type"),
        };

        self.coerce_untyped_expr(&mut expr, ty)?;

        Ok(Global {
            name,
            mutability,
            ty,
            value: expr,
            accesses: Vec::new(),
        })
    }

    fn build_function_definition(&mut self, item: &ast::Item) -> Result<hir::Function, ()> {
        let (signature, block) = match &item.kind {
            ast::ItemKind::FunctionDefinition { signature, block } => (signature, block),
            _ => unreachable!(),
        };

        let locals = signature
            .params
            .inner
            .iter()
            .map(|param| hir::Local {
                name: param.inner.name.clone(),
                ty: self.resolve_type(&param.inner.annotation.ty).unwrap(),
                mutability: param.inner.mutable,
                accesses: Vec::new(),
            })
            .collect::<Vec<_>>();

        let lookup = locals
            .iter()
            .enumerate()
            .map(|(index, param)| ((ScopeIndex(0), param.name.symbol), LocalIndex(index as u32)))
            .collect();

        let func_index = self.global.resolve_func(signature.name.symbol).unwrap();
        let type_index = self
            .global
            .functions
            .get(func_index.0 as usize)
            .copied()
            .unwrap();
        let func_type = self
            .global
            .function_types
            .get(type_index.0 as usize)
            .unwrap()
            .clone();

        let root_scope = BlockScope {
            parent: None,
            label: None,
            kind: BlockKind::Block,
            locals,
            inferred_type: None,
            expected_type: Some(func_type.result),
        };

        let mut ctx = FunctionContext {
            func_index,
            frame: StackFrame {
                scopes: vec![root_scope],
            },
            scope_index: ScopeIndex(0),
            lookup,
        };
        let block = self.build_block_expression(&mut ctx, &block)?;

        Ok(hir::Function {
            ty: func_type,
            name: signature.name.clone(),
            stack: ctx.frame,
            block: Box::new(block),
        })
    }

    fn build_enum_item(&mut self, item: &ast::Item) -> Result<Enum, ()> {
        let (name, annotation, variants) = match &item.kind {
            ast::ItemKind::EnumDefinition {
                name,
                annotation,
                variants,
            } => (name.clone(), annotation, variants),
            _ => unreachable!(),
        };

        let ty = match self.resolve_type(&annotation.ty)? {
            Type::Primitive(ty) => ty,
            ty => {
                self.diagnostics.push(
                    InvalidEnumTypeDiagnostic {
                        file_id: self.ast.file_id,
                        ty,
                        span: name.span,
                    }
                    .report(&self.global),
                );

                return Err(());
            }
        };

        let variants = variants
            .inner
            .iter()
            .map(|variant| {
                let expr = &variant.inner.value;
                let mut value = match expr.kind {
                    ast::ExprKind::Int { value } => hir::Expression {
                        kind: hir::ExprKind::Int(value),
                        ty: None,
                        span: expr.span,
                    },
                    ast::ExprKind::Float { value } => hir::Expression {
                        kind: hir::ExprKind::Float(value),
                        ty: None,
                        span: expr.span,
                    },
                    _ => panic!("invalid enum value"),
                };

                self.coerce_untyped_expr(&mut value, Type::Primitive(ty))?;

                Ok(EnumVariant {
                    name: variant.inner.name.clone(),
                    value,
                })
            })
            .collect::<Result<Box<_>, _>>()?;

        // TODO: ensure that the enum variants are unique

        let lookup = variants
            .iter()
            .enumerate()
            .map(|(index, variant)| (variant.name.symbol, EnumVariantIndex(index as u32)))
            .collect();

        Ok(Enum {
            name,
            ty,
            variants,
            lookup,
        })
    }

    fn build_statement(
        &mut self,
        ctx: &mut FunctionContext,
        statement: &ast::Separated<ast::StmtKind>,
    ) -> Result<hir::Expression, ()> {
        match &statement.inner {
            ast::StmtKind::Expression { .. } => {
                self.build_expression_statement(ctx, &statement.inner)
            }
            ast::StmtKind::LocalDefinition { .. } => {
                self.build_local_definition_statement(ctx, statement)
            }
        }
    }

    fn build_expression_statement(
        &mut self,
        ctx: &mut FunctionContext,
        stmt: &ast::StmtKind,
    ) -> Result<Expression, ()> {
        let value = match &stmt {
            ast::StmtKind::Expression { expr: value } => value,
            _ => unreachable!(),
        };

        let value = self.build_expression(
            ctx,
            value,
            AccessContext {
                access_kind: AccessKind::Read,
                expected_type: None,
            },
        )?;
        match value.ty {
            Some(hir::Type::Unit) => Ok(value),
            Some(hir::Type::Never) => {
                let scope = ctx
                    .frame
                    .scopes
                    .get_mut(ctx.scope_index.0 as usize)
                    .unwrap();
                scope.inferred_type = scope.inferred_type.or(Some(Type::Never));

                Ok(value)
            }
            None => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: value.span,
                    }
                    .report(),
                );

                Err(())
            }
            _ => {
                self.diagnostics.push(
                    UnusedValueDiagnostic {
                        file_id: self.ast.file_id,
                        span: value.span,
                    }
                    .report(),
                );

                let start_span = TextSpan::new(value.span.start().0, value.span.start().0);
                let span = value.span;
                Ok(Expression {
                    kind: ExprKind::Binary {
                        left: Box::new(Expression {
                            kind: ExprKind::Placeholder,
                            ty: value.ty,
                            span: start_span,
                        }),
                        operator: BinaryOp {
                            kind: ast::BinOpKind::Assign,
                            span: start_span,
                        },
                        right: Box::new(value),
                    },
                    span,
                    ty: Some(Type::Unit),
                })
            }
        }
    }

    fn resolve_type(&mut self, type_expr: &ast::TypeExpression) -> Result<Type, ()> {
        match self.global.resolve_type(type_expr) {
            Ok(ty) => Ok(ty),
            Err(_) => {
                self.diagnostics.push(
                    UndeclaredIdentifierDiagnostic {
                        file_id: self.ast.file_id,
                        span: type_expr.span,
                    }
                    .report(),
                );
                Err(())
            }
        }
    }

    fn build_local_definition_statement(
        &mut self,
        ctx: &mut FunctionContext,
        stmt: &ast::Separated<ast::StmtKind>,
    ) -> Result<Expression, ()> {
        let (mutability, name, annotation, value) = match &stmt.inner {
            ast::StmtKind::LocalDefinition {
                mutable,
                name,
                annotation,
                value,
                ..
            } => (mutable.clone(), name.clone(), annotation, value),
            _ => unreachable!(),
        };

        let expected_type = match annotation {
            Some(annotation) => Some(self.resolve_type(&annotation.ty).unwrap()),
            None => None,
        };
        let mut value = self.build_expression(
            ctx,
            value,
            AccessContext {
                expected_type,
                access_kind: AccessKind::Read,
            },
        )?;

        let ty = match (value.ty, expected_type) {
            (Some(ty), None) => ty,
            (Some(actual_type), Some(expected_type)) => {
                if actual_type.coercible_to(expected_type) {
                    expected_type
                } else {
                    self.diagnostics.push(
                        TypeMistmatchDiagnostic {
                            file_id: self.ast.file_id,
                            expected: expected_type,
                            actual: actual_type,
                            span: value.span,
                        }
                        .report(&self.global),
                    );
                    expected_type // Recover by using the expected type
                }
            }
            (None, Some(expected_type)) => {
                self.coerce_untyped_expr(&mut value, expected_type)?;
                expected_type
            }
            (None, None) => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: name.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        let local_index = ctx.push_local(hir::Local {
            name: name.clone(),
            ty,
            mutability,
            accesses: Vec::new(),
        });

        let span = TextSpan::new(stmt.span.start().0, value.span.end().0);
        Ok(hir::Expression {
            kind: hir::ExprKind::LocalDeclaration {
                name,
                scope_index: ctx.scope_index,
                local_index,
                expr: Box::new(value),
            },
            ty: match ty {
                Type::Never => Some(Type::Never),
                _ => Some(Type::Unit),
            },
            span,
        })
    }

    fn build_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &ast::Expression,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        use ast::ExprKind;
        match &expr.kind {
            ExprKind::Int { value } => Ok(hir::Expression {
                kind: hir::ExprKind::Int(*value),
                ty: None,
                span: expr.span,
            }),
            ExprKind::Float { value } => Ok(hir::Expression {
                kind: hir::ExprKind::Float(*value),
                ty: None,
                span: expr.span,
            }),
            ExprKind::Identifier { .. } => {
                self.build_identifier_expression(func_ctx, expr, access_ctx)
            }
            ExprKind::Binary { .. } => self.build_binary_expression(func_ctx, expr, access_ctx),
            ExprKind::Grouping { value } => {
                self.build_expression(func_ctx, &value.inner, access_ctx)
            }
            ExprKind::Unary { .. } => self.build_unary_expression(func_ctx, expr, access_ctx),
            ExprKind::Call { .. } => self.build_call_expression(func_ctx, expr),
            ExprKind::Namespace { .. } => self.build_namespace_expression(expr),
            ExprKind::Return { .. } => self.build_return_expression(func_ctx, expr),
            ExprKind::Block { .. } => func_ctx.enter_block(
                BlockScope {
                    label: None,
                    kind: BlockKind::Block,
                    parent: Some(func_ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
                    expected_type: access_ctx.expected_type,
                },
                |ctx| self.build_block_expression(ctx, expr),
            ),
            ExprKind::IfElse { .. } => {
                self.build_if_else_expression(func_ctx, expr, None, access_ctx)
            }
            ExprKind::Loop { .. } => self.build_loop_expression(func_ctx, expr, None, access_ctx),
            ExprKind::Cast { .. } => self.build_cast_expression(func_ctx, expr, access_ctx),
            ExprKind::Break { .. } => self.build_break_expression(func_ctx, expr),
            ExprKind::Continue { .. } => self.build_continue_expression(func_ctx, expr),
            ExprKind::Unreachable => Ok(hir::Expression {
                kind: hir::ExprKind::Unreachable,
                ty: Some(Type::Never),
                span: expr.span,
            }),
            ExprKind::Label { .. } => self.build_label_expression(func_ctx, expr, access_ctx),
        }
    }

    fn build_label_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
        access_ctx: AccessContext,
    ) -> Result<hir::Expression, ()> {
        let (label, block) = match &expr.kind {
            ast::ExprKind::Label { label, block } => (label.clone(), block),
            _ => unreachable!(),
        };

        match block.kind {
            ast::ExprKind::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: Some(label.symbol),
                    kind: BlockKind::Block,
                    parent: Some(ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
                    expected_type: access_ctx.expected_type,
                },
                |ctx| self.build_block_expression(ctx, block),
            ),
            ast::ExprKind::IfElse { .. } => self.build_if_else_expression(
                ctx,
                block,
                Some(label),
                AccessContext {
                    expected_type: access_ctx.expected_type,
                    access_kind: AccessKind::Read,
                },
            ),
            ast::ExprKind::Loop { .. } => self.build_loop_expression(
                ctx,
                block,
                Some(label),
                AccessContext {
                    expected_type: access_ctx.expected_type,
                    access_kind: AccessKind::Read,
                },
            ),
            _ => unreachable!(),
        }
    }

    fn build_identifier_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &ast::Expression,
        access_ctx: AccessContext,
    ) -> Result<hir::Expression, ()> {
        let symbol = match expr.kind {
            ast::ExprKind::Identifier { symbol } => symbol,
            _ => unreachable!(),
        };
        match func_ctx.resolve_local(symbol) {
            Some((scope_index, local_index)) => {
                let local = func_ctx
                    .frame
                    .get_mut_local(scope_index, local_index)
                    .unwrap();

                local.accesses.push(hir::VariableAccess {
                    kind: access_ctx.access_kind,
                    span: expr.span,
                });

                return Ok(hir::Expression {
                    kind: hir::ExprKind::Local {
                        local_index,
                        scope_index,
                    },
                    ty: Some(local.ty),
                    span: expr.span,
                });
            }
            None => {}
        }

        match self.global.resolve_value(symbol) {
            Some(global) => match global {
                GlobalValue::Bool { value } => Ok(Expression {
                    kind: ExprKind::Bool(value),
                    ty: Some(Type::Bool),
                    span: expr.span,
                }),
                GlobalValue::Placeholder => Ok(Expression {
                    kind: ExprKind::Placeholder,
                    ty: access_ctx.expected_type,
                    span: expr.span,
                }),
                GlobalValue::Function { func_index } => Ok(Expression {
                    kind: ExprKind::Function(func_index),
                    ty: Some(Type::Function(
                        self.global
                            .functions
                            .get(func_index.0 as usize)
                            .copied()
                            .unwrap(),
                    )),
                    span: expr.span,
                }),
                GlobalValue::EnumVariant {
                    enum_index,
                    variant_index,
                } => Ok(Expression {
                    kind: ExprKind::EnumVariant {
                        enum_index,
                        variant_index,
                    },
                    ty: Some(Type::Enum(enum_index)),
                    span: expr.span,
                }),
                GlobalValue::Global { global_index } => {
                    let global = self
                        .global
                        .globals
                        .get_mut(global_index.0 as usize)
                        .unwrap();

                    global.accesses.push(hir::VariableAccess {
                        kind: access_ctx.access_kind,
                        span: expr.span,
                    });

                    Ok(Expression {
                        kind: ExprKind::Global { global_index },
                        ty: Some(global.ty),
                        span: expr.span,
                    })
                }
                _ => unreachable!(),
            },
            None => {
                self.diagnostics.push(
                    UndeclaredIdentifierDiagnostic {
                        file_id: self.ast.file_id,
                        span: expr.span,
                    }
                    .report(),
                );

                Ok(hir::Expression {
                    kind: hir::ExprKind::Error,
                    ty: access_ctx.expected_type.or(Some(hir::Type::Unknown)),
                    span: expr.span,
                })
            }
        }
    }

    fn build_namespace_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (namespace, member) = match &expr.kind {
            ast::ExprKind::Namespace { namespace, member } => (namespace, member),
            _ => unreachable!("expected namespace member expression"),
        };

        let namespace_type = self.global.resolve_type(namespace).unwrap();
        match namespace_type {
            hir::Type::Enum(enum_index) => {
                let enum_ = self.hir.get_enum(enum_index).unwrap();
                let variant_index = match enum_.lookup.get(&member.symbol).copied() {
                    Some(index) => index,
                    None => {
                        self.diagnostics.push(
                            UnknownEnumVariantDiagnostic {
                                file_id: self.ast.file_id,
                                span: member.span,
                            }
                            .report(),
                        );

                        return Ok(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: Some(hir::Type::Enum(enum_index)),
                            span: expr.span,
                        });
                    }
                };

                Ok(hir::Expression {
                    kind: hir::ExprKind::EnumVariant {
                        enum_index,
                        variant_index,
                    },
                    ty: Some(hir::Type::Enum(enum_index)),
                    span: expr.span,
                })
            }
            _ => panic!("unknown namespace type"),
        }
    }

    fn build_continue_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let label = match &expr.kind {
            ast::ExprKind::Continue { label } => label.clone(),
            _ => unreachable!(),
        };

        let scope_index = match label {
            Some(label) => match ctx.resolve_label(label.symbol) {
                Some(scope_index) => scope_index,
                None => {
                    self.diagnostics.push(
                        UndeclaredLabelDiagnostic {
                            file_id: self.ast.file_id,
                            span: label.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
            },
            None => ctx
                .get_closest_loop_block()
                .expect("continue expression must be inside a loop or a block with a label"),
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::Continue { scope_index },
            ty: Some(hir::Type::Never),
            span: expr.span,
        })
    }

    fn build_loop_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &ast::Expression,
        label: Option<ast::Identifier>,
        access_ctx: AccessContext,
    ) -> Result<hir::Expression, ()> {
        let block = match &expr.kind {
            ast::ExprKind::Loop { block } => block,
            _ => unreachable!(),
        };

        func_ctx.enter_block(
            BlockScope {
                label: label.map(|l| l.symbol),
                kind: BlockKind::Loop,
                parent: Some(func_ctx.scope_index),
                locals: Vec::new(),
                inferred_type: None,
                expected_type: access_ctx.expected_type,
            },
            |ctx| {
                let block = self.build_block_expression(ctx, block)?;

                let scope = &ctx.frame.scopes[ctx.scope_index.0 as usize];
                match (scope.expected_type, scope.inferred_type) {
                    (Some(expected_type), Some(inferred_type))
                        if !inferred_type.coercible_to(expected_type) =>
                    {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected: expected_type,
                                actual: inferred_type,
                                span: expr.span,
                            }
                            .report(&self.global),
                        );
                        return Err(());
                    }
                    _ => {}
                }

                let ty = Some(block.ty.expect("loop block should have a type"));
                Ok(hir::Expression {
                    kind: hir::ExprKind::Loop {
                        scope_index: ctx.scope_index,
                        block: Box::new(block),
                    },
                    ty,
                    span: expr.span,
                })
            },
        )
    }

    fn build_cast_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
        access_ctx: AccessContext,
    ) -> Result<hir::Expression, ()> {
        let (value, cast_type) = match &expr.kind {
            ast::ExprKind::Cast { value, ty } => (value, ty),
            _ => unreachable!(),
        };

        match self.resolve_type(cast_type) {
            Ok(cast_type) => {
                let mut value = self.build_expression(
                    ctx,
                    value,
                    AccessContext {
                        expected_type: Some(cast_type),
                        access_kind: access_ctx.access_kind,
                    },
                )?;
                match value.ty {
                    Some(ty) => if ty != cast_type {},
                    None => self.coerce_untyped_expr(&mut value, cast_type)?,
                };

                Ok(value)
            }
            Err(_) => self.build_expression(ctx, value, access_ctx),
        }
    }

    fn infer_block_type(
        &mut self,
        scope: &BlockScope,
        value: &hir::Expression,
    ) -> Result<Type, ()> {
        match value.ty {
            Some(result_type) => match scope.inferred_type {
                Some(inferred) if !result_type.coercible_to(inferred) => {
                    self.diagnostics.push(
                        TypeMistmatchDiagnostic {
                            file_id: self.ast.file_id,
                            expected: inferred,
                            actual: result_type,
                            span: value.span,
                        }
                        .report(&self.global),
                    );
                    Ok(inferred)
                }
                Some(inferred) => Ok(inferred),
                None => match scope.expected_type {
                    Some(expected) if !result_type.coercible_to(expected) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected,
                                actual: result_type,
                                span: value.span,
                            }
                            .report(&self.global),
                        );
                        Err(())
                    }
                    _ => Ok(result_type),
                },
            },
            None => match scope.inferred_type.or(scope.expected_type) {
                Some(ty) => Ok(ty),
                None => {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.ast.file_id,
                            span: value.span,
                        }
                        .report(),
                    );
                    return Err(());
                }
            },
        }
    }

    fn build_block_statements(
        &mut self,
        ctx: &mut FunctionContext,
        statements: &[ast::Separated<ast::StmtKind>],
    ) -> BlockState<Box<[hir::Expression]>> {
        let mut expressions = Vec::with_capacity(statements.len());
        for stmt in statements.iter() {
            let expr = match self.build_statement(ctx, &stmt) {
                Ok(expr) => expr,
                Err(_) => continue,
            };

            match expr.ty {
                Some(Type::Unit) => expressions.push(expr),
                Some(Type::Never) => {
                    expressions.push(expr);
                    return BlockState::Exhaustive(expressions.into_boxed_slice());
                }
                _ => unreachable!(),
            }
        }

        BlockState::Incomplete(expressions.into_boxed_slice())
    }

    fn report_unused_locals(&mut self, block: &BlockScope) {
        for local in block.locals.iter() {
            if local.accesses.is_empty() {
                self.diagnostics.push(
                    UnusedVariableDiagnostic {
                        file_id: self.ast.file_id,
                        span: local.name.span,
                    }
                    .report(),
                );
            }

            match local.mutability {
                Some(mut_span)
                    if !local.accesses.iter().any(|access| {
                        access.kind == AccessKind::Write || access.kind == AccessKind::ReadWrite
                    }) =>
                {
                    self.diagnostics.push(
                        UnnecessaryMutabilityDiagnostic {
                            file_id: self.ast.file_id,
                            span: TextSpan::merge(mut_span, local.name.span),
                        }
                        .report(),
                    );
                }
                _ => {}
            }
        }
    }

    fn build_block_expression(
        &mut self,
        ctx: &mut FunctionContext,
        block: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let statements = match &block.kind {
            ast::ExprKind::Block(block) => block,
            _ => unreachable!(),
        };

        let (statements, result) = match statements.inner.split_last() {
            Some((last, rest)) if last.separator.is_none() => match &last.inner {
                ast::StmtKind::Expression { expr } => (rest, Some(expr.as_ref())),
                _ => (statements.inner.as_ref(), None),
            },
            _ => (statements.inner.as_ref(), None),
        };

        let expressions = match self.build_block_statements(ctx, statements) {
            BlockState::Exhaustive(expressions) => {
                self.report_unused_locals(&ctx.frame.scopes[ctx.scope_index.0 as usize]);
                if result.is_some() || expressions.len() < statements.len() {
                    self.diagnostics.push(
                        UnreachableCodeDiagnostic {
                            file_id: self.ast.file_id,
                            span: TextSpan::merge(
                                match statements.get(expressions.len()) {
                                    Some(stmt) => stmt.span,
                                    None => result.unwrap().span,
                                },
                                match result {
                                    Some(result) => result.span,
                                    None => statements.last().unwrap().span,
                                },
                            ),
                        }
                        .report(),
                    );
                }

                let scope = &mut ctx.frame.scopes[ctx.scope_index.0 as usize];
                let inferred_type = scope.inferred_type.unwrap_or(Type::Never);
                scope.inferred_type = Some(inferred_type);

                return Ok(hir::Expression {
                    kind: ExprKind::Block {
                        scope_index: ctx.scope_index,
                        expressions,
                        result: None,
                    },
                    ty: Some(inferred_type),
                    span: block.span,
                });
            }
            BlockState::Incomplete(expressions) => expressions,
        };

        match ctx.frame.scopes[ctx.scope_index.0 as usize].kind {
            BlockKind::Loop => {
                let result = match result {
                    Some(result) => Some(self.build_expression(
                        ctx,
                        &result,
                        AccessContext {
                            expected_type: Some(Type::Unit),
                            access_kind: AccessKind::Read,
                        },
                    )?),
                    None => None,
                };

                self.report_unused_locals(&ctx.frame.scopes[ctx.scope_index.0 as usize]);

                Ok(hir::Expression {
                    kind: ExprKind::Block {
                        scope_index: ctx.scope_index,
                        expressions,
                        result: result.map(Box::new),
                    },
                    ty: Some(
                        ctx.frame.scopes[ctx.scope_index.0 as usize]
                            .inferred_type
                            .unwrap_or(Type::Never),
                    ),
                    span: block.span,
                })
            }
            BlockKind::Block => {
                let result = self.build_block_result(ctx, result.as_deref())?;

                self.report_unused_locals(&ctx.frame.scopes[ctx.scope_index.0 as usize]);

                let scope = &ctx.frame.scopes[ctx.scope_index.0 as usize];
                let inferred_type = scope.inferred_type.expect("should have inferred type");
                match scope.expected_type {
                    Some(expected_type) if !inferred_type.coercible_to(expected_type) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected: expected_type,
                                actual: inferred_type,
                                span: block.span,
                            }
                            .report(&self.global),
                        );
                        return Err(());
                    }
                    _ => {}
                }

                Ok(hir::Expression {
                    kind: ExprKind::Block {
                        scope_index: ctx.scope_index,
                        expressions,
                        result: result.map(Box::new),
                    },
                    ty: Some(inferred_type),
                    span: block.span,
                })
            }
        }
    }

    fn build_block_result(
        &mut self,
        ctx: &mut FunctionContext,
        result: Option<&ast::Expression>,
    ) -> Result<Option<hir::Expression>, ()> {
        match result {
            Some(result) => {
                let mut result = self.build_expression(
                    ctx,
                    result,
                    AccessContext {
                        expected_type: ctx.frame.scopes[ctx.scope_index.0 as usize].expected_type,
                        access_kind: AccessKind::Read,
                    },
                )?;

                let scope = &mut ctx.frame.scopes[ctx.scope_index.0 as usize];
                let inferred_type = self.infer_block_type(scope, &result)?;
                scope.inferred_type = Some(inferred_type);
                match result.ty {
                    None => {
                        _ = self.coerce_untyped_expr(&mut result, inferred_type);
                    }
                    _ => {}
                }

                Ok(Some(result))
            }
            None => {
                let scope = &mut ctx.frame.scopes[ctx.scope_index.0 as usize];
                let inferred_type = scope.inferred_type.unwrap_or(Type::Unit);
                scope.inferred_type = Some(inferred_type);

                Ok(None)
            }
        }
    }

    fn build_return_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let value = match &expr.kind {
            ast::ExprKind::Return { value } => value,
            _ => unreachable!(),
        };

        match value {
            Some(value) => Ok(self
                .build_expression(
                    ctx,
                    value,
                    AccessContext {
                        expected_type: ctx.frame.scopes.get(0).unwrap().expected_type,
                        access_kind: AccessKind::Read,
                    },
                )
                .and_then(|mut value| {
                    let scope = ctx.frame.scopes.get_mut(0).unwrap();
                    let inferred_type = self.infer_block_type(scope, &value)?;
                    scope.inferred_type = Some(inferred_type);
                    match value.ty {
                        None => {
                            self.coerce_untyped_expr(&mut value, inferred_type)?;
                        }
                        _ => {}
                    }

                    match scope.expected_type {
                        Some(expected_type) if !inferred_type.coercible_to(expected_type) => {
                            self.diagnostics.push(
                                TypeMistmatchDiagnostic {
                                    file_id: self.ast.file_id,
                                    expected: expected_type,
                                    actual: inferred_type,
                                    span: value.span,
                                }
                                .report(&self.global),
                            );
                            return Err(());
                        }
                        _ => {}
                    };

                    Ok(hir::Expression {
                        kind: hir::ExprKind::Return {
                            value: Some(Box::new(value)),
                        },
                        ty: Some(hir::Type::Never),
                        span: expr.span,
                    })
                })
                .unwrap_or(hir::Expression {
                    kind: hir::ExprKind::Unreachable,
                    ty: Some(hir::Type::Never),
                    span: expr.span,
                })),
            None => {
                let scope = ctx
                    .frame
                    .scopes
                    .get_mut(ctx.scope_index.0 as usize)
                    .unwrap();

                let inferred_type = scope.inferred_type.unwrap_or(Type::Unit);
                scope.inferred_type = Some(inferred_type);

                match scope.expected_type {
                    Some(expected_type) if inferred_type.coercible_to(expected_type) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected: expected_type,
                                actual: inferred_type,
                                span: expr.span,
                            }
                            .report(&self.global),
                        );
                        return Err(());
                    }
                    _ => {}
                };

                Ok(hir::Expression {
                    kind: hir::ExprKind::Return { value: None },
                    ty: Some(hir::Type::Never),
                    span: expr.span,
                })
            }
        }
    }

    fn build_break_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (label, value) = match &expr.kind {
            ast::ExprKind::Break { label, value } => (label.clone(), value),
            _ => unreachable!(),
        };

        let scope_index = match label {
            Some(label) => match ctx.resolve_label(label.symbol) {
                Some(scope_index) => scope_index,
                None => {
                    self.diagnostics.push(
                        UndeclaredLabelDiagnostic {
                            file_id: self.ast.file_id,
                            span: label.span,
                        }
                        .report(),
                    );

                    return Ok(Expression {
                        kind: ExprKind::Unreachable,
                        ty: Some(Type::Never),
                        span: expr.span,
                    });
                }
            },
            None => match ctx.get_closest_loop_block() {
                Some(scope_index) => scope_index,
                None => {
                    self.diagnostics.push(
                        BreakOutsideOfLoopDiagnostic {
                            file_id: self.ast.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );

                    return Ok(Expression {
                        kind: ExprKind::Error,
                        ty: Some(Type::Never),
                        span: expr.span,
                    });
                }
            },
        };

        match value {
            Some(value) => Ok(self
                .build_expression(
                    ctx,
                    value,
                    AccessContext {
                        expected_type: ctx
                            .frame
                            .scopes
                            .get(scope_index.0 as usize)
                            .unwrap()
                            .expected_type,
                        access_kind: AccessKind::Read,
                    },
                )
                .and_then(|mut value| {
                    let scope = ctx.frame.scopes.get_mut(scope_index.0 as usize).unwrap();
                    let inferred_type = self.infer_block_type(scope, &value)?;
                    match value.ty {
                        None => {
                            self.coerce_untyped_expr(&mut value, inferred_type)?;
                        }
                        _ => {}
                    }
                    scope.inferred_type = Some(inferred_type);

                    Ok(Expression {
                        kind: ExprKind::Break {
                            scope_index,
                            value: Some(Box::new(value)),
                        },
                        ty: Some(Type::Never),
                        span: expr.span,
                    })
                })
                .unwrap_or(hir::Expression {
                    kind: hir::ExprKind::Unreachable,
                    ty: Some(Type::Never),
                    span: expr.span,
                })),
            None => {
                let scope = ctx.frame.scopes.get_mut(scope_index.0 as usize).unwrap();
                match scope.inferred_type {
                    Some(inferred) => match Type::Unit.coercible_to(inferred) {
                        true => {}
                        false => {
                            self.diagnostics.push(
                                TypeMistmatchDiagnostic {
                                    file_id: self.ast.file_id,
                                    expected: inferred,
                                    actual: Type::Unit,
                                    span: expr.span,
                                }
                                .report(&self.global),
                            );
                        }
                    },
                    None => {
                        scope.inferred_type = Some(Type::Unit);
                    }
                }

                Ok(Expression {
                    kind: ExprKind::Break {
                        scope_index,
                        value: None,
                    },
                    ty: Some(Type::Never),
                    span: expr.span,
                })
            }
        }
    }

    fn build_if_else_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
        label: Option<ast::Identifier>,
        access_ctx: AccessContext,
    ) -> Result<hir::Expression, ()> {
        let (condition, then_block, maybe_else_block) = match &expr.kind {
            ast::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => (condition, then_block, else_block),
            _ => unreachable!(),
        };

        let condition = self.build_expression(
            ctx,
            condition,
            AccessContext {
                expected_type: Some(Type::Bool),
                access_kind: AccessKind::Read,
            },
        )?;

        let then_block = match then_block.kind {
            ast::ExprKind::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: label.clone().map(|l| l.symbol),
                    kind: BlockKind::Block,
                    parent: Some(ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
                    expected_type: match maybe_else_block {
                        Some(_) => access_ctx.expected_type,
                        None => None,
                    },
                },
                |ctx| self.build_block_expression(ctx, then_block),
            )?,
            _ => unreachable!(),
        };
        let (else_block, ty) = match maybe_else_block {
            Some(ast_else_block) => {
                let else_block = match ast_else_block.kind {
                    ast::ExprKind::Block { .. } => ctx.enter_block(
                        BlockScope {
                            label: label.map(|l| l.symbol),
                            kind: BlockKind::Block,
                            parent: Some(ctx.scope_index),
                            locals: Vec::new(),
                            inferred_type: None,
                            expected_type: access_ctx.expected_type,
                        },
                        |ctx| self.build_block_expression(ctx, ast_else_block),
                    )?,
                    _ => unreachable!(),
                };

                match (then_block.ty, else_block.ty) {
                    (Some(ty1), Some(ty2)) => match Type::unify(ty1, ty2) {
                        Ok(ty) => (Some(else_block), Some(ty)),
                        Err(_) => {
                            self.diagnostics.push(
                                TypeMistmatchDiagnostic {
                                    file_id: self.ast.file_id,
                                    expected: ty1,
                                    actual: ty2,
                                    span: ast_else_block.span,
                                }
                                .report(&self.global),
                            );
                            return Err(());
                        }
                    },
                    _ => unreachable!(),
                }
            }
            None => match then_block.ty {
                Some(hir::Type::Unit | hir::Type::Never) => (None, Some(hir::Type::Unit)),
                ty => panic!(
                    "if you want to return a value from if-else, you must provide an else block {ty:?}"
                ),
            },
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::IfElse {
                condition: Box::new(condition),
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new),
            },
            ty,
            span: expr.span,
        })
    }

    fn build_call_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (ast_callee, ast_arguments) = match &expr.kind {
            ast::ExprKind::Call { callee, arguments } => (callee, arguments),
            _ => unreachable!("expected call expression"),
        };

        let callee = self.build_expression(
            ctx,
            ast_callee,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
        )?;
        let func_index = match callee.ty {
            Some(Type::Function(func_index)) => func_index,
            Some(ty) => {
                self.diagnostics.push(
                    CannotCallExpressionDiagnostic {
                        file_id: self.ast.file_id,
                        span: ast_callee.span,
                        ty,
                    }
                    .report(&self.global),
                );

                return Ok(hir::Expression {
                    kind: hir::ExprKind::Call {
                        callee: Box::new(callee),
                        arguments: Box::new([]),
                    },
                    ty: Some(Type::Unknown),
                    span: expr.span,
                });
            }
            None => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: ast_callee.span,
                    }
                    .report(),
                );

                return Ok(hir::Expression {
                    kind: hir::ExprKind::Call {
                        callee: Box::new(callee),
                        arguments: Box::new([]),
                    },
                    ty: Some(Type::Unknown),
                    span: expr.span,
                });
            }
        };

        let arguments: Box<_> = ast_arguments
            .inner
            .iter()
            .enumerate()
            .map(|(index, argument)| {
                let func_type = self
                    .global
                    .function_types
                    .get(func_index.0 as usize)
                    .unwrap();
                let expected_type = func_type.params.get(index).copied().unwrap();

                let mut argument = self.build_expression(
                    ctx,
                    &argument.inner,
                    AccessContext {
                        expected_type: Some(expected_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match argument.ty {
                    Some(ty) if !ty.coercible_to(expected_type) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected: expected_type,
                                actual: ty,
                                span: argument.span,
                            }
                            .report(&self.global),
                        );
                    }
                    Some(_) => {}
                    None => {
                        self.coerce_untyped_expr(&mut argument, expected_type)?;
                    }
                }

                Ok(argument)
            })
            .collect::<Result<_, _>>()?;

        Ok(hir::Expression {
            kind: hir::ExprKind::Call {
                callee: Box::new(callee),
                arguments,
            },
            ty: Some(
                self.global
                    .function_types
                    .get(func_index.0 as usize)
                    .unwrap()
                    .result,
            ),
            span: expr.span,
        })
    }

    fn build_unary_expression(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let (operator, ast_operand) = match &expr.kind {
            ast::ExprKind::Unary { operator, operand } => (operator.clone(), operand),
            _ => unreachable!(),
        };
        let mut operand = self.build_expression(
            ctx,
            ast_operand,
            AccessContext {
                expected_type: access_ctx.expected_type,
                access_kind: AccessKind::Read,
            },
        )?;

        match operator.kind {
            ast::UnOpKind::InvertSign | ast::UnOpKind::BitNot => match operand.ty {
                Some(Type::Primitive(_)) | None => {
                    let ty = operand.ty.clone();
                    Ok(hir::Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty,
                        span: expr.span,
                    })
                }
                _ => panic!("can't apply unary operator to this type"),
            },
            // ast::UnOpKind::InvertSign | ast::UnOpKind::BitNot => match operand.ty {
            //     Some(Type::Primitive(_)) | None => {
            //         let ty = operand.ty.clone();
            //         Ok(hir::Expression {
            //             kind: ExprKind::Unary {
            //                 operator,
            //                 operand: Box::new(operand),
            //             },
            //             ty,
            //             span: expr.span,
            //         })
            //     }
            //     _ => panic!("can't apply unary operator to this type"),
            // },
            ast::UnOpKind::Not => match operand.ty {
                Some(Type::Bool) => Ok(hir::Expression {
                    kind: ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty: Some(Type::Bool),
                    span: expr.span,
                }),
                Some(ty) => {
                    self.diagnostics.push(
                        UnaryOperatorCannotBeAppliedDiagnostic {
                            file_id: self.ast.file_id,
                            operator: operator.clone(),
                            operand: TypeWithSpan {
                                ty,
                                span: operand.span,
                            },
                        }
                        .report(&self.global),
                    );

                    Ok(hir::Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: Some(Type::Bool),
                        span: expr.span,
                    })
                }
                None => {
                    _ = self.coerce_untyped_expr(&mut operand, Type::Bool);

                    Ok(hir::Expression {
                        kind: ExprKind::Unary {
                            operator,
                            operand: Box::new(operand),
                        },
                        ty: Some(Type::Bool),
                        span: expr.span,
                    })
                }
            },
        }
    }

    fn build_binary_expression(
        &mut self,
        func_ctx: &mut FunctionContext,
        expr: &ast::Expression,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let operator = match &expr.kind {
            ast::ExprKind::Binary { operator, .. } => operator.clone(),
            _ => unreachable!(),
        };

        use ast::BinOpKind;
        match operator.kind {
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Rem => {
                self.build_arithmetic_expr(func_ctx, expr, access_ctx)
            }
            BinOpKind::Assign => self.build_assignment_expr(func_ctx, expr),
            BinOpKind::AddAssign
            | BinOpKind::SubAssign
            | BinOpKind::MulAssign
            | BinOpKind::DivAssign
            | BinOpKind::RemAssign => self.build_arithmetic_assignment_expr(func_ctx, expr),
            BinOpKind::Eq
            | BinOpKind::NotEq
            | BinOpKind::Less
            | BinOpKind::LessEq
            | BinOpKind::Greater
            | BinOpKind::GreaterEq => self.build_comparison_binary_expr(func_ctx, expr),
            BinOpKind::And | BinOpKind::Or => self.build_logical_binary_expr(func_ctx, expr),
            BinOpKind::BitAnd
            | BinOpKind::BitOr
            | BinOpKind::BitXor
            | BinOpKind::LeftShift
            | BinOpKind::RightShift => self.build_bitwise_binary_expr(func_ctx, expr, access_ctx),
        }
    }

    fn build_assignment_expr(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let left = self.build_expression(
            ctx,
            left,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Write,
            },
        )?;
        match left.kind {
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let local = match ctx.frame.get_mut_local(scope_index, local_index) {
                    Some(local) => local,
                    None => {
                        self.diagnostics.push(
                            UndeclaredIdentifierDiagnostic {
                                file_id: self.ast.file_id,
                                span: left.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                };
                match local.mutability {
                    None => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    _ => {}
                }

                let local_type = local.ty;

                let mut right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: Some(local_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match right.ty {
                    Some(ty) if !ty.coercible_to(local_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left: TypeWithSpan {
                                    ty: local_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right: TypeWithSpan {
                                    ty,
                                    span: right.span,
                                },
                            }
                            .report(&self.global),
                        );
                    }
                    Some(_) => {}
                    None => {
                        self.coerce_untyped_expr(&mut right, local_type)?;
                    }
                }

                Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                })
            }
            hir::ExprKind::Global { global_index } => {
                let global = self
                    .global
                    .globals
                    .get_mut(global_index.0 as usize)
                    .unwrap();

                match global.mutability {
                    None => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    _ => {}
                }

                let global_type = global.ty;
                let mut right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: Some(global_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match right.ty {
                    Some(ty) if !ty.coercible_to(global_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left: TypeWithSpan {
                                    ty: global_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right: TypeWithSpan {
                                    ty,
                                    span: right.span,
                                },
                            }
                            .report(&self.global),
                        );
                    }
                    Some(_) => {}
                    None => {
                        self.coerce_untyped_expr(&mut right, global_type)?;
                    }
                }

                Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                })
            }
            hir::ExprKind::Placeholder => {
                let right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: None,
                        access_kind: AccessKind::Read,
                    },
                )?;
                let right_type = match right.ty {
                    Some(ty) => ty,
                    None => {
                        self.diagnostics.push(
                            TypeAnnotationRequiredDiagnostic {
                                file_id: self.ast.file_id,
                                span: right.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                };

                return Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        left: Box::new(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: Some(right_type),
                            span: left.span,
                        }),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                });
            }
            _ => {
                self.diagnostics.push(
                    InvalidAssignmentTargetDiagnostic {
                        file_id: self.ast.file_id,
                        span: left.span,
                    }
                    .report(),
                );

                Ok(hir::Expression {
                    kind: hir::ExprKind::Unreachable,
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                })
            }
        }
    }

    fn build_arithmetic_assignment_expr(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let left = self.build_expression(
            ctx,
            left,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::ReadWrite,
            },
        )?;
        match left.kind {
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let local = match ctx.frame.get_mut_local(scope_index, local_index) {
                    Some(local) => local,
                    None => {
                        self.diagnostics.push(
                            UndeclaredIdentifierDiagnostic {
                                file_id: self.ast.file_id,
                                span: left.span,
                            }
                            .report(),
                        );
                        return Err(());
                    }
                };
                match local.ty {
                    Type::Primitive(_) => {}
                    _ => {
                        self.diagnostics.push(
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.ast.file_id,
                                operator,
                                operand: TypeWithSpan {
                                    ty: local.ty,
                                    span: left.span,
                                },
                            }
                            .report(&self.global),
                        );

                        return Err(());
                    }
                }

                match local.mutability {
                    None => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    _ => {}
                }

                let local_type = local.ty;
                let mut right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: Some(local_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match right.ty {
                    Some(ty) if !ty.coercible_to(local_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left: TypeWithSpan {
                                    ty: local_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right: TypeWithSpan {
                                    ty,
                                    span: right.span,
                                },
                            }
                            .report(&self.global),
                        );
                    }
                    Some(_) => {}
                    None => {
                        self.coerce_untyped_expr(&mut right, local_type)?;
                    }
                }

                Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                })
            }
            ExprKind::Global { global_index } => {
                let global = self
                    .global
                    .globals
                    .get_mut(global_index.0 as usize)
                    .unwrap();

                match global.ty {
                    Type::Primitive(_) => {}
                    _ => {
                        self.diagnostics.push(
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.ast.file_id,
                                operator,
                                operand: TypeWithSpan {
                                    ty: global.ty,
                                    span: left.span,
                                },
                            }
                            .report(&self.global),
                        );

                        return Err(());
                    }
                }

                match global.mutability {
                    None => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    Some(_) => {}
                }

                let global_type = global.ty;
                let mut right = self.build_expression(
                    ctx,
                    right,
                    AccessContext {
                        expected_type: Some(global_type),
                        access_kind: AccessKind::Read,
                    },
                )?;
                match right.ty {
                    Some(ty) if !ty.coercible_to(global_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left: TypeWithSpan {
                                    ty: global_type,
                                    span: left.span,
                                },
                                operator: operator.clone(),
                                right: TypeWithSpan {
                                    ty,
                                    span: right.span,
                                },
                            }
                            .report(&self.global),
                        );
                    }
                    Some(_) => {}
                    None => {
                        self.coerce_untyped_expr(&mut right, global_type)?;
                    }
                }

                Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                })
            }
            _ => {
                self.diagnostics.push(
                    InvalidAssignmentTargetDiagnostic {
                        file_id: self.ast.file_id,
                        span: left.span,
                    }
                    .report(),
                );

                Ok(hir::Expression {
                    kind: hir::ExprKind::Error,
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                })
            }
        }
    }

    fn build_bitwise_binary_expr(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
        access_ctx: AccessContext,
    ) -> Result<hir::Expression, ()> {
        let (left, right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let mut left = self.build_expression(ctx, left, access_ctx.clone())?;
        let mut right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: left.ty.or(access_ctx.expected_type),
                access_kind: access_ctx.access_kind,
            },
        )?;

        match (left.ty, right.ty) {
            (Some(Type::Primitive(p1)), Some(Type::Primitive(p2)))
                if p1.is_integer() && p2.is_integer() && p1 == p2 =>
            {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Primitive(p1)),
                    span: expr.span,
                })
            }
            (Some(ty), None) => {
                match ty {
                    Type::Primitive(primitive) if primitive.is_integer() => {}
                    _ => {
                        self.diagnostics.push(
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.ast.file_id,
                                operator: operator.clone(),
                                operand: TypeWithSpan {
                                    ty,
                                    span: left.span,
                                },
                            }
                            .report(&self.global),
                        );
                    }
                }
                self.coerce_untyped_expr(&mut right, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(ty),
                    span: expr.span,
                })
            }
            (None, Some(ty)) => {
                match ty {
                    Type::Primitive(primitive) if primitive.is_integer() => {}
                    _ => {
                        self.diagnostics.push(
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.ast.file_id,
                                operator: operator.clone(),
                                operand: TypeWithSpan {
                                    ty,
                                    span: right.span,
                                },
                            }
                            .report(&self.global),
                        );
                    }
                }
                self.coerce_untyped_expr(&mut left, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(ty),
                    span: expr.span,
                })
            }
            (None, None) => match access_ctx.expected_type {
                Some(expected_type) => {
                    self.coerce_untyped_expr(&mut left, expected_type)?;
                    self.coerce_untyped_expr(&mut right, expected_type)?;

                    match expected_type {
                        Type::Primitive(primitive) if primitive.is_integer() => {}
                        _ => {
                            self.diagnostics.push(
                                BinaryOperatorCannotBeAppliedDiagnostic {
                                    file_id: self.ast.file_id,
                                    operator: operator.clone(),
                                    operand: TypeWithSpan {
                                        ty: expected_type,
                                        span: left.span,
                                    },
                                }
                                .report(&self.global),
                            );
                        }
                    }

                    Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ty: Some(expected_type),
                        span: expr.span,
                    })
                }
                None => {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.ast.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                    Err(())
                }
            },
            (Some(left_type), Some(right_type)) => {
                self.diagnostics.push(
                    BinaryExpressionMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        left: TypeWithSpan {
                            ty: left_type,
                            span: left.span,
                        },
                        operator: operator.clone(),
                        right: TypeWithSpan {
                            ty: right_type,
                            span: right.span,
                        },
                    }
                    .report(&self.global),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: access_ctx.expected_type.or(Some(Type::Unknown)),
                    span: expr.span,
                })
            }
        }
    }

    fn build_arithmetic_expr(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
        access_ctx: AccessContext,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let mut left = self.build_expression(
            ctx,
            left,
            AccessContext {
                expected_type: access_ctx.expected_type,
                access_kind: AccessKind::Read,
            },
        )?;
        let mut right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: left.ty.or(access_ctx.expected_type),
                access_kind: AccessKind::Read,
            },
        )?;

        match (left.ty, right.ty) {
            (Some(Type::Primitive(ty1)), Some(Type::Primitive(ty2))) if ty1 == ty2 => {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Primitive(ty1)),
                    span: expr.span,
                })
            }
            (None, Some(ty)) => {
                match ty {
                    Type::Primitive(_) => {}
                    ty => {
                        self.diagnostics.push(
                            BinaryOperatorCannotBeAppliedDiagnostic {
                                file_id: self.ast.file_id,
                                operator: operator.clone(),
                                operand: TypeWithSpan {
                                    ty,
                                    span: right.span,
                                },
                            }
                            .report(&self.global),
                        );

                        return Ok(Expression {
                            kind: ExprKind::Binary {
                                operator,
                                left: Box::new(left),
                                right: Box::new(right),
                            },
                            ty: match access_ctx.expected_type {
                                Some(expected) => Some(expected),
                                None => Some(Type::Unknown),
                            },
                            span: expr.span,
                        });
                    }
                }
                self.coerce_untyped_expr(&mut left, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(ty),
                    span: expr.span,
                })
            }
            (Some(ty), None) => {
                self.coerce_untyped_expr(&mut right, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(ty),
                    span: expr.span,
                })
            }
            (Some(Type::Never), _) => {
                self.diagnostics.push(
                    UnreachableCodeDiagnostic {
                        file_id: self.ast.file_id,
                        span: right.span,
                    }
                    .report(),
                );

                return Ok(left);
            }
            (_, Some(Type::Never)) => {
                self.diagnostics.push(
                    UnreachableCodeDiagnostic {
                        file_id: self.ast.file_id,
                        span: operator.span,
                    }
                    .report(),
                );

                return Ok(right);
            }
            (None, None) => match access_ctx.expected_type {
                Some(_) => Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: None,
                    span: expr.span,
                }),
                None => {
                    self.diagnostics.push(
                        TypeAnnotationRequiredDiagnostic {
                            file_id: self.ast.file_id,
                            span: expr.span,
                        }
                        .report(),
                    );
                    Err(())
                }
            },
            (Some(left_type), Some(right_type)) => {
                self.diagnostics.push(
                    BinaryExpressionMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        left: TypeWithSpan {
                            ty: left_type,
                            span: left.span,
                        },
                        operator: operator.clone(),
                        right: TypeWithSpan {
                            ty: right_type,
                            span: right.span,
                        },
                    }
                    .report(&self.global),
                );

                match access_ctx.expected_type {
                    Some(expected) => Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        ty: Some(expected),
                        span: expr.span,
                    }),
                    None => Err(()),
                }
            }
        }
    }

    fn build_comparison_binary_expr(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (left, right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let mut left = self.build_expression(
            ctx,
            left,
            AccessContext {
                expected_type: None,
                access_kind: AccessKind::Read,
            },
        )?;
        let mut right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: left.ty,
                access_kind: AccessKind::Read,
            },
        )?;

        match (left.ty, right.ty) {
            (Some(Type::Primitive(primitive_1)), Some(Type::Primitive(primitive_2)))
                if primitive_1 == primitive_2 =>
            {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                    span: expr.span,
                })
            }
            (Some(Type::Enum(enum_index_1)), Some(Type::Enum(enum_index_2)))
                if enum_index_1 == enum_index_2 =>
            {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                    span: expr.span,
                })
            }
            (Some(Type::Bool), Some(Type::Bool)) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: Some(Type::Bool),
                span: expr.span,
            }),
            (None, Some(ty)) => {
                self.coerce_untyped_expr(&mut left, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                    span: expr.span,
                })
            }
            (Some(ty), None) => {
                self.coerce_untyped_expr(&mut right, ty)?;

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                    span: expr.span,
                })
            }
            (None, None) => {
                self.diagnostics.push(
                    ComparisonTypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        left: left.span,
                        right: right.span,
                    }
                    .report(),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                    span: expr.span,
                })
            }
            (Some(left_type), Some(right_type)) => {
                self.diagnostics.push(
                    BinaryExpressionMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        left: TypeWithSpan {
                            ty: left_type,
                            span: left.span,
                        },
                        operator: operator.clone(),
                        right: TypeWithSpan {
                            ty: right_type,
                            span: right.span,
                        },
                    }
                    .report(&self.global),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                    span: expr.span,
                })
            }
        }
    }

    fn build_logical_binary_expr(
        &mut self,
        ctx: &mut FunctionContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (left, right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let left = self.build_expression(
            ctx,
            left,
            AccessContext {
                expected_type: Some(Type::Bool),
                access_kind: AccessKind::Read,
            },
        )?;
        match left.ty {
            Some(Type::Bool) => {}
            Some(actual) => {
                self.diagnostics.push(
                    TypeMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        expected: Type::Bool,
                        actual,
                        span: left.span,
                    }
                    .report(&self.global),
                );
            }
            None => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: left.span,
                    }
                    .report(),
                );
            }
        }
        let right = self.build_expression(
            ctx,
            right,
            AccessContext {
                expected_type: Some(Type::Bool),
                access_kind: AccessKind::Read,
            },
        )?;
        match right.ty {
            Some(Type::Bool) => {}
            Some(actual) => {
                self.diagnostics.push(
                    TypeMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        expected: Type::Bool,
                        actual,
                        span: right.span,
                    }
                    .report(&self.global),
                );
            }
            None => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: right.span,
                    }
                    .report(),
                );
            }
        }

        Ok(hir::Expression {
            kind: hir::ExprKind::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            },
            ty: Some(hir::Type::Bool),
            span: expr.span,
        })
    }

    fn coerce_untyped_expr(
        &mut self,
        expr: &mut hir::Expression,
        target_type: hir::Type,
    ) -> Result<(), ()> {
        match expr.kind {
            hir::ExprKind::Int(_) => self.coerce_untyped_int_expr(expr, target_type),
            hir::ExprKind::Float(_) => self.coerce_untyped_float_expr(expr, target_type),
            hir::ExprKind::Unary { .. } => self.coerce_untyped_unary_expr(expr, target_type),
            hir::ExprKind::Binary { .. } => {
                self.coerce_untyped_binary_expression(expr, target_type)
            }
            _ => unreachable!(),
        }
    }

    fn coerce_untyped_int_expr(
        &mut self,
        expr: &mut hir::Expression,
        target_type: hir::Type,
    ) -> Result<(), ()> {
        match target_type {
            Type::Primitive(PrimitiveType::I32) => {
                let value = match expr.kind {
                    hir::ExprKind::Int(value) => value,
                    _ => unreachable!(),
                };

                if value > i32::MAX as i64 || value < i32::MIN as i64 {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            primitive: PrimitiveType::I32,
                            value,
                            span: expr.span,
                        }
                        .report(),
                    );
                }

                expr.ty = Some(Type::Primitive(PrimitiveType::I32));
                Ok(())
            }
            Type::Primitive(PrimitiveType::I64) => {
                let value = match expr.kind {
                    hir::ExprKind::Int(value) => value,
                    _ => unreachable!(),
                };
                if value > i64::MAX || value < i64::MIN {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            primitive: PrimitiveType::I64,
                            value,
                            span: expr.span,
                        }
                        .report(),
                    );
                }

                expr.ty = Some(Type::Primitive(PrimitiveType::I64));
                Ok(())
            }
            Type::Primitive(PrimitiveType::F32) => {
                let value = match expr.kind {
                    hir::ExprKind::Int(value) => value,
                    _ => unreachable!(),
                };
                if (value as f32) > f32::MAX || (value as f32) < f32::MIN {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            primitive: PrimitiveType::F32,
                            value,
                            span: expr.span,
                        }
                        .report(),
                    );
                }

                expr.ty = Some(Type::Primitive(PrimitiveType::F32));
                Ok(())
            }
            Type::Primitive(PrimitiveType::F64) => {
                let value = match expr.kind {
                    hir::ExprKind::Int(value) => value,
                    _ => unreachable!(),
                };
                if (value as f64) > f64::MAX || (value as f64) < f64::MIN {
                    self.diagnostics.push(
                        IntegerLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            primitive: PrimitiveType::F64,
                            value,
                            span: expr.span,
                        }
                        .report(),
                    );
                }

                expr.ty = Some(Type::Primitive(PrimitiveType::F64));
                Ok(())
            }
            target_type => {
                self.diagnostics.push(
                    UnableToCoerceDiagnostic {
                        file_id: self.ast.file_id,
                        target_type,
                        span: expr.span,
                    }
                    .report(&self.global),
                );

                Err(())
            }
        }
    }

    fn coerce_untyped_float_expr(
        &mut self,
        expr: &mut hir::Expression,
        target_type: hir::Type,
    ) -> Result<(), ()> {
        match target_type {
            Type::Primitive(PrimitiveType::F32) => {
                let value = match expr.kind {
                    hir::ExprKind::Float(value) => value,
                    _ => unreachable!(),
                };
                if value > f32::MAX as f64 || value < f32::MIN as f64 {
                    self.diagnostics.push(
                        FloatLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            primitive: PrimitiveType::F32,
                            value,
                            span: expr.span,
                        }
                        .report(),
                    );
                }

                expr.ty = Some(Type::Primitive(PrimitiveType::F32));
                Ok(())
            }
            Type::Primitive(PrimitiveType::F64) => {
                let value = match expr.kind {
                    hir::ExprKind::Float(value) => value,
                    _ => unreachable!(),
                };
                if value > f64::MAX || value < f64::MIN {
                    self.diagnostics.push(
                        FloatLiteralOutOfRangeDiagnostic {
                            file_id: self.ast.file_id,
                            primitive: PrimitiveType::F64,
                            value,
                            span: expr.span,
                        }
                        .report(),
                    );
                }

                expr.ty = Some(Type::Primitive(PrimitiveType::F64));
                Ok(())
            }
            target_type => {
                self.diagnostics.push(
                    UnableToCoerceDiagnostic {
                        file_id: self.ast.file_id,
                        target_type,
                        span: expr.span,
                    }
                    .report(&self.global),
                );

                Err(())
            }
        }
    }

    fn coerce_untyped_unary_expr(
        &mut self,
        expr: &mut hir::Expression,
        target_type: hir::Type,
    ) -> Result<(), ()> {
        let (operand, operator) = match &mut expr.kind {
            hir::ExprKind::Unary { operand, operator } => (operand, operator.clone()),
            _ => unreachable!(),
        };

        match operator.kind {
            ast::UnOpKind::BitNot | ast::UnOpKind::InvertSign => match target_type {
                Type::Primitive(PrimitiveType::I32 | PrimitiveType::I64) => {}
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        match self.coerce_untyped_expr(operand, target_type) {
            Ok(_) => {
                expr.ty = Some(target_type);
                Ok(())
            }
            Err(_) => Err(()),
        }
    }

    fn coerce_untyped_binary_expression(
        &mut self,
        expr: &mut hir::Expression,
        target_type: hir::Type,
    ) -> Result<(), ()> {
        let (left, right, operator) = match &mut expr.kind {
            hir::ExprKind::Binary {
                operator,
                left,
                right,
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        match operator.kind {
            operator if operator.is_arithmetic() => match target_type {
                Type::Primitive(_) => {}
                target_type => {
                    self.diagnostics.push(
                        UnableToCoerceDiagnostic {
                            file_id: self.ast.file_id,
                            target_type,
                            span: expr.span,
                        }
                        .report(&self.global),
                    );
                    return Err(());
                }
            },
            operator if operator.is_bitwise() => match target_type {
                Type::Primitive(primitive) => match primitive {
                    PrimitiveType::I32
                    | PrimitiveType::I64
                    | PrimitiveType::U32
                    | PrimitiveType::U64 => {}
                    PrimitiveType::F32 | PrimitiveType::F64 => {
                        self.diagnostics.push(
                            UnableToCoerceDiagnostic {
                                file_id: self.ast.file_id,
                                target_type,
                                span: expr.span,
                            }
                            .report(&self.global),
                        );
                        return Err(());
                    }
                },
                target_type => {
                    self.diagnostics.push(
                        UnableToCoerceDiagnostic {
                            file_id: self.ast.file_id,
                            target_type,
                            span: expr.span,
                        }
                        .report(&self.global),
                    );
                    return Err(());
                }
            },
            _ => unreachable!(),
        };

        match (
            self.coerce_untyped_expr(left, target_type),
            self.coerce_untyped_expr(right, target_type),
        ) {
            (Ok(_), Ok(_)) => {
                expr.ty = Some(target_type);
                Ok(())
            }
            _ => Err(()),
        }
    }
}
