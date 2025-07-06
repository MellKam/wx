use core::panic;

use codespan_reporting::diagnostic::Diagnostic;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use crate::hir::diagnostics::{
    BinaryExpressionMistmatchDiagnostic, BreakOutsideOfLoopDiagnostic,
    CannotMutateImmutableDiagnostic, ComparisonTypeAnnotationRequiredDiagnostic,
    FloatLiteralOutOfRangeDiagnostic, IntegerLiteralOutOfRangeDiagnostic,
    InvalidEnumTypeDiagnostic, NonCallableIdentifierDiagnostic, OperatorCannotBeAppliedDiagnostic,
    TypeAnnotationRequiredDiagnostic, TypeMistmatchDiagnostic, UnableToCoerceDiagnostic,
    UndeclaredIdentifierDiagnostic, UndeclaredLabelDiagnostic, UnknownEnumVariantDiagnostic,
    UnreachableCodeDiagnostic, UnusedValueDiagnostic,
};
use crate::hir::global::{GlobalContext, GlobalValue};
use crate::hir::local::{BlockKind, BlockScope, LocalContext};
use crate::hir::*;
use crate::span::TextSpan;
use crate::{ast, hir};

pub struct Builder<'ast, 'interner> {
    ast: &'ast ast::Ast,
    global: GlobalContext<'interner>,
    diagnostics: Vec<Diagnostic<FileId>>,
    hir: HIR,
}

enum BlockState<T> {
    Exhaustive(T),
    Incomplete(T),
}

impl<'ast, 'interner> Builder<'ast, 'interner> {
    pub fn build(
        ast: &'ast ast::Ast,
        interner: &'interner mut StringInterner<StringBackend<SymbolU32>>,
    ) -> (HIR, Vec<Diagnostic<FileId>>) {
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

        (hir, diagnostics)
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
                .iter()
                .map(|param| self.resolve_type(&param.ty))
                .collect::<Result<Box<_>, ()>>()?,
            result: self.resolve_type(&signature.result)?,
        })
    }

    fn build_global_item(&mut self, item: &ast::Item) -> Result<Global, ()> {
        let (name, ty, value_expr, mutable) = match &item.kind {
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
            mutability: match mutable {
                Some(_) => Mutability::Mutable,
                None => Mutability::Const,
            },
            ty,
            value: expr,
        })
    }

    fn build_function_definition(&mut self, item: &ast::Item) -> Result<hir::Function, ()> {
        let (signature, block) = match &item.kind {
            ast::ItemKind::FunctionDefinition { signature, block } => (signature, block),
            _ => unreachable!(),
        };

        let locals = signature
            .params
            .iter()
            .map(|param| hir::Local {
                name: param.name,
                ty: self.resolve_type(&param.ty).unwrap(),
                mutability: match param.mutable {
                    Some(_) => Mutability::Mutable,
                    None => Mutability::Const,
                },
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

        let mut ctx = LocalContext {
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
            name: signature.name,
            stack: ctx.frame,
            block: Box::new(block),
        })
    }

    fn build_enum_item(&mut self, item: &ast::Item) -> Result<Enum, ()> {
        let (name, ty, variants) = match &item.kind {
            ast::ItemKind::EnumDefinition { name, ty, variants } => (name.clone(), ty, variants),
            _ => unreachable!(),
        };

        let ty = match self.resolve_type(ty)? {
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
            .iter()
            .map(|variant| {
                let mut value = match variant.value.kind {
                    ast::ExprKind::Int { value } => hir::Expression {
                        kind: hir::ExprKind::Int(value),
                        ty: None,
                        span: variant.value.span,
                    },
                    ast::ExprKind::Float { value } => hir::Expression {
                        kind: hir::ExprKind::Float(value),
                        ty: None,
                        span: variant.value.span,
                    },
                    _ => panic!("invalid enum value"),
                };

                self.coerce_untyped_expr(&mut value, Type::Primitive(ty))?;

                Ok(EnumVariant {
                    name: variant.name.clone(),
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
        ctx: &mut LocalContext,
        statement: &ast::Statement,
    ) -> Result<hir::Expression, ()> {
        match &statement.kind {
            ast::StmtKind::DelimitedExpression { .. } => {
                self.build_expression_statement(ctx, statement)
            }
            ast::StmtKind::LocalDefinition { .. } => {
                self.build_local_definition_statement(ctx, statement)
            }
        }
    }

    fn build_expression_statement(
        &mut self,
        ctx: &mut LocalContext,
        stmt: &ast::Statement,
    ) -> Result<Expression, ()> {
        let ast_value = match &stmt.kind {
            ast::StmtKind::DelimitedExpression { value } => value,
            _ => unreachable!(),
        };

        let value = self.build_expression(ctx, ast_value, None)?;
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
            _ => {
                self.diagnostics.push(
                    UnusedValueDiagnostic {
                        file_id: self.ast.file_id,
                        span: ast_value.span,
                    }
                    .report(),
                );

                let placeholder = Expression {
                    kind: ExprKind::Placeholder,
                    ty: value.ty,
                    span: TextSpan::new(value.span.start().0, value.span.start().0),
                };

                let span = value.span;
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator: ast::BinaryOp::Assign,
                        left: Box::new(placeholder),
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
        ctx: &mut LocalContext,
        stmt: &ast::Statement,
    ) -> Result<Expression, ()> {
        let (maybe_mutable, name, ty, value) = match &stmt.kind {
            ast::StmtKind::LocalDefinition {
                mutable,
                name,
                ty,
                value,
            } => (mutable.clone(), name.clone(), ty, value),
            _ => unreachable!(),
        };

        let expected_type = match ty {
            Some(ty) => Some(self.resolve_type(ty).unwrap()),
            None => None,
        };
        let mut value = self.build_expression(ctx, value, expected_type)?;

        let ty = match (value.ty, expected_type) {
            (Some(ty), None) => ty,
            (Some(actual_type), Some(expected_type)) => {
                if actual_type == expected_type {
                    actual_type
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
            name,
            ty,
            mutability: match maybe_mutable {
                Some(_) => Mutability::Mutable,
                None => Mutability::Const,
            },
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
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
                self.build_identifier_expression(ctx, expr, expected_type)
            }
            ExprKind::Binary { .. } => self.build_binary_expression(ctx, expr, expected_type),
            ExprKind::Grouping { value } => self.build_expression(ctx, value, expected_type),
            ExprKind::Unary { .. } => self.build_unary_expression(ctx, expr, expected_type),
            ExprKind::Call { .. } => self.build_call_expression(ctx, expr),
            ExprKind::Namespace { .. } => self.build_namespace_expression(expr),
            ExprKind::Return { .. } => self.build_return_expression(ctx, expr),
            ExprKind::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: None,
                    kind: BlockKind::Block,
                    parent: Some(ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
                    expected_type,
                },
                |ctx| self.build_block_expression(ctx, expr),
            ),
            ExprKind::IfElse { .. } => {
                self.build_if_else_expression(ctx, expr, None, expected_type)
            }
            ExprKind::Loop { .. } => self.build_loop_expression(ctx, expr, None, expected_type),
            ExprKind::Cast { .. } => self.build_cast_expression(ctx, expr, expected_type),
            ExprKind::Break { .. } => self.build_break_expression(ctx, expr),
            ExprKind::Continue { .. } => self.build_continue_expression(ctx, expr),
            ExprKind::Unreachable => Ok(hir::Expression {
                kind: hir::ExprKind::Unreachable,
                ty: Some(Type::Never),
                span: expr.span,
            }),
            ExprKind::Label { .. } => self.build_label_expression(ctx, expr, expected_type),
        }
    }

    fn build_label_expression(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let (label, block) = match &expr.kind {
            ast::ExprKind::Label { label, block } => (label.clone(), block),
            _ => unreachable!("expected label expression"),
        };

        match block.kind {
            ast::ExprKind::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: Some(label.symbol),
                    kind: BlockKind::Block,
                    parent: Some(ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
                    expected_type,
                },
                |ctx| self.build_block_expression(ctx, block),
            ),
            ast::ExprKind::IfElse { .. } => {
                self.build_if_else_expression(ctx, block, Some(label), expected_type)
            }
            ast::ExprKind::Loop { .. } => {
                self.build_loop_expression(ctx, block, Some(label), expected_type)
            }
            _ => unreachable!(),
        }
    }

    fn build_identifier_expression(
        &mut self,
        ctx: &LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let symbol = match expr.kind {
            ast::ExprKind::Identifier { symbol } => symbol,
            _ => unreachable!(),
        };
        match ctx.resolve_local(symbol) {
            Some((scope_index, local_index)) => {
                let local = ctx.frame.get_local(scope_index, local_index).unwrap();

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
                    ty: expected_type,
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
                GlobalValue::Global { global_index } => Ok(Expression {
                    kind: ExprKind::Global { global_index },
                    ty: Some(self.global.globals.get(global_index.0 as usize).unwrap().ty),
                    span: expr.span,
                }),
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
                Err(())
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
        ctx: &mut LocalContext,
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        label: Option<ast::Identifier>,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let block = match &expr.kind {
            ast::ExprKind::Loop { block } => block,
            _ => unreachable!(),
        };

        ctx.enter_block(
            BlockScope {
                label: label.map(|l| l.symbol),
                kind: BlockKind::Loop,
                parent: Some(ctx.scope_index),
                locals: Vec::new(),
                inferred_type: None,
                expected_type,
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let (ast_value, ast_type) = match &expr.kind {
            ast::ExprKind::Cast { value, ty } => (value, ty),
            _ => unreachable!("expected cast expression"),
        };

        match self.resolve_type(ast_type) {
            Ok(cast_type) => {
                let mut value = self.build_expression(ctx, ast_value, Some(cast_type))?;
                match value.ty {
                    Some(ty) => if ty != cast_type {},
                    None => self.coerce_untyped_expr(&mut value, cast_type)?,
                };

                Ok(value)
            }
            Err(_) => self.build_expression(ctx, ast_value, expected_type),
        }
    }

    fn infer_block_type(
        &mut self,
        scope: &BlockScope,
        value: &hir::Expression,
        ast_value: &ast::Expression,
    ) -> Result<Type, ()> {
        match value.ty {
            Some(result_type) => match scope.inferred_type {
                Some(inferred) => match result_type.coercible_to(inferred) {
                    true => Ok(inferred),
                    false => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected: inferred,
                                actual: result_type,
                                span: ast_value.span,
                            }
                            .report(&self.global),
                        );
                        Ok(inferred)
                    }
                },
                None => match scope.expected_type {
                    Some(expected) if !result_type.coercible_to(expected) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected,
                                actual: result_type,
                                span: ast_value.span,
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
                            span: ast_value.span,
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
        ctx: &mut LocalContext,
        statements: &[ast::Statement],
    ) -> BlockState<Vec<hir::Expression>> {
        let mut expressions = Vec::with_capacity(statements.len());
        for stmt in statements.iter() {
            let stmt_expr = match self.build_statement(ctx, stmt) {
                Ok(expr) => expr,
                Err(_) => continue,
            };

            match stmt_expr.ty {
                Some(Type::Unit) => expressions.push(stmt_expr),
                Some(Type::Never) => {
                    expressions.push(stmt_expr);
                    // doesn't make sense to continue checking after that
                    return BlockState::Exhaustive(expressions);
                }
                _ => unreachable!(),
            }
        }

        BlockState::Incomplete(expressions)
    }

    fn build_block_expression(
        &mut self,
        ctx: &mut LocalContext,
        block: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (statements, maybe_ast_result) = match &block.kind {
            ast::ExprKind::Block { statements, result } => (statements, result),
            _ => panic!("expected block expression"),
        };

        let expressions = match self.build_block_statements(ctx, statements) {
            BlockState::Exhaustive(expressions) => {
                if maybe_ast_result.is_some() || expressions.len() < statements.len() {
                    self.diagnostics.push(
                        UnreachableCodeDiagnostic {
                            file_id: self.ast.file_id,
                            span: TextSpan::merge(
                                match statements.get(expressions.len()) {
                                    Some(stmt) => stmt.span,
                                    None => maybe_ast_result.as_deref().unwrap().span,
                                },
                                match maybe_ast_result {
                                    Some(ast_result) => ast_result.span,
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
                        expressions: expressions.into_boxed_slice(),
                        result: None,
                    },
                    ty: Some(inferred_type),
                    span: block.span,
                });
            }
            BlockState::Incomplete(expressions) => expressions,
        };

        let BlockScope {
            kind: block_kind, ..
        } = ctx.frame.scopes[ctx.scope_index.0 as usize];
        match block_kind {
            BlockKind::Loop => match maybe_ast_result {
                Some(ast_result) => {
                    let result = self.build_expression(ctx, ast_result, Some(Type::Unit))?;
                    let result = match result.ty {
                        Some(ty) if ty.coercible_to(Type::Unit) => Some(result),
                        _ => todo!("loop result must be coercible to unit"),
                    };

                    Ok(hir::Expression {
                        kind: ExprKind::Block {
                            scope_index: ctx.scope_index,
                            expressions: expressions.into_boxed_slice(),
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
                None => Ok(hir::Expression {
                    kind: ExprKind::Block {
                        scope_index: ctx.scope_index,
                        expressions: expressions.into_boxed_slice(),
                        result: None,
                    },
                    ty: Some(
                        ctx.frame.scopes[ctx.scope_index.0 as usize]
                            .inferred_type
                            .unwrap_or(Type::Never),
                    ),
                    span: block.span,
                }),
            },
            BlockKind::Block => {
                let result =
                    self.build_block_result_expression(ctx, maybe_ast_result.as_deref())?;

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
                        expressions: expressions.into_boxed_slice(),
                        result: result.map(Box::new),
                    },
                    ty: Some(inferred_type),
                    span: block.span,
                })
            }
        }
    }

    fn build_block_result_expression(
        &mut self,
        ctx: &mut LocalContext,
        ast_result: Option<&ast::Expression>,
    ) -> Result<Option<hir::Expression>, ()> {
        match ast_result {
            Some(ast_result) => {
                let mut result = self.build_expression(
                    ctx,
                    ast_result,
                    ctx.frame.scopes[ctx.scope_index.0 as usize].expected_type,
                )?;

                let scope = &mut ctx.frame.scopes[ctx.scope_index.0 as usize];
                let inferred_type = self.infer_block_type(scope, &result, ast_result)?;
                scope.inferred_type = Some(inferred_type);
                match result.ty {
                    None => {
                        self.coerce_untyped_expr(&mut result, inferred_type)?;
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let maybe_ast_value = match &expr.kind {
            ast::ExprKind::Return { value } => value,
            _ => unreachable!(),
        };

        match maybe_ast_value {
            Some(ast_value) => Ok(self
                .build_expression(
                    ctx,
                    ast_value,
                    ctx.frame.scopes.get(0).unwrap().expected_type,
                )
                .and_then(|mut value| {
                    let scope = ctx.frame.scopes.get_mut(0).unwrap();
                    let inferred_type = self.infer_block_type(scope, &value, ast_value)?;
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
                                    span: ast_value.span,
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (label, maybe_ast_value) = match &expr.kind {
            ast::ExprKind::Break { label, value } => (label.clone(), value),
            _ => unreachable!("expected break expression"),
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
                        kind: ExprKind::Unreachable,
                        ty: Some(Type::Never),
                        span: expr.span,
                    });
                }
            },
        };

        match maybe_ast_value {
            Some(ast_value) => Ok(self
                .build_expression(
                    ctx,
                    ast_value,
                    ctx.frame
                        .scopes
                        .get(scope_index.0 as usize)
                        .unwrap()
                        .expected_type,
                )
                .and_then(|mut value| {
                    let scope = ctx.frame.scopes.get_mut(scope_index.0 as usize).unwrap();
                    let inferred_type = self.infer_block_type(scope, &value, ast_value)?;
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        label: Option<ast::Identifier>,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let (ast_condition, ast_then_block, maybe_ast_else_block) = match &expr.kind {
            ast::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => (condition, then_block, else_block),
            _ => unreachable!("expected if expression"),
        };

        let condition = self.build_expression(ctx, ast_condition, Some(Type::Bool))?;

        let then_block = match ast_then_block.kind {
            ast::ExprKind::Block { .. } => ctx.enter_block(
                BlockScope {
                    label: label.map(|l| l.symbol),
                    kind: BlockKind::Block,
                    parent: Some(ctx.scope_index),
                    locals: Vec::new(),
                    inferred_type: None,
                    expected_type: match maybe_ast_else_block {
                        Some(_) => expected_type,
                        None => None,
                    },
                },
                |ctx| self.build_block_expression(ctx, ast_then_block),
            )?,
            _ => unreachable!(),
        };
        let (else_block, ty) = match maybe_ast_else_block {
            Some(ast_else_block) => {
                let else_block = match ast_else_block.kind {
                    ast::ExprKind::Block { .. } => ctx.enter_block(
                        BlockScope {
                            label: label.map(|l| l.symbol),
                            kind: BlockKind::Block,
                            parent: Some(ctx.scope_index),
                            locals: Vec::new(),
                            inferred_type: None,
                            expected_type,
                        },
                        |ctx| self.build_block_expression(ctx, ast_else_block),
                    )?,
                    _ => unreachable!("expected block expression"),
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (ast_callee, ast_arguments) = match &expr.kind {
            ast::ExprKind::Call { callee, arguments } => (callee, arguments),
            _ => unreachable!("expected call expression"),
        };

        let callee = self.build_expression(ctx, ast_callee, None)?;
        let func_index = match callee.ty {
            Some(Type::Function(func_index)) => func_index,
            _ => {
                self.diagnostics.push(
                    NonCallableIdentifierDiagnostic {
                        file_id: self.ast.file_id,
                        span: ast_callee.span,
                    }
                    .report(),
                );
                return Err(());
            }
        };

        // if func_index.0 == ctx.func_index.0 {}

        let arguments: Box<_> = ast_arguments
            .iter()
            .enumerate()
            .map(|(index, ast_argument)| {
                let func_type = self
                    .global
                    .function_types
                    .get(func_index.0 as usize)
                    .unwrap();
                let expected_type = func_type.params.get(index).copied().unwrap();

                let mut argument = self.build_expression(ctx, ast_argument, Some(expected_type))?;
                match argument.ty {
                    Some(ty) if !ty.coercible_to(expected_type) => {
                        self.diagnostics.push(
                            TypeMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                expected: expected_type,
                                actual: ty,
                                span: ast_argument.span,
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<Expression, ()> {
        let (operator, ast_operand) = match &expr.kind {
            ast::ExprKind::Unary { operator, operand } => (operator.clone(), operand),
            _ => unreachable!(),
        };
        let operand = self.build_expression(ctx, ast_operand, expected_type)?;

        match operator {
            ast::UnaryOp::InvertSign | ast::UnaryOp::BitNot => match operand.ty {
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
            ast::UnaryOp::Not => match operand.ty {
                Some(Type::Bool) => Ok(hir::Expression {
                    kind: ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty: Some(Type::Bool),
                    span: expr.span,
                }),
                _ => panic!("can't apply logical not to this type"),
            },
        }
    }

    fn build_binary_expression(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<Expression, ()> {
        let operator = match expr.kind {
            ast::ExprKind::Binary { operator, .. } => operator.clone(),
            _ => unreachable!(),
        };

        use ast::BinaryOp;
        match operator {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                self.build_arithmetic_expr(ctx, expr, expected_type)
            }
            BinaryOp::Assign => self.build_assignment_expr(ctx, expr),
            BinaryOp::AddAssign
            | BinaryOp::SubAssign
            | BinaryOp::MulAssign
            | BinaryOp::DivAssign
            | BinaryOp::RemAssign => self.build_arithmetic_assignment_expr(ctx, expr),
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => self.build_comparison_binary_expr(ctx, expr),
            BinaryOp::And | BinaryOp::Or => self.build_logical_binary_expr(ctx, expr),
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::LeftShift
            | BinaryOp::RightShift => self.build_bitwise_binary_expr(ctx, expr, expected_type),
        }
    }

    fn build_assignment_expr(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
    ) -> Result<Expression, ()> {
        let (ast_left, ast_right) = match &expr.kind {
            ast::ExprKind::Binary { left, right, .. } => (left, right),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, ast_left, None)?;
        match left.kind {
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let local = match ctx.frame.get_local(scope_index, local_index) {
                    Some(local) => local.clone(),
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
                    hir::Mutability::Const => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    hir::Mutability::Mutable => {}
                }

                let mut right = self.build_expression(ctx, ast_right, Some(local.ty))?;
                match right.ty {
                    Some(ty) if !ty.coercible_to(local.ty) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left_span: ast_left.span,
                                left_type: local.ty,
                                operator: ast::BinaryOp::Assign,
                                right_span: ast_right.span,
                                right_type: ty,
                            }
                            .report(&self.global),
                        );
                    }
                    Some(_) => {}
                    None => {
                        self.coerce_untyped_expr(&mut right, local.ty)?;
                    }
                }

                Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        left: Box::new(left),
                        operator: ast::BinaryOp::Assign,
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
                    .get(global_index.0 as usize)
                    .expect("global variable not found");

                match global.mutability {
                    hir::Mutability::Const => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    hir::Mutability::Mutable => {}
                }

                let global_type = global.ty;

                let mut right = self.build_expression(ctx, ast_right, Some(global_type))?;
                match right.ty {
                    Some(ty) if !ty.coercible_to(global_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left_span: ast_left.span,
                                left_type: global_type,
                                operator: ast::BinaryOp::Assign,
                                right_span: ast_right.span,
                                right_type: ty,
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
                        operator: ast::BinaryOp::Assign,
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                })
            }
            hir::ExprKind::Placeholder => {
                let right = self.build_expression(ctx, ast_right, None)?;
                let right_type = match right.ty {
                    Some(ty) => ty,
                    None => panic!("can't drop untyped value"),
                };

                return Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        operator: ast::BinaryOp::Assign,
                        left: Box::new(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: Some(right_type),
                            span: ast_left.span,
                        }),
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                });
            }
            _ => panic!("left side of assignment must be a variable"),
        }
    }

    fn build_arithmetic_assignment_expr(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
    ) -> Result<Expression, ()> {
        let (ast_left, ast_right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let left = self.build_expression(ctx, ast_left, None)?;
        match left.kind {
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let local = match ctx.frame.get_local(scope_index, local_index) {
                    Some(local) => local.clone(),
                    None => panic!("can't assign to undeclared variable"),
                };
                match local.ty {
                    Type::Primitive(_) => {}
                    to_type => {
                        self.diagnostics.push(
                            OperatorCannotBeAppliedDiagnostic {
                                file_id: self.ast.file_id,
                                operator,
                                to_type,
                                span: expr.span,
                            }
                            .report(&self.global),
                        );

                        return Err(());
                    }
                }

                match local.mutability {
                    hir::Mutability::Const => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    hir::Mutability::Mutable => {}
                }

                let mut right = self.build_expression(ctx, ast_right, Some(local.ty))?;
                match right.ty {
                    Some(ty) if !ty.coercible_to(local.ty) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left_span: ast_left.span,
                                left_type: local.ty,
                                operator,
                                right_span: ast_right.span,
                                right_type: ty,
                            }
                            .report(&self.global),
                        );
                    }
                    Some(_) => {}
                    None => {
                        self.coerce_untyped_expr(&mut right, local.ty)?;
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
                    .get(global_index.0 as usize)
                    .expect("global variable not found");

                match global.ty {
                    Type::Primitive(_) => {}
                    to_type => {
                        self.diagnostics.push(
                            OperatorCannotBeAppliedDiagnostic {
                                file_id: self.ast.file_id,
                                operator,
                                to_type,
                                span: expr.span,
                            }
                            .report(&self.global),
                        );

                        return Err(());
                    }
                }

                match global.mutability {
                    hir::Mutability::Const => {
                        self.diagnostics.push(
                            CannotMutateImmutableDiagnostic {
                                file_id: self.ast.file_id,
                                span: expr.span,
                            }
                            .report(),
                        );
                    }
                    hir::Mutability::Mutable => {}
                }

                let global_type = global.ty;

                let mut right = self.build_expression(ctx, ast_right, Some(global_type))?;
                match right.ty {
                    Some(ty) if !ty.coercible_to(global_type) => {
                        self.diagnostics.push(
                            BinaryExpressionMistmatchDiagnostic {
                                file_id: self.ast.file_id,
                                left_span: ast_left.span,
                                left_type: global_type,
                                operator: ast::BinaryOp::Assign,
                                right_span: ast_right.span,
                                right_type: ty,
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
                        operator: ast::BinaryOp::Assign,
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                    span: expr.span,
                })
            }
            _ => panic!("left side of assignment must be a variable"),
        }
    }

    fn build_bitwise_binary_expr(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let (ast_left, ast_right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let mut left = self.build_expression(ctx, ast_left, expected_type)?;
        let mut right = self.build_expression(ctx, ast_right, expected_type)?;

        match (left.ty, right.ty) {
            (
                Some(Type::Primitive(PrimitiveType::I32)),
                Some(Type::Primitive(PrimitiveType::I32)),
            )
            | (
                Some(Type::Primitive(PrimitiveType::I64)),
                Some(Type::Primitive(PrimitiveType::I64)),
            ) => {
                let ty = left.ty;
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty,
                    span: expr.span,
                })
            }
            (Some(ty), None) => {
                match ty {
                    Type::Primitive(_) => {}
                    _ => panic!("bitwise operator can only be applied to primitive types"),
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
                    Type::Primitive(_) => {}
                    _ => panic!("bitwise operator can only be applied to primitive types"),
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
            (None, None) => {
                self.diagnostics.push(
                    TypeAnnotationRequiredDiagnostic {
                        file_id: self.ast.file_id,
                        span: expr.span,
                    }
                    .report(),
                );

                Err(())
            }
            (Some(left_type), Some(right_type)) => {
                self.diagnostics.push(
                    BinaryExpressionMistmatchDiagnostic {
                        file_id: self.ast.file_id,
                        left_span: ast_left.span,
                        left_type,
                        operator,
                        right_span: ast_right.span,
                        right_type,
                    }
                    .report(&self.global),
                );

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: expected_type,
                    span: expr.span,
                })
            }
        }
    }

    fn build_arithmetic_expr(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<Expression, ()> {
        let (left, right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!("expected binary expression"),
        };

        let mut left = self.build_expression(ctx, left, expected_type)?;
        let mut right = self.build_expression(ctx, right, expected_type)?;

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
                    _ => panic!("arithmetic operator can only be applied to primitive types"),
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
            (None, None) => match expected_type {
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
                        left_span: left.span,
                        left_type,
                        operator,
                        right_span: right.span,
                        right_type,
                    }
                    .report(&self.global),
                );

                match expected_type {
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (ast_left, ast_right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let mut left = self.build_expression(ctx, ast_left, None)?;
        let mut right = self.build_expression(ctx, ast_right, None)?;

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
                        left: ast_left.span,
                        right: ast_right.span,
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
                        left_span: ast_left.span,
                        left_type,
                        right_span: ast_right.span,
                        right_type,
                        operator,
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (ast_left, ast_right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, ast_left, Some(Type::Bool))?;
        let right = self.build_expression(ctx, ast_right, Some(Type::Bool))?;

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
            Type::Primitive(PrimitiveType::I64) => match expr.kind {
                hir::ExprKind::Int(value) => {
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
                _ => unreachable!(),
            },
            Type::Primitive(PrimitiveType::F32) => match expr.kind {
                hir::ExprKind::Float(value) => {
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
                hir::ExprKind::Int(value) => {
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

                    expr.kind = hir::ExprKind::Float(value as f64);
                    expr.ty = Some(Type::Primitive(PrimitiveType::F32));

                    Ok(())
                }
                _ => unreachable!(),
            },
            Type::Primitive(PrimitiveType::F64) => match expr.kind {
                hir::ExprKind::Float(value) => {
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
                hir::ExprKind::Int(value) => {
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

                    expr.kind = hir::ExprKind::Float(value as f64);
                    expr.ty = Some(Type::Primitive(PrimitiveType::F64));

                    Ok(())
                }
                _ => unreachable!(),
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

        match operator {
            ast::UnaryOp::BitNot | ast::UnaryOp::InvertSign => match target_type {
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

        match operator {
            operator if operator.is_arithmetic() || operator.is_bitwise() => match target_type {
                Type::Primitive(PrimitiveType::I32 | PrimitiveType::I64) => {}
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

#[cfg(test)]
mod tests {
    use string_interner::StringInterner;

    use super::*;
    use crate::files::Files;

    #[test]
    fn should_coerce_never_expression() {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add(
                "test.wx".to_string(),
                "func test(): unit { unreachable }".to_string(),
            )
            .unwrap();

        let (ast, diagnostics) =
            ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        assert_eq!(diagnostics.len(), 0);
        assert_eq!(ast.items.len(), 1);

        let (hir, diagnostics) = hir::Builder::build(&ast, &mut interner);

        assert_eq!(diagnostics.len(), 0);
        assert_eq!(hir.functions.len(), 1);
        assert_eq!(
            hir.functions[0].stack.scopes[0].expected_type,
            Some(hir::Type::Unit)
        );
        assert_eq!(
            hir.functions[0].stack.scopes[0].inferred_type,
            Some(hir::Type::Never)
        );
        assert_eq!(hir.functions[0].block.ty, Some(hir::Type::Never));
    }

    #[test]
    fn should_infer_local_type() {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add(
                "test.wx".to_string(),
                "func test(a: i32): i32 { local x = a; x }".to_string(),
            )
            .unwrap();

        let (ast, diagnostics) =
            ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

        assert_eq!(diagnostics.len(), 0);
        assert_eq!(ast.items.len(), 1);

        let (hir, diagnostics) = hir::Builder::build(&ast, &mut interner);

        assert_eq!(diagnostics.len(), 0);
        assert_eq!(hir.functions.len(), 1);
        assert_eq!(
            hir.functions[0].stack.scopes[0].locals[0].ty,
            hir::Type::Primitive(PrimitiveType::I32)
        );
        assert_eq!(
            hir.functions[0].stack.scopes[0].inferred_type,
            Some(hir::Type::Primitive(PrimitiveType::I32))
        );
    }
}
