use core::panic;
use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::diagnostics::DiagnosticContext;
use crate::{ast, hir};

pub struct Builder<'ast, 'interner> {
    ast: &'ast ast::Ast,
    global: hir::GlobalContext<'interner>,
    diagnostics: Vec<hir::diagnostics::DiagnosticContext>,
    hir: hir::HIR,
}

impl<'ast, 'interner> Builder<'ast, 'interner> {
    pub fn build(
        ast: &'ast ast::Ast,
        interner: &'interner StringInterner<StringBackend<SymbolU32>>,
    ) -> (hir::HIR, Vec<DiagnosticContext>) {
        let mut builder = Builder {
            ast,
            global: hir::GlobalContext::new(interner),
            diagnostics: Vec::new(),
            hir: hir::HIR::new(),
        };

        for (index, item) in builder.ast.items.iter().enumerate() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition(def) => {
                    let func_type = builder.build_function_type(&def.signature).unwrap();

                    builder
                        .global
                        .add_function(def.signature.name.symbol, func_type);
                }
                ast::ItemKind::Enum(item_enum) => {
                    let enum_ = builder.build_enum(ast::ItemId(index as u32), item_enum);
                    builder.global.push_enum(enum_);
                }
                _ => unimplemented!("item kind not implemented"),
            }
        }

        let mut functions = Vec::new();

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition(def) => {
                    match builder.build_function_definition(def) {
                        Ok(func) => functions.push(func),
                        Err(_) => continue,
                    }
                }
                ast::ItemKind::Enum(_) => {}
                _ => unimplemented!("item kind not implemented"),
            }
        }

        (hir, builder.diagnostics)
    }

    #[inline]
    fn push_expr(&mut self, expr: hir::Expression) -> hir::ExprIndex {
        let index = hir::ExprIndex(self.hir.expressions.len() as u32);
        self.hir.expressions.push(expr);
        index
    }

    fn build_function_type(
        &mut self,
        signature: &ast::FunctionSignature,
    ) -> Result<hir::FunctionType, ()> {
        let params = signature
            .params
            .iter()
            .map(|param| {
                self.resolve_type(param.ty.clone())
                    .unwrap_or(hir::Type::Unknown)
            })
            .collect();

        let result = match &signature.result {
            Some(ty) => self.resolve_type(ty.clone()).unwrap_or(hir::Type::Unknown),
            None => hir::Type::Unit,
        };

        Ok(hir::FunctionType { params, result })
    }

    fn build_enum(&mut self, item_id: ast::ItemId, def: &ast::ItemEnum) -> hir::Enum {
        let ty = match self.resolve_type(def.ty.clone()) {
            Some(hir::Type::Primitive(ty)) => ty,
            None => hir::PrimitiveType::I32,
            ty => {
                self.diagnostics
                    .push(DiagnosticContext::InvalidEnumRepresentation {
                        file_id: self.ast.file_id,
                        type_: ty.unwrap(),
                        item_id,
                        span: def.ty.span,
                    });

                hir::PrimitiveType::I32
            }
        };

        let variants: Vec<hir::EnumVariant> = def
            .variants
            .iter()
            .enumerate()
            .map(|(variant_index, variant)| {
                let value_expr = self
                    .ast
                    .get_expr(variant.value)
                    .expect("invalid expression");
                let value = match &value_expr.kind {
                    ast::ExprKind::Int { value } => value.clone(),
                    _ => {
                        let diagnostic = DiagnosticContext::InvalidEnumValue {
                            file_id: self.ast.file_id,
                            item_id,
                            variant_index,
                        };
                        self.diagnostics.push(diagnostic);

                        0 // fallback value
                    }
                };

                hir::EnumVariant {
                    name: variant.name.symbol,
                    value,
                }
            })
            .collect();

        let variants_lookup: HashMap<SymbolU32, hir::EnumVariantIndex> = variants
            .iter()
            .enumerate()
            .map(|(index, variant)| (variant.name, hir::EnumVariantIndex(index as u32)))
            .collect();

        hir::Enum {
            name: def.name.symbol,
            ty,
            variants: variants.into_boxed_slice(),
            lookup: variants_lookup,
        }
    }

    fn build_function_definition(
        &mut self,
        def: &ast::ItemFunctionDefinition,
    ) -> Result<hir::Function, ()> {
        use hir::*;
        let global_value = self
            .global
            .value_lookup
            .get(&def.signature.name.symbol)
            .cloned()
            .unwrap();
        let func_index = match global_value {
            GlobalValue::Function(index) => index,
            _ => panic!("expected function index"),
        };
        let func_type = self.global.get_function(func_index).unwrap().clone();

        let locals: Vec<Local> = def
            .signature
            .params
            .iter()
            .map(|param| Local {
                name: param.name.symbol,
                ty: self.resolve_type(param.ty.clone()).unwrap_or(Type::Unknown),
                mutability: Mutability::Mutable,
            })
            .collect();

        let local_lookup: HashMap<LookupKey, LookupValue> = locals
            .iter()
            .enumerate()
            .map(|(index, param)| {
                (
                    (LookupType::Local, ScopeIndex(0), param.name),
                    LookupValue::Local(LocalIndex::from(index)),
                )
            })
            .collect();

        let mut ctx = FunctionContext {
            func_index,
            frame: StackFrame {
                scopes: vec![BlockScope {
                    parent_scope: None,
                    locals,
                    result: Some(func_type.result.clone()),
                }],
            },
            scope_index: ScopeIndex(0),
            lookup: local_lookup,
        };

        let block = self.build_block_expression(&mut ctx, def.block)?;

        Ok(hir::Function {
            ty: func_type,
            name: def.signature.name.symbol,
            export: def.export.is_some(),
            stack: ctx.frame,
            block,
        })
    }

    fn build_statement(
        &mut self,
        ctx: &mut hir::FunctionContext,
        stmt_id: ast::StmtId,
    ) -> Result<hir::Expression, ()> {
        let stmt = self.ast.get_stmt(stmt_id).expect("invalid statement id");
        match &stmt.kind {
            ast::StmtKind::DelimitedExpression { value } => {
                self.build_expression_statement(ctx, stmt_id, *value)
            }
            ast::StmtKind::ConstDefinition { .. } => self.build_declaration_statement(ctx, stmt_id),
            ast::StmtKind::MutableDefinition { .. } => {
                self.build_declaration_statement(ctx, stmt_id)
            }
        }
    }

    fn build_expression_statement(
        &mut self,
        ctx: &mut hir::FunctionContext,
        stmt_id: ast::StmtId,
        value: ast::ExprId,
    ) -> Result<hir::ExprIndex, ()> {
        let expr_index = self.build_expression(ctx, value)?;
        let expr = self.expressions[expr_index.0 as usize].clone();
        match expr.ty {
            Some(hir::Type::Unit) => Ok(expr_index),
            Some(hir::Type::Never) => {
                let scope = ctx
                    .frame
                    .scopes
                    .get_mut(ctx.scope_index.0 as usize)
                    .unwrap();
                match scope.result {
                    Some(_) => Ok(expr),
                    None => {
                        scope.result = Some(hir::Type::Never);
                        Ok(expr)
                    }
                }
            }
            _ => {
                let ast_expr = self.ast.get_expr(expr_id).unwrap();
                self.diagnostics
                    .push(DiagnosticContext::UnusedExpressionValue {
                        file_id: self.ast.file_id,
                        span: ast_expr.span,
                    });

                Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        operator: ast::BinaryOp::Assign,
                        lhs: Box::new(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: expr.ty.clone(),
                        }),
                        rhs: Box::new(expr),
                    },
                    ty: Some(hir::Type::Unit),
                })
            }
        }
    }

    fn build_declaration_statement(
        &mut self,
        ctx: &mut hir::FunctionContext,
        stmt_id: ast::StmtId,
    ) -> Result<hir::Expression, ()> {
        let (name, ty, value, mutability) = match &stmt.kind {
            ast::StmtKind::ConstDefinition { name, ty, value } => (
                name.clone(),
                ty.clone(),
                value.clone(),
                hir::Mutability::Const,
            ),
            ast::StmtKind::MutableDefinition { name, ty, value } => (
                name.clone(),
                ty.clone(),
                value.clone(),
                hir::Mutability::Mutable,
            ),
            _ => unreachable!("expected declaration statement"),
        };

        let value_expr = self.build_expression(ctx, value)?;
        let binding_type = match ty {
            Some(ty) => match self.resolve_type(ty) {
                Some(ty) => ty,
                None => match value_expr.ty {
                    Some(ty) => ty,
                    None => hir::Type::Unknown,
                },
            },
            None => match value_expr.ty {
                Some(ty) => ty,
                None => {
                    self.diagnostics
                        .push(DiagnosticContext::TypeAnnotationRequired {
                            file_id: self.ast.file_id,
                            span: name.span,
                        });
                    return Err(());
                }
            },
        };
        let coerced_rhs = self
            .coerce_expr(value_expr, value, binding_type)
            .expect("can't assign to this type");

        let local_index = ctx.push_local(hir::Local {
            name: name.symbol,
            ty: binding_type,
            mutability,
        });

        Ok(hir::Expression {
            kind: hir::ExprKind::LocalDeclaration {
                scope_index: ctx.scope_index,
                local_index,
                expr: Box::new(coerced_rhs),
            },
            ty: Some(binding_type),
        })
    }

    fn build_identifier_expression(
        &mut self,
        ctx: &hir::FunctionContext,
        expr_id: ast::ExprId,
        symbol: SymbolU32,
    ) -> Result<hir::ExprIndex, ()> {
        match ctx.resolve_local(symbol) {
            Some((scope_index, local_index)) => {
                let local = ctx.frame.get_local(scope_index, local_index).unwrap();
                return Ok(self.push_expr(hir::Expression {
                    kind: hir::ExprKind::Local {
                        local_index,
                        scope_index,
                    },
                    ty: Some(local.ty),
                }));
            }
            None => {}
        }
        match self.global.resolve_value(symbol) {
            Some(expr) => return Ok(self.push_expr(expr)),
            None => {}
        };

        self.diagnostics
            .push(DiagnosticContext::UndeclaredIdentifier {
                file_id: self.ast.file_id,
                span: self.ast.get_expr(expr_id).unwrap().span,
            });
        Err(())
    }

    fn build_namespace_member_expression(
        &mut self,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let (namespace, member) = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::NamespaceMember { namespace, member } => (namespace, member),
            _ => unreachable!("expected namespace member expression"),
        };

        let ty = match self.global.resolve_type(namespace.symbol) {
            Some(ty) => ty,
            None => panic!("undeclared namespace"),
        };

        match ty {
            hir::Type::Enum(enum_index) => {
                let enum_ = self.hir.get_enum(enum_index).unwrap();
                let variant_index = match enum_.resolve_variant(member.symbol) {
                    Some(index) => index,
                    None => {
                        self.diagnostics
                            .push(DiagnosticContext::UnknownEnumVariant {
                                file_id: self.ast.file_id,
                                enum_index,
                                span: member.span,
                            });

                        return Ok(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: Some(hir::Type::Enum(enum_index)),
                        });
                    }
                };

                Ok(hir::Expression {
                    kind: hir::ExprKind::EnumVariant {
                        enum_index,
                        variant_index,
                    },
                    ty: Some(hir::Type::Enum(enum_index)),
                })
            }
            _ => panic!("unknown namespace type"),
        }
    }

    fn build_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::ExprIndex, ()> {
        let expr = self.ast.get_expr(expr_id).unwrap();
        use ast::ExprKind;
        match &expr.kind {
            ExprKind::Int { value } => Ok(self.push_expr(hir::Expression {
                kind: hir::ExprKind::Int(value.clone()),
                ty: None,
            })),
            ExprKind::Identifier { symbol } => {
                self.build_identifier_expression(ctx, expr_id, *symbol)
            }
            ExprKind::Binary { .. } => self.build_binary_expression(ctx, expr_id),
            ExprKind::Unary { operator, operand } => Ok(Builder::build_unary_expr(
                *operator,
                self.build_expression(ctx, *operand)?,
            )),
            ExprKind::Call { .. } => self.build_call_expression(ctx, expr_id),
            ExprKind::NamespaceMember { .. } => self.build_namespace_member_expression(expr_id),
            ExprKind::Return { value } => self.build_return_expression(ctx, *value),
            ExprKind::Grouping { value } => self.build_expression(ctx, *value),
            ExprKind::Block { .. } => {
                ctx.enter_scope(|ctx| self.build_block_expression(ctx, expr_id))
            }
            ExprKind::IfElse { .. } => self.build_if_else_expression(ctx, expr_id),
            ExprKind::Cast { .. } => self.build_cast_expression(ctx, expr_id),
            ExprKind::Break { .. } => self.build_break_expression(ctx, expr_id),
            ExprKind::Label { label, block } => ctx.enter_scope_with_label(label.symbol, |ctx| {
                self.build_block_expression(ctx, *block)
            }),
        }
    }

    fn build_break_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let (label, value) = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Break { label, value } => (label.clone(), value.clone()),
            _ => unreachable!("expected break expression"),
        };

        let scope_index = match label {
            Some(label) => ctx.resolve_label(label.symbol).expect("invalid label"),
            None => ctx.scope_index,
        };

        let value_expr = match value {
            Some(value_id) => {
                let expr = self.build_expression(ctx, value_id)?;
                match expr.ty {
                    Some(value_type) => {
                        let scope = ctx.frame.scopes.get_mut(scope_index.0 as usize).unwrap();
                        match scope.result {
                            Some(ty) if ty == value_type => {}
                            Some(_) => panic!("type mistmatch in bream expression"),
                            None => scope.result = Some(value_type),
                        }
                    }
                    None => panic!("break expression value must have a type"),
                }

                Some(expr)
            }
            None => {
                let scope = ctx.frame.scopes.get_mut(scope_index.0 as usize).unwrap();
                match scope.result {
                    Some(hir::Type::Unit) => None,
                    Some(ty) => panic!("can't break without a value, expected {ty}"),
                    None => {
                        scope.result = Some(hir::Type::Unit);
                        None
                    }
                }
            }
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::Break {
                scope_index,
                value: value_expr.map(Box::new),
            },
            ty: Some(hir::Type::Never),
        })
    }

    fn build_cast_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let (value_id, ty) = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Cast { value, ty } => (value, ty),
            _ => unreachable!("expected cast expression"),
        };

        let value = self.build_expression(ctx, *value_id)?;
        match value.ty {
            Some(_) => {
                panic!("can't coerce already typed expression");
            }
            None => {}
        }
        let ty = match self.resolve_type(ty.clone()) {
            Some(ty) => ty,
            None => panic!("invalid type in cast expression"),
        };
        let coerced_value = self.coerce_expr(value, *value_id, ty)?;

        Ok(coerced_value)
    }

    fn build_if_else_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let (condition_id, then_block_id, else_block_id) =
            match &self.ast.get_expr(expr_id).unwrap().kind {
                ast::ExprKind::IfElse {
                    condition,
                    then_block,
                    else_block,
                } => (condition, then_block, else_block),
                _ => unreachable!("expected if expression"),
            };

        let condition = self.build_expression(ctx, *condition_id)?;
        let coerced_condition = self.coerce_expr(condition, *condition_id, hir::Type::Bool)?;

        let mut then_block = match self.ast.get_expr(*then_block_id).unwrap().kind {
            ast::ExprKind::Block { .. } => {
                ctx.enter_scope(|ctx| self.build_block_expression(ctx, *then_block_id))?
            }
            _ => unreachable!("expected block expression"),
        };
        let (else_block, ty) = match else_block_id {
            Some(else_block_id) => {
                let else_block = match self.ast.get_expr(*else_block_id).unwrap().kind {
                    ast::ExprKind::Block { .. } => {
                        ctx.enter_scope(|ctx| self.build_block_expression(ctx, *else_block_id))?
                    }
                    _ => unreachable!("expected block expression"),
                };

                match (then_block.ty, else_block.ty) {
                    (Some(ty1), Some(ty2)) if ty1 == ty2 => (Some(else_block), Some(ty1)),
                    (Some(ty1), Some(ty2)) => {
                        todo!("handle type mistmatch in if-else {} {}", ty1, ty2)
                    }
                    (None, None) => (Some(else_block), None),
                    (Some(ty), None) => {
                        let coerced = self
                            .coerce_expr(else_block, *else_block_id, ty)
                            .expect("the type of else block can't be coerced to then block type");

                        (Some(coerced), Some(ty))
                    }
                    (None, Some(ty)) => {
                        then_block = self
                            .coerce_expr(then_block, *then_block_id, ty)
                            .expect("the type of then block can't be coerced to else block type");

                        (Some(else_block), Some(ty))
                    }
                }
            }
            None => match then_block.ty {
                Some(hir::Type::Unit | hir::Type::Never) => (None, Some(hir::Type::Unit)),
                _ => panic!(
                    "if you want to return a value from if-else, you must provide an else block"
                ),
            },
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::IfElse {
                condition: Box::new(coerced_condition),
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new),
            },
            ty,
        })
    }

    fn build_block_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        block_id: ast::ExprId,
    ) -> Result<hir::ExprIndex, ()> {
        let block = self
            .ast
            .get_expr(block_id)
            .expect("invalid block expression");

        let (statements, result) = match &block.kind {
            ast::ExprKind::Block { statements, result } => (statements, result),
            _ => panic!("expected block expression"),
        };

        let expressions: Vec<hir::ExprIndex> = Vec::with_capacity(statements.len());
        for stmt_index in statements.iter() {
            let stmt = self.ast.get_stmt(*stmt_index).unwrap();
            let expr = self.build_statement(ctx, stmt)?;
            self.expressions.push(expr);
        }

        let (result, result_type) = match *result {
            Some(result) => {
                let expr = self.build_expression(ctx, result)?;
                let scope = ctx
                    .frame
                    .scopes
                    .get_mut(ctx.scope_index.0 as usize)
                    .unwrap();

                match (expr.ty, scope.result) {
                    (Some(expr_type), Some(result_type)) => {
                        if expr_type != result_type {
                            panic!(
                                "type mistmatch in block expression result {expr_type} != {result_type}"
                            );
                        }
                        (Some(expr), Some(result_type))
                    }
                    (Some(expr_type), None) => {
                        scope.result = Some(expr_type.clone());
                        (Some(expr), Some(expr_type))
                    }
                    (None, Some(result_type)) => {
                        let coerced = self.coerce_expr(expr, result, result_type)?;
                        scope.result = Some(result_type.clone());
                        (Some(coerced), Some(result_type))
                    }
                    (None, None) => {
                        // this means that we didn't have any break expressions,
                        // to get the type from
                        // and our result expression is untyped
                        (Some(expr), None)
                    }
                }
            }
            None => {
                let scope = ctx
                    .frame
                    .scopes
                    .get_mut(ctx.scope_index.0 as usize)
                    .unwrap();
                scope.result = Some(scope.result.unwrap_or(hir::Type::Unit));

                (None, scope.result)
            }
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::Block {
                scope_index: ctx.scope_index,
                expressions: expressions.into_boxed_slice(),
                result: result.map(Box::new),
            },
            ty: result_type,
        })
    }

    fn build_return_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        value: Option<ast::ExprId>,
    ) -> Result<hir::Expression, ()> {
        let result = ctx
            .frame
            .scopes
            .get_mut(0)
            .unwrap()
            .result
            .expect("function must have a result type");

        let expr = match value {
            Some(expr_id) => {
                let expr = self.build_expression(ctx, expr_id)?;
                self.coerce_expr(expr, expr_id, result)?
            }
            None => {
                match result {
                    hir::Type::Unit => {}
                    _ => panic!("return type mistmatch, can't return unit"),
                }
                return Ok(hir::Expression {
                    kind: hir::ExprKind::Return { value: None },
                    ty: Some(hir::Type::Never),
                });
            }
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::Return {
                value: Some(Box::new(expr)),
            },
            ty: Some(hir::Type::Never),
        })
    }

    fn build_call_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let (callee_id, arguments) = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Call { callee, arguments } => (callee, arguments),
            _ => unreachable!("expected call expression"),
        };

        let callee = self.build_expression(ctx, *callee_id)?;
        let func_index = match callee.kind {
            hir::ExprKind::Function(func_index) => func_index,
            _ => {
                let callee_ast_expr = self.ast.get_expr(*callee_id).unwrap();
                self.diagnostics
                    .push(DiagnosticContext::NonCallableIdentifier {
                        file_id: self.ast.file_id,
                        span: callee_ast_expr.span,
                    });
                return Err(());
            }
        };

        let func_type = match self.global.get_function(func_index).cloned() {
            Some(func_type) => func_type,
            None => panic!("invalid function index"),
        };
        let arguments: Vec<_> = arguments
            .iter()
            .enumerate()
            .map(|(index, arg)| {
                let expr = self.build_expression(ctx, *arg)?;

                self.coerce_expr(expr, *arg, func_type.params.get(index).copied().unwrap())
            })
            .collect::<Result<_, _>>()?;

        Ok(hir::Expression {
            kind: hir::ExprKind::Call {
                callee: Box::new(callee),
                arguments,
            },
            ty: Some(func_type.result),
        })
    }

    fn build_unary_expr(operator: ast::UnaryOp, operand: hir::Expression) -> hir::Expression {
        match operand.ty {
            Some(hir::Type::Primitive(ty)) => hir::Expression {
                kind: hir::ExprKind::Unary {
                    operator,
                    operand: Box::new(operand),
                },
                ty: Some(hir::Type::Primitive(ty)),
            },
            None => hir::Expression {
                kind: hir::ExprKind::Unary {
                    operator,
                    operand: Box::new(operand),
                },
                ty: None,
            },
            _ => panic!("can't apply unary operator to this type"),
        }
    }

    fn coerce_expr(
        &mut self,
        rhs: hir::Expression,
        rhs_id: ast::ExprId,
        expected_type: hir::Type,
    ) -> Result<hir::Expression, ()> {
        match (expected_type, rhs.ty) {
            (ty, None) => self.coerce_untyped_expr(rhs, rhs_id, ty),
            (hir::Type::Primitive(ty), Some(hir::Type::Primitive(ty2))) if ty == ty2 => {
                Ok(rhs.clone())
            }
            (hir::Type::Bool, Some(hir::Type::Bool)) => Ok(rhs.clone()),
            (hir::Type::Enum(index_1), Some(hir::Type::Enum(index_2))) if index_1 == index_2 => {
                Ok(rhs.clone())
            }
            (expected_type, rhs_type) => {
                let ast_expr = self.ast.get_expr(rhs_id).unwrap();
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected: expected_type,
                    actual: rhs_type,
                    span: ast_expr.span,
                });
                Err(())
            }
        }
    }

    fn coerce_untyped_expr(
        &mut self,
        expr: hir::Expression,
        expr_id: ast::ExprId,
        target_type: hir::Type,
    ) -> Result<hir::Expression, ()> {
        match (expr.kind, target_type) {
            (hir::ExprKind::Int(value), ty) => self.coerce_untyped_int_expr(value, expr_id, ty),
            (hir::ExprKind::Unary { operator, operand }, ty) => {
                let ast_expr = self.ast.get_expr(expr_id).unwrap();
                let operand_id = match &ast_expr.kind {
                    ast::ExprKind::Unary { operand, .. } => *operand,
                    _ => unreachable!("expected unary expression"),
                };

                let coerced_operand = self.coerce_untyped_expr(*operand, operand_id, ty.clone())?;

                Ok(hir::Expression {
                    kind: hir::ExprKind::Unary {
                        operator,
                        operand: Box::new(coerced_operand),
                    },
                    ty: Some(ty),
                })
            }
            (hir::ExprKind::Binary { lhs, operator, rhs }, ty) => {
                let ast_expr = self.ast.get_expr(expr_id).unwrap();
                let (left_id, right_id) = match &ast_expr.kind {
                    ast::ExprKind::Binary(expr) => (expr.left.clone(), expr.right.clone()),
                    _ => unreachable!("expected binary expression"),
                };

                let ty = match operator {
                    operator if operator.is_arithmetic() || operator.is_bitwise() => match ty {
                        hir::Type::Primitive(_) => ty,
                        ty => {
                            self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                                file_id: self.ast.file_id,
                                expected: ty,
                                actual: None,
                                span: ast_expr.span,
                            });
                            return Err(());
                        }
                    },
                    operator if operator.is_comparison() || operator.is_logical() => {
                        panic!("type annotation required")
                    }
                    _ => unreachable!("assignment should always have a known type"),
                };

                let lhs = self.coerce_untyped_expr(*lhs, left_id, ty.clone())?;
                let rhs = self.coerce_untyped_expr(*rhs, right_id, ty.clone())?;

                Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        operator,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    ty: Some(ty),
                })
            }
            (
                hir::ExprKind::Block {
                    expressions,
                    result,
                    scope_index,
                },
                ty,
            ) => self.coerce_untyped_block_expr(expressions, result, scope_index, expr_id, ty),
            (
                hir::ExprKind::IfElse {
                    condition,
                    then_block,
                    else_block,
                },
                ty,
            ) => {
                let (then_block_id, else_block_id) = match &self.ast.get_expr(expr_id).unwrap().kind
                {
                    ast::ExprKind::IfElse {
                        then_block,
                        else_block,
                        ..
                    } => (*then_block, *else_block),
                    _ => unreachable!("expected if expression"),
                };
                let coerced_then_block =
                    self.coerce_untyped_expr(*then_block, then_block_id, ty.clone())?;
                let coerced_else_block = match else_block {
                    Some(else_block) => {
                        Some(self.coerce_untyped_expr(*else_block, else_block_id.unwrap(), ty)?)
                    }
                    None => None,
                };
                Ok(hir::Expression {
                    kind: hir::ExprKind::IfElse {
                        condition,
                        then_block: Box::new(coerced_then_block),
                        else_block: coerced_else_block.map(Box::new),
                    },
                    ty: Some(ty),
                })
            }
            (kind, _) => panic!("expected literal untyped expression, got {:?}", kind),
        }
    }

    fn coerce_untyped_block_expr(
        &mut self,
        expressions: Box<[hir::Expression]>,
        result: Option<Box<hir::Expression>>,
        scope_index: hir::ScopeIndex,
        expr_id: ast::ExprId,
        ty: hir::Type,
    ) -> Result<hir::Expression, ()> {
        match result {
            Some(result) => {
                let result_expr_id = match &self.ast.get_expr(expr_id).unwrap().kind {
                    ast::ExprKind::Block { result, .. } => result.unwrap(),
                    kind => unreachable!("expected block expression {:?}", kind),
                };
                let coerced = self.coerce_expr(*result, result_expr_id, ty)?;
                Ok(hir::Expression {
                    kind: hir::ExprKind::Block {
                        result: Some(Box::new(coerced.clone())),
                        expressions,
                        scope_index,
                    },
                    ty: Some(ty),
                })
            }
            None => match ty {
                hir::Type::Unit => Ok(hir::Expression {
                    kind: hir::ExprKind::Block {
                        result: None,
                        expressions,
                        scope_index,
                    },
                    ty: Some(hir::Type::Unit),
                }),
                _ => {
                    let ast_expr = self.ast.get_expr(expr_id).unwrap();
                    self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                        file_id: self.ast.file_id,
                        expected: ty,
                        actual: None,
                        span: ast_expr.span,
                    });
                    Err(())
                }
            },
        }
    }

    fn coerce_untyped_int_expr(
        &mut self,
        hir_expr_id: hir::ExprIndex,
        ast_expr_id: ast::ExprId,
        target_type: hir::Type,
    ) -> Result<hir::ExprIndex, ()> {
        use hir::{ExprKind, PrimitiveType, Type};
        let hir_expr = self
            .hir
            .expressions
            .get_mut(hir_expr_id.0 as usize)
            .unwrap();

        match target_type {
            Type::Primitive(primitive) => match primitive {
                PrimitiveType::I32 => {
                    let value = match hir_expr.kind {
                        hir::ExprKind::Int(value) => value,
                        _ => unreachable!(),
                    };

                    if value > i32::MAX as i64 || value < i32::MIN as i64 {
                        let ast_expr = self.ast.get_expr(ast_expr_id).unwrap();
                        self.diagnostics.push(DiagnosticContext::LiteralOutOfRange {
                            file_id: self.ast.file_id,
                            primitive,
                            value,
                            span: ast_expr.span,
                        });

                        *hir_expr = hir::Expression {
                            kind: ExprKind::Int(0),
                            ty: Some(Type::Primitive(primitive)),
                        };
                    } else {
                        hir_expr.ty = Some(Type::Primitive(PrimitiveType::I32));
                    }
                    Ok(hir_expr_id)
                }
                PrimitiveType::I64 => {
                    hir_expr.ty = Some(Type::Primitive(PrimitiveType::I64));
                    Ok(hir_expr_id)
                }
            },
            _ => {
                let ast_expr = self.ast.get_expr(ast_expr_id).unwrap();
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected: target_type,
                    actual: None,
                    span: ast_expr.span,
                });
                return Err(());
            }
        }
    }

    fn build_binary_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::ExprIndex, ()> {
        let operator = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Binary(expr) => expr.operator.clone(),
            _ => unreachable!("expected binary expression"),
        };

        use ast::BinaryOp;
        match operator {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                self.build_arithmetic_expr(ctx, expr_id)
            }
            BinaryOp::Assign => self.build_assignment_expr(ctx, expr_id),
            BinaryOp::AddAssign
            | BinaryOp::SubAssign
            | BinaryOp::MulAssign
            | BinaryOp::DivAssign
            | BinaryOp::RemAssign => self.build_arithmetic_assignment_expr(ctx, expr_id),
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => self.build_comparison_binary_expr(ctx, expr_id),
            BinaryOp::And | BinaryOp::Or => self.build_logical_binary_expr(ctx, expr_id),
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::LeftShift
            | BinaryOp::RightShift => self.build_bitwise_binary_expr(ctx, expr_id),
        }
    }

    fn build_assignment_expr(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Binary(expr) => expr.clone(),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, expr.left)?;
        let right = self.build_expression(ctx, expr.right)?;

        let (scope_index, local_index) = match left.kind {
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => (scope_index, local_index),
            hir::ExprKind::Placeholder => {
                let right_type = match right.ty {
                    Some(ty) => ty,
                    None => panic!("can't drop untyped value"),
                };

                return Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        operator: ast::BinaryOp::Assign,
                        lhs: Box::new(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: Some(right_type),
                        }),
                        rhs: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                });
            }
            _ => panic!("left side of assignment must be a variable"),
        };

        let local = match ctx.frame.get_local(scope_index, local_index) {
            Some(local) => local,
            None => panic!("can't assign to undeclared variable"),
        };
        match local.mutability {
            hir::Mutability::Mutable => {}
            hir::Mutability::Const => panic!("can't assign to const variable"),
        }

        let coerced_rhs = self
            .coerce_expr(right, expr.right, local.ty)
            .expect("invalid assignment type");

        Ok(hir::Expression {
            kind: hir::ExprKind::Binary {
                lhs: Box::new(left),
                operator: ast::BinaryOp::Assign,
                rhs: Box::new(coerced_rhs),
            },
            ty: Some(hir::Type::Unit),
        })
    }

    fn build_arithmetic_assignment_expr(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Binary(expr) => expr.clone(),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, expr.left)?;
        let right = self.build_expression(ctx, expr.right)?;

        let (scope_index, local_index) = match left.kind {
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => (scope_index, local_index),
            _ => panic!("left side of assignment must be a variable"),
        };

        let local = match ctx.frame.get_local(scope_index, local_index) {
            Some(local) => local,
            None => panic!("can't assign to undeclared variable"),
        };
        match local.mutability {
            hir::Mutability::Mutable => {}
            hir::Mutability::Const => panic!("can't assign to const variable"),
        }

        let coerced_rhs = self
            .coerce_expr(right, expr.right, local.ty)
            .expect("invalid assignment type");

        Ok(hir::Expression {
            kind: hir::ExprKind::Binary {
                lhs: Box::new(left),
                operator: expr.operator,
                rhs: Box::new(coerced_rhs),
            },
            ty: Some(hir::Type::Unit),
        })
    }

    fn build_bitwise_binary_expr(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Binary(expr) => expr.clone(),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, expr.left)?;
        let right = self.build_expression(ctx, expr.right)?;

        use hir::{ExprKind, Expression, PrimitiveType, Type};
        match (left.ty, right.ty) {
            (
                Some(Type::Primitive(PrimitiveType::I32)),
                Some(Type::Primitive(PrimitiveType::I32)),
            )
            | (
                Some(Type::Primitive(PrimitiveType::I64)),
                Some(Type::Primitive(PrimitiveType::I64)),
            ) => {
                let ty = left.ty.clone();
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator: expr.operator,
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    },
                    ty,
                })
            }
            (Some(Type::Primitive(PrimitiveType::I64 | PrimitiveType::I32)), None)
            | (None, Some(Type::Primitive(PrimitiveType::I64 | PrimitiveType::I32))) => {
                let (typed, untyped) = match left.ty {
                    Some(_) => ((left.clone(), expr.left), (right, expr.right)),
                    None => ((right, expr.right), (left.clone(), expr.left)),
                };

                let ty = typed.0.ty.unwrap();
                let coerced = self
                    .coerce_untyped_expr(untyped.0, untyped.1, ty)
                    .expect("invalid coercion");

                let (left, right) = match left.ty {
                    Some(_) => (typed.0, coerced),
                    None => (coerced, typed.0),
                };

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator: expr.operator,
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    },
                    ty: Some(ty),
                })
            }
            (None, None) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator: expr.operator,
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                },
                ty: None,
            }),
            _ => panic!("type mismatch in bitwise binary expression"),
        }
    }

    fn build_arithmetic_expr(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::ExprIndex, ()> {
        let expr = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Binary(expr) => expr.clone(),
            _ => unreachable!("expected binary expression"),
        };

        let left_id = self.build_expression(ctx, expr.left)?;
        let right_id = self.build_expression(ctx, expr.right)?;
        let left = self.hir.get_expr(left_id).unwrap();
        let right = self.hir.get_expr(right_id).unwrap();

        use hir::{ExprKind, Expression, Type};
        match (left.ty, right.ty) {
            (Some(Type::Primitive(primitive_1)), Some(Type::Primitive(primitive_2)))
                if primitive_1 == primitive_2 =>
            {
                Ok(self.push_expr(Expression {
                    kind: ExprKind::Binary {
                        operator: expr.operator,
                        lhs: left_id,
                        rhs: right_id,
                    },
                    ty: Some(Type::Primitive(primitive_1)),
                }))
            }
            (None, Some(Type::Primitive(ty))) | (Some(Type::Primitive(ty)), None) => {
                let (typed, untyped) = match left.ty {
                    Some(_) => ((left.clone(), expr.left), (right, expr.right)),
                    None => ((right, expr.right), (left.clone(), expr.left)),
                };

                let coerced = self
                    .coerce_untyped_expr(untyped.0, untyped.1, Type::Primitive(ty))
                    .expect("invalid coercion");

                let (left, right) = match left.ty {
                    Some(_) => (typed.0, coerced),
                    None => (coerced, typed.0),
                };

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator: expr.operator,
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    },
                    ty: Some(Type::Primitive(ty)),
                })
            }
            (None, None) => Ok(self.push_expr(Expression {
                kind: ExprKind::Binary {
                    operator: expr.operator,
                    lhs: left_id,
                    rhs: right_id,
                },
                ty: None,
            })),
            _ => panic!("type mismatch in binary expression"),
        }
    }

    fn build_comparison_binary_expr(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Binary(expr) => expr.clone(),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, expr.left)?;
        let right = self.build_expression(ctx, expr.right)?;

        use hir::{ExprKind, Expression, Type};
        match (left.ty, right.ty) {
            (Some(Type::Primitive(primitive_1)), Some(Type::Primitive(primitive_2)))
                if primitive_1 == primitive_2 =>
            {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator: expr.operator,
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                })
            }
            (Some(Type::Enum(enum_index_1)), Some(Type::Enum(enum_index_2)))
                if enum_index_1 == enum_index_2 =>
            {
                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator: expr.operator,
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                })
            }
            (Some(Type::Bool), Some(Type::Bool)) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator: expr.operator,
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                },
                ty: Some(Type::Bool),
            }),
            (None, Some(ty)) | (Some(ty), None) => {
                let (typed, untyped) = match left.ty {
                    Some(_) => ((left.clone(), expr.left), (right, expr.right)),
                    None => ((right, expr.right), (left.clone(), expr.left)),
                };

                let coerced = self
                    .coerce_untyped_expr(untyped.0, untyped.1, ty)
                    .expect("invalid coercion");

                let (left, right) = match left.ty {
                    Some(_) => (typed.0, coerced),
                    None => (coerced, typed.0),
                };

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator: expr.operator,
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                })
            }
            (None, None) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator: expr.operator,
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                },
                // even though we know that the result of a comparison is always a boolean
                // we mark it as None, so later we can easily check that this node requires coercion
                // this created simpler tree traversal logic
                ty: None,
            }),
            (l, r) => panic!("can't compare these types {:?} {:?}", l, r),
        }
    }

    fn coerce_boolean_expr(
        &mut self,
        expr: hir::Expression,
        ast_expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        match expr.ty {
            Some(hir::Type::Bool) => Ok(expr),
            None => {
                let coerced = self.coerce_untyped_expr(expr, ast_expr_id, hir::Type::Bool)?;
                Ok(coerced)
            }
            _ => {
                let ast_expr = self.ast.get_expr(ast_expr_id).unwrap();
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected: hir::Type::Bool,
                    actual: expr.ty,
                    span: ast_expr.span,
                });
                Err(())
            }
        }
    }

    fn build_logical_binary_expr(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Binary(expr) => expr.clone(),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, expr.left)?;
        let right = self.build_expression(ctx, expr.right)?;

        let (left, right) = match (
            self.coerce_boolean_expr(left, expr.left),
            self.coerce_boolean_expr(right, expr.right),
        ) {
            (Ok(coerced_left), Ok(coerced_right)) => (coerced_left, coerced_right),
            _ => return Err(()),
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::Binary {
                operator: expr.operator,
                lhs: Box::new(left),
                rhs: Box::new(right),
            },
            ty: Some(hir::Type::Bool),
        })
    }
}
