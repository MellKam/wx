use core::panic;
use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::ScopeIndex;
use super::diagnostics::DiagnosticContext;
use crate::{ast, hir};

pub struct Builder<'a> {
    ast: &'a ast::Ast,
    interner: &'a StringInterner<StringBackend<SymbolU32>>,
    global: hir::GlobalScope,
    diagnostics: Vec<hir::diagnostics::DiagnosticContext>,
}

impl<'a> Builder<'a> {
    pub fn build(
        ast: &'a ast::Ast,
        interner: &'a StringInterner<StringBackend<SymbolU32>>,
    ) -> (hir::HIR, Vec<DiagnosticContext>) {
        let mut builder = Builder {
            ast,
            interner,
            global: hir::GlobalScope::new(),
            diagnostics: Vec::new(),
        };

        for (item_id, item) in builder.ast.items.iter().enumerate() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition(def) => {
                    let func_type = builder.create_function_type(&def.signature).unwrap();

                    builder
                        .global
                        .add_function(def.signature.name.symbol, func_type);
                }
                ast::ItemKind::Enum(item_enum) => {
                    let enum_ = builder.build_enum(ast::ItemId(item_id as u32), item_enum);
                    builder.global.add_enum(enum_);
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

        let hir = hir::HIR {
            functions,
            enums: builder.global.enums,
        };

        (hir, builder.diagnostics)
    }

    fn resolve_type(&mut self, identifier: ast::Identifier) -> hir::Type {
        let text = self.interner.resolve(identifier.symbol).unwrap();
        match hir::PrimitiveType::try_from(text) {
            Ok(ty) => return hir::Type::Primitive(ty),
            _ => {}
        }
        match self.global.type_lookup.get(&identifier.symbol).cloned() {
            Some(hir::GlobalType::Enum(index)) => return hir::Type::Enum(index),
            _ => {}
        }

        self.diagnostics.push(DiagnosticContext::UnknownType {
            file_id: self.ast.file_id,
            span: identifier.span,
        });

        hir::Type::Unknown
    }

    fn create_function_type(
        &mut self,
        signature: &ast::FunctionSignature,
    ) -> Result<hir::FunctionType, ()> {
        let params = signature
            .params
            .iter()
            .map(|param| self.resolve_type(param.ty.clone()))
            .collect();

        let result = match &signature.result {
            Some(ty) => self.resolve_type(ty.clone()),
            None => hir::Type::Unit,
        };

        Ok(hir::FunctionType { params, result })
    }

    fn build_enum(&mut self, item_id: ast::ItemId, def: &ast::ItemEnum) -> hir::Enum {
        let ty = match self.resolve_type(def.ty.clone()) {
            hir::Type::Primitive(ty) => ty,
            hir::Type::Unknown => hir::PrimitiveType::I32,
            ty => {
                self.diagnostics
                    .push(DiagnosticContext::InvalidEnumRepresentation {
                        file_id: self.ast.file_id,
                        type_: ty,
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
            variant_lookup: variants_lookup,
        }
    }

    fn build_function_definition(
        &mut self,
        def: &ast::ItemFunctionDefinition,
    ) -> Result<hir::Function, ()> {
        let global_value = self
            .global
            .value_lookup
            .get(&def.signature.name.symbol)
            .cloned()
            .unwrap();
        let func_index = match global_value {
            hir::GlobalValue::Function(index) => index,
            _ => panic!("expected function index"),
        };
        let func_type = self.global.get_function(func_index).unwrap().clone();

        let locals: Vec<hir::Local> = def
            .signature
            .params
            .iter()
            .map(|param| hir::Local {
                name: param.name.symbol,
                ty: self.resolve_type(param.ty.clone()),
                mutability: hir::Mutability::Const,
            })
            .collect();

        let local_lookup: HashMap<(ScopeIndex, SymbolU32), hir::LocalIndex> = locals
            .iter()
            .enumerate()
            .map(|(index, param)| ((ScopeIndex(0), param.name), hir::LocalIndex(index as u32)))
            .collect();

        let mut ctx = hir::FunctionContext {
            func_index,
            scopes: hir::LocalScopes {
                scopes: vec![hir::LocalScope {
                    parent_scope: None,
                    locals,
                }],
            },
            scope_index: hir::ScopeIndex(0),
            local_lookup,
        };

        let expr = self.build_block_expression(&mut ctx, def.block)?;
        let (expressions, result) = match expr.kind {
            hir::ExprKind::Block {
                expressions,
                result,
                ..
            } => {
                let result = match result {
                    Some(result) => Some(self.coerce_expr(*result, def.block, func_type.result)?),
                    None => None,
                };
                (expressions, result)
            }
            _ => unreachable!("expected block expression"),
        };

        Ok(hir::Function {
            ty: func_type,
            name: def.signature.name.symbol,
            export: def.export.is_some(),
            scopes: ctx.scopes,
            expressions,
            result,
        })
    }

    fn build_statement(
        &mut self,
        ctx: &mut hir::FunctionContext,
        stmt: &ast::Statement,
    ) -> Result<hir::Expression, ()> {
        match &stmt.kind {
            ast::StmtKind::DelimitedExpression { value } => {
                let expr = self.build_expression(ctx, *value)?;
                match expr.ty {
                    Some(hir::Type::Unit) => {}
                    Some(hir::Type::Never) => {}
                    _ => {
                        let ast_expr = self.ast.get_expr(*value).unwrap();
                        self.diagnostics
                            .push(DiagnosticContext::UnusedExpressionValue {
                                file_id: self.ast.file_id,
                                span: ast_expr.span,
                            });

                        return Ok(hir::Expression {
                            kind: hir::ExprKind::Binary {
                                operator: ast::BinaryOperator::Assign,
                                lhs: Box::new(hir::Expression {
                                    kind: hir::ExprKind::Placeholder,
                                    ty: expr.ty.clone(),
                                }),
                                rhs: Box::new(expr),
                            },
                            ty: Some(hir::Type::Unit),
                        });
                    }
                }
                Ok(expr)
            }
            ast::StmtKind::ConstDefinition { .. } => self.build_declaration_statement(ctx, stmt),
            ast::StmtKind::MutableDefinition { .. } => self.build_declaration_statement(ctx, stmt),
        }
    }

    fn build_declaration_statement(
        &mut self,
        ctx: &mut hir::FunctionContext,
        stmt: &ast::Statement,
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
            Some(ty) => self.resolve_type(ty),
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
    ) -> Result<hir::Expression, ()> {
        let symbol = match self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Identifier { symbol } => symbol,
            _ => unreachable!("expected identifier expression"),
        };

        match self.interner.resolve(symbol).unwrap() {
            "_" => {
                return Ok(hir::Expression {
                    kind: hir::ExprKind::Placeholder,
                    ty: None,
                });
            }
            _ => {}
        };

        match ctx.resolve_local(symbol) {
            Some((scope_index, local_index)) => {
                let local = ctx.scopes.get_local(scope_index, local_index).unwrap();
                return Ok(hir::Expression {
                    kind: hir::ExprKind::Local {
                        local_index,
                        scope_index,
                    },
                    ty: Some(local.ty),
                });
            }
            None => {}
        }

        match self.global.value_lookup.get(&symbol).cloned() {
            Some(value) => match value {
                hir::GlobalValue::Function(func_index) => {
                    return Ok(hir::Expression {
                        kind: hir::ExprKind::Function(func_index),
                        ty: Some(hir::Type::Function(func_index)),
                    });
                }
                hir::GlobalValue::EnumVariant {
                    enum_index,
                    variant_index,
                } => {
                    return Ok(hir::Expression {
                        kind: hir::ExprKind::EnumVariant {
                            enum_index,
                            variant_index,
                        },
                        ty: Some(hir::Type::Enum(enum_index)),
                    });
                }
            },
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

        let global_type = match self.global.type_lookup.get(&namespace.symbol).cloned() {
            Some(item) => item,
            None => panic!("undeclared namespace"),
        };
        let enum_index = match global_type {
            hir::GlobalType::Enum(enum_index) => enum_index,
        };

        let enum_ = self.global.get_enum(enum_index).expect("invalid enum");
        match enum_.variant_lookup.get(&member.symbol).copied() {
            Some(variant_index) => Ok(hir::Expression {
                kind: hir::ExprKind::EnumVariant {
                    enum_index,
                    variant_index,
                },
                ty: Some(hir::Type::Enum(enum_index)),
            }),
            None => {
                self.diagnostics
                    .push(DiagnosticContext::UnknownEnumVariant {
                        file_id: self.ast.file_id,
                        enum_index,
                        span: member.span,
                    });

                Ok(hir::Expression {
                    kind: hir::ExprKind::Placeholder,
                    ty: Some(hir::Type::Enum(enum_index)),
                })
            }
        }
    }

    fn build_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = self.ast.get_expr(expr_id).unwrap();
        match &expr.kind {
            ast::ExprKind::Int { value } => Ok(hir::Expression {
                kind: hir::ExprKind::Int(value.clone()),
                ty: None,
            }),
            ast::ExprKind::Identifier { .. } => self.build_identifier_expression(ctx, expr_id),
            ast::ExprKind::Binary { .. } => self.build_binary_expression(ctx, expr_id),
            ast::ExprKind::Unary { operator, operand } => Ok(Builder::build_unary_expression(
                *operator,
                self.build_expression(ctx, *operand)?,
            )),
            ast::ExprKind::Call { .. } => self.build_call_expression(ctx, expr_id),
            ast::ExprKind::NamespaceMember { .. } => {
                self.build_namespace_member_expression(expr_id)
            }
            ast::ExprKind::Return { value } => self.build_return_expression(ctx, *value),
            ast::ExprKind::Grouping { .. } => self.build_expression(ctx, expr_id),
            ast::ExprKind::Block { .. } => {
                ctx.enter_scope(|ctx| self.build_block_expression(ctx, expr_id))
            }
            ast::ExprKind::IfElse { .. } => self.build_if_else_expression(ctx, expr_id),
        }
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
        let coerced_condition =
            self.coerce_expr(condition, *condition_id, hir::Type::Enum(hir::EnumIndex(0)))?;

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
                            .expect("can't assign to this type");

                        (Some(coerced), Some(ty))
                    }
                    (None, Some(ty)) => {
                        then_block = self
                            .coerce_expr(then_block, *then_block_id, ty)
                            .expect("can't assign to this type");

                        (Some(else_block), Some(ty))
                    }
                }
            }
            None => (None, then_block.ty.clone()),
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
    ) -> Result<hir::Expression, ()> {
        let block = self
            .ast
            .get_expr(block_id)
            .expect("invalid block expression");

        let (statements, result) = match &block.kind {
            ast::ExprKind::Block { statements, result } => (statements, result),
            _ => panic!("expected block expression"),
        };

        let expressions: Vec<_> = statements
            .iter()
            .map(|&stmt_id| self.ast.get_stmt(stmt_id).unwrap())
            .map(|stmt| self.build_statement(ctx, stmt))
            .collect::<Result<Vec<_>, _>>()?;

        let mut ty: Option<hir::Type> = Some(hir::Type::Unit);
        let result = match result {
            Some(result) => {
                let expr = self.build_expression(ctx, *result)?;
                ty = expr.ty.clone();
                Some(Box::new(expr))
            }
            None => None,
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::Block {
                scope_index: ctx.scope_index,
                expressions: expressions.into_boxed_slice(),
                result,
            },
            ty,
        })
    }

    fn build_return_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        value: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = self.build_expression(ctx, value)?;
        let func_result = match self.global.get_function(ctx.func_index) {
            Some(func_type) => func_type.result,
            None => panic!("invalid function index"),
        };
        let coerced = self.coerce_expr(expr, value, func_result)?;

        Ok(hir::Expression {
            kind: hir::ExprKind::Return(Box::new(coerced)),
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

    fn build_unary_expression(
        operator: ast::UnaryOperator,
        operand: hir::Expression,
    ) -> hir::Expression {
        match operand.ty {
            Some(hir::Type::Primitive(ty)) => hir::Expression {
                kind: hir::ExprKind::Unary {
                    operator,
                    operand: Box::new(operand),
                },
                ty: Some(hir::Type::Primitive(ty)),
            },
            None => match operand.kind {
                hir::ExprKind::Int(value) => {
                    let value = match operator {
                        ast::UnaryOperator::Invert => -value,
                    };

                    return hir::Expression {
                        kind: hir::ExprKind::Int(value),
                        ty: None,
                    };
                }
                _ => panic!("is it even possible??"),
            },
            _ => panic!("can't apply unary operator to this type"),
        }
    }

    fn build_assignment_expression(
        &mut self,
        ctx: &mut hir::FunctionContext,
        left_id: ast::ExprId,
        right_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let left = self.build_expression(ctx, left_id)?;
        let right = self.build_expression(ctx, right_id)?;

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
                        operator: ast::BinaryOperator::Assign,
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

        let local = match ctx.scopes.get_local(scope_index, local_index) {
            Some(local) => local,
            None => panic!("can't assign to undeclared variable"),
        };
        match local.mutability {
            hir::Mutability::Mutable => {}
            hir::Mutability::Const => panic!("can't assign to const variable"),
        }

        let coerced_rhs = self
            .coerce_expr(right, right_id, local.ty)
            .expect("invalid assignment type");

        Ok(hir::Expression {
            kind: hir::ExprKind::Binary {
                lhs: Box::new(left),
                operator: ast::BinaryOperator::Assign,
                rhs: Box::new(coerced_rhs),
            },
            ty: Some(hir::Type::Unit),
        })
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
        ty: hir::Type,
    ) -> Result<hir::Expression, ()> {
        match (expr.kind, ty) {
            (hir::ExprKind::Int(value), ty) => self.coerce_untyped_int_expr(value, expr_id, ty),
            (hir::ExprKind::Binary { lhs, operator, rhs }, ty) => {
                let (left_id, right_id) = match self.ast.get_expr(expr_id).unwrap().kind {
                    ast::ExprKind::Binary { left, right, .. } => (left, right),
                    _ => unreachable!("expected binary expression"),
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
                let coerced_then_block =
                    self.coerce_untyped_expr(*then_block, expr_id, ty.clone())?;
                let coerced_else_block = match else_block {
                    Some(else_block) => Some(self.coerce_untyped_expr(*else_block, expr_id, ty)?),
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
                let result_expr_id = match self.ast.get_expr(expr_id).unwrap().kind {
                    ast::ExprKind::Block { result, .. } => result.unwrap(),
                    _ => unreachable!("expected block expression"),
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
        value: i64,
        expr_id: ast::ExprId,
        ty: hir::Type,
    ) -> Result<hir::Expression, ()> {
        let ast_expr = self.ast.get_expr(expr_id).unwrap();
        match ty {
            hir::Type::Primitive(primitive) => match primitive {
                hir::PrimitiveType::I32 => {
                    if value > i32::MAX as i64 || value < i32::MIN as i64 {
                        self.diagnostics.push(DiagnosticContext::LiteralOutOfRange {
                            file_id: self.ast.file_id,
                            primitive,
                            value,
                            span: ast_expr.span,
                        });
                        Ok(hir::Expression {
                            kind: hir::ExprKind::Int(0),
                            ty: Some(hir::Type::Primitive(primitive)),
                        })
                    } else {
                        Ok(hir::Expression {
                            kind: hir::ExprKind::Int(value),
                            ty: Some(hir::Type::Primitive(primitive)),
                        })
                    }
                }
                hir::PrimitiveType::I64 => Ok(hir::Expression {
                    kind: hir::ExprKind::Int(value),
                    ty: Some(hir::Type::Primitive(primitive)),
                }),
            },
            _ => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected: ty,
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
    ) -> Result<hir::Expression, ()> {
        let (left_id, right_id, operator) = match &self.ast.get_expr(expr_id).unwrap().kind {
            ast::ExprKind::Binary {
                left: left_id,
                right: right_id,
                operator,
            } => (left_id.clone(), right_id.clone(), operator.clone()),
            _ => unreachable!("expected binary expression"),
        };

        match operator {
            ast::BinaryOperator::Assign => {
                return self.build_assignment_expression(ctx, left_id, right_id);
            }
            _ => {}
        }

        let left = self.build_expression(ctx, left_id)?;
        let right = self.build_expression(ctx, right_id)?;

        use hir::*;
        match operator {
            ast::BinaryOperator::Add
            | ast::BinaryOperator::Subtract
            | ast::BinaryOperator::Multiply
            | ast::BinaryOperator::Divide
            | ast::BinaryOperator::Remainder => match (left.ty, right.ty) {
                (None, None) => {
                    let evaluated =
                        Builder::try_evaluate_untyped_binary_expr(operator, &left, &right);
                    match evaluated {
                        Some(expr) => Ok(expr),
                        None => Ok(Expression {
                            kind: ExprKind::Binary {
                                operator,
                                lhs: Box::new(left),
                                rhs: Box::new(right),
                            },
                            ty: None,
                        }),
                    }
                }
                (None, Some(ty)) | (Some(ty), None) => {
                    let (typed, untyped) = match left.ty {
                        Some(_) => ((left.clone(), left_id), (right, right_id)),
                        None => ((right, right_id), (left.clone(), left_id)),
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
                            operator,
                            lhs: Box::new(left),
                            rhs: Box::new(right),
                        },
                        ty: Some(ty),
                    })
                }
                (Some(Type::Primitive(primitive_1)), Some(Type::Primitive(primitive_2)))
                    if primitive_1 == primitive_2 =>
                {
                    Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            lhs: Box::new(left),
                            rhs: Box::new(right),
                        },
                        ty: Some(Type::Primitive(primitive_1)),
                    })
                }
                _ => panic!("type mismatch in binary expression"),
            },
            ast::BinaryOperator::Eq => match (left.ty, right.ty) {
                (Some(Type::Primitive(primitive_1)), Some(Type::Primitive(primitive_2)))
                    if primitive_1 == primitive_2 =>
                {
                    Ok(Expression {
                        kind: ExprKind::Binary {
                            operator,
                            lhs: Box::new(left),
                            rhs: Box::new(right),
                        },
                        ty: Some(Type::Enum(hir::EnumIndex(0))),
                    })
                }
                (None, None) => {
                    let evaluated =
                        Builder::try_evaluate_untyped_binary_expr(operator, &left, &right);
                    match evaluated {
                        Some(expr) => Ok(expr),
                        None => Ok(Expression {
                            kind: ExprKind::Binary {
                                operator,
                                lhs: Box::new(left),
                                rhs: Box::new(right),
                            },
                            ty: Some(Type::Enum(hir::EnumIndex(0))),
                        }),
                    }
                }
                (None, Some(ty)) | (Some(ty), None) => {
                    let (typed, untyped) = match left.ty {
                        Some(_) => ((left.clone(), left_id), (right, right_id)),
                        None => ((right, right_id), (left.clone(), left_id)),
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
                            operator,
                            lhs: Box::new(left),
                            rhs: Box::new(right),
                        },
                        ty: Some(Type::Enum(hir::EnumIndex(0))),
                    })
                }
                _ => panic!("can't compare these types"),
            },
            _ => unimplemented!("binary operator not implemented"),
        }
    }

    fn try_evaluate_untyped_binary_expr(
        operator: ast::BinaryOperator,
        left: &hir::Expression,
        right: &hir::Expression,
    ) -> Option<hir::Expression> {
        match (&left.kind, &right.kind) {
            (hir::ExprKind::Int(left), &hir::ExprKind::Int(right)) => {
                let value = match operator {
                    ast::BinaryOperator::Add => left.checked_add(right),
                    ast::BinaryOperator::Subtract => left.checked_sub(right),
                    ast::BinaryOperator::Multiply => left.checked_mul(right),
                    ast::BinaryOperator::Divide => left.checked_div(right),
                    ast::BinaryOperator::Remainder => left.checked_rem(right),
                    _ => unimplemented!("unimplemented binary operator"),
                }
                .expect("integer overflow");

                Some(hir::Expression {
                    kind: hir::ExprKind::Int(value),
                    ty: None,
                })
            }
            _ => None,
        }
    }
}
