use core::panic;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::diagnostics::DiagnosticContext;
use crate::hir::global::{GlobalContext, GlobalValue};
use crate::hir::local::{BlockScope, LocalContext, LocalType, LocalValue};
use crate::hir::*;
use crate::{ast, hir};

pub struct Builder<'ast, 'interner> {
    ast: &'ast ast::Ast,
    global: GlobalContext<'interner>,
    diagnostics: Vec<diagnostics::DiagnosticContext>,
    hir: HIR,
}

impl<'ast, 'interner> Builder<'ast, 'interner> {
    pub fn build(
        ast: &'ast ast::Ast,
        interner: &'interner mut StringInterner<StringBackend<SymbolU32>>,
    ) -> (HIR, Vec<DiagnosticContext>) {
        let mut builder = Builder {
            ast,
            global: GlobalContext::new(interner, &ast.items),
            diagnostics: Vec::new(),
            hir: HIR::new(ast.file_id),
        };

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::ExportModifier { item, .. } => builder.build_item(item),
                ast::ItemKind::FunctionDefinition { .. } => builder.build_item(item),
                _ => unimplemented!("item kind not implemented"),
            }
        }

        let Builder {
            mut hir,
            global,
            diagnostics,
            ..
        } = builder;
        hir.enums = global.enums;

        (hir, diagnostics)
    }

    fn build_item(&mut self, item: &ast::Item) {
        match &item.kind {
            ast::ItemKind::FunctionDefinition { .. } => {
                match self.build_function_definition(item) {
                    Ok(func) => self.hir.functions.push(func),
                    Err(_) => {}
                }
            }
            _ => unimplemented!("item kind not implemented"),
        }
    }

    fn build_function_definition(&mut self, item: &ast::Item) -> Result<hir::Function, ()> {
        let (signature, block) = match &item.kind {
            ast::ItemKind::FunctionDefinition { signature, block } => (signature, block),
            _ => unreachable!(),
        };
        let func_index = self.global.resolve_function(signature.name.symbol).unwrap();
        let func_type = self
            .global
            .functions
            .get(func_index.0 as usize)
            .unwrap()
            .clone();

        let locals: Vec<Local> = signature
            .params
            .iter()
            .map(|param| Local {
                name: param.name.symbol,
                ty: self
                    .global
                    .resolve_type(param.ty.symbol)
                    .unwrap_or(Type::Unknown),
                mutability: Mutability::Mutable,
            })
            .collect();

        let lookup = locals
            .iter()
            .enumerate()
            .map(|(index, param)| {
                (
                    (LocalType::Local, ScopeIndex(0), param.name),
                    LocalValue::Local(LocalIndex(index as u32)),
                )
            })
            .collect();

        let mut ctx = LocalContext {
            func_index,
            frame: StackFrame {
                scopes: vec![BlockScope {
                    parent_scope: None,
                    locals,
                    result: Some(func_type.result),
                }],
            },
            scope_index: ScopeIndex(0),
            lookup,
        };
        let block = self.build_block_expression(&mut ctx, &block)?;

        Ok(hir::Function {
            ty: func_type,
            name: signature.name.symbol,
            stack: ctx.frame,
            block: Box::new(block),
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
                match scope.result {
                    Some(_) => Ok(value),
                    None => {
                        scope.result = Some(hir::Type::Never);
                        Ok(value)
                    }
                }
            }
            _ => {
                self.diagnostics
                    .push(DiagnosticContext::UnusedExpressionValue {
                        file_id: self.ast.file_id,
                        span: ast_value.span,
                    });

                Ok(hir::Expression {
                    kind: hir::ExprKind::Binary {
                        operator: ast::BinaryOp::Assign,
                        left: Box::new(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: value.ty.clone(),
                        }),
                        right: Box::new(value),
                    },
                    ty: Some(hir::Type::Unit),
                })
            }
        }
    }

    fn build_local_definition_statement(
        &mut self,
        ctx: &mut LocalContext,
        stmt: &ast::Statement,
    ) -> Result<Expression, ()> {
        let (maybe_mutable, name, ty, ast_value) = match &stmt.kind {
            ast::StmtKind::LocalDefinition {
                mutable,
                name,
                ty,
                value,
            } => (mutable.clone(), name.clone(), ty.clone(), value),
            _ => unreachable!(),
        };

        let expected_type = match ty {
            Some(ty) => match self.global.resolve_type(ty.symbol) {
                Some(ty) => Some(ty),
                None => panic!("failed to resolve type"),
            },
            None => None,
        };
        let value = self.build_expression(ctx, ast_value, expected_type)?;
        let ty = match value.ty {
            Some(ty) => ty,
            None => {
                self.diagnostics
                    .push(DiagnosticContext::TypeAnnotationRequired {
                        file_id: self.ast.file_id,
                        span: name.span,
                    });
                return Err(());
            }
        };

        let local_index = ctx.push_local(hir::Local {
            name: name.symbol,
            ty,
            mutability: match maybe_mutable {
                Some(_) => Mutability::Mutable,
                None => Mutability::Const,
            },
        });

        Ok(hir::Expression {
            kind: hir::ExprKind::LocalDeclaration {
                scope_index: ctx.scope_index,
                local_index,
                expr: Box::new(value),
            },
            ty: Some(Type::Unit),
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
            ExprKind::Int { .. } => self.build_int_expression(expr, expected_type),
            ExprKind::Grouping { value } => self.build_expression(ctx, value, expected_type),
            ExprKind::Identifier { .. } => {
                self.build_identifier_expression(ctx, expr, expected_type)
            }
            ExprKind::Binary { .. } => self.build_binary_expression(ctx, expr, expected_type),
            ExprKind::Unary { .. } => self.build_unary_expression(ctx, expr, expected_type),
            ExprKind::Call { .. } => self.build_call_expression(ctx, expr, expected_type),
            ExprKind::Namespace { .. } => self.build_namespace_expression(expr, expected_type),
            ExprKind::Return { .. } => self.build_return_expression(ctx, expr, expected_type),
            ExprKind::Block { .. } => {
                ctx.enter_scope(expected_type, |ctx| self.build_block_expression(ctx, expr))
            }
            ExprKind::Label { label, block } => {
                ctx.enter_scope_with_label(label.symbol, expected_type, |ctx| {
                    self.build_block_expression(ctx, block)
                })
            }
            ExprKind::IfElse { .. } => self.build_if_else_expression(ctx, expr, expected_type),
            ExprKind::Cast { .. } => self.build_cast_expression(ctx, expr, expected_type),
            ExprKind::Break { .. } => self.build_break_expression(ctx, expr, expected_type),
            ExprKind::Continue { .. } => unimplemented!(),
            ExprKind::Loop { .. } => unimplemented!(),
        }
    }

    fn build_int_expression(
        &mut self,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<Expression, ()> {
        let value = match expr.kind {
            ast::ExprKind::Int { value } => value,
            _ => unreachable!(),
        };

        match expected_type {
            Some(ty) => Ok(self.coerce_untyped_int_expr(
                hir::Expression {
                    kind: hir::ExprKind::Int(value),
                    ty: None,
                },
                expr,
                ty,
            )?),
            None => Ok(hir::Expression {
                kind: hir::ExprKind::Int(value),
                ty: None,
            }),
        }
    }

    fn build_identifier_expression(
        &mut self,
        ctx: &LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<Expression, ()> {
        let symbol = match expr.kind {
            ast::ExprKind::Identifier { symbol } => symbol.clone(),
            _ => unreachable!(),
        };
        match ctx.resolve_local(symbol) {
            Some((scope_index, local_index)) => {
                let local = ctx.frame.get_local(scope_index, local_index).unwrap();
                match expected_type {
                    Some(ty) if ty != local.ty => {
                        self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                            file_id: self.ast.file_id,
                            expected: ty,
                            actual: local.ty,
                            span: expr.span,
                        });
                        return Err(());
                    }
                    _ => {}
                }

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
        match self.global.resolve_value(symbol) {
            Some(global) => match global {
                GlobalValue::Bool { value } => {
                    match expected_type {
                        Some(Type::Bool) => {}
                        Some(ty) => {
                            self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                                file_id: self.ast.file_id,
                                expected: ty,
                                actual: Type::Bool,
                                span: expr.span,
                            });
                            return Err(());
                        }
                        None => {}
                    }

                    return Ok(Expression {
                        kind: ExprKind::Bool(value),
                        ty: Some(Type::Bool),
                    });
                }
                GlobalValue::Placeholder => {
                    return Ok(Expression {
                        kind: ExprKind::Placeholder,
                        ty: expected_type,
                    });
                }
                GlobalValue::Function { func_index } => {
                    match expected_type {
                        Some(Type::Function(index)) if index == func_index => {}
                        Some(ty) => {
                            self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                                file_id: self.ast.file_id,
                                expected: ty,
                                actual: Type::Function(func_index),
                                span: expr.span,
                            });
                            return Err(());
                        }
                        None => {}
                    }

                    return Ok(Expression {
                        kind: ExprKind::Function(func_index),
                        ty: Some(Type::Function(func_index)),
                    });
                }
                GlobalValue::EnumVariant {
                    enum_index,
                    variant_index,
                } => {
                    match expected_type {
                        Some(Type::Enum(index)) if index == enum_index => {}
                        Some(ty) => {
                            self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                                file_id: self.ast.file_id,
                                expected: ty,
                                actual: Type::Enum(enum_index),
                                span: expr.span,
                            });
                            return Err(());
                        }
                        None => {}
                    }

                    return Ok(Expression {
                        kind: ExprKind::EnumVariant {
                            enum_index,
                            variant_index,
                        },
                        ty: Some(Type::Enum(enum_index)),
                    });
                }
                _ => unreachable!(),
            },
            None => {}
        };

        self.diagnostics
            .push(DiagnosticContext::UndeclaredIdentifier {
                file_id: self.ast.file_id,
                span: expr.span,
            });
        Err(())
    }

    fn build_namespace_expression(
        &mut self,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let (namespace, member) = match &expr.kind {
            ast::ExprKind::Namespace { namespace, member } => (namespace, member),
            _ => unreachable!("expected namespace member expression"),
        };

        let namespace_type = match self.global.resolve_type(namespace.symbol) {
            Some(ty) => ty,
            None => panic!("undeclared namespace"),
        };

        match expected_type {
            Some(ty) if ty == namespace_type => {}
            Some(ty) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected: ty,
                    actual: namespace_type,
                    span: expr.span,
                });
                return Err(());
            }
            None => {}
        }

        match namespace_type {
            hir::Type::Enum(enum_index) => {
                let enum_ = self.hir.get_enum(enum_index).unwrap();
                let variant_index = match enum_.lookup.get(&member.symbol).copied() {
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

    fn build_break_expression(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        match expected_type {
            Some(Type::Never) | None => {}
            Some(expected) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual: Type::Never,
                    span: expr.span,
                });
            }
        }

        let (label, maybe_ast_value) = match &expr.kind {
            ast::ExprKind::Break { label, value } => (label.clone(), value),
            _ => unreachable!("expected break expression"),
        };

        let scope_index = match label {
            Some(label) => ctx.resolve_label(label.symbol).expect("invalid label"),
            None => ctx.scope_index,
        };
        let scope = ctx.frame.scopes.get(scope_index.0 as usize).unwrap();
        println!("{:#?}", ctx.frame.scopes);

        let maybe_value = match maybe_ast_value {
            Some(ast_value) => {
                println!("scope.result {:?}", scope.result);
                let value = self.build_expression(ctx, ast_value, scope.result)?;
                println!("got type: {:?}", value.ty);
                match value.ty {
                    Some(value_type) => {
                        let scope = ctx.frame.scopes.get_mut(scope_index.0 as usize).unwrap();
                        match scope.result {
                            Some(ty) if ty == value_type => {}
                            Some(expected) => {
                                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                                    file_id: self.ast.file_id,
                                    expected,
                                    actual: value_type,
                                    span: ast_value.span,
                                });
                                return Err(());
                            }
                            None => scope.result = Some(value_type),
                        }
                    }
                    None => panic!("break expression value must have a type"),
                }

                Some(value)
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
                value: maybe_value.map(Box::new),
            },
            ty: Some(hir::Type::Never),
        })
    }

    fn build_cast_expression(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let (ast_value, ty) = match &expr.kind {
            ast::ExprKind::Cast { value, ty } => (value, ty),
            _ => unreachable!("expected cast expression"),
        };

        let cast_type = match self.global.resolve_type(ty.symbol) {
            Some(ty) => ty,
            None => panic!("invalid type in cast expression"),
        };
        let value = self.build_expression(ctx, ast_value, Some(cast_type))?;

        match expected_type {
            Some(ty) if ty == cast_type => {}
            None => {}
            Some(expected) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual: cast_type,
                    span: expr.span,
                });
            }
        }

        Ok(value)
    }

    fn build_if_else_expression(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
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

        let mut then_block = match ast_then_block.kind {
            ast::ExprKind::Block { .. } => ctx.enter_scope(expected_type, |ctx| {
                self.build_block_expression(ctx, ast_then_block)
            })?,
            _ => unreachable!(),
        };
        let (else_block, ty) = match maybe_ast_else_block {
            Some(ast_else_block) => {
                let else_block = match ast_else_block.kind {
                    ast::ExprKind::Block { .. } => ctx.enter_scope(expected_type, |ctx| {
                        self.build_block_expression(ctx, ast_else_block)
                    })?,
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
                            .coerce_expr(else_block, ast_else_block, ty)
                            .expect("the type of else block can't be coerced to then block type");

                        (Some(coerced), Some(ty))
                    }
                    (None, Some(ty)) => {
                        then_block = self
                            .coerce_expr(then_block, ast_then_block, ty)
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
                condition: Box::new(condition),
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new),
            },
            ty,
        })
    }

    fn build_block_expression(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
    ) -> Result<hir::Expression, ()> {
        let (statements, maybe_ast_result) = match &expr.kind {
            ast::ExprKind::Block { statements, result } => (statements, result),
            _ => panic!("expected block expression"),
        };

        let x = 'a: {
            if 5 == 0 {
                break 'a 5;
            };

            loop {}
        };

        let mut expressions: Vec<_> = Vec::with_capacity(statements.len());
        for stmt in statements.iter() {
            let expr = self.build_statement(ctx, stmt)?;
            let ty = expr.ty;
            expressions.push(expr);

            match ty {
                Some(Type::Unit) => {}
                Some(Type::Never) => {
                    // don't process the rest of statements as they are unreachable
                    // TODO: report warning about unreachable code if any

                    break;
                }
                ty => unreachable!("statement must return either unit or never: {ty:?}"),
            }
        }

        let (result, result_type) = match maybe_ast_result {
            Some(ast_result) => {
                let expected_type = ctx
                    .frame
                    .scopes
                    .get(ctx.scope_index.0 as usize)
                    .unwrap()
                    .result;
                let expr = self.build_expression(ctx, ast_result, expected_type)?;
                let scope = ctx
                    .frame
                    .scopes
                    .get_mut(ctx.scope_index.0 as usize)
                    .unwrap();

                match (expr.ty, scope.result) {
                    (Some(expr_type), Some(result_type)) => {
                        if expr_type != result_type {
                            self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                                file_id: self.ast.file_id,
                                expected: result_type,
                                actual: expr_type,
                                span: ast_result.span,
                            });
                            return Err(());
                        }
                        (Some(expr), Some(result_type))
                    }
                    (Some(expr_type), None) => {
                        scope.result = Some(expr_type);
                        (Some(expr), Some(expr_type))
                    }
                    (None, Some(_)) => unreachable!("probably"),
                    (None, None) => {
                        self.diagnostics
                            .push(DiagnosticContext::TypeAnnotationRequired {
                                file_id: self.ast.file_id,
                                span: ast_result.span,
                            });
                        return Err(());
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
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        match expected_type {
            Some(Type::Never) | None => {}
            Some(expected) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual: Type::Never,
                    span: expr.span,
                });
            }
        }

        let maybe_ast_value = match &expr.kind {
            ast::ExprKind::Return { value } => value,
            _ => unreachable!(),
        };

        let expected_type = ctx
            .frame
            .scopes
            .get(0)
            .unwrap()
            .result
            .expect("function must have return type");

        match maybe_ast_value {
            Some(ast_value) => {
                let value = self.build_expression(ctx, ast_value, Some(expected_type))?;

                Ok(hir::Expression {
                    kind: hir::ExprKind::Return {
                        value: Some(Box::new(value)),
                    },
                    ty: Some(hir::Type::Never),
                })
            }
            None => {
                match expected_type {
                    hir::Type::Unit => {}
                    _ => panic!("return type mistmatch, can't return unit"),
                }
                Ok(hir::Expression {
                    kind: hir::ExprKind::Return { value: None },
                    ty: Some(hir::Type::Never),
                })
            }
        }
    }

    fn build_call_expression(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let (ast_callee, ast_arguments) = match &expr.kind {
            ast::ExprKind::Call { callee, arguments } => (callee, arguments),
            _ => unreachable!("expected call expression"),
        };

        let callee = self.build_expression(ctx, ast_callee, None)?;
        let func_index = match callee.kind {
            hir::ExprKind::Function(func_index) => func_index,
            _ => {
                self.diagnostics
                    .push(DiagnosticContext::NonCallableIdentifier {
                        file_id: self.ast.file_id,
                        span: ast_callee.span,
                    });
                return Err(());
            }
        };

        let func_type = match self.global.functions.get(func_index.0 as usize) {
            Some(func_type) => func_type.clone(),
            None => panic!("invalid function index"),
        };
        match expected_type {
            Some(ty) if ty == func_type.result => {}
            None => {}
            Some(expected) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual: func_type.result,
                    span: expr.span,
                });
            }
        }
        let arguments: Box<_> = ast_arguments
            .iter()
            .enumerate()
            .map(|(index, ast_argument)| {
                let expected_type = func_type.params.get(index).copied().unwrap();
                self.build_expression(ctx, ast_argument, Some(expected_type))
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
                Some(Type::Primitive(ty)) => Ok(hir::Expression {
                    kind: ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty: Some(Type::Primitive(ty)),
                }),
                None => Ok(Expression {
                    kind: ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty: None,
                }),
                _ => panic!("can't apply unary operator to this type"),
            },
            ast::UnaryOp::Not => match operand.ty {
                Some(Type::Bool) => Ok(hir::Expression {
                    kind: ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty: Some(Type::Bool),
                }),
                _ => panic!("can't apply logical not to this type"),
            },
        }
    }

    fn coerce_expr(
        &mut self,
        expr: hir::Expression,
        ast_expr: &ast::Expression,
        expected_type: Type,
    ) -> Result<hir::Expression, ()> {
        match (expected_type, expr.ty) {
            (ty, None) => self.coerce_untyped_expr(expr, ast_expr, ty),
            (hir::Type::Primitive(ty), Some(hir::Type::Primitive(ty2))) if ty == ty2 => {
                Ok(expr.clone())
            }
            (hir::Type::Bool, Some(hir::Type::Bool)) => Ok(expr.clone()),
            (hir::Type::Enum(index_1), Some(hir::Type::Enum(index_2))) if index_1 == index_2 => {
                Ok(expr.clone())
            }
            (expected, Some(actual)) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual,
                    span: ast_expr.span,
                });
                Err(())
            }
        }
    }

    fn coerce_untyped_expr(
        &mut self,
        expr: hir::Expression,
        ast_expr: &ast::Expression,
        target_type: hir::Type,
    ) -> Result<hir::Expression, ()> {
        match expr.kind {
            hir::ExprKind::Int(_) => self.coerce_untyped_int_expr(expr, ast_expr, target_type),
            hir::ExprKind::Unary { operator, operand } => {
                let ast_operand = match &ast_expr.kind {
                    ast::ExprKind::Unary { operand, .. } => operand,
                    _ => unreachable!("expected unary expression"),
                };

                let operand = self.coerce_untyped_expr(*operand, ast_operand, target_type)?;
                Ok(hir::Expression {
                    kind: hir::ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty: Some(target_type),
                })
            }
            hir::ExprKind::Binary { .. } => {
                self.coerce_untyped_binary_expression(expr, ast_expr, target_type)
            }
            hir::ExprKind::Block { .. } => {
                self.coerce_untyped_block_expr(expr, ast_expr, target_type)
            }
            hir::ExprKind::IfElse { .. } => {
                self.coerce_untyped_if_else_expression(expr, ast_expr, target_type)
            }
            kind => panic!("expected literal untyped expression, got {:?}", kind),
        }
    }

    fn coerce_untyped_binary_expression(
        &mut self,
        expr: hir::Expression,
        ast_expr: &ast::Expression,
        target_type: hir::Type,
    ) -> Result<hir::Expression, ()> {
        let (left, right, operator) = match expr.kind {
            hir::ExprKind::Binary {
                operator,
                left,
                right,
            } => (left, right, operator),
            _ => unreachable!(),
        };
        let (ast_left, ast_right) = match &ast_expr.kind {
            ast::ExprKind::Binary { left, right, .. } => (left, right),
            _ => unreachable!(),
        };

        let ty = match operator {
            operator if operator.is_arithmetic() || operator.is_bitwise() => match target_type {
                hir::Type::Primitive(_) => target_type,
                _ => panic!("can't apply binary operator and get {target_type}"),
            },
            operator if operator.is_comparison() || operator.is_logical() => {
                panic!("type annotation required")
            }
            _ => unreachable!("assignment should always have a known type"),
        };

        let left = self.coerce_untyped_expr(*left, &ast_left, ty.clone())?;
        let right = self.coerce_untyped_expr(*right, &ast_right, ty.clone())?;

        Ok(hir::Expression {
            kind: hir::ExprKind::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            },
            ty: Some(ty),
        })
    }

    fn coerce_untyped_if_else_expression(
        &mut self,
        expr: hir::Expression,
        ast_expr: &ast::Expression,
        target_type: hir::Type,
    ) -> Result<hir::Expression, ()> {
        let (condition, then_block, else_block) = match expr.kind {
            hir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => (condition, then_block, else_block),
            _ => unreachable!(),
        };
        let (ast_then_block, ast_else_block) = match &ast_expr.kind {
            ast::ExprKind::IfElse {
                then_block,
                else_block,
                ..
            } => (then_block, else_block),
            _ => unreachable!(),
        };

        let coerced_then_block =
            self.coerce_untyped_expr(*then_block, &ast_then_block, target_type)?;
        let coerced_else_block = match else_block {
            Some(else_block) => Some(self.coerce_untyped_expr(
                *else_block,
                ast_else_block.as_deref().unwrap(),
                target_type,
            )?),
            None => None,
        };
        Ok(hir::Expression {
            kind: hir::ExprKind::IfElse {
                condition,
                then_block: Box::new(coerced_then_block),
                else_block: coerced_else_block.map(Box::new),
            },
            ty: Some(target_type),
        })
    }

    fn coerce_untyped_block_expr(
        &mut self,
        expr: hir::Expression,
        ast_expr: &ast::Expression,
        target_type: hir::Type,
    ) -> Result<hir::Expression, ()> {
        let (expressions, result, scope_index) = match expr.kind {
            hir::ExprKind::Block {
                expressions,
                result,
                scope_index,
            } => (expressions, result, scope_index.clone()),
            _ => unreachable!(),
        };
        let ast_result = match &ast_expr.kind {
            ast::ExprKind::Block { result, .. } => result,
            _ => unreachable!(),
        };

        match result {
            Some(result) => {
                let result =
                    self.coerce_expr(*result, ast_result.as_deref().unwrap(), target_type)?;
                Ok(hir::Expression {
                    kind: hir::ExprKind::Block {
                        result: Some(Box::new(result)),
                        expressions,
                        scope_index,
                    },
                    ty: Some(target_type),
                })
            }
            None => match target_type {
                hir::Type::Unit => Ok(hir::Expression {
                    kind: hir::ExprKind::Block {
                        result: None,
                        expressions,
                        scope_index,
                    },
                    ty: Some(hir::Type::Unit),
                }),
                _ => todo!("figure out when it happens and how to handle this"),
            },
        }
    }

    fn coerce_untyped_int_expr(
        &mut self,
        expr: hir::Expression,
        ast_expr: &ast::Expression,
        target_type: hir::Type,
    ) -> Result<hir::Expression, ()> {
        match target_type {
            Type::Primitive(primitive) => match primitive {
                PrimitiveType::I32 => {
                    let value = match expr.kind {
                        hir::ExprKind::Int(value) => value,
                        _ => unreachable!(),
                    };

                    if value > i32::MAX as i64 || value < i32::MIN as i64 {
                        self.diagnostics.push(DiagnosticContext::LiteralOutOfRange {
                            file_id: self.ast.file_id,
                            primitive,
                            value,
                            span: ast_expr.span,
                        });

                        Ok(hir::Expression {
                            kind: ExprKind::Int(0),
                            ty: Some(Type::Primitive(PrimitiveType::I32)),
                        })
                    } else {
                        Ok(hir::Expression {
                            kind: ExprKind::Int(value),
                            ty: Some(Type::Primitive(PrimitiveType::I32)),
                        })
                    }
                }
                PrimitiveType::I64 => {
                    let value = match expr.kind {
                        hir::ExprKind::Int(value) => value,
                        _ => unreachable!(),
                    };

                    Ok(hir::Expression {
                        kind: ExprKind::Int(value),
                        ty: Some(Type::Primitive(PrimitiveType::I64)),
                    })
                }
            },
            _ => panic!("WTF! can't coerce this thing to primitive number"),
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
            BinaryOp::Assign => self.build_assignment_expr(ctx, expr, expected_type),
            BinaryOp::AddAssign
            | BinaryOp::SubAssign
            | BinaryOp::MulAssign
            | BinaryOp::DivAssign
            | BinaryOp::RemAssign => {
                self.build_arithmetic_assignment_expr(ctx, expr, expected_type)
            }
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => self.build_comparison_binary_expr(ctx, expr, expected_type),
            BinaryOp::And | BinaryOp::Or => {
                self.build_logical_binary_expr(ctx, expr, expected_type)
            }
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
        expected_type: Option<Type>,
    ) -> Result<Expression, ()> {
        match expected_type {
            None | Some(Type::Unit) => {}
            Some(expected) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual: Type::Unit,
                    span: expr.span,
                });
            }
        }

        let (ast_left, ast_right) = match &expr.kind {
            ast::ExprKind::Binary { left, right, .. } => (left, right),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, ast_left, None)?;
        let (scope_index, local_index) = match left.kind {
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => (scope_index, local_index),
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
                        }),
                        right: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
                });
            }
            _ => panic!("left side of assignment must be a variable"),
        };
        let local = match ctx.frame.get_local(scope_index, local_index) {
            Some(local) => local.clone(),
            None => panic!("can't assign to undeclared variable"),
        };
        match local.mutability {
            hir::Mutability::Const => panic!("can't assign to const variable"),
            hir::Mutability::Mutable => {}
        }

        let right = self.build_expression(ctx, ast_right, Some(local.ty))?;

        Ok(hir::Expression {
            kind: hir::ExprKind::Binary {
                left: Box::new(left),
                operator: ast::BinaryOp::Assign,
                right: Box::new(right),
            },
            ty: Some(hir::Type::Unit),
        })
    }

    fn build_arithmetic_assignment_expr(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<Expression, ()> {
        match expected_type {
            None | Some(Type::Unit) => {}
            Some(expected) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual: Type::Unit,
                    span: expr.span,
                });
            }
        }

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
        let (scope_index, local_index) = match left.kind {
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => (scope_index, local_index),
            _ => panic!("left side of assignment must be a variable"),
        };

        let local = match ctx.frame.get_local(scope_index, local_index) {
            Some(local) => local.clone(),
            None => panic!("can't assign to undeclared variable"),
        };
        match local.mutability {
            hir::Mutability::Const => panic!("can't assign to const variable"),
            hir::Mutability::Mutable => {}
        }

        let right = self.build_expression(ctx, ast_right, Some(local.ty))?;

        Ok(hir::Expression {
            kind: hir::ExprKind::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            },
            ty: Some(hir::Type::Unit),
        })
    }

    fn build_bitwise_binary_expr(
        &mut self,
        ctx: &mut LocalContext,
        expression: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        let (ast_left, ast_right, operator) = match &expression.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!(),
        };

        let left = self.build_expression(ctx, ast_left, expected_type)?;
        let right = self.build_expression(ctx, ast_right, expected_type)?;

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
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty,
                })
            }
            (Some(Type::Primitive(PrimitiveType::I64 | PrimitiveType::I32)), None)
            | (None, Some(Type::Primitive(PrimitiveType::I64 | PrimitiveType::I32))) => {
                let (left, right, ty) = match left.ty {
                    Some(_) => {
                        let ty = left.ty.unwrap();
                        let coerced = self
                            .coerce_untyped_expr(right, &ast_right, ty)
                            .expect("invalid coercion");

                        (left, coerced, ty)
                    }
                    None => {
                        let ty = right.ty.unwrap();
                        let coerced = self
                            .coerce_untyped_expr(left, &ast_left, ty)
                            .expect("invalid coercion");

                        (coerced, right, ty)
                    }
                };

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(ty),
                })
            }
            (None, None) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: None,
            }),
            _ => panic!("type mismatch in bitwise binary expression"),
        }
    }

    fn build_arithmetic_expr(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<Expression, ()> {
        let (ast_left, ast_right, operator) = match &expr.kind {
            ast::ExprKind::Binary {
                left,
                right,
                operator,
                ..
            } => (left, right, operator.clone()),
            _ => unreachable!("expected binary expression"),
        };

        let left = self.build_expression(ctx, ast_left, expected_type)?;
        let right = self.build_expression(ctx, ast_right, expected_type)?;

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
                    ty: Some(Type::Primitive(primitive_1)),
                })
            }
            (None, Some(Type::Primitive(ty))) | (Some(Type::Primitive(ty)), None) => {
                let (left, right) = match left.ty {
                    Some(_) => {
                        let coerced = self
                            .coerce_untyped_expr(right, &ast_right, Type::Primitive(ty))
                            .expect("invalid coercion");

                        (left, coerced)
                    }
                    None => {
                        let coerced = self
                            .coerce_untyped_expr(left, &ast_left, Type::Primitive(ty))
                            .expect("invalid coercion");

                        (coerced, right)
                    }
                };

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Primitive(ty)),
                })
            }
            (None, None) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: None,
            }),
            _ => panic!("type mismatch in binary expression"),
        }
    }

    fn build_comparison_binary_expr(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        match expected_type {
            None | Some(Type::Bool) => {}
            Some(expected) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual: Type::Bool,
                    span: expr.span,
                });
            }
        }

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
        let right = self.build_expression(ctx, ast_right, None)?;

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
                })
            }
            (Some(Type::Bool), Some(Type::Bool)) => Ok(Expression {
                kind: ExprKind::Binary {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                ty: Some(Type::Bool),
            }),
            (None, Some(ty)) | (Some(ty), None) => {
                let (left, right) = match left.ty {
                    Some(_) => {
                        let coerced = self
                            .coerce_untyped_expr(right, &ast_right, ty)
                            .expect("invalid coercion");

                        (left, coerced)
                    }
                    None => {
                        let coerced = self
                            .coerce_untyped_expr(left, &ast_left, ty)
                            .expect("invalid coercion");

                        (coerced, right)
                    }
                };

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                })
            }
            (None, None) => {
                self.diagnostics
                    .push(DiagnosticContext::ComparisonTypeAnnotationRequired {
                        file_id: self.ast.file_id,
                        left: ast_left.span,
                        right: ast_right.span,
                    });

                Ok(Expression {
                    kind: ExprKind::Binary {
                        operator,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty: Some(Type::Bool),
                })
            }
            (l, r) => panic!("can't compare these types {:?} {:?}", l, r),
        }
    }

    fn build_logical_binary_expr(
        &mut self,
        ctx: &mut LocalContext,
        expr: &ast::Expression,
        expected_type: Option<Type>,
    ) -> Result<hir::Expression, ()> {
        match expected_type {
            None | Some(Type::Bool) => {}
            Some(expected) => {
                self.diagnostics.push(DiagnosticContext::TypeMistmatch {
                    file_id: self.ast.file_id,
                    expected,
                    actual: Type::Bool,
                    span: expr.span,
                });
            }
        }

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
        })
    }
}
