use core::panic;
use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use crate::{ast, hir};

pub struct HIRBuilder<'a> {
    ast: &'a ast::Ast,
    interner: &'a StringInterner<StringBackend<SymbolU32>>,
    function_lookup: HashMap<SymbolU32, hir::FunctionIndex>,
    hir: hir::HIR,
}

fn try_from_symbol_to_runtime_type(
    interner: &StringInterner<StringBackend<SymbolU32>>,
    symbol: SymbolU32,
) -> Option<hir::RuntimeType> {
    match interner.resolve(symbol)? {
        "()" => Some(hir::RuntimeType::Unit),
        "i32" => Some(hir::RuntimeType::I32),
        "i64" => Some(hir::RuntimeType::I64),
        _ => None,
    }
}

#[derive(Debug)]
struct FunctionContext {
    locals: Vec<hir::Local>,
    local_lookup: HashMap<SymbolU32, hir::LocalIndex>,
    result: hir::RuntimeType,
}

impl FunctionContext {
    #[inline]
    fn lookup_local(&self, symbol: SymbolU32) -> Option<hir::Local> {
        let local_index = self.local_lookup.get(&symbol).copied()?;
        self.locals.get(local_index.0 as usize).cloned()
    }

    #[inline]
    fn push_local(
        &mut self,
        id: SymbolU32,
        ty: hir::RuntimeType,
        binding: hir::BindingType,
    ) -> hir::LocalIndex {
        let local_index = hir::LocalIndex(self.locals.len() as u32);
        self.locals.push(hir::Local {
            name: id,
            index: local_index,
            ty,
            binding,
        });
        self.local_lookup.insert(id, local_index);
        local_index
    }
}

impl<'a> HIRBuilder<'a> {
    pub fn build(
        ast: &'a ast::Ast,
        interner: &'a StringInterner<StringBackend<SymbolU32>>,
    ) -> hir::HIR {
        let mut builder = HIRBuilder {
            ast,
            interner,
            function_lookup: HashMap::new(),
            hir: hir::HIR {
                functions: Vec::new(),
            },
        };

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition { signature, .. } => {
                    let index = hir::FunctionIndex(builder.function_lookup.len() as u32);
                    builder.function_lookup.insert(signature.name, index);
                }
                _ => panic!("Expected function definition"),
            }
        }

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition { signature, body } => {
                    builder
                        .hir
                        .functions
                        .push(builder.build_function(signature, body));
                }
                _ => panic!("Expected function definition"),
            }
        }

        builder.hir
    }

    fn build_function(
        &self,
        signature: &ast::FunctionSignature,
        body: &Vec<ast::StmtId>,
    ) -> hir::Function {
        let result = match signature.output {
            Some(ty) => try_from_symbol_to_runtime_type(&self.interner, ty).expect("invalid type"),
            None => hir::RuntimeType::Unit,
        };

        let mut ctx = FunctionContext {
            locals: Vec::with_capacity(signature.params.len()),
            local_lookup: HashMap::with_capacity(signature.params.len()),
            result,
        };

        for param in signature.params.iter() {
            ctx.push_local(
                param.name,
                try_from_symbol_to_runtime_type(&self.interner, param.ty).expect("invalid type"),
                hir::BindingType::Param,
            );
        }

        let mut hir_body: Vec<hir::Statement> = Vec::with_capacity(body.len());
        for stmt in body.iter() {
            let stmt = self.ast.get_stmt(*stmt).expect("invalid statement");
            hir_body.push(self.build_statement(&mut ctx, stmt));
        }

        hir::Function {
            name: signature.name,
            locals: ctx.locals,
            result,
            body: hir_body,
        }
    }

    fn build_statement(
        &self,
        ctx: &mut FunctionContext,
        statement: &ast::Statement,
    ) -> hir::Statement {
        match statement.kind {
            ast::StmtKind::Expression { expr } => {
                let hir_expr = self.build_expression(ctx, expr);
                let ty = match hir_expr.ty {
                    hir::Type::Comptime(_) => panic!("unused comptime expression"),
                    hir::Type::Runtime(ty) => ty,
                };
                match ty {
                    hir::RuntimeType::Unit => {}
                    _ => panic!("expression value me be consumed"),
                }
                hir::Statement::Expr {
                    ty: hir::RuntimeType::Unit,
                    expr: hir_expr,
                }
            }
            ast::StmtKind::ConstDefinition { name, ty, value } => {
                match ctx.local_lookup.get(&name).copied() {
                    Some(_) => panic!("variable already defined"),
                    None => {}
                }

                let hir_expr = self.build_expression(ctx, value);
                let local_type =
                    try_from_symbol_to_runtime_type(&self.interner, ty).expect("invalid type");
                match hir::Type::resolve(hir::Type::Runtime(local_type), hir_expr.ty) {
                    Ok(_) => {}
                    Err(_) => panic!("type mismatch in const definition"),
                }

                let local_index = ctx.push_local(name, local_type, hir::BindingType::Const);

                hir::Statement::Local {
                    index: local_index,
                    ty: local_type,
                    expr: hir_expr,
                }
            }
            ast::StmtKind::MutableDefinition { name, ty, value } => {
                match ctx.local_lookup.get(&name).copied() {
                    Some(_) => panic!("variable already defined"),
                    None => {}
                }

                let hir_expr = self.build_expression(ctx, value);
                let local_type =
                    try_from_symbol_to_runtime_type(&self.interner, ty).expect("invalid type");
                match hir::Type::resolve(hir::Type::Runtime(local_type), hir_expr.ty) {
                    Ok(_) => {}
                    Err(_) => panic!("type mismatch in const definition"),
                }

                let local_index = ctx.push_local(name, local_type, hir::BindingType::Mutable);
                hir::Statement::Local {
                    index: local_index,
                    ty: local_type,
                    expr: hir_expr,
                }
            }
            ast::StmtKind::Return { value } => {
                let hir_expr = self.build_expression(ctx, value);
                match hir::Type::resolve(hir::Type::Runtime(ctx.result), hir_expr.ty) {
                    Ok(_) => {}
                    Err(_) => panic!("type mismatch in return statement"),
                };
                hir::Statement::Return {
                    ty: ctx.result,
                    expr: hir_expr,
                }
            }
        }
    }

    fn build_assignment_expr(
        &self,
        ctx: &mut FunctionContext,
        left: ast::ExprId,
        right: ast::ExprId,
    ) -> hir::Expression {
        let left = self.build_expression(ctx, left);
        let right = self.build_expression(ctx, right);

        let local_index = match left.kind {
            hir::ExprKind::Local(index) => index,
            _ => panic!("left side of assignment must be a local variable"),
        };
        let local = match ctx.locals.get(local_index.0 as usize) {
            Some(local) => local,
            None => panic!("can't assing to undeclared variable"),
        };

        match local.binding {
            hir::BindingType::Mutable => {}
            hir::BindingType::Param => panic!("can't assign to parameter"),
            hir::BindingType::Const => panic!("can't assign to const variable"),
        }
        match hir::Type::resolve(left.ty, right.ty) {
            Ok(_) => {}
            Err(_) => panic!("type mismatch in assignment"),
        }

        hir::Expression {
            kind: hir::ExprKind::Binary {
                operator: ast::BinaryOperator::Assign,
                lhs: Box::new(left),
                rhs: Box::new(right),
            },
            ty: hir::Type::Runtime(hir::RuntimeType::Unit),
        }
    }

    fn build_expression(&self, ctx: &mut FunctionContext, expr_id: ast::ExprId) -> hir::Expression {
        let expr = self.ast.get_expr(expr_id).expect("Invalid expression");
        match expr.kind.clone() {
            ast::ExprKind::Int { value } => hir::Expression {
                kind: hir::ExprKind::Int(value),
                ty: hir::Type::Comptime(hir::ComptimeType::Int),
            },
            ast::ExprKind::Identifier { symbol } => {
                let local = match ctx.lookup_local(symbol) {
                    Some(local) => local,
                    None => panic!("undeclared variable"),
                };
                hir::Expression {
                    kind: hir::ExprKind::Local(local.index),
                    ty: hir::Type::Runtime(local.ty),
                }
            }
            ast::ExprKind::Binary {
                left: left_id,
                right: right_id,
                operator,
            } => {
                match operator {
                    ast::BinaryOperator::Assign => {
                        return self.build_assignment_expr(ctx, left_id, right_id);
                    }
                    _ => {}
                }
                let left = self.build_expression(ctx, left_id);
                let right = self.build_expression(ctx, right_id);

                let result = match hir::Type::resolve(left.ty, right.ty) {
                    Ok(ty) => ty,
                    _ => panic!("type mismatch in binary expression"),
                };

                hir::Expression {
                    kind: hir::ExprKind::Binary {
                        operator: operator.clone(),
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    },
                    ty: result,
                }
            }
            ast::ExprKind::Unary { operator, operand } => {
                let operand = self.build_expression(ctx, operand);
                let ty = operand.ty;
                hir::Expression {
                    kind: hir::ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty,
                }
            }
        }
    }
}
