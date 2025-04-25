use core::panic;
use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::{
    FuncIndex, HIR, HIRExpr, HIRExprKind, HIRFunction, HIRLocal, HIRStmtKind, PrimitiveType,
};
use crate::ast::{self, BindingType, ExprId, ExprKind, StmtKind};
use crate::hir::{HIRStmt, LocalIndex};

pub struct HIRBuilder<'a> {
    ast: &'a ast::Ast,
    interner: &'a StringInterner<StringBackend<SymbolU32>>,
    function_lookup: HashMap<SymbolU32, FuncIndex>,
}

fn try_from_symbol_to_primitive(
    interner: &StringInterner<StringBackend<SymbolU32>>,
    symbol: SymbolU32,
) -> Option<PrimitiveType> {
    match interner.resolve(symbol)? {
        "()" => Some(PrimitiveType::Unit),
        "i32" => Some(PrimitiveType::I32),
        "i64" => Some(PrimitiveType::I64),
        _ => None,
    }
}

#[derive(Debug)]
struct HIRFunctionContext {
    locals: Vec<HIRLocal>,
    local_lookup: HashMap<SymbolU32, LocalIndex>,
    result: PrimitiveType,
}

impl HIRFunctionContext {
    #[inline]
    fn lookup_local(&self, symbol: SymbolU32) -> Option<HIRLocal> {
        let local_index = self.local_lookup.get(&symbol).copied()?;
        self.locals.get(local_index.0 as usize).copied()
    }

    #[inline]
    fn push_local(&mut self, id: SymbolU32, ty: PrimitiveType, binding: BindingType) -> LocalIndex {
        let local_index = LocalIndex(self.locals.len() as u32);
        self.locals.push(HIRLocal {
            index: local_index,
            ty,
            binding,
        });
        self.local_lookup.insert(id, local_index);
        local_index
    }
}

impl<'a> HIRBuilder<'a> {
    pub fn new(ast: &'a ast::Ast, interner: &'a StringInterner<StringBackend<SymbolU32>>) -> Self {
        HIRBuilder {
            ast,
            interner,
            function_lookup: HashMap::new(),
        }
    }

    pub fn build(&mut self) -> HIR {
        let mut hir = HIR {
            functions: Vec::new(),
        };

        for item in self.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition { signature, .. } => {
                    let index = FuncIndex(self.function_lookup.len() as u32);
                    self.function_lookup.insert(signature.id, index);
                }
                _ => panic!("Expected function definition"),
            }
        }

        for item in self.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition { signature, body } => {
                    hir.functions.push(self.build_function(signature, body));
                }
                _ => panic!("Expected function definition"),
            }
        }

        hir
    }

    fn build_function(
        &self,
        signature: &ast::FunctionSignature,
        body: &Vec<ast::StmtId>,
    ) -> HIRFunction {
        let result = match signature.result {
            Some(ty) => try_from_symbol_to_primitive(&self.interner, ty).expect("invalid type"),
            None => PrimitiveType::Unit,
        };

        let mut ctx = HIRFunctionContext {
            locals: Vec::with_capacity(signature.params.len()),
            local_lookup: HashMap::with_capacity(signature.params.len()),
            result,
        };

        for param in signature.params.iter() {
            ctx.push_local(
                param.id,
                try_from_symbol_to_primitive(&self.interner, param.ty).expect("Invalid type"),
                BindingType::Const,
            );
        }

        let mut hir_body: Vec<HIRStmt> = Vec::with_capacity(body.len());
        for stmt in body.iter() {
            let stmt = self.ast.get_stmt(*stmt).expect("invalid statement");
            hir_body.push(self.build_statement(&mut ctx, stmt));
        }

        HIRFunction {
            locals: ctx.locals,
            result,
            body: hir_body,
        }
    }

    fn build_statement(&self, ctx: &mut HIRFunctionContext, statement: &ast::Statement) -> HIRStmt {
        match statement.kind {
            StmtKind::Expression { expr } => {
                let hir_expr = self.build_expression(ctx, expr);
                HIRStmt {
                    kind: HIRStmtKind::Expr { expr: hir_expr },
                }
            }
            StmtKind::ConstDefinition { name, ty, value } => {
                match ctx.local_lookup.get(&name).copied() {
                    Some(_) => panic!("variable already defined"),
                    None => {}
                }

                let hir_expr = self.build_expression(ctx, value);
                let local_type =
                    try_from_symbol_to_primitive(&self.interner, ty).expect("invalid type");
                if hir_expr.ty != local_type {
                    panic!("type mismatch in const definition");
                }

                let local_index = ctx.push_local(name, local_type, BindingType::Const);

                HIRStmt {
                    kind: HIRStmtKind::Local {
                        local_index,
                        expr: hir_expr,
                    },
                }
            }
            StmtKind::MutableDefinition { name, ty, value } => {
                match ctx.local_lookup.get(&name).copied() {
                    Some(_) => panic!("variable already defined"),
                    None => {}
                }

                let hir_expr = self.build_expression(ctx, value);
                let local_type =
                    try_from_symbol_to_primitive(&self.interner, ty).expect("invalid type");
                if hir_expr.ty != local_type {
                    panic!("type mismatch in const definition");
                }

                let local_index = ctx.push_local(name, local_type, BindingType::Mutable);
                HIRStmt {
                    kind: HIRStmtKind::Local {
                        local_index,
                        expr: hir_expr,
                    },
                }
            }
            StmtKind::Return { value } => {
                let hir_expr = self.build_expression(ctx, value);
                if hir_expr.ty != ctx.result {
                    panic!("return type mismatch");
                }
                HIRStmt {
                    kind: HIRStmtKind::Return { expr: hir_expr },
                }
            }
            _ => panic!("unimplemented"),
        }
    }

    fn build_assignment_expr(
        &self,
        ctx: &mut HIRFunctionContext,
        left: ExprId,
        right: ExprId,
    ) -> HIRExpr {
        let left = self.build_expression(ctx, left);
        let right = self.build_expression(ctx, right);

        let local_index = match left.kind {
            HIRExprKind::Local(index) => index,
            _ => panic!("left side of assignment must be a local variable"),
        };
        let local = match ctx.locals.get(local_index.0 as usize).copied() {
            Some(local) => local,
            None => panic!("can't assing to undeclared variable"),
        };

        if local.binding == BindingType::Const {
            panic!("can't assign to const variable")
        }
        if left.ty != right.ty {
            panic!("type mismatch in assignment");
        }

        HIRExpr {
            kind: HIRExprKind::Binary {
                operator: ast::BinaryOperator::Assign,
                lhs: Box::new(left),
                rhs: Box::new(right),
            },
            ty: PrimitiveType::Unit,
        }
    }

    fn build_expression(&self, ctx: &mut HIRFunctionContext, expr_id: ExprId) -> HIRExpr {
        let expr = self.ast.get_expr(expr_id).expect("Invalid expression");
        match expr.kind.clone() {
            ExprKind::Int { value } => HIRExpr {
                kind: HIRExprKind::Int(value),
                ty: PrimitiveType::I32,
            },
            ExprKind::Identifier { symbol } => {
                let local = match ctx.lookup_local(symbol) {
                    Some(local) => local,
                    None => panic!("undeclared variable"),
                };
                HIRExpr {
                    kind: HIRExprKind::Local(local.index),
                    ty: local.ty,
                }
            }
            ExprKind::Binary {
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

                if left.ty != right.ty {
                    panic!("type mismatch in binary expression");
                }

                let ty = left.ty;
                HIRExpr {
                    kind: HIRExprKind::Binary {
                        operator: operator.clone(),
                        lhs: Box::new(left),
                        rhs: Box::new(right),
                    },
                    ty,
                }
            }
            ExprKind::Unary { operator, operand } => {
                let operand = self.build_expression(ctx, operand);
                let ty = operand.ty;
                HIRExpr {
                    kind: HIRExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty,
                }
            }
        }
    }
}
