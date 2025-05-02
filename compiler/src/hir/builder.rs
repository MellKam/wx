use core::panic;
use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use crate::{ast, hir};

pub struct Builder<'a> {
    ast: &'a ast::Ast,
    interner: &'a StringInterner<StringBackend<SymbolU32>>,
    function_lookup: HashMap<SymbolU32, (hir::FunctionIndex, hir::FunctionSignature)>,
    hir: hir::HIR,
}

#[derive(Debug)]
struct FunctionContext {
    pub signature: hir::FunctionSignature,
    pub locals: Vec<hir::Local>,
    pub local_lookup: HashMap<SymbolU32, hir::LocalIndex>,
}

impl From<hir::FunctionSignature> for FunctionContext {
    fn from(signature: hir::FunctionSignature) -> Self {
        let locals: Vec<hir::Local> = signature
            .params
            .iter()
            .map(|param| hir::Local {
                name: param.name,
                ty: param.ty,
                mutability: hir::Mutability::Const,
            })
            .collect();

        let mut local_lookup = HashMap::new();
        for (index, param) in locals.iter().enumerate() {
            local_lookup.insert(param.name, index as hir::LocalIndex);
        }

        FunctionContext {
            signature,
            locals,
            local_lookup,
        }
    }
}

impl FunctionContext {
    #[inline]
    fn lookup_local(&self, symbol: SymbolU32) -> Option<hir::Local> {
        let local_index = self.local_lookup.get(&symbol).copied()?;
        self.locals.get(local_index as usize).cloned()
    }

    #[inline]
    fn push_local(
        &mut self,
        name: SymbolU32,
        ty: hir::Type,
        mutability: hir::Mutability,
    ) -> hir::LocalIndex {
        self.locals.push(hir::Local {
            name,
            ty,
            mutability,
        });
        let index = self.locals.len() as hir::LocalIndex;
        self.local_lookup.insert(name, index);
        index
    }
}

impl<'a> Builder<'a> {
    pub fn build(
        ast: &'a ast::Ast,
        interner: &'a StringInterner<StringBackend<SymbolU32>>,
    ) -> hir::HIR {
        let mut builder = Builder {
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
                    builder.function_lookup.insert(
                        signature.name,
                        (
                            builder.function_lookup.len() as hir::FunctionIndex,
                            builder.convert_function_signature(signature),
                        ),
                    );
                }
                _ => {}
            }
        }

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition { body, signature } => {
                    let (_, hir_signature) = builder.function_lookup.get(&signature.name).unwrap();

                    builder.hir.functions.push(builder.build_function(
                        signature.name,
                        hir_signature,
                        body,
                    ));
                }
                _ => panic!("Expected function definition"),
            }
        }

        builder.hir
    }

    fn convert_function_signature(
        &self,
        signature: &ast::FunctionSignature,
    ) -> hir::FunctionSignature {
        let params = signature
            .params
            .iter()
            .map(|param| {
                let text = self.interner.resolve(param.ty).expect("invalid type");
                let ty = hir::Type::try_from(text).expect("invalid type");
                match ty {
                    hir::Type::ComptimeInt | hir::Type::Unit | hir::Type::Never => {
                        panic!("invalid param type")
                    }
                    _ => {}
                }

                hir::FunctionParam {
                    name: param.name,
                    ty,
                }
            })
            .collect();

        let result = match signature.output {
            Some(ty) => {
                let text = self.interner.resolve(ty).expect("invalid type");
                hir::Type::try_from(text).expect("invalid type")
            }
            None => hir::Type::Unit,
        };

        hir::FunctionSignature { params, result }
    }

    fn build_function(
        &self,
        name: SymbolU32,
        signature: &hir::FunctionSignature,
        body: &Vec<ast::StmtId>,
    ) -> hir::Function {
        let mut ctx = FunctionContext::from(signature.clone());

        let body = body
            .iter()
            .map(|stmt_id| {
                let stmt = self.ast.get_stmt(*stmt_id).expect("invalid statement");
                self.build_statement(&mut ctx, stmt)
            })
            .collect();

        hir::Function {
            name,
            locals: ctx.locals[ctx.signature.params.len()..].to_vec(),
            signature: ctx.signature,
            body,
        }
    }

    fn build_declaration_statement(
        &self,
        ctx: &mut FunctionContext,
        name: SymbolU32,
        ty: SymbolU32,
        value: ast::ExprId,
        mutability: hir::Mutability,
    ) -> hir::Statement {
        match ctx.local_lookup.get(&name).copied() {
            Some(_) => panic!("variable already defined"),
            None => {}
        }

        let value_expr = self.build_expression(ctx, value);
        let binding_type = hir::Type::try_from(self.interner.resolve(ty).expect("invalid type"))
            .expect("invalid type");
        match value_expr.ty.can_coerce_into(binding_type) {
            Ok(_) => {}
            Err(_) => panic!("type mismatch in assignment"),
        };

        let local_index = ctx.push_local(name, binding_type, mutability);

        hir::Statement::Local {
            index: local_index,
            expr: value_expr,
        }
    }

    fn build_statement(
        &self,
        ctx: &mut FunctionContext,
        statement: &ast::Statement,
    ) -> hir::Statement {
        match statement.kind {
            ast::StmtKind::Expression { expr } => {
                let expr = self.build_expression(ctx, expr);
                match expr.ty {
                    hir::Type::Unit => {}
                    hir::Type::Never => {}
                    _ => panic!("value must be consumed or dropped"),
                }
                hir::Statement::Expr { expr }
            }
            ast::StmtKind::ConstDefinition { name, ty, value } => {
                self.build_declaration_statement(ctx, name, ty, value, hir::Mutability::Const)
            }
            ast::StmtKind::MutableDefinition { name, ty, value } => {
                self.build_declaration_statement(ctx, name, ty, value, hir::Mutability::Mutable)
            }
            ast::StmtKind::Assignment { name, value } => {
                let name = match self.ast.get_expr(name).expect("invalid expression").kind {
                    ast::ExprKind::Identifier { symbol } => symbol,
                    _ => panic!("left side of assignment must be a local variable"),
                };

                let local_index = match ctx.local_lookup.get(&name).copied() {
                    Some(index) => index,
                    None => panic!("variable not defined"),
                };

                let value_expr = self.build_expression(ctx, value);
                let local = match ctx.locals.get(local_index as usize) {
                    Some(local) => local,
                    None => panic!("can't assign to undeclared variable"),
                };

                match local.mutability {
                    hir::Mutability::Mutable => {}
                    hir::Mutability::Const => panic!("can't assign to const variable"),
                }
                match value_expr.ty.can_coerce_into(local.ty.into()) {
                    Ok(_) => {}
                    Err(_) => panic!("type mismatch in assignment"),
                };

                hir::Statement::Assign {
                    index: local_index,
                    expr: value_expr,
                }
            }
            ast::StmtKind::Return { value } => {
                let value_expr = self.build_expression(ctx, value);
                match value_expr.ty.can_coerce_into(ctx.signature.result) {
                    Ok(_) => {}
                    Err(_) => panic!("type mismatch in assignment"),
                };

                hir::Statement::Return { expr: value_expr }
            }
        }
    }

    fn build_expression(&self, ctx: &mut FunctionContext, expr_id: ast::ExprId) -> hir::Expression {
        let expr = self.ast.get_expr(expr_id).expect("invalid expression");
        match expr.kind.clone() {
            ast::ExprKind::Int { value } => hir::Expression {
                kind: hir::ExprKind::Int(value),
                ty: hir::Type::ComptimeInt,
            },
            ast::ExprKind::Identifier { symbol } => {
                let local_index = match ctx.local_lookup.get(&symbol).copied() {
                    Some(index) => index,
                    None => panic!("undeclared binding"),
                };
                let local = match ctx.locals.get(local_index as usize).cloned() {
                    Some(local) => local,
                    None => panic!("undeclared binding"),
                };
                hir::Expression {
                    kind: hir::ExprKind::Local(local_index as hir::LocalIndex),
                    ty: local.ty.into(),
                }
            }
            ast::ExprKind::Binary {
                left: left_id,
                right: right_id,
                operator,
            } => Builder::build_binary_expression(
                operator,
                self.build_expression(ctx, left_id),
                self.build_expression(ctx, right_id),
            ),
            ast::ExprKind::Unary { operator, operand } => {
                Builder::build_unary_expression(operator, self.build_expression(ctx, operand))
            }
        }
    }

    fn build_unary_expression(
        operator: ast::UnaryOperator,
        operand: hir::Expression,
    ) -> hir::Expression {
        match operand.ty {
            hir::Type::ComptimeInt | hir::Type::I32 | hir::Type::I64 => {
                let ty = operand.ty;
                hir::Expression {
                    kind: hir::ExprKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    ty,
                }
            }
            _ => panic!("can't apply unary operator to this type"),
        }
    }

    fn build_binary_expression(
        operator: ast::BinaryOperator,
        left: hir::Expression,
        right: hir::Expression,
    ) -> hir::Expression {
        let ty = match (left.ty, right.ty) {
            (hir::Type::ComptimeInt, hir::Type::ComptimeInt) => hir::Type::ComptimeInt,
            (hir::Type::ComptimeInt, hir::Type::I32 | hir::Type::I64) => right.ty,
            (hir::Type::I32 | hir::Type::I64, hir::Type::ComptimeInt) => left.ty,
            (hir::Type::I32 | hir::Type::I64, hir::Type::I32 | hir::Type::I64) => left.ty,
            (hir::Type::Never, _) | (_, hir::Type::Never) => hir::Type::Never,
            _ => {
                panic!("type mismatch in binary expression");
            }
        };

        hir::Expression {
            kind: hir::ExprKind::Binary {
                operator,
                lhs: Box::new(left),
                rhs: Box::new(right),
            },
            ty,
        }
    }
}
