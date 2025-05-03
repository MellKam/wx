use core::panic;
use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use crate::{ast, hir};

pub struct Builder<'a> {
    ast: &'a ast::Ast,
    interner: &'a StringInterner<StringBackend<SymbolU32>>,
    functions: Vec<hir::FunctionType>,
    function_lookup: HashMap<SymbolU32, hir::FunctionIndex>,
    hir: hir::HIR,
}

#[derive(Debug)]
struct BlockScope {
    pub locals: Vec<hir::Local>,
    pub local_lookup: HashMap<SymbolU32, hir::LocalIndex>,
}

impl BlockScope {
    fn from_function_signature(
        interner: &StringInterner<StringBackend>,
        signature: &ast::FunctionSignature,
    ) -> Self {
        let params: Vec<hir::Local> = signature
            .params
            .iter()
            .map(|param| {
                let text = interner.resolve(param.ty).expect("invalid type");
                let ty = hir::PrimitiveType::try_from(text).expect("invalid type");
                match ty {
                    hir::PrimitiveType::Unit | hir::PrimitiveType::Never => {
                        panic!("invalid param type")
                    }
                    _ => {}
                }

                hir::Local {
                    name: param.name,
                    ty,
                    mutability: hir::Mutability::Const,
                }
            })
            .collect();

        let mut local_lookup = HashMap::with_capacity(params.len());
        for (index, param) in params.iter().enumerate() {
            local_lookup.insert(param.name, index as hir::LocalIndex);
        }

        Self {
            locals: params,
            local_lookup,
        }
    }

    #[inline]
    fn push_local(&mut self, local: hir::Local) -> hir::LocalIndex {
        let index = self.locals.len() as hir::LocalIndex;
        self.locals.push(local.clone());
        self.local_lookup.insert(local.name, index);
        index
    }
}

impl hir::FunctionType {
    fn from_signature(
        interner: &StringInterner<StringBackend>,
        signature: &ast::FunctionSignature,
    ) -> Self {
        let params: Vec<hir::PrimitiveType> = signature
            .params
            .iter()
            .map(|param| {
                let text = interner.resolve(param.ty).expect("invalid type");
                hir::PrimitiveType::try_from(text).expect("invalid type")
            })
            .collect();

        let result = match signature.result {
            Some(ty) => {
                let text = interner.resolve(ty).expect("invalid type");
                hir::PrimitiveType::try_from(text).expect("invalid type")
            }
            None => hir::PrimitiveType::Unit,
        };

        hir::FunctionType { params, result }
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
            functions: Vec::new(),
            hir: hir::HIR {
                functions: Vec::new(),
            },
        };

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition(def) => {
                    let index = builder.functions.len() as hir::FunctionIndex;
                    builder.functions.push(hir::FunctionType::from_signature(
                        builder.interner,
                        &def.signature,
                    ));
                    builder.function_lookup.insert(def.signature.name, index);
                }
                _ => {}
            }
        }

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition(def) => {
                    builder.hir.functions.push(builder.build_function(def));
                }
                _ => panic!("Expected function definition"),
            }
        }

        builder.hir
    }

    fn build_function(&self, def: &ast::FunctionDefinition) -> hir::Function {
        let func_index = self
            .function_lookup
            .get(&def.signature.name)
            .copied()
            .unwrap();
        let func_type = self
            .functions
            .get(func_index as usize)
            .expect("invalid function index");
        let mut scope = BlockScope::from_function_signature(self.interner, &def.signature);

        let body = def
            .body
            .iter()
            .map(|stmt_id| {
                let stmt = self.ast.get_stmt(*stmt_id).expect("invalid statement");
                self.build_statement(func_index, &mut scope, stmt)
            })
            .collect();

        hir::Function {
            export: def.export.is_some(),
            name: def.signature.name,
            signature: func_type.clone(),
            locals: scope.locals,
            body,
        }
    }

    fn build_statement(
        &self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        statement: &ast::Statement,
    ) -> hir::Statement {
        match statement.kind {
            ast::StmtKind::Expression { expr } => {
                let expr = self.build_expression(func_index, scope, expr);
                match expr.ty {
                    hir::Type::Primitive(hir::PrimitiveType::Unit) => {}
                    hir::Type::Primitive(hir::PrimitiveType::Never) => {}
                    _ => panic!("value must be consumed or dropped"),
                }
                hir::Statement::Expr { expr }
            }
            ast::StmtKind::ConstDefinition { name, ty, value } => {
                let ty = match ty {
                    None => panic!("variable type must be specified"),
                    Some(ty) => ty,
                };
                self.build_declaration_statement(
                    func_index,
                    scope,
                    name,
                    ty,
                    value,
                    hir::Mutability::Const,
                )
            }
            ast::StmtKind::MutableDefinition { name, ty, value } => {
                let ty = match ty {
                    None => panic!("variable type must be specified"),
                    Some(ty) => ty,
                };
                self.build_declaration_statement(
                    func_index,
                    scope,
                    name,
                    ty,
                    value,
                    hir::Mutability::Mutable,
                )
            }
            ast::StmtKind::Return { value } => {
                let value_expr = self.build_expression(func_index, scope, value);
                let func_type = self
                    .functions
                    .get(func_index as usize)
                    .expect("invalid function index");
                match value_expr.ty {
                    hir::Type::Comptime(ty) if ty.can_coerce_into(func_type.result) => {}
                    hir::Type::Primitive(ty) if ty == func_type.result => {}
                    _ => panic!("invalid return type"),
                }

                hir::Statement::Return { expr: value_expr }
            }
        }
    }

    fn build_declaration_statement(
        &self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        name: SymbolU32,
        ty: SymbolU32,
        value: ast::ExprId,
        mutability: hir::Mutability,
    ) -> hir::Statement {
        match scope.local_lookup.get(&name).copied() {
            Some(_) => panic!("variable already defined"),
            None => {}
        }

        let value_expr = self.build_expression(func_index, scope, value);
        let binding_type =
            hir::PrimitiveType::try_from(self.interner.resolve(ty).expect("invalid type"))
                .expect("invalid type");
        match value_expr.ty {
            hir::Type::Comptime(ty) if ty.can_coerce_into(binding_type) => {}
            hir::Type::Primitive(ty) if ty == binding_type => {}
            _ => panic!("type mismatch in assignment"),
        };

        let local_index = scope.push_local(hir::Local {
            name,
            ty: binding_type,
            mutability,
        });

        hir::Statement::Decl {
            index: local_index,
            expr: value_expr,
        }
    }

    fn build_expression(
        &self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        expr_id: ast::ExprId,
    ) -> hir::Expression {
        let expr = self.ast.get_expr(expr_id).expect("invalid expression");
        match expr.kind.clone() {
            ast::ExprKind::Int { value } => hir::Expression {
                kind: hir::ExprKind::Int(value),
                ty: hir::Type::Comptime(hir::ComptimeType::Int),
            },
            ast::ExprKind::Identifier { symbol } => {
                match self.interner.resolve(symbol).unwrap() {
                    "_" => {
                        return hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: hir::Type::Primitive(hir::PrimitiveType::Unit),
                        };
                    }
                    _ => {}
                };

                match scope.local_lookup.get(&symbol).copied() {
                    Some(index) => {
                        let local = match scope.locals.get(index as usize).cloned() {
                            Some(local) => local,
                            None => panic!("undeclared binding"),
                        };
                        hir::Expression {
                            kind: hir::ExprKind::Local(index as hir::LocalIndex),
                            ty: hir::Type::Primitive(local.ty),
                        }
                    }
                    None => {
                        let func_index = match self.function_lookup.get(&symbol).copied() {
                            Some(index) => index,
                            None => panic!("undeclared binding"),
                        };
                        let func_type = match self.functions.get(func_index as usize).cloned() {
                            Some(func_type) => func_type,
                            None => panic!("invalid function index"),
                        };
                        hir::Expression {
                            kind: hir::ExprKind::Function(func_index),
                            ty: hir::Type::Function(func_type),
                        }
                    }
                }
            }
            ast::ExprKind::Binary {
                left: left_id,
                right: right_id,
                operator,
            } => {
                let left = self.build_expression(func_index, scope, left_id);
                let right = self.build_expression(func_index, scope, right_id);
                self.build_binary_expression(scope, operator, left, right)
            }
            ast::ExprKind::Unary { operator, operand } => Builder::build_unary_expression(
                operator,
                self.build_expression(func_index, scope, operand),
            ),
            ast::ExprKind::Call { callee, arguments } => {
                let callee = self.build_expression(func_index, scope, callee);
                let func_index = match callee.kind {
                    hir::ExprKind::Function(func_index) => func_index,
                    _ => panic!("can't call non-function"),
                };
                let func_type = match self.functions.get(func_index as usize).cloned() {
                    Some(func_type) => func_type,
                    None => panic!("invalid function index"),
                };
                let arguments = arguments
                    .iter()
                    .map(|arg| self.build_expression(func_index, scope, *arg))
                    .collect();

                hir::Expression {
                    kind: hir::ExprKind::Call {
                        callee: Box::new(callee),
                        arguments,
                    },
                    ty: hir::Type::Primitive(func_type.result),
                }
            }
        }
    }

    fn build_unary_expression(
        operator: ast::UnaryOperator,
        operand: hir::Expression,
    ) -> hir::Expression {
        match operand.ty {
            hir::Type::Comptime(hir::ComptimeType::Int)
            | hir::Type::Primitive(hir::PrimitiveType::I32)
            | hir::Type::Primitive(hir::PrimitiveType::I64) => {
                let ty = operand.ty.clone();
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
        &self,
        scope: &mut BlockScope,
        operator: ast::BinaryOperator,
        left: hir::Expression,
        right: hir::Expression,
    ) -> hir::Expression {
        use hir::{ComptimeType, PrimitiveType, Type};
        let ty = match operator {
            ast::BinaryOperator::Assign => {
                let local_index = match left.kind {
                    hir::ExprKind::Local(index) => index,
                    hir::ExprKind::Placeholder => {
                        match right.ty {
                            Type::Comptime(_) => panic!("can't drop comptime type"),
                            _ => {}
                        }
                        return hir::Expression {
                            kind: hir::ExprKind::Binary {
                                operator,
                                lhs: Box::new(hir::Expression {
                                    kind: hir::ExprKind::Placeholder,
                                    ty: right.ty.clone(),
                                }),
                                rhs: Box::new(right),
                            },
                            ty: Type::Primitive(PrimitiveType::Unit),
                        };
                    }
                    _ => panic!("left side of assignment must be a variable"),
                };
                let local = match scope.locals.get(local_index as usize) {
                    Some(local) => local,
                    None => panic!("can't assign to undeclared variable"),
                };
                match local.mutability {
                    hir::Mutability::Mutable => {}
                    hir::Mutability::Const => panic!("can't assign to const variable"),
                }
                match right.ty {
                    Type::Comptime(ty) if ty.can_coerce_into(local.ty) => {}
                    Type::Primitive(ty) if ty == local.ty => {}
                    _ => panic!("type mismatch in assignment"),
                }

                Type::Primitive(PrimitiveType::Unit)
            }
            _ => match (left.ty.clone(), right.ty.clone()) {
                (Type::Comptime(ComptimeType::Int), Type::Comptime(ComptimeType::Int)) => {
                    Type::Comptime(ComptimeType::Int)
                }
                (
                    Type::Comptime(ComptimeType::Int),
                    Type::Primitive(PrimitiveType::I32 | PrimitiveType::I64),
                ) => right.ty.clone(),
                (
                    Type::Primitive(PrimitiveType::I32) | Type::Primitive(PrimitiveType::I64),
                    Type::Comptime(ComptimeType::Int),
                ) => left.ty.clone(),
                (Type::Primitive(PrimitiveType::I32), Type::Primitive(PrimitiveType::I32)) => {
                    Type::Primitive(PrimitiveType::I32)
                }
                (Type::Primitive(PrimitiveType::I64), Type::Primitive(PrimitiveType::I64)) => {
                    Type::Primitive(PrimitiveType::I64)
                }
                _ => {
                    panic!("type mismatch in binary expression");
                }
            },
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
