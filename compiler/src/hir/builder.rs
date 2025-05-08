use core::panic;
use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::HIR;
use super::diagnostics::DiagnosticContext;
use crate::{ast, hir};

pub struct Builder<'a> {
    ast: &'a ast::Ast,
    interner: &'a StringInterner<StringBackend<SymbolU32>>,
    global: GlobalScope,
    diagnostics: Vec<hir::diagnostics::DiagnosticContext>,
}

#[derive(Debug, Clone, Copy)]
enum GlobalItem {
    Function(hir::FunctionIndex),
    Enum(hir::EnumIndex),
    EnumVariant {
        enum_index: hir::EnumIndex,
        variant_index: hir::EnumVariantIndex,
    },
}

#[derive(Debug)]
struct EnumLookup {
    pub name: SymbolU32,
    pub ty: hir::PrimitiveType,
    pub variants_lookup: HashMap<SymbolU32, hir::EnumVariantIndex>,
    pub variants: Vec<hir::EnumVariant>,
}

impl From<hir::Enum> for EnumLookup {
    fn from(enum_: hir::Enum) -> Self {
        Self {
            name: enum_.name,
            ty: enum_.ty,
            variants_lookup: enum_
                .variants
                .iter()
                .enumerate()
                .map(|(index, variant)| (variant.name, index as hir::EnumVariantIndex))
                .collect(),
            variants: enum_.variants,
        }
    }
}

#[derive(Debug)]
struct GlobalScope {
    pub functions: Vec<hir::FunctionType>,
    pub enums: Vec<EnumLookup>,
    pub lookup: HashMap<SymbolU32, GlobalItem>,
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
                let text = interner.resolve(param.ty.symbol).expect("invalid type");
                let ty = hir::PrimitiveType::try_from(text).expect("invalid type");
                match ty {
                    hir::PrimitiveType::Unit | hir::PrimitiveType::Never => {
                        panic!("invalid param type")
                    }
                    _ => {}
                }

                hir::Local {
                    name: param.name.symbol,
                    ty: hir::Type::Primitive(ty),
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

impl<'a> Builder<'a> {
    fn to_hir_type(&self, ty: SymbolU32) -> hir::Type {
        let text = self.interner.resolve(ty).expect("invalid type");
        match hir::PrimitiveType::try_from(text) {
            Ok(ty) => hir::Type::Primitive(ty),
            Err(_) => match self.global.lookup.get(&ty).copied() {
                Some(GlobalItem::Enum(index)) => hir::Type::Enum(index),
                Some(GlobalItem::EnumVariant { enum_index, .. }) => hir::Type::Enum(enum_index),
                ty => panic!("invalid type {:?}", ty),
            },
        }
    }

    fn build_function_type(&self, signature: &ast::FunctionSignature) -> hir::FunctionType {
        let params: Vec<hir::Type> = signature
            .params
            .iter()
            .map(|param| self.to_hir_type(param.ty.symbol))
            .collect();

        let result = match &signature.result {
            Some(ty) => self.to_hir_type(ty.symbol),
            None => hir::Type::Primitive(hir::PrimitiveType::Unit),
        };

        hir::FunctionType { params, result }
    }

    pub fn build(
        ast: &'a ast::Ast,
        interner: &'a StringInterner<StringBackend<SymbolU32>>,
    ) -> (hir::HIR, Vec<DiagnosticContext>) {
        let mut builder = Builder {
            ast,
            interner,
            global: GlobalScope {
                functions: Vec::new(),
                enums: Vec::new(),
                lookup: HashMap::new(),
            },
            diagnostics: Vec::new(),
        };

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition(def) => {
                    let index = builder.global.functions.len() as hir::FunctionIndex;
                    builder
                        .global
                        .functions
                        .push(builder.build_function_type(&def.signature));
                    builder
                        .global
                        .lookup
                        .insert(def.signature.name.symbol, GlobalItem::Function(index));
                }
                ast::ItemKind::Enum(item_enum) => {
                    let index = builder.global.enums.len() as hir::EnumIndex;
                    builder
                        .global
                        .enums
                        .push(builder.build_enum_lookup(item_enum));
                    builder
                        .global
                        .lookup
                        .insert(item_enum.name.symbol, GlobalItem::Enum(index));
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

        let hir = HIR {
            functions,
            enums: builder
                .global
                .enums
                .into_iter()
                .map(|enum_| hir::Enum {
                    name: enum_.name,
                    ty: enum_.ty,
                    variants: enum_.variants,
                })
                .collect(),
        };

        (hir, builder.diagnostics)
    }

    fn build_function_definition(
        &mut self,
        def: &ast::ItemFunctionDefinition,
    ) -> Result<hir::Function, ()> {
        let global_item = self
            .global
            .lookup
            .get(&def.signature.name.symbol)
            .copied()
            .unwrap();
        let func_index = match global_item {
            GlobalItem::Function(index) => index,
            _ => panic!("expected function"),
        };
        let ty = self
            .global
            .functions
            .get(func_index as usize)
            .expect("invalid function index")
            .clone();

        let block = self.build_function_root_block(def)?;

        Ok(hir::Function {
            export: def.export.is_some(),
            name: def.signature.name.symbol,
            ty,
            block,
        })
    }

    fn build_enum_lookup(&self, def: &ast::ItemEnum) -> EnumLookup {
        let ty = hir::PrimitiveType::try_from(
            self.interner.resolve(def.ty.symbol).expect("invalid type"),
        )
        .expect("invalid type");

        let variants: Vec<hir::EnumVariant> = def
            .variants
            .iter()
            .map(|variant| {
                let value_expr = self
                    .ast
                    .get_expr(variant.value)
                    .expect("invalid expression");
                let value = match &value_expr.kind {
                    ast::ExprKind::Int { value } => value.clone(),
                    kind => panic!("invalid enum variant value {:?}", kind),
                };

                hir::EnumVariant {
                    name: variant.name.symbol,
                    value,
                }
            })
            .collect();

        let variants_lookup: HashMap<SymbolU32, usize> = variants
            .iter()
            .enumerate()
            .map(|(index, variant)| (variant.name, index as hir::EnumVariantIndex))
            .collect();

        EnumLookup {
            name: def.name.symbol,
            ty,
            variants_lookup,
            variants,
        }
    }

    fn build_function_root_block(
        &mut self,
        def: &ast::ItemFunctionDefinition,
    ) -> Result<hir::Block, ()> {
        let global_item = self
            .global
            .lookup
            .get(&def.signature.name.symbol)
            .copied()
            .unwrap();
        let func_index = match global_item {
            GlobalItem::Function(index) => index,
            _ => panic!("expected function"),
        };
        let func_type = self
            .global
            .functions
            .get(func_index as usize)
            .expect("invalid function index")
            .clone();

        let mut scope = BlockScope::from_function_signature(self.interner, &def.signature);

        let statements = def
            .block
            .statements
            .iter()
            .map(|stmt_id| self.ast.get_stmt(*stmt_id).expect("invalid statement"))
            .filter_map(|stmt| self.build_statement(func_index, &mut scope, stmt).ok())
            .collect();

        let result = match def.block.result {
            Some(expr_id) => {
                let expr = self.build_expression(func_index, &mut scope, expr_id)?;

                match (func_type.result, expr.ty) {
                    (hir::Type::Primitive(ty), hir::Type::Comptime(ty2))
                        if ty2.coercible_to(ty) => {}
                    (hir::Type::Enum(index_1), hir::Type::Enum(index_2)) if index_1 == index_2 => {}
                    _ => panic!("invalid return type"),
                }

                Some(expr)
            }
            None => None,
        };

        Ok(hir::Block {
            locals: scope.locals,
            statements,
            result,
            ty: func_type.result,
        })
    }

    fn build_statement(
        &mut self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        statement: &ast::Statement,
    ) -> Result<hir::Statement, ()> {
        match statement.kind {
            ast::StmtKind::Expression { expr } => {
                let expr = self.build_expression(func_index, scope, expr)?;
                match expr.ty {
                    hir::Type::Primitive(hir::PrimitiveType::Unit) => {}
                    hir::Type::Primitive(hir::PrimitiveType::Never) => {}
                    _ => panic!("value must be consumed or dropped"),
                }
                Ok(hir::Statement::Expr { expr })
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
                let value_expr = self.build_expression(func_index, scope, value)?;
                let func_type = self
                    .global
                    .functions
                    .get(func_index as usize)
                    .expect("invalid function index");

                match (func_type.result, value_expr.ty) {
                    (hir::Type::Primitive(ty), hir::Type::Comptime(ty2))
                        if ty2.coercible_to(ty) => {}
                    (hir::Type::Enum(index_1), hir::Type::Enum(index_2)) if index_1 == index_2 => {}
                    _ => panic!("invalid return type"),
                }

                Ok(hir::Statement::Return { expr: value_expr })
            }
        }
    }

    fn build_declaration_statement(
        &mut self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        name: SymbolU32,
        ty: SymbolU32,
        value: ast::ExprId,
        mutability: hir::Mutability,
    ) -> Result<hir::Statement, ()> {
        match scope.local_lookup.get(&name).copied() {
            Some(_) => panic!("variable already defined"),
            None => {}
        }

        let value_expr = self.build_expression(func_index, scope, value)?;
        let binding_type = self.to_hir_type(ty);
        match (binding_type, value_expr.ty) {
            (hir::Type::Primitive(ty), hir::Type::Comptime(ty2)) if ty2.coercible_to(ty) => {}
            (hir::Type::Enum(index_1), hir::Type::Enum(index_2)) if index_1 == index_2 => {}
            _ => panic!("type mismatch in assignment"),
        }

        let local_index = scope.push_local(hir::Local {
            name,
            ty: binding_type,
            mutability,
        });

        Ok(hir::Statement::Decl {
            index: local_index,
            expr: value_expr,
        })
    }

    fn build_expression(
        &mut self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = self.ast.get_expr(expr_id).expect("invalid expression");
        match expr.kind.clone() {
            ast::ExprKind::Int { value } => Ok(hir::Expression {
                kind: hir::ExprKind::Int(value),
                ty: hir::Type::Comptime(hir::ComptimeType::Int),
            }),
            ast::ExprKind::Identifier { symbol } => {
                match self.interner.resolve(symbol).unwrap() {
                    "_" => {
                        return Ok(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: hir::Type::Primitive(hir::PrimitiveType::Unit),
                        });
                    }
                    _ => {}
                };

                match scope.local_lookup.get(&symbol).copied() {
                    Some(index) => {
                        let local = match scope.locals.get(index as usize).cloned() {
                            Some(local) => local,
                            None => panic!("undeclared binding"),
                        };
                        Ok(hir::Expression {
                            kind: hir::ExprKind::Local(index as hir::LocalIndex),
                            ty: local.ty,
                        })
                    }
                    None => {
                        let global_item = match self.global.lookup.get(&symbol).copied() {
                            Some(item) => item,
                            None => panic!("undeclared binding"),
                        };
                        match global_item {
                            GlobalItem::Enum(_) => panic!("can't use enum as expression"),
                            GlobalItem::EnumVariant {
                                enum_index,
                                variant_index,
                            } => Ok(hir::Expression {
                                kind: hir::ExprKind::EnumVariant {
                                    enum_index,
                                    variant_index,
                                },
                                ty: hir::Type::Enum(enum_index),
                            }),
                            GlobalItem::Function(func_index) => Ok(hir::Expression {
                                kind: hir::ExprKind::Function(func_index),
                                ty: hir::Type::Function(func_index),
                            }),
                        }
                    }
                }
            }
            ast::ExprKind::Binary {
                left: left_id,
                right: right_id,
                operator,
            } => {
                let left = self.build_expression(func_index, scope, left_id)?;
                let right = self.build_expression(func_index, scope, right_id)?;
                Ok(self.build_binary_expression(scope, operator, left, right))
            }
            ast::ExprKind::Unary { operator, operand } => Ok(Builder::build_unary_expression(
                operator,
                self.build_expression(func_index, scope, operand)?,
            )),
            ast::ExprKind::Call { callee, arguments } => {
                let callee = self.build_expression(func_index, scope, callee)?;
                let func_index = match callee.kind {
                    hir::ExprKind::Function(func_index) => func_index,
                    _ => panic!("can't call non-function"),
                };
                let func_type = match self.global.functions.get(func_index as usize).cloned() {
                    Some(func_type) => func_type,
                    None => panic!("invalid function index"),
                };
                let arguments = arguments
                    .iter()
                    .filter_map(|arg| self.build_expression(func_index, scope, *arg).ok())
                    .collect();

                Ok(hir::Expression {
                    kind: hir::ExprKind::Call {
                        callee: Box::new(callee),
                        arguments,
                    },
                    ty: func_type.result,
                })
            }
            ast::ExprKind::NamespaceMember { namespace, member } => {
                self.build_namespace_member_expression(namespace, member)
            }
        }
    }

    fn build_namespace_member_expression(
        &mut self,
        namespace: ast::ExprId,
        member: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let namespace_expr = self.ast.get_expr(namespace).expect("invalid expression");
        let enum_index = match namespace_expr.kind {
            ast::ExprKind::Identifier { symbol } => {
                let global_item = match self.global.lookup.get(&symbol).copied() {
                    Some(item) => item,
                    None => panic!("undeclared namespace"),
                };
                match global_item {
                    GlobalItem::Enum(enum_index) => enum_index,
                    _ => panic!("undeclared namespace"),
                }
            }
            _ => panic!("invalid namespace"),
        };

        let member_expr = self.ast.get_expr(member).expect("invalid expression");
        let variant_index = match member_expr.kind {
            ast::ExprKind::Identifier { symbol } => {
                let enum_ = self
                    .global
                    .enums
                    .get(enum_index as usize)
                    .expect("invalid enum");
                let variant_index = match enum_.variants_lookup.get(&symbol).copied() {
                    Some(index) => index,
                    None => {
                        self.diagnostics
                            .push(DiagnosticContext::UnknownEnumVariant {
                                file_id: self.ast.file_id,
                                enum_index,
                                span: member_expr.span,
                            });

                        return Ok(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: hir::Type::Enum(enum_index),
                        });
                    }
                };

                variant_index
            }
            _ => panic!("invalid enum variant"),
        };

        Ok(hir::Expression {
            kind: hir::ExprKind::EnumVariant {
                enum_index,
                variant_index,
            },
            ty: hir::Type::Enum(enum_index),
        })
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
        use hir::*;
        let ty = match operator {
            ast::BinaryOperator::Assign => {
                let local_index = match left.kind {
                    ExprKind::Local(index) => index,
                    ExprKind::Placeholder => {
                        match right.ty {
                            Type::Comptime(_) => panic!("can't drop comptime type"),
                            _ => {}
                        }
                        return Expression {
                            kind: ExprKind::Binary {
                                operator,
                                lhs: Box::new(Expression {
                                    kind: ExprKind::Placeholder,
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
                    Mutability::Mutable => {}
                    Mutability::Const => panic!("can't assign to const variable"),
                }
                match (local.ty, right.ty) {
                    (hir::Type::Primitive(ty), hir::Type::Comptime(ty2))
                        if ty2.coercible_to(ty) => {}
                    (hir::Type::Enum(index_1), hir::Type::Enum(index_2)) if index_1 == index_2 => {}
                    _ => panic!("invalid return type"),
                }

                Type::Primitive(PrimitiveType::Unit)
            }
            _ => self.resolve_binary_result(operator, left.ty.clone(), right.ty.clone()),
        };

        Expression {
            kind: ExprKind::Binary {
                operator,
                lhs: Box::new(left),
                rhs: Box::new(right),
            },
            ty,
        }
    }

    fn resolve_binary_result(
        &self,
        operator: ast::BinaryOperator,
        left: hir::Type,
        right: hir::Type,
    ) -> hir::Type {
        use ast::BinaryOperator::*;
        use hir::{ComptimeType, PrimitiveType, Type};

        match operator {
            Add | Subtract | Multiply => match (left, right) {
                // ComptimeInt
                (Type::Comptime(ComptimeType::Int), Type::Comptime(ComptimeType::Int)) => {
                    Type::Comptime(ComptimeType::Int)
                }

                // I32
                (Type::Primitive(PrimitiveType::I32), right_type)
                    if right_type.coercible_to(PrimitiveType::I32) =>
                {
                    Type::Primitive(PrimitiveType::I32)
                }
                (left_type, Type::Primitive(PrimitiveType::I32))
                    if left_type.coercible_to(PrimitiveType::I32) =>
                {
                    Type::Primitive(PrimitiveType::I32)
                }

                // I64
                (Type::Primitive(PrimitiveType::I64), right_type)
                    if right_type.coercible_to(PrimitiveType::I64) =>
                {
                    Type::Primitive(PrimitiveType::I64)
                }
                (left_type, Type::Primitive(PrimitiveType::I64))
                    if left_type.coercible_to(PrimitiveType::I64) =>
                {
                    Type::Primitive(PrimitiveType::I64)
                }
                _ => panic!("type mismatch in binary expression"),
            },
            Eq => {
                let global_item = self
                    .global
                    .lookup
                    .get(&self.interner.get("bool").unwrap())
                    .copied()
                    .unwrap();
                let bool_enum_id = match global_item {
                    GlobalItem::Enum(enum_index) => enum_index,
                    _ => panic!("expected bool enum from std"),
                };

                match (left, right) {
                    (Type::Primitive(PrimitiveType::I32), Type::Primitive(PrimitiveType::I32)) => {
                        Type::Enum(bool_enum_id)
                    }
                    (Type::Primitive(PrimitiveType::I64), Type::Primitive(PrimitiveType::I64)) => {
                        Type::Enum(bool_enum_id)
                    }
                    (Type::Comptime(ComptimeType::Int), Type::Comptime(ComptimeType::Int)) => {
                        Type::Enum(bool_enum_id)
                    }
                    (Type::Enum(index_1), Type::Enum(index_2)) if index_1 == index_2 => {
                        Type::Enum(bool_enum_id)
                    }
                    _ => panic!("type mismatch in binary expression"),
                }
            }
            _ => unimplemented!("operator not implemented"),
        }
    }
}
