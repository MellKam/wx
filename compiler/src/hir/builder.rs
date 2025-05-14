use core::panic;
use std::collections::HashMap;

use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use super::diagnostics::DiagnosticContext;
use super::{EnumIndex, HIR};
use crate::ast::Identifier;
use crate::{ast, hir};

pub struct Builder<'a> {
    ast: &'a ast::Ast,
    interner: &'a StringInterner<StringBackend<SymbolU32>>,
    global: GlobalScope,
    diagnostics: Vec<hir::diagnostics::DiagnosticContext>,
}

#[derive(Debug, Clone)]
enum GlobalValue {
    Function(hir::FunctionIndex),
    EnumVariant {
        enum_index: hir::EnumIndex,
        variant_index: hir::EnumVariantIndex,
    },
}

#[derive(Debug, Clone)]
enum GlobalType {
    Enum(hir::EnumIndex),
}

#[derive(Debug)]
struct EnumWithLookup {
    pub name: SymbolU32,
    pub type_: hir::PrimitiveType,
    pub variants_lookup: HashMap<SymbolU32, hir::EnumVariantIndex>,
    pub variants: Vec<hir::EnumVariant>,
}

#[derive(Debug)]
struct GlobalScope {
    pub functions: Vec<hir::FunctionType>,
    pub enums: Vec<EnumWithLookup>,
    pub type_lookup: HashMap<SymbolU32, GlobalType>,
    pub value_lookup: HashMap<SymbolU32, GlobalValue>,
}

impl From<hir::Enum> for EnumWithLookup {
    fn from(enum_: hir::Enum) -> Self {
        Self {
            name: enum_.name,
            type_: enum_.ty,
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
struct BlockScope {
    pub locals: Vec<hir::Local>,
    pub local_lookup: HashMap<SymbolU32, hir::LocalIndex>,
}

impl BlockScope {
    #[inline]
    fn push_local(&mut self, local: hir::Local) -> hir::LocalIndex {
        let index = self.locals.len() as hir::LocalIndex;
        self.locals.push(local.clone());
        self.local_lookup.insert(local.name, index);
        index
    }
}

impl<'a> Builder<'a> {
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
                type_lookup: HashMap::new(),
                value_lookup: HashMap::new(),
            },
            diagnostics: Vec::new(),
        };

        for item in builder.ast.items.iter() {
            match &item.kind {
                ast::ItemKind::FunctionDefinition(def) => {
                    let index = builder.global.functions.len() as hir::FunctionIndex;
                    let func_type = match builder.create_function_type(&def.signature) {
                        Ok(func_type) => func_type,
                        Err(_) => {
                            builder
                                .diagnostics
                                .push(DiagnosticContext::UnknownEnumVariant {
                                    file_id: builder.ast.file_id,
                                    enum_index: 0,
                                    span: item.span,
                                });
                            continue;
                        }
                    };

                    builder.global.functions.push(func_type);
                    builder
                        .global
                        .value_lookup
                        .insert(def.signature.name.symbol, GlobalValue::Function(index));
                }
                ast::ItemKind::Enum(item_enum) => {
                    let enum_index = builder.global.enums.len() as hir::EnumIndex;
                    let enum_ = builder.build_enum_with_lookup(item_enum, enum_index);
                    builder.global.enums.push(enum_);
                    builder
                        .global
                        .type_lookup
                        .insert(item_enum.name.symbol, GlobalType::Enum(enum_index));
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
                    ty: enum_.type_,
                    variants: enum_.variants,
                })
                .collect(),
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
            Some(GlobalType::Enum(index)) => return hir::Type::Enum(index),
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

    fn create_function_scope(
        &mut self,
        signature: &ast::FunctionSignature,
    ) -> Result<BlockScope, ()> {
        let locals: Vec<hir::Local> = signature
            .params
            .iter()
            .map(|param| hir::Local {
                name: param.name.symbol,
                ty: self.resolve_type(param.ty.clone()),
                mutability: hir::Mutability::Const,
            })
            .collect();

        let local_lookup: HashMap<SymbolU32, hir::LocalIndex> = locals
            .iter()
            .enumerate()
            .map(|(index, param)| (param.name, index as hir::LocalIndex))
            .collect();

        Ok(BlockScope {
            locals,
            local_lookup,
        })
    }

    fn build_enum_with_lookup(
        &mut self,
        def: &ast::ItemEnum,
        enum_index: EnumIndex,
    ) -> EnumWithLookup {
        let type_ = match self.resolve_type(def.ty.clone()) {
            hir::Type::Primitive(ty) => ty,
            hir::Type::Unknown => hir::PrimitiveType::I32,
            type_ => {
                self.diagnostics
                    .push(DiagnosticContext::InvalidEnumRepresentation {
                        file_id: self.ast.file_id,
                        enum_index,
                        type_,
                        span: def.ty.span,
                    });

                hir::PrimitiveType::I32
            }
        };

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
                    _ => {
                        let expr = self
                            .ast
                            .get_expr(variant.value)
                            .expect("invalid expression");
                        let diagnostic = DiagnosticContext::InvalidEnumValue {
                            file_id: self.ast.file_id,
                            enum_index,
                            span: expr.span,
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

        let variants_lookup: HashMap<SymbolU32, usize> = variants
            .iter()
            .enumerate()
            .map(|(index, variant)| (variant.name, index as hir::EnumVariantIndex))
            .collect();

        EnumWithLookup {
            name: def.name.symbol,
            type_,
            variants_lookup,
            variants,
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
            GlobalValue::Function(index) => index,
            _ => panic!("expected function index"),
        };
        let func_type = self
            .global
            .functions
            .get(func_index as usize)
            .unwrap()
            .clone();

        let mut scope = self.create_function_scope(&def.signature)?;

        let mut expressions: Vec<hir::Expression> = def
            .block
            .statements
            .iter()
            .map(|&stmt_id| self.ast.get_stmt(stmt_id).unwrap())
            .map(|stmt| self.build_statement(func_index, &mut scope, stmt))
            .collect::<Result<_, _>>()?;

        match def.block.result {
            Some(expr_id) => {
                let expr = self.build_expression(func_index, &mut scope, expr_id)?;

                let coerced = self.coerce_expr(expr, expr_id, func_type.result)?;
                expressions.push(coerced);
            }
            None => {}
        };

        let block = hir::Block {
            locals: scope.locals,
            expressions,
            ty: func_type.result,
        };
        Ok(hir::Function {
            ty: func_type,
            name: def.signature.name.symbol,
            export: def.export.is_some(),
            block,
        })
    }

    fn build_statement(
        &mut self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        statement: &ast::Statement,
    ) -> Result<hir::Expression, ()> {
        match &statement.kind {
            ast::StmtKind::DelimitedExpression { value } => {
                let expr = self.build_expression(func_index, scope, *value)?;
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
            ast::StmtKind::ConstDefinition { name, ty, value } => self.build_declaration_statement(
                func_index,
                scope,
                name.clone(),
                ty.clone(),
                *value,
                hir::Mutability::Const,
            ),
            ast::StmtKind::MutableDefinition { name, ty, value } => self
                .build_declaration_statement(
                    func_index,
                    scope,
                    name.clone(),
                    ty.clone(),
                    *value,
                    hir::Mutability::Mutable,
                ),
        }
    }

    fn build_declaration_statement(
        &mut self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        name: Identifier,
        ty: Option<Identifier>,
        value: ast::ExprId,
        mutability: hir::Mutability,
    ) -> Result<hir::Expression, ()> {
        match scope.local_lookup.get(&name.symbol).copied() {
            Some(_) => panic!("variable already defined"),
            None => {}
        }

        let value_expr = self.build_expression(func_index, scope, value)?;
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

        let local_index = scope.push_local(hir::Local {
            name: name.symbol,
            ty: binding_type,
            mutability,
        });

        Ok(hir::Expression {
            kind: hir::ExprKind::LocalDeclaration {
                index: local_index,
                expr: Box::new(coerced_rhs),
            },
            ty: Some(binding_type),
        })
    }

    fn build_identifier_expression(
        &mut self,
        scope: &BlockScope,
        expr_id: ast::ExprId,
        symbol: SymbolU32,
    ) -> Result<hir::Expression, ()> {
        match self.interner.resolve(symbol).unwrap() {
            "_" => {
                return Ok(hir::Expression {
                    kind: hir::ExprKind::Placeholder,
                    ty: Some(hir::Type::Unit),
                });
            }
            _ => {}
        };

        match scope.local_lookup.get(&symbol).copied() {
            Some(index) => {
                let local = match scope.locals.get(index as usize).cloned() {
                    Some(local) => local,
                    None => unreachable!(),
                };
                return Ok(hir::Expression {
                    kind: hir::ExprKind::Local(index as hir::LocalIndex),
                    ty: Some(local.ty),
                });
            }
            None => {}
        }

        match self.global.value_lookup.get(&symbol).cloned() {
            Some(value) => match value {
                GlobalValue::Function(func_index) => {
                    return Ok(hir::Expression {
                        kind: hir::ExprKind::Function(func_index),
                        ty: Some(hir::Type::Function(func_index)),
                    });
                }
                GlobalValue::EnumVariant {
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
        namespace: ast::Identifier,
        member: ast::Identifier,
    ) -> Result<hir::Expression, ()> {
        let global_type = match self.global.type_lookup.get(&namespace.symbol).cloned() {
            Some(item) => item,
            None => panic!("undeclared namespace"),
        };
        let enum_index = match global_type {
            GlobalType::Enum(enum_index) => enum_index,
        };

        let enum_ = self
            .global
            .enums
            .get(enum_index as usize)
            .expect("invalid enum");
        let variant_index = match enum_.variants_lookup.get(&member.symbol).copied() {
            Some(index) => index,
            None => {
                self.diagnostics
                    .push(DiagnosticContext::UnknownEnumVariant {
                        file_id: self.ast.file_id,
                        enum_index,
                        span: member.span,
                    });

                // TODO: how would this work with empty enum?
                0 // fallback value
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

    fn build_expression(
        &mut self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        expr_id: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = self.ast.get_expr(expr_id).unwrap();
        match &expr.kind {
            ast::ExprKind::Int { value } => Ok(hir::Expression {
                kind: hir::ExprKind::Int(value.clone()),
                ty: None,
            }),
            ast::ExprKind::Identifier { symbol } => {
                self.build_identifier_expression(scope, expr_id, *symbol)
            }
            ast::ExprKind::Binary {
                left: left_id,
                right: right_id,
                operator,
            } => {
                let left = self.build_expression(func_index, scope, *left_id)?;
                let right = self.build_expression(func_index, scope, *right_id)?;
                Ok(
                    self.build_binary_expression(
                        scope, *operator, left, *left_id, right, *right_id,
                    ),
                )
            }
            ast::ExprKind::Unary { operator, operand } => Ok(Builder::build_unary_expression(
                *operator,
                self.build_expression(func_index, scope, *operand)?,
            )),
            ast::ExprKind::Call { callee, arguments } => {
                self.build_call_expression(func_index, scope, *callee, &arguments)
            }
            ast::ExprKind::NamespaceMember { namespace, member } => {
                self.build_namespace_member_expression(namespace.clone(), member.clone())
            }
            ast::ExprKind::Return { value } => {
                self.build_return_expression(func_index, scope, *value)
            }
            _ => unimplemented!(),
        }
    }

    fn build_return_expression(
        &mut self,
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        value: ast::ExprId,
    ) -> Result<hir::Expression, ()> {
        let expr = self.build_expression(func_index, scope, value)?;
        let func_result = match self.global.functions.get(func_index as usize) {
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
        func_index: hir::FunctionIndex,
        scope: &mut BlockScope,
        callee_id: ast::ExprId,
        arguments: &Vec<ast::ExprId>,
    ) -> Result<hir::Expression, ()> {
        let callee = self.build_expression(func_index, scope, callee_id)?;
        let func_index = match callee.kind {
            hir::ExprKind::Function(func_index) => func_index,
            _ => {
                let callee_ast_expr = self.ast.get_expr(callee_id).unwrap();
                self.diagnostics
                    .push(DiagnosticContext::NonCallableIdentifier {
                        file_id: self.ast.file_id,
                        span: callee_ast_expr.span,
                    });
                return Err(());
            }
        };

        let func_type = match self.global.functions.get(func_index as usize).cloned() {
            Some(func_type) => func_type,
            None => panic!("invalid function index"),
        };
        let arguments: Vec<_> = arguments
            .iter()
            .enumerate()
            .map(|(index, arg)| {
                let expr = self.build_expression(func_index, scope, *arg)?;

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
        scope: &mut BlockScope,
        left: hir::Expression,
        right: hir::Expression,
        right_id: ast::ExprId,
    ) -> hir::Expression {
        let local_index = match left.kind {
            hir::ExprKind::Local(index) => index,
            hir::ExprKind::Placeholder => {
                return hir::Expression {
                    kind: hir::ExprKind::Binary {
                        operator: ast::BinaryOperator::Assign,
                        lhs: Box::new(hir::Expression {
                            kind: hir::ExprKind::Placeholder,
                            ty: right.ty.clone(),
                        }),
                        rhs: Box::new(right),
                    },
                    ty: Some(hir::Type::Unit),
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

        let coerced_rhs = self
            .coerce_expr(right, right_id, local.ty)
            .expect("invalid assignment type");

        hir::Expression {
            kind: hir::ExprKind::Binary {
                lhs: Box::new(left),
                operator: ast::BinaryOperator::Assign,
                rhs: Box::new(coerced_rhs),
            },
            ty: Some(hir::Type::Unit),
        }
    }

    fn coerce_expr(
        &mut self,
        rhs: hir::Expression,
        rhs_id: ast::ExprId,
        expected_type: hir::Type,
    ) -> Result<hir::Expression, ()> {
        match (expected_type, rhs.ty) {
            (ty, None) => self.coerce_untyped_expr(&rhs, rhs_id, ty),
            (hir::Type::Primitive(ty), Some(hir::Type::Primitive(ty2))) if ty == ty2 => Ok(rhs),
            (hir::Type::Enum(index_1), Some(hir::Type::Enum(index_2))) if index_1 == index_2 => {
                Ok(rhs)
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
        expr: &hir::Expression,
        expr_id: ast::ExprId,
        ty: hir::Type,
    ) -> Result<hir::Expression, ()> {
        match (&expr.kind, ty) {
            (hir::ExprKind::Int(value), ty) => self.coerce_untyped_int_expr(*value, expr_id, ty),
            (kind, _) => panic!("expected literal untyped expression, got {:?}", kind),
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
        scope: &mut BlockScope,
        operator: ast::BinaryOperator,
        left: hir::Expression,
        left_id: ast::ExprId,
        right: hir::Expression,
        right_id: ast::ExprId,
    ) -> hir::Expression {
        use hir::*;
        match operator {
            ast::BinaryOperator::Assign => {
                self.build_assignment_expression(scope, left, right, right_id)
            }
            ast::BinaryOperator::Add
            | ast::BinaryOperator::Subtract
            | ast::BinaryOperator::Multiply => match (left.ty, right.ty) {
                (None, None) => Builder::evaluate_untyped_binary_expr(operator, left, right),
                (None, Some(ty)) | (Some(ty), None) => {
                    let (typed, untyped) = match left.ty {
                        Some(_) => ((left.clone(), left_id), (right, right_id)),
                        None => ((right, right_id), (left.clone(), left_id)),
                    };

                    let coerced = self
                        .coerce_untyped_expr(&untyped.0, untyped.1, ty)
                        .expect("invalid coercion");

                    let (left, right) = match left.ty {
                        Some(_) => (typed.0, coerced),
                        None => (coerced, typed.0),
                    };

                    Expression {
                        kind: ExprKind::Binary {
                            operator,
                            lhs: Box::new(left),
                            rhs: Box::new(right),
                        },
                        ty: Some(ty),
                    }
                }
                (Some(Type::Primitive(primitive_1)), Some(Type::Primitive(primitive_2)))
                    if primitive_1 == primitive_2 =>
                {
                    Expression {
                        kind: ExprKind::Binary {
                            operator,
                            lhs: Box::new(left),
                            rhs: Box::new(right),
                        },
                        ty: Some(Type::Primitive(primitive_1)),
                    }
                }
                _ => panic!("type mismatch in binary expression"),
            },
            _ => unimplemented!("binary operator not implemented"),
        }
    }

    fn evaluate_untyped_binary_expr(
        operator: ast::BinaryOperator,
        left: hir::Expression,
        right: hir::Expression,
    ) -> hir::Expression {
        match (left.kind, right.kind) {
            (hir::ExprKind::Int(left), hir::ExprKind::Int(right)) => {
                let value = match operator {
                    ast::BinaryOperator::Add => left.checked_add(right),
                    ast::BinaryOperator::Subtract => left.checked_sub(right),
                    ast::BinaryOperator::Multiply => left.checked_mul(right),
                    ast::BinaryOperator::Divide => left.checked_div(right),
                    ast::BinaryOperator::Remainder => left.checked_rem(right),
                    _ => unimplemented!("unimplemented binary operator"),
                }
                .expect("integer overflow");

                hir::Expression {
                    kind: hir::ExprKind::Int(value),
                    ty: None,
                }
            }
            _ => panic!("can't evaluate untyped expression"),
        }
    }
}
