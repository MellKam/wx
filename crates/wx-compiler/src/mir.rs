use std::collections::HashMap;

use string_interner::symbol::SymbolU32;

use crate::{ast, tir};

pub type LocalIndex = u32;
pub type ScopeIndex = u32;
pub type GlobalIndex = u32;
pub type SignatureIndex = u32;
pub type FunctionIndex = u32;
pub type TupleIndex = u32;
pub type StringIndex = u32;

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ExprKind {
    Noop,
    Bool {
        value: bool,
    },
    Function {
        func_index: FunctionIndex,
    },
    Int {
        value: i64,
    },
    Float {
        value: f64,
    },
    LocalGet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    LocalSet {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    String {
        string_index: StringIndex,
    },
    Global {
        global_index: GlobalIndex,
    },
    GlobalSet {
        global_index: GlobalIndex,
        value: Box<Expression>,
    },
    Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Mul {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Div {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Rem {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    And {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Or {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    Drop {
        value: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Eqz {
        value: Box<Expression>,
    },
    NotEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Block {
        scope_index: ScopeIndex,
        expressions: Box<[Expression]>,
    },
    Break {
        scope_index: ScopeIndex,
        value: Option<Box<Expression>>,
    },
    Continue {
        scope_index: ScopeIndex,
    },
    Unreachable,
    IfElse {
        condition: Box<Expression>,
        then_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
    BitAnd {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitOr {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitXor {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    BitNot {
        value: Box<Expression>,
    },
    LeftShift {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    RightShift {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Less {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    LessEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Greater {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    GreaterEq {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Loop {
        scope_index: ScopeIndex,
        block: Box<Expression>,
    },
    Neg {
        value: Box<Expression>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    U32,
    U64,
    Unit,
    Never,
    Bool,
    Pointer,
    Tuple { tuple_index: TupleIndex },
    Function { signature_index: SignatureIndex },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Clone, PartialEq, Eq)]
struct Tuple {
    fields: Box<[Type]>,
}

impl std::hash::Hash for Tuple {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for field in self.fields.iter() {
            field.hash(state);
        }
    }
}

// the role of MIR is to desugar the syntax like x += 1 into x = x + 1 and lower
// the concepts like enums into primitive constants, convert labels from symbols
// in interner into numeric indices

#[cfg_attr(test, derive(serde::Serialize))]
pub struct MIR {
    pub functions: Vec<Function>,
    pub signatures: Vec<FunctionSignature>,
    pub globals: Vec<Global>,
    pub exports: Vec<ExportItem>,
    pub imports: Vec<ImportModule>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct ImportModule {
    pub name: String,
    pub items: Vec<ImportModuleItem>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ImportModuleItem {
    Function {
        name: String,
        func_index: FunctionIndex,
        signature_index: SignatureIndex,
    },
    Global {
        name: String,
        global_index: GlobalIndex,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum ExportItem {
    Function {
        func_index: FunctionIndex,
        name: String,
    },
    Global {
        global_index: GlobalIndex,
        name: String,
    },
}

#[cfg_attr(test, derive(serde::Serialize))]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Local {
    pub ty: Type,
    pub mutability: Mutability,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct BlockScope {
    pub kind: tir::BlockKind,
    pub parent: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub result: Type,
}

#[derive(Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSignature {
    pub items: Box<[Type]>,
    pub params_count: usize,
}

impl FunctionSignature {
    pub fn params(&self) -> &[Type] {
        &self.items[..self.params_count]
    }

    pub fn result(&self) -> Type {
        self.items[self.params_count]
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
    pub signature_index: SignatureIndex,
    pub scopes: Vec<BlockScope>,
    pub block: Expression,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
    pub ty: Type,
    pub mutability: Mutability,
    pub value: Expression,
}

struct TuplePool {
    lookup: HashMap<Tuple, TupleIndex>,
    tuples: Vec<Tuple>,
}

impl TuplePool {
    const STRING_TUPLE_INDEX: TupleIndex = 0;

    fn new() -> Self {
        let mut lookup = HashMap::new();
        let string_tuple = Tuple {
            fields: Box::new([Type::Pointer, Type::U32]),
        };
        lookup.insert(string_tuple.clone(), 0);

        TuplePool {
            lookup,
            tuples: vec![string_tuple],
        }
    }

    fn add(&mut self, tuple: Tuple) -> TupleIndex {
        if let Some(&index) = self.lookup.get(&tuple) {
            index
        } else {
            let index = self.lookup.len() as TupleIndex;
            self.lookup.insert(tuple.clone(), index);
            self.tuples.push(tuple);
            index
        }
    }

    fn get(&self, index: TupleIndex) -> Option<&Tuple> {
        self.tuples.get(index as usize)
    }
}

impl MIR {
    pub fn build(tir: &tir::TIR, interner: &ast::StringInterner) -> MIR {
        let mut builder = Builder {
            interner,
            string_pool: StringPool {
                lookup: HashMap::new(),
                strings: Vec::new(),
            },
            namespaces: &tir.namespaces,
            tuple_pool: TuplePool::new(),
        };

        let functions = tir
            .defined_functions
            .values()
            .map(|func| builder.lower_function(func))
            .collect();

        let globals = tir
            .defined_globals
            .values()
            .map(|global| builder.lower_global(global))
            .collect();

        MIR {
            functions,
            globals,
            signatures: tir
                .signatures
                .iter()
                .map(|signature| FunctionSignature {
                    items: signature
                        .items
                        .iter()
                        .map(|&ty| builder.lower_type(ty))
                        .collect(),
                    params_count: signature.params_count,
                })
                .collect(),
            imports: tir
                .namespaces
                .iter()
                .filter_map(|namespace| match namespace {
                    tir::Namespace::ImportModule(module) => Some(ImportModule {
                        name: interner
                            .resolve(module.external_name.inner)
                            .unwrap()
                            .to_string(),
                        items: module
                            .lookup
                            .iter()
                            .map(|(symbol, value)| match value {
                                tir::ImportValue::Function { func_index } => {
                                    let signature_index = tir.declared_functions
                                        [*func_index as usize]
                                        .signature_index;
                                    ImportModuleItem::Function {
                                        name: interner.resolve(*symbol).unwrap().to_string(),
                                        func_index: *func_index,
                                        signature_index,
                                    }
                                }
                                tir::ImportValue::Global { global_index } => {
                                    ImportModuleItem::Global {
                                        name: interner.resolve(*symbol).unwrap().to_string(),
                                        global_index: *global_index,
                                    }
                                }
                            })
                            .collect(),
                    }),
                    _ => None,
                })
                .collect(),
            exports: tir
                .exports
                .iter()
                .map(|export| match export {
                    tir::ExportItem::Function {
                        func_index,
                        external_name,
                        internal_name,
                    } => ExportItem::Function {
                        func_index: *func_index,
                        name: interner
                            .resolve(
                                external_name
                                    .clone()
                                    .map(|n| n.inner)
                                    .unwrap_or(internal_name.inner),
                            )
                            .unwrap()
                            .to_string(),
                    },
                    tir::ExportItem::Global {
                        global_index,
                        external_name,
                        internal_name,
                    } => ExportItem::Global {
                        global_index: *global_index,
                        name: interner
                            .resolve(
                                external_name
                                    .clone()
                                    .map(|n| n.inner)
                                    .unwrap_or(internal_name.inner),
                            )
                            .unwrap()
                            .to_string(),
                    },
                })
                .collect(),
        }
    }
}

struct StringPool {
    lookup: HashMap<SymbolU32, StringIndex>,
    strings: Vec<SymbolU32>,
}

impl StringPool {
    fn new() -> Self {
        StringPool {
            lookup: HashMap::new(),
            strings: Vec::new(),
        }
    }

    fn add(&mut self, symbol: SymbolU32) -> StringIndex {
        if let Some(&index) = self.lookup.get(&symbol) {
            index
        } else {
            let index = self.lookup.len() as StringIndex;
            self.lookup.insert(symbol, index);
            index
        }
    }

    fn get(&self, index: StringIndex) -> Option<SymbolU32> {
        self.strings.get(index as usize).cloned()
    }
}

struct Builder<'tir, 'interner> {
    interner: &'interner ast::StringInterner,
    namespaces: &'tir [tir::Namespace],
    string_pool: StringPool,
    tuple_pool: TuplePool,
}

impl<'tir, 'interner> Builder<'tir, 'interner> {
    fn lower_type(&self, ty: tir::Type) -> Type {
        match ty {
            tir::Type::I32 => Type::I32,
            tir::Type::I64 => Type::I64,
            tir::Type::F32 => Type::F32,
            tir::Type::F64 => Type::F64,
            tir::Type::U32 => Type::U32,
            tir::Type::U64 => Type::U64,
            tir::Type::Unit => Type::Unit,
            tir::Type::Never => Type::Never,
            tir::Type::Bool => Type::Bool,
            tir::Type::String => Type::Tuple {
                tuple_index: TuplePool::STRING_TUPLE_INDEX,
            },
            tir::Type::Function { signature_index } => Type::Function { signature_index },
            // TODO: handle enums
            tir::Type::Unknown | tir::Type::Error | tir::Type::Namespace { .. } => {
                // These shouldn't appear in valid TIR, but handle gracefully
                Type::I32
            }
        }
    }

    fn lower_function(&mut self, func: &tir::Function) -> Function {
        let frame = func
            .stack
            .scopes
            .iter()
            .map(|scope| self.lower_block_scope(scope))
            .collect();

        let block = self.lower_expression(&func.block);

        Function {
            signature_index: func.signature_index,
            scopes: frame,
            block,
        }
    }

    fn lower_block_scope(&self, scope: &tir::BlockScope) -> BlockScope {
        let locals = scope
            .locals
            .iter()
            .map(|local| Local {
                ty: self.lower_type(local.ty),
                mutability: if local.mut_span.is_some() {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                },
            })
            .collect();

        BlockScope {
            kind: scope.kind,
            parent: scope.parent,
            locals,
            result: self.lower_type(scope.inferred_type.unwrap_or(tir::Type::Unit)),
        }
    }

    fn lower_global(&mut self, global: &tir::Global) -> Global {
        Global {
            ty: self.lower_type(global.ty.inner),
            mutability: if global.mut_span.is_some() {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            value: self.lower_expression(&global.value.inner),
        }
    }

    fn lower_expression(&mut self, expr: &tir::Expression) -> Expression {
        let ty = self.lower_type(expr.ty);
        let kind = self.lower_expr_kind(&expr.kind);

        Expression { kind, ty }
    }

    fn lower_expr_kind(&mut self, kind: &tir::ExprKind) -> ExprKind {
        use crate::ast::{BinaryOp, UnaryOp};

        match kind {
            tir::ExprKind::Error | tir::ExprKind::Placeholder => ExprKind::Noop,
            tir::ExprKind::Unreachable => ExprKind::Unreachable,
            tir::ExprKind::Int { value } => ExprKind::Int { value: *value },
            tir::ExprKind::Float { value } => ExprKind::Float { value: *value },
            tir::ExprKind::Bool { value } => ExprKind::Bool { value: *value },
            tir::ExprKind::Global { global_index } => ExprKind::Global {
                global_index: *global_index,
            },
            tir::ExprKind::Local {
                scope_index,
                local_index,
            } => ExprKind::LocalGet {
                scope_index: *scope_index,
                local_index: *local_index,
            },
            tir::ExprKind::Function { func_index } => ExprKind::Function {
                func_index: *func_index,
            },
            tir::ExprKind::String { symbol } => ExprKind::String {
                string_index: self.string_pool.add(*symbol),
            },
            tir::ExprKind::Return { value } => ExprKind::Return {
                value: value.as_ref().map(|v| Box::new(self.lower_expression(v))),
            },
            tir::ExprKind::EnumVariant {
                namespace_index,
                variant_index,
            } => match &self.namespaces[*namespace_index as usize] {
                tir::Namespace::Enum(enum_) => {
                    let variant = &enum_.variants[*variant_index as usize];
                    self.lower_expression(&variant.value).kind
                }
                _ => unreachable!(),
            },
            tir::ExprKind::Call { callee, arguments } => {
                let callee = Box::new(self.lower_expression(callee));
                let arguments = arguments
                    .iter()
                    .map(|arg| self.lower_expression(arg))
                    .collect();
                ExprKind::Call { callee, arguments }
            }
            tir::ExprKind::NamespaceAccess {
                namespace_index,
                member,
                ..
            } => {
                let namespace = &self.namespaces[*namespace_index as usize];
                match namespace {
                    tir::Namespace::ImportModule(_) => self.lower_expression(member).kind,
                    tir::Namespace::Enum(_) => self.lower_expression(member).kind,
                }
            }
            tir::ExprKind::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let condition = Box::new(self.lower_expression(condition));
                let then_block = Box::new(self.lower_expression(then_block));
                let else_block = else_block
                    .as_ref()
                    .map(|e| Box::new(self.lower_expression(e)));
                ExprKind::IfElse {
                    condition,
                    then_block,
                    else_block,
                }
            }
            tir::ExprKind::Break { scope_index, value } => ExprKind::Break {
                scope_index: *scope_index,
                value: value.as_ref().map(|v| Box::new(self.lower_expression(v))),
            },
            tir::ExprKind::Continue { scope_index } => ExprKind::Continue {
                scope_index: *scope_index,
            },
            tir::ExprKind::Loop { scope_index, block } => ExprKind::Loop {
                scope_index: *scope_index,
                block: Box::new(self.lower_expression(block)),
            },
            tir::ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => {
                let mut lowered_exprs: Vec<Expression> = expressions
                    .iter()
                    .map(|e| self.lower_expression(e))
                    .collect();

                // Add the result expression if present
                if let Some(result) = result {
                    lowered_exprs.push(self.lower_expression(result));
                }

                ExprKind::Block {
                    scope_index: *scope_index,
                    expressions: lowered_exprs.into_boxed_slice(),
                }
            }
            tir::ExprKind::LocalDeclaration {
                scope_index,
                local_index,
                value,
                ..
            } => {
                // Lower local declaration to assignment
                ExprKind::LocalSet {
                    scope_index: *scope_index,
                    local_index: *local_index,
                    value: Box::new(self.lower_expression(value)),
                }
            }
            tir::ExprKind::Unary { operator, operand } => {
                let operand = Box::new(self.lower_expression(operand));
                match operator.inner {
                    UnaryOp::InvertSign => ExprKind::Neg { value: operand },
                    UnaryOp::Not => ExprKind::Eqz { value: operand },
                    UnaryOp::BitNot => ExprKind::BitNot { value: operand },
                }
            }
            tir::ExprKind::Binary {
                operator,
                left,
                right,
            } => {
                use BinaryOp::*;

                match operator.inner {
                    // Handle compound assignments by desugaring
                    Assign => {
                        // left = right
                        // This requires left to be an assignable expression
                        self.lower_assignment(left, right)
                    }
                    AddAssign | SubAssign | MulAssign | DivAssign | RemAssign => {
                        // x += y becomes x = x + y
                        self.lower_compound_assignment(operator.inner, left, right)
                    }
                    // Regular binary operations
                    Add => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Add { left, right }
                    }
                    Sub => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Sub { left, right }
                    }
                    Mul => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Mul { left, right }
                    }
                    Div => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Div { left, right }
                    }
                    Rem => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Rem { left, right }
                    }
                    Eq => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Eq { left, right }
                    }
                    NotEq => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::NotEq { left, right }
                    }
                    Less => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Less { left, right }
                    }
                    LessEq => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::LessEq { left, right }
                    }
                    Greater => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Greater { left, right }
                    }
                    GreaterEq => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::GreaterEq { left, right }
                    }
                    And => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::And { left, right }
                    }
                    Or => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::Or { left, right }
                    }
                    BitAnd => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::BitAnd { left, right }
                    }
                    BitOr => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::BitOr { left, right }
                    }
                    BitXor => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::BitXor { left, right }
                    }
                    LeftShift => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::LeftShift { left, right }
                    }
                    RightShift => {
                        let left = Box::new(self.lower_expression(left));
                        let right = Box::new(self.lower_expression(right));
                        ExprKind::RightShift { left, right }
                    }
                }
            }
        }
    }

    fn lower_assignment(&mut self, left: &tir::Expression, right: &tir::Expression) -> ExprKind {
        match &left.kind {
            tir::ExprKind::Local {
                scope_index,
                local_index,
            } => ExprKind::LocalSet {
                scope_index: *scope_index,
                local_index: *local_index,
                value: Box::new(self.lower_expression(right)),
            },
            tir::ExprKind::Global { global_index } => ExprKind::GlobalSet {
                global_index: *global_index,
                value: Box::new(self.lower_expression(right)),
            },
            _ => {
                // This shouldn't happen in well-formed TIR, but handle gracefully
                ExprKind::Noop
            }
        }
    }

    fn lower_compound_assignment(
        &mut self,
        op: crate::ast::BinaryOp,
        left: &tir::Expression,
        right: &tir::Expression,
    ) -> ExprKind {
        use crate::ast::BinaryOp::*;

        // Desugar x += y to x = x + y
        let binary_op = match op {
            AddAssign => Add,
            SubAssign => Sub,
            MulAssign => Mul,
            DivAssign => Div,
            RemAssign => Rem,
            _ => unreachable!(),
        };

        // Create the binary operation: x + y
        let binary_expr_kind = match binary_op {
            Add => ExprKind::Add {
                left: Box::new(self.lower_expression(left)),
                right: Box::new(self.lower_expression(right)),
            },
            Sub => ExprKind::Sub {
                left: Box::new(self.lower_expression(left)),
                right: Box::new(self.lower_expression(right)),
            },
            Mul => ExprKind::Mul {
                left: Box::new(self.lower_expression(left)),
                right: Box::new(self.lower_expression(right)),
            },
            Div => ExprKind::Div {
                left: Box::new(self.lower_expression(left)),
                right: Box::new(self.lower_expression(right)),
            },
            Rem => ExprKind::Rem {
                left: Box::new(self.lower_expression(left)),
                right: Box::new(self.lower_expression(right)),
            },
            _ => unreachable!(),
        };

        let binary_expr = Expression {
            kind: binary_expr_kind,
            ty: self.lower_type(left.ty),
        };

        // Now assign the result back to left: x = (x + y)
        match &left.kind {
            tir::ExprKind::Local {
                scope_index,
                local_index,
            } => ExprKind::LocalSet {
                scope_index: *scope_index,
                local_index: *local_index,
                value: Box::new(binary_expr),
            },
            tir::ExprKind::Global { global_index } => ExprKind::GlobalSet {
                global_index: *global_index,
                value: Box::new(binary_expr),
            },
            _ => {
                // This shouldn't happen in well-formed TIR
                ExprKind::Noop
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{ast, tir};

    #[allow(unused)]
    struct TestCase {
        interner: ast::StringInterner,
        files: ast::Files,
        ast: ast::AST,
        tir: tir::TIR,
        mir: MIR,
    }

    impl<'case> TestCase {
        fn new(source: &str) -> Self {
            let mut interner = ast::StringInterner::new();
            let mut files = ast::Files::new();
            let file_id = files
                .add("main.wx".to_string(), source.to_string())
                .unwrap();
            let ast =
                ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
            let tir = tir::TIR::build(&ast, &mut interner);
            let mir = MIR::build(&tir, &interner);

            TestCase {
                interner,
                files,
                ast,
                tir,
                mir,
            }
        }
    }

    #[test]
    fn test_parse_simple_addition() {
        let case = TestCase::new(indoc! {"
            import \"console\" {
                fn log(ptr: i32, len: i32) -> unit;
            }

            fn main() -> unit {
                console::log(\"hello world\");
            }

            export { main }
        "});
        insta::assert_yaml_snapshot!(case.mir);
    }
}
