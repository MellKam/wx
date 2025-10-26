pub mod builder;
pub mod diagnostics;
mod global;

#[cfg(test)]
mod tests;

use std::collections::HashMap;
use std::hash::Hash;

pub use builder::*;
#[cfg(test)]
use serde::Serialize;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use crate::ast;
use crate::files::FileId;
use crate::span::TextSpan;

pub struct HIR {
    pub file_id: FileId,
    pub functions: Vec<Function>,
    pub enums: Vec<Enum>,
    pub globals: Vec<Global>,
    pub exports: Vec<ExportItem>,
}

impl HIR {
    pub fn new(file_id: FileId) -> Self {
        HIR {
            file_id,
            functions: Vec::new(),
            enums: Vec::new(),
            globals: Vec::new(),
            exports: Vec::new(),
        }
    }

    pub fn push_func(&mut self, function: Function) -> FuncIndex {
        let index = FuncIndex(self.functions.len() as u32);
        self.functions.push(function);
        index
    }

    pub fn get_func(&self, index: FuncIndex) -> Option<&Function> {
        self.functions.get(index.0 as usize)
    }

    pub fn push_enum(&mut self, enum_: Enum) -> EnumIndex {
        let index = EnumIndex(self.enums.len() as u32);
        self.enums.push(enum_);
        index
    }

    pub fn get_enum(&self, index: EnumIndex) -> Option<&Enum> {
        self.enums.get(index.0 as usize)
    }
}

#[derive(Clone, Copy)]
#[cfg_attr(test, derive(Debug, Serialize))]
pub enum ExportItem {
    Function { func_index: FuncIndex },
    Global { global_index: GlobalIndex },
}

impl Type {
    pub const fn is_primitive(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::F32 | Type::F64 => true,
            _ => false,
        }
    }

    pub const fn is_integer(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 => true,
            _ => false,
        }
    }

    pub const fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 => true,
            _ => false,
        }
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SignatureIndex(pub u32);

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy)]
pub struct LocalIndex(pub u32);

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeIndex(pub u32);

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncIndex(pub u32);

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumIndex(pub u32);

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy, PartialEq)]
pub struct EnumVariantIndex(pub u32);

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalIndex(pub u32);

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    U32,
    U64,
    Function(SignatureIndex),
    Enum(EnumIndex),
    Bool,
    Unit,
    Never,
    Unknown,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct TypeWithSpan {
    pub ty: Type,
    pub span: TextSpan,
}

impl Type {
    pub fn coercible_to(self, other: Type) -> bool {
        match (self, other) {
            (a, b) if a == b => true,
            (Type::Never, _) => true,
            (Type::Unknown, _) => true,
            _ => false,
        }
    }

    pub fn unify(a: Type, b: Type) -> Result<Type, ()> {
        match (a, b) {
            (a, b) if a == b => Ok(a),
            (_, Type::Never) | (Type::Never, _) => Ok(b),
            (Type::Unknown, _) | (_, Type::Unknown) => Ok(Type::Unknown),
            _ => Err(()),
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "bool" => Ok(Type::Bool),
            "unit" => Ok(Type::Unit),
            "never" => Ok(Type::Never),
            _ => Err(()),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct FunctionSignature {
    pub items: Box<[Type]>,
    pub params_count: u32,
}

impl FunctionSignature {
    pub fn params(&self) -> &[Type] {
        &self.items[0..self.params_count as usize]
    }

    pub fn result(&self) -> Type {
        self.items[self.params_count as usize]
    }
}

impl Hash for FunctionSignature {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for item in self.items.iter() {
            item.hash(state);
        }
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub enum ExprKind {
    Error,
    Placeholder,
    Int(i64),
    Float(f64),
    Bool(bool),
    Global {
        global_index: GlobalIndex,
    },
    LocalDeclaration {
        name: ast::Identifier,
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        expr: Box<Expression>,
    },
    Local {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    Function(FuncIndex),
    Return {
        value: Option<Box<Expression>>,
    },
    EnumVariant {
        enum_index: EnumIndex,
        variant_index: EnumVariantIndex,
    },
    Unary {
        operator: ast::UnaryOp,
        operand: Box<Expression>,
    },
    Binary {
        operator: ast::BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    Block {
        scope_index: ScopeIndex,
        expressions: Box<[Expression]>,
        result: Option<Box<Expression>>,
    },
    IfElse {
        condition: Box<Expression>,
        then_block: Box<Expression>,
        else_block: Option<Box<Expression>>,
    },
    Break {
        scope_index: ScopeIndex,
        value: Option<Box<Expression>>,
    },
    Continue {
        scope_index: ScopeIndex,
    },
    Unreachable,
    Loop {
        scope_index: ScopeIndex,
        block: Box<Expression>,
    },
}

impl ExprKind {
    pub fn unwrap_block(&self) -> (ScopeIndex, &Box<[Expression]>, &Option<Box<Expression>>) {
        match self {
            ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => (*scope_index, expressions, result),
            _ => panic!("expected block expression"),
        }
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct Expression {
    pub kind: ExprKind,
    pub span: TextSpan,
    pub ty: Option<Type>,
}

#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub enum AccessKind {
    Read,
    Write,
    ReadWrite,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
struct AccessContext {
    expected_type: Option<Type>,
    access_kind: AccessKind,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct VariableAccess {
    pub span: TextSpan,
    pub kind: AccessKind,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct Local {
    pub name: ast::Identifier,
    pub ty: Type,
    pub mutability: Option<TextSpan>,
    pub accesses: Vec<VariableAccess>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
#[derive(Clone, Copy, PartialEq)]
pub enum BlockKind {
    Block,
    /// Loop blocks have an implicit `continue` at the end.
    /// Their type is inferred from `break` expressions, not the final
    /// expression.
    Loop,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct BlockScope {
    pub kind: BlockKind,
    pub label: Option<SymbolU32>,
    pub parent: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub inferred_type: Option<Type>,
    pub expected_type: Option<Type>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct StackFrame {
    pub scopes: Vec<BlockScope>,
}

impl StackFrame {
    pub fn push_local(&mut self, scope_index: ScopeIndex, local: Local) -> LocalIndex {
        let scope = &mut self.scopes[scope_index.0 as usize];
        let local_index = LocalIndex(scope.locals.len() as u32);
        scope.locals.push(local);
        local_index
    }

    pub fn get_local(&self, scope_index: ScopeIndex, local_index: LocalIndex) -> Option<&Local> {
        self.scopes
            .get(scope_index.0 as usize)?
            .locals
            .get(local_index.0 as usize)
    }

    pub fn get_mut_local(
        &mut self,
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    ) -> Option<&mut Local> {
        self.scopes
            .get_mut(scope_index.0 as usize)?
            .locals
            .get_mut(local_index.0 as usize)
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct FunctionParam {
    pub name: ast::Identifier,
    pub ty: TypeWithSpan,
    pub mutability: Option<TextSpan>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct Function {
    pub name: ast::Identifier,
    pub params: Box<[FunctionParam]>,
    pub result: TypeWithSpan,
    pub stack: StackFrame,
    pub block: Box<Expression>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct Enum {
    pub name: ast::Identifier,
    pub ty: Type,
    pub variants: Box<[EnumVariant]>,
    pub lookup: HashMap<SymbolU32, EnumVariantIndex>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct EnumVariant {
    pub name: ast::Identifier,
    pub value: Expression,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(Serialize))]
pub struct Global {
    pub name: ast::Identifier,
    pub ty: Type,
    pub mutability: Option<TextSpan>,
    pub value: Expression,
    pub accesses: Vec<VariableAccess>,
}
