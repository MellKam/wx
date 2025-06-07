pub mod builder;
pub mod diagnostics;
pub mod evaluator;
mod global;
mod local;

use std::collections::HashMap;
use std::str;

pub use builder::*;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;

use crate::ast;
use crate::files::FileId;
use crate::hir::local::StackFrame;

#[derive(Debug, Clone)]
pub enum ExportItem {
    Function {
        func_index: FuncIndex,
        name: SymbolU32,
    },
    // TODO: Global
}

#[derive(Debug, Clone)]
pub struct HIR {
    pub file_id: FileId,
    pub functions: Vec<Function>,
    pub enums: Vec<Enum>,
    pub exports: Vec<ExportItem>,
}

impl HIR {
    pub fn new(file_id: FileId) -> Self {
        HIR {
            file_id,
            functions: Vec::new(),
            enums: Vec::new(),
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    I32,
    I64,
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Function(FuncIndex),
    Enum(EnumIndex),
    Bool,
    Unit,
    Never,
    Unknown,
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i32" => Ok(Type::Primitive(PrimitiveType::I32)),
            "i64" => Ok(Type::Primitive(PrimitiveType::I64)),
            "bool" => Ok(Type::Bool),
            "unit" => Ok(Type::Unit),
            "never" => Ok(Type::Never),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Primitive(ty) => write!(f, "{}", ty),
            Type::Function(index) => write!(f, "function({})", index.0),
            Type::Enum(index) => write!(f, "enum({})", index.0),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "unit"),
            Type::Never => write!(f, "never"),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Box<[Type]>,
    pub result: Type,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone, Copy)]
pub struct LocalIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuncIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EnumIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EnumVariantIndex(pub u32);

#[derive(Debug, Clone)]
pub enum ExprKind {
    Placeholder,
    Int(i64),
    Bool(bool),
    LocalDeclaration {
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
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mutability {
    Mutable,
    Const,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub name: SymbolU32,
    pub ty: Type,
    pub mutability: Mutability,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: SymbolU32,
    pub ty: FunctionType,
    pub stack: StackFrame,
    pub block: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: SymbolU32,
    pub ty: PrimitiveType,
    pub variants: Box<[EnumVariant]>,
    pub lookup: HashMap<SymbolU32, EnumVariantIndex>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: SymbolU32,
    pub value: i64,
}
