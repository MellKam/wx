pub mod builder;
pub mod diagnostics;
pub mod evaluator;
use std::collections::HashMap;
use std::str;

pub use builder::*;
use string_interner::symbol::SymbolU32;

use crate::ast;

#[derive(Debug, Clone)]
pub struct HIR {
    pub functions: Vec<Function>,
    pub enums: Vec<Enum>,
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

impl TryFrom<&str> for PrimitiveType {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i32" => Ok(PrimitiveType::I32),
            "i64" => Ok(PrimitiveType::I64),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Function(FunctionIndex),
    Enum(EnumIndex),
    Bool,
    Unit,
    Never,
    Unknown,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LocalIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunctionIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EnumIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EnumVariantIndex(pub u32);

#[derive(Debug, Clone, PartialEq)]
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
    Function(FunctionIndex),
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
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
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
pub struct BlockScope {
    pub parent_scope: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub result: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub scopes: Vec<BlockScope>,
}

impl StackFrame {
    pub fn push_local(&mut self, scope_index: ScopeIndex, local: Local) -> LocalIndex {
        let scope = self
            .scopes
            .get_mut(scope_index.0 as usize)
            .expect("invalid scope index");
        let local_index = LocalIndex(scope.locals.len() as u32);
        scope.locals.push(local);
        local_index
    }

    pub fn get_local(&self, scope_index: ScopeIndex, local_index: LocalIndex) -> Option<&Local> {
        let scope = self
            .scopes
            .get(scope_index.0 as usize)
            .expect("invalid scope index");

        scope.locals.get(local_index.0 as usize)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub export: bool,
    pub name: SymbolU32,
    pub ty: FunctionType,
    pub scopes: StackFrame,
    pub expressions: Box<[Expression]>,
    pub result: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: SymbolU32,
    pub ty: PrimitiveType,
    pub variants: Box<[EnumVariant]>,
    pub variant_lookup: HashMap<SymbolU32, EnumVariantIndex>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: SymbolU32,
    pub value: i64,
}

#[derive(Debug, Clone)]
pub enum GlobalValue {
    Function(FunctionIndex),
    EnumVariant {
        enum_index: EnumIndex,
        variant_index: EnumVariantIndex,
    },
}

#[derive(Debug, Clone)]
pub enum GlobalType {
    Enum(EnumIndex),
}

#[derive(Debug)]
pub struct GlobalScope {
    pub functions: Vec<FunctionType>,
    pub enums: Vec<Enum>,
    pub type_lookup: HashMap<SymbolU32, GlobalType>,
    pub value_lookup: HashMap<SymbolU32, GlobalValue>,
}

impl GlobalScope {
    pub fn new() -> Self {
        GlobalScope {
            functions: Vec::new(),
            enums: Vec::new(),
            type_lookup: HashMap::new(),
            value_lookup: HashMap::new(),
        }
    }

    pub fn get_enum(&self, enum_index: EnumIndex) -> Option<&Enum> {
        self.enums.get(enum_index.0 as usize)
    }

    pub fn add_enum(&mut self, enum_: Enum) -> EnumIndex {
        let index = EnumIndex(self.enums.len() as u32);
        self.type_lookup.insert(enum_.name, GlobalType::Enum(index));
        self.enums.push(enum_);
        index
    }

    pub fn get_function(&self, func_index: FunctionIndex) -> Option<&FunctionType> {
        self.functions.get(func_index.0 as usize)
    }

    pub fn add_function(&mut self, func_name: SymbolU32, func_type: FunctionType) -> FunctionIndex {
        let index = FunctionIndex(self.functions.len() as u32);
        self.value_lookup
            .insert(func_name, GlobalValue::Function(index));
        self.functions.push(func_type);
        index
    }
}

#[derive(Debug, Clone, Copy)]
enum LookupValue {
    Local(LocalIndex),
    Label(ScopeIndex),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LookupType {
    Local,
    Label,
}

type LookupKey = (LookupType, ScopeIndex, SymbolU32);

#[derive(Debug, Clone)]
struct FunctionContext {
    pub lookup: HashMap<LookupKey, LookupValue>,
    pub func_index: FunctionIndex,
    pub scope_index: ScopeIndex,
    pub frame: StackFrame,
}

impl FunctionContext {
    pub fn push_local(&mut self, local: Local) -> LocalIndex {
        let name = local.name;
        let index = self.frame.push_local(self.scope_index, local);
        self.lookup.insert(
            (LookupType::Local, self.scope_index, name),
            LookupValue::Local(index),
        );
        index
    }

    pub fn resolve_local(&self, symbol: SymbolU32) -> Option<(ScopeIndex, LocalIndex)> {
        let mut scope_index = self.scope_index;

        loop {
            match self.lookup.get(&(LookupType::Local, scope_index, symbol)) {
                Some(&value) => {
                    return Some((
                        scope_index,
                        match value {
                            LookupValue::Local(local_index) => local_index,
                            _ => unreachable!(),
                        },
                    ));
                }
                None => match self.frame.scopes.get(scope_index.0 as usize) {
                    Some(scope) => scope_index = scope.parent_scope?,
                    None => break,
                },
            }
        }

        None
    }

    pub fn resolve_label(&self, symbol: SymbolU32) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            match self.lookup.get(&(LookupType::Label, scope_index, symbol)) {
                Some(&LookupValue::Label(label_index)) => return Some(label_index),
                _ => match self.frame.scopes.get(scope_index.0 as usize) {
                    Some(scope) => scope_index = scope.parent_scope?,
                    None => break,
                },
            }
        }

        None
    }

    pub fn enter_scope<T>(&mut self, handler: impl FnOnce(&mut Self) -> T) -> T {
        let new_scope = BlockScope {
            parent_scope: Some(self.scope_index),
            locals: Vec::new(),
            result: None,
        };
        self.scope_index = ScopeIndex(self.frame.scopes.len() as u32);
        self.frame.scopes.push(new_scope);

        let result = handler(self);

        self.scope_index = match self.frame.scopes.get(self.scope_index.0 as usize) {
            Some(scope) => scope.parent_scope.unwrap(),
            None => unreachable!("invalid current scope index"),
        };
        result
    }

    pub fn enter_scope_with_label<T>(
        &mut self,
        label: SymbolU32,
        handler: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.lookup.insert(
            (LookupType::Label, self.scope_index, label),
            LookupValue::Label(ScopeIndex(self.frame.scopes.len() as u32)),
        );
        self.enter_scope(handler)
    }
}
