pub mod builder;
pub mod diagnostics;
pub mod evaluator;
use std::collections::HashMap;
use std::str;

pub use builder::*;
use string_interner::backend::StringBackend;
use string_interner::symbol::SymbolU32;
use string_interner::{StringInterner, Symbol};

use crate::ast;

#[derive(Debug, Clone)]
pub struct HIR {
    pub expressions: Vec<Expression>,
    pub functions: Vec<Function>,
    pub enums: Vec<Enum>,
}

impl HIR {
    pub fn new() -> Self {
        HIR {
            expressions: Vec::new(),
            functions: Vec::new(),
            enums: Vec::new(),
        }
    }

    pub fn push_expr(&mut self, expr: Expression) -> ExprIndex {
        let index = ExprIndex(self.expressions.len() as u32);
        self.expressions.push(expr);
        index
    }

    pub fn set_expr(
        &mut self,
        index: ExprIndex,
        cb: impl FnOnce(&mut Expression),
    ) -> Result<(), ()> {
        match self.expressions.get_mut(index.0 as usize) {
            Some(expr) => {
                cb(expr);
                Ok(())
            }
            None => Err(()),
        }
    }

    pub fn get_expr(&self, index: ExprIndex) -> Option<&Expression> {
        self.expressions.get(index.0 as usize)
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
pub struct ExprIndex(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct LocalIndex(pub u32);

impl From<usize> for LocalIndex {
    fn from(value: usize) -> Self {
        LocalIndex(u32::try_from(value).expect("LocalIndex overflow"))
    }
}

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
        expr: ExprIndex,
    },
    Local {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    Function(FuncIndex),
    Return {
        value: Option<ExprIndex>,
    },
    EnumVariant {
        enum_index: EnumIndex,
        variant_index: EnumVariantIndex,
    },
    Unary {
        operator: ast::UnaryOp,
        operand: ExprIndex,
    },
    Binary {
        operator: ast::BinaryOp,
        lhs: ExprIndex,
        rhs: ExprIndex,
    },
    Call {
        callee: ExprIndex,
        arguments: Vec<Expression>,
    },
    Block {
        scope_index: ScopeIndex,
        expressions: Box<[ExprIndex]>,
        result: Option<ExprIndex>,
    },
    IfElse {
        condition: ExprIndex,
        then_block: ExprIndex,
        else_block: Option<ExprIndex>,
    },
    Break {
        scope_index: ScopeIndex,
        value: Option<ExprIndex>,
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
    pub stack: StackFrame,
    pub block: ExprIndex,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: SymbolU32,
    pub ty: PrimitiveType,
    pub variants: Box<[EnumVariant]>,
    pub lookup: HashMap<SymbolU32, EnumVariantIndex>,
}

impl Enum {
    pub fn resolve_variant(&self, symbol: SymbolU32) -> Option<EnumVariantIndex> {
        self.lookup.get(&symbol).copied()
    }
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: SymbolU32,
    pub value: i64,
}

#[derive(Debug, Clone)]
pub enum GlobalValue {
    Function {
        func_index: FuncIndex,
    },
    Enum {
        enum_index: EnumIndex,
    },
    EnumVariant {
        enum_index: EnumIndex,
        variant_index: EnumVariantIndex,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GlobalType {
    Type,
    Value,
}

#[derive(Debug)]
pub struct GlobalContext<'interner> {
    pub interner: &'interner StringInterner<StringBackend>,
    pub lookup: HashMap<(GlobalType, SymbolU32), GlobalValue>,
}

impl<'interner> GlobalContext<'interner> {
    pub fn new(interner: &'interner StringInterner<StringBackend>) -> Self {
        GlobalContext {
            interner,
            lookup: HashMap::new(),
        }
    }

    pub fn add_enum(&mut self, name: SymbolU32, index: EnumIndex) {
        self.lookup.insert(
            (GlobalType::Type, name),
            GlobalValue::Enum { enum_index: index },
        );
    }

    pub fn add_func(&mut self, name: SymbolU32, index: FuncIndex) {
        self.lookup.insert(
            (GlobalType::Value, name),
            GlobalValue::Function { func_index: index },
        );
    }

    pub fn resolve_type(&self, symbol: SymbolU32) -> Option<Type> {
        let text = self.interner.resolve(symbol).unwrap();
        match Type::try_from(text) {
            Ok(ty) => return Some(ty),
            Err(_) => {}
        }
        match self.lookup.get(&(GlobalType::Type, symbol)) {
            Some(GlobalValue::Enum { enum_index }) => Some(Type::Enum(*enum_index)),
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn resolve_value(&mut self, symbol: SymbolU32) -> Option<Expression> {
        match self.interner.resolve(symbol).unwrap() {
            "_" => {
                return Some(Expression {
                    kind: ExprKind::Placeholder,
                    ty: None,
                });
            }
            "true" => {
                return Some(Expression {
                    kind: ExprKind::Bool(true),
                    ty: Some(Type::Bool),
                });
            }
            "false" => {
                return Some(Expression {
                    kind: ExprKind::Bool(false),
                    ty: Some(Type::Bool),
                });
            }
            _ => {}
        };
        match self.lookup.get(&(GlobalType::Value, symbol)).cloned() {
            Some(value) => match value {
                GlobalValue::Function { func_index } => {
                    return Some(Expression {
                        kind: ExprKind::Function(func_index),
                        ty: Some(Type::Function(func_index)),
                    });
                }
                GlobalValue::EnumVariant {
                    enum_index,
                    variant_index,
                } => {
                    return Some(Expression {
                        kind: ExprKind::EnumVariant {
                            enum_index,
                            variant_index,
                        },
                        ty: Some(Type::Enum(enum_index)),
                    });
                }
                _ => unreachable!(),
            },
            None => {}
        };

        None
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
    pub func_index: FuncIndex,
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
