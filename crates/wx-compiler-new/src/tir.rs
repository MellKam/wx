use std::collections::HashMap;

use string_interner::symbol::SymbolU32;

use crate::ast;

pub struct TIR {
    pub file_id: ast::FileId,
    pub functions: Vec<Function>,
    pub enums: Vec<Enum>,
    pub globals: Vec<Global>,
    pub exports: Vec<ExportItem>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    U32,
    U64,
    Unit,
    Never,
    Unknown,
    Error,
    Bool,
    Function { signature_index: u32 },
    Enum { enum_index: u32 },
}

impl Type {
    pub fn is_primitive(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::F32 | Type::F64 => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 => true,
            _ => false,
        }
    }

    // pub fn coercible_to(self, other: Type) -> bool {
    //     match (self, other) {
    //         (a, b) if a == b => true,
    //         (Type::Never, _) => true,
    //         (Type::Error, _) => true,
    //         (Type::Unknown, _) => false,
    //         _ => false,
    //     }
    // }

    pub fn unify(a: Type, b: Type) -> Result<Type, ()> {
        match (a, b) {
            (a, b) if a == b => Ok(a),
            (_, Type::Never) | (Type::Never, _) => Ok(b),
            // ??? do we need to propagate error type like this?
            (Type::Error, _) | (_, Type::Error) => Ok(Type::Error),
            _ => Err(()),
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, <Type as TryFrom<&str>>::Error> {
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
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionSignature {
    params_count: u32,
    items: Box<[Type]>,
}

impl FunctionSignature {
    pub fn params(&self) -> &[Type] {
        &self.items[..self.params_count as usize]
    }

    pub fn result(&self) -> Type {
        self.items[self.params_count as usize]
    }
}

impl std::hash::Hash for FunctionSignature {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.params_count.hash(state);
        for item in self.items.iter().copied() {
            item.hash(state);
        }
    }
}

type LocalIndex = u32;
type ScopeIndex = u32;
type FunctionIndex = u32;
type EnumIndex = u32;
type GlobalIndex = u32;
type SignatureIndex = u32;
type EnumVariantIndex = u32;

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum ExprKind {
    Error,
    Placeholder,
    Unreachable,
    Int {
        value: i64,
    },
    Float {
        value: f64,
    },
    Bool {
        value: bool,
    },
    Global {
        global_index: GlobalIndex,
    },
    LocalDeclaration {
        name: ast::Spanned<SymbolU32>,
        scope_index: ScopeIndex,
        local_index: LocalIndex,
        value: Box<Expression>,
    },
    Local {
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    },
    Function {
        func_index: FunctionIndex,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    EnumVariant {
        enum_index: EnumIndex,
        variant_index: EnumVariantIndex,
    },
    Unary {
        operator: ast::Spanned<ast::UnaryOp>,
        operand: Box<Expression>,
    },
    Binary {
        operator: ast::Spanned<ast::BinaryOp>,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    Block {
        scope_index: ScopeIndex,
        statements: Box<[Expression]>,
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
    Loop {
        scope_index: ScopeIndex,
        block: Box<Expression>,
    },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Expression {
    pub kind: ExprKind,
    pub ty: Type,
    pub span: ast::TextSpan,
}

#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum AccessKind {
    Read,
    Write,
    ReadWrite,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
struct AccessContext {
    expected_type: Option<Type>,
    access_kind: AccessKind,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct VariableAccess {
    pub span: ast::TextSpan,
    pub kind: AccessKind,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Local {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: Type,
    pub mutability: Option<ast::TextSpan>,
    pub accesses: Vec<VariableAccess>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq)]
pub enum BlockKind {
    Block,
    /// Loop blocks have an implicit `continue` at the end.
    /// Their type is inferred from `break` expressions, not the final
    /// expression.
    Loop,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct BlockScope {
    pub kind: BlockKind,
    pub label: Option<SymbolU32>,
    pub parent: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub inferred_type: Option<Type>,
    pub expected_type: Option<Type>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StackFrame {
    pub scopes: Vec<BlockScope>,
}

impl StackFrame {
    pub fn push_local(&mut self, scope_index: u32, local: Local) -> LocalIndex {
        let scope = &mut self.scopes[scope_index as usize];
        let local_index = scope.locals.len() as LocalIndex;
        scope.locals.push(local);
        local_index
    }

    pub fn get_local(&self, scope_index: ScopeIndex, local_index: LocalIndex) -> Option<&Local> {
        self.scopes
            .get(scope_index as usize)?
            .locals
            .get(local_index as usize)
    }

    pub fn get_mut_local(
        &mut self,
        scope_index: ScopeIndex,
        local_index: LocalIndex,
    ) -> Option<&mut Local> {
        self.scopes
            .get_mut(scope_index as usize)?
            .locals
            .get_mut(local_index as usize)
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionParam {
    pub mut_span: Option<ast::TextSpan>,
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<Type>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
    pub name: ast::Spanned<SymbolU32>,
    pub params: Box<[FunctionParam]>,
    pub result: ast::Spanned<Type>,
    pub stack: StackFrame,
    pub block: Box<Expression>,
    pub accesses: Vec<ast::TextSpan>,
}

#[cfg_attr(test, derive(Debug, serde::Serialize))]
#[derive(Clone, Copy)]
pub enum ExportItem {
    Function { func_index: u32 },
    Global { global_index: u32 },
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Enum {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: Type,
    pub variants: Box<[EnumVariant]>,
    pub lookup: HashMap<SymbolU32, EnumVariantIndex>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct EnumVariant {
    pub name: ast::Spanned<SymbolU32>,
    pub value: Box<Expression>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Global {
    pub name: ast::Spanned<SymbolU32>,
    pub ty: ast::Spanned<Type>,
    pub mut_span: Option<ast::TextSpan>,
    pub value: Box<Expression>,
    pub accesses: Vec<VariableAccess>,
}

#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GlobalValue {
    Global {
        global_index: GlobalIndex,
    },
    Function {
        func_index: FunctionIndex,
    },
    Enum {
        enum_index: EnumIndex,
    },
    EnumVariant {
        enum_index: EnumIndex,
        variant_index: EnumVariantIndex,
    },
    True,
    False,
    Unreachable,
    Placeholder,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolCategory {
    Type,
    Value,
}

pub struct GlobalContext<'interner> {
    pub interner: &'interner ast::StringInterner,
    pub exports: Vec<ExportItem>,
    pub globals: Vec<Global>,
    pub functions: Vec<SignatureIndex>,
    pub function_types: Vec<FunctionSignature>,
    pub function_lookup: HashMap<FunctionSignature, SignatureIndex>,
    pub enums: Vec<Enum>,
    pub symbol_lookup: HashMap<(SymbolCategory, SymbolU32), GlobalValue>,
}

impl<'interner> GlobalContext<'interner> {
    pub fn new(interner: &'interner mut ast::StringInterner) -> Self {
        let mut lookup = HashMap::new();
        lookup.insert(
            (SymbolCategory::Value, interner.get_or_intern("_")),
            GlobalValue::Placeholder,
        );
        lookup.insert(
            (SymbolCategory::Value, interner.get_or_intern("true")),
            GlobalValue::True,
        );
        lookup.insert(
            (SymbolCategory::Value, interner.get_or_intern("false")),
            GlobalValue::False,
        );
        lookup.insert(
            (SymbolCategory::Value, interner.get_or_intern("unreachable")),
            GlobalValue::Unreachable,
        );

        GlobalContext {
            exports: Vec::new(),
            function_types: Vec::new(),
            enums: Vec::new(),
            interner,
            globals: Vec::new(),
            functions: Vec::new(),
            function_lookup: HashMap::new(),
            symbol_lookup: lookup,
        }
    }

    pub fn resolve_type(&mut self, type_expr: &ast::TypeExpression) -> Result<Type, ()> {
        match &type_expr {
            ast::TypeExpression::Error => Err(()),
            ast::TypeExpression::Identifier { symbol } => {
                let symbol = *symbol;
                let text = self.interner.resolve(symbol).unwrap();
                match Type::try_from(text) {
                    Ok(ty) => return Ok(ty),
                    Err(_) => {}
                }
                match self.symbol_lookup.get(&(SymbolCategory::Type, symbol)) {
                    Some(GlobalValue::Enum { enum_index }) => Ok(Type::Enum {
                        enum_index: *enum_index,
                    }),
                    Some(_) => Err(()),
                    None => Err(()),
                }
            }
            ast::TypeExpression::Function { params, result } => {
                let result = self.resolve_type(&result.inner.inner).unwrap();
                let items = params
                    .inner
                    .iter()
                    .map(|ty| self.resolve_type(&ty.inner.inner).unwrap())
                    .chain(Some(result))
                    .collect::<Box<_>>();
                let signature_index = self.ensure_signature_index(&FunctionSignature {
                    items,
                    params_count: params.inner.len() as u32,
                });
                Ok(Type::Function { signature_index })
            }
        }
    }

    pub fn build_signature(
        &mut self,
        params: &ast::Grouped<Box<[ast::Separated<ast::FunctionParam>]>>,
        result: &ast::TypeExpression,
    ) -> FunctionSignature {
        let result = self.resolve_type(result).unwrap();
        // TODO: we can preallocate box
        let items = params
            .inner
            .iter()
            .map(|ty| {
                self.resolve_type(&ty.inner.type_annotation.inner.inner)
                    .unwrap()
            })
            .chain(Some(result))
            .collect::<Box<_>>();
        FunctionSignature {
            items,
            params_count: params.inner.len() as u32,
        }
    }

    pub fn ensure_signature_index(&mut self, func_type: &FunctionSignature) -> SignatureIndex {
        match self.function_lookup.get(func_type).cloned() {
            Some(type_index) => type_index,
            None => {
                let signature_index = self.function_types.len() as u32;
                self.function_types.push(func_type.clone());
                self.function_lookup
                    .insert(func_type.clone(), signature_index);
                signature_index
            }
        }
    }

    pub fn resolve_value(&mut self, symbol: SymbolU32) -> Option<GlobalValue> {
        match self
            .symbol_lookup
            .get(&(SymbolCategory::Value, symbol))
            .cloned()
        {
            Some(value) => Some(value),
            None => None,
        }
    }

    pub fn resolve_func(&self, symbol: SymbolU32) -> Option<FunctionIndex> {
        match self
            .symbol_lookup
            .get(&(SymbolCategory::Value, symbol))
            .cloned()
        {
            Some(GlobalValue::Function { func_index }) => Some(func_index),
            _ => None,
        }
    }

    pub fn display_type(&self, ty: Type) -> String {
        match ty {
            Type::Unknown => "unknown".to_string(),
            Type::Error => "error".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Never => "never".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Enum { enum_index } => self
                .interner
                .resolve(self.enums[enum_index as usize].name.inner)
                .unwrap()
                .to_string(),
            Type::Function { signature_index } => {
                let func = &self.function_types[signature_index as usize];
                let params = func
                    .params()
                    .iter()
                    .map(|param| self.display_type(*param))
                    .collect::<Box<[_]>>()
                    .join(", ");

                format!("fn({}) -> {}", params, self.display_type(func.result()))
            }
        }
    }
}

pub struct FunctionContext {
    pub lookup: HashMap<(ScopeIndex, SymbolU32), LocalIndex>,
    pub func_index: FunctionIndex,
    pub scope_index: ScopeIndex,
    pub frame: StackFrame,
}

impl FunctionContext {
    pub fn push_local(&mut self, local: Local) -> LocalIndex {
        let name_symbol = local.name.inner;
        let index = self.frame.push_local(self.scope_index, local);
        self.lookup.insert((self.scope_index, name_symbol), index);
        index
    }

    pub fn resolve_local(&self, symbol: SymbolU32) -> Option<(ScopeIndex, LocalIndex)> {
        let mut scope_index = self.scope_index;

        loop {
            if let Some(&value) = self.lookup.get(&(scope_index, symbol)) {
                return Some((scope_index, value));
            }

            scope_index = self.frame.scopes[scope_index as usize].parent?;
        }
    }

    pub fn enter_block<T>(
        &mut self,
        block: BlockScope,
        handler: fn(&mut FunctionContext) -> T,
    ) -> T {
        let parent_scope_index = self.scope_index;
        self.scope_index = self.frame.scopes.len() as u32;
        self.frame.scopes.push(block);

        let result = handler(self);

        self.scope_index = parent_scope_index;
        result
    }

    pub fn resolve_label(&self, symbol: SymbolU32) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.frame.scopes[scope_index as usize];
            if scope.label == Some(symbol) {
                return Some(scope_index);
            }

            scope_index = match scope.parent {
                Some(parent) => parent,
                None => return None,
            };
        }
    }

    pub fn get_closest_loop_block(&self) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.frame.scopes[scope_index as usize];
            match scope.kind {
                BlockKind::Loop => return Some(scope_index),
                _ => {}
            }

            scope_index = match scope.parent {
                Some(parent) => parent,
                None => return None,
            }
        }
    }
}
