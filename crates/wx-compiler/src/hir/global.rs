use crate::hir::*;

#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub enum GlobalValue {
    Global {
        global_index: GlobalIndex,
    },
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
    Bool {
        value: bool,
    },
    Placeholder,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum LookupCategory {
    Type,
    Value,
}

pub struct GlobalContext<'interner> {
    pub interner: &'interner StringInterner<StringBackend>,
    pub exports: Vec<ExportItem>,
    pub globals: Vec<Global>,
    pub functions: Vec<SignatureIndex>,
    pub function_types: Vec<FunctionSignature>,
    pub function_lookup: HashMap<FunctionSignature, SignatureIndex>,
    pub enums: Vec<Enum>,
    pub symbol_lookup: HashMap<(LookupCategory, SymbolU32), GlobalValue>,
}

impl<'interner> GlobalContext<'interner> {
    pub fn new(interner: &'interner mut StringInterner<StringBackend>) -> Self {
        let mut lookup = HashMap::new();
        lookup.insert(
            (LookupCategory::Value, interner.get_or_intern("_")),
            GlobalValue::Placeholder,
        );
        lookup.insert(
            (LookupCategory::Value, interner.get_or_intern("true")),
            GlobalValue::Bool { value: true },
        );
        lookup.insert(
            (LookupCategory::Value, interner.get_or_intern("false")),
            GlobalValue::Bool { value: false },
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
        match &type_expr.kind {
            ast::TypeExprKind::Error => Err(()),
            ast::TypeExprKind::Identifier { symbol } => {
                let symbol = *symbol;
                let text = self.interner.resolve(symbol).unwrap();
                match Type::try_from(text) {
                    Ok(ty) => return Ok(ty),
                    Err(_) => {}
                }
                match self.symbol_lookup.get(&(LookupCategory::Type, symbol)) {
                    Some(GlobalValue::Enum { enum_index }) => Ok(Type::Enum(*enum_index)),
                    Some(_) => Err(()),
                    None => Err(()),
                }
            }
            ast::TypeExprKind::Function { params, result } => {
                let result = self.resolve_type(&result.ty).unwrap();
                let items = params
                    .inner
                    .iter()
                    .map(|ty| self.resolve_type(&ty.inner).unwrap())
                    .chain(Some(result))
                    .collect::<Box<_>>();
                let type_index = self.ensure_signature_index(&FunctionSignature {
                    items,
                    params_count: params.inner.len() as u32,
                });
                Ok(Type::Function(type_index))
            }
        }
    }

    pub fn build_signature(
        &mut self,
        params: &ast::Grouped<Box<[ast::Separated<ast::FunctionParam>]>>,
        result: &ast::TypeAnnotation,
    ) -> FunctionSignature {
        let result = self.resolve_type(&result.ty).unwrap();
        let items = params
            .inner
            .iter()
            .map(|ty| self.resolve_type(&ty.inner.annotation.ty).unwrap())
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
                let type_index = SignatureIndex(self.function_types.len() as u32);
                self.function_types.push(func_type.clone());
                self.function_lookup.insert(func_type.clone(), type_index);
                type_index
            }
        }
    }

    pub fn resolve_value(&mut self, symbol: SymbolU32) -> Option<GlobalValue> {
        match self
            .symbol_lookup
            .get(&(LookupCategory::Value, symbol))
            .cloned()
        {
            Some(value) => Some(value),
            None => None,
        }
    }

    pub fn resolve_func(&self, symbol: SymbolU32) -> Option<FuncIndex> {
        match self
            .symbol_lookup
            .get(&(LookupCategory::Value, symbol))
            .cloned()
        {
            Some(GlobalValue::Function { func_index }) => Some(func_index),
            _ => None,
        }
    }

    pub fn display_type(&self, ty: Type) -> String {
        match ty {
            Type::Unknown => "unknown".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Never => "never".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Enum(enum_index) => self
                .interner
                .resolve(self.enums[enum_index.0 as usize].name.symbol)
                .unwrap()
                .to_string(),
            Type::Function(func_index) => {
                let func = &self.function_types[func_index.0 as usize];
                let params = func
                    .params()
                    .iter()
                    .map(|param| self.display_type(*param))
                    .collect::<Box<[_]>>()
                    .join(", ");

                format!("func({}) -> {}", params, self.display_type(func.result()))
            }
        }
    }
}
