use crate::hir::*;

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
    Bool {
        value: bool,
    },
    Placeholder,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LookupCategory {
    Type,
    Value,
}

#[derive(Debug)]
pub struct GlobalContext<'interner> {
    pub exports: Vec<ExportItem>,
    pub functions: Vec<FunctionType>,
    pub enums: Vec<Enum>,
    pub interner: &'interner StringInterner<StringBackend>,
    pub lookup: HashMap<(LookupCategory, SymbolU32), GlobalValue>,
}

impl<'interner> GlobalContext<'interner> {
    pub fn new(
        interner: &'interner mut StringInterner<StringBackend>,
        items: &Vec<ast::Item>,
    ) -> Self {
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

        let mut global = GlobalContext {
            exports: Vec::new(),
            functions: Vec::new(),
            enums: Vec::new(),
            interner,
            lookup,
        };

        for item in items.iter() {
            match &item.kind {
                ast::ItemKind::ExportModifier { item, .. } => global.add_exported_item(item),
                _ => global.add_item(item),
            }
        }

        global
    }

    fn build_function(&self, signature: &ast::FunctionSignature) -> FunctionType {
        let params = signature
            .params
            .iter()
            .map(|param| Local {
                name: param.name.clone(),
                ty: self.resolve_type(param.ty.symbol).unwrap_or(Type::Unit),
                mutability: match param.mutable {
                    Some(_) => Mutability::Mutable,
                    None => Mutability::Const,
                },
            })
            .collect();

        let result = match &signature.result {
            Some(ty) => self.resolve_type(ty.symbol).unwrap_or(Type::Unit),
            None => Type::Unit,
        };

        FunctionType { params, result }
    }

    fn build_enum(&self, item: &ast::Item) -> Enum {
        let (name, ty, variants) = match &item.kind {
            ast::ItemKind::EnumDefinition { name, ty, variants } => {
                (name.clone(), ty.clone(), variants)
            }
            _ => unreachable!(),
        };

        let ty = match self.resolve_type(ty.symbol.clone()) {
            Some(Type::Primitive(ty)) => ty,
            _ => panic!("invalid enum representation type"),
        };

        let variants: Box<[_]> = variants
            .iter()
            .map(|variant| {
                let value = match variant.value.kind {
                    ast::ExprKind::Int { value } => value.clone(),
                    _ => panic!("invalid enum value"),
                };

                EnumVariant {
                    name: variant.name.clone(),
                    value,
                }
            })
            .collect();

        let lookup = variants
            .iter()
            .enumerate()
            .map(|(index, variant)| (variant.name.symbol, EnumVariantIndex(index as u32)))
            .collect();

        Enum {
            name,
            ty,
            variants,
            lookup,
        }
    }

    fn add_exported_item(&mut self, item: &ast::Item) {
        match &item.kind {
            ast::ItemKind::FunctionDefinition { .. } => {
                let index = FuncIndex(self.functions.len() as u32);
                self.exports
                    .push(ExportItem::Function { func_index: index });
                self.add_item(item);
            }
            _ => panic!("only functions can be exported"),
        }
    }

    fn add_item(&mut self, item: &ast::Item) {
        match &item.kind {
            ast::ItemKind::FunctionDefinition { signature, .. } => {
                self.lookup.insert(
                    (LookupCategory::Value, signature.name.symbol),
                    GlobalValue::Function {
                        func_index: FuncIndex(self.functions.len() as u32),
                    },
                );
                self.functions.push(self.build_function(&signature));
            }
            ast::ItemKind::EnumDefinition { name, .. } => {
                let enum_index = EnumIndex(self.enums.len() as u32);
                self.lookup.insert(
                    (LookupCategory::Type, name.symbol),
                    GlobalValue::Enum { enum_index },
                );
                self.enums.push(self.build_enum(&item));
            }
            _ => unreachable!(),
        }
    }

    pub fn resolve_type(&self, symbol: SymbolU32) -> Option<Type> {
        let text = self.interner.resolve(symbol).unwrap();
        match Type::try_from(text) {
            Ok(ty) => return Some(ty),
            Err(_) => {}
        }
        match self.lookup.get(&(LookupCategory::Type, symbol)) {
            Some(GlobalValue::Enum { enum_index }) => Some(Type::Enum(*enum_index)),
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn resolve_value(&mut self, symbol: SymbolU32) -> Option<GlobalValue> {
        match self.lookup.get(&(LookupCategory::Value, symbol)).cloned() {
            Some(value) => Some(value),
            None => None,
        }
    }

    pub fn resolve_function(&self, symbol: SymbolU32) -> Option<FuncIndex> {
        match self.lookup.get(&(LookupCategory::Value, symbol)).cloned() {
            Some(GlobalValue::Function { func_index }) => Some(func_index),
            _ => None,
        }
    }

    pub fn get_type_display(&self, ty: Type) -> String {
        match ty {
            Type::Unit => "unit".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Never => "never".to_string(),
            Type::Primitive(primitive) => match primitive {
                PrimitiveType::I32 => "i32".to_string(),
                PrimitiveType::I64 => "i64".to_string(),
            },
            Type::Enum(enum_index) => {
                let enum_ = self.enums.get(enum_index.0 as usize).unwrap();
                self.interner
                    .resolve(enum_.name.symbol)
                    .unwrap()
                    .to_string()
            }
            Type::Function(func_index) => {
                let func = self.functions.get(func_index.0 as usize).unwrap();
                let mut result = String::from("fn(");

                for param in func.params.iter() {
                    let name = self.interner.resolve(param.name.symbol).unwrap();
                    let ty = self.get_type_display(param.ty).to_string();
                    result.push_str(name);
                    result.push_str(": ");
                    result.push_str(&ty);
                }

                result.push_str(") -> ");
                result.push_str(&self.get_type_display(func.result));

                result
            }
        }
    }
}
