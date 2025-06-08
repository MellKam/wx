use crate::hir::*;

#[derive(Debug, Clone)]
pub struct BlockScope {
    pub parent_scope: Option<ScopeIndex>,
    pub locals: Vec<Local>,
    pub inferred_type: Option<Type>,
    pub expected_type: Option<Type>,
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

#[derive(Debug, Clone, Copy)]
pub enum LocalValue {
    Local(LocalIndex),
    /// Storing `ScopeIndex` here is unnecessary since each block has at most
    /// one label. The `ScopeIndex` can be retrieved from the lookup key
    Label,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LocalType {
    Local,
    Label,
}

#[derive(Debug, Clone)]
pub struct LocalContext {
    pub lookup: HashMap<(LocalType, ScopeIndex, SymbolU32), LocalValue>,
    pub func_index: FuncIndex,
    pub scope_index: ScopeIndex,
    pub frame: StackFrame,
}

impl LocalContext {
    pub fn push_local(&mut self, local: Local) -> LocalIndex {
        let name = local.name;
        let index = self.frame.push_local(self.scope_index, local);
        self.lookup.insert(
            (LocalType::Local, self.scope_index, name),
            LocalValue::Local(index),
        );
        index
    }

    pub fn resolve_local(&self, symbol: SymbolU32) -> Option<(ScopeIndex, LocalIndex)> {
        let mut scope_index = self.scope_index;

        loop {
            if let Some(&value) = self.lookup.get(&(LocalType::Local, scope_index, symbol)) {
                return Some((
                    scope_index,
                    match value {
                        LocalValue::Local(local_index) => local_index,
                        _ => unreachable!(),
                    },
                ));
            }

            scope_index = self
                .frame
                .scopes
                .get(scope_index.0 as usize)
                .expect("scope should be found")
                .parent_scope?;
        }
    }

    pub fn resolve_label(&self, symbol: SymbolU32) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            if let Some(_) = self.lookup.get(&(LocalType::Label, scope_index, symbol)) {
                return Some(scope_index);
            }

            scope_index = self
                .frame
                .scopes
                .get(scope_index.0 as usize)
                .expect("scope should be found")
                .parent_scope?;
        }
    }

    pub fn enter_scope<T>(
        &mut self,
        expected_type: Option<Type>,
        handler: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let parent_scope_index = self.scope_index;
        let new_scope = BlockScope {
            parent_scope: Some(parent_scope_index),
            locals: Vec::new(),
            expected_type,
            inferred_type: None,
        };
        self.scope_index = ScopeIndex(self.frame.scopes.len() as u32);
        self.frame.scopes.push(new_scope);

        let result = handler(self);

        self.scope_index = parent_scope_index;
        result
    }

    pub fn set_scope_label(&mut self, label: SymbolU32) {
        self.lookup.insert(
            (LocalType::Label, self.scope_index, label),
            LocalValue::Label,
        );
    }
}
