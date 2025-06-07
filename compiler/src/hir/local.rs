use crate::hir::*;

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

#[derive(Debug, Clone, Copy)]
pub enum LocalValue {
    Local(LocalIndex),
    Label(ScopeIndex),
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
            match self.lookup.get(&(LocalType::Local, scope_index, symbol)) {
                Some(&value) => {
                    return Some((
                        scope_index,
                        match value {
                            LocalValue::Local(local_index) => local_index,
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
            match self.lookup.get(&(LocalType::Label, scope_index, symbol)) {
                Some(&LocalValue::Label(label_index)) => return Some(label_index),
                _ => match self.frame.scopes.get(scope_index.0 as usize) {
                    Some(scope) => scope_index = scope.parent_scope?,
                    None => break,
                },
            }
        }

        None
    }

    pub fn enter_scope<T>(
        &mut self,
        result: Option<Type>,
        handler: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let new_scope = BlockScope {
            parent_scope: Some(self.scope_index),
            locals: Vec::new(),
            result,
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
        result: Option<Type>,
        handler: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.lookup.insert(
            (LocalType::Label, self.scope_index, label),
            LocalValue::Label(ScopeIndex(self.frame.scopes.len() as u32)),
        );
        self.enter_scope(result, handler)
    }
}
