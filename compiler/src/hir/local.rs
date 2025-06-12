use crate::hir::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockKind {
    Block,
    /// Loop blocks have an implicit `continue` at the end.
    /// Their type is inferred from `break` expressions, not the final
    /// expression.
    Loop,
}

#[derive(Debug, Clone)]
pub struct BlockScope {
    pub kind: BlockKind,
    pub label: Option<SymbolU32>,
    pub parent: Option<ScopeIndex>,
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
        let scope = &mut self.scopes[scope_index.0 as usize];
        let local_index = LocalIndex(scope.locals.len() as u32);
        scope.locals.push(local);
        local_index
    }

    pub fn get_local(&self, scope_index: ScopeIndex, local_index: LocalIndex) -> Option<&Local> {
        self.scopes[scope_index.0 as usize]
            .locals
            .get(local_index.0 as usize)
    }
}

#[derive(Debug, Clone)]
pub struct LocalContext {
    pub lookup: HashMap<(ScopeIndex, SymbolU32), LocalIndex>,
    pub func_index: FuncIndex,
    pub scope_index: ScopeIndex,
    pub frame: StackFrame,
}

impl LocalContext {
    pub fn push_local(&mut self, local: Local) -> LocalIndex {
        let name = local.name;
        let index = self.frame.push_local(self.scope_index, local);
        self.lookup.insert((self.scope_index, name), index);
        index
    }

    pub fn resolve_local(&self, symbol: SymbolU32) -> Option<(ScopeIndex, LocalIndex)> {
        let mut scope_index = self.scope_index;

        loop {
            if let Some(&value) = self.lookup.get(&(scope_index, symbol)) {
                return Some((scope_index, value));
            }

            scope_index = self.frame.scopes[scope_index.0 as usize].parent?;
        }
    }

    pub fn enter_block<T>(&mut self, block: BlockScope, handler: impl FnOnce(&mut Self) -> T) -> T {
        let parent_scope_index = self.scope_index;
        self.scope_index = ScopeIndex(self.frame.scopes.len() as u32);
        self.frame.scopes.push(block);

        let result = handler(self);

        self.scope_index = parent_scope_index;
        result
    }

    pub fn resolve_label(&self, symbol: SymbolU32) -> Option<ScopeIndex> {
        let mut scope_index = self.scope_index;

        loop {
            let scope = &self.frame.scopes[scope_index.0 as usize];
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
            let scope = &self.frame.scopes[scope_index.0 as usize];
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
