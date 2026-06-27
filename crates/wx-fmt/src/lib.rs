use string_interner::symbol::SymbolU32;
use wx_compiler::ast;

#[cfg(test)]
mod tests;

type NodeId = u32;

macro_rules! define_text {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $($variant:ident => $str:literal,)*
        }
    ) => {
        $(#[$meta])*
        #[derive(Clone, Copy)]
        $vis enum $name {
            $($variant,)*
        }

        impl $name {
            fn as_str(self) -> &'static str {
                match self {
                    $(Self::$variant => $str,)*
                }
            }
        }
    };
}

define_text! {
    enum Text {
        // keywords with trailing space
        Fn      => "fn ",
        Pub     => "pub ",
        Struct  => "struct ",
        Impl    => "impl ",
        Trait   => "trait ",
        Module  => "module ",
        Use     => "use ",
        Memory  => "memory ",
        Import  => "import ",
        Global  => "global ",
        Mut     => "mut ",
        Const   => "const ",
        TypeKw  => "type ",
        Typeset => "typeset ",
        Local   => "local ",
        Loop    => "loop ",
        If      => "if ",
        Enum    => "enum ",
        // keywords without trailing space
        Break       => "break",
        Continue    => "continue",
        Return      => "return",
        Unreachable => "unreachable",
        // punctuation
        Semi             => ";",
        Comma            => ",",
        Dot              => ".",
        DotStar          => ".*",
        DotDot           => "..",
        LBrace           => "{",
        LBraceSpace      => "{ ",
        RBrace           => "}",
        LParen           => "(",
        RParen           => ")",
        LBracket         => "[",
        RBracket         => "]",
        Lt               => "<",
        Gt               => ">",
        Star             => "*",
        Underscore       => "_",
        SliceBrackets    => "[]",
        HashLBracket     => "#[",
        ColonColon       => "::",
        ColonColonLt     => "::<",
        ColonColonLBrace => "::{",
        // spaced punctuation / keywords
        Arrow            => " -> ",
        As               => " as ",
        ColonSp          => ": ",
        CommaSp          => ", ",
        PlusSp           => " + ",
        EqSp             => " = ",
        EqBare           => " =",
        Else             => " else ",
        ForKw            => " for ",
        LabelColon       => " :",
        Space            => " ",
        SpaceLBrace      => " {",
        SpaceLBraceSpace => " { ",
        SpaceRBrace      => " }",
        Where        => " where ",
        // compound tokens
        ExportLBrace => "export {",
        FnParen      => "fn(",
        MinPages     => "min_pages: ",
        MaxPages     => "max_pages: ",
        // binary operators
        Add       => "+",
        Sub       => "-",
        Div       => "/",
        Rem       => "%",
        EqEq      => "==",
        NotEq     => "!=",
        LtEq      => "<=",
        GtEq      => ">=",
        AndAnd    => "&&",
        OrOr      => "||",
        Assign    => "=",
        AddAssign => "+=",
        SubAssign => "-=",
        MulAssign => "*=",
        DivAssign => "/=",
        RemAssign => "%=",
        Amp       => "&",
        Pipe      => "|",
        Caret     => "^",
        LtLt      => "<<",
        GtGt      => ">>",
        // unary-only
        Bang => "!",
    }
}

impl From<ast::BinaryOp> for Text {
    fn from(op: ast::BinaryOp) -> Self {
        match op {
            ast::BinaryOp::Add => Text::Add,
            ast::BinaryOp::Sub => Text::Sub,
            ast::BinaryOp::Mul => Text::Star,
            ast::BinaryOp::Div => Text::Div,
            ast::BinaryOp::Rem => Text::Rem,
            ast::BinaryOp::Eq => Text::EqEq,
            ast::BinaryOp::NotEq => Text::NotEq,
            ast::BinaryOp::Less => Text::Lt,
            ast::BinaryOp::LessEq => Text::LtEq,
            ast::BinaryOp::Greater => Text::Gt,
            ast::BinaryOp::GreaterEq => Text::GtEq,
            ast::BinaryOp::And => Text::AndAnd,
            ast::BinaryOp::Or => Text::OrOr,
            ast::BinaryOp::Assign => Text::Assign,
            ast::BinaryOp::AddAssign => Text::AddAssign,
            ast::BinaryOp::SubAssign => Text::SubAssign,
            ast::BinaryOp::MulAssign => Text::MulAssign,
            ast::BinaryOp::DivAssign => Text::DivAssign,
            ast::BinaryOp::RemAssign => Text::RemAssign,
            ast::BinaryOp::BitAnd => Text::Amp,
            ast::BinaryOp::BitOr => Text::Pipe,
            ast::BinaryOp::BitXor => Text::Caret,
            ast::BinaryOp::LeftShift => Text::LtLt,
            ast::BinaryOp::RightShift => Text::GtGt,
        }
    }
}

impl From<ast::UnaryOp> for Text {
    fn from(op: ast::UnaryOp) -> Self {
        match op {
            ast::UnaryOp::InvertSign => Text::Sub,
            ast::UnaryOp::Not => Text::Bang,
            ast::UnaryOp::BitNot => Text::Caret,
        }
    }
}

#[derive(Clone, Copy)]
enum Node {
    Text(Text),
    /// A slice of the original source addressed by span
    SourceText(ast::TextSpan),
    /// An interned symbol resolved during rendering
    Symbol {
        symbol: SymbolU32,
        len: u32,
    },
    /// A possible line break, otherwise rendered as a space
    SoftLine,
    /// A possible line break, otherwise nothing
    Line,
    /// Always emit a newline followed by indentation, regardless of mode
    HardLine,
    /// Always emit a bare newline with no indentation — use to insert a blank line between items
    BlankLine,
    /// A sequence of nodes concatenated together
    /// Children stored in `Arena::children[start .. start + len]`.
    Concat {
        start: u32,
        len: u32,
    },
    /// All lines under this node must either break or not break together
    Group(NodeId),
    /// Increases the indentation level for all lines within this node
    Indent(NodeId),
    /// A trailing comma emitted only in Break mode
    IfBreakComma,
}

struct Arena {
    nodes: Vec<Node>,
    children: Vec<NodeId>,
}

impl Arena {
    fn new() -> Self {
        Self {
            nodes: Vec::new(),
            children: Vec::new(),
        }
    }

    #[inline]
    fn alloc(&mut self, node: Node) -> NodeId {
        let id = self.nodes.len() as u32;
        self.nodes.push(node);
        id
    }

    fn concat(&mut self, children: Vec<NodeId>) -> NodeId {
        match children.len() {
            0 => self.alloc(Node::Concat { start: 0, len: 0 }),
            1 => children[0],
            _ => {
                let start = self.children.len() as u32;
                let len = children.len() as u32;
                self.children.extend_from_slice(&children);
                self.alloc(Node::Concat { start, len })
            }
        }
    }

    #[inline]
    fn concat2(&mut self, a: NodeId, b: NodeId) -> NodeId {
        let start = self.children.len() as u32;
        self.children.push(a);
        self.children.push(b);
        self.alloc(Node::Concat { start, len: 2 })
    }

    #[inline]
    fn concat3(&mut self, a: NodeId, b: NodeId, c: NodeId) -> NodeId {
        let start = self.children.len() as u32;
        self.children.push(a);
        self.children.push(b);
        self.children.push(c);
        self.alloc(Node::Concat { start, len: 3 })
    }

    #[inline]
    fn concat4(&mut self, a: NodeId, b: NodeId, c: NodeId, d: NodeId) -> NodeId {
        let start = self.children.len() as u32;
        self.children.push(a);
        self.children.push(b);
        self.children.push(c);
        self.children.push(d);
        self.alloc(Node::Concat { start, len: 4 })
    }

    #[inline]
    fn concat5(&mut self, a: NodeId, b: NodeId, c: NodeId, d: NodeId, e: NodeId) -> NodeId {
        let start = self.children.len() as u32;
        self.children.push(a);
        self.children.push(b);
        self.children.push(c);
        self.children.push(d);
        self.children.push(e);
        self.alloc(Node::Concat { start, len: 5 })
    }

    #[inline]
    fn group(&mut self, inner: NodeId) -> NodeId {
        self.alloc(Node::Group(inner))
    }

    #[inline]
    fn indent(&mut self, inner: NodeId) -> NodeId {
        self.alloc(Node::Indent(inner))
    }
}

struct Builder<'a> {
    interner: &'a ast::StringInterner,
    source: &'a str,
    comments: &'a ast::CommentMap,
    arena: Arena,
}

impl<'a> Builder<'a> {
    #[inline]
    fn text(&mut self, t: Text) -> NodeId {
        self.arena.alloc(Node::Text(t))
    }

    #[inline]
    fn hard_line(&mut self) -> NodeId {
        self.arena.alloc(Node::HardLine)
    }

    #[inline]
    fn soft_line(&mut self) -> NodeId {
        self.arena.alloc(Node::SoftLine)
    }

    #[inline]
    fn line(&mut self) -> NodeId {
        self.arena.alloc(Node::Line)
    }

    #[inline]
    fn blank_line(&mut self) -> NodeId {
        self.arena.alloc(Node::BlankLine)
    }

    #[inline]
    fn if_break_comma(&mut self) -> NodeId {
        self.arena.alloc(Node::IfBreakComma)
    }

    #[inline]
    fn symbol(&mut self, symbol: SymbolU32) -> NodeId {
        let len = self.interner.resolve(symbol).unwrap().len() as u32;
        self.arena.alloc(Node::Symbol { symbol, len })
    }

    #[inline]
    fn source_text(&mut self, span: ast::TextSpan) -> NodeId {
        self.arena.alloc(Node::SourceText(span))
    }

    fn count_blank_lines(source: &str, end_pos: usize, start_pos: usize) -> usize {
        if start_pos <= end_pos {
            return 0;
        }
        let between = &source[end_pos..start_pos];
        let newlines = between.matches('\n').count();
        newlines.saturating_sub(1).min(1)
    }

    fn build(&mut self, ast: &ast::AST) -> NodeId {
        self.build_item_list(&ast.items, true)
    }

    fn build_item_list(
        &mut self,
        items: &[ast::Separated<ast::Spanned<ast::Item>>],
        toplevel: bool,
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = Vec::new();

        if toplevel {
            if let Some(first) = items.first() {
                let spans: Vec<ast::TextSpan> = self
                    .comments
                    .between(0, first.inner.span.start)
                    .iter()
                    .map(|c| c.span)
                    .collect();
                for span in spans {
                    nodes.push(self.source_text(span));
                    nodes.push(self.hard_line());
                }
            }
        }

        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                let prev = &items[index - 1];
                let gap_spans: Vec<ast::TextSpan> = self
                    .comments
                    .between(prev.inner.span.end, item.inner.span.start)
                    .iter()
                    .map(|c| c.span)
                    .collect();

                if prev.inner.inner.is_block_like() || item.inner.inner.is_block_like() {
                    nodes.push(self.blank_line());
                } else {
                    let blank_end = gap_spans
                        .first()
                        .map_or(item.inner.span.start as usize, |s| s.start as usize);
                    let blank = Self::count_blank_lines(
                        self.source,
                        prev.inner.span.end as usize,
                        blank_end,
                    );
                    for _ in 0..blank {
                        nodes.push(self.blank_line());
                    }
                }
                for span in &gap_spans {
                    nodes.push(self.hard_line());
                    nodes.push(self.source_text(*span));
                }
                nodes.push(self.hard_line());
            }
            let id = self.build_item(&item.inner.inner);
            nodes.push(id);
        }
        self.arena.concat(nodes)
    }

    fn build_item(&mut self, item: &ast::Item) -> NodeId {
        match item {
            ast::Item::Function {
                signature,
                block,
                attributes,
                pub_span,
                ..
            } => {
                let mut items: Vec<NodeId> = Vec::new();
                self.build_attributes(&mut items, attributes);
                if pub_span.is_some() {
                    items.push(self.text(Text::Pub));
                }
                self.build_function_signature(&mut items, signature);
                items.push(self.text(Text::Space));
                let body = self.build_fn_body(block);
                items.push(body);
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
            ast::Item::FunctionDeclaration {
                pub_span,
                attributes,
                signature,
                ..
            } => {
                let mut items: Vec<NodeId> = Vec::new();
                self.build_attributes(&mut items, attributes);
                if pub_span.is_some() {
                    items.push(self.text(Text::Pub));
                }
                self.build_function_signature(&mut items, signature);
                items.push(self.text(Text::Semi));
                self.arena.concat(items)
            }
            ast::Item::IntrinsicFunction { signature, .. } => {
                let mut items: Vec<NodeId> = Vec::new();
                self.build_function_signature(&mut items, signature);
                items.push(self.text(Text::Semi));
                self.arena.concat(items)
            }
            ast::Item::Global {
                pub_span,
                mut_span,
                name,
                ty: type_annotation,
                value,
                ..
            } => self.build_global_definition(*pub_span, *mut_span, name, type_annotation, value),
            ast::Item::Export { entries } => self.build_export_definition(entries),
            ast::Item::Import {
                module,
                alias,
                entries,
            } => self.build_import_definition(module, alias, entries),
            ast::Item::Memory {
                name, kind, config, ..
            } => {
                let name_id = self.symbol(name.inner);
                let kind_id = self.build_bound_expression(&kind.inner);
                let mut parts: Vec<NodeId> = vec![
                    self.text(Text::Memory),
                    name_id,
                    self.text(Text::ColonSp),
                    kind_id,
                ];
                if let Some(cfg) = config {
                    let mut fields: Vec<NodeId> = Vec::new();
                    if let Some(min) = &cfg.min_pages {
                        let span = min.span;
                        fields.push(self.text(Text::MinPages));
                        fields.push(self.source_text(span));
                    }
                    if let Some(max) = &cfg.max_pages {
                        let span = max.span;
                        if !fields.is_empty() {
                            fields.push(self.text(Text::CommaSp));
                        }
                        fields.push(self.text(Text::MaxPages));
                        fields.push(self.source_text(span));
                    }
                    if !fields.is_empty() {
                        parts.push(self.text(Text::SpaceLBraceSpace));
                        parts.extend(fields);
                        parts.push(self.text(Text::SpaceRBrace));
                    }
                }
                parts.push(self.text(Text::Semi));
                self.arena.concat(parts)
            }
            ast::Item::Enum {
                pub_span,
                repr,
                name,
                variants,
                ..
            } => self.build_enum_definition(*pub_span, repr, name, variants),
            ast::Item::Impl {
                type_params,
                target,
                items,
                ..
            } => self.build_impl_definition(type_params, target, items),
            ast::Item::ImplTrait {
                trait_name,
                target,
                items,
                ..
            } => self.build_impl_trait_definition(trait_name, target, items),
            ast::Item::Const {
                pub_span,
                name,
                ty,
                value,
                ..
            } => self.build_const_definition(*pub_span, name, ty, value),
            ast::Item::Module {
                pub_span,
                name,
                items,
            } => self.build_module_definition(*pub_span, name, items),
            ast::Item::ModuleDeclaration { pub_span, name } => {
                self.build_module_declaration(*pub_span, name)
            }
            ast::Item::Trait {
                pub_span,
                name,
                supertraits,
                items,
                ..
            } => self.build_trait_definition(*pub_span, name, supertraits.as_ref(), items),
            ast::Item::Struct {
                id: _,
                name,
                type_params,
                fields,
                pub_span,
            } => self.build_struct_declaration(name, type_params, fields, *pub_span),
            ast::Item::TypeSet {
                pub_span,
                name,
                members,
                ..
            } => {
                let mut items: Vec<NodeId> = Vec::new();
                if pub_span.is_some() {
                    items.push(self.text(Text::Pub));
                }
                items.push(self.text(Text::Typeset));
                items.push(self.symbol(name.inner));
                items.push(self.text(Text::SpaceLBraceSpace));
                for (i, m) in members.iter().enumerate() {
                    if i > 0 {
                        items.push(self.text(Text::CommaSp));
                    }
                    let ty = self.build_type_expression(&m.inner.inner);
                    items.push(ty);
                }
                items.push(self.text(Text::SpaceRBrace));
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
            ast::Item::Use { path } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::Use)];
                for (i, segment) in path.iter().enumerate() {
                    if i > 0 {
                        items.push(self.text(Text::ColonColon));
                    }
                    items.push(self.symbol(segment.inner));
                }
                items.push(self.text(Text::ColonColon));
                items.push(self.text(Text::Star));
                items.push(self.text(Text::Semi));
                self.arena.concat(items)
            }
        }
    }

    fn build_import_definition(
        &mut self,
        module: &ast::Spanned<SymbolU32>,
        alias: &Option<ast::Spanned<SymbolU32>>,
        entries: &[ast::Separated<ast::Spanned<ast::ImportEntry>>],
    ) -> NodeId {
        let mut items: Vec<NodeId> = vec![self.text(Text::Import), self.symbol(module.inner)];

        if let Some(alias) = alias {
            items.push(self.text(Text::As));
            items.push(self.symbol(alias.inner));
        }

        items.push(self.text(Text::SpaceLBrace));

        if !entries.is_empty() {
            let mut entry_items: Vec<NodeId> = vec![self.line()];

            for (index, entry) in entries.iter().enumerate() {
                let mut entry_nodes: Vec<NodeId> = Vec::new();

                if let Some(ext_name) = &entry.inner.inner.external_name {
                    entry_nodes.push(self.symbol(ext_name.inner));
                    entry_nodes.push(self.text(Text::ColonSp));
                }

                match &entry.inner.inner.declaration {
                    ast::ImportDeclaration::Function { signature, .. } => {
                        self.build_function_signature(&mut entry_nodes, signature);
                        let concat = self.arena.concat(entry_nodes);
                        let group = self.arena.group(concat);
                        entry_nodes = vec![group];
                    }
                    ast::ImportDeclaration::Global {
                        mut_span, name, ty, ..
                    } => {
                        entry_nodes.push(self.text(Text::Global));
                        if mut_span.is_some() {
                            entry_nodes.push(self.text(Text::Mut));
                        }
                        entry_nodes.push(self.symbol(name.inner));
                        entry_nodes.push(self.text(Text::ColonSp));
                        entry_nodes.push(self.build_type_expression(&ty.inner));
                    }
                    ast::ImportDeclaration::Memory { name, kind, .. } => {
                        entry_nodes.push(self.text(Text::Memory));
                        entry_nodes.push(self.symbol(name.inner));
                        entry_nodes.push(self.text(Text::ColonSp));
                        entry_nodes.push(self.build_bound_expression(&kind.inner));
                    }
                }

                if entry.separator.is_some() {
                    entry_nodes.push(self.text(Text::Semi));
                }

                let entry_concat = self.arena.concat(entry_nodes);
                entry_items.push(entry_concat);
                if index + 1 < entries.len() {
                    entry_items.push(self.line());
                }
            }

            let concat = self.arena.concat(entry_items);
            let indented = self.arena.indent(concat);
            items.push(indented);
        }

        items.push(self.line());
        items.push(self.text(Text::RBrace));
        self.arena.concat(items)
    }

    fn build_module_definition(
        &mut self,
        pub_span: Option<ast::TextSpan>,
        name: &ast::Spanned<SymbolU32>,
        items: &[ast::Separated<ast::Spanned<ast::Item>>],
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = Vec::new();
        if pub_span.is_some() {
            nodes.push(self.text(Text::Pub));
        }
        nodes.push(self.text(Text::Module));
        nodes.push(self.symbol(name.inner));
        nodes.push(self.text(Text::SpaceLBrace));

        if !items.is_empty() {
            let body = self.build_item_list(items, false);
            let hl = self.hard_line();
            let inner = self.arena.concat2(hl, body);
            let indented = self.arena.indent(inner);
            nodes.push(indented);
            nodes.push(self.hard_line());
        }

        nodes.push(self.text(Text::RBrace));
        let concat = self.arena.concat(nodes);
        self.arena.group(concat)
    }

    fn build_module_declaration(
        &mut self,
        pub_span: Option<ast::TextSpan>,
        name: &ast::Spanned<SymbolU32>,
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = Vec::new();
        if pub_span.is_some() {
            nodes.push(self.text(Text::Pub));
        }
        nodes.push(self.text(Text::Module));
        nodes.push(self.symbol(name.inner));
        nodes.push(self.text(Text::Semi));
        self.arena.concat(nodes)
    }

    fn build_impl_definition(
        &mut self,
        type_params: &[ast::TypeParam],
        target: &ast::Spanned<ast::TypeExpression>,
        items: &[ast::Separated<ast::Spanned<ast::ImplItem>>],
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = vec![self.text(Text::Impl)];
        self.build_type_params(&mut nodes, type_params);
        if !type_params.is_empty() {
            nodes.push(self.text(Text::Space));
        }
        nodes.push(self.build_type_expression(&target.inner));
        nodes.push(self.text(Text::SpaceLBrace));

        if !items.is_empty() {
            let body = self.build_impl_item_list(items);
            let hl = self.hard_line();
            let inner = self.arena.concat2(hl, body);
            nodes.push(self.arena.indent(inner));
            nodes.push(self.hard_line());
        }

        nodes.push(self.text(Text::RBrace));
        let concat = self.arena.concat(nodes);
        self.arena.group(concat)
    }

    fn build_impl_trait_definition(
        &mut self,
        trait_name: &[ast::PathSegment],
        target: &ast::Spanned<ast::TypeExpression>,
        items: &[ast::Separated<ast::Spanned<ast::ImplItem>>],
    ) -> NodeId {
        let impl_kw = self.text(Text::Impl);
        let trait_id = self.build_path_segments(trait_name);
        let for_kw = self.text(Text::ForKw);
        let target_id = self.build_type_expression(&target.inner);
        let brace = self.text(Text::SpaceLBrace);
        let mut nodes: Vec<NodeId> = vec![impl_kw, trait_id, for_kw, target_id, brace];

        if !items.is_empty() {
            let body = self.build_impl_item_list(items);
            let hl = self.hard_line();
            let inner = self.arena.concat2(hl, body);
            nodes.push(self.arena.indent(inner));
            nodes.push(self.hard_line());
        }

        nodes.push(self.text(Text::RBrace));
        let concat = self.arena.concat(nodes);
        self.arena.group(concat)
    }

    fn build_impl_item_list(
        &mut self,
        items: &[ast::Separated<ast::Spanned<ast::ImplItem>>],
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = Vec::new();
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                nodes.push(self.blank_line());
                let prev = &items[index - 1];
                let gap_spans: Vec<ast::TextSpan> = self
                    .comments
                    .between(prev.inner.span.end, item.inner.span.start)
                    .iter()
                    .map(|c| c.span)
                    .collect();
                for span in gap_spans {
                    nodes.push(self.hard_line());
                    nodes.push(self.source_text(span));
                }
                nodes.push(self.hard_line());
            }
            nodes.push(self.build_impl_item(&item.inner.inner));
        }
        self.arena.concat(nodes)
    }

    fn build_impl_item(&mut self, item: &ast::ImplItem) -> NodeId {
        match item {
            ast::ImplItem::Method {
                pub_span,
                attributes,
                signature,
                block,
                ..
            } => {
                let mut nodes: Vec<NodeId> = Vec::new();
                self.build_attributes(&mut nodes, attributes);
                if pub_span.is_some() {
                    nodes.push(self.text(Text::Pub));
                }
                self.build_function_signature(&mut nodes, signature);
                nodes.push(self.text(Text::Space));
                nodes.push(self.build_fn_body(block));
                let concat = self.arena.concat(nodes);
                self.arena.group(concat)
            }
            ast::ImplItem::Const {
                name, ty, value, ..
            } => {
                let mut nodes: Vec<NodeId> = vec![self.text(Text::Const), self.symbol(name.inner)];
                if let Some(ty) = ty {
                    nodes.push(self.text(Text::ColonSp));
                    nodes.push(self.build_type_expression(&ty.inner));
                }
                nodes.push(self.text(Text::EqSp));
                nodes.push(self.build_expression(value));
                nodes.push(self.text(Text::Semi));
                self.arena.concat(nodes)
            }
            ast::ImplItem::AssociatedType { name, ty, .. } => {
                let type_kw = self.text(Text::TypeKw);
                let name_sym = self.symbol(name.inner);
                let eq = self.text(Text::EqSp);
                let ty_id = self.build_type_expression(&ty.inner);
                let semi = self.text(Text::Semi);
                self.arena.concat5(type_kw, name_sym, eq, ty_id, semi)
            }
        }
    }

    fn build_trait_definition(
        &mut self,
        pub_span: Option<ast::TextSpan>,
        name: &ast::Spanned<SymbolU32>,
        supertraits: Option<&ast::Spanned<ast::BoundExpression>>,
        items: &[ast::Separated<ast::Spanned<ast::TraitItem>>],
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = Vec::new();
        if pub_span.is_some() {
            nodes.push(self.text(Text::Pub));
        }
        nodes.push(self.text(Text::Trait));
        nodes.push(self.symbol(name.inner));

        if let Some(spanned) = supertraits {
            nodes.push(self.text(Text::ColonSp));
            nodes.push(self.build_bound_expression(&spanned.inner));
        }

        nodes.push(self.text(Text::SpaceLBrace));

        if !items.is_empty() {
            let body = self.build_trait_item_list(items);
            let hl = self.hard_line();
            let inner = self.arena.concat2(hl, body);
            nodes.push(self.arena.indent(inner));
            nodes.push(self.hard_line());
        }

        nodes.push(self.text(Text::RBrace));
        let concat = self.arena.concat(nodes);
        self.arena.group(concat)
    }

    fn build_trait_item_list(
        &mut self,
        items: &[ast::Separated<ast::Spanned<ast::TraitItem>>],
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = Vec::new();
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                nodes.push(self.blank_line());
                let prev = &items[index - 1];
                let gap_spans: Vec<ast::TextSpan> = self
                    .comments
                    .between(prev.inner.span.end, item.inner.span.start)
                    .iter()
                    .map(|c| c.span)
                    .collect();
                for span in gap_spans {
                    nodes.push(self.hard_line());
                    nodes.push(self.source_text(span));
                }
                nodes.push(self.hard_line());
            }
            nodes.push(self.build_trait_item(&item.inner.inner));
        }
        self.arena.concat(nodes)
    }

    fn build_trait_item(&mut self, item: &ast::TraitItem) -> NodeId {
        match item {
            ast::TraitItem::Function {
                attributes,
                signature,
                body,
                ..
            } => {
                let mut nodes: Vec<NodeId> = Vec::new();
                self.build_attributes(&mut nodes, attributes);
                self.build_function_signature(&mut nodes, signature);
                match body {
                    Some(body) => {
                        nodes.push(self.text(Text::Space));
                        nodes.push(self.build_fn_body(body));
                        let concat = self.arena.concat(nodes);
                        self.arena.group(concat)
                    }
                    None => {
                        nodes.push(self.text(Text::Semi));
                        self.arena.concat(nodes)
                    }
                }
            }
            ast::TraitItem::Const { name, ty, .. } => {
                let const_kw = self.text(Text::Const);
                let name_sym = self.symbol(name.inner);
                let colon = self.text(Text::ColonSp);
                let ty_id = self.build_type_expression(&ty.inner);
                let semi = self.text(Text::Semi);
                self.arena.concat5(const_kw, name_sym, colon, ty_id, semi)
            }
            ast::TraitItem::AssociatedType { name, bounds, .. } => {
                let mut nodes: Vec<NodeId> = vec![self.text(Text::TypeKw), self.symbol(name.inner)];
                if let Some(b) = bounds {
                    nodes.push(self.text(Text::ColonSp));
                    nodes.push(self.build_bound_expression(&b.inner));
                }
                nodes.push(self.text(Text::Semi));
                self.arena.concat(nodes)
            }
        }
    }

    fn build_const_definition(
        &mut self,
        pub_span: Option<ast::TextSpan>,
        name: &ast::Spanned<SymbolU32>,
        ty: &Option<Box<ast::Spanned<ast::TypeExpression>>>,
        value: &ast::Spanned<ast::Expression>,
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = Vec::new();
        if pub_span.is_some() {
            nodes.push(self.text(Text::Pub));
        }
        nodes.push(self.text(Text::Const));
        nodes.push(self.symbol(name.inner));
        if let Some(ty) = ty {
            nodes.push(self.text(Text::ColonSp));
            nodes.push(self.build_type_expression(&ty.inner));
        }
        nodes.push(self.text(Text::EqSp));
        nodes.push(self.build_expression(value));
        nodes.push(self.text(Text::Semi));
        self.arena.concat(nodes)
    }

    fn build_enum_definition(
        &mut self,
        pub_span: Option<ast::TextSpan>,
        repr: &Option<Box<ast::Spanned<ast::TypeExpression>>>,
        name: &ast::Spanned<SymbolU32>,
        variants: &[ast::Separated<ast::Spanned<ast::EnumVariant>>],
    ) -> NodeId {
        let mut nodes: Vec<NodeId> = Vec::new();
        if pub_span.is_some() {
            nodes.push(self.text(Text::Pub));
        }
        nodes.push(self.text(Text::Enum));
        nodes.push(self.symbol(name.inner));
        if let Some(repr) = repr {
            nodes.push(self.text(Text::ColonSp));
            nodes.push(self.build_type_expression(&repr.inner));
        }
        nodes.push(self.text(Text::SpaceLBrace));

        if !variants.is_empty() {
            let mut variant_items: Vec<NodeId> = vec![self.hard_line()];
            for (index, variant) in variants.iter().enumerate() {
                let mut vn: Vec<NodeId> = vec![self.symbol(variant.inner.inner.name.inner)];
                if let Some(value) = &variant.inner.inner.value {
                    vn.push(self.text(Text::EqSp));
                    vn.push(self.build_expression(value));
                }
                if index + 1 < variants.len() {
                    vn.push(self.text(Text::Comma));
                } else {
                    vn.push(self.if_break_comma());
                }
                variant_items.push(self.arena.concat(vn));
                if index + 1 < variants.len() {
                    variant_items.push(self.hard_line());
                }
            }

            let concat = self.arena.concat(variant_items);
            nodes.push(self.arena.indent(concat));
            nodes.push(self.hard_line());
        }

        nodes.push(self.text(Text::RBrace));
        self.arena.concat(nodes)
    }

    fn build_struct_declaration(
        &mut self,
        name: &ast::Spanned<SymbolU32>,
        type_params: &[ast::TypeParam],
        fields: &[ast::Separated<ast::Spanned<ast::StructField>>],
        pub_span: Option<ast::TextSpan>,
    ) -> NodeId {
        let mut items: Vec<NodeId> = Vec::new();
        if pub_span.is_some() {
            items.push(self.text(Text::Pub));
        }
        items.push(self.text(Text::Struct));
        items.push(self.symbol(name.inner));
        self.build_type_params(&mut items, type_params);
        items.push(self.text(Text::SpaceLBrace));

        if !fields.is_empty() {
            let field_count = fields.len();
            let mut field_items: Vec<NodeId> = vec![self.hard_line()];
            for (index, field) in fields.iter().enumerate() {
                let mut fn_: Vec<NodeId> = Vec::new();
                if field.inner.inner.pub_span.is_some() {
                    fn_.push(self.text(Text::Pub));
                }
                fn_.push(self.symbol(field.inner.inner.name.inner));
                fn_.push(self.text(Text::ColonSp));
                fn_.push(self.build_type_expression(&field.inner.inner.ty.inner));
                fn_.push(self.text(Text::Comma));
                field_items.push(self.arena.concat(fn_));
                if index + 1 < field_count {
                    field_items.push(self.hard_line());
                }
            }

            let concat = self.arena.concat(field_items);
            items.push(self.arena.indent(concat));
            items.push(self.hard_line());
        }

        items.push(self.text(Text::RBrace));
        self.arena.concat(items)
    }

    fn build_export_definition(
        &mut self,
        entries: &[ast::Separated<ast::Spanned<ast::ExportEntry>>],
    ) -> NodeId {
        let mut items: Vec<NodeId> = vec![self.text(Text::ExportLBrace)];

        if !entries.is_empty() {
            let mut entry_items: Vec<NodeId> = vec![self.line()];
            for (index, entry) in entries.iter().enumerate() {
                let mut en: Vec<NodeId> = vec![self.symbol(entry.inner.inner.name.inner)];
                if let Some(alias) = &entry.inner.inner.alias {
                    en.push(self.text(Text::As));
                    en.push(self.symbol(alias.inner));
                }
                if entry.separator.is_some() {
                    en.push(self.text(Text::Comma));
                }
                entry_items.push(self.arena.concat(en));
                if index + 1 < entries.len() {
                    entry_items.push(self.line());
                }
            }

            let concat = self.arena.concat(entry_items);
            items.push(self.arena.indent(concat));
        }

        items.push(self.line());
        items.push(self.text(Text::RBrace));
        self.arena.concat(items)
    }

    fn build_attributes(&mut self, out: &mut Vec<NodeId>, attributes: &[ast::Attribute]) {
        for attr in attributes {
            out.push(self.text(Text::HashLBracket));
            out.push(self.symbol(attr.name.inner));
            out.push(self.text(Text::RBracket));
            out.push(self.hard_line());
        }
    }

    fn build_type_params(&mut self, out: &mut Vec<NodeId>, type_params: &[ast::TypeParam]) {
        if type_params.is_empty() {
            return;
        }
        out.push(self.text(Text::Lt));
        let mut inner: Vec<NodeId> = vec![self.line()];
        for (index, param) in type_params.iter().enumerate() {
            inner.push(self.symbol(param.name.inner));
            if let Some(bounds) = &param.bounds {
                inner.push(self.text(Text::ColonSp));
                inner.push(self.build_bound_expression(&bounds.inner));
            }
            if index + 1 < type_params.len() {
                inner.push(self.text(Text::Comma));
                inner.push(self.soft_line());
            } else {
                inner.push(self.if_break_comma());
            }
        }
        let inner_concat = self.arena.concat(inner);
        out.push(self.arena.indent(inner_concat));
        out.push(self.line());
        out.push(self.text(Text::Gt));
    }

    fn build_function_signature(
        &mut self,
        out: &mut Vec<NodeId>,
        signature: &ast::FunctionSignature,
    ) {
        out.push(self.text(Text::Fn));
        out.push(self.symbol(signature.name.inner));
        self.build_type_params(out, &signature.type_params);
        out.push(self.text(Text::LParen));

        if !signature.params.is_empty() {
            let mut params: Vec<NodeId> = vec![self.line()];
            for (index, param) in signature.params.iter().enumerate() {
                if param.inner.inner.mut_span.is_some() {
                    params.push(self.text(Text::Mut));
                }
                params.push(self.symbol(param.inner.inner.name.inner));
                if let Some(ty) = &param.inner.inner.ty {
                    params.push(self.text(Text::ColonSp));
                    params.push(self.build_type_expression(&ty.inner));
                }
                if index + 1 < signature.params.len() {
                    params.push(self.text(Text::Comma));
                    params.push(self.soft_line());
                } else {
                    params.push(self.if_break_comma());
                }
            }
            let params_concat = self.arena.concat(params);
            out.push(self.arena.indent(params_concat));
            out.push(self.line());
        }

        out.push(self.text(Text::RParen));
        if let Some(result) = &signature.result {
            out.push(self.text(Text::Arrow));
            out.push(self.build_type_expression(&result.inner));
        }
    }

    fn build_global_definition(
        &mut self,
        pub_span: Option<ast::TextSpan>,
        mut_span: Option<ast::TextSpan>,
        name: &ast::Spanned<SymbolU32>,
        type_annotation: &Option<Box<ast::Spanned<ast::TypeExpression>>>,
        value: &Box<ast::Spanned<ast::Expression>>,
    ) -> NodeId {
        let mut items: Vec<NodeId> = Vec::new();
        if pub_span.is_some() {
            items.push(self.text(Text::Pub));
        }
        items.push(self.text(Text::Global));
        if mut_span.is_some() {
            items.push(self.text(Text::Mut));
        }
        items.push(self.symbol(name.inner));
        if let Some(annotation) = type_annotation {
            items.push(self.text(Text::ColonSp));
            items.push(self.build_type_expression(&annotation.inner));
        }
        items.push(self.text(Text::EqSp));
        items.push(self.build_expression(value));
        items.push(self.text(Text::Semi));
        self.arena.concat(items)
    }

    fn build_fn_body(&mut self, block: &ast::Spanned<ast::Expression>) -> NodeId {
        match &block.inner {
            ast::Expression::Block { statements } => self.build_block(statements, true),
            _ => unreachable!("function body must be a block expression"),
        }
    }

    fn build_block(
        &mut self,
        statements: &[ast::Separated<ast::Spanned<ast::Statement>>],
        force_break: bool,
    ) -> NodeId {
        let mut items: Vec<NodeId> = vec![self.text(Text::LBrace)];

        if !statements.is_empty() {
            let single = !force_break && statements.len() == 1;
            let mut inner: Vec<NodeId> = Vec::new();
            inner.push(if single {
                self.soft_line()
            } else {
                self.hard_line()
            });

            for (index, statement) in statements.iter().enumerate() {
                if index > 0 {
                    let prev_end = statements[index - 1].inner.span.end as u32;
                    let curr_start = statement.inner.span.start as u32;
                    let gap_spans: Vec<ast::TextSpan> = self
                        .comments
                        .between(prev_end, curr_start)
                        .iter()
                        .map(|c| c.span)
                        .collect();
                    let blank_end = gap_spans
                        .first()
                        .map_or(curr_start as usize, |s| s.start as usize);
                    let blank_lines =
                        Self::count_blank_lines(self.source, prev_end as usize, blank_end);
                    for _ in 0..blank_lines {
                        inner.push(self.hard_line());
                    }
                    for span in &gap_spans {
                        inner.push(self.source_text(*span));
                        inner.push(self.hard_line());
                    }
                }
                inner.push(self.build_statement(&statement.inner.inner));
                if index + 1 == statements.len() {
                    if statement.separator.is_some() {
                        inner.push(self.text(Text::Semi));
                    }
                } else {
                    let needs_semi = if statement.inner.inner.is_block_like() {
                        statement.separator.is_some()
                    } else {
                        true
                    };
                    if needs_semi {
                        inner.push(self.text(Text::Semi));
                    }
                    inner.push(self.hard_line());
                }
            }

            let inner_concat = self.arena.concat(inner);
            items.push(self.arena.indent(inner_concat));
            items.push(if single {
                self.soft_line()
            } else {
                self.hard_line()
            });
        }

        items.push(self.text(Text::RBrace));
        let concat = self.arena.concat(items);
        self.arena.group(concat)
    }

    fn build_call_args(
        &mut self,
        out: &mut Vec<NodeId>,
        arguments: &[ast::Separated<ast::Spanned<ast::Expression>>],
    ) {
        out.push(self.text(Text::LParen));
        if !arguments.is_empty() {
            let mut arg_nodes: Vec<NodeId> = vec![self.line()];
            for (index, arg) in arguments.iter().enumerate() {
                arg_nodes.push(self.build_expression(&arg.inner));
                if index + 1 < arguments.len() {
                    arg_nodes.push(self.text(Text::Comma));
                    arg_nodes.push(self.soft_line());
                } else {
                    arg_nodes.push(self.if_break_comma());
                }
            }
            let args_concat = self.arena.concat(arg_nodes);
            out.push(self.arena.indent(args_concat));
            out.push(self.line());
        }
        out.push(self.text(Text::RParen));
    }

    fn build_expression(&mut self, expression: &ast::Spanned<ast::Expression>) -> NodeId {
        match &expression.inner {
            ast::Expression::Path(path) => self.build_path_segments(path),
            ast::Expression::Binary {
                left,
                operator,
                right,
            } => {
                let mut operands: Vec<&ast::Spanned<ast::Expression>> = vec![right, left];
                let mut current = left;
                while let ast::Expression::Binary {
                    left: l,
                    operator: op,
                    right: r,
                } = &current.inner
                {
                    if op.inner == operator.inner {
                        *operands.last_mut().unwrap() = r;
                        operands.push(l);
                        current = l;
                    } else {
                        break;
                    }
                }
                operands.reverse();

                let first = self.build_expression(operands[0]);
                let mut parts: Vec<NodeId> = vec![first];
                let op_text = Text::from(operator.inner);
                for operand in &operands[1..] {
                    let sl = self.soft_line();
                    let op_id = self.text(op_text);
                    let sp = self.text(Text::Space);
                    let operand_id = self.build_expression(operand);
                    let inner = self.arena.concat4(sl, op_id, sp, operand_id);
                    parts.push(self.arena.indent(inner));
                }
                let concat = self.arena.concat(parts);
                self.arena.group(concat)
            }
            ast::Expression::Block { statements } => self.build_block(statements, false),
            ast::Expression::Unreachable => self.text(Text::Unreachable),
            ast::Expression::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let if_kw = self.text(Text::If);
                let cond = self.build_expression(condition);
                let sp = self.text(Text::Space);
                let then_id = self.build_expression(then_block);
                let mut items: Vec<NodeId> = vec![if_kw, cond, sp, then_id];
                if let Some(else_block) = else_block {
                    items.push(self.text(Text::Else));
                    items.push(self.build_expression(else_block));
                }
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
            ast::Expression::Loop { block } => {
                let loop_kw = self.text(Text::Loop);
                let block_id = self.build_expression(block);
                let concat = self.arena.concat2(loop_kw, block_id);
                self.arena.group(concat)
            }
            ast::Expression::Break { label, value } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::Break)];
                if let Some(label) = label {
                    items.push(self.text(Text::LabelColon));
                    items.push(self.symbol(label.inner));
                }
                if let Some(value) = value {
                    items.push(self.text(Text::Space));
                    items.push(self.build_expression(value));
                }
                self.arena.concat(items)
            }
            ast::Expression::Return { value } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::Return)];
                if let Some(value) = value {
                    items.push(self.text(Text::Space));
                    items.push(self.build_expression(value));
                }
                self.arena.concat(items)
            }
            ast::Expression::Cast { value, ty } => {
                let val = self.build_expression(value);
                let as_kw = self.text(Text::As);
                let ty_id = self.build_type_expression(&ty.inner);
                self.arena.concat3(val, as_kw, ty_id)
            }
            ast::Expression::Continue { label } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::Continue)];
                if let Some(label) = label {
                    items.push(self.text(Text::LabelColon));
                    items.push(self.symbol(label.inner));
                }
                self.arena.concat(items)
            }
            ast::Expression::Int { .. } | ast::Expression::Float { .. } => {
                self.source_text(expression.span)
            }
            ast::Expression::Grouping { value } => {
                let open = self.text(Text::LParen);
                let val = self.build_expression(value);
                let close = self.text(Text::RParen);
                self.arena.concat3(open, val, close)
            }
            ast::Expression::Call { callee, arguments } => {
                let callee_id = self.build_expression(callee);
                let mut items: Vec<NodeId> = vec![callee_id];
                self.build_call_args(&mut items, arguments);
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
            ast::Expression::MethodCall(mc) => {
                let obj_id = self.build_expression(&mc.object);
                let dot = self.text(Text::Dot);
                let method_sym = self.symbol(mc.method.inner);
                let mut items: Vec<NodeId> = vec![obj_id, dot, method_sym];
                if !mc.type_args.is_empty() {
                    items.push(self.text(Text::ColonColonLt));
                    for (i, arg) in mc.type_args.iter().enumerate() {
                        if i > 0 {
                            items.push(self.text(Text::CommaSp));
                        }
                        items.push(self.build_type_expression(&arg.inner));
                    }
                    items.push(self.text(Text::Gt));
                }
                self.build_call_args(&mut items, &mc.arguments);
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
            ast::Expression::Label { label, block } => {
                let sym = self.symbol(label.inner);
                let colon_sp = self.text(Text::ColonSp);
                let block_id = self.build_expression(block);
                self.arena.concat3(sym, colon_sp, block_id)
            }
            ast::Expression::Error => unreachable!(),
            ast::Expression::Unary { operator, operand } => {
                let op_id = self.text(Text::from(operator.inner));
                let operand_id = self.build_expression(operand);
                self.arena.concat2(op_id, operand_id)
            }
            ast::Expression::String { .. } | ast::Expression::Char { .. } => {
                self.source_text(expression.span)
            }
            ast::Expression::ObjectAccess { object, member } => {
                let obj_id = self.build_expression(object);
                let dot = self.text(Text::Dot);
                let sym = self.symbol(member.inner);
                self.arena.concat3(obj_id, dot, sym)
            }
            ast::Expression::Deref { pointer } => {
                let ptr_id = self.build_expression(pointer);
                let dot_star = self.text(Text::DotStar);
                self.arena.concat2(ptr_id, dot_star)
            }
            ast::Expression::StructInit { path, fields } => {
                let path_id = self.build_path_segments(path);
                let open = self.text(Text::ColonColonLBrace);
                let mut items: Vec<NodeId> = vec![path_id, open];

                let has_block_value = fields.iter().any(|f| {
                    f.inner
                        .inner
                        .value
                        .as_ref()
                        .is_some_and(|v| v.inner.is_block_like())
                });
                if !fields.is_empty() {
                    let field_count = fields.len();
                    let sep = if has_block_value {
                        self.hard_line()
                    } else {
                        self.soft_line()
                    };
                    let mut field_items: Vec<NodeId> = vec![sep];
                    for (index, field) in fields.iter().enumerate() {
                        field_items.push(self.symbol(field.inner.inner.name.inner));
                        if let Some(value) = &field.inner.inner.value {
                            field_items.push(self.text(Text::ColonSp));
                            field_items.push(self.build_expression(value));
                        }
                        let is_last = index + 1 == field_count;
                        if !is_last || has_block_value {
                            field_items.push(self.text(Text::Comma));
                        } else {
                            field_items.push(self.if_break_comma());
                        }
                        if !is_last {
                            field_items.push(if has_block_value {
                                self.hard_line()
                            } else {
                                self.soft_line()
                            });
                        }
                    }

                    let concat = self.arena.concat(field_items);
                    items.push(self.arena.indent(concat));
                    items.push(if has_block_value {
                        self.hard_line()
                    } else {
                        self.soft_line()
                    });
                }

                items.push(self.text(Text::RBrace));
                if has_block_value {
                    self.arena.concat(items)
                } else {
                    let concat = self.arena.concat(items);
                    self.arena.group(concat)
                }
            }
            ast::Expression::TypeApplication { callee, args } => {
                let callee_id = self.build_expression(callee);
                let mut items: Vec<NodeId> = vec![callee_id, self.text(Text::ColonColonLt)];
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        items.push(self.text(Text::CommaSp));
                    }
                    items.push(self.build_type_expression(&arg.inner));
                }
                items.push(self.text(Text::Gt));
                self.arena.concat(items)
            }
            ast::Expression::IntrinsicCall {
                name,
                type_args,
                arguments,
            } => {
                let mut items: Vec<NodeId> = vec![self.symbol(name.inner)];
                if !type_args.is_empty() {
                    items.push(self.text(Text::ColonColonLt));
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            items.push(self.text(Text::CommaSp));
                        }
                        items.push(self.build_type_expression(&arg.inner));
                    }
                    items.push(self.text(Text::Gt));
                }
                items.push(self.text(Text::LParen));
                for (index, arg) in arguments.iter().enumerate() {
                    items.push(self.build_expression(&arg.inner));
                    if index + 1 < arguments.len() {
                        items.push(self.text(Text::CommaSp));
                    }
                }
                items.push(self.text(Text::RParen));
                self.arena.concat(items)
            }
            ast::Expression::ArrayList { elements } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::LBracket)];
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        items.push(self.text(Text::CommaSp));
                    }
                    items.push(self.build_expression(element));
                }
                items.push(self.text(Text::RBracket));
                self.arena.concat(items)
            }
            ast::Expression::ArrayRepeat { value, count } => {
                let open = self.text(Text::LBracket);
                let val = self.build_expression(value);
                let semi_sp = self.text(Text::Semi);
                let sp = self.text(Text::Space);
                let cnt = self.build_expression(count);
                let close = self.text(Text::RBracket);
                self.arena.concat(vec![open, val, semi_sp, sp, cnt, close])
            }
            ast::Expression::Index { object, index } => {
                let obj_id = self.build_expression(object);
                let open = self.text(Text::LBracket);
                let idx = self.build_expression(index);
                let close = self.text(Text::RBracket);
                self.arena.concat4(obj_id, open, idx, close)
            }
            ast::Expression::SliceRange { object, start, end } => {
                let obj_id = self.build_expression(object);
                let mut parts: Vec<NodeId> = vec![obj_id, self.text(Text::LBracket)];
                if let Some(s) = start {
                    parts.push(self.build_expression(s));
                }
                parts.push(self.text(Text::DotDot));
                if let Some(e) = end {
                    parts.push(self.build_expression(e));
                }
                parts.push(self.text(Text::RBracket));
                self.arena.concat(parts)
            }
            ast::Expression::Tuple { elements } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::LParen)];

                if !elements.is_empty() {
                    let last_idx = elements.len() - 1;
                    let mut element_items: Vec<NodeId> = vec![self.line()];
                    for (index, element) in elements.iter().enumerate() {
                        let el = self.build_expression(element);
                        let mut nodes: Vec<NodeId> = vec![el];
                        if index < last_idx {
                            nodes.push(self.text(Text::Comma));
                        } else if elements.len() == 1 {
                            nodes.push(self.text(Text::Comma));
                        } else {
                            nodes.push(self.if_break_comma());
                        }
                        element_items.push(self.arena.concat(nodes));
                        if index < last_idx {
                            element_items.push(self.soft_line());
                        }
                    }

                    let concat = self.arena.concat(element_items);
                    items.push(self.arena.indent(concat));
                    items.push(self.line());
                }

                items.push(self.text(Text::RParen));
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
        }
    }

    fn build_pattern(&mut self, out: &mut Vec<NodeId>, pattern: &ast::Pattern) {
        match pattern {
            ast::Pattern::Wildcard => out.push(self.text(Text::Underscore)),
            ast::Pattern::Binding { mut_span, name } => {
                if mut_span.is_some() {
                    out.push(self.text(Text::Mut));
                }
                out.push(self.symbol(name.inner));
            }
            ast::Pattern::Tuple { elements } => {
                out.push(self.text(Text::LParen));
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        out.push(self.text(Text::CommaSp));
                    }
                    self.build_pattern(out, &element.inner.inner);
                }
                out.push(self.text(Text::RParen));
            }
            ast::Pattern::Struct { name, fields } => {
                out.push(self.symbol(name.inner));
                out.push(self.text(Text::SpaceLBrace));
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push(self.text(Text::CommaSp));
                    } else {
                        out.push(self.text(Text::Space));
                    }
                    out.push(self.symbol(field.inner.inner.name.inner));
                    if let Some(pat) = &field.inner.inner.pattern {
                        out.push(self.text(Text::ColonSp));
                        self.build_pattern(out, &pat.inner);
                    }
                }
                if !fields.is_empty() {
                    out.push(self.text(Text::Space));
                }
                out.push(self.text(Text::RBrace));
            }
        }
    }

    fn build_statement(&mut self, statement: &ast::Statement) -> NodeId {
        match statement {
            ast::Statement::Expression(expression) => self.build_expression(expression),
            ast::Statement::LocalDefinition {
                pattern,
                ty: type_annotation,
                value,
            } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::Local)];
                self.build_pattern(&mut items, &pattern.inner);
                if let Some(annotation) = type_annotation {
                    items.push(self.text(Text::ColonSp));
                    items.push(self.build_type_expression(&annotation.inner));
                }
                let value_node = self.build_expression(value);
                if value.inner.is_block_like() {
                    items.push(self.text(Text::EqSp));
                    items.push(value_node);
                } else {
                    items.push(self.text(Text::EqBare));
                    let sl = self.soft_line();
                    let inner = self.arena.concat2(sl, value_node);
                    items.push(self.arena.indent(inner));
                }
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
        }
    }

    fn build_path_segments(&mut self, path: &[ast::PathSegment]) -> NodeId {
        let mut items: Vec<NodeId> = Vec::new();
        for (i, seg) in path.iter().enumerate() {
            if i > 0 {
                items.push(self.text(Text::ColonColon));
            }
            items.push(self.symbol(seg.ident.inner));
            if !seg.type_args.is_empty() {
                items.push(self.text(Text::ColonColonLt));
                for (j, arg) in seg.type_args.iter().enumerate() {
                    if j > 0 {
                        items.push(self.text(Text::CommaSp));
                    }
                    items.push(self.build_type_expression(&arg.inner));
                }
                items.push(self.text(Text::Gt));
            }
        }
        self.arena.concat(items)
    }

    fn build_bound_expression(&mut self, bound: &ast::BoundExpression) -> NodeId {
        match bound {
            ast::BoundExpression::Path(segs) => self.build_path_segments(segs),
            ast::BoundExpression::WithBindings { path, bindings } => {
                let base_id = self.build_bound_expression(path);
                let where_kw = self.text(Text::Where);
                let open = self.text(Text::LBraceSpace);
                let close = self.text(Text::SpaceRBrace);
                let mut binding_parts: Vec<NodeId> = Vec::new();
                for (i, binding) in bindings.iter().enumerate() {
                    if i > 0 {
                        binding_parts.push(self.text(Text::CommaSp));
                    }
                    let key = self.symbol(binding.name.inner);
                    let eq = self.text(Text::EqSp);
                    let ty = self.build_type_expression(&binding.ty.inner);
                    binding_parts.push(self.arena.concat3(key, eq, ty));
                }
                let bindings_concat = self.arena.concat(binding_parts);
                self.arena
                    .concat5(base_id, where_kw, open, bindings_concat, close)
            }
            ast::BoundExpression::BoundList(items) => {
                let mut parts: Vec<NodeId> = Vec::new();
                for (i, b) in items.iter().enumerate() {
                    if i > 0 {
                        parts.push(self.text(Text::PlusSp));
                    }
                    parts.push(self.build_bound_expression(&b.inner));
                }
                self.arena.concat(parts)
            }
        }
    }

    fn build_type_expression(&mut self, type_expression: &ast::TypeExpression) -> NodeId {
        match type_expression {
            ast::TypeExpression::Infer => self.text(Text::Underscore),
            ast::TypeExpression::Path(path) => self.build_path_segments(path),
            ast::TypeExpression::Function { params, result } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::FnParen)];

                if !params.is_empty() {
                    let mut param_items: Vec<NodeId> = vec![self.line()];
                    for (index, param) in params.iter().enumerate() {
                        if let Some(name) = &param.inner.inner.name {
                            param_items.push(self.symbol(name.inner));
                            param_items.push(self.text(Text::ColonSp));
                        }
                        param_items.push(self.build_type_expression(&param.inner.inner.ty.inner));
                        if index + 1 < params.len() {
                            param_items.push(self.text(Text::Comma));
                            param_items.push(self.soft_line());
                        } else {
                            param_items.push(self.if_break_comma());
                        }
                    }
                    let params_concat = self.arena.concat(param_items);
                    items.push(self.arena.indent(params_concat));
                    items.push(self.line());
                }

                items.push(self.text(Text::RParen));
                if let Some(result) = result {
                    items.push(self.text(Text::Arrow));
                    items.push(self.build_type_expression(&result.inner));
                }
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
            ast::TypeExpression::Pointer { mutability, inner } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::Star)];
                if mutability.is_some() {
                    items.push(self.text(Text::Mut));
                }
                items.push(self.build_type_expression(&inner.inner));
                self.arena.concat(items)
            }
            ast::TypeExpression::Slice { mutability, inner } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::SliceBrackets)];
                if mutability.is_some() {
                    items.push(self.text(Text::Mut));
                }
                items.push(self.build_type_expression(&inner.inner));
                self.arena.concat(items)
            }
            ast::TypeExpression::Array {
                size,
                mutability,
                inner,
            } => {
                let mut items: Vec<NodeId> = vec![
                    self.text(Text::LBracket),
                    self.source_text(size.span),
                    self.text(Text::RBracket),
                ];
                if mutability.is_some() {
                    items.push(self.text(Text::Mut));
                }
                items.push(self.build_type_expression(&inner.inner));
                self.arena.concat(items)
            }
            ast::TypeExpression::Tuple { elements } => {
                let mut items: Vec<NodeId> = vec![self.text(Text::LParen)];

                if !elements.is_empty() {
                    let last_idx = elements.len() - 1;
                    let mut element_items: Vec<NodeId> = vec![self.line()];
                    for (index, element) in elements.iter().enumerate() {
                        let ty = self.build_type_expression(&element.inner);
                        let mut nodes: Vec<NodeId> = vec![ty];
                        if index < last_idx {
                            nodes.push(self.text(Text::Comma));
                        } else if elements.len() == 1 {
                            nodes.push(self.text(Text::Comma));
                        } else {
                            nodes.push(self.if_break_comma());
                        }
                        element_items.push(self.arena.concat(nodes));
                        if index < last_idx {
                            element_items.push(self.soft_line());
                        }
                    }

                    let concat = self.arena.concat(element_items);
                    items.push(self.arena.indent(concat));
                    items.push(self.line());
                }

                items.push(self.text(Text::RParen));
                let concat = self.arena.concat(items);
                self.arena.group(concat)
            }
            ast::TypeExpression::MemoryTagged { memory, inner } => {
                let mem_id = self.build_path_segments(memory);
                let sep = self.text(Text::ColonColon);
                let ty = self.build_type_expression(&inner.inner);
                self.arena.concat3(mem_id, sep, ty)
            }
            ast::TypeExpression::GenericApplication { name, args } => {
                let mut inner_parts: Vec<NodeId> = Vec::new();
                for (i, sep) in args.iter().enumerate() {
                    if i > 0 {
                        inner_parts.push(self.text(Text::CommaSp));
                    }
                    inner_parts.push(self.build_type_expression(&sep.inner.inner));
                }
                let name_sym = self.symbol(name.inner);
                let open = self.text(Text::Lt);
                let inner_concat = self.arena.concat(inner_parts);
                let close = self.text(Text::Gt);
                self.arena.concat4(name_sym, open, inner_concat, close)
            }
        }
    }
}

pub struct RendererConfig {
    pub max_line_width: u32,
    pub indent_width: u8,
    pub trailing_comma: bool,
}

impl Default for RendererConfig {
    fn default() -> Self {
        Self {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        }
    }
}

struct Renderer<'a> {
    config: RendererConfig,
    interner: &'a ast::StringInterner,
    source: &'a str,
    arena: &'a Arena,
    buffer: String,
    position: usize,
    indent: usize,
}

#[derive(Clone, Copy)]
enum RenderMode {
    Flat,
    Break,
}

impl<'a> Renderer<'a> {
    fn new(
        config: RendererConfig,
        interner: &'a ast::StringInterner,
        source: &'a str,
        arena: &'a Arena,
    ) -> Self {
        Self {
            config,
            interner,
            source,
            arena,
            buffer: String::new(),
            position: 0,
            indent: 0,
        }
    }

    fn render(mut self, root: NodeId) -> String {
        self.render_node(root, RenderMode::Break);
        self.buffer
    }

    fn render_node(&mut self, id: NodeId, mode: RenderMode) {
        match self.arena.nodes[id as usize] {
            Node::Text(t) => {
                let s = t.as_str();
                self.buffer.push_str(s);
                self.position += s.len();
            }
            Node::SourceText(span) => {
                let text = span.extract_str(self.source);
                self.buffer.push_str(text);
                self.position += text.len();
            }
            Node::Symbol { symbol, .. } => {
                let resolved = self.interner.resolve(symbol).unwrap();
                self.buffer.push_str(resolved);
                self.position += resolved.len();
            }
            Node::SoftLine => match mode {
                RenderMode::Flat => {
                    self.buffer.push(' ');
                    self.position += 1;
                }
                RenderMode::Break => {
                    self.buffer.push('\n');
                    self.buffer.push_str(&" ".repeat(self.indent));
                    self.position = self.indent;
                }
            },
            Node::Line => match mode {
                RenderMode::Flat => {}
                RenderMode::Break => {
                    self.buffer.push('\n');
                    self.buffer.push_str(&" ".repeat(self.indent));
                    self.position = self.indent;
                }
            },
            Node::BlankLine => {
                self.buffer.push('\n');
                self.position = 0;
            }
            Node::HardLine => {
                self.buffer.push('\n');
                self.buffer.push_str(&" ".repeat(self.indent));
                self.position = self.indent;
            }
            Node::Concat { start, len } => {
                for i in start as usize..(start + len) as usize {
                    self.render_node(self.arena.children[i], mode);
                }
            }
            Node::Group(inner_id) => {
                let mode = if self.measure_flat(id)
                    <= self.config.max_line_width as usize - self.position
                {
                    RenderMode::Flat
                } else {
                    RenderMode::Break
                };
                self.render_node(inner_id, mode);
            }
            Node::Indent(inner_id) => {
                self.indent += self.config.indent_width as usize;
                self.render_node(inner_id, mode);
                self.indent -= self.config.indent_width as usize;
            }
            Node::IfBreakComma => match mode {
                RenderMode::Flat => {}
                RenderMode::Break => {
                    self.buffer.push(',');
                    self.position += 1;
                }
            },
        }
    }

    fn measure_flat(&self, id: NodeId) -> usize {
        let mut width = 0usize;
        let mut stack: Vec<NodeId> = vec![id];
        while let Some(current_id) = stack.pop() {
            match self.arena.nodes[current_id as usize] {
                Node::Text(t) => width += t.as_str().len(),
                Node::SourceText(span) => width += (span.end - span.start) as usize,
                Node::Symbol { len, .. } => width += len as usize,
                Node::SoftLine => width += 1,
                Node::Line | Node::IfBreakComma => {}
                Node::BlankLine | Node::HardLine => return width,
                Node::Group(inner) | Node::Indent(inner) => stack.push(inner),
                Node::Concat { start, len } => {
                    for i in (start as usize..(start + len) as usize).rev() {
                        stack.push(self.arena.children[i]);
                    }
                }
            }
        }
        width
    }
}

pub fn format(
    ast: &ast::AST,
    interner: &ast::StringInterner,
    source: &str,
    config: RendererConfig,
) -> String {
    let mut builder = Builder {
        interner,
        source,
        comments: &ast.comments,
        arena: Arena::new(),
    };
    let body = builder.build(ast);
    let hl = builder.hard_line();
    let root = builder.arena.concat2(body, hl);
    let Builder { arena, .. } = builder;
    Renderer::new(config, interner, source, &arena).render(root)
}
