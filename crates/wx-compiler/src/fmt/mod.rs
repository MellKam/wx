use string_interner::symbol::SymbolU32;

use crate::ast::*;

#[cfg(test)]
mod tests;

#[cfg_attr(test, derive(Debug, serde::Serialize))]
enum Node {
    /// A static text fragment that never requires allocation
    StaticText(&'static str),
    /// A slice of the original source addressed by span
    SourceText(TextSpan),
    /// An interned symbol resolved during rendering
    Symbol { symbol: SymbolU32, len: u32 },
    /// A possible line break, otherwise rendered as a space
    SoftLine,
    /// A possible line break, otherwise nothing
    Line,
    /// Always break the line
    HardLine,
    /// Always emit a blank line without indentation whitespace
    BlankLine,
    /// A sequence of nodes concatenated together
    Concat(Box<[Node]>),
    /// All lines under this node must either break or not break together
    Group(Box<Node>),
    /// Increases the indentation level for all lines within this node
    Indent(Box<Node>),
    /// Text that only appears in break mode
    IfBreak(char),
}

struct Builder<'a> {
    interner: &'a StringInterner,
    source: &'a str,
    comments: &'a CommentMap,
}

impl<'a> Builder<'a> {
    fn symbol(&self, symbol: SymbolU32) -> Node {
        let len = self.interner.resolve(symbol).unwrap().len() as u32;
        Node::Symbol { symbol, len }
    }

    fn count_blank_lines(source: &str, end_pos: usize, start_pos: usize) -> usize {
        if start_pos <= end_pos {
            return 0;
        }
        let between = &source[end_pos..start_pos];
        let newlines = between.matches('\n').count();
        // Subtract 1 because one newline is normal separation, max 1 blank line
        newlines.saturating_sub(1).min(1)
    }

    fn build(&self, ast: &AST) -> Node {
        self.build_item_list(&ast.items, true)
    }

    fn build_item_list(&self, items: &[Separated<Spanned<Item>>], toplevel: bool) -> Node {
        let mut nodes = Vec::new();

        // Emit any comments that appear before the first item.
        if toplevel {
            if let Some(first) = items.first() {
                for c in self.comments.between(0, first.inner.span.start) {
                    nodes.push(Node::SourceText(c.span));
                    nodes.push(Node::HardLine);
                }
            }
        }

        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                let prev = &items[index - 1];
                let gap_comments =
                    self.comments.between(prev.inner.span.end, item.inner.span.start);

                if prev.inner.inner.is_block_like() || item.inner.inner.is_block_like() {
                    // Always one blank line around block-like items.
                    nodes.push(Node::BlankLine);
                } else {
                    // Between compact items, mirror blank lines from source.
                    // When comments are present, measure only the gap before
                    // the first comment so the comment's own newline isn't
                    // counted as a blank line.
                    let blank_end = gap_comments
                        .first()
                        .map_or(item.inner.span.start as usize, |c| c.span.start as usize);
                    let blank = Self::count_blank_lines(
                        self.source,
                        prev.inner.span.end as usize,
                        blank_end,
                    );
                    for _ in 0..blank {
                        nodes.push(Node::BlankLine);
                    }
                }
                // Emit comments that fall in the gap between the two items.
                for c in gap_comments {
                    nodes.push(Node::HardLine);
                    nodes.push(Node::SourceText(c.span));
                }
                nodes.push(Node::HardLine);
            }
            nodes.push(self.build_item(&item.inner.inner));
        }
        Node::Concat(nodes.into())
    }

    fn build_item(&self, item: &Item) -> Node {
        match item {
            Item::Function {
                signature,
                block,
                attributes,
                pub_span,
                ..
            } => {
                let mut items = Vec::new();
                self.build_attributes(&mut items, attributes);
                if pub_span.is_some() {
                    items.push(Node::StaticText("pub "));
                }
                self.build_function_signature(&mut items, signature);
                items.push(Node::StaticText(" "));
                items.push(self.build_fn_body(block));
                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Item::FunctionDeclaration {
                pub_span,
                attributes,
                signature,
                ..
            } => {
                let mut items = Vec::new();
                self.build_attributes(&mut items, attributes);
                if pub_span.is_some() {
                    items.push(Node::StaticText("pub "));
                }
                self.build_function_signature(&mut items, signature);
                items.push(Node::StaticText(";"));
                Node::Concat(items.into())
            }
            Item::IntrinsicFunction { signature, .. } => {
                let mut items = Vec::new();
                self.build_function_signature(&mut items, signature);
                items.push(Node::StaticText(";"));
                Node::Concat(items.into())
            }
            Item::Global {
                pub_span,
                mut_span,
                name,
                ty: type_annotation,
                value,
                ..
            } => self.build_global_definition(
                *pub_span,
                *mut_span,
                name,
                type_annotation,
                value,
            ),
            Item::Export { entries } => self.build_export_definition(entries),
            Item::Import {
                module,
                alias,
                entries,
            } => self.build_import_definition(module, alias, entries),
            Item::Memory {
                name, kind, config, ..
            } => {
                let mut parts = vec![
                    Node::StaticText("memory "),
                    self.symbol(name.inner),
                    Node::StaticText(": "),
                    self.build_type_expression(&kind.inner),
                ];
                if let Some(cfg) = config {
                    let mut fields: Vec<Node> = Vec::new();
                    if let Some(min) = &cfg.min_pages {
                        fields.push(Node::StaticText("min_pages: "));
                        fields.push(Node::SourceText(min.span));
                    }
                    if let Some(max) = &cfg.max_pages {
                        if !fields.is_empty() {
                            fields.push(Node::StaticText(", "));
                        }
                        fields.push(Node::StaticText("max_pages: "));
                        fields.push(Node::SourceText(max.span));
                    }
                    if !fields.is_empty() {
                        parts.push(Node::StaticText(" { "));
                        parts.extend(fields);
                        parts.push(Node::StaticText(" }"));
                    }
                }
                parts.push(Node::StaticText(";"));
                Node::Concat(parts.into())
            }
            Item::Enum {
                pub_span,
                repr,
                name,
                variants,
                ..
            } => self.build_enum_definition(*pub_span, repr, name, variants),
            Item::Impl {
                type_params,
                target,
                items,
            } => self.build_impl_definition(type_params, target, items),
            Item::ImplTrait {
                trait_name,
                target,
                items,
                ..
            } => self.build_impl_trait_definition(trait_name, target, items),
            Item::Const {
                pub_span,
                name,
                ty,
                value,
                ..
            } => self.build_const_definition(*pub_span, name, ty, value),
            Item::Module {
                pub_span,
                name,
                items,
            } => self.build_module_definition(*pub_span, name, items),
            Item::ModuleDeclaration { pub_span, name } => {
                self.build_module_declaration(*pub_span, name)
            }
            Item::Trait {
                pub_span,
                name,
                supertraits,
                items,
                ..
            } => self.build_trait_definition(*pub_span, name, supertraits, items),
            Item::Struct {
                id: _,
                name,
                type_params,
                fields,
                pub_span,
            } => self.build_struct_declaration(name, type_params, fields, *pub_span),
            Item::TypeSet {
                pub_span,
                name,
                members,
                ..
            } => {
                let mut items = Vec::new();
                if pub_span.is_some() {
                    items.push(Node::StaticText("pub "));
                }
                items.push(Node::StaticText("typeset "));
                items.push(self.symbol(name.inner));
                items.push(Node::StaticText(" { "));
                for (i, m) in members.iter().enumerate() {
                    if i > 0 {
                        items.push(Node::StaticText(", "));
                    }
                    items.push(self.build_type_expression(&m.inner.inner));
                }
                items.push(Node::StaticText(" }"));
                Node::Group(Box::new(Node::Concat(items.into())))
            }
        }
    }

    fn build_import_definition(
        &self,
        module: &Spanned<SymbolU32>,
        alias: &Option<Spanned<SymbolU32>>,
        entries: &[Separated<Spanned<ImportEntry>>],
    ) -> Node {
        let mut items = Vec::new();

        items.push(Node::StaticText("import "));
        items.push(self.symbol(module.inner));

        if let Some(alias) = alias {
            items.push(Node::StaticText(" as "));
            items.push(self.symbol(alias.inner));
        }

        items.push(Node::StaticText(" {"));

        if !entries.is_empty() {
            let entry_items = entries
                .iter()
                .enumerate()
                .flat_map(|(index, entry)| {
                    let mut entry_nodes = Vec::new();

                    if let Some(ext_name) = &entry.inner.inner.external_name {
                        entry_nodes.push(self.symbol(ext_name.inner));
                        entry_nodes.push(Node::StaticText(": "));
                    }

                    match &entry.inner.inner.declaration {
                        ImportDeclaration::Function { signature, .. } => {
                            self.build_function_signature(&mut entry_nodes, signature);
                            entry_nodes =
                                vec![Node::Group(Box::new(Node::Concat(entry_nodes.into())))];
                        }
                        ImportDeclaration::Global {
                            mut_span, name, ty, ..
                        } => {
                            entry_nodes.push(Node::StaticText("global "));
                            if mut_span.is_some() {
                                entry_nodes.push(Node::StaticText("mut "));
                            }
                            entry_nodes.push(self.symbol(name.inner));
                            entry_nodes.push(Node::StaticText(": "));
                            entry_nodes.push(self.build_type_expression(&ty.inner));
                        }
                        ImportDeclaration::Memory { name, kind, .. } => {
                            entry_nodes.push(Node::StaticText("memory "));
                            entry_nodes.push(self.symbol(name.inner));
                            entry_nodes.push(Node::StaticText(": "));
                            entry_nodes.push(self.build_type_expression(&kind.inner));
                        }
                    }

                    if entry.separator.is_some() {
                        entry_nodes.push(Node::StaticText(";"));
                    }

                    let mut result = vec![Node::Concat(entry_nodes.into())];

                    if index + 1 < entries.len() {
                        result.push(Node::Line);
                    }

                    result
                })
                .collect::<Vec<_>>();

            items.push(Node::Indent(Box::new(Node::Concat(
                std::iter::once(Node::Line)
                    .chain(entry_items)
                    .collect::<Box<[_]>>(),
            ))));
        }

        items.push(Node::Line);
        items.push(Node::StaticText("}"));
        Node::Concat(items.into())
    }

    fn build_module_definition(
        &self,
        pub_span: Option<TextSpan>,
        name: &Spanned<SymbolU32>,
        items: &[Separated<Spanned<Item>>],
    ) -> Node {
        let mut nodes = Vec::new();
        if pub_span.is_some() {
            nodes.push(Node::StaticText("pub "));
        }
        nodes.push(Node::StaticText("module "));
        nodes.push(self.symbol(name.inner));
        nodes.push(Node::StaticText(" {"));

        if !items.is_empty() {
            nodes.push(Node::Indent(Box::new(Node::Concat(
                vec![
                    Node::HardLine,
                    self.build_item_list(items, false),
                ]
                .into(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(nodes.into())))
    }

    fn build_module_declaration(
        &self,
        pub_span: Option<TextSpan>,
        name: &Spanned<SymbolU32>,
    ) -> Node {
        let mut nodes = Vec::new();
        if pub_span.is_some() {
            nodes.push(Node::StaticText("pub "));
        }
        nodes.push(Node::StaticText("module "));
        nodes.push(self.symbol(name.inner));
        nodes.push(Node::StaticText(";"));
        Node::Concat(nodes.into())
    }

    fn build_impl_definition(
        &self,
        type_params: &[TypeParam],
        target: &Spanned<TypeExpression>,
        items: &[Separated<Spanned<ImplItem>>],
    ) -> Node {
        let mut nodes = vec![Node::StaticText("impl ")];
        self.build_type_params(&mut nodes, type_params);
        if !type_params.is_empty() {
            nodes.push(Node::StaticText(" "));
        }
        nodes.push(self.build_type_expression(&target.inner));
        nodes.push(Node::StaticText(" {"));

        if !items.is_empty() {
            nodes.push(Node::Indent(Box::new(Node::Concat(
                vec![
                    Node::HardLine,
                    self.build_impl_item_list(items),
                ]
                .into(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(nodes.into())))
    }

    fn build_impl_trait_definition(
        &self,
        trait_name: &Spanned<TypeExpression>,
        target: &Spanned<TypeExpression>,
        items: &[Separated<Spanned<ImplItem>>],
    ) -> Node {
        let mut nodes = vec![
            Node::StaticText("impl "),
            self.build_type_expression(&trait_name.inner),
            Node::StaticText(" for "),
            self.build_type_expression(&target.inner),
            Node::StaticText(" {"),
        ];

        if !items.is_empty() {
            nodes.push(Node::Indent(Box::new(Node::Concat(
                vec![
                    Node::HardLine,
                    self.build_impl_item_list(items),
                ]
                .into(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(nodes.into())))
    }

    fn build_impl_item_list(&self, items: &[Separated<Spanned<ImplItem>>]) -> Node {
        let mut nodes = Vec::new();
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                nodes.push(Node::BlankLine);
                let prev = &items[index - 1];
                for c in self.comments.between(prev.inner.span.end, item.inner.span.start) {
                    nodes.push(Node::HardLine);
                    nodes.push(Node::SourceText(c.span));
                }
                nodes.push(Node::HardLine);
            }
            nodes.push(self.build_impl_item(&item.inner.inner));
        }
        Node::Concat(nodes.into())
    }

    fn build_impl_item(&self, item: &ImplItem) -> Node {
        match item {
            ImplItem::Method {
                pub_span,
                attributes,
                signature,
                block,
                ..
            } => {
                let mut nodes = Vec::new();
                self.build_attributes(&mut nodes, attributes);
                if pub_span.is_some() {
                    nodes.push(Node::StaticText("pub "));
                }
                self.build_function_signature(&mut nodes, signature);
                nodes.push(Node::StaticText(" "));
                nodes.push(self.build_fn_body(block));
                Node::Group(Box::new(Node::Concat(nodes.into())))
            }
            ImplItem::Const {
                name, ty, value, ..
            } => {
                let mut nodes = vec![
                    Node::StaticText("const "),
                    self.symbol(name.inner),
                ];

                if let Some(ty) = ty {
                    nodes.push(Node::StaticText(": "));
                    nodes.push(self.build_type_expression(&ty.inner));
                }

                nodes.push(Node::StaticText(" = "));
                nodes.push(self.build_expression(value));
                nodes.push(Node::StaticText(";"));
                Node::Concat(nodes.into())
            }
            ImplItem::AssociatedType { name, ty, .. } => Node::Concat(
                vec![
                    Node::StaticText("type "),
                    self.symbol(name.inner),
                    Node::StaticText(" = "),
                    self.build_type_expression(&ty.inner),
                    Node::StaticText(";"),
                ]
                .into(),
            ),
        }
    }

    fn build_trait_definition(
        &self,
        pub_span: Option<TextSpan>,
        name: &Spanned<SymbolU32>,
        supertraits: &[Spanned<TypeExpression>],
        items: &[Separated<Spanned<TraitItem>>],
    ) -> Node {
        let mut nodes = Vec::new();
        if pub_span.is_some() {
            nodes.push(Node::StaticText("pub "));
        }
        nodes.push(Node::StaticText("trait "));
        nodes.push(self.symbol(name.inner));

        if !supertraits.is_empty() {
            nodes.push(Node::StaticText(": "));
            for (index, supertrait) in supertraits.iter().enumerate() {
                if index > 0 {
                    nodes.push(Node::StaticText(" + "));
                }
                nodes.push(self.build_type_expression(&supertrait.inner));
            }
        }

        nodes.push(Node::StaticText(" {"));

        if !items.is_empty() {
            nodes.push(Node::Indent(Box::new(Node::Concat(
                vec![
                    Node::HardLine,
                    self.build_trait_item_list(items),
                ]
                .into(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(nodes.into())))
    }

    fn build_trait_item_list(&self, items: &[Separated<Spanned<TraitItem>>]) -> Node {
        let mut nodes = Vec::new();
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                nodes.push(Node::BlankLine);
                let prev = &items[index - 1];
                for c in self.comments.between(prev.inner.span.end, item.inner.span.start) {
                    nodes.push(Node::HardLine);
                    nodes.push(Node::SourceText(c.span));
                }
                nodes.push(Node::HardLine);
            }
            nodes.push(self.build_trait_item(&item.inner.inner));
        }
        Node::Concat(nodes.into())
    }

    fn build_trait_item(&self, item: &TraitItem) -> Node {
        match item {
            TraitItem::Function {
                attributes,
                signature,
                body,
                ..
            } => {
                let mut nodes = Vec::new();
                self.build_attributes(&mut nodes, attributes);
                self.build_function_signature(&mut nodes, signature);
                match body {
                    Some(body) => {
                        nodes.push(Node::StaticText(" "));
                        nodes.push(self.build_fn_body(body));
                        Node::Group(Box::new(Node::Concat(nodes.into())))
                    }
                    None => {
                        nodes.push(Node::StaticText(";"));
                        Node::Concat(nodes.into())
                    }
                }
            }
            TraitItem::Const { name, ty, .. } => Node::Concat(
                vec![
                    Node::StaticText("const "),
                    self.symbol(name.inner),
                    Node::StaticText(": "),
                    self.build_type_expression(&ty.inner),
                    Node::StaticText(";"),
                ]
                .into(),
            ),
            TraitItem::AssociatedType { name, bounds, .. } => {
                let mut nodes = vec![
                    Node::StaticText("type "),
                    self.symbol(name.inner),
                ];

                if !bounds.is_empty() {
                    nodes.push(Node::StaticText(": "));
                    for (index, bound) in bounds.iter().enumerate() {
                        if index > 0 {
                            nodes.push(Node::StaticText(" + "));
                        }
                        nodes.push(self.build_type_expression(&bound.inner));
                    }
                }

                nodes.push(Node::StaticText(";"));
                Node::Concat(nodes.into())
            }
        }
    }

    fn build_const_definition(
        &self,
        pub_span: Option<TextSpan>,
        name: &Spanned<SymbolU32>,
        ty: &Option<Box<Spanned<TypeExpression>>>,
        value: &Spanned<Expression>,
    ) -> Node {
        let mut nodes = Vec::new();
        if pub_span.is_some() {
            nodes.push(Node::StaticText("pub "));
        }
        nodes.push(Node::StaticText("const "));
        nodes.push(self.symbol(name.inner));

        if let Some(ty) = ty {
            nodes.push(Node::StaticText(": "));
            nodes.push(self.build_type_expression(&ty.inner));
        }

        nodes.push(Node::StaticText(" = "));
        nodes.push(self.build_expression(value));
        nodes.push(Node::StaticText(";"));
        Node::Concat(nodes.into())
    }

    fn build_enum_definition(
        &self,
        pub_span: Option<TextSpan>,
        repr: &Option<Box<Spanned<TypeExpression>>>,
        name: &Spanned<SymbolU32>,
        variants: &[Separated<Spanned<EnumVariant>>],
    ) -> Node {
        let mut nodes = Vec::new();
        if pub_span.is_some() {
            nodes.push(Node::StaticText("pub "));
        }
        nodes.push(Node::StaticText("enum "));
        nodes.push(self.symbol(name.inner));

        if let Some(repr) = repr {
            nodes.push(Node::StaticText(": "));
            nodes.push(self.build_type_expression(&repr.inner));
        }

        nodes.push(Node::StaticText(" {"));

        if !variants.is_empty() {
            let variant_items = variants
                .iter()
                .enumerate()
                .flat_map(|(index, variant)| {
                    let mut variant_nodes =
                        vec![self.symbol(variant.inner.inner.name.inner)];

                    if let Some(value) = &variant.inner.inner.value {
                        variant_nodes.push(Node::StaticText(" = "));
                        variant_nodes.push(self.build_expression(value));
                    }

                    if index + 1 < variants.len() {
                        variant_nodes.push(Node::StaticText(","));
                    } else {
                        variant_nodes.push(Node::IfBreak(','));
                    }

                    let mut result = vec![Node::Concat(variant_nodes.into())];
                    if index + 1 < variants.len() {
                        result.push(Node::HardLine);
                    }
                    result
                })
                .collect::<Vec<_>>();

            nodes.push(Node::Indent(Box::new(Node::Concat(
                std::iter::once(Node::HardLine)
                    .chain(variant_items)
                    .collect::<Box<[_]>>(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Concat(nodes.into())
    }

    fn build_struct_declaration(
        &self,
        name: &Spanned<SymbolU32>,
        type_params: &[TypeParam],
        fields: &[Separated<Spanned<StructField>>],
        pub_span: Option<TextSpan>,
    ) -> Node {
        let mut items = Vec::new();
        if pub_span.is_some() {
            items.push(Node::StaticText("pub "));
        }
        items.push(Node::StaticText("struct "));
        items.push(self.symbol(name.inner));
        self.build_type_params(&mut items, type_params);
        items.push(Node::StaticText(" {"));

        if !fields.is_empty() {
            let field_count = fields.len();
            let field_items = fields
                .iter()
                .enumerate()
                .flat_map(|(index, field)| {
                    let mut nodes = Vec::new();

                    if field.inner.inner.pub_span.is_some() {
                        nodes.push(Node::StaticText("pub "));
                    }
                    nodes.push(self.symbol(field.inner.inner.name.inner));
                    nodes.push(Node::StaticText(": "));
                    nodes.push(self.build_type_expression(&field.inner.inner.ty.inner));
                    nodes.push(Node::StaticText(","));

                    let mut result = vec![Node::Concat(nodes.into())];
                    if index + 1 < field_count {
                        result.push(Node::HardLine);
                    }
                    result
                })
                .collect::<Vec<_>>();

            items.push(Node::Indent(Box::new(Node::Concat(
                std::iter::once(Node::HardLine)
                    .chain(field_items)
                    .collect::<Box<[_]>>(),
            ))));
            items.push(Node::HardLine);
        }

        items.push(Node::StaticText("}"));
        Node::Concat(items.into())
    }

    fn build_export_definition(&self, entries: &[Separated<Spanned<ExportEntry>>]) -> Node {
        let mut items = Vec::new();
        items.push(Node::StaticText("export {"));

        if !entries.is_empty() {
            let entry_items = entries
                .iter()
                .enumerate()
                .flat_map(|(index, entry)| {
                    let mut entry_nodes = Vec::new();

                    entry_nodes.push(self.symbol(entry.inner.inner.name.inner));

                    if let Some(alias) = &entry.inner.inner.alias {
                        entry_nodes.push(Node::StaticText(" as "));
                        entry_nodes.push(self.symbol(alias.inner));
                    }

                    if entry.separator.is_some() {
                        entry_nodes.push(Node::StaticText(","));
                    }

                    let mut result = vec![Node::Concat(entry_nodes.into())];

                    // Add line break after each entry except the last
                    if index + 1 < entries.len() {
                        result.push(Node::Line);
                    }

                    result
                })
                .collect::<Vec<_>>();

            items.push(Node::Indent(Box::new(Node::Concat(
                std::iter::once(Node::Line)
                    .chain(entry_items)
                    .collect::<Box<[_]>>(),
            ))));
        }

        items.push(Node::Line);
        items.push(Node::StaticText("}"));
        Node::Concat(items.into())
    }

    fn build_attributes(&self, out: &mut Vec<Node>, attributes: &[Attribute]) {
        for attr in attributes {
            out.push(Node::StaticText("#["));
            out.push(self.symbol(attr.name.inner));
            out.push(Node::StaticText("]"));
            out.push(Node::HardLine);
        }
    }

    fn build_type_params(&self, out: &mut Vec<Node>, type_params: &[TypeParam]) {
        if type_params.is_empty() {
            return;
        }
        out.push(Node::StaticText("<"));
        for (index, param) in type_params.iter().enumerate() {
            out.push(self.symbol(param.name.inner));
            if !param.bounds.is_empty() {
                out.push(Node::StaticText(": "));
                for (bi, bound) in param.bounds.iter().enumerate() {
                    out.push(self.build_type_expression(&bound.inner));
                    if bi + 1 < param.bounds.len() {
                        out.push(Node::StaticText(" + "));
                    }
                }
            }
            if index + 1 < type_params.len() {
                out.push(Node::StaticText(", "));
            }
        }
        out.push(Node::StaticText(">"));
    }

    fn build_function_signature(&self, out: &mut Vec<Node>, signature: &FunctionSignature) {
        out.push(Node::StaticText("fn "));
        out.push(self.symbol(signature.name.inner));

        self.build_type_params(out, &signature.type_params);

        out.push(Node::StaticText("("));

        if signature.params.len() > 0 {
            out.push(Node::Indent(Box::new({
                let mut params = Vec::new();
                params.push(Node::Line);

                for (index, param) in signature.params.iter().enumerate() {
                    if param.inner.inner.mut_span.is_some() {
                        params.push(Node::StaticText("mut "));
                    }
                    params.push(self.symbol(param.inner.inner.name.inner));
                    if let Some(ty) = &param.inner.inner.ty {
                        params.push(Node::StaticText(": "));
                        params.push(self.build_type_expression(&ty.inner));
                    }
                    if index + 1 < signature.params.len() {
                        params.push(Node::StaticText(","));
                        params.push(Node::SoftLine);
                    } else {
                        params.push(Node::IfBreak(','));
                    }
                }

                Node::Concat(params.into())
            })));
            out.push(Node::Line);
        }

        out.push(Node::StaticText(")"));
        if let Some(result) = &signature.result {
            out.push(Node::StaticText(" -> "));
            out.push(self.build_type_expression(&result.inner));
        }
    }

    fn build_global_definition(
        &self,
        pub_span: Option<TextSpan>,
        mut_span: Option<TextSpan>,
        name: &Spanned<SymbolU32>,
        type_annotation: &Option<Box<Spanned<TypeExpression>>>,
        value: &Box<Spanned<Expression>>,
    ) -> Node {
        let mut items = Vec::new();
        if pub_span.is_some() {
            items.push(Node::StaticText("pub "));
        }
        items.push(Node::StaticText("global "));
        if mut_span.is_some() {
            items.push(Node::StaticText("mut "));
        }
        items.push(self.symbol(name.inner));

        if let Some(annotation) = type_annotation {
            items.push(Node::StaticText(": "));
            items.push(self.build_type_expression(&annotation.inner));
        }

        items.push(Node::StaticText(" = "));
        items.push(self.build_expression(value));
        items.push(Node::StaticText(";"));

        Node::Concat(items.into())
    }

    /// Builds a function/method/trait-method body — always multi-line.
    fn build_fn_body(&self, block: &Spanned<Expression>) -> Node {
        match &block.inner {
            Expression::Block { statements } => self.build_block(statements, true),
            _ => unreachable!("function body must be a block expression"),
        }
    }

    /// `force_break` keeps the block always multi-line (used for function /
    /// method / trait bodies).  When `false`, a single-statement block uses
    /// `SoftLine` separators so the renderer can collapse it to one line when
    /// the content fits within `max_line_width`.
    fn build_block(
        &self,
        statements: &[Separated<Spanned<Statement>>],
        force_break: bool,
    ) -> Node {
        let mut items = Vec::new();
        items.push(Node::StaticText("{"));

        if !statements.is_empty() {
            let single = !force_break && statements.len() == 1;
            items.push(Node::Indent(Box::new({
                let mut inner = Vec::new();
                inner.push(if single {
                    Node::SoftLine
                } else {
                    Node::HardLine
                });

                for (index, statement) in statements.iter().enumerate() {
                    if index > 0 {
                        let prev_end = statements[index - 1].inner.span.end as u32;
                        let curr_start = statement.inner.span.start as u32;
                        let gap_comments = self.comments.between(prev_end, curr_start);
                        // Measure blank lines only up to the first comment so
                        // the comment's own newline isn't counted as a blank line.
                        let blank_end = gap_comments
                            .first()
                            .map_or(curr_start as usize, |c| c.span.start as usize);
                        let blank_lines = Self::count_blank_lines(
                            self.source,
                            prev_end as usize,
                            blank_end,
                        );
                        for _ in 0..blank_lines {
                            inner.push(Node::HardLine);
                        }
                        // Emit comments that fall in the gap between the two statements.
                        for c in gap_comments {
                            inner.push(Node::SourceText(c.span));
                            inner.push(Node::HardLine);
                        }
                    }
                    inner.push(self.build_statement(&statement.inner.inner));
                    if index + 1 == statements.len() {
                        match statement.separator {
                            Some(_) => inner.push(Node::StaticText(";")),
                            None => {}
                        }
                    } else {
                        let needs_semi = if statement.inner.inner.is_block_like() {
                            statement.separator.is_some()
                        } else {
                            true
                        };
                        if needs_semi {
                            inner.push(Node::StaticText(";"));
                        }
                        inner.push(Node::HardLine);
                    }
                }

                Node::Concat(inner.into())
            })));
            items.push(if single {
                Node::SoftLine
            } else {
                Node::HardLine
            })
        }

        items.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(items.into())))
    }

    /// Append `(arg1, arg2, …)` to `out`.
    ///
    /// Flat mode: `(arg1, arg2, arg3)`
    /// Break mode: `(\n    arg1,\n    arg2,\n    arg3,\n)`
    fn build_call_args(&self, out: &mut Vec<Node>, arguments: &[Separated<Spanned<Expression>>]) {
        out.push(Node::StaticText("("));
        if !arguments.is_empty() {
            let mut arg_nodes = vec![Node::Line];
            for (index, arg) in arguments.iter().enumerate() {
                arg_nodes.push(self.build_expression(&arg.inner));
                if index + 1 < arguments.len() {
                    arg_nodes.push(Node::StaticText(","));
                    arg_nodes.push(Node::SoftLine);
                } else {
                    arg_nodes.push(Node::IfBreak(','));
                }
            }
            out.push(Node::Indent(Box::new(Node::Concat(arg_nodes.into()))));
            out.push(Node::Line);
        }
        out.push(Node::StaticText(")"));
    }

    fn build_expression(&self, expression: &Spanned<Expression>) -> Node {
        match &expression.inner {
            Expression::Path(path) => self.build_path(path),
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                // Flatten left-recursive chains of the same operator into one
                // group so that all operands break together rather than one at
                // a time from the inside out.
                //
                //   a | b | c | d   →  Group([a, Indent(SL | b), Indent(SL | c), Indent(SL | d)])
                //
                // In flat mode:  "a | b | c | d"
                // In break mode: "a\n    | b\n    | c\n    | d"
                let mut operands: Vec<&Spanned<Expression>> = vec![right, left];
                let mut current = left;
                while let Expression::Binary {
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

                let mut parts = vec![self.build_expression(operands[0])];
                for operand in &operands[1..] {
                    parts.push(Node::Indent(Box::new(Node::Concat(
                        vec![
                            Node::SoftLine,
                            Node::StaticText(operator.inner.as_str()),
                            Node::StaticText(" "),
                            self.build_expression(operand),
                        ]
                        .into(),
                    ))));
                }
                Node::Group(Box::new(Node::Concat(parts.into())))
            }
            Expression::Block { statements } => self.build_block(statements, false),
            Expression::Unreachable => Node::StaticText("unreachable"),
            Expression::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("if "));
                items.push(self.build_expression(condition));
                items.push(Node::StaticText(" "));
                items.push(self.build_expression(then_block));
                if let Some(else_block) = else_block {
                    items.push(Node::StaticText(" else "));
                    items.push(self.build_expression(else_block));
                }

                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Expression::Loop { block } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("loop "));
                items.push(self.build_expression(block));

                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Expression::Break { label, value } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("break"));
                if let Some(label) = label {
                    items.push(Node::StaticText(" :"));
                    items.push(self.symbol(label.inner));
                }
                if let Some(value) = value {
                    items.push(Node::StaticText(" "));
                    items.push(self.build_expression(value));
                }

                Node::Concat(items.into())
            }
            Expression::Return { value } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("return"));
                if let Some(value) = value {
                    items.push(Node::StaticText(" "));
                    items.push(self.build_expression(value));
                }

                Node::Concat(items.into())
            }
            Expression::Cast { value, ty } => {
                let mut items = Vec::new();
                items.push(self.build_expression(value));
                items.push(Node::StaticText(" as "));
                items.push(self.build_type_expression(&ty.inner));

                Node::Concat(items.into())
            }
            Expression::Continue { label } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("continue"));
                if let Some(label) = label {
                    items.push(Node::StaticText(" :"));
                    items.push(self.symbol(label.inner));
                }

                Node::Concat(items.into())
            }
            Expression::Int { .. } => {
                // Preserve the original literal text from source
                Node::SourceText(expression.span)
            }
            Expression::Float { .. } => {
                // Preserve the original literal text from source
                Node::SourceText(expression.span)
            }
            Expression::Grouping { value } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("("));
                items.push(self.build_expression(value));
                items.push(Node::StaticText(")"));
                Node::Concat(items.into())
            }
            Expression::Call { callee, arguments } => {
                let mut items = Vec::new();
                items.push(self.build_expression(callee));
                self.build_call_args(&mut items, arguments);
                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Expression::MethodCall(mc) => {
                let (object, method, type_args, arguments) =
                    (&mc.object, &mc.method, &mc.type_args, &mc.arguments);
                let mut items = Vec::new();
                items.push(self.build_expression(object));
                items.push(Node::StaticText("."));
                items.push(self.symbol(method.inner));
                if !type_args.is_empty() {
                    items.push(Node::StaticText("::<"));
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            items.push(Node::StaticText(", "));
                        }
                        items.push(self.build_type_expression(&arg.inner));
                    }
                    items.push(Node::StaticText(">"));
                }
                self.build_call_args(&mut items, arguments);
                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Expression::Label { label, block } => {
                let mut items = Vec::new();
                items.push(self.symbol(label.inner));
                items.push(Node::StaticText(": "));
                items.push(self.build_expression(block));
                Node::Concat(items.into())
            }
            Expression::Error => unreachable!(),
            Expression::Unary { operator, operand } => {
                let mut items = Vec::new();
                items.push(Node::StaticText(operator.inner.as_str()));
                items.push(self.build_expression(operand));

                Node::Concat(items.into())
            }
            Expression::String { .. } | Expression::Char { .. } => {
                Node::SourceText(expression.span)
            }
            Expression::ObjectAccess { object, member } => {
                let mut items = Vec::new();
                items.push(self.build_expression(object));
                items.push(Node::StaticText("."));
                items.push(self.symbol(member.inner));
                Node::Concat(items.into())
            }
            Expression::Deref { pointer } => Node::Concat(
                vec![
                    self.build_expression(pointer),
                    Node::StaticText(".*"),
                ]
                .into(),
            ),
            Expression::StructInit { path, fields } => {
                let mut items = Vec::new();
                items.push(self.build_path(path));
                items.push(Node::StaticText("::{"));

                let has_block_value = fields.iter().any(|f| {
                    f.inner
                        .inner
                        .value
                        .as_ref()
                        .is_some_and(|v| v.inner.is_block_like())
                });
                if !fields.is_empty() {
                    let field_count = fields.len();
                    let mut field_items: Vec<Node> = Vec::new();
                    for (index, field) in fields.iter().enumerate() {
                        field_items.push(self.symbol(field.inner.inner.name.inner));
                        if let Some(value) = &field.inner.inner.value {
                            field_items.push(Node::StaticText(": "));
                            field_items.push(self.build_expression(value));
                        }
                        let is_last = index + 1 == field_count;
                        if !is_last || has_block_value {
                            field_items.push(Node::StaticText(","));
                        } else {
                            field_items.push(Node::IfBreak(','));
                        }
                        if !is_last {
                            field_items.push(if has_block_value {
                                Node::HardLine
                            } else {
                                Node::SoftLine
                            });
                        }
                    }

                    items.push(Node::Indent(Box::new(Node::Concat(
                        std::iter::once(if has_block_value {
                            Node::HardLine
                        } else {
                            Node::SoftLine
                        })
                        .chain(field_items)
                        .collect::<Box<[_]>>(),
                    ))));
                    items.push(if has_block_value {
                        Node::HardLine
                    } else {
                        Node::SoftLine
                    });
                }

                items.push(Node::StaticText("}"));
                if has_block_value {
                    Node::Concat(items.into())
                } else {
                    Node::Group(Box::new(Node::Concat(items.into())))
                }
            }
            Expression::TypeApplication { callee, args } => {
                let mut items = Vec::new();
                items.push(self.build_expression(callee));
                items.push(Node::StaticText("::<"));
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        items.push(Node::StaticText(", "));
                    }
                    items.push(self.build_type_expression(&arg.inner));
                }
                items.push(Node::StaticText(">"));
                Node::Concat(items.into())
            }
            Expression::IntrinsicCall {
                name,
                type_args,
                arguments,
            } => {
                let mut items = Vec::new();
                items.push(self.symbol(name.inner));
                if !type_args.is_empty() {
                    items.push(Node::StaticText("::<"));
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            items.push(Node::StaticText(", "));
                        }
                        items.push(self.build_type_expression(&arg.inner));
                    }
                    items.push(Node::StaticText(">"));
                }
                items.push(Node::StaticText("("));
                for (index, arg) in arguments.iter().enumerate() {
                    items.push(self.build_expression(&arg.inner));
                    if index + 1 < arguments.len() {
                        items.push(Node::StaticText(", "));
                    }
                }
                items.push(Node::StaticText(")"));
                Node::Concat(items.into())
            }
            Expression::ArrayList { elements } => {
                let mut items = vec![Node::StaticText("[")];
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        items.push(Node::StaticText(", "));
                    }
                    items.push(self.build_expression(element));
                }
                items.push(Node::StaticText("]"));
                Node::Concat(items.into())
            }
            Expression::ArrayRepeat { value, count } => Node::Concat(Box::new([
                Node::StaticText("["),
                self.build_expression(value),
                Node::StaticText("; "),
                self.build_expression(count),
                Node::StaticText("]"),
            ])),
            Expression::Index { object, index } => Node::Concat(Box::new([
                self.build_expression(object),
                Node::StaticText("["),
                self.build_expression(index),
                Node::StaticText("]"),
            ])),
            Expression::SliceRange { object, start, end } => {
                let mut parts = vec![
                    self.build_expression(object),
                    Node::StaticText("["),
                ];
                if let Some(s) = start {
                    parts.push(self.build_expression(s));
                }
                parts.push(Node::StaticText(".."));
                if let Some(e) = end {
                    parts.push(self.build_expression(e));
                }
                parts.push(Node::StaticText("]"));
                Node::Concat(parts.into_boxed_slice())
            }
            Expression::Tuple { elements } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("("));

                if !elements.is_empty() {
                    let last_idx = elements.len() - 1;
                    let element_items = elements
                        .iter()
                        .enumerate()
                        .flat_map(|(index, element)| {
                            let mut nodes = Vec::new();
                            nodes.push(self.build_expression(element));

                            if index < last_idx {
                                nodes.push(Node::StaticText(","));
                            } else if elements.len() == 1 {
                                // Single-element tuple always needs trailing comma
                                nodes.push(Node::StaticText(","));
                            } else {
                                nodes.push(Node::IfBreak(','));
                            }

                            let mut result = vec![Node::Concat(nodes.into())];
                            if index < last_idx {
                                result.push(Node::SoftLine);
                            }
                            result
                        })
                        .collect::<Vec<_>>();

                    items.push(Node::Indent(Box::new(Node::Concat(
                        std::iter::once(Node::Line)
                            .chain(element_items)
                            .collect::<Box<[_]>>(),
                    ))));
                    items.push(Node::Line);
                }

                items.push(Node::StaticText(")"));
                Node::Group(Box::new(Node::Concat(items.into())))
            }
        }
    }

    fn build_pattern(out: &mut Vec<Node>, interner: &StringInterner, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard => out.push(Node::StaticText("_")),
            Pattern::Binding { mut_span, name } => {
                if mut_span.is_some() {
                    out.push(Node::StaticText("mut "));
                }
                let len = interner.resolve(name.inner).unwrap().len() as u32;
                out.push(Node::Symbol { symbol: name.inner, len });
            }
            Pattern::Tuple { elements } => {
                out.push(Node::StaticText("("));
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        out.push(Node::StaticText(", "));
                    }
                    Self::build_pattern(out, interner, &element.inner.inner);
                }
                out.push(Node::StaticText(")"));
            }
            Pattern::Struct { name, fields } => {
                let len = interner.resolve(name.inner).unwrap().len() as u32;
                out.push(Node::Symbol { symbol: name.inner, len });
                out.push(Node::StaticText(" {"));
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push(Node::StaticText(", "));
                    } else {
                        out.push(Node::StaticText(" "));
                    }
                    let flen = interner.resolve(field.inner.inner.name.inner).unwrap().len() as u32;
                    out.push(Node::Symbol { symbol: field.inner.inner.name.inner, len: flen });
                    if let Some(pat) = &field.inner.inner.pattern {
                        out.push(Node::StaticText(": "));
                        Self::build_pattern(out, interner, &pat.inner);
                    }
                }
                if !fields.is_empty() {
                    out.push(Node::StaticText(" "));
                }
                out.push(Node::StaticText("}"));
            }
        }
    }

    fn build_statement(&self, statement: &Statement) -> Node {
        match statement {
            Statement::Expression(expression) => self.build_expression(expression),
            Statement::LocalDefinition {
                pattern,
                ty: type_annotation,
                value,
            } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("local "));
                Self::build_pattern(&mut items, self.interner, &pattern.inner);
                if let Some(annotation) = type_annotation {
                    items.push(Node::StaticText(": "));
                    items.push(self.build_type_expression(&annotation.inner));
                }
                let value_node = self.build_expression(value);
                if value.inner.is_block_like() {
                    // Block-like values (if/else, loop, struct init, …) manage
                    // their own indentation; keep " = " on the same line.
                    items.push(Node::StaticText(" = "));
                    items.push(value_node);
                } else {
                    // Scalar/expression values: " =" then a SoftLine so the
                    // value can move to the next line when it doesn't fit.
                    items.push(Node::StaticText(" ="));
                    items.push(Node::Indent(Box::new(Node::Concat(
                        vec![Node::SoftLine, value_node].into(),
                    ))));
                }

                Node::Group(Box::new(Node::Concat(items.into())))
            }
        }
    }

    fn build_path(&self, path: &Path) -> Node {
        let mut items: Vec<Node> = Vec::new();
        for (i, seg) in path.segments.iter().enumerate() {
            if i > 0 {
                items.push(Node::StaticText("::"));
            }
            items.push(self.symbol(seg.ident.inner));
            if !seg.type_args.is_empty() {
                items.push(Node::StaticText("::<"));
                for (j, arg) in seg.type_args.iter().enumerate() {
                    if j > 0 {
                        items.push(Node::StaticText(", "));
                    }
                    items.push(self.build_type_expression(&arg.inner));
                }
                items.push(Node::StaticText(">"));
            }
        }
        Node::Concat(items.into())
    }

    fn build_type_expression(&self, type_expression: &TypeExpression) -> Node {
        match type_expression {
            TypeExpression::Infer => Node::StaticText("_"),
            TypeExpression::Path(path) => self.build_path(path),
            TypeExpression::Function { params, result } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("fn("));

                if params.len() > 0 {
                    items.push(Node::Indent(Box::new({
                        let mut param_items = Vec::new();
                        param_items.push(Node::Line);

                        for (index, param) in params.iter().enumerate() {
                            if let Some(name) = &param.inner.inner.name {
                                param_items.push(self.symbol(name.inner));
                                param_items.push(Node::StaticText(": "));
                            }
                            param_items.push(self.build_type_expression(
                                &param.inner.inner.ty.inner,
                            ));
                            if index + 1 < params.len() {
                                param_items.push(Node::StaticText(","));
                                param_items.push(Node::SoftLine);
                            } else {
                                param_items.push(Node::IfBreak(','));
                            }
                        }

                        Node::Concat(param_items.into())
                    })));
                    items.push(Node::Line);
                }

                items.push(Node::StaticText(")"));
                if let Some(result) = result {
                    items.push(Node::StaticText(" -> "));
                    items.push(self.build_type_expression(&result.inner));
                }

                Node::Group(Box::new(Node::Concat(items.into())))
            }
            TypeExpression::Pointer { mutability, inner } => {
                let mut items = vec![Node::StaticText("*")];
                if mutability.is_some() {
                    items.push(Node::StaticText("mut "));
                }
                items.push(self.build_type_expression(&inner.inner));
                Node::Concat(items.into())
            }
            TypeExpression::Slice { mutability, inner } => {
                let mut items = vec![Node::StaticText("[]")];
                if mutability.is_some() {
                    items.push(Node::StaticText("mut "));
                }
                items.push(self.build_type_expression(&inner.inner));
                Node::Concat(items.into())
            }
            TypeExpression::Array {
                size,
                mutability,
                inner,
            } => {
                let mut items = vec![
                    Node::StaticText("["),
                    Node::SourceText(size.span),
                    Node::StaticText("]"),
                ];
                if mutability.is_some() {
                    items.push(Node::StaticText("mut "));
                }
                items.push(self.build_type_expression(&inner.inner));
                Node::Concat(items.into())
            }
            TypeExpression::Tuple { elements } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("("));

                if !elements.is_empty() {
                    let last_idx = elements.len() - 1;
                    let element_items = elements
                        .iter()
                        .enumerate()
                        .flat_map(|(index, element)| {
                            let mut nodes =
                                vec![self.build_type_expression(&element.inner)];

                            if index < last_idx {
                                nodes.push(Node::StaticText(","));
                            } else if elements.len() == 1 {
                                nodes.push(Node::StaticText(","));
                            } else {
                                nodes.push(Node::IfBreak(','));
                            }

                            let mut result = vec![Node::Concat(nodes.into())];
                            if index < last_idx {
                                result.push(Node::SoftLine);
                            }
                            result
                        })
                        .collect::<Vec<_>>();

                    items.push(Node::Indent(Box::new(Node::Concat(
                        std::iter::once(Node::Line)
                            .chain(element_items)
                            .collect::<Box<[_]>>(),
                    ))));
                    items.push(Node::Line);
                }

                items.push(Node::StaticText(")"));
                Node::Group(Box::new(Node::Concat(items.into())))
            }
            TypeExpression::MemoryTagged { memory, inner } => Node::Concat(
                vec![
                    self.build_path(memory),
                    Node::StaticText("::"),
                    self.build_type_expression(&inner.inner),
                ]
                .into(),
            ),
            TypeExpression::GenericApplication { name, args } => {
                let mut inner_parts: Vec<Node> = Vec::new();
                for (i, sep) in args.iter().enumerate() {
                    if i > 0 {
                        inner_parts.push(Node::StaticText(", "));
                    }
                    match &sep.inner.inner {
                        GenericArg::Type(ty) => {
                            inner_parts.push(self.build_type_expression(&ty.inner));
                        }
                        GenericArg::Binding { name: key, ty } => {
                            inner_parts.push(Node::Concat(
                                vec![
                                    self.symbol(key.inner),
                                    Node::StaticText(" = "),
                                    self.build_type_expression(&ty.inner),
                                ]
                                .into(),
                            ));
                        }
                    }
                }
                Node::Concat(
                    vec![
                        self.symbol(name.inner),
                        Node::StaticText("<"),
                        Node::Concat(inner_parts.into()),
                        Node::StaticText(">"),
                    ]
                    .into(),
                )
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

pub struct Renderer<'interner> {
    config: RendererConfig,
    interner: &'interner StringInterner,
    source: &'interner str,
    buffer: String,
    position: usize,
    indent: usize,
}

#[derive(Clone, Copy)]
enum RenderMode {
    Flat,
    Break,
}

impl<'interner> Renderer<'interner> {
    pub fn new(
        config: RendererConfig,
        interner: &'interner StringInterner,
        source: &'interner str,
    ) -> Self {
        Self {
            config,
            interner,
            source,
            buffer: String::new(),
            position: 0,
            indent: 0,
        }
    }

    fn render(mut self, node: &Node) -> String {
        self.render_node(node, RenderMode::Break);
        self.buffer
    }

    fn render_node(&mut self, node: &Node, mode: RenderMode) {
        match node {
            Node::StaticText(s) => {
                self.buffer.push_str(s);
                self.position += s.len();
            }
            Node::SourceText(span) => {
                let text = span.extract_str(self.source);
                self.buffer.push_str(text);
                self.position += text.len();
            }
            Node::Symbol { symbol, .. } => {
                let resolved = self.interner.resolve(*symbol).unwrap();
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
                    self.buffer.push_str(" ".repeat(self.indent).as_str());
                    self.position = self.indent;
                }
            },
            Node::Line => match mode {
                RenderMode::Flat => {}
                RenderMode::Break => {
                    self.buffer.push('\n');
                    self.buffer.push_str(" ".repeat(self.indent).as_str());
                    self.position = self.indent;
                }
            },
            Node::BlankLine => {
                self.buffer.push('\n');
                self.position = 0;
            }
            Node::Concat(nodes) => {
                for node in nodes {
                    self.render_node(node, mode);
                }
            }
            Node::Group(inner) => {
                let mode = if self.measure_flat(node)
                    <= self.config.max_line_width as usize - self.position
                {
                    RenderMode::Flat
                } else {
                    RenderMode::Break
                };
                self.render_node(inner, mode);
            }

            Node::Indent(inner) => {
                self.indent += self.config.indent_width as usize;
                self.render_node(inner, mode);
                self.indent -= self.config.indent_width as usize;
            }
            Node::IfBreak(ch) => match mode {
                RenderMode::Flat => {}
                RenderMode::Break => {
                    self.buffer.push(*ch);
                    self.position += 1;
                }
            },
            Node::HardLine => {
                self.buffer.push('\n');
                self.buffer.push_str(" ".repeat(self.indent).as_str());
                self.position = self.indent;
            }
        }
    }

    fn measure_flat(&self, node: &Node) -> usize {
        let mut width = 0;
        let mut stack = vec![node];

        while let Some(current) = stack.pop() {
            match current {
                Node::StaticText(s) => width += s.len(),
                Node::SourceText(span) => width += (span.end - span.start) as usize,
                Node::Symbol { len, .. } => width += *len as usize,
                Node::SoftLine => width += 1,
                Node::Line => {}
                Node::BlankLine => return width,
                Node::IfBreak(_) => {}
                Node::HardLine => return width,
                Node::Group(inner) => stack.push(inner),
                Node::Indent(inner) => stack.push(inner),
                Node::Concat(nodes) => {
                    stack.reserve(nodes.len());
                    stack.extend(nodes.iter().rev());
                }
            }
        }

        width
    }
}

pub fn format(
    ast: &AST,
    interner: &StringInterner,
    source: &str,
    config: RendererConfig,
) -> String {
    let builder = Builder { interner, source, comments: &ast.comments };
    let mut root_items = Vec::new();
    root_items.push(builder.build(ast));
    root_items.push(Node::HardLine);

    let root = Node::Concat(root_items.into());
    let renderer = Renderer::new(config, interner, source);
    renderer.render(&root)
}
