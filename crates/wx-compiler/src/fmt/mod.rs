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

struct Builder;

impl Builder {
    fn symbol(interner: &StringInterner, symbol: SymbolU32) -> Node {
        let len = interner.resolve(symbol).unwrap().len() as u32;
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

    fn build(ast: &AST, interner: &StringInterner, source: &str) -> Node {
        Self::build_item_list(interner, source, &ast.items)
    }

    fn build_item_list(
        interner: &StringInterner,
        source: &str,
        items: &[Separated<Spanned<Item>>],
    ) -> Node {
        let mut nodes = Vec::new();
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                nodes.push(Node::BlankLine);
                nodes.push(Node::HardLine);
            }
            nodes.push(Self::build_item(interner, source, &item.inner.inner));
        }
        Node::Concat(nodes.into())
    }

    fn build_item(interner: &StringInterner, source: &str, item: &Item) -> Node {
        match item {
            Item::Function {
                signature,
                block,
                attributes,
                pub_span,
                ..
            } => {
                let mut items = Vec::new();
                Builder::build_attributes(&mut items, interner, attributes);
                if pub_span.is_some() {
                    items.push(Node::StaticText("pub "));
                }
                Builder::build_function_signature(&mut items, interner, signature);
                items.push(Node::StaticText(" "));
                items.push(Self::build_expression(interner, source, block));
                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Item::FunctionDeclaration {
                pub_span,
                attributes,
                signature,
                ..
            } => {
                let mut items = Vec::new();
                Builder::build_attributes(&mut items, interner, attributes);
                if pub_span.is_some() {
                    items.push(Node::StaticText("pub "));
                }
                Builder::build_function_signature(&mut items, interner, signature);
                items.push(Node::StaticText(";"));
                Node::Concat(items.into())
            }
            Item::IntrinsicFunction { signature, .. } => {
                let mut items = Vec::new();
                Builder::build_function_signature(&mut items, interner, signature);
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
            } => Self::build_global_definition(
                interner,
                source,
                *pub_span,
                *mut_span,
                name,
                type_annotation,
                value,
            ),
            Item::Export { entries } => Builder::build_export_definition(interner, entries),
            Item::Import {
                module,
                alias,
                entries,
            } => Self::build_import_definition(interner, module, alias, entries),
            Item::Memory { name, kind, .. } => Node::Concat(
                vec![
                    Node::StaticText("memory "),
                    Self::symbol(interner, name.inner),
                    Node::StaticText(": "),
                    Self::build_type_expression(interner, &kind.inner),
                    Node::StaticText(";"),
                ]
                .into(),
            ),
            Item::Enum {
                pub_span,
                repr,
                name,
                variants,
                ..
            } => Self::build_enum_definition(interner, source, *pub_span, repr, name, variants),
            Item::Impl { type_params, target, items } => {
                Self::build_impl_definition(interner, source, type_params, target, items)
            }
            Item::ImplTrait {
                trait_name,
                target,
                items,
                ..
            } => Self::build_impl_trait_definition(interner, source, trait_name, target, items),
            Item::Const {
                pub_span,
                name,
                ty,
                value,
                ..
            } => Self::build_const_definition(interner, source, *pub_span, name, ty, value),
            Item::Module {
                pub_span,
                name,
                items,
            } => Self::build_module_definition(interner, source, *pub_span, name, items),
            Item::ModuleDeclaration { pub_span, name } => {
                Self::build_module_declaration(interner, *pub_span, name)
            }
            Item::Trait {
                pub_span,
                name,
                supertraits,
                items,
                ..
            } => {
                Self::build_trait_definition(interner, source, *pub_span, name, supertraits, items)
            }
            Item::Struct {
                id: _,
                name,
                type_params,
                fields,
                pub_span,
            } => Self::build_struct_declaration(interner, name, type_params, fields, *pub_span),
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
                items.push(Self::symbol(interner, name.inner));
                items.push(Node::StaticText(" { "));
                for (i, m) in members.iter().enumerate() {
                    if i > 0 {
                        items.push(Node::StaticText(", "));
                    }
                    items.push(Self::build_type_expression(interner, &m.inner.inner));
                }
                items.push(Node::StaticText(" }"));
                Node::Group(Box::new(Node::Concat(items.into())))
            }
        }
    }

    fn build_import_definition(
        interner: &StringInterner,
        module: &Spanned<SymbolU32>,
        alias: &Option<Spanned<SymbolU32>>,
        entries: &[Separated<Spanned<ImportEntry>>],
    ) -> Node {
        let mut items = Vec::new();

        items.push(Node::StaticText("import "));
        items.push(Self::symbol(interner, module.inner));

        if let Some(alias) = alias {
            items.push(Node::StaticText(" as "));
            items.push(Self::symbol(interner, alias.inner));
        }

        items.push(Node::StaticText(" {"));

        if !entries.is_empty() {
            let entry_items = entries
                .iter()
                .enumerate()
                .flat_map(|(index, entry)| {
                    let mut entry_nodes = Vec::new();

                    if let Some(ext_name) = &entry.inner.inner.external_name {
                        entry_nodes.push(Self::symbol(interner, ext_name.inner));
                        entry_nodes.push(Node::StaticText(": "));
                    }

                    match &entry.inner.inner.declaration {
                        ImportDeclaration::Function { signature, .. } => {
                            Builder::build_function_signature(
                                &mut entry_nodes,
                                interner,
                                signature,
                            );
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
                            entry_nodes.push(Self::symbol(interner, name.inner));
                            entry_nodes.push(Node::StaticText(": "));
                            entry_nodes.push(Self::build_type_expression(interner, &ty.inner));
                        }
                        ImportDeclaration::Memory { name, kind, .. } => {
                            entry_nodes.push(Node::StaticText("memory "));
                            entry_nodes.push(Self::symbol(interner, name.inner));
                            entry_nodes.push(Node::StaticText(": "));
                            entry_nodes.push(Self::build_type_expression(interner, &kind.inner));
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
        interner: &StringInterner,
        source: &str,
        pub_span: Option<TextSpan>,
        name: &Spanned<SymbolU32>,
        items: &[Separated<Spanned<Item>>],
    ) -> Node {
        let mut nodes = Vec::new();
        if pub_span.is_some() {
            nodes.push(Node::StaticText("pub "));
        }
        nodes.push(Node::StaticText("module "));
        nodes.push(Self::symbol(interner, name.inner));
        nodes.push(Node::StaticText(" {"));

        if !items.is_empty() {
            nodes.push(Node::Indent(Box::new(Node::Concat(
                vec![
                    Node::HardLine,
                    Self::build_item_list(interner, source, items),
                ]
                .into(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(nodes.into())))
    }

    fn build_module_declaration(
        interner: &StringInterner,
        pub_span: Option<TextSpan>,
        name: &Spanned<SymbolU32>,
    ) -> Node {
        let mut nodes = Vec::new();
        if pub_span.is_some() {
            nodes.push(Node::StaticText("pub "));
        }
        nodes.push(Node::StaticText("module "));
        nodes.push(Self::symbol(interner, name.inner));
        nodes.push(Node::StaticText(";"));
        Node::Concat(nodes.into())
    }

    fn build_impl_definition(
        interner: &StringInterner,
        source: &str,
        type_params: &[TypeParam],
        target: &Spanned<TypeExpression>,
        items: &[Separated<Spanned<ImplItem>>],
    ) -> Node {
        let mut nodes = vec![Node::StaticText("impl ")];
        Self::build_type_params(&mut nodes, interner, type_params);
        if !type_params.is_empty() {
            nodes.push(Node::StaticText(" "));
        }
        nodes.push(Self::build_type_expression(interner, &target.inner));
        nodes.push(Node::StaticText(" {"));

        if !items.is_empty() {
            nodes.push(Node::Indent(Box::new(Node::Concat(
                vec![
                    Node::HardLine,
                    Self::build_impl_item_list(interner, source, items),
                ]
                .into(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(nodes.into())))
    }

    fn build_impl_trait_definition(
        interner: &StringInterner,
        source: &str,
        trait_name: &Spanned<TypeExpression>,
        target: &Spanned<TypeExpression>,
        items: &[Separated<Spanned<ImplItem>>],
    ) -> Node {
        let mut nodes = vec![
            Node::StaticText("impl "),
            Self::build_type_expression(interner, &trait_name.inner),
            Node::StaticText(" for "),
            Self::build_type_expression(interner, &target.inner),
            Node::StaticText(" {"),
        ];

        if !items.is_empty() {
            nodes.push(Node::Indent(Box::new(Node::Concat(
                vec![
                    Node::HardLine,
                    Self::build_impl_item_list(interner, source, items),
                ]
                .into(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(nodes.into())))
    }

    fn build_impl_item_list(
        interner: &StringInterner,
        source: &str,
        items: &[Separated<Spanned<ImplItem>>],
    ) -> Node {
        let mut nodes = Vec::new();
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                nodes.push(Node::BlankLine);
                nodes.push(Node::HardLine);
            }
            nodes.push(Self::build_impl_item(interner, source, &item.inner.inner));
        }
        Node::Concat(nodes.into())
    }

    fn build_impl_item(interner: &StringInterner, source: &str, item: &ImplItem) -> Node {
        match item {
            ImplItem::Method {
                pub_span,
                attributes,
                signature,
                block,
                ..
            } => {
                let mut nodes = Vec::new();
                Self::build_attributes(&mut nodes, interner, attributes);
                if pub_span.is_some() {
                    nodes.push(Node::StaticText("pub "));
                }
                Self::build_function_signature(&mut nodes, interner, signature);
                nodes.push(Node::StaticText(" "));
                nodes.push(Self::build_expression(interner, source, block));
                Node::Group(Box::new(Node::Concat(nodes.into())))
            }
            ImplItem::Const {
                name, ty, value, ..
            } => {
                let mut nodes = vec![
                    Node::StaticText("const "),
                    Self::symbol(interner, name.inner),
                ];

                if let Some(ty) = ty {
                    nodes.push(Node::StaticText(": "));
                    nodes.push(Self::build_type_expression(interner, &ty.inner));
                }

                nodes.push(Node::StaticText(" = "));
                nodes.push(Self::build_expression(interner, source, value));
                nodes.push(Node::StaticText(";"));
                Node::Concat(nodes.into())
            }
            ImplItem::AssociatedType { name, ty, .. } => Node::Concat(
                vec![
                    Node::StaticText("type "),
                    Self::symbol(interner, name.inner),
                    Node::StaticText(" = "),
                    Self::build_type_expression(interner, &ty.inner),
                    Node::StaticText(";"),
                ]
                .into(),
            ),
        }
    }

    fn build_trait_definition(
        interner: &StringInterner,
        source: &str,
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
        nodes.push(Self::symbol(interner, name.inner));

        if !supertraits.is_empty() {
            nodes.push(Node::StaticText(": "));
            for (index, supertrait) in supertraits.iter().enumerate() {
                if index > 0 {
                    nodes.push(Node::StaticText(" + "));
                }
                nodes.push(Self::build_type_expression(interner, &supertrait.inner));
            }
        }

        nodes.push(Node::StaticText(" {"));

        if !items.is_empty() {
            nodes.push(Node::Indent(Box::new(Node::Concat(
                vec![
                    Node::HardLine,
                    Self::build_trait_item_list(interner, source, items),
                ]
                .into(),
            ))));
            nodes.push(Node::HardLine);
        }

        nodes.push(Node::StaticText("}"));
        Node::Group(Box::new(Node::Concat(nodes.into())))
    }

    fn build_trait_item_list(
        interner: &StringInterner,
        source: &str,
        items: &[Separated<Spanned<TraitItem>>],
    ) -> Node {
        let mut nodes = Vec::new();
        for (index, item) in items.iter().enumerate() {
            if index > 0 {
                nodes.push(Node::BlankLine);
                nodes.push(Node::HardLine);
            }
            nodes.push(Self::build_trait_item(interner, source, &item.inner.inner));
        }
        Node::Concat(nodes.into())
    }

    fn build_trait_item(interner: &StringInterner, source: &str, item: &TraitItem) -> Node {
        match item {
            TraitItem::Function {
                attributes,
                signature,
                body,
                ..
            } => {
                let mut nodes = Vec::new();
                Self::build_attributes(&mut nodes, interner, attributes);
                Self::build_function_signature(&mut nodes, interner, signature);
                match body {
                    Some(body) => {
                        nodes.push(Node::StaticText(" "));
                        nodes.push(Self::build_expression(interner, source, body));
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
                    Self::symbol(interner, name.inner),
                    Node::StaticText(": "),
                    Self::build_type_expression(interner, &ty.inner),
                    Node::StaticText(";"),
                ]
                .into(),
            ),
            TraitItem::AssociatedType { name, bounds, .. } => {
                let mut nodes = vec![
                    Node::StaticText("type "),
                    Self::symbol(interner, name.inner),
                ];

                if !bounds.is_empty() {
                    nodes.push(Node::StaticText(": "));
                    for (index, bound) in bounds.iter().enumerate() {
                        if index > 0 {
                            nodes.push(Node::StaticText(" + "));
                        }
                        nodes.push(Self::build_type_expression(interner, &bound.inner));
                    }
                }

                nodes.push(Node::StaticText(";"));
                Node::Concat(nodes.into())
            }
        }
    }

    fn build_const_definition(
        interner: &StringInterner,
        source: &str,
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
        nodes.push(Self::symbol(interner, name.inner));

        if let Some(ty) = ty {
            nodes.push(Node::StaticText(": "));
            nodes.push(Self::build_type_expression(interner, &ty.inner));
        }

        nodes.push(Node::StaticText(" = "));
        nodes.push(Self::build_expression(interner, source, value));
        nodes.push(Node::StaticText(";"));
        Node::Concat(nodes.into())
    }

    fn build_enum_definition(
        interner: &StringInterner,
        source: &str,
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
        nodes.push(Self::symbol(interner, name.inner));

        if let Some(repr) = repr {
            nodes.push(Node::StaticText(": "));
            nodes.push(Self::build_type_expression(interner, &repr.inner));
        }

        nodes.push(Node::StaticText(" {"));

        if !variants.is_empty() {
            let variant_items = variants
                .iter()
                .enumerate()
                .flat_map(|(index, variant)| {
                    let mut variant_nodes =
                        vec![Self::symbol(interner, variant.inner.inner.name.inner)];

                    if let Some(value) = &variant.inner.inner.value {
                        variant_nodes.push(Node::StaticText(" = "));
                        variant_nodes.push(Self::build_expression(interner, source, value));
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
        interner: &StringInterner,
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
        items.push(Self::symbol(interner, name.inner));
        Self::build_type_params(&mut items, interner, type_params);
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
                    nodes.push(Self::symbol(interner, field.inner.inner.name.inner));
                    nodes.push(Node::StaticText(": "));
                    nodes.push(Self::build_type_expression(
                        interner,
                        &field.inner.inner.ty.inner,
                    ));
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

    fn build_export_definition(
        interner: &StringInterner,
        entries: &[Separated<Spanned<ExportEntry>>],
    ) -> Node {
        let mut items = Vec::new();
        items.push(Node::StaticText("export {"));

        if !entries.is_empty() {
            let entry_items = entries
                .iter()
                .enumerate()
                .flat_map(|(index, entry)| {
                    let mut entry_nodes = Vec::new();

                    entry_nodes.push(Self::symbol(interner, entry.inner.inner.name.inner));

                    if let Some(alias) = &entry.inner.inner.alias {
                        entry_nodes.push(Node::StaticText(" as "));
                        entry_nodes.push(Self::symbol(interner, alias.inner));
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

    fn build_attributes(out: &mut Vec<Node>, interner: &StringInterner, attributes: &[Attribute]) {
        for attr in attributes {
            out.push(Node::StaticText("#["));
            out.push(Self::symbol(interner, attr.name.inner));
            out.push(Node::StaticText("]"));
            out.push(Node::HardLine);
        }
    }

    fn build_type_params(out: &mut Vec<Node>, interner: &StringInterner, type_params: &[TypeParam]) {
        if type_params.is_empty() {
            return;
        }
        out.push(Node::StaticText("<"));
        for (index, param) in type_params.iter().enumerate() {
            out.push(Self::symbol(interner, param.name.inner));
            if !param.bounds.is_empty() {
                out.push(Node::StaticText(": "));
                for (bi, bound) in param.bounds.iter().enumerate() {
                    out.push(Self::build_type_expression(interner, &bound.inner));
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

    fn build_function_signature(
        out: &mut Vec<Node>,
        interner: &StringInterner,
        signature: &FunctionSignature,
    ) {
        out.push(Node::StaticText("fn "));
        out.push(Self::symbol(interner, signature.name.inner));

        Self::build_type_params(out, interner, &signature.type_params);

        out.push(Node::StaticText("("));

        if signature.params.len() > 0 {
            out.push(Node::Indent(Box::new({
                let mut params = Vec::new();
                params.push(Node::Line);

                for (index, param) in signature.params.iter().enumerate() {
                    if param.inner.inner.mut_span.is_some() {
                        params.push(Node::StaticText("mut "));
                    }
                    params.push(Self::symbol(interner, param.inner.inner.name.inner));
                    if let Some(ty) = &param.inner.inner.ty {
                        params.push(Node::StaticText(": "));
                        params.push(Self::build_type_expression(interner, &ty.inner));
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
            out.push(Self::build_type_expression(interner, &result.inner));
        }
    }

    fn build_global_definition(
        interner: &StringInterner,
        source: &str,
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
        items.push(Self::symbol(interner, name.inner));

        if let Some(annotation) = type_annotation {
            items.push(Node::StaticText(": "));
            items.push(Self::build_type_expression(interner, &annotation.inner));
        }

        items.push(Node::StaticText(" = "));
        items.push(Self::build_expression(interner, source, value));
        items.push(Node::StaticText(";"));

        Node::Concat(items.into())
    }

    fn build_expression(
        interner: &StringInterner,
        source: &str,
        expression: &Spanned<Expression>,
    ) -> Node {
        match &expression.inner {
            Expression::Path(path) => Self::build_path(interner, path),
            Expression::Binary {
                left,
                operator,
                right,
            } => Node::Concat(
                vec![
                    Self::build_expression(interner, source, left),
                    Node::SoftLine,
                    Node::StaticText(operator.inner.as_str()),
                    Node::StaticText(" "),
                    Self::build_expression(interner, source, right),
                ]
                .into(),
            ),
            Expression::Block { statements } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("{"));

                match statements.as_ref() {
                    statements if statements.len() == 0 => {}
                    statements => {
                        items.push(Node::Indent(Box::new({
                            let mut items = Vec::new();
                            items.push(Node::HardLine);

                            for (index, statement) in statements.iter().enumerate() {
                                // Preserve blank lines between statements
                                if index > 0 {
                                    let prev_end = statements[index - 1].inner.span.end as usize;
                                    let curr_start = statement.inner.span.start as usize;
                                    let blank_lines =
                                        Self::count_blank_lines(source, prev_end, curr_start);
                                    for _ in 0..blank_lines {
                                        items.push(Node::HardLine);
                                    }
                                }
                                items.push(Self::build_statement(
                                    interner,
                                    source,
                                    &statement.inner.inner,
                                ));
                                if index + 1 == statements.len() {
                                    // Last statement: preserve original separator
                                    match statement.separator {
                                        Some(_) => {
                                            items.push(Node::StaticText(";"));
                                        }
                                        None => {}
                                    }
                                } else {
                                    // Non-last statement: non-block-like always gets `;`;
                                    // block-like preserves an explicit `;` but doesn't add one.
                                    let needs_semi = if statement.inner.inner.is_block_like() {
                                        statement.separator.is_some()
                                    } else {
                                        true
                                    };
                                    if needs_semi {
                                        items.push(Node::StaticText(";"));
                                    }
                                    items.push(Node::HardLine);
                                }
                            }

                            Node::Concat(items.into())
                        })));
                        items.push(Node::HardLine);
                    }
                }

                items.push(Node::StaticText("}"));
                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Expression::Unreachable => Node::StaticText("unreachable"),
            Expression::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("if "));
                items.push(Self::build_expression(interner, source, condition));
                items.push(Node::StaticText(" "));
                items.push(Self::build_expression(interner, source, then_block));
                if let Some(else_block) = else_block {
                    items.push(Node::StaticText(" else "));
                    items.push(Self::build_expression(interner, source, else_block));
                }

                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Expression::Loop { block } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("loop "));
                items.push(Self::build_expression(interner, source, block));

                Node::Group(Box::new(Node::Concat(items.into())))
            }
            Expression::Break { label, value } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("break"));
                if let Some(label) = label {
                    items.push(Node::StaticText(" :"));
                    items.push(Self::symbol(interner, label.inner));
                }
                if let Some(value) = value {
                    items.push(Node::StaticText(" "));
                    items.push(Self::build_expression(interner, source, value));
                }

                Node::Concat(items.into())
            }
            Expression::Return { value } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("return"));
                if let Some(value) = value {
                    items.push(Node::StaticText(" "));
                    items.push(Self::build_expression(interner, source, value));
                }

                Node::Concat(items.into())
            }
            Expression::Cast { value, ty } => {
                let mut items = Vec::new();
                items.push(Self::build_expression(interner, source, value));
                items.push(Node::StaticText(" as "));
                items.push(Self::build_type_expression(interner, &ty.inner));

                Node::Concat(items.into())
            }
            Expression::Continue { label } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("continue"));
                if let Some(label) = label {
                    items.push(Node::StaticText(" :"));
                    items.push(Self::symbol(interner, label.inner));
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
                items.push(Self::build_expression(interner, source, value));
                items.push(Node::StaticText(")"));
                Node::Concat(items.into())
            }
            Expression::Call { callee, arguments } => {
                let mut items = Vec::new();
                items.push(Self::build_expression(interner, source, callee));
                items.push(Node::StaticText("("));

                for (index, arg) in arguments.iter().enumerate() {
                    items.push(Self::build_expression(interner, source, &arg.inner));
                    if index + 1 < arguments.len() {
                        items.push(Node::StaticText(", "));
                    }
                }

                items.push(Node::StaticText(")"));
                Node::Concat(items.into())
            }
            Expression::MethodCall(mc) => {
                let (object, method, type_args, arguments) =
                    (&mc.object, &mc.method, &mc.type_args, &mc.arguments);
                let mut items = Vec::new();
                items.push(Self::build_expression(interner, source, object));
                items.push(Node::StaticText("."));
                items.push(Self::symbol(interner, method.inner));
                if !type_args.is_empty() {
                    items.push(Node::StaticText("::<"));
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            items.push(Node::StaticText(", "));
                        }
                        items.push(Self::build_type_expression(interner, &arg.inner));
                    }
                    items.push(Node::StaticText(">"));
                }
                items.push(Node::StaticText("("));
                for (index, arg) in arguments.iter().enumerate() {
                    items.push(Self::build_expression(interner, source, &arg.inner));
                    if index + 1 < arguments.len() {
                        items.push(Node::StaticText(", "));
                    }
                }
                items.push(Node::StaticText(")"));
                Node::Concat(items.into())
            }
            Expression::Label { label, block } => {
                let mut items = Vec::new();
                items.push(Self::symbol(interner, label.inner));
                items.push(Node::StaticText(": "));
                items.push(Self::build_expression(interner, source, block));
                Node::Concat(items.into())
            }
            Expression::Error => unreachable!(),
            Expression::Unary { operator, operand } => {
                let mut items = Vec::new();
                items.push(Node::StaticText(operator.inner.as_str()));
                items.push(Self::build_expression(interner, source, operand));

                Node::Concat(items.into())
            }
            Expression::String { .. } | Expression::Char { .. } => {
                Node::SourceText(expression.span)
            }
            Expression::ObjectAccess { object, member } => {
                let mut items = Vec::new();
                items.push(Self::build_expression(interner, source, object));
                items.push(Node::StaticText("."));
                items.push(Self::symbol(interner, member.inner));
                Node::Concat(items.into())
            }
            Expression::Deref { pointer } => Node::Concat(
                vec![
                    Self::build_expression(interner, source, pointer),
                    Node::StaticText(".*"),
                ]
                .into(),
            ),
            Expression::StructInit { path, fields } => {
                let mut items = Vec::new();
                items.push(Self::build_path(interner, path));
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
                        field_items.push(Self::symbol(interner, field.inner.inner.name.inner));
                        if let Some(value) = &field.inner.inner.value {
                            field_items.push(Node::StaticText(": "));
                            field_items.push(Self::build_expression(interner, source, value));
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
                items.push(Self::build_expression(interner, source, callee));
                items.push(Node::StaticText("::<"));
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        items.push(Node::StaticText(", "));
                    }
                    items.push(Self::build_type_expression(interner, &arg.inner));
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
                items.push(Self::symbol(interner, name.inner));
                if !type_args.is_empty() {
                    items.push(Node::StaticText("::<"));
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            items.push(Node::StaticText(", "));
                        }
                        items.push(Self::build_type_expression(interner, &arg.inner));
                    }
                    items.push(Node::StaticText(">"));
                }
                items.push(Node::StaticText("("));
                for (index, arg) in arguments.iter().enumerate() {
                    items.push(Self::build_expression(interner, source, &arg.inner));
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
                    items.push(Self::build_expression(interner, source, element));
                }
                items.push(Node::StaticText("]"));
                Node::Concat(items.into())
            }
            Expression::ArrayRepeat { value, count } => Node::Concat(Box::new([
                Node::StaticText("["),
                Self::build_expression(interner, source, value),
                Node::StaticText("; "),
                Self::build_expression(interner, source, count),
                Node::StaticText("]"),
            ])),
            Expression::Index { object, index } => Node::Concat(Box::new([
                Self::build_expression(interner, source, object),
                Node::StaticText("["),
                Self::build_expression(interner, source, index),
                Node::StaticText("]"),
            ])),
            Expression::SliceRange { object, start, end } => {
                let mut parts = vec![
                    Self::build_expression(interner, source, object),
                    Node::StaticText("["),
                ];
                if let Some(s) = start {
                    parts.push(Self::build_expression(interner, source, s));
                }
                parts.push(Node::StaticText(".."));
                if let Some(e) = end {
                    parts.push(Self::build_expression(interner, source, e));
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
                            nodes.push(Self::build_expression(interner, source, element));

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
                out.push(Self::symbol(interner, name.inner));
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
                out.push(Self::symbol(interner, name.inner));
                out.push(Node::StaticText(" {"));
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push(Node::StaticText(", "));
                    } else {
                        out.push(Node::StaticText(" "));
                    }
                    out.push(Self::symbol(interner, field.inner.inner.name.inner));
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

    fn build_statement(interner: &StringInterner, source: &str, statement: &Statement) -> Node {
        match statement {
            Statement::Expression(expression) => {
                Self::build_expression(interner, source, expression)
            }
            Statement::LocalDefinition {
                pattern,
                ty: type_annotation,
                value,
            } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("local "));
                Self::build_pattern(&mut items, interner, &pattern.inner);
                if let Some(annotation) = type_annotation {
                    items.push(Node::StaticText(": "));
                    items.push(Self::build_type_expression(interner, &annotation.inner));
                }
                items.push(Node::StaticText(" = "));

                // Only indent if it's not a block-like expression that manages its own
                // indentation
                let value_node = Self::build_expression(interner, source, value);
                if !value.inner.is_block_like() {
                    items.push(Node::Indent(Box::new(value_node)));
                } else {
                    items.push(value_node);
                }

                Node::Group(Box::new(Node::Concat(items.into())))
            }
        }
    }

    fn build_path(interner: &StringInterner, path: &Path) -> Node {
        let mut items: Vec<Node> = Vec::new();
        for (i, seg) in path.segments.iter().enumerate() {
            if i > 0 {
                items.push(Node::StaticText("::"));
            }
            items.push(Self::symbol(interner, seg.ident.inner));
            if !seg.type_args.is_empty() {
                items.push(Node::StaticText("::<"));
                for (j, arg) in seg.type_args.iter().enumerate() {
                    if j > 0 {
                        items.push(Node::StaticText(", "));
                    }
                    items.push(Self::build_type_expression(interner, &arg.inner));
                }
                items.push(Node::StaticText(">"));
            }
        }
        Node::Concat(items.into())
    }

    fn build_type_expression(interner: &StringInterner, type_expression: &TypeExpression) -> Node {
        match type_expression {
            TypeExpression::Infer => Node::StaticText("_"),
            TypeExpression::Path(path) => Self::build_path(interner, path),
            TypeExpression::Function { params, result } => {
                let mut items = Vec::new();
                items.push(Node::StaticText("fn("));

                if params.len() > 0 {
                    items.push(Node::Indent(Box::new({
                        let mut param_items = Vec::new();
                        param_items.push(Node::Line);

                        for (index, param) in params.iter().enumerate() {
                            if let Some(name) = &param.inner.inner.name {
                                param_items.push(Self::symbol(interner, name.inner));
                                param_items.push(Node::StaticText(": "));
                            }
                            param_items.push(Self::build_type_expression(
                                interner,
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
                    items.push(Self::build_type_expression(interner, &result.inner));
                }

                Node::Group(Box::new(Node::Concat(items.into())))
            }
            TypeExpression::Pointer { mutability, inner } => {
                let mut items = vec![Node::StaticText("*")];
                if mutability.is_some() {
                    items.push(Node::StaticText("mut "));
                }
                items.push(Self::build_type_expression(interner, &inner.inner));
                Node::Concat(items.into())
            }
            TypeExpression::Slice { mutability, inner } => {
                let mut items = vec![Node::StaticText("[]")];
                if mutability.is_some() {
                    items.push(Node::StaticText("mut "));
                }
                items.push(Self::build_type_expression(interner, &inner.inner));
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
                items.push(Self::build_type_expression(interner, &inner.inner));
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
                                vec![Self::build_type_expression(interner, &element.inner)];

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
                    Self::build_path(interner, memory),
                    Node::StaticText("::"),
                    Self::build_type_expression(interner, &inner.inner),
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
                            inner_parts.push(Self::build_type_expression(interner, &ty.inner));
                        }
                        GenericArg::Binding { name: key, ty } => {
                            inner_parts.push(Node::Concat(
                                vec![
                                    Self::symbol(interner, key.inner),
                                    Node::StaticText(" = "),
                                    Self::build_type_expression(interner, &ty.inner),
                                ]
                                .into(),
                            ));
                        }
                    }
                }
                Node::Concat(
                    vec![
                        Self::symbol(interner, name.inner),
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
    let mut root_items = Vec::new();
    root_items.push(Builder::build(ast, interner, source));
    root_items.push(Node::HardLine);

    let root = Node::Concat(root_items.into());
    let renderer = Renderer::new(config, interner, source);
    renderer.render(&root)
}
