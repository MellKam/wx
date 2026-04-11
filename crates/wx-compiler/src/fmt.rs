use string_interner::symbol::SymbolU32;

use crate::ast::*;

#[cfg_attr(test, derive(Debug, serde::Serialize))]
enum Node {
    /// An atomic sequence of tokens that cannot be broken up
    Text(String),
    /// A possible line break, otherwise rendered as a space
    SoftLine,
    /// A possible line break, otherwise nothing
    Line,
    /// Always break the line
    HardLine,
    /// A sequence of nodes concatenated together
    Concat(Vec<Node>),
    /// All lines under this node must either break or not break together
    Group(Box<Node>),
    /// Increases the indentation level for all lines within this node
    Indent(Box<Node>),
    /// Text that only appears in break mode
    IfBreak(String),
}

struct Builder;

impl Builder {
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
        let mut items = Vec::new();
        for (index, item) in ast.items.iter().enumerate() {
            // Add spacing between top-level items
            if index > 0 {
                items.push(Node::HardLine);
                items.push(Node::HardLine);
            }

            items.push(match &item.inner.inner {
                Item::Function { signature, block } => {
                    Self::build_function_definition(interner, source, signature, block)
                }
                Item::Global {
                    mut_span,
                    name,
                    type_annotation,
                    value,
                } => Self::build_global_definition(
                    interner,
                    source,
                    *mut_span,
                    name,
                    type_annotation,
                    value,
                ),
                Item::Export { entries } => {
                    let mut items = Vec::new();
                    items.push(Node::Text("export {".to_string()));

                    if !entries.inner.is_empty() {
                        let entry_items = entries
                            .inner
                            .iter()
                            .enumerate()
                            .flat_map(|(index, entry)| {
                                let mut entry_nodes = Vec::new();

                                let name = interner.resolve(entry.inner.inner.name.inner).unwrap();
                                entry_nodes.push(Node::Text(name.to_string()));

                                if let Some(alias) = &entry.inner.inner.alias {
                                    let alias_str = interner.resolve(alias.inner).unwrap();
                                    entry_nodes.push(Node::Text(format!(" as {}", alias_str)));
                                }

                                if entry.separator.is_some() {
                                    entry_nodes.push(Node::Text(",".to_string()));
                                }

                                let mut result = vec![Node::Concat(entry_nodes)];

                                // Add line break after each entry except the last
                                if index + 1 < entries.inner.len() {
                                    result.push(Node::Line);
                                }

                                result
                            })
                            .collect::<Vec<_>>();

                        items.push(Node::Indent(Box::new(Node::Concat(
                            std::iter::once(Node::Line)
                                .chain(entry_items)
                                .collect::<Vec<_>>(),
                        ))));
                    }

                    items.push(Node::Line);
                    items.push(Node::Text("}".to_string()));
                    Node::Concat(items)
                }
                Item::Import {
                    module,
                    alias,
                    entries,
                } => {
                    let mut items = Vec::new();

                    // "import "module""
                    items.push(Node::Text("import ".to_string()));
                    let module_str = interner.resolve(module.inner).unwrap();
                    items.push(Node::Text(module_str.to_string()));

                    // Optional "as alias"
                    if let Some(alias) = alias {
                        items.push(Node::Text(" as ".to_string()));
                        let alias_str = interner.resolve(alias.inner).unwrap();
                        items.push(Node::Text(alias_str.to_string()));
                    }

                    items.push(Node::Text(" {".to_string()));

                    if !entries.inner.is_empty() {
                        let entry_items = entries
                            .inner
                            .iter()
                            .enumerate()
                            .flat_map(|(index, entry)| {
                                let mut entry_nodes = Vec::new();

                                // Optional external name: "X":
                                if let Some(ext_name) = &entry.inner.inner.external_name {
                                    let ext_str = interner.resolve(ext_name.inner).unwrap();
                                    entry_nodes.push(Node::Text(format!("{}: ", ext_str)));
                                }

                                // Declaration
                                match &entry.inner.inner.declaration {
                                    crate::ast::ImportDeclaration::Function { signature } => {
                                        // Format function signature
                                        entry_nodes.push(Node::Text("fn ".to_string()));
                                        let fn_name =
                                            interner.resolve(signature.name.inner).unwrap();
                                        entry_nodes.push(Node::Text(format!("{}(", fn_name)));

                                        // Parameters
                                        for (i, param) in signature.params.inner.iter().enumerate()
                                        {
                                            let param_name = interner
                                                .resolve(param.inner.inner.name.inner)
                                                .unwrap();
                                            let mut_prefix = if param.inner.inner.mut_span.is_some()
                                            {
                                                "mut "
                                            } else {
                                                ""
                                            };
                                            entry_nodes.push(Node::Text(format!(
                                                "{}{}: ",
                                                mut_prefix, param_name
                                            )));
                                            entry_nodes.push(Self::build_type_expression(
                                                interner,
                                                &param.inner.inner.type_annotation.inner.inner,
                                            ));

                                            if i + 1 < signature.params.inner.len() {
                                                entry_nodes.push(Node::Text(", ".to_string()));
                                            }
                                        }

                                        entry_nodes.push(Node::Text(")".to_string()));
                                        match &signature.result {
                                            Some(result) => {
                                                entry_nodes.push(Node::Text(" -> ".to_string()));
                                                entry_nodes.push(Self::build_type_expression(
                                                    interner,
                                                    &result.inner.inner,
                                                ));
                                            }
                                            None => {}
                                        }
                                    }
                                    crate::ast::ImportDeclaration::Global {
                                        mut_span,
                                        name,
                                        type_annotation,
                                    } => {
                                        // Format global declaration
                                        let mut_prefix =
                                            if mut_span.is_some() { "mut " } else { "" };
                                        let global_name = interner.resolve(name.inner).unwrap();
                                        entry_nodes.push(Node::Text(format!(
                                            "global {}{}: ",
                                            mut_prefix, global_name
                                        )));
                                        entry_nodes.push(Self::build_type_expression(
                                            interner,
                                            &type_annotation.inner.inner,
                                        ));
                                    }
                                }

                                if entry.separator.is_some() {
                                    entry_nodes.push(Node::Text(";".to_string()));
                                }

                                let mut result = vec![Node::Concat(entry_nodes)];

                                // Add line break after each entry except the last
                                if index + 1 < entries.inner.len() {
                                    result.push(Node::Line);
                                }

                                result
                            })
                            .collect::<Vec<_>>();

                        items.push(Node::Indent(Box::new(Node::Concat(
                            std::iter::once(Node::Line)
                                .chain(entry_items)
                                .collect::<Vec<_>>(),
                        ))));
                    }

                    items.push(Node::Line);
                    items.push(Node::Text("}".to_string()));
                    Node::Concat(items)
                }
            });
        }

        Node::Concat(items)
    }

    fn build_function_definition(
        interner: &StringInterner,
        source: &str,
        signature: &FunctionSignature,
        block: &Spanned<Expression>,
    ) -> Node {
        let mut items = Vec::new();
        items.push(Node::Text(format!(
            "fn {}(",
            interner.resolve(signature.name.inner).unwrap()
        )));

        if signature.params.inner.len() > 0 {
            items.push(Node::Indent(Box::new({
                let mut items = Vec::new();
                items.push(Node::Line);

                for (index, param) in signature.params.inner.iter().enumerate() {
                    items.push(Node::Text(format!(
                        "{}{}: ",
                        match param.inner.inner.mut_span {
                            Some(_) => "mut ",
                            None => "",
                        },
                        interner
                            .resolve(param.inner.inner.name.inner)
                            .unwrap()
                            .to_string()
                    )));
                    items.push(Self::build_type_expression(
                        interner,
                        &param.inner.inner.type_annotation.inner.inner,
                    ));
                    if index + 1 < signature.params.inner.len() {
                        items.push(Node::Text(",".to_string()));
                        items.push(Node::SoftLine);
                    } else {
                        items.push(Node::IfBreak(",".to_string()));
                    }
                }

                Node::Concat(items)
            })));
            items.push(Node::Line);
        }

        items.push(Node::Text(") ".to_string()));
        match &signature.result {
            Some(result) => {
                items.push(Node::Text("-> ".to_string()));
                items.push(Self::build_type_expression(interner, &result.inner.inner));
                items.push(Node::Text(" ".to_string()));
            }
            None => {}
        }
        items.push(Self::build_expression(interner, source, block));

        Node::Group(Box::new(Node::Concat(items)))
    }

    fn build_global_definition(
        interner: &StringInterner,
        source: &str,
        mut_span: Option<TextSpan>,
        name: &Spanned<SymbolU32>,
        type_annotation: &Option<Annotated<Box<Spanned<TypeExpression>>>>,
        value: &Box<Spanned<Expression>>,
    ) -> Node {
        let mut items = Vec::new();
        items.push(Node::Text(format!(
            "global {}{}",
            if mut_span.is_some() { "mut " } else { "" },
            interner.resolve(name.inner).unwrap()
        )));

        if let Some(annotation) = type_annotation {
            items.push(Node::Text(": ".to_string()));
            items.push(Self::build_type_expression(
                interner,
                &annotation.inner.inner,
            ));
        }

        items.push(Node::Text(" = ".to_string()));
        items.push(Self::build_expression(interner, source, value));
        items.push(Node::Text(";".to_string()));

        Node::Concat(items)
    }

    fn build_expression(
        interner: &StringInterner,
        source: &str,
        expression: &Spanned<Expression>,
    ) -> Node {
        match &expression.inner {
            Expression::Identifier { symbol } => {
                Node::Text(interner.resolve(*symbol).unwrap().to_string())
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => Node::Concat(vec![
                Self::build_expression(interner, source, left),
                Node::SoftLine,
                Node::Text(format!("{} ", operator.inner.as_str())),
                Self::build_expression(interner, source, right),
            ]),
            Expression::Block { statements } => {
                let mut items = Vec::new();
                items.push(Node::Text("{".to_string()));

                match statements.inner.as_ref() {
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
                                            items.push(Node::Text(";".to_string()));
                                        }
                                        None => {}
                                    }
                                } else {
                                    // Non-last statement: only add semicolon if not block-like
                                    if !statement.inner.inner.is_block_like() {
                                        items.push(Node::Text(";".to_string()));
                                    }
                                    items.push(Node::HardLine);
                                }
                            }

                            Node::Concat(items)
                        })));
                        items.push(Node::HardLine);
                    }
                }

                items.push(Node::Text("}".to_string()));
                Node::Group(Box::new(Node::Concat(items)))
            }
            Expression::Unreachable => Node::Text("unreachable".to_string()),
            Expression::IfElse {
                condition,
                then_block,
                else_block,
            } => {
                let mut items = Vec::new();
                items.push(Node::Text("if ".to_string()));
                items.push(Self::build_expression(interner, source, condition));
                items.push(Node::Text(" ".to_string()));
                items.push(Self::build_expression(interner, source, then_block));
                if let Some(else_block) = else_block {
                    items.push(Node::Text(" else ".to_string()));
                    items.push(Self::build_expression(interner, source, else_block));
                }

                Node::Group(Box::new(Node::Concat(items)))
            }
            Expression::Loop { block } => {
                let mut items = Vec::new();
                items.push(Node::Text("loop ".to_string()));
                items.push(Self::build_expression(interner, source, block));

                Node::Group(Box::new(Node::Concat(items)))
            }
            Expression::Break { label, value } => {
                let mut items = Vec::new();
                items.push(Node::Text("break".to_string()));
                if let Some(label) = label {
                    items.push(Node::Text(format!(
                        " :{}",
                        interner.resolve(label.inner).unwrap()
                    )));
                }
                if let Some(value) = value {
                    items.push(Node::Text(" ".to_string()));
                    items.push(Self::build_expression(interner, source, value));
                }

                Node::Concat(items)
            }
            Expression::Return { value } => {
                let mut items = Vec::new();
                items.push(Node::Text("return".to_string()));
                if let Some(value) = value {
                    items.push(Node::Text(" ".to_string()));
                    items.push(Self::build_expression(interner, source, value));
                }

                Node::Concat(items)
            }
            Expression::Cast { value, ty } => {
                let mut items = Vec::new();
                items.push(Self::build_expression(interner, source, value));
                items.push(Node::Text(" as ".to_string()));
                items.push(Self::build_type_expression(interner, &ty.inner));

                Node::Concat(items)
            }
            Expression::Continue { label } => {
                let mut items = Vec::new();
                items.push(Node::Text("continue".to_string()));
                if let Some(label) = label {
                    items.push(Node::Text(format!(
                        " :{}",
                        interner.resolve(label.inner).unwrap()
                    )));
                }

                Node::Concat(items)
            }
            Expression::Int { .. } => {
                // Preserve the original literal text from source
                Node::Text(expression.span.extract_str(source).to_string())
            }
            Expression::Float { .. } => {
                // Preserve the original literal text from source
                Node::Text(expression.span.extract_str(source).to_string())
            }
            Expression::Grouping { value } => {
                let mut items = Vec::new();
                items.push(Node::Text("(".to_string()));
                items.push(Self::build_expression(interner, source, &value.inner));
                items.push(Node::Text(")".to_string()));
                Node::Concat(items)
            }
            Expression::Call { callee, arguments } => {
                let mut items = Vec::new();
                items.push(Self::build_expression(interner, source, callee));
                items.push(Node::Text("(".to_string()));

                for (index, arg) in arguments.inner.iter().enumerate() {
                    items.push(Self::build_expression(interner, source, &arg.inner));
                    if index + 1 < arguments.inner.len() {
                        items.push(Node::Text(", ".to_string()));
                    }
                }

                items.push(Node::Text(")".to_string()));
                Node::Concat(items)
            }
            Expression::Label { label, block } => {
                let mut items = Vec::new();
                items.push(Node::Text(format!(
                    "{}: ",
                    interner.resolve(label.inner).unwrap()
                )));
                items.push(Self::build_expression(interner, source, block));
                Node::Concat(items)
            }
            Expression::NamespaceAccess { namespace, member } => {
                let mut items = Vec::new();
                items.push(Self::build_type_expression(interner, &namespace.inner));
                items.push(Node::Text("::".to_string()));
                items.push(Node::Text(
                    interner.resolve(member.inner).unwrap().to_string(),
                ));
                Node::Concat(items)
            }
            Expression::Error => Node::Text("/* error */".to_string()),
            Expression::Unary { operator, operand } => {
                let mut items = Vec::new();
                items.push(Node::Text(operator.inner.as_str().to_string()));
                items.push(Self::build_expression(interner, source, operand));

                Node::Concat(items)
            }
            Expression::String { .. } | Expression::Char { .. } => {
                Node::Text(expression.span.extract_str(source).to_string())
            }
            Expression::ObjectAccess { object, member } => {
                let mut items = Vec::new();
                items.push(Self::build_expression(interner, source, object));
                items.push(Node::Text(".".to_string()));
                items.push(Node::Text(
                    interner.resolve(member.inner).unwrap().to_string(),
                ));
                Node::Concat(items)
            }
        }
    }

    fn build_statement(interner: &StringInterner, source: &str, statement: &Statement) -> Node {
        match statement {
            Statement::Expression(expression) => {
                Self::build_expression(interner, source, expression)
            }
            Statement::LocalDefinition {
                mut_span,
                name,
                type_annotation,
                value,
            } => {
                let mut items = Vec::new();
                items.push(Node::Text(format!(
                    "local {}{}",
                    match mut_span {
                        Some(_) => "mut ",
                        None => "",
                    },
                    interner.resolve(name.inner).unwrap()
                )));
                match type_annotation {
                    Some(annotation) => {
                        items.push(Node::Text(": ".to_string()));
                        items.push(Self::build_type_expression(
                            interner,
                            &annotation.inner.inner,
                        ));
                    }
                    None => {}
                }
                items.push(Node::Text(" = ".to_string()));

                // Only indent if it's not a block-like expression that manages its own
                // indentation
                let value_node = Self::build_expression(interner, source, value);
                if !value.inner.is_block_like() {
                    items.push(Node::Indent(Box::new(value_node)));
                } else {
                    items.push(value_node);
                }

                Node::Group(Box::new(Node::Concat(items)))
            }
        }
    }

    fn build_type_expression(interner: &StringInterner, type_expression: &TypeExpression) -> Node {
        match type_expression {
            TypeExpression::Identifier { symbol } => {
                Node::Text(interner.resolve(*symbol).unwrap().to_string())
            }
            TypeExpression::Function { params, result } => {
                let mut items = Vec::new();
                items.push(Node::Text("fn(".to_string()));

                if params.inner.len() > 0 {
                    items.push(Node::Indent(Box::new({
                        let mut param_items = Vec::new();
                        param_items.push(Node::Line);

                        for (index, param) in params.inner.iter().enumerate() {
                            param_items
                                .push(Self::build_type_expression(interner, &param.inner.inner));
                            if index + 1 < params.inner.len() {
                                param_items.push(Node::Text(",".to_string()));
                                param_items.push(Node::SoftLine);
                            } else {
                                param_items.push(Node::IfBreak(",".to_string()));
                            }
                        }

                        Node::Concat(param_items)
                    })));
                    items.push(Node::Line);
                }

                items.push(Node::Text(") -> ".to_string()));
                items.push(Self::build_type_expression(interner, &result.inner.inner));

                Node::Group(Box::new(Node::Concat(items)))
            }
            TypeExpression::Error => unreachable!(),
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

pub struct Renderer {
    config: RendererConfig,
    buffer: String,
    position: usize,
    indent: usize,
}

#[derive(Clone, Copy)]
enum RenderMode {
    Flat,
    Break,
}

impl Renderer {
    pub fn new(config: RendererConfig) -> Self {
        Self {
            config,
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
            Node::Text(s) => {
                self.buffer.push_str(s);
                self.position += s.len();
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
            Node::IfBreak(s) => match mode {
                RenderMode::Flat => {}
                RenderMode::Break => {
                    self.buffer.push_str(s);
                    self.position += s.len();
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
                Node::Text(s) => width += s.len(),
                Node::SoftLine => width += 1,
                Node::Line => {}
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

    let root = Node::Concat(root_items);
    let renderer = Renderer::new(config);
    renderer.render(&root)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;

    #[allow(unused)]
    struct TestCase {
        interner: StringInterner,
        files: Files,
        ast: AST,
    }

    impl<'case> TestCase {
        fn new(source: &str) -> Self {
            let mut interner = StringInterner::new();
            let mut files = Files::new();
            let file_id = files
                .add("main.wx".to_string(), source.to_string())
                .unwrap();
            let ast = Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);

            TestCase {
                interner,
                files,
                ast,
            }
        }
    }

    #[test]
    fn test_format_simple_function() {
        let case = TestCase::new(indoc! {"
            fn add(a: i32, b: i32) -> i32 { 
                a + b
            }

            export { add, add as \"plus\", minus }
        "});
        let output = format(
            &case.ast,
            &case.interner,
            &case.files.get(case.ast.file_id).unwrap().source,
            RendererConfig {
                max_line_width: 40,
                indent_width: 4,
                trailing_comma: true,
            },
        );
        println!("{}", output);
    }

    #[test]
    fn test_format_import_block() {
        let case = TestCase::new(indoc! {"
            import \"math\" {
                fn sqrt(x: f64) -> f64;
                fn pow(base: f64, exponent: f64) -> f64;
                fn log(x: string);
            }

            fn main() {
                local x = sqrt(2.0);
                local y = pow(x, 2.0);
            }

            export { main }
        "});
        let output = format(
            &case.ast,
            &case.interner,
            &case.files.get(case.ast.file_id).unwrap().source,
            RendererConfig {
                max_line_width: 80,
                indent_width: 4,
                trailing_comma: true,
            },
        );
        println!("{}", output);
    }
}
