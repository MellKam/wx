use crate::ast::*;

#[cfg_attr(test, derive(Debug, serde::Serialize))]
enum Node {
    /// An atomic sequence of tokens that cannot be broken up
    Text(String),
    /// A possible line break, otherwise rendered as a space
    BreakOrSpace,
    /// A possible line break, otherwise rendered as nothing
    BreakOrNone,
    /// Always break the line
    Break,
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
    fn build(ast: &Ast, interner: &StringInterner) -> Node {
        let mut items = Vec::new();
        for item in ast.items.iter() {
            items.push(match &item.inner {
                Item::FunctionDefinition { signature, block } => {
                    let mut items = Vec::new();
                    items.push(Node::Text(format!(
                        "fn {}(",
                        interner.resolve(signature.name.inner).unwrap()
                    )));

                    if signature.params.inner.len() > 0 {
                        items.push(Node::Indent(Box::new({
                            let mut items = Vec::new();
                            // items.push(Node::BreakOrNone);

                            for (index, param) in signature.params.inner.iter().enumerate() {
                                items.push(Node::Text(format!(
                                    "{}: ",
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
                                    items.push(Node::BreakOrSpace);
                                } else {
                                    items.push(Node::IfBreak(",".to_string()));
                                }
                            }

                            Node::Concat(items)
                        })));
                        items.push(Node::BreakOrNone);
                    }
                    items.push(Node::Text(") -> ".to_string()));
                    items.push(Self::build_type_expression(
                        interner,
                        &signature.result.inner.inner,
                    ));
                    items.push(Node::Text(" ".to_string()));
                    items.push(Self::build_expression(interner, &block.inner));

                    Node::Group(Box::new(Node::Concat(items)))
                }
                _ => todo!(),
            });
        }

        Node::Concat(items)
    }

    fn build_expression(interner: &StringInterner, expression: &Expression) -> Node {
        match expression {
            Expression::Identifier { symbol } => {
                Node::Text(interner.resolve(*symbol).unwrap().to_string())
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => Node::Group(Box::new(Node::Concat(vec![
                Self::build_expression(interner, &left.inner),
                Node::BreakOrSpace,
                Node::Text(format!("{} ", operator.inner.as_str())),
                Self::build_expression(interner, &right.inner),
            ]))),
            Expression::Block { statements } => {
                let mut items = Vec::new();
                items.push(Node::Text("{".to_string()));

                match statements.inner.as_ref() {
                    [
                        Separated {
                            inner:
                                Spanned {
                                    inner: Statement::Expression(expression),
                                    ..
                                },
                            separator: None,
                        },
                    ] => {
                        items.push(Node::BreakOrSpace);
                        items.push(Self::build_expression(interner, &expression.inner));
                    }
                    statements if statements.len() == 0 => {}
                    statements => {
                        items.push(Node::Indent(Box::new({
                            let mut items = Vec::new();
                            items.push(Node::Break);

                            for (index, statement) in statements.iter().enumerate() {
                                items.push(Self::build_statement(interner, &statement.inner.inner));
                                if index + 1 == statements.len() {
                                    match statement.separator {
                                        Some(_) => {
                                            items.push(Node::Text(";".to_string()));
                                        }
                                        None => {}
                                    }
                                } else {
                                    items.push(Node::Text(";".to_string()));
                                    items.push(Node::Break);
                                }
                            }

                            Node::Concat(items)
                        })));
                        items.push(Node::Break);
                    }
                }

                items.push(Node::BreakOrSpace);
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
                items.push(Self::build_expression(interner, &condition.inner));
                items.push(Node::Text(" ".to_string()));
                items.push(Self::build_expression(interner, &then_block.inner));
                if let Some(else_block) = else_block {
                    items.push(Node::Text(" else ".to_string()));
                    items.push(Self::build_expression(interner, &else_block.inner));
                }

                Node::Group(Box::new(Node::Concat(items)))
            }
            Expression::Loop { block } => {
                let mut items = Vec::new();
                items.push(Node::Text("loop ".to_string()));
                items.push(Self::build_expression(interner, &block.inner));

                Node::Concat(items)
            }
            Expression::Break { label, value } => {
                let mut items = Vec::new();
                items.push(Node::Text("break".to_string()));
                if let Some(label) = label {
                    items.push(Node::Text(format!(
                        " {}",
                        interner.resolve(label.inner).unwrap()
                    )));
                }
                if let Some(value) = value {
                    items.push(Node::Text(" ".to_string()));
                    items.push(Self::build_expression(interner, &value.inner));
                }

                Node::Concat(items)
            }
            Expression::Return { value } => {
                let mut items = Vec::new();
                items.push(Node::Text("return".to_string()));
                if let Some(value) = value {
                    items.push(Node::Text(" ".to_string()));
                    items.push(Self::build_expression(interner, &value.inner));
                }

                Node::Concat(items)
            }
            Expression::Cast { value, ty } => {
                let mut items = Vec::new();
                items.push(Self::build_expression(interner, &value.inner));
                items.push(Node::Text(" as ".to_string()));
                items.push(Self::build_type_expression(interner, &ty.inner));

                Node::Concat(items)
            }
            Expression::Continue { label } => {
                let mut items = Vec::new();
                items.push(Node::Text("continue".to_string()));
                if let Some(label) = label {
                    items.push(Node::Text(format!(
                        " {}",
                        interner.resolve(label.inner).unwrap()
                    )));
                }

                Node::Concat(items)
            }
            Expression::Int { value } => Node::Text(value.to_string()),
            Expression::Unary { operator, operand } => {
                let mut items = Vec::new();
                items.push(Node::Text(operator.inner.as_str().to_string()));
                items.push(Self::build_expression(interner, &operand.inner));

                Node::Concat(items)
            }
            _ => todo!(),
        }
    }

    fn build_statement(interner: &StringInterner, statement: &Statement) -> Node {
        match statement {
            Statement::Expression(expression) => {
                Self::build_expression(interner, &expression.inner)
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
                items.push(Node::BreakOrSpace);
                items.push(Node::Text("= ".to_string()));
                items.push(Self::build_expression(interner, &value.inner));

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
                Node::Group(Box::new(Node::Concat(vec![
                    Node::Text("fn(".to_string()),
                    Node::Indent(Box::new({
                        let mut items = Vec::new();

                        items.push(Node::BreakOrSpace);
                        for param in params.inner.iter() {
                            items.push(Self::build_type_expression(interner, &param.inner.inner));
                            items.push(Node::Text(",".to_string()));
                            items.push(Node::BreakOrSpace);
                        }

                        Node::Concat(items)
                    })),
                    Node::Text(") -> ".to_string()),
                    Self::build_type_expression(interner, &result.inner.inner),
                ])))
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
            Node::BreakOrSpace => match mode {
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
            Node::BreakOrNone => match mode {
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
            Node::Break => {
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
                Node::BreakOrSpace => width += 1,
                Node::BreakOrNone => {}
                Node::IfBreak(_) => {}
                Node::Break => return width,
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

pub fn format(ast: &Ast, interner: &StringInterner, config: RendererConfig) -> String {
    let root = Builder::build(ast, interner);
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
        ast: Ast,
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
            fn pow(base: i32, mut exp: i32) -> i32 {
                if exp < 0 {
                    return 0; 
                };
                
                local mut result: i32 = 1;
                loop {
                    if exp == 0 {
                    break result; 
                    };
                    
                    result *= base;
                    exp -= 1;
                }
            }"});
        let output = format(
            &case.ast,
            &case.interner,
            RendererConfig {
                max_line_width: 80,
                indent_width: 4,
                trailing_comma: true,
            },
        );
        println!("{}", output);
    }
}
