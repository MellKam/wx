use crate::wasm;

pub trait Encode {
    fn encode(&self, sink: &mut String);
}

pub trait EncodeWithContext {
    fn encode_with_context(
        &self,
        sink: &mut String,
        module: &wasm::Module,
        func_index: wasm::FunctionIndex,
    );
}

impl Encode for i32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for i64 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for u32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(self.to_string().as_str());
    }
}

impl Encode for wasm::ValueType {
    fn encode(&self, sink: &mut String) {
        match self {
            wasm::ValueType::I32 => sink.push_str("i32"),
            wasm::ValueType::I64 => sink.push_str("i64"),
        }
    }
}

impl EncodeWithContext for wasm::Expression {
    fn encode_with_context(
        &self,
        sink: &mut String,
        module: &wasm::Module,
        func_index: wasm::FunctionIndex,
    ) {
        match self {
            wasm::Expression::Nop => {
                sink.push_str("(nop)");
            }
            wasm::Expression::LocalGet { local: local_index } => {
                sink.push_str("(local.get $");
                let local = module
                    .get_function(func_index)
                    .locals
                    .get(local_index.0 as usize)
                    .unwrap();
                sink.push_str(format!("{}_{})", local.name, local_index.0).as_str());
            }
            wasm::Expression::LocalSet {
                local: local_index,
                value,
            } => {
                sink.push_str("(local.set $");
                let local = module
                    .get_function(func_index)
                    .locals
                    .get(local_index.0 as usize)
                    .unwrap();

                sink.push_str(format!("{}_{}", local.name, local_index.0).as_str());
                sink.push_str(" ");
                module
                    .get_expr(*value)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::Return { value } => match value {
                Some(value) => {
                    sink.push_str("(return ");
                    module
                        .get_expr(*value)
                        .encode_with_context(sink, module, func_index);
                    sink.push_str(")");
                }
                None => {
                    sink.push_str("(return)");
                }
            },
            wasm::Expression::IfElse {
                condition,
                result,
                then_branch,
                else_branch,
            } => {
                sink.push_str("(if ");
                match result {
                    wasm::BlockResult::Empty => {}
                    wasm::BlockResult::SingleValue(ty) => {
                        sink.push_str("(result ");
                        ty.encode(sink);
                        sink.push_str(")");
                    }
                }
                module
                    .get_expr(*condition)
                    .encode_with_context(sink, module, func_index);

                sink.push_str("(then");
                module
                    .get_expr(*then_branch)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");

                match else_branch {
                    Some(else_branch) => {
                        sink.push_str("(else");
                        module
                            .get_expr(*else_branch)
                            .encode_with_context(sink, module, func_index);
                        sink.push_str(")");
                    }
                    None => {}
                }
                sink.push_str(")");
            }
            wasm::Expression::Block {
                expressions,
                result,
            } => {
                sink.push_str("(block ");
                match result {
                    wasm::BlockResult::Empty => {}
                    wasm::BlockResult::SingleValue(ty) => {
                        sink.push_str("(result ");
                        ty.encode(sink);
                        sink.push_str(")");
                    }
                }

                for expr_index in expressions {
                    let expr = module.get_expr(*expr_index);
                    expr.encode_with_context(sink, module, func_index);
                }
            }
            wasm::Expression::Break { value, depth } => {
                sink.push_str("(br ");
                depth.encode(sink);
                match value {
                    Some(value) => {
                        sink.push_str(" ");
                        module
                            .get_expr(*value)
                            .encode_with_context(sink, module, func_index);
                    }
                    None => {}
                }
                sink.push_str(")");
            }
            wasm::Expression::Drop { value } => {
                sink.push_str("(drop");
                module
                    .get_expr(*value)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::Call {
                arguments,
                function,
            } => {
                let func_name = module.get_function(*function).name;
                sink.push_str(format!("(call ${} ", func_name).as_str());
                for arg in arguments {
                    module
                        .get_expr(*arg)
                        .encode_with_context(sink, module, func_index);
                }
                sink.push_str(")");
            }
            wasm::Expression::I32Const { value } => {
                sink.push_str("(i32.const ");
                value.encode(sink);
                sink.push_str(")");
            }
            wasm::Expression::I64Const { value } => {
                sink.push_str("(i64.const ");
                value.encode(sink);
                sink.push_str(")");
            }
            wasm::Expression::I32Add { left, right } => {
                sink.push_str("(i32.add");
                module
                    .get_expr(*left)
                    .encode_with_context(sink, module, func_index);
                module
                    .get_expr(*right)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::I32Sub { left, right } => {
                sink.push_str("(i32.sub");
                module
                    .get_expr(*left)
                    .encode_with_context(sink, module, func_index);
                module
                    .get_expr(*right)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::I32Mul { left, right } => {
                sink.push_str("(i32.mul");
                module
                    .get_expr(*left)
                    .encode_with_context(sink, module, func_index);
                module
                    .get_expr(*right)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::I32Eq { left, right } => {
                sink.push_str("(i32.eq");
                module
                    .get_expr(*left)
                    .encode_with_context(sink, module, func_index);
                module
                    .get_expr(*right)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::I64Add { left, right } => {
                sink.push_str("(i64.add");
                module
                    .get_expr(*left)
                    .encode_with_context(sink, module, func_index);
                module
                    .get_expr(*right)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::I64Sub { left, right } => {
                sink.push_str("(i64.sub");
                module
                    .get_expr(*left)
                    .encode_with_context(sink, module, func_index);
                module
                    .get_expr(*right)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::I64Mul { left, right } => {
                sink.push_str("(i64.mul");
                module
                    .get_expr(*left)
                    .encode_with_context(sink, module, func_index);
                module
                    .get_expr(*right)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
            wasm::Expression::I64Eq { left, right } => {
                sink.push_str("(i64.eq");
                module
                    .get_expr(*left)
                    .encode_with_context(sink, module, func_index);
                module
                    .get_expr(*right)
                    .encode_with_context(sink, module, func_index);
                sink.push_str(")");
            }
        }
    }
}

impl EncodeWithContext for wasm::FunctionBody<'_> {
    fn encode_with_context(
        &self,
        sink: &mut String,
        module: &wasm::Module,
        func_index: wasm::FunctionIndex,
    ) {
        sink.push_str(format!("(func ${}", self.name).as_str());

        let export_item = module.exports.items.iter().find(|item| match item {
            wasm::ExportItem::Function(func_export) => func_export.index.0 == func_index.0,
        });
        match export_item {
            Some(wasm::ExportItem::Function(func_export)) => {
                sink.push_str(format!(" (export \"{}\")", func_export.name).as_str());
            }
            _ => {}
        };

        let func_type = module.get_type(
            module
                .functions
                .functions
                .get(func_index.0 as usize)
                .unwrap()
                .clone(),
        );

        let params = self.locals.get(..func_type.param_count).unwrap_or(&[]);
        for (param_index, param) in params.iter().enumerate() {
            sink.push_str("(param $");
            sink.push_str(format!("{}_{}", param.name, param_index).as_str());
            sink.push_str(" ");
            param.ty.encode(sink);
            sink.push_str(")");
        }

        for result in func_type.results() {
            sink.push_str("(result ");
            result.encode(sink);
            sink.push_str(")");
        }

        let actual_locals = self.locals.get(func_type.param_count..).unwrap_or(&[]);
        for (local_index, local) in actual_locals.iter().enumerate() {
            sink.push_str("(local $");
            sink.push_str(
                format!("{}_{}", local.name, local_index + func_type.param_count).as_str(),
            );
            sink.push_str(" ");
            local.ty.encode(sink);
            sink.push_str(")");
        }

        for expr_index in &self.expressions {
            module
                .get_expr(*expr_index)
                .encode_with_context(sink, module, func_index);
        }
        sink.push_str(")");
    }
}

impl wasm::Module<'_> {
    pub fn to_wat(&self) -> String {
        let mut sink = String::new();
        sink.push_str("(module");
        for (index, func) in self.code.functions.iter().enumerate() {
            func.encode_with_context(&mut sink, self, wasm::FunctionIndex(index as u32));
        }
        sink.push_str(")");
        sink
    }
}
