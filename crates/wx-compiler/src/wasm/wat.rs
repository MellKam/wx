use crate::wasm;

trait Encode {
    fn encode(&self, sink: &mut String);
}

impl Encode for i32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(&self.to_string());
    }
}

impl Encode for i64 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(&self.to_string());
    }
}

impl Encode for u32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(&self.to_string());
    }
}

impl Encode for f32 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(&self.to_string());
    }
}

impl Encode for f64 {
    fn encode(&self, sink: &mut String) {
        sink.push_str(&self.to_string());
    }
}

impl Encode for wasm::ValueType {
    fn encode(&self, sink: &mut String) {
        match self {
            wasm::ValueType::I32 => sink.push_str("i32"),
            wasm::ValueType::I64 => sink.push_str("i64"),
            wasm::ValueType::F32 => sink.push_str("f32"),
            wasm::ValueType::F64 => sink.push_str("f64"),
        }
    }
}

#[derive(Clone)]
struct EncodeContext<'a> {
    module: &'a wasm::Module<'a>,
    func_index: wasm::FuncIndex,
}

impl EncodeContext<'_> {
    fn encode_expr(&self, sink: &mut String, expr_index: wasm::ExprIndex) {
        self.module
            .get_expr(expr_index)
            .encode_with_context(sink, self.clone());
    }
}

trait EncodeWithContext {
    fn encode_with_context(&self, sink: &mut String, ctx: EncodeContext);
}

impl EncodeWithContext for wasm::Expression {
    fn encode_with_context(&self, sink: &mut String, ctx: EncodeContext) {
        use wasm::Expression;
        match self {
            Expression::Nop => {
                sink.push_str("(nop)");
            }
            Expression::LocalGet { local_index } => {
                let local = ctx
                    .module
                    .get_function(ctx.func_index)
                    .locals
                    .get(local_index.0 as usize)
                    .unwrap();
                sink.push_str(format!("(local.get ${}_{})", local.name, local_index.0).as_str());
            }
            Expression::GlobalGet { global_index } => {
                let global = ctx
                    .module
                    .globals
                    .globals
                    .get(global_index.0 as usize)
                    .unwrap();
                sink.push_str(format!("(global.get ${})", global.name).as_str());
            }
            Expression::GlobalSet {
                global: global_index,
                value,
            } => {
                let global = ctx
                    .module
                    .globals
                    .globals
                    .get(global_index.0 as usize)
                    .unwrap();
                sink.push_str(format!("(global.set ${} ", global.name).as_str());
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::LocalSet { local_index, value } => {
                let local = ctx
                    .module
                    .get_function(ctx.func_index)
                    .locals
                    .get(local_index.0 as usize)
                    .unwrap();

                sink.push_str(format!("(local.set ${}_{} ", local.name, local_index.0).as_str());
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::Return { value } => match value {
                Some(value) => {
                    sink.push_str("(return ");
                    ctx.encode_expr(sink, *value);
                    sink.push_str(")");
                }
                None => {
                    sink.push_str("(return)");
                }
            },
            Expression::IfElse {
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
                ctx.encode_expr(sink, *condition);

                sink.push_str("(then");
                ctx.encode_expr(sink, *then_branch);
                sink.push_str(")");

                match else_branch {
                    Some(else_branch) => {
                        sink.push_str("(else");
                        ctx.encode_expr(sink, *else_branch);
                        sink.push_str(")");
                    }
                    None => {}
                }
                sink.push_str(")");
            }
            Expression::Block {
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
                    ctx.encode_expr(sink, *expr_index);
                }
                sink.push_str(")");
            }
            Expression::Loop {
                expressions,
                result,
            } => {
                sink.push_str("(loop ");
                match result {
                    wasm::BlockResult::Empty => {}
                    wasm::BlockResult::SingleValue(ty) => {
                        sink.push_str("(result ");
                        ty.encode(sink);
                        sink.push_str(")");
                    }
                }

                for expr_index in expressions {
                    ctx.encode_expr(sink, *expr_index);
                }
                sink.push_str(")");
            }
            Expression::Unreachable => {
                sink.push_str("(unreachable)");
            }
            Expression::Break { value, depth } => {
                sink.push_str("(br ");
                depth.encode(sink);
                match *value {
                    Some(value) => {
                        sink.push_str(" ");
                        ctx.encode_expr(sink, value);
                    }
                    None => {}
                }
                sink.push_str(")");
            }
            Expression::Drop { value } => {
                sink.push_str("(drop ");
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::Call {
                arguments,
                function,
            } => {
                let func_name = ctx.module.get_function(*function).name;
                sink.push_str("(call $");
                sink.push_str(func_name);
                sink.push_str(" ");
                for arg in arguments {
                    ctx.encode_expr(sink, *arg);
                }
                sink.push_str(")");
            }
            Expression::CallIndirect {
                expr,
                type_index,
                arguments,
                ..
            } => {
                ctx.encode_expr(sink, *expr);
                sink.push_str("(call_indirect (type ");
                type_index.0.encode(sink);
                sink.push_str(") ");
                for arg in arguments {
                    ctx.encode_expr(sink, *arg);
                }
                sink.push_str(")");
            }
            Expression::I32Const { value } => {
                sink.push_str("(i32.const ");
                value.encode(sink);
                sink.push_str(")");
            }
            Expression::I64Const { value } => {
                sink.push_str("(i64.const ");
                value.encode(sink);
                sink.push_str(")");
            }
            Expression::F32Const { value } => {
                sink.push_str("(f32.const ");
                value.encode(sink);
                sink.push_str(")");
            }
            Expression::F64Const { value } => {
                sink.push_str("(f64.const ");
                value.encode(sink);
                sink.push_str(")");
            }
            Expression::I32Add { left, right } => {
                sink.push_str("(i32.add ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32Sub { left, right } => {
                sink.push_str("(i32.sub ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32Mul { left, right } => {
                sink.push_str("(i32.mul ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32Eq { left, right } => {
                sink.push_str("(i32.eq ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Add { left, right } => {
                sink.push_str("(i64.add ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Sub { left, right } => {
                sink.push_str("(i64.sub ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Mul { left, right } => {
                sink.push_str("(i64.mul ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Eq { left, right } => {
                sink.push_str("(i64.eq ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32And { left, right } => {
                sink.push_str("(i32.and ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32Or { left, right } => {
                sink.push_str("(i32.or ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32Eqz { value } => {
                sink.push_str("(i32.eqz ");
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::I64And { left, right } => {
                sink.push_str("(i64.and ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Or { left, right } => {
                sink.push_str("(i64.or ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Eqz { value } => {
                sink.push_str("(i64.eqz ");
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::I32Ne { left, right } => {
                sink.push_str("(i32.ne ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Ne { left, right } => {
                sink.push_str("(i64.ne ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32LtS { left, right } => {
                sink.push_str("(i32.lt_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32GtS { left, right } => {
                sink.push_str("(i32.gt_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32LeS { left, right } => {
                sink.push_str("(i32.le_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32GeS { left, right } => {
                sink.push_str("(i32.ge_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64LtS { left, right } => {
                sink.push_str("(i64.lt_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64GtS { left, right } => {
                sink.push_str("(i64.gt_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64LeS { left, right } => {
                sink.push_str("(i64.le_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64GeS { left, right } => {
                sink.push_str("(i64.ge_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32Shl { left, right } => {
                sink.push_str("(i32.shl ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32ShrS { left, right } => {
                sink.push_str("(i32.shr_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Shl { left, right } => {
                sink.push_str("(i64.shl ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64ShrS { left, right } => {
                sink.push_str("(i64.shr_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32DivS { left, right } => {
                sink.push_str("(i32.div_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32RemS { left, right } => {
                sink.push_str("(i32.rem_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64DivS { left, right } => {
                sink.push_str("(i64.div_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64RemS { left, right } => {
                sink.push_str("(i64.rem_s ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32Xor { left, right } => {
                sink.push_str("(i32.xor ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64Xor { left, right } => {
                sink.push_str("(i64.xor ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Add { left, right } => {
                sink.push_str("(f32.add ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Sub { left, right } => {
                sink.push_str("(f32.sub ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Mul { left, right } => {
                sink.push_str("(f32.mul ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Add { left, right } => {
                sink.push_str("(f64.add ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Sub { left, right } => {
                sink.push_str("(f64.sub ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Mul { left, right } => {
                sink.push_str("(f64.mul ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Eq { left, right } => {
                sink.push_str("(f32.eq ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Ne { left, right } => {
                sink.push_str("(f32.ne ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Eq { left, right } => {
                sink.push_str("(f64.eq ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Ne { left, right } => {
                sink.push_str("(f64.ne ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Lt { left, right } => {
                sink.push_str("(f32.lt ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Gt { left, right } => {
                sink.push_str("(f32.gt ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Lt { left, right } => {
                sink.push_str("(f64.lt ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Gt { left, right } => {
                sink.push_str("(f64.gt ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Le { left, right } => {
                sink.push_str("(f32.le ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Ge { left, right } => {
                sink.push_str("(f32.ge ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Le { left, right } => {
                sink.push_str("(f64.le ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Ge { left, right } => {
                sink.push_str("(f64.ge ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Div { left, right } => {
                sink.push_str("(f32.div ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F64Div { left, right } => {
                sink.push_str("(f64.div ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::F32Neg { value } => {
                sink.push_str("(f32.neg ");
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::F64Neg { value } => {
                sink.push_str("(f64.neg ");
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::F32Trunc { value } => {
                sink.push_str("(f32.trunc ");
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::F64Trunc { value } => {
                sink.push_str("(f64.trunc ");
                ctx.encode_expr(sink, *value);
                sink.push_str(")");
            }
            Expression::I32DivU { left, right } => {
                sink.push_str("(i32.div_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32RemU { left, right } => {
                sink.push_str("(i32.rem_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64DivU { left, right } => {
                sink.push_str("(i64.div_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64RemU { left, right } => {
                sink.push_str("(i64.rem_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32GtU { left, right } => {
                sink.push_str("(i32.gt_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32LtU { left, right } => {
                sink.push_str("(i32.lt_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32LeU { left, right } => {
                sink.push_str("(i32.le_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32GeU { left, right } => {
                sink.push_str("(i32.ge_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64GtU { left, right } => {
                sink.push_str("(i64.gt_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64LtU { left, right } => {
                sink.push_str("(i64.lt_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64LeU { left, right } => {
                sink.push_str("(i64.le_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64GeU { left, right } => {
                sink.push_str("(i64.ge_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I32ShrU { left, right } => {
                sink.push_str("(i32.shr_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
            Expression::I64ShrU { left, right } => {
                sink.push_str("(i64.shr_u ");
                ctx.encode_expr(sink, *left);
                ctx.encode_expr(sink, *right);
                sink.push_str(")");
            }
        }
    }
}

impl EncodeWithContext for wasm::FunctionBody<'_> {
    fn encode_with_context(&self, sink: &mut String, ctx: EncodeContext) {
        sink.push_str(format!("(func ${}", self.name).as_str());

        let export_item = ctx.module.exports.items.iter().find(|item| match item {
            wasm::ExportItem::Function { func_index, .. } => func_index.0 == ctx.func_index.0,
            wasm::ExportItem::Global { .. } => false,
        });
        match export_item {
            Some(wasm::ExportItem::Function { name, .. }) => {
                sink.push_str(format!(" (export \"{}\")", name).as_str());
            }
            _ => {}
        };

        let func_type = ctx.module.get_type(
            ctx.module
                .functions
                .types
                .get(ctx.func_index.0 as usize)
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
            ctx.encode_expr(sink, *expr_index);
        }
        sink.push_str(")");
    }
}

impl wasm::Module<'_> {
    pub fn to_wat(&self) -> String {
        let mut sink = String::new();
        sink.push_str("(module");
        for (index, func) in self.code.functions.iter().enumerate() {
            func.encode_with_context(
                &mut sink,
                EncodeContext {
                    module: self,
                    func_index: wasm::FuncIndex(index as u32),
                },
            );
        }
        sink.push_str(")");
        sink
    }
}
