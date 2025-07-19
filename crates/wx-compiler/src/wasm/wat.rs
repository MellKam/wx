use string_interner::{StringInterner, backend::StringBackend};

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

struct EncodeContext<'sink, 'module, 'interner> {
    sink: &'sink mut String,
    module: &'module wasm::Module,
    interner: &'interner StringInterner<StringBackend>,
    func_index: wasm::FuncIndex,
}

trait ContextEncode {
    fn encode(&self, ctx: &mut EncodeContext);
}

impl ContextEncode for wasm::Expression {
    fn encode(&self, ctx: &mut EncodeContext) {
        use wasm::Expression;
        match self {
            Expression::I32Const { value } => {
                ctx.sink.push_str("(i32.const ");
                value.encode(ctx.sink);
                ctx.sink.push_str(")");
            }
            Expression::I64Const { value } => {
                ctx.sink.push_str("(i64.const ");
                value.encode(ctx.sink);
                ctx.sink.push_str(")");
            }
            Expression::F32Const { value } => {
                ctx.sink.push_str("(f32.const ");
                value.encode(ctx.sink);
                ctx.sink.push_str(")");
            }
            Expression::F64Const { value } => {
                ctx.sink.push_str("(f64.const ");
                value.encode(ctx.sink);
                ctx.sink.push_str(")");
            }
            Expression::Nop => {
                ctx.sink.push_str("(nop)");
            }
            Expression::LocalGet { local_index } => {
                let local = &ctx.module.code.functions[ctx.func_index.0 as usize].locals
                    [local_index.0 as usize];
                ctx.sink.push_str(
                    format!(
                        "(local.get ${}_{})",
                        ctx.interner.resolve(local.name).unwrap(),
                        local_index.0
                    )
                    .as_str(),
                );
            }
            Expression::LocalSet { local_index, value } => {
                let local = &ctx.module.code.functions[ctx.func_index.0 as usize].locals
                    [local_index.0 as usize];

                ctx.sink.push_str(
                    format!(
                        "(local.set ${}_{} ",
                        ctx.interner.resolve(local.name).unwrap(),
                        local_index.0
                    )
                    .as_str(),
                );
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::GlobalGet { global_index } => {
                let global = &ctx.module.globals.globals[global_index.0 as usize];
                ctx.sink.push_str(
                    format!(
                        "(global.get ${})",
                        ctx.interner.resolve(global.name).unwrap(),
                    )
                    .as_str(),
                );
            }
            Expression::GlobalSet {
                global: global_index,
                value,
            } => {
                let global = &ctx.module.globals.globals[global_index.0 as usize];
                ctx.sink.push_str("(global.set $");
                let name = ctx.interner.resolve(global.name).unwrap();
                ctx.sink.push_str(name);
                ctx.sink.push_str(" ");
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::Return { value } => match value {
                Some(value) => {
                    ctx.sink.push_str("(return ");
                    value.encode(ctx);
                    ctx.sink.push_str(")");
                }
                None => {
                    ctx.sink.push_str("(return)");
                }
            },
            Expression::IfElse {
                condition,
                result,
                then_branch,
                else_branch,
            } => {
                ctx.sink.push_str("(if ");
                match result {
                    wasm::BlockResult::Empty => {}
                    wasm::BlockResult::SingleValue(ty) => {
                        ctx.sink.push_str("(result ");
                        ty.encode(ctx.sink);
                        ctx.sink.push_str(")");
                    }
                }
                condition.encode(ctx);

                ctx.sink.push_str("(then");
                then_branch.encode(ctx);
                ctx.sink.push_str(")");

                match else_branch {
                    Some(else_branch) => {
                        ctx.sink.push_str("(else");
                        else_branch.encode(ctx);
                        ctx.sink.push_str(")");
                    }
                    None => {}
                }
                ctx.sink.push_str(")");
            }
            Expression::Block {
                expressions,
                result,
            } => {
                ctx.sink.push_str("(block ");
                match result {
                    wasm::BlockResult::Empty => {}
                    wasm::BlockResult::SingleValue(ty) => {
                        ctx.sink.push_str("(result ");
                        ty.encode(ctx.sink);
                        ctx.sink.push_str(")");
                    }
                }

                for expr in expressions.iter() {
                    expr.encode(ctx);
                }
                ctx.sink.push_str(")");
            }
            Expression::Loop {
                expressions,
                result,
            } => {
                ctx.sink.push_str("(loop ");
                match result {
                    wasm::BlockResult::Empty => {}
                    wasm::BlockResult::SingleValue(ty) => {
                        ctx.sink.push_str("(result ");
                        ty.encode(ctx.sink);
                        ctx.sink.push_str(")");
                    }
                }

                for expr in expressions.iter() {
                    expr.encode(ctx);
                }
                ctx.sink.push_str(")");
            }
            Expression::Unreachable => {
                ctx.sink.push_str("(unreachable)");
            }
            Expression::Break { value, depth } => {
                ctx.sink.push_str("(br ");
                depth.encode(ctx.sink);
                match value {
                    Some(value) => {
                        ctx.sink.push_str(" ");
                        value.encode(ctx);
                    }
                    None => {}
                }
                ctx.sink.push_str(")");
            }
            Expression::Drop { value } => {
                ctx.sink.push_str("(drop ");
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::Call {
                arguments,
                function,
            } => {
                let name = ctx
                    .interner
                    .resolve(ctx.module.code.functions[function.0 as usize].name)
                    .unwrap();
                ctx.sink.push_str("(call $");
                ctx.sink.push_str(name);
                ctx.sink.push_str(" ");
                for argument in arguments.iter() {
                    argument.encode(ctx);
                }
                ctx.sink.push_str(")");
            }
            Expression::CallIndirect {
                expr,
                type_index,
                arguments,
                ..
            } => {
                expr.encode(ctx);
                ctx.sink.push_str("(call_indirect (type ");
                type_index.0.encode(ctx.sink);
                ctx.sink.push_str(") ");
                for argument in arguments.iter() {
                    argument.encode(ctx);
                }
                ctx.sink.push_str(")");
            }
            Expression::I32Add { left, right } => {
                ctx.sink.push_str("(i32.add ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32Sub { left, right } => {
                ctx.sink.push_str("(i32.sub ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32Mul { left, right } => {
                ctx.sink.push_str("(i32.mul ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32Eq { left, right } => {
                ctx.sink.push_str("(i32.eq ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Add { left, right } => {
                ctx.sink.push_str("(i64.add ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Sub { left, right } => {
                ctx.sink.push_str("(i64.sub ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Mul { left, right } => {
                ctx.sink.push_str("(i64.mul ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Eq { left, right } => {
                ctx.sink.push_str("(i64.eq ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32And { left, right } => {
                ctx.sink.push_str("(i32.and ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32Or { left, right } => {
                ctx.sink.push_str("(i32.or ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32Eqz { value } => {
                ctx.sink.push_str("(i32.eqz ");
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64And { left, right } => {
                ctx.sink.push_str("(i64.and ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Or { left, right } => {
                ctx.sink.push_str("(i64.or ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Eqz { value } => {
                ctx.sink.push_str("(i64.eqz ");
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32Ne { left, right } => {
                ctx.sink.push_str("(i32.ne ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Ne { left, right } => {
                ctx.sink.push_str("(i64.ne ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32LtS { left, right } => {
                ctx.sink.push_str("(i32.lt_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32GtS { left, right } => {
                ctx.sink.push_str("(i32.gt_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32LeS { left, right } => {
                ctx.sink.push_str("(i32.le_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32GeS { left, right } => {
                ctx.sink.push_str("(i32.ge_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64LtS { left, right } => {
                ctx.sink.push_str("(i64.lt_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64GtS { left, right } => {
                ctx.sink.push_str("(i64.gt_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64LeS { left, right } => {
                ctx.sink.push_str("(i64.le_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64GeS { left, right } => {
                ctx.sink.push_str("(i64.ge_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32Shl { left, right } => {
                ctx.sink.push_str("(i32.shl ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32ShrS { left, right } => {
                ctx.sink.push_str("(i32.shr_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Shl { left, right } => {
                ctx.sink.push_str("(i64.shl ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64ShrS { left, right } => {
                ctx.sink.push_str("(i64.shr_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32DivS { left, right } => {
                ctx.sink.push_str("(i32.div_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32RemS { left, right } => {
                ctx.sink.push_str("(i32.rem_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64DivS { left, right } => {
                ctx.sink.push_str("(i64.div_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64RemS { left, right } => {
                ctx.sink.push_str("(i64.rem_s ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32Xor { left, right } => {
                ctx.sink.push_str("(i32.xor ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64Xor { left, right } => {
                ctx.sink.push_str("(i64.xor ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Add { left, right } => {
                ctx.sink.push_str("(f32.add ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Sub { left, right } => {
                ctx.sink.push_str("(f32.sub ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Mul { left, right } => {
                ctx.sink.push_str("(f32.mul ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Add { left, right } => {
                ctx.sink.push_str("(f64.add ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Sub { left, right } => {
                ctx.sink.push_str("(f64.sub ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Mul { left, right } => {
                ctx.sink.push_str("(f64.mul ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Eq { left, right } => {
                ctx.sink.push_str("(f32.eq ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Ne { left, right } => {
                ctx.sink.push_str("(f32.ne ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Eq { left, right } => {
                ctx.sink.push_str("(f64.eq ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Ne { left, right } => {
                ctx.sink.push_str("(f64.ne ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Lt { left, right } => {
                ctx.sink.push_str("(f32.lt ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Gt { left, right } => {
                ctx.sink.push_str("(f32.gt ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Lt { left, right } => {
                ctx.sink.push_str("(f64.lt ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Gt { left, right } => {
                ctx.sink.push_str("(f64.gt ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Le { left, right } => {
                ctx.sink.push_str("(f32.le ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Ge { left, right } => {
                ctx.sink.push_str("(f32.ge ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Le { left, right } => {
                ctx.sink.push_str("(f64.le ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Ge { left, right } => {
                ctx.sink.push_str("(f64.ge ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Div { left, right } => {
                ctx.sink.push_str("(f32.div ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Div { left, right } => {
                ctx.sink.push_str("(f64.div ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Neg { value } => {
                ctx.sink.push_str("(f32.neg ");
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Neg { value } => {
                ctx.sink.push_str("(f64.neg ");
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F32Trunc { value } => {
                ctx.sink.push_str("(f32.trunc ");
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::F64Trunc { value } => {
                ctx.sink.push_str("(f64.trunc ");
                value.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32DivU { left, right } => {
                ctx.sink.push_str("(i32.div_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32RemU { left, right } => {
                ctx.sink.push_str("(i32.rem_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64DivU { left, right } => {
                ctx.sink.push_str("(i64.div_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64RemU { left, right } => {
                ctx.sink.push_str("(i64.rem_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32GtU { left, right } => {
                ctx.sink.push_str("(i32.gt_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32LtU { left, right } => {
                ctx.sink.push_str("(i32.lt_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32LeU { left, right } => {
                ctx.sink.push_str("(i32.le_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32GeU { left, right } => {
                ctx.sink.push_str("(i32.ge_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64GtU { left, right } => {
                ctx.sink.push_str("(i64.gt_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64LtU { left, right } => {
                ctx.sink.push_str("(i64.lt_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64LeU { left, right } => {
                ctx.sink.push_str("(i64.le_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64GeU { left, right } => {
                ctx.sink.push_str("(i64.ge_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I32ShrU { left, right } => {
                ctx.sink.push_str("(i32.shr_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
            Expression::I64ShrU { left, right } => {
                ctx.sink.push_str("(i64.shr_u ");
                left.encode(ctx);
                right.encode(ctx);
                ctx.sink.push_str(")");
            }
        }
    }
}

impl ContextEncode for wasm::FunctionBody {
    fn encode(&self, ctx: &mut EncodeContext) {
        let name = ctx.interner.resolve(self.name).unwrap();
        ctx.sink.push_str(format!("(func ${}", name).as_str());

        let export_item = ctx.module.exports.items.iter().find(|item| match item {
            wasm::ExportItem::Function { func_index, .. } => func_index.0 == ctx.func_index.0,
            wasm::ExportItem::Global { .. } => false,
        });
        match export_item {
            Some(wasm::ExportItem::Function { name, .. }) => {
                let name = ctx.interner.resolve(*name).unwrap();
                ctx.sink.push_str(" (export \"");
                ctx.sink.push_str(name);
                ctx.sink.push_str("\")");
            }
            _ => {}
        };

        let type_index = ctx.module.functions.types[ctx.func_index.0 as usize];
        let func_type = ctx.module.types.signatures[type_index.0 as usize].clone();

        let params = self.locals.get(..func_type.param_count).unwrap_or(&[]);
        for (param_index, param) in params.iter().enumerate() {
            ctx.sink.push_str("(param $");
            let name = ctx.interner.resolve(param.name).unwrap();
            ctx.sink
                .push_str(format!("{}_{}", name, param_index).as_str());
            ctx.sink.push_str(" ");
            param.ty.encode(ctx.sink);
            ctx.sink.push_str(")");
        }

        for result in func_type.results().iter().copied() {
            ctx.sink.push_str("(result ");
            result.encode(ctx.sink);
            ctx.sink.push_str(")");
        }

        let actual_locals = self.locals.get(func_type.param_count..).unwrap_or(&[]);
        for (local_index, local) in actual_locals.iter().enumerate() {
            ctx.sink.push_str("(local $");
            let name = ctx.interner.resolve(local.name).unwrap();
            ctx.sink
                .push_str(format!("{}_{}", name, local_index + func_type.param_count).as_str());
            ctx.sink.push_str(" ");
            local.ty.encode(ctx.sink);
            ctx.sink.push_str(")");
        }

        for expr in self.expressions.iter() {
            expr.encode(ctx);
        }
        ctx.sink.push_str(")");
    }
}

impl wasm::Module {
    pub fn to_wat(&self, interner: &StringInterner<StringBackend>) -> String {
        let mut sink = String::new();
        sink.push_str("(module");
        for (index, func) in self.code.functions.iter().enumerate() {
            func.encode(&mut EncodeContext {
                module: self,
                func_index: wasm::FuncIndex(index as u32),
                interner,
                sink: &mut sink,
            });
        }
        sink.push_str(")");
        sink
    }
}
