use crate::mir::{self, *};
use crate::{ast, hir};

#[derive(Debug)]
struct ScopeMeta {
    relative_index: u32,
    locals_offset: u32,
}

pub struct Builder<'hir> {
    hir: &'hir hir::HIR,
    mir: MIR,
    scopes_meta: Box<[ScopeMeta]>,
    // func_index: usize,
}

impl<'hir> Builder<'hir> {
    pub fn build_function(hir: &'hir hir::HIR, func_index: usize) -> Result<MIR, ()> {
        let func = &hir.functions[func_index];

        let mut scopes_meta: Vec<ScopeMeta> = Vec::with_capacity(func.stack.scopes.len());
        scopes_meta.push(ScopeMeta {
            relative_index: 0,
            locals_offset: 0,
        });
        for scope in func.stack.scopes.iter().skip(1) {
            let parent_index = scope.parent.unwrap().0 as usize;
            let parent_scope_meta = &scopes_meta[parent_index];
            let relative_index = parent_scope_meta.relative_index + 1;
            let parent_scope = &func.stack.scopes[parent_index];
            let locals_offset = parent_scope_meta.locals_offset + parent_scope.locals.len() as u32;
            scopes_meta.push(ScopeMeta {
                relative_index,
                locals_offset,
            });
        }
        // println!("scopes_meta: {:#?}", scopes_meta);

        let mut builder = Builder {
            hir,
            mir: MIR {
                data_nodes: Vec::new(),
                data_lookup: HashMap::new(),
                block: Block {
                    statements: Vec::new(),
                    relative_locals: Box::new([]),
                    result: StackResult::Unit,
                },
                params: func
                    .params
                    .iter()
                    .map(|param| mir::FunctionParam {
                        name: param.name.symbol,
                        ty: ResultType::from_hir(hir, param.ty.ty).unwrap_value(),
                    })
                    .collect(),
                symbol: func.name.symbol,
            },
            scopes_meta: scopes_meta.into_boxed_slice(),
            // func_index,
        };

        match &func.block.kind {
            hir::ExprKind::Block {
                scope_index: _,
                expressions,
                result,
            } => {
                let root_scope = func.stack.scopes.first().unwrap();
                let mut locals: Vec<StackResult> = Vec::with_capacity(root_scope.locals.len());
                for (local_index, local) in
                    root_scope.locals[0..func.params.len()].iter().enumerate()
                {
                    let node_id = builder.mir.ensure_data_node(DataNodeKind::Parameter {
                        index: local_index as u32,
                        symbol: local.name.symbol,
                        ty: ResultType::from_hir(hir, local.ty).unwrap_value(),
                    });
                    locals.push(StackResult::Value(node_id));
                }
                for local in root_scope.locals[func.params.len()..].iter() {
                    locals.push(builder.create_default_local_value(local.ty));
                }
                let mut block = Block {
                    relative_locals: locals.into_boxed_slice(),
                    statements: Vec::new(),
                    result: StackResult::Unit,
                };

                for expr in expressions.iter() {
                    builder.build_expr(&mut block, expr)?;
                }
                match result {
                    Some(result) => {
                        builder.mir.block.result = builder.build_expr(&mut block, result)?;
                    }
                    None => {}
                };

                block.result = builder.mir.block.result;
                builder.mir.block = block;
            }
            _ => unreachable!(),
        }

        Ok(builder.mir)
    }

    fn create_default_local_value(&mut self, ty: hir::Type) -> StackResult {
        match ty {
            hir::Type::I32 | hir::Type::Bool | hir::Type::U32 | hir::Type::Function(_) => {
                StackResult::Value(self.mir.ensure_data_node(DataNodeKind::Int {
                    value: 0,
                    ty: ValueType::I32,
                }))
            }
            hir::Type::I64 | hir::Type::U64 => {
                StackResult::Value(self.mir.ensure_data_node(DataNodeKind::Int {
                    value: 0,
                    ty: ValueType::I64,
                }))
            }
            hir::Type::F32 => StackResult::Value(self.mir.ensure_data_node(DataNodeKind::Float {
                value: 0,
                ty: ValueType::F32,
            })),
            hir::Type::F64 => StackResult::Value(self.mir.ensure_data_node(DataNodeKind::Float {
                value: 0,
                ty: ValueType::F64,
            })),
            hir::Type::Enum(enum_index) => {
                let enum_def = &self.hir.enums[enum_index.0 as usize];
                self.create_default_local_value(enum_def.ty)
            }
            hir::Type::Unit => StackResult::Unit,
            hir::Type::Never => StackResult::Never,
            hir::Type::Unknown => panic!("unknown type"),
        }
    }

    fn build_block(
        &mut self,
        parent_block: &Block,
        scope_index: hir::ScopeIndex,
        expressions: &Box<[hir::Expression]>,
        result: &Option<Box<hir::Expression>>,
    ) -> Result<Block, ()> {
        let scope = &self.hir.functions[0].stack.scopes[scope_index.0 as usize];
        let scope_offset = self.scopes_meta[scope_index.0 as usize].locals_offset as usize;
        let mut relative_locals = Vec::with_capacity(scope_offset + scope.locals.len());
        unsafe {
            relative_locals.set_len(scope_offset + scope.locals.len());
        }
        relative_locals[..parent_block.relative_locals.len()]
            .copy_from_slice(&parent_block.relative_locals[..]);
        for (local_index, local) in scope.locals.iter().enumerate() {
            relative_locals[scope_offset + local_index] = self.create_default_local_value(local.ty);
        }

        let mut block = Block {
            relative_locals: relative_locals.into_boxed_slice(),
            statements: Vec::new(),
            result: StackResult::Unit,
        };

        for expr in expressions.iter() {
            match self.build_expr(&mut block, expr)? {
                StackResult::Unit => {}
                StackResult::Never => return Ok(block),
                StackResult::Value(_) => {
                    println!("{:#?}", expr);
                    panic!("unused value in statement");
                }
            }
        }

        block.result = match result {
            Some(result) => self.build_expr(&mut block, result)?,
            None => StackResult::Unit,
        };

        return Ok(block);
    }

    fn build_expr(&mut self, block: &mut Block, expr: &hir::Expression) -> Result<StackResult, ()> {
        match &expr.kind {
            hir::ExprKind::Int(value) => {
                let result = self.mir.ensure_data_node(DataNodeKind::Int {
                    value: *value,
                    ty: ResultType::from_hir(self.hir, expr.ty.unwrap()).unwrap_value(),
                });
                Ok(StackResult::Value(result))
            }
            hir::ExprKind::Local {
                scope_index,
                local_index,
            } => {
                let relative_local_index =
                    self.scopes_meta[scope_index.0 as usize].locals_offset + local_index.0;
                Ok(block.relative_locals[relative_local_index as usize])
            }
            hir::ExprKind::LocalDeclaration {
                name: _,
                scope_index,
                local_index,
                expr,
            } => {
                let result = self.build_expr(block, expr)?;
                let relative_local_index =
                    self.scopes_meta[scope_index.0 as usize].locals_offset + local_index.0;
                block.relative_locals[relative_local_index as usize] = result;
                Ok(StackResult::Unit)
            }
            hir::ExprKind::Return { value } => {
                let result = match value {
                    Some(expr) => self.build_expr(block, expr).unwrap(),
                    None => StackResult::Unit,
                };
                // TODO: handle phi nodes if already assigned
                self.mir.block.result = result;
                Ok(StackResult::Never)
            }
            hir::ExprKind::Bool(value) => {
                let node_id = self.mir.ensure_data_node(DataNodeKind::Int {
                    value: if *value { 1 } else { 0 },
                    ty: ValueType::I32,
                });
                Ok(StackResult::Value(node_id))
            }
            hir::ExprKind::Float(value) => {
                let node_id = self.mir.ensure_data_node(DataNodeKind::Float {
                    value: value.to_bits(),
                    ty: ResultType::from_hir(self.hir, expr.ty.unwrap()).unwrap_value(),
                });
                Ok(StackResult::Value(node_id))
            }
            hir::ExprKind::Block {
                scope_index,
                expressions,
                result,
            } => {
                let new_block = self.build_block(&block, *scope_index, expressions, result)?;

                block.relative_locals.copy_from_slice(
                    &new_block.relative_locals
                        [0..self.scopes_meta[scope_index.0 as usize].locals_offset as usize],
                );
                Ok(new_block.result)
            }
            // hir::ExprKind::IfElse {
            //     condition,
            //     then_block,
            //     else_block,
            // } => {
            //     let condition_id = self.build_expr(block, condition)?.unwrap();
            //     let (then_result, then_locals) = match &then_block.kind {
            //         hir::ExprKind::Block {
            //             expressions,
            //             result,
            //             scope_index,
            //         } => {
            //             let scope = &self.hir.functions[0].stack.scopes[scope_index.0 as usize];
            //             let mut then_locals: Box<[Option<DataNodeId>]> = {
            //                 let mut new_locals = vec![
            //                     None;
            //                     self.scopes_meta[scope_index.0 as usize].locals_offset
            //                         as usize
            //                         + scope.locals.len()
            //                 ];
            //                 new_locals[..locals.len()].copy_from_slice(&locals[..]);
            //                 new_locals.into_boxed_slice()
            //             };

            //             for expr in expressions.iter() {
            //                 self.build_expr(&mut then_locals, expr)?;
            //             }
            //             let then_result = match &result {
            //                 Some(result) => self.build_expr(&mut then_locals, result).unwrap(),
            //                 None => None,
            //             };
            //             (then_result, then_locals)
            //         }
            //         _ => unreachable!(),
            //     };
            //     let (else_result, else_locals) = match else_block {
            //         Some(else_block) => match &else_block.kind {
            //             hir::ExprKind::Block {
            //                 expressions,
            //                 result,
            //                 scope_index,
            //             } => {
            //                 let scope = &self.hir.functions[0].stack.scopes[scope_index.0 as
            // usize];                 let mut else_locals: Box<[Option<DataNodeId>]> =
            // {                     let mut new_locals = vec![
            //                         None;
            //                         self.scopes_meta[scope_index.0 as usize].locals_offset
            //                             as usize
            //                             + scope.locals.len()
            //                     ];
            //                     new_locals[..locals.len()].copy_from_slice(&locals[..]);
            //                     new_locals.into_boxed_slice()
            //                 };

            //                 for expr in expressions.iter() {
            //                     self.build_expr(&mut else_locals, expr)?;
            //                 }
            //                 let else_result = match &result {
            //                     Some(result) => self.build_expr(&mut else_locals,
            // result).unwrap(),                     None => None,
            //                 };
            //                 (else_result, else_locals)
            //             }
            //             _ => unreachable!(),
            //         },
            //         None => (None, locals.clone()),
            //     };

            //     for i in 0..locals.len() {
            //         let then_value = then_locals[i];
            //         let else_value = else_locals[i];
            //         if then_value == else_value {
            //             locals[i] = then_value;
            //         } else {
            //             let ty = self.mir.data_nodes[then_value.unwrap() as usize].kind.ty();
            //             locals[i] = Some(self.mir.ensure_data_node(DataNodeKind::Phi {
            //                 left: then_value.unwrap(),
            //                 right: else_value.unwrap(),
            //                 ty,
            //             }));
            //         }
            //     }

            //     let result = match (then_result, else_result) {
            //         (Some(then_id), Some(else_id)) => {
            //             let ty = self.mir.data_nodes[then_id as usize].kind.ty();
            //             let result_id = self.mir.ensure_data_node(DataNodeKind::Phi {
            //                 left: then_id,
            //                 right: else_id,
            //                 ty,
            //             });
            //             Some(result_id)
            //         }
            //         _ => None,
            //     };

            //     Ok(result)
            // }
            hir::ExprKind::Binary {
                left,
                operator,
                right,
            } => {
                let left_result = self.build_expr(block, &left)?;
                let right_result = self.build_expr(block, &right)?;
                if operator.kind == ast::BinOpKind::Assign {
                    match &left.kind {
                        hir::ExprKind::Local {
                            scope_index,
                            local_index,
                        } => {
                            let relative_local_index = self.scopes_meta[scope_index.0 as usize]
                                .locals_offset
                                + local_index.0;
                            block.relative_locals[relative_local_index as usize] = right_result;
                            return Ok(StackResult::Unit);
                        }
                        _ => unimplemented!(),
                    }
                }

                let left = left_result.unwrap_value();
                let right = right_result.unwrap_value();
                let ty = ResultType::from_hir(self.hir, expr.ty.unwrap()).unwrap_value();
                let node_kind = match operator.kind {
                    ast::BinOpKind::Add => DataNodeKind::Add { left, right, ty },
                    ast::BinOpKind::Sub => DataNodeKind::Sub { left, right, ty },
                    ast::BinOpKind::Mul => DataNodeKind::Mul { left, right, ty },
                    ast::BinOpKind::Div => DataNodeKind::Div { left, right, ty },
                    ast::BinOpKind::Less => DataNodeKind::Lt { left, right, ty },
                    ast::BinOpKind::LessEq => DataNodeKind::LtEq { left, right, ty },
                    ast::BinOpKind::Greater => DataNodeKind::Gt { left, right, ty },
                    ast::BinOpKind::GreaterEq => DataNodeKind::GtEq { left, right, ty },
                    ast::BinOpKind::Eq => DataNodeKind::Eq { left, right, ty },
                    ast::BinOpKind::NotEq => DataNodeKind::NotEq { left, right, ty },
                    _ => todo!(),
                };
                let result_id = self.mir.ensure_data_node(node_kind);
                self.mir.data_nodes[left as usize].uses.push(result_id);
                self.mir.data_nodes[right as usize].uses.push(result_id);
                Ok(StackResult::Value(result_id))
            }
            expr => todo!("build_expr: {:?}", expr),
        }
    }
}

// test code
#[cfg(test)]
mod tests {
    use indoc::indoc;
    use string_interner::StringInterner;

    use super::*;
    use crate::files;

    #[test]
    fn build_simple_function() {
        let source = indoc! {"
            export func test(mut n: i32): i32 {
                local d = n * 10;
                local m: i32 = 10;
                n = { 
                  local d = n + m + d;
                  d + 8
                };
                return { n / 2 * d };
            }
        "};
        let mut interner = StringInterner::new();
        let mut files = files::Files::new();
        let file_id = files.add("main".to_string(), source.to_string()).unwrap();

        let ast =
            ast::parser::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
        let hir = hir::Builder::build(&ast.ast, &mut interner);
        if hir.diagnostics.len() > 0 {
            panic!("{:#?}", hir.diagnostics);
        }

        let mir = Builder::build_function(&hir.hir, 0).unwrap();
        println!("{:#?}", mir);
    }
}
