use std::collections::HashMap;

use wasm_bindgen::prelude::*;
use wx_compiler::*;

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> Result<Vec<u8>, JsValue> {
    let mut builder = vfs::CompilationGraphBuilder::new();
    let stdlib_id = builder
        .load_stdlib()
        .map_err(|err| serde_wasm_bindgen::to_value(&format!("{err:?}")).unwrap())?;
    let root_id = builder
        .load_binary(
            filename.clone(),
            &vfs::VirtualFileSource::new(HashMap::from([(filename, source)])),
        )
        .map_err(|err| serde_wasm_bindgen::to_value(&format!("{err:?}")).unwrap())?;
    let mut compilation = builder.build(root_id, stdlib_id);
    let ast_diagnostics: Vec<_> = compilation
        .crates
        .iter()
        .flat_map(|crate_graph| crate_graph.diagnostics.iter().cloned())
        .collect();
    if !ast_diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&ast_diagnostics).unwrap());
    }
    let hir = tir::TIR::build(&mut compilation);
    if !hir.diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&hir.diagnostics).unwrap());
    }
    let mir = mir::MIR::build(&hir, &compilation.interner, compilation.id_generator);
    let module = codegen::Builder::build(&mir, &compilation.interner).unwrap();
    let bytecode = module.encode();

    Ok(bytecode)
}
