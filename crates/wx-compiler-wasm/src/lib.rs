use string_interner::StringInterner;
use wasm_bindgen::prelude::*;
use wx_compiler::*;

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> Result<Vec<u8>, JsValue> {
    let mut interner = StringInterner::new();
    let compilation = wx_compiler::vfs::load_inline_compilation(filename, source, &mut interner)
        .map_err(|err| serde_wasm_bindgen::to_value(&format!("{err:?}")).unwrap())?;
    let ast_diagnostics: Vec<_> = compilation
        .crates
        .iter()
        .flat_map(|crate_graph| crate_graph.diagnostics.iter().cloned())
        .collect();
    if !ast_diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&ast_diagnostics).unwrap());
    }
    let hir = tir::TIR::build(&compilation, &mut interner);
    if !hir.diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&hir.diagnostics).unwrap());
    }
    let mir = mir::MIR::build(&hir, &interner, compilation.id_generator);
    let module = codegen::Builder::build(&mir, &interner).unwrap();
    let bytecode = module.encode();

    Ok(bytecode)
}
