use string_interner::StringInterner;
use wasm_bindgen::prelude::*;
use wx_compiler::*;

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> Result<Vec<u8>, JsValue> {
    let mut interner = StringInterner::new();
    let mut files = wx_compiler::ast::Files::new();
    let main_file = files.add(filename.clone(), source).unwrap();
    let ast = ast::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );
    if !ast.diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&ast.diagnostics).unwrap());
    }
    let hir = tir::TIR::build(&ast, &mut interner);
    if !hir.diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&hir.diagnostics).unwrap());
    }
    let mir = mir::MIR::build(&hir, &interner);
    let module = codegen::Builder::build(&mir, &interner).unwrap();
    let bytecode = module.encode();

    Ok(bytecode)
}
