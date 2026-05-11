use string_interner::StringInterner;
use wasm_bindgen::prelude::*;
use wx_compiler::*;

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> Result<Vec<u8>, JsValue> {
    let mut interner = StringInterner::new();
    let mut files = wx_compiler::ast::Files::new();

    let stdlib_id = files
        .add(wx_compiler::STDLIB_FILENAME.to_string(), wx_compiler::STDLIB_SOURCE.to_string())
        .unwrap();
    let stdlib_ast = ast::Parser::parse(
        stdlib_id,
        &files.get(stdlib_id).unwrap().source,
        &mut interner,
    );

    let main_file = files.add(filename.clone(), source).unwrap();
    let ast = ast::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );
    if !ast.diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&ast.diagnostics).unwrap());
    }
    let hir = tir::TIR::build(&[&stdlib_ast, &ast], &mut interner);
    if !hir.diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&hir.diagnostics).unwrap());
    }
    let mir = mir::MIR::build(&hir, &interner);
    let module = codegen::Builder::build(&mir, &interner).unwrap();
    let bytecode = module.encode();

    Ok(bytecode)
}
