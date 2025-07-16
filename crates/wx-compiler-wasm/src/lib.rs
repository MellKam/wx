use string_interner::StringInterner;
use wasm_bindgen::prelude::*;
use wx_compiler::*;

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> Result<Vec<u8>, JsValue> {
    let mut interner = StringInterner::new();
    let mut files = wx_compiler::files::Files::new();
    let main_file = files.add(filename.clone(), source).unwrap();
    let ast = ast::parser::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );
    if !ast.diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&ast.diagnostics).unwrap());
    }
    let hir = hir::Builder::build(&ast.ast, &mut interner);
    if !hir.diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&hir.diagnostics).unwrap());
    }
    let mir = mir::Builder::build(&hir.hir);
    let module = wasm::Builder::build(&mir).unwrap();
    let bytecode = wasm::Encoder::encode(&module, &interner);

    Ok(bytecode)
}
