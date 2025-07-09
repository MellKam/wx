use string_interner::StringInterner;
use wasm_bindgen::prelude::*;
use wx_compiler::*;

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> Result<Vec<u8>, JsValue> {
    let mut interner = StringInterner::new();
    let mut files = wx_compiler::files::Files::new();
    let main_file = files.add(filename.clone(), source).unwrap();
    let (ast, diagnostics) = ast::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );
    if !diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&diagnostics).unwrap());
    }
    let (hir, diagnostics) = hir::Builder::build(&ast, &mut interner);
    if !diagnostics.is_empty() {
        return Err(serde_wasm_bindgen::to_value(&diagnostics).unwrap());
    }
    let mir = mir::Builder::build(&hir);
    let module = wasm::Builder::build(&mir, &mut interner).unwrap();
    let bytecode = wasm::Encoder::encode(&module);

    Ok(bytecode)
}
