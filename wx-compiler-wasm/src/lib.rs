use string_interner::StringInterner;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> Option<Vec<u8>> {
    let mut interner = StringInterner::new();
    let mut files = wx_compiler::files::Files::new();
    let main_file = files.add(filename.clone(), source).unwrap();
    let result = wx_compiler::compile_module(&mut interner, &files, main_file);
    let module = match result.module {
        Some(module) => module,
        None => panic!("compilation error"),
    };

    let bytecode = wx_compiler::encode_module(&module);
    Some(bytecode)
}
