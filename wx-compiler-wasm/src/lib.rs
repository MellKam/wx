use string_interner::StringInterner;
use wasm_bindgen::prelude::*;
use wx_compiler::*;

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> Option<Vec<u8>> {
    let mut interner = StringInterner::new();
    let mut files = wx_compiler::files::Files::new();
    let main_file = files.add(filename.clone(), source).unwrap();
    let (ast, _) = ast::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );
    let (hir, _) = hir::Builder::build(&ast, &mut interner);
    let mir = mir::Builder::build(&hir);
    let module = wasm::Builder::build(&mir, &mut interner);
    let bytecode = wasm::Encoder::encode(&module);

    Some(bytecode)
}
