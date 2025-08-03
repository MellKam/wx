use codespan_reporting::diagnostic::{Diagnostic, Severity};
use serde::Serialize;
use string_interner::StringInterner;
use wasm_bindgen::prelude::*;
use wx_compiler::{files::FileId, *};

#[derive(Serialize)]
struct CompilationResult {
    bytecode: Option<Vec<u8>>,
    diagnostics: Vec<Diagnostic<FileId>>,
}

#[wasm_bindgen]
pub fn compile(filename: String, source: String) -> JsValue {
    let mut interner = StringInterner::new();
    let mut files = wx_compiler::files::Files::new();
    let main_file = files.add(filename.clone(), source).unwrap();
    let ast = ast::parser::Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );

    if ast
        .diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == Severity::Error)
    {
        return serde_wasm_bindgen::to_value(&CompilationResult {
            bytecode: None,
            diagnostics: ast.diagnostics,
        })
        .unwrap();
    }

    let hir = hir::Builder::build(&ast.ast, &mut interner);

    if hir
        .diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == Severity::Error)
    {
        return serde_wasm_bindgen::to_value(&CompilationResult {
            bytecode: None,
            diagnostics: ast.diagnostics.into_iter().chain(hir.diagnostics).collect(),
        })
        .unwrap();
    }

    let mir = mir::Builder::build(&hir.hir);
    let module = wasm::Builder::build(&mir).unwrap();
    let bytecode = wasm::Encoder::encode(&module, &interner);

    serde_wasm_bindgen::to_value(&CompilationResult {
        bytecode: Some(bytecode),
        diagnostics: ast.diagnostics.into_iter().chain(hir.diagnostics).collect(),
    })
    .unwrap()
}
