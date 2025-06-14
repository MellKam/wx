use ast::Parser;
use codespan_reporting::diagnostic::Diagnostic;
use files::Files;
use string_interner::StringInterner;
use string_interner::backend::StringBackend;

use crate::files::FileId;

mod ast;
pub mod files;
mod hir;
mod mir;
mod span;
mod wasm;

pub struct CompilationResult<'a> {
    pub module: Option<wasm::Module<'a>>,
    pub diagnostics: Vec<Diagnostic<FileId>>,
}

pub fn compile_module<'interner>(
    mut interner: &'interner mut StringInterner<StringBackend>,
    files: &Files,
    main_file: FileId,
) -> CompilationResult<'interner> {
    let (ast, diagnostics) = Parser::parse(
        main_file,
        &files.get(main_file).unwrap().source,
        &mut interner,
    );

    if !diagnostics.is_empty() {
        return CompilationResult {
            module: None,
            diagnostics,
        };
    }

    let (hir, diagnostics) = hir::Builder::build(&ast, &mut interner);
    if !diagnostics.is_empty() {
        return CompilationResult {
            module: None,
            diagnostics,
        };
    }

    let mir = mir::Builder::build(&hir);
    let module = wasm::Builder::build(&mir, interner);

    CompilationResult {
        module: Some(module),
        diagnostics: Vec::new(),
    }
}

pub fn encode_module<'interner>(module: &wasm::Module<'interner>) -> Vec<u8> {
    wasm::Encoder::encode(module)
}

pub fn encode_module_to_wat<'interner>(module: &wasm::Module<'interner>) -> String {
    module.to_wat()
}
