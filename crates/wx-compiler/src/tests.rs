use super::*;
use indoc::indoc;
use insta::{assert_binary_snapshot, assert_yaml_snapshot};
use string_interner::{StringInterner, backend::StringBackend};

#[allow(unused)]
struct TestCase {
    files: files::Files,
    interner: StringInterner<StringBackend>,
    ast: ast::parser::ParserResult,
    hir: hir::builder::BuilderResult,
    mir: mir::MIR,
    module: wasm::Module,
    bytecode: Vec<u8>,
}

impl TestCase {
    fn assert_snapshot(name: &str, source: &str) {
        let mut interner = StringInterner::new();
        let mut files = files::Files::new();
        let file_id = files
            .add(name.to_string() + ".wx", source.to_string())
            .unwrap();

        let ast =
            ast::parser::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
        assert_yaml_snapshot!(name.to_string() + "_ast", ast);

        if ast.diagnostics.len() > 0 {
            return;
        }

        let hir = hir::Builder::build(&ast.ast, &mut interner);
        assert_yaml_snapshot!(name.to_string() + "_hir", hir);

        let mir = mir::Builder::build(&hir.hir);
        assert_yaml_snapshot!(name.to_string() + "_mir", mir);

        let module = wasm::Builder::build(&mir).unwrap();
        assert_yaml_snapshot!(name.to_string() + "_wasm", module);

        let bytecode = wasm::Encoder::encode(&module, &interner);
        assert_binary_snapshot!((name.to_string() + ".wasm").as_ref(), bytecode.clone());
    }
}

#[test]
fn factorial() {
    TestCase::assert_snapshot(
        "factorial",
        indoc! {"
        export func factorial(n: i32): i32 {
            if n <= 1 { return 1 };
            n * factorial(n - 1)
        }
    "},
    );
}

#[test]
fn pow() {
    TestCase::assert_snapshot(
        "pow",
        indoc! {"
        export func pow(base: i32, mut exp: i32): i32 {
            if exp < 0 {
                return 0; 
            };
            
            local mut result: i32 = 1;
            loop {
                if exp == 0 {
                break result; 
                };
                
                result *= base;
                exp -= 1;
            }
        }
    "},
    );
}

#[test]
fn func_pointers() {
    TestCase::assert_snapshot(
        "func_pointers",
        indoc! {"
        func add(a: i32, b: i32): i32 { a + b }
        func sub(a: i32, b: i32): i32 { a - b }

        func apply(
            binop: func(i32, i32) -> i32, 
            a: i32, 
            b: i32,
        ): i32 {
            binop(a, b)
        }

        export func main(): i32 {
            local a = apply(add, 5, 10);
            local b = apply(sub, 10, 5);
            local c: i32 = 5;

            a + b
        }
    "},
    );
}
