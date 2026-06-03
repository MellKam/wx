use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self};
use indoc::indoc;

use super::*;

const STD: &str = indoc! {"
    trait PointerSize {}
    impl PointerSize for u32 {}
    impl PointerSize for u64 {}

    trait Memory {
        type Size: PointerSize;
        const MEMORY_INDEX: u32;

        fn grow(self, delta: Self::Size) -> Self::Size;
        fn size(self) -> Self::Size;
    }

    trait Memory32: Memory<Size = u32> {}
    trait Memory64: Memory<Size = u64> {}

    pub struct string {
        ptr: u32,
        len: u32,
    }
"};
use crate::{ast, mir, tir, vfs};

#[allow(unused)]
struct TestCase {
    interner: ast::StringInterner,
    graph: vfs::CompilationGraph,
    tir: tir::TIR,
    mir: mir::MIR,
    wasm: WasmModule,
    bytecode: Vec<u8>,
}

impl<'case> TestCase {
    fn new(source: &str) -> Self {
        let mut interner = ast::StringInterner::new();
        let graph = vfs::load_single_file_compilation(
            "main.wx".to_string(),
            source.to_string(),
            &mut interner,
        )
        .unwrap();
        let crate_graph = graph.crate_graph(graph.entry_crate);
        let ast = &crate_graph.modules[crate_graph.root.as_u32() as usize].ast;
        if ast.diagnostics.len() > 0
            && ast
                .diagnostics
                .iter()
                .any(|d| d.severity == codespan_reporting::diagnostic::Severity::Error)
        {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();

            for diagnostic in ast.diagnostics.iter() {
                term::emit_to_io_write(&mut writer.lock(), &config, &graph.files, diagnostic)
                    .unwrap();
            }
            std::process::exit(1);
        }
        let tir = tir::TIR::build(&graph, &mut interner);
        if tir.diagnostics.len() > 0
            && tir
                .diagnostics
                .iter()
                .any(|d| d.severity == codespan_reporting::diagnostic::Severity::Error)
        {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();

            for diagnostic in tir.diagnostics.iter() {
                term::emit_to_io_write(&mut writer.lock(), &config, &graph.files, diagnostic)
                    .unwrap();
            }
            std::process::exit(1);
        }
        // insta::assert_yaml_snapshot!(tir.functions);
        let mir = mir::MIR::build(&tir, &interner, graph.id_generator);
        // insta::assert_yaml_snapshot!(mir);
        let wasm = Builder::build(&mir, &interner).unwrap();
        let bytecode = wasm.encode();

        TestCase {
            interner,
            graph,
            tir,
            mir,
            wasm,
            bytecode,
        }
    }
}

#[test]
fn test_parse_simple_addition() {
    let case = TestCase::new(indoc! {"
        fn add(mut a: i32, b: i32) -> i32 { a += b; a }

        export { add }
    "});
    // insta::assert_yaml_snapshot!(case.bytecode);

    // Execute the wasm bytecode using wasmtime to verify it works
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).expect("Failed to create module");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance =
        wasmtime::Instance::new(&mut store, &module, &[]).expect("Failed to instantiate");

    let add = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "add")
        .expect("Failed to get add function");

    // Test: 5 + 3 = 8
    let result = add
        .call(&mut store, (5, 3))
        .expect("Failed to call add function");
    assert_eq!(result, 8, "add(5, 3) should return 8");

    // Test: 10 + 20 = 30
    let result = add
        .call(&mut store, (10, 20))
        .expect("Failed to call add function");
    assert_eq!(result, 30, "add(10, 20) should return 30");

    // Test: -5 + 3 = -2
    let result = add
        .call(&mut store, (-5, 3))
        .expect("Failed to call add function");
    assert_eq!(result, -2, "add(-5, 3) should return -2");
}

#[test]
fn test_arithmetic_operations() {
    let case = TestCase::new(indoc! {"
        fn sub(a: i32, b: i32) -> i32 { a - b }
        fn mul(a: i32, b: i32) -> i32 { a * b }
        fn div(a: i32, b: i32) -> i32 { a / b }
        fn rem(a: i32, b: i32) -> i32 { a % b }

        export {
            sub,
            mul,
            div,
            rem
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let sub = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "sub")
        .unwrap();
    assert_eq!(sub.call(&mut store, (10, 3)).unwrap(), 7);
    assert_eq!(sub.call(&mut store, (5, 10)).unwrap(), -5);

    let mul = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "mul")
        .unwrap();
    assert_eq!(mul.call(&mut store, (6, 7)).unwrap(), 42);
    assert_eq!(mul.call(&mut store, (-3, 4)).unwrap(), -12);

    let div = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "div")
        .unwrap();
    assert_eq!(div.call(&mut store, (20, 4)).unwrap(), 5);
    assert_eq!(div.call(&mut store, (15, 4)).unwrap(), 3);

    let rem = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "rem")
        .unwrap();
    assert_eq!(rem.call(&mut store, (10, 3)).unwrap(), 1);
    assert_eq!(rem.call(&mut store, (20, 7)).unwrap(), 6);
}

#[test]
fn test_comparison_operations() {
    let case = TestCase::new(indoc! {"
        fn lt(a: i32, b: i32) -> i32 {
            if a < b { 1 } else { 0 }
        }
        fn gt(a: i32, b: i32) -> i32 {
            if a > b { 1 } else { 0 }
        }
        fn eq(a: i32, b: i32) -> i32 {
            if a == b { 1 } else { 0 }
        }
        fn ne(a: i32, b: i32) -> i32 {
            if a != b { 1 } else { 0 }
        }

        export {
            lt,
            gt,
            eq,
            ne
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let lt = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "lt")
        .unwrap();
    assert_eq!(lt.call(&mut store, (5, 10)).unwrap(), 1);
    assert_eq!(lt.call(&mut store, (10, 5)).unwrap(), 0);

    let gt = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "gt")
        .unwrap();
    assert_eq!(gt.call(&mut store, (10, 5)).unwrap(), 1);
    assert_eq!(gt.call(&mut store, (5, 10)).unwrap(), 0);

    let eq = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "eq")
        .unwrap();
    assert_eq!(eq.call(&mut store, (5, 5)).unwrap(), 1);
    assert_eq!(eq.call(&mut store, (5, 10)).unwrap(), 0);

    let ne = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "ne")
        .unwrap();
    assert_eq!(ne.call(&mut store, (5, 10)).unwrap(), 1);
    assert_eq!(ne.call(&mut store, (5, 5)).unwrap(), 0);
}

#[test]
fn test_conditional_expression() {
    let case = TestCase::new(indoc! {"
        fn max(a: i32, b: i32) -> i32 {
            if a > b { a } else { b }
        }
        fn abs(a: i32) -> i32 {
            if a < 0 { -a } else { a }
        }

        export {
            max,
            abs
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let max = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "max")
        .unwrap();
    assert_eq!(max.call(&mut store, (5, 10)).unwrap(), 10);
    assert_eq!(max.call(&mut store, (10, 5)).unwrap(), 10);
    assert_eq!(max.call(&mut store, (7, 7)).unwrap(), 7);

    let abs = instance
        .get_typed_func::<i32, i32>(&mut store, "abs")
        .unwrap();
    assert_eq!(abs.call(&mut store, 5).unwrap(), 5);
    assert_eq!(abs.call(&mut store, -5).unwrap(), 5);
    assert_eq!(abs.call(&mut store, 0).unwrap(), 0);
}

#[test]
fn test_loops() {
    let case = TestCase::new(indoc! {"
        fn factorial(n: i32) -> i32 {
            local mut result: i32 = 1;
            local mut i: i32 = 1;
            loop {
                if i > n { break result };
                result *= i;
                i += 1;
            }
        }
        fn sum_to_n(n: i32) -> i32 {
            local mut sum: i32 = 0;
            local mut i: i32 = 1;
            loop {
                if i > n { break };
                sum += i;
                i += 1;
            };
            sum
        }

        export {
            factorial,
            sum_to_n
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let factorial = instance
        .get_typed_func::<i32, i32>(&mut store, "factorial")
        .unwrap();
    assert_eq!(factorial.call(&mut store, 5).unwrap(), 120);
    assert_eq!(factorial.call(&mut store, 6).unwrap(), 720);
    assert_eq!(factorial.call(&mut store, 0).unwrap(), 1);

    let sum_to_n = instance
        .get_typed_func::<i32, i32>(&mut store, "sum_to_n")
        .unwrap();
    assert_eq!(sum_to_n.call(&mut store, 10).unwrap(), 55);
    assert_eq!(sum_to_n.call(&mut store, 100).unwrap(), 5050);
}

#[test]
fn test_i64_operations() {
    let case = TestCase::new(indoc! {"
        fn add64(a: i64, b: i64) -> i64 { a + b }
        fn mul64(a: i64, b: i64) -> i64 { a * b }

        export {
            add64,
            mul64
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let add64 = instance
        .get_typed_func::<(i64, i64), i64>(&mut store, "add64")
        .unwrap();
    assert_eq!(
        add64.call(&mut store, (1000000000, 2000000000)).unwrap(),
        3000000000
    );
    assert_eq!(add64.call(&mut store, (-500, 1000)).unwrap(), 500);

    let mul64 = instance
        .get_typed_func::<(i64, i64), i64>(&mut store, "mul64")
        .unwrap();
    assert_eq!(
        mul64.call(&mut store, (1000000, 1000000)).unwrap(),
        1000000000000
    );
}

#[test]
fn test_f32_operations() {
    let case = TestCase::new(indoc! {"
        fn add_f32(a: f32, b: f32) -> f32 { a + b }
        fn mul_f32(a: f32, b: f32) -> f32 { a * b }
        fn div_f32(a: f32, b: f32) -> f32 { a / b }

        export {
            add_f32,
            mul_f32,
            div_f32
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let add_f32 = instance
        .get_typed_func::<(f32, f32), f32>(&mut store, "add_f32")
        .unwrap();
    assert!((add_f32.call(&mut store, (1.5, 2.5)).unwrap() - 4.0).abs() < 0.001);

    let mul_f32 = instance
        .get_typed_func::<(f32, f32), f32>(&mut store, "mul_f32")
        .unwrap();
    assert!((mul_f32.call(&mut store, (2.5, 4.0)).unwrap() - 10.0).abs() < 0.001);

    let div_f32 = instance
        .get_typed_func::<(f32, f32), f32>(&mut store, "div_f32")
        .unwrap();
    assert!((div_f32.call(&mut store, (10.0, 4.0)).unwrap() - 2.5).abs() < 0.001);
}

#[test]
fn test_f64_operations() {
    let case = TestCase::new(indoc! {"
        fn add_f64(a: f64, b: f64) -> f64 { a + b }
        fn sub_f64(a: f64, b: f64) -> f64 { a - b }

        export {
            add_f64,
            sub_f64
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let add_f64 = instance
        .get_typed_func::<(f64, f64), f64>(&mut store, "add_f64")
        .unwrap();
    assert!((add_f64.call(&mut store, (1.5, 2.5)).unwrap() - 4.0).abs() < 0.0001);

    let sub_f64 = instance
        .get_typed_func::<(f64, f64), f64>(&mut store, "sub_f64")
        .unwrap();
    assert!((sub_f64.call(&mut store, (10.5, 3.5)).unwrap() - 7.0).abs() < 0.0001);
}

#[test]
fn test_bitwise_operations() {
    let case = TestCase::new(indoc! {"
        fn bit_and(a: i32, b: i32) -> i32 { a & b }
        fn bit_or(a: i32, b: i32) -> i32 { a | b }
        fn bit_xor(a: i32, b: i32) -> i32 { a ^ b }
        fn left_shift(a: i32, b: i32) -> i32 { a << b }
        fn right_shift(a: i32, b: i32) -> i32 { a >> b }

        export {
            bit_and,
            bit_or,
            bit_xor,
            left_shift,
            right_shift
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let bit_and = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "bit_and")
        .unwrap();
    assert_eq!(bit_and.call(&mut store, (0b1100, 0b1010)).unwrap(), 0b1000);

    let bit_or = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "bit_or")
        .unwrap();
    assert_eq!(bit_or.call(&mut store, (0b1100, 0b1010)).unwrap(), 0b1110);

    let bit_xor = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "bit_xor")
        .unwrap();
    assert_eq!(bit_xor.call(&mut store, (0b1100, 0b1010)).unwrap(), 0b0110);

    let left_shift = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "left_shift")
        .unwrap();
    assert_eq!(left_shift.call(&mut store, (5, 2)).unwrap(), 20);

    let right_shift = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "right_shift")
        .unwrap();
    assert_eq!(right_shift.call(&mut store, (20, 2)).unwrap(), 5);
}

#[test]
fn test_logical_operations() {
    let case = TestCase::new(indoc! {"
        fn and(a: i32, b: i32) -> i32 { ((a != 0) && (b != 0)) as i32 }
        fn or(a: i32, b: i32) -> i32 { ((a != 0) || (b != 0)) as i32 }

        export { and, or }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let and = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "and")
        .unwrap();
    assert_eq!(and.call(&mut store, (1, 1)).unwrap(), 1);
    assert_eq!(and.call(&mut store, (1, 0)).unwrap(), 0);
    assert_eq!(and.call(&mut store, (0, 1)).unwrap(), 0);
    assert_eq!(and.call(&mut store, (0, 0)).unwrap(), 0);

    let or = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "or")
        .unwrap();
    assert_eq!(or.call(&mut store, (1, 1)).unwrap(), 1);
    assert_eq!(or.call(&mut store, (1, 0)).unwrap(), 1);
    assert_eq!(or.call(&mut store, (0, 1)).unwrap(), 1);
    assert_eq!(or.call(&mut store, (0, 0)).unwrap(), 0);
}

#[test]
fn test_global_variables() {
    let case = TestCase::new(indoc! {"
        global mut global_counter: i32 = 0

        fn increment() -> i32 {
            global_counter += 1;
            global_counter
        }

        fn get_counter() -> i32 {
            global_counter
        }

        export {
            increment,
            get_counter
        }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let increment = instance
        .get_typed_func::<(), i32>(&mut store, "increment")
        .unwrap();
    let get_counter = instance
        .get_typed_func::<(), i32>(&mut store, "get_counter")
        .unwrap();

    assert_eq!(get_counter.call(&mut store, ()).unwrap(), 0);
    assert_eq!(increment.call(&mut store, ()).unwrap(), 1);
    assert_eq!(increment.call(&mut store, ()).unwrap(), 2);
    assert_eq!(get_counter.call(&mut store, ()).unwrap(), 2);
    assert_eq!(increment.call(&mut store, ()).unwrap(), 3);
    assert_eq!(get_counter.call(&mut store, ()).unwrap(), 3);
}

#[test]
fn test_fibonacci() {
    let case = TestCase::new(indoc! {"
        fn fibonacci(n: i32) -> i32 {
            if n <= 1 { return n };
            local mut a: i32 = 0;
            local mut b: i32 = 1;
            local mut i: i32 = 2;
            loop {
                if i > n { break };
                local temp = a + b;
                a = b;
                b = temp;
                i += 1;
            };
            b
        }

        export { fibonacci }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let fibonacci = instance
        .get_typed_func::<i32, i32>(&mut store, "fibonacci")
        .unwrap();
    assert_eq!(fibonacci.call(&mut store, 0).unwrap(), 0);
    assert_eq!(fibonacci.call(&mut store, 1).unwrap(), 1);
    assert_eq!(fibonacci.call(&mut store, 2).unwrap(), 1);
    assert_eq!(fibonacci.call(&mut store, 3).unwrap(), 2);
    assert_eq!(fibonacci.call(&mut store, 4).unwrap(), 3);
    assert_eq!(fibonacci.call(&mut store, 5).unwrap(), 5);
    assert_eq!(fibonacci.call(&mut store, 10).unwrap(), 55);
}

#[test]
fn test_imports() {
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;

        import \"console\" {
            fn log(value: string) -> unit;
        }

        fn main() -> unit {
            local y = \"Hello World!\";
            local x = \"Hello World!\";
            console::log(x);
            console::log(y);
        }

        export { main, heap as \"memory\" }
    "}
    ));

    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut linker = wasmtime::Linker::new(&engine);

    linker
        .func_wrap(
            "console",
            "log",
            |mut caller: wasmtime::Caller<'_, ()>, ptr: i32, len: i32| {
                let memory = match caller.get_export("memory") {
                    Some(wasmtime::Extern::Memory(mem)) => mem,
                    _ => panic!("Failed to find memory export"),
                };
                let data = memory
                    .data(&caller)
                    .get(ptr as usize..(ptr + len) as usize)
                    .expect("Failed to read string from memory");
                let message = std::str::from_utf8(data).expect("Invalid UTF-8 string");
                println!("console.log: {}", message);
            },
        )
        .unwrap();

    let mut store = wasmtime::Store::new(&engine, ());
    let instance = linker.instantiate(&mut store, &module).unwrap();

    let main = instance
        .get_typed_func::<(), ()>(&mut store, "main")
        .unwrap();
    main.call(&mut store, ()).unwrap();
}

#[test]
fn test_dead_function_strings_excluded_from_data_section() {
    // String data from functions eliminated by DCE must not appear in the
    // wasm data section. The lazy string pool only adds strings when their
    // StringPointer instruction is actually emitted, so dead code can never
    // contribute bytes to the binary.
    let case = TestCase::new(&format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;

        import \"env\" {
            fn log(message: string);
        }

        fn live_fn() {
            env::log(\"this-string-must-appear\");
        }

        fn dead_fn() {
            env::log(\"this-string-must-not-appear\");
        }

        export { live_fn, heap }
    "}
    ));

    let bytecode = &case.bytecode;
    assert!(
        bytecode
            .windows(b"this-string-must-appear".len())
            .any(|w| w == b"this-string-must-appear"),
        "live string missing from bytecode"
    );
    assert!(
        !bytecode
            .windows(b"this-string-must-not-appear".len())
            .any(|w| w == b"this-string-must-not-appear"),
        "dead string should not appear in bytecode"
    );
}

// ── WAT snapshots
// ─────────────────────────────────────────────────────────────

#[test]
fn test_globals_wat() {
    // global.get / global.set should use the correct wasm indices.
    let case = TestCase::new(indoc! {"
        global mut counter: i32 = 0

        fn increment() -> i32 {
            counter += 1;
            counter
        }

        export { increment }
    "});
    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_inline_expansion_wat() {
    // An #[inline] function must be fully substituted into its caller —
    // the WAT must contain exactly one `func` and no `call` instruction.
    let case = TestCase::new(indoc! {"
        #[inline]
        fn double(x: i32) -> i32 { x * 2 }

        fn quad(x: i32) -> i32 { double(double(x)) }

        export { quad }
    "});
    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_struct_init_wat() {
    // StructCreate lowers to pushing each field value in declaration order.
    // The WAT must show the struct fields as multi-value results (both params
    // passed through as the return tuple).
    let case = TestCase::new(indoc! {"
        struct Point {
            x: i32,
            y: i32,
        }

        fn make_point(x: i32, y: i32) -> Point {
            Point::{ x: x, y: y }
        }

        export { make_point }
    "});
    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_struct_field_access_wat() {
    // A struct is flattened to individual wasm params; field access lowers to
    // local.get on the corresponding slot index.
    let case = TestCase::new(indoc! {"
        struct Point {
            x: i32,
            y: i32,
        }

        fn sum(p: Point) -> i32 {
            p.x + p.y
        }

        export { sum }
    "});
    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_non_inline_call_wat() {
    // A non-inline callee must appear as a separate `func` in the binary and
    // be referenced via a `call` instruction — not inlined.
    let case = TestCase::new(indoc! {"
        fn double(x: i32) -> i32 { x * 2 }

        fn apply_twice(x: i32) -> i32 { double(double(x)) }

        export { apply_twice }
    "});
    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_lerp() {
    let case = TestCase::new(indoc! {"
        fn lerp(a: f32, b: f32, t: f32) -> f32 {
            a + (b - a) * t
        }

        fn main() -> f32 {
            local x: f32 = lerp(0.0, 100.0, 0.5);
            if x != 50.0 { unreachable } else { x }
        }

        export { main }
    "});

    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let linker = wasmtime::Linker::new(&engine);

    let mut store = wasmtime::Store::new(&engine, ());
    let instance = linker.instantiate(&mut store, &module).unwrap();
    let main = instance
        .get_typed_func::<(), f32>(&mut store, "main")
        .unwrap();
    let result = main.call(&mut store, ()).unwrap();
    assert!(result == 50.0, "Expected main() to return 50.0");
}

// ── tuples ────────────────────────────────────────────────────────────────────

#[test]
fn test_tuple_return_wat() {
    // A function returning a tuple must produce a multi-value wasm signature
    // `(result i32 i32)` and the body must push both values.
    let case = TestCase::new(indoc! {"
        fn make_pair(a: i32, b: i32) -> (i32, i32) {
            (a, b)
        }

        export { make_pair }
    "});
    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_tuple_block_result_wat() {
    // A block whose result is a tuple must use a multi-value block type
    // referencing a type-section entry, not a single-value type.
    let case = TestCase::new(indoc! {"
        fn make_pair(x: i32) -> (i32, i32) {
            local t: (i32, i32) = {
                (x, x + 1)
            };
            t
        }

        export { make_pair }
    "});
    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

// ── traits ────────────────────────────────────────────────────────────────────

#[test]
fn test_trait_method_dispatch() {
    // Execution test: a method defined in an `impl Trait for Type` block is
    // callable and produces the correct result.
    let case = TestCase::new(indoc! {"
        trait Addable {
            fn add_one(self) -> i32;
        }

        struct Counter {
            value: i32,
        }

        impl Addable for Counter {
            fn add_one(self) -> i32 {
                self.value + 1
            }
        }

        fn run() -> i32 {
            local c = Counter::{ value: 41 };
            c.add_one()
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let run = instance
        .get_typed_func::<(), i32>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, ()).unwrap(), 42);
}

#[test]
fn test_trait_multiple_methods() {
    // A struct implementing a trait with two methods; both are callable and
    // produce the right results.
    let case = TestCase::new(indoc! {"
        trait Ops {
            fn double(self) -> i32;
            fn triple(self) -> i32;
        }

        struct Num {
            n: i32,
        }

        impl Ops for Num {
            fn double(self) -> i32 { self.n * 2 }
            fn triple(self) -> i32 { self.n * 3 }
        }

        fn run_double(n: i32) -> i32 {
            local x = Num::{ n: n };
            x.double()
        }

        fn run_triple(n: i32) -> i32 {
            local x = Num::{ n: n };
            x.triple()
        }

        export { run_double, run_triple }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let run_double = instance
        .get_typed_func::<i32, i32>(&mut store, "run_double")
        .unwrap();
    let run_triple = instance
        .get_typed_func::<i32, i32>(&mut store, "run_triple")
        .unwrap();

    assert_eq!(run_double.call(&mut store, 7).unwrap(), 14);
    assert_eq!(run_triple.call(&mut store, 7).unwrap(), 21);
}

#[test]
fn test_trait_associated_const() {
    // An associated constant declared in the trait and provided by the impl
    // must be accessible via `Type::CONST` syntax and produce the right value.
    let case = TestCase::new(indoc! {"
        trait Sized {
            const SIZE: u32;
        }

        struct Point {
            x: i32,
            y: i32,
        }

        impl Sized for Point {
            const SIZE: u32 = 8;
        }

        fn run() -> u32 {
            Point::SIZE
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let run = instance
        .get_typed_func::<(), u32>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, ()).unwrap(), 8);
}

#[test]
fn test_trait_default_method() {
    // A default method defined in the trait body calls another (abstract) method
    // on Self.  The default body must compile with `self` having the trait type,
    // and the conformance checker must NOT require the impl to provide it.

    let case = TestCase::new(indoc! {"
        trait Scalable {
            fn value(self) -> i32;
            fn doubled(self) -> i32 {
                self.value() * 2
            }
        }

        struct Num {
            n: i32,
        }

        impl Scalable for Num {
            fn value(self) -> i32 {
                self.n
            }
        }

        fn run() -> i32 {
            local x = Num::{ n: 21 };
            x.doubled()
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let run = instance
        .get_typed_func::<(), i32>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, ()).unwrap(), 42);
}

#[test]
fn test_tuple_roundtrip() {
    // Execution test: swap(3, 7) must return (7, 3).
    let case = TestCase::new(indoc! {"
        fn swap(a: i32, b: i32) -> (i32, i32) {
            (b, a)
        }

        export { swap }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let swap = instance
        .get_typed_func::<(i32, i32), (i32, i32)>(&mut store, "swap")
        .unwrap();
    assert_eq!(swap.call(&mut store, (3, 7)).unwrap(), (7, 3));
    assert_eq!(swap.call(&mut store, (0, 1)).unwrap(), (1, 0));
}

#[test]
fn test_generic_identity_monomorphized() {
    // identity<T>(t: T) -> T called with i32; the mono pass must emit a concrete
    // function and the export must return the passed value unchanged.
    let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T {
            t
        }

        fn run() -> i32 {
            identity(42)
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let run = instance
        .get_typed_func::<(), i32>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, ()).unwrap(), 42);
}

// ── aggregate call results
// ────────────────────────────────────────────────────

#[test]
fn test_struct_returned_from_call() {
    // Calls a wx function that returns a struct, then accesses fields.
    // Exercises AggregateCallResult: the multi-return values are captured into
    // per-field locals and read back via AggregateGet.
    let case = TestCase::new(indoc! {"
        struct Point {
            x: i32,
            y: i32,
        }

        fn translate(p: Point, dx: i32, dy: i32) -> Point {
            Point::{ x: p.x + dx, y: p.y + dy }
        }

        fn run() -> i32 {
            local p = Point::{ x: 3, y: 7 };
            local q = translate(p, 10, 20);
            q.x + q.y
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    let run = instance
        .get_typed_func::<(), i32>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, ()).unwrap(), 40); // (3+10) + (7+20)
}

#[test]
fn test_struct_chained_transforms() {
    // Two back-to-back calls each returning a struct; the second call receives
    // the first call's result as an argument.  Verifies that multiple independent
    // AggregateCallResult nodes don't clobber each other's captured locals.
    let case = TestCase::new(indoc! {"
        struct Vec2 {
            x: i32,
            y: i32,
        }

        fn scale(v: Vec2, factor: i32) -> Vec2 {
            Vec2::{ x: v.x * factor, y: v.y * factor }
        }

        fn run() -> i32 {
            local v = Vec2::{ x: 3, y: 4 };
            local v2 = scale(v, 2);
            local v3 = scale(v2, 3);
            v3.x + v3.y
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    let run = instance
        .get_typed_func::<(), i32>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, ()).unwrap(), 42); // (3*6) + (4*6)
}

#[test]
fn test_struct_in_conditional() {
    // An if-else expression whose both branches produce a struct.  The builder
    // merges the two Aggregate nodes field-by-field into Phi nodes; the
    // scheduler captures each branch's fields into phi locals.
    let case = TestCase::new(indoc! {"
        struct Point {
            x: i32,
            y: i32,
        }

        fn run(flag: i32) -> i32 {
            local p = if flag > 0 {
                Point::{ x: 10, y: 20 }
            } else {
                Point::{ x: 1, y: 2 }
            };
            p.x + p.y
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    let run = instance
        .get_typed_func::<i32, i32>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, 1).unwrap(), 30); // 10 + 20
    assert_eq!(run.call(&mut store, -1).unwrap(), 3); // 1 + 2
}

#[test]
fn test_struct_i64_fields() {
    // A struct with i64 fields passed through a call and returned.  Verifies
    // that type flattening uses I64 WASM locals throughout the pipeline.
    let case = TestCase::new(indoc! {"
        struct Stats {
            x: i64,
            y: i64,
        }

        fn add_stats(a: Stats, b: Stats) -> Stats {
            Stats::{ x: a.x + b.x, y: a.y + b.y }
        }

        fn run() -> i64 {
            local a = Stats::{ x: 10, y: 20 };
            local b = Stats::{ x: 30, y: 40 };
            local c = add_stats(a, b);
            c.x + c.y
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    let run = instance
        .get_typed_func::<(), i64>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, ()).unwrap(), 100); // (10+30) + (20+40)
}

#[test]
fn test_struct_call_result_wat() {
    // WAT snapshot for a function that takes a struct (as flattened params) and
    // returns a struct (as multi-value result).  Pins the WASM signature shape:
    // (param i32 i32 i32 i32) (result i32 i32).
    let case = TestCase::new(indoc! {"
        struct Point {
            x: i32,
            y: i32,
        }

        fn translate(p: Point, dx: i32, dy: i32) -> Point {
            Point::{ x: p.x + dx, y: p.y + dy }
        }

        export { translate }
    "});
    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

// ── Pointer dereference ──────────────────────────────────────────────────────

#[test]
fn test_pointer_deref_load_and_store() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32;

        fn read(ptr: heap::*i32) -> i32 {
            ptr.*
        }

        fn write(ptr: heap::*mut i32, val: i32) {
            ptr.* = val
        }

        export { heap, read, write }
    "}
    );
    let case = TestCase::new(&src);

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiation failed");

    let mem = instance
        .get_memory(&mut store, "heap")
        .expect("heap memory not exported");
    mem.grow(&mut store, 1).expect("grow failed"); // allocate 1 page (64 KiB)
    let read = instance
        .get_typed_func::<i32, i32>(&mut store, "read")
        .expect("read not found");
    let write = instance
        .get_typed_func::<(i32, i32), ()>(&mut store, "write")
        .expect("write not found");

    // Write 42 at byte address 0, read it back via the exported function.
    mem.write(&mut store, 0, &42i32.to_le_bytes()).unwrap();
    let val = read.call(&mut store, 0).expect("read failed");
    assert_eq!(val, 42);

    // Use the exported write to store 99 at byte address 8, verify via host.
    write.call(&mut store, (8, 99)).expect("write failed");
    let mut buf = [0u8; 4];
    mem.read(&mut store, 8, &mut buf).unwrap();
    assert_eq!(i32::from_le_bytes(buf), 99);
}

#[test]
fn test_pointer_deref_increment() {
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32 {
            min: 1
        }

        fn increment(ptr: heap::*mut i32) {
            ptr.* += 1
        }

        export { heap, increment }
    "}
    );
    let case = TestCase::new(&src);

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiation failed");

    let mem = instance
        .get_memory(&mut store, "heap")
        .expect("heap memory not exported");
    let increment = instance
        .get_typed_func::<i32, ()>(&mut store, "increment")
        .expect("increment not found");

    // Store 10 at address 0, call increment three times, expect 13.
    mem.write(&mut store, 0, &10i32.to_le_bytes()).unwrap();
    increment.call(&mut store, 0).unwrap();
    increment.call(&mut store, 0).unwrap();
    increment.call(&mut store, 0).unwrap();
    let mut buf = [0u8; 4];
    mem.read(&mut store, 0, &mut buf).unwrap();
    assert_eq!(i32::from_le_bytes(buf), 13);

    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_struct_pointer_load_and_store() {
    // Exercises struct-typed pointer loads and stores end-to-end.
    //
    // store_point: a struct write expands to one store per field; field y sits
    //   at base + 4 so its address is computed via i32.add.
    // load_x / load_y: the whole struct is loaded (one load per field), but
    //   only the requested field local is returned — the other is allocated but
    //   never read (no DCE yet).
    //
    // The wasmtime checks verify field layout from the host side (byte offsets)
    // and that individual field loads return the correct values.
    // The WAT snapshot pins the emitted instruction shape.
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32 {
            min: 1
        }

        struct Point {
            x: i32,
            y: i32,
        }

        fn store_point(ptr: heap::*mut Point, x: i32, y: i32) {
            ptr.* = Point::{ x: x, y: y }
        }

        fn load_x(ptr: heap::*Point) -> i32 {
            local p: Point = ptr.*;
            p.x
        }

        fn load_y(ptr: heap::*Point) -> i32 {
            local p: Point = ptr.*;
            p.y
        }

        export { heap, store_point, load_x, load_y }
    "}
    );
    let case = TestCase::new(&src);

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiation failed");

    let mem = instance
        .get_memory(&mut store, "heap")
        .expect("heap memory not exported");
    let store_point = instance
        .get_typed_func::<(i32, i32, i32), ()>(&mut store, "store_point")
        .expect("store_point not found");
    let load_x = instance
        .get_typed_func::<i32, i32>(&mut store, "load_x")
        .expect("load_x not found");
    let load_y = instance
        .get_typed_func::<i32, i32>(&mut store, "load_y")
        .expect("load_y not found");

    // Store Point{x:10, y:20} at byte address 0 via the wx function and verify
    // the physical layout from the host: x at offset 0, y at offset 4.
    store_point
        .call(&mut store, (0, 10, 20))
        .expect("store_point failed");

    let mut buf = [0u8; 4];
    mem.read(&mut store, 0, &mut buf).unwrap();
    assert_eq!(
        i32::from_le_bytes(buf),
        10,
        "x field should be at byte offset 0"
    );
    mem.read(&mut store, 4, &mut buf).unwrap();
    assert_eq!(
        i32::from_le_bytes(buf),
        20,
        "y field should be at byte offset 4"
    );

    // Load individual fields back via wx and confirm correct values.
    assert_eq!(load_x.call(&mut store, 0).expect("load_x failed"), 10);
    assert_eq!(load_y.call(&mut store, 0).expect("load_y failed"), 20);

    // Repeat at a non-zero base address (16) to exercise the i32.add offset
    // arithmetic for the y field.
    store_point
        .call(&mut store, (16, 42, 99))
        .expect("store_point failed");
    assert_eq!(load_x.call(&mut store, 16).expect("load_x failed"), 42);
    assert_eq!(load_y.call(&mut store, 16).expect("load_y failed"), 99);

    insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

// ── Generic structs ───────────────────────────────────────────────────────────

#[test]
fn test_generic_struct_f32_fields() {
    // Point<f32> must use F32 wasm types throughout. If codegen inherits I32
    // wasm value types from a sibling Point<i32> instantiation — or fails to
    // substitute the TypeParam before choosing the wasm value type — the f32
    // values would be bit-reinterpreted as integers and arithmetic would produce
    // garbage. Both instantiations coexist in the same module.
    let case = TestCase::new(indoc! {"
        struct Point<T> {
            x: T,
            y: T,
        }

        fn sum_f32(p: Point<f32>) -> f32 {
            p.x + p.y
        }

        fn sum_i32(p: Point<i32>) -> i32 {
            p.x + p.y
        }

        export { sum_f32, sum_i32 }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let sum_f32 = instance
        .get_typed_func::<(f32, f32), f32>(&mut store, "sum_f32")
        .unwrap();
    let result = sum_f32.call(&mut store, (1.5, 2.5)).unwrap();
    assert!(
        (result - 4.0).abs() < 0.001,
        "sum_f32(1.5, 2.5) expected 4.0, got {result}"
    );

    let sum_i32 = instance
        .get_typed_func::<(i32, i32), i32>(&mut store, "sum_i32")
        .unwrap();
    assert_eq!(sum_i32.call(&mut store, (3, 7)).unwrap(), 10);
}

#[test]
fn test_generic_struct_two_type_params() {
    // Pair<A, B> has two independent type parameters; codegen must assign the
    // correct wasm value type to each field slot independently. With A=i32 and
    // B=f32, if both slots collapse to the same wasm type the f32 result is
    // garbled.
    let case = TestCase::new(indoc! {"
        struct Pair<A, B> {
            first: A,
            second: B,
        }

        fn get_first(p: Pair<i32, f32>) -> i32 {
            p.first
        }

        fn get_second(p: Pair<i32, f32>) -> f32 {
            p.second
        }

        export { get_first, get_second }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let get_first = instance
        .get_typed_func::<(i32, f32), i32>(&mut store, "get_first")
        .unwrap();
    let get_second = instance
        .get_typed_func::<(i32, f32), f32>(&mut store, "get_second")
        .unwrap();

    assert_eq!(get_first.call(&mut store, (42, 1.5)).unwrap(), 42);
    let s = get_second.call(&mut store, (42, 1.5)).unwrap();
    assert!((s - 1.5).abs() < 0.001, "expected 1.5, got {s}");
}

#[test]
fn test_generic_struct_from_generic_function() {
    // A generic function constructs and returns a generic struct. The codegen
    // must use the monomorphized return type for the wasm multi-value signature.
    // Two instantiations (i32 and f32) coexist to catch aggregate index aliasing.
    let case = TestCase::new(indoc! {"
        struct Wrap<T> {
            value: T,
        }

        fn make_wrap<T>(v: T) -> Wrap<T> {
            Wrap::{ value: v }
        }

        fn run_i32() -> i32 {
            local w: Wrap<i32> = make_wrap(99);
            w.value
        }

        fn run_f32() -> f32 {
            local w: Wrap<f32> = make_wrap(3.5);
            w.value
        }

        export { run_i32, run_f32 }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let run_i32 = instance
        .get_typed_func::<(), i32>(&mut store, "run_i32")
        .unwrap();
    assert_eq!(run_i32.call(&mut store, ()).unwrap(), 99);

    let run_f32 = instance
        .get_typed_func::<(), f32>(&mut store, "run_f32")
        .unwrap();
    let r = run_f32.call(&mut store, ()).unwrap();
    assert!((r - 3.5).abs() < 0.001, "expected 3.5, got {r}");
}

#[test]
fn test_generic_struct_in_conditional() {
    // Both if/else branches produce a generic struct of the same instantiation.
    // Phi nodes for each field slot must use the correct wasm types. Tests both
    // an i32 instantiation and an f32 instantiation to catch type-confusion.
    let case = TestCase::new(indoc! {"
        struct Vec2<T> {
            x: T,
            y: T,
        }

        fn select_i32(flag: i32) -> i32 {
            local v: Vec2<i32> = if flag > 0 {
                Vec2::{ x: 10, y: 20 }
            } else {
                Vec2::{ x: 1, y: 2 }
            };
            v.x + v.y
        }

        fn select_f32(flag: i32) -> f32 {
            local v: Vec2<f32> = if flag > 0 {
                Vec2::{ x: 1.5, y: 2.5 }
            } else {
                Vec2::{ x: 0.5, y: 0.5 }
            };
            v.x + v.y
        }

        export { select_i32, select_f32 }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let select_i32 = instance
        .get_typed_func::<i32, i32>(&mut store, "select_i32")
        .unwrap();
    assert_eq!(select_i32.call(&mut store, 1).unwrap(), 30);
    assert_eq!(select_i32.call(&mut store, -1).unwrap(), 3);

    let select_f32 = instance
        .get_typed_func::<i32, f32>(&mut store, "select_f32")
        .unwrap();
    let r_true = select_f32.call(&mut store, 1).unwrap();
    let r_false = select_f32.call(&mut store, -1).unwrap();
    assert!((r_true - 4.0).abs() < 0.001, "expected 4.0, got {r_true}");
    assert!((r_false - 1.0).abs() < 0.001, "expected 1.0, got {r_false}");
}

#[test]
fn test_generic_struct_chained_calls() {
    // Two back-to-back calls each returning a generic struct; the second call
    // receives the first's result. Exercises that multiple independent
    // AggregateCallResult nodes for a generic aggregate don't clobber each
    // other's captured locals.
    let case = TestCase::new(indoc! {"
        struct Vec2<T> {
            x: T,
            y: T,
        }

        fn scale(v: Vec2<i32>, factor: i32) -> Vec2<i32> {
            Vec2::{ x: v.x * factor, y: v.y * factor }
        }

        fn run() -> i32 {
            local v: Vec2<i32> = Vec2::{ x: 3, y: 4 };
            local v2 = scale(v, 2);
            local v3 = scale(v2, 3);
            v3.x + v3.y
        }

        export { run }
    "});

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    let run = instance
        .get_typed_func::<(), i32>(&mut store, "run")
        .unwrap();
    assert_eq!(run.call(&mut store, ()).unwrap(), 42); // (3*6) + (4*6)
}

#[test]
fn test_generic_struct_pointer_load_store() {
    // Memory load/store via a pointer to a generic struct `heap::*mut Point<i32>`.
    // Codegen must emit the correct field offsets for the monomorphized aggregate
    // (x@0, y@4), going through the same path as the non-generic pointer tests.
    let src = format!(
        "{STD}\n{}",
        indoc! {"
        memory heap: Memory32 {
            min: 1
        }

        struct Point<T> {
            x: T,
            y: T,
        }

        fn store_pt(ptr: heap::*mut Point<i32>, x: i32, y: i32) {
            ptr.* = Point::{ x: x, y: y }
        }

        fn load_x(ptr: heap::*Point<i32>) -> i32 {
            local p = ptr.*;
            p.x
        }

        fn load_y(ptr: heap::*Point<i32>) -> i32 {
            local p = ptr.*;
            p.y
        }

        export { heap, store_pt, load_x, load_y }
    "}
    );
    let case = TestCase::new(&src);

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instantiation failed");

    let mem = instance
        .get_memory(&mut store, "heap")
        .expect("heap not exported");
    let store_pt = instance
        .get_typed_func::<(i32, i32, i32), ()>(&mut store, "store_pt")
        .expect("store_pt not found");
    let load_x = instance
        .get_typed_func::<i32, i32>(&mut store, "load_x")
        .expect("load_x not found");
    let load_y = instance
        .get_typed_func::<i32, i32>(&mut store, "load_y")
        .expect("load_y not found");

    store_pt.call(&mut store, (0, 7, 13)).unwrap();
    assert_eq!(load_x.call(&mut store, 0).unwrap(), 7);
    assert_eq!(load_y.call(&mut store, 0).unwrap(), 13);

    // Verify byte layout: x at offset 0, y at offset 4.
    let mut buf = [0u8; 4];
    mem.read(&mut store, 0, &mut buf).unwrap();
    assert_eq!(i32::from_le_bytes(buf), 7, "x should be at byte offset 0");
    mem.read(&mut store, 4, &mut buf).unwrap();
    assert_eq!(i32::from_le_bytes(buf), 13, "y should be at byte offset 4");

    // Non-zero base address exercises the i32.add offset arithmetic.
    store_pt.call(&mut store, (16, 42, 99)).unwrap();
    assert_eq!(load_x.call(&mut store, 16).unwrap(), 42);
    assert_eq!(load_y.call(&mut store, 16).unwrap(), 99);
}
