use std::collections::HashMap;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self};
use indoc::indoc;

use super::*;

use crate::{mir, tir, vfs};

#[allow(unused)]
struct TestCase {
	graph: vfs::CompilationGraph,
	tir: tir::TIR,
	mir: mir::MIR,
	wasm: WasmModule,
	bytecode: Vec<u8>,
}

impl<'case> TestCase {
	fn new(source: &str) -> Self {
		let mut builder = vfs::CompilationGraphBuilder::new();
		let stdlib_id = builder.load_stdlib();
		let prefixed = format!("use std::*;\n{source}");
		let root_id = builder
			.load_binary(
				"main.wx".to_string(),
				&vfs::VirtualFileSource::new(HashMap::from([(
					"main.wx".to_string(),
					prefixed,
				)])),
			)
			.unwrap();
		let mut graph = builder.build(root_id, stdlib_id);
		let root_crate = &graph.crates[root_id.as_usize()];
		if root_crate.diagnostics.iter().any(|d| {
			d.severity == codespan_reporting::diagnostic::Severity::Error
		}) {
			let writer = StandardStream::stderr(ColorChoice::Always);
			let config = codespan_reporting::term::Config::default();
			for diagnostic in root_crate.diagnostics.iter() {
				term::emit_to_io_write(
					&mut writer.lock(),
					&config,
					&graph.files,
					diagnostic,
				)
				.unwrap();
			}
			std::process::exit(1);
		}
		let tir = tir::TIR::build(&mut graph);
		if tir.diagnostics.iter().any(|d| {
			d.severity == codespan_reporting::diagnostic::Severity::Error
		}) {
			let writer = StandardStream::stderr(ColorChoice::Always);
			let config = codespan_reporting::term::Config::default();
			for diagnostic in tir.diagnostics.iter() {
				term::emit_to_io_write(
					&mut writer.lock(),
					&config,
					&graph.files,
					diagnostic,
				)
				.unwrap();
			}
			std::process::exit(1);
		}
		let mir = mir::MIR::build(&tir, &graph.interner, graph.id_generator);
		let wasm = Builder::build(&mir, &graph.interner).unwrap();
		let bytecode = wasm.encode();

		TestCase {
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
	let module = wasmtime::Module::new(&engine, &case.bytecode)
		.expect("Failed to create module");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("Failed to instantiate");

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
	assert!(
		(add_f32.call(&mut store, (1.5, 2.5)).unwrap() - 4.0).abs() < 0.001
	);

	let mul_f32 = instance
		.get_typed_func::<(f32, f32), f32>(&mut store, "mul_f32")
		.unwrap();
	assert!(
		(mul_f32.call(&mut store, (2.5, 4.0)).unwrap() - 10.0).abs() < 0.001
	);

	let div_f32 = instance
		.get_typed_func::<(f32, f32), f32>(&mut store, "div_f32")
		.unwrap();
	assert!(
		(div_f32.call(&mut store, (10.0, 4.0)).unwrap() - 2.5).abs() < 0.001
	);
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
	assert!(
		(add_f64.call(&mut store, (1.5, 2.5)).unwrap() - 4.0).abs() < 0.0001
	);

	let sub_f64 = instance
		.get_typed_func::<(f64, f64), f64>(&mut store, "sub_f64")
		.unwrap();
	assert!(
		(sub_f64.call(&mut store, (10.5, 3.5)).unwrap() - 7.0).abs() < 0.0001
	);
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
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };

        import \"console\" as console {
            fn log(value: []u8) -> ();
        }

        fn main() {
            local y = \"Hello World!\";
            local x = \"Hello World!\";
            console::log(x);
            console::log(y);
        }

        export { main, heap as \"memory\" }
    "});

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
				let message =
					std::str::from_utf8(data).expect("Invalid UTF-8 string");
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
	// wasm data section. The static layout step only collects entries from
	// live functions, so dead code never contributes bytes to the binary.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };

        import \"env\" as env {
            fn log(message: []u8);
        }

        fn live_fn() {
            env::log(\"this-string-must-appear\");
        }

        fn dead_fn() {
            env::log(\"this-string-must-not-appear\");
        }

        export { live_fn, heap }
    "});

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

// ── global initializer execution
// ─────────────────────────────────────────────────────────

#[test]
fn test_global_init_constant_executes() {
	// Zero-init in the global section + start function assignment must produce
	// the declared value, not the WASM zero default.
	let case = TestCase::new(indoc! {"
        global mut x: i32 = 42
        fn get() -> i32 { x }
        export { get }
    "});
	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
	let get = instance
		.get_typed_func::<(), i32>(&mut store, "get")
		.unwrap();
	assert_eq!(get.call(&mut store, ()).unwrap(), 42);
}

#[test]
fn test_global_init_arithmetic_executes() {
	let case = TestCase::new(indoc! {"
        global mut x: i32 = 2 + 3
        fn get() -> i32 { x }
        export { get }
    "});
	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
	let get = instance
		.get_typed_func::<(), i32>(&mut store, "get")
		.unwrap();
	assert_eq!(get.call(&mut store, ()).unwrap(), 5);
}

#[test]
fn test_global_init_function_call_executes() {
	let case = TestCase::new(indoc! {"
        fn compute() -> i32 { 7 as i32 }
        global mut x: i32 = compute()
        fn get() -> i32 { x }
        export { get }
    "});
	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
	let get = instance
		.get_typed_func::<(), i32>(&mut store, "get")
		.unwrap();
	assert_eq!(get.call(&mut store, ()).unwrap(), 7);
}

#[test]
fn test_global_init_block_with_locals_executes() {
	let case = TestCase::new(indoc! {"
        global mut x: i32 = {
            local a = 3 as i32;
            local b = 4 as i32;
            a * b
        }
        fn get() -> i32 { x }
        export { get }
    "});
	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
	let get = instance
		.get_typed_func::<(), i32>(&mut store, "get")
		.unwrap();
	assert_eq!(get.call(&mut store, ()).unwrap(), 12);
}

#[test]
fn test_global_init_multiple_sequential_executes() {
	// g2 is declared after g1: when g2's initializer runs, g1 already holds 10.
	let case = TestCase::new(indoc! {"
        global mut g1: i32 = 10
        global mut g2: i32 = g1 + 1
        fn get_g1() -> i32 { g1 }
        fn get_g2() -> i32 { g2 }
        export { get_g1, get_g2 }
    "});
	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
	let get_g1 = instance
		.get_typed_func::<(), i32>(&mut store, "get_g1")
		.unwrap();
	let get_g2 = instance
		.get_typed_func::<(), i32>(&mut store, "get_g2")
		.unwrap();
	assert_eq!(get_g1.call(&mut store, ()).unwrap(), 10);
	assert_eq!(get_g2.call(&mut store, ()).unwrap(), 11);
}

#[test]
fn test_global_init_reverse_order_sees_zero() {
	// g2 is declared before g1: when g2 is initialized in the start function,
	// g1 is still the WASM zero-default (0), so g2 = 0 + 1 = 1, not 10 + 1 = 11.
	// Both use non-literal initializers so both are emitted into the start function
	// in declaration order.
	let case = TestCase::new(indoc! {"
        fn ten() -> i32 { 10 }
        global mut g2: i32 = g1 + 1
        global mut g1: i32 = ten()
        fn get_g1() -> i32 { g1 }
        fn get_g2() -> i32 { g2 }
        export { get_g1, get_g2 }
    "});
	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
	let get_g1 = instance
		.get_typed_func::<(), i32>(&mut store, "get_g1")
		.unwrap();
	let get_g2 = instance
		.get_typed_func::<(), i32>(&mut store, "get_g2")
		.unwrap();
	assert_eq!(get_g1.call(&mut store, ()).unwrap(), 10);
	assert_eq!(get_g2.call(&mut store, ()).unwrap(), 1); // saw g1 == 0
}

#[test]
fn test_global_init_f64_executes() {
	let case = TestCase::new(indoc! {"
        global mut f: f64 = 1.0 + 2.5
        fn get() -> f64 { f }
        export { get }
    "});
	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
	let get = instance
		.get_typed_func::<(), f64>(&mut store, "get")
		.unwrap();
	assert_eq!(get.call(&mut store, ()).unwrap(), 3.5);
}

#[test]
fn test_global_init_if_expression_executes() {
	let case = TestCase::new(indoc! {"
        fn flag() -> bool { true }
        global mut x: i32 = if flag() { 100 as i32 } else { 200 as i32 }
        fn get() -> i32 { x }
        export { get }
    "});
	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
	let get = instance
		.get_typed_func::<(), i32>(&mut store, "get")
		.unwrap();
	assert_eq!(get.call(&mut store, ()).unwrap(), 100);
}

#[test]
fn test_global_init_generic_null_pointer_executes() {
	// global mut head: heap::*Node = ptr::null()
	// null() is a generic function: null<M: Memory, T>() -> M::*T
	// Type params must be inferred from the global's declared type.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 }
        struct Node { x: i32 }
        global mut head: heap::*Node = ptr::null()
        fn get_head() -> u32 { head as u32 }
        export { get_head }
    "});
	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");
	let get_head = instance
		.get_typed_func::<(), i32>(&mut store, "get_head")
		.unwrap();
	assert_eq!(get_head.call(&mut store, ()).unwrap(), 0);
}

#[test]
fn test_global_immutable_with_init_wat() {
	// An immutable global with a non-trivial initializer goes through the start
	// function, which calls global.set.  WASM forbids global.set on an immutable
	// global, so this snapshot documents the currently-generated (invalid) output.
	// TODO: either force WASM-mutable for all initialised globals, or reject
	//       immutable globals with non-const initialisers at compile time.
	let case = TestCase::new(indoc! {"
        global x: i32 = 42
        fn get() -> i32 { x }
        export { get }
    "});
	insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
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
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn read(ptr: heap::*i32) -> i32 {
            ptr.*
        }

        fn write(ptr: heap::*mut i32, val: i32) {
            ptr.* = val
        }

        export { heap, read, write }
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

	let mem = instance
		.get_memory(&mut store, "heap")
		.expect("heap memory not exported");
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
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1
        }

        fn increment(ptr: heap::*mut i32) {
            ptr.* += 1
        }

        export { heap, increment }
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

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
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1
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
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

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

#[test]
fn test_struct_field_write_through_pointer() {
	// `ptr.*.field = val` — field write through a mutable pointer.
	// Uses the byte-offset PointerStore path, not whole-struct assignment.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 }
        struct Point { x: i32, y: i32 }
        fn set_x(ptr: heap::*mut Point, v: i32) { ptr.*.x = v }
        fn set_y(ptr: heap::*mut Point, v: i32) { ptr.*.y = v }
        fn get_x(ptr: heap::*Point) -> i32 { ptr.*.x }
        fn get_y(ptr: heap::*Point) -> i32 { ptr.*.y }
        export { heap, set_x, set_y, get_x, get_y }
    "});
	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");
	let set_x = instance
		.get_typed_func::<(i32, i32), ()>(&mut store, "set_x")
		.unwrap();
	let set_y = instance
		.get_typed_func::<(i32, i32), ()>(&mut store, "set_y")
		.unwrap();
	let get_x = instance
		.get_typed_func::<i32, i32>(&mut store, "get_x")
		.unwrap();
	let get_y = instance
		.get_typed_func::<i32, i32>(&mut store, "get_y")
		.unwrap();
	set_x.call(&mut store, (0, 42)).unwrap();
	set_y.call(&mut store, (0, 99)).unwrap();
	assert_eq!(get_x.call(&mut store, 0).unwrap(), 42);
	assert_eq!(get_y.call(&mut store, 0).unwrap(), 99);
}

#[test]
fn test_local_struct_field_assignment() {
	// `local mut s.field = val` — AggregateSet path: reconstruct struct with one field replaced.
	let case = TestCase::new(indoc! {"
        struct Point { x: i32, y: i32 }
        fn make(x: i32, y: i32) -> Point {
            local mut p = Point::{ x: 0, y: 0 };
            p.x = x;
            p.y = y;
            p
        }
        fn get_x(p: Point) -> i32 { p.x }
        fn get_y(p: Point) -> i32 { p.y }
        export { make, get_x, get_y }
    "});
	assert!(case.tir.diagnostics.is_empty());
	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");
	let make = instance
		.get_typed_func::<(i32, i32), (i32, i32)>(&mut store, "make")
		.unwrap();
	let (x, y) = make.call(&mut store, (7, 13)).unwrap();
	assert_eq!(x, 7);
	assert_eq!(y, 13);
}

// ── Generic structs
// ───────────────────────────────────────────────────────────

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
	// Memory load/store via a pointer to a generic struct `heap::*Point<i32>`.
	// Codegen must emit the correct field offsets for the monomorphized aggregate
	// (x@0, y@4), going through the same path as the non-generic pointer tests.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1
        }

        struct Point<T> {
            x: T,
            y: T,
        }

        fn store_pt(ptr: heap::*mut Point<i32>, x: i32, y: i32) {
            ptr.* = Point::{ x, y }
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
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

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

// ── memory intrinsics
// ─────────────────────────────────────────────────────────

#[test]
fn test_memory_grow_and_size() {
	// memory.size() returns the current page count; memory.grow(n) returns the
	// old page count and extends by n pages. Both route through @memory_size /
	// @memory_grow intrinsics via the Memory trait default methods.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn size_pages() -> u32 {
            heap.size()
        }

        fn grow_by(delta: u32) -> u32 {
            heap.grow(delta)
        }

        export { heap, size_pages, grow_by }
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

	let size_pages = instance
		.get_typed_func::<(), u32>(&mut store, "size_pages")
		.expect("size_pages not found");
	let grow_by = instance
		.get_typed_func::<u32, u32>(&mut store, "grow_by")
		.expect("grow_by not found");

	// Initial size is 1 page (64 KB).
	assert_eq!(size_pages.call(&mut store, ()).unwrap(), 1);

	// Grow by 2; memory.grow returns the *previous* page count.
	let old_size = grow_by.call(&mut store, 2).unwrap();
	assert_eq!(old_size, 1, "grow should return the old page count");

	// New size is 3 pages.
	assert_eq!(size_pages.call(&mut store, ()).unwrap(), 3);
}

#[test]
fn test_memory_size_before_grow_ordering() {
	// Captures memory.size() BEFORE memory.grow(), then returns the captured value.
	// If MemorySize is treated as a floating data node, the scheduler may emit
	// memory.size after memory.grow, returning 2 instead of 1.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn capture_size_before_grow() -> u32 {
            local before = heap.size();
            _ = heap.grow(1);
            before
        }

        export { capture_size_before_grow }
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

	let f = instance
		.get_typed_func::<(), u32>(&mut store, "capture_size_before_grow")
		.expect("function not found");

	// memory starts at 1 page; size() captured before grow(1) should be 1, not 2.
	assert_eq!(
		f.call(&mut store, ()).unwrap(),
		1,
		"size was captured after grow — ordering bug!"
	);
}

// ── arrays ────────────────────────────────────────────────────────────────────

#[test]
fn test_array_literal_read_by_index() {
	// End-to-end: a static array literal is placed in the data segment at
	// address 0; indexing it must return the right element via i32.load at
	// base + i * elem_size.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn get(i: u32) -> i32 {
            local arr: heap::[4]i32 = [10, 20, 30, 40];
            arr[i]
        }

        export { get }
    "});

	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

	let get = instance
		.get_typed_func::<u32, i32>(&mut store, "get")
		.unwrap();
	assert_eq!(get.call(&mut store, 0).unwrap(), 10);
	assert_eq!(get.call(&mut store, 1).unwrap(), 20);
	assert_eq!(get.call(&mut store, 2).unwrap(), 30);
	assert_eq!(get.call(&mut store, 3).unwrap(), 40);
}

#[test]
fn test_array_write_and_read_back() {
	// Writing to a[i] and reading a[j] on a mutable heap array.  The host
	// initialises a 4-element block at address 0, then the wx functions do
	// a round-trip write+read to verify PointerStore/PointerLoad addressing.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn write(arr: [4]mut i32, i: u32, v: i32) { arr[i] = v; }
        fn read(arr: [4]i32, i: u32) -> i32 { arr[i] }

        export { heap, write, read }
    "});

	let engine = wasmtime::Engine::default();
	let module = wasmtime::Module::new(&engine, &case.bytecode).unwrap();
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

	let heap = instance.get_memory(&mut store, "heap").unwrap();
	// No static data in this program — address 0 is free.
	let initial: &[u8] = &[
		1u8, 0, 0, 0, // [0] = 1
		2, 0, 0, 0, // [1] = 2
		3, 0, 0, 0, // [2] = 3
		4, 0, 0, 0, // [3] = 4
	];
	heap.write(&mut store, 0, initial).unwrap();

	let read = instance
		.get_typed_func::<(i32, u32), i32>(&mut store, "read")
		.unwrap();
	let write = instance
		.get_typed_func::<(i32, u32, i32), ()>(&mut store, "write")
		.unwrap();

	assert_eq!(read.call(&mut store, (0, 0)).unwrap(), 1);
	assert_eq!(read.call(&mut store, (0, 3)).unwrap(), 4);

	// Overwrite index 1 and confirm the change.
	write.call(&mut store, (0, 1, 99)).unwrap();
	assert_eq!(read.call(&mut store, (0, 1)).unwrap(), 99);

	// Ensure adjacent elements are untouched.
	assert_eq!(read.call(&mut store, (0, 0)).unwrap(), 1);
	assert_eq!(read.call(&mut store, (0, 2)).unwrap(), 3);
}

#[test]
fn test_dead_array_excluded_from_data_section() {
	// DCE removes functions that are not reachable from exports.  The static
	// array owned by a dead function must not appear in the WASM data segment.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn live() -> i32 { 42 }

        fn dead() -> i32 {
            local arr: [3]i32 = [0xDE, 0xAD, 0xBE];
            arr[0]
        }

        export { live }
    "});

	// The sentinel values from `dead` must not appear anywhere in the binary.
	let marker = &[0xDE_u8, 0, 0, 0]; // 0xDE as LE i32
	assert!(
		!case.bytecode.windows(4).any(|w| w == marker),
		"dead function's array bytes must not appear in the data section"
	);
}

#[test]
fn test_array_index_wat() {
	// WAT snapshot: pins the data segment placement and the load/store
	// instruction shape (i32.add + i32.mul offset arithmetic).
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn get(i: u32) -> i32 {
            local arr: [4]i32 = [10, 20, 30, 40];
            arr[i]
        }

        export { get }
    "});
	insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_slice_range_wat() {
	// WAT snapshot for all four exclusive-range forms on a slice operand.
	// Slices lower to two WASM values: (ptr: i32, len: i32).
	//
	// s[..]      — structural copy: same ptr + same len, no arithmetic.
	//              Potential optimization: could be a noop if result is used
	//              in place (identical bit pattern); currently emits two
	//              local.get instructions.
	//
	// s[..to]    — ptr unchanged, len = to (no subtraction needed).
	//
	// s[from..]  — ptr offset by from*sizeof(i32), len = slice_len − from.
	//              `from` is spilled to a temp so it can be used for both
	//              the add and the subtract.
	//
	// s[from..to] — ptr offset by from*sizeof(i32), len = to − from.
	//
	// Edge cases NOT yet handled:
	//   • bounds checking (out-of-range access is UB at runtime)
	//   • from > to produces a nonsensical negative length
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn full_copy(s: heap::[]i32) -> heap::[]i32 {
            s[..]
        }

        fn to_limit(s: heap::[]i32, to: u32) -> heap::[]i32 {
            s[..to]
        }

        fn from_start(s: heap::[]i32, from: u32) -> heap::[]i32 {
            s[from..]
        }

        fn bounded(s: heap::[]i32, from: u32, to: u32) -> heap::[]i32 {
            s[from..to]
        }

        export { full_copy, to_limit, from_start, bounded }
    "});
	insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_slice_range_array_wat() {
	// WAT snapshot for slicing an array (static size known at compile time).
	// Unlike the slice case, `end` defaults to the compile-time array length
	// when omitted, so no AggregateGet for len is emitted.
	//
	// arr[..]    — ptr = array base, len = const 4 (array size)
	// arr[i..n]  — ptr = base + i*4, len = n − i
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn full_array(arr: heap::[4]i32) -> heap::[]i32 {
            arr[..]
        }

        fn partial_array(arr: heap::[4]i32, i: u32, n: u32) -> heap::[]i32 {
            arr[i..n]
        }

        export { full_array, partial_array }
    "});
	insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_narrow_pointer_deref_sign_extension_and_byte_isolation() {
	// Exercises the narrow load/store fix end-to-end:
	//
	// 1. Zero-extension: reading 0xFF through *u8 must yield 255, not -1.
	// 2. Sign-extension: reading 0xFF through *i8 must yield -1.
	// 3. Byte isolation: writing through *u8 must not touch adjacent bytes.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        fn read_u8(ptr: heap::*u8) -> u8 { ptr.* }
        fn read_i8(ptr: heap::*i8) -> i8 { ptr.* }
        fn write_u8(ptr: heap::*mut u8, val: u8) { ptr.* = val }

        export { heap, read_u8, read_i8, write_u8 }
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

	let mem = instance
		.get_memory(&mut store, "heap")
		.expect("heap not exported");
	let read_u8 = instance
		.get_typed_func::<i32, i32>(&mut store, "read_u8")
		.expect("read_u8");
	let read_i8 = instance
		.get_typed_func::<i32, i32>(&mut store, "read_i8")
		.expect("read_i8");
	let write_u8 = instance
		.get_typed_func::<(i32, i32), ()>(&mut store, "write_u8")
		.expect("write_u8");

	// Place 0xFF at address 0, 0x00 at address 1 via the host.
	mem.write(&mut store, 0, &[0xFF, 0x00]).unwrap();

	// Zero-extension: u8 load of 0xFF must be 255, not -1.
	assert_eq!(read_u8.call(&mut store, 0).unwrap(), 255);

	// Sign-extension: i8 load of 0xFF must be -1.
	assert_eq!(read_i8.call(&mut store, 0).unwrap(), -1);

	// Byte isolation: writing 0xAB at address 0 must not touch address 1.
	mem.write(&mut store, 1, &[0x42]).unwrap();
	write_u8.call(&mut store, (0, 0xAB)).unwrap();
	let mut buf = [0u8; 2];
	mem.read(&mut store, 0, &mut buf).unwrap();
	assert_eq!(buf[0], 0xAB, "byte at address 0 should be 0xAB");
	assert_eq!(buf[1], 0x42, "adjacent byte at address 1 must be untouched");
}

#[test]
fn test_global_read_before_write_returns_old_value() {
	// alloc() must return the *pre-advance* bump pointer, not the updated one.
	// This exercises the GlobalGet-always-spill fix: without it the return
	// re-emits `global.get` after the `global.set`, yielding new_end instead
	// of the original ptr.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        global mut bump: heap::*u8 = heap::DATA_END;

        fn alloc(size: u32) -> heap::*u8 {
            local ptr: u32 = bump as u32;
            bump = (ptr + size) as heap::*u8;
            ptr as heap::*u8
        }

        export { heap, alloc }
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

	let alloc = instance
		.get_typed_func::<i32, i32>(&mut store, "alloc")
		.expect("alloc");

	let p0 = alloc.call(&mut store, 8).unwrap() as u32;
	let p1 = alloc.call(&mut store, 8).unwrap() as u32;
	let p2 = alloc.call(&mut store, 16).unwrap() as u32;

	assert_eq!(p1, p0 + 8, "second alloc must start right after first");
	assert_eq!(p2, p0 + 16, "third alloc must start right after second");
}

#[test]
fn test_global_initialized_to_data_end() {
	// A mutable global initialized to `heap::DATA_END` must receive the
	// compile-time static-segment-end offset as its WASM init expression,
	// and reading it back at runtime must return that same value.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        global mut bump: heap::*u8 = heap::DATA_END;

        fn get_bump() -> u32 { bump as u32 }
        fn advance(n: u32) { bump = (bump as u32 + n) as heap::*u8 }

        export { heap, get_bump, advance }
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");

	let get_bump = instance
		.get_typed_func::<(), i32>(&mut store, "get_bump")
		.expect("get_bump");
	let advance = instance
		.get_typed_func::<i32, ()>(&mut store, "advance")
		.expect("advance");

	let initial = get_bump.call(&mut store, ()).unwrap() as u32;
	// With the minimal STD stub (no string literals) DATA_END is 0.
	// The key invariant is that advance shifts bump by exactly the requested amount.
	advance.call(&mut store, 16).unwrap();
	let after = get_bump.call(&mut store, ()).unwrap() as u32;
	assert_eq!(
		after,
		initial + 16,
		"advance must shift bump by exactly 16 bytes"
	);
}

#[test]
fn test_null_pointer_comparison() {
	// null<M, T>() returns a zero pointer. Verify that:
	//  1. null() compares equal to another null() (the `node.next == ptr::null()` pattern)
	//  2. a non-zero pointer does NOT compare equal to null()
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } {
            min_pages: 1,
        };

        struct Node { value: i32, next: *Node }

        fn make_null() -> *Node { ptr::null() }
        fn is_null_ptr(p: *Node) -> bool { p == ptr::null() }
        fn ptr_from_addr() -> *Node { 4 as heap::*Node }

        export { heap, make_null, is_null_ptr, ptr_from_addr }
    "});

	assert!(
		case.tir.diagnostics.is_empty(),
		"unexpected diagnostics: {:?}",
		case.tir.diagnostics
	);

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation");

	let make_null = instance
		.get_typed_func::<(), i32>(&mut store, "make_null")
		.expect("make_null");
	let is_null_ptr = instance
		.get_typed_func::<i32, i32>(&mut store, "is_null_ptr")
		.expect("is_null_ptr");
	let ptr_from_addr = instance
		.get_typed_func::<(), i32>(&mut store, "ptr_from_addr")
		.expect("ptr_from_addr");

	assert_eq!(
		make_null.call(&mut store, ()).unwrap(),
		0,
		"null() must be 0"
	);
	assert_eq!(
		is_null_ptr.call(&mut store, 0).unwrap(),
		1,
		"null ptr is null"
	);
	assert_eq!(
		is_null_ptr.call(&mut store, 4).unwrap(),
		0,
		"non-null ptr is not null"
	);
	assert_eq!(
		ptr_from_addr.call(&mut store, ()).unwrap(),
		4,
		"4 as *Node must be 4"
	);
}

#[test]
fn test_size_of_and_align_of_intrinsics() {
	// size_of and align_of are inlined to compile-time Int nodes by MIR
	// lowering, so the exported functions are simple i32.const returns.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 };

        fn size_u8() -> u32    { size_of::<u8, heap>() }
        fn size_u16() -> u32   { size_of::<u16, heap>() }
        fn size_u32() -> u32   { size_of::<u32, heap>() }
        fn size_u64() -> u32   { size_of::<u64, heap>() }
        fn align_u8() -> u32   { align_of::<u8, heap>() }
        fn align_u16() -> u32  { align_of::<u16, heap>() }
        fn align_u32() -> u32  { align_of::<u32, heap>() }

        export { size_u8, size_u16, size_u32, size_u64, align_u8, align_u16, align_u32 }
    "});

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation");

	let mut get = |name: &str| {
		instance
			.get_typed_func::<(), i32>(&mut store, name)
			.unwrap()
	};
	let size_u8 = get("size_u8");
	let size_u16 = get("size_u16");
	let size_u32 = get("size_u32");
	let size_u64 = get("size_u64");
	let align_u8 = get("align_u8");
	let align_u16 = get("align_u16");
	let align_u32 = get("align_u32");

	assert_eq!(size_u8.call(&mut store, ()).unwrap(), 1);
	assert_eq!(size_u16.call(&mut store, ()).unwrap(), 2);
	assert_eq!(size_u32.call(&mut store, ()).unwrap(), 4);
	assert_eq!(size_u64.call(&mut store, ()).unwrap(), 8);
	assert_eq!(align_u8.call(&mut store, ()).unwrap(), 1);
	assert_eq!(align_u16.call(&mut store, ()).unwrap(), 2);
	assert_eq!(align_u32.call(&mut store, ()).unwrap(), 4);
}

#[test]
fn test_check_wad_magic_wat() {
	// Constant slice indices 0-3 must all fold into `i32.load8_u offset=N`
	// with no runtime Add/Mul — the four loads become offset=0,1,2,3.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 };

        pub fn check_wad_magic(data: heap::[]u8) -> bool {
            data[0] == 0x49
                && data[1] == 0x57
                && data[2] == 0x41
                && data[3] == 0x44
        }

        export { check_wad_magic }
    "});
	insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

#[test]
fn test_constant_index_offset_folding_wat() {
	// Constant array and slice indices must be folded directly into the WASM
	// memarg immediate (e.g. `i32.load offset=8`) with no runtime Add/Mul.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 };

        fn get_arr() -> i32 {
            local arr: [4]i32 = [10, 20, 30, 40];
            arr[2]
        }

        fn get_slice(s: heap::[]i32) -> i32 {
            s[3]
        }

        export { get_arr, get_slice }
    "});
	insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}

// ── AddressOf (.& / .&mut)
// ───────────────────────────────────────────────────────────

#[test]
fn test_address_of_array_element() {
	// `arr[i].&mut` on a mutable heap array returns a *mut pointer to element i.
	// The byte address must equal base + i * elem_size; writing through the
	// returned pointer must update the correct memory slot.
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 }

        fn elem_ptr(arr: heap::[4]mut i32, i: u32) -> heap::*mut i32 {
            arr[i].&mut
        }

        export { heap, elem_ptr }
    "});
	assert!(case.tir.diagnostics.is_empty());

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");
	let mem = instance.get_memory(&mut store, "heap").unwrap();
	let elem_ptr = instance
		.get_typed_func::<(i32, i32), i32>(&mut store, "elem_ptr")
		.unwrap();

	// Initialise four i32s at base address 0: [10, 20, 30, 40].
	for (i, &v) in [10i32, 20, 30, 40].iter().enumerate() {
		mem.write(&mut store, i * 4, &v.to_le_bytes()).unwrap();
	}

	// elem_ptr(base=0, i=2) must return byte offset 8.
	assert_eq!(elem_ptr.call(&mut store, (0, 2)).unwrap(), 8);
	// elem_ptr(base=0, i=0) must return byte offset 0.
	assert_eq!(elem_ptr.call(&mut store, (0, 0)).unwrap(), 0);
	// elem_ptr(base=0, i=3) must return byte offset 12.
	assert_eq!(elem_ptr.call(&mut store, (0, 3)).unwrap(), 12);
}

#[test]
fn test_address_of_struct_field() {
	// `ptr.*.field.&` returns a pointer to the field's byte position within the struct.
	// `x` is at offset 0; `y` is at offset 4 (both are i32 fields with alignment 4,
	// sorted in declaration order since alignment is equal).
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 }
        struct Point { x: i32, y: i32 }

        fn x_addr(ptr: heap::*Point) -> heap::*i32 { ptr.*.x.& }
        fn y_addr(ptr: heap::*Point) -> heap::*i32 { ptr.*.y.& }

        export { heap, x_addr, y_addr }
    "});
	assert!(case.tir.diagnostics.is_empty());

	let engine = wasmtime::Engine::default();
	let module =
		wasmtime::Module::new(&engine, &case.bytecode).expect("invalid wasm");
	let mut store = wasmtime::Store::new(&engine, ());
	let instance = wasmtime::Instance::new(&mut store, &module, &[])
		.expect("instantiation failed");
	let x_addr = instance
		.get_typed_func::<i32, i32>(&mut store, "x_addr")
		.unwrap();
	let y_addr = instance
		.get_typed_func::<i32, i32>(&mut store, "y_addr")
		.unwrap();

	let base: i32 = 64; // arbitrary non-zero base
	// x is the first field: address == base + 0
	assert_eq!(x_addr.call(&mut store, base).unwrap(), base);
	// y is the second field: address == base + 4
	assert_eq!(y_addr.call(&mut store, base).unwrap(), base + 4);
}

#[test]
fn test_address_of_wat() {
	// WAT snapshot pinning the emitted instructions for .& operations:
	//
	// elem_ptr — dynamic index: the address is base + i * 4.
	//   Expected: local.get 0, local.get 1, i32.const 4, i32.mul, i32.add
	//
	// y_addr   — field at static offset 4: the address is ptr + 4.
	//   Expected: local.get 0, i32.const 4, i32.add
	//
	// x_addr   — field at static offset 0: the address IS the pointer.
	//   Expected: local.get 0  (no arithmetic needed)
	let case = TestCase::new(indoc! {"
        memory heap: Memory where { Size = u32 } { min_pages: 1 }
        struct Point { x: i32, y: i32 }

        fn elem_ptr(arr: heap::[4]i32, i: u32) -> heap::*i32 { arr[i].& }
        fn x_addr(ptr: heap::*Point) -> heap::*i32 { ptr.*.x.& }
        fn y_addr(ptr: heap::*Point) -> heap::*i32 { ptr.*.y.& }

        export { heap, elem_ptr, x_addr, y_addr }
    "});
	assert!(case.tir.diagnostics.is_empty());
	insta::assert_snapshot!(wasmprinter::print_bytes(&case.bytecode).unwrap());
}
