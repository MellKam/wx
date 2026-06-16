use std::collections::HashMap;

use indoc::indoc;

use super::*;
use crate::{STDLIB_SOURCE, tir, vfs};

#[allow(unused)]
struct TestCase {
    graph: vfs::CompilationGraph,
    tir: tir::TIR,
    mir: MIR,
}

impl TestCase {
    fn new(source: &str) -> Self {
        let mut builder = vfs::CompilationGraphBuilder::new();
        let stdlib_id = builder
            .load_crate(
                "std.wx".to_string(),
                &vfs::VirtualFileSource::new(HashMap::from([(
                    "std.wx".to_string(),
                    STDLIB_SOURCE.to_string(),
                )])),
            )
            .unwrap();
        let root_id = builder
            .load_crate(
                "main.wx".to_string(),
                &vfs::VirtualFileSource::new(HashMap::from([(
                    "main.wx".to_string(),
                    source.to_string(),
                )])),
            )
            .unwrap();
        let mut graph = builder.build(root_id, stdlib_id);
        let tir = tir::TIR::build(&mut graph);
        let mir = MIR::build(&tir, &graph.interner, graph.id_generator);
        TestCase { graph, tir, mir }
    }
}

// Minimal inline definitions shared across tests that need them.

/// ASCII helper and the char methods needed by tests that call
/// `to_ascii_uppercase` / `to_ascii_lowercase`.
const CHAR_ASCII_METHODS: &str = indoc! {"
    const ASCII_CASE_MASK: u8 = 0b0010_0000;

    impl char {
        #[inline]
        pub fn is_ascii_lowercase(self) -> bool {
            self >= 'a' && self <= 'z'
        }

        #[inline]
        pub fn to_ascii_uppercase(self) -> char {
            if self.is_ascii_lowercase() {
                ((self as u8) ^ ASCII_CASE_MASK) as char
            } else {
                self
            }
        }
    }
"};

// ── primitives
// ────────────────────────────────────────────────────────────────

#[test]
fn test_char_lowered_to_u32() {
    // char is a primitive; MIR should represent it as U32.
    let case = TestCase::new(indoc! {"
        fn identity(c: char) -> char {
            c
        }

        export { identity }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

// ── structs
// ───────────────────────────────────────────────────────────────────

#[test]
fn test_struct_field_access_lowered_to_local_tuple_get() {
    // ObjectAccess on a struct local → LocalTupleGet in MIR.
    let case = TestCase::new(indoc! {"
        struct Point {
            x: u32,
            y: u32,
        }

        fn get_x(p: Point) -> u32 {
            p.x
        }

        export { get_x }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_struct_init_lowered_to_struct_create() {
    // StructInit → StructCreate in MIR.
    let case = TestCase::new(indoc! {"
        struct Point {
            x: u32,
            y: u32,
        }

        fn make_point(x: u32, y: u32) -> Point {
            Point::{ x: x, y: y }
        }

        export { make_point }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_global_struct_type() {
    // A function returning a struct type should produce Tuple-typed MIR.
    let case = TestCase::new(indoc! {"
        struct Vec2 {
            x: u32,
            y: u32,
        }

        fn get_origin() -> Vec2 {
            Vec2::{ x: 0, y: 0 }
        }

        export { get_origin }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

// ── associated consts
// ─────────────────────────────────────────────────────────

#[test]
fn test_size_associated_const() {
    // SIZE/ALIGN are built-in associated consts seeded by the type system;
    // they should lower to a plain Int in MIR.
    let case = TestCase::new(indoc! {"
        fn get_u32_size() -> u32 {
            u32::SIZE
        }

        export { get_u32_size }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

// ── string literals
// ───────────────────────────────────────────────────────────

// ── inline methods
// ────────────────────────────────────────────────────────────

#[test]
fn test_struct_method_call() {
    // Both #[inline] methods get substituted into `to_upper`; the snapshot
    // shows only the arithmetic body with no Call nodes remaining.
    let case = TestCase::new(&format!(
        "{CHAR_ASCII_METHODS}\n{}",
        indoc! {"
        fn to_upper(c: char) -> char {
            c.to_ascii_uppercase()
        }

        export { to_upper }
    "}
    ));
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_inline_method_is_substituted() {
    // A call to an #[inline] method must be replaced by its body in MIR —
    // the snapshot shows the inlined if/xor logic with no Call node.
    let case = TestCase::new(&format!(
        "{CHAR_ASCII_METHODS}\n{}",
        indoc! {"
        fn to_upper(c: char) -> char {
            c.to_ascii_uppercase()
        }

        export { to_upper }
    "}
    ));
    insta::assert_yaml_snapshot!(case.mir);
}

// ── memory instructions
// ───────────────────────────────────────────────────────

#[test]
fn test_memory_grow_lowers_to_memory_grow() {
    // heap.grow(delta) → MemoryGrow { memory_index: 0, delta }
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;

        pub fn f(delta: u32) -> u32 {
            heap.grow(delta)
        }

        export { f }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_memory_size_lowers_to_memory_size() {
    // heap.size() → MemorySize { memory_index: 0 }
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;

        pub fn f() -> u32 {
            heap.size()
        }

        export { f }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_memory_data_end_lowers_to_memory_offset() {
    // heap::DATA_END → MemoryOffset { memory_index: 0 }
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;

        pub fn f() -> heap::*mut u8 {
            heap::DATA_END
        }

        export { f }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_memory_index_lowers_to_int() {
    // heap::MEMORY_INDEX → Int { value: 0 } (the wasm linear memory index)
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;

        pub fn f() -> u32 {
            heap::MEMORY_INDEX
        }

        export { f }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

// ── generic over Memory ───────────────────────────────────────────────────────

#[test]
fn test_generic_over_memory_monomorphizes_size_type() {
    // A generic fn<M: Memory> called with two concrete memories must produce
    // two monomorphized instances with the right concrete Size types (u32 / u64).
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;
        memory stack: Memory<Size = u64>;

        fn pass<M: Memory>(mem: M, n: M::Size) -> M::Size {
            n
        }

        pub fn use_heap(n: u32) -> u32 {
            pass(heap, n)
        }

        pub fn use_stack(n: u64) -> u64 {
            pass(stack, n)
        }

        export { use_heap, use_stack }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

// ── call graph / DCE ─────────────────────────────────────────────────────────

#[test]
fn test_dead_function_removed_by_dce() {
    // `unused` is never exported or called — DCE must eliminate it.
    let case = TestCase::new(indoc! {"
        fn used(x: u32) -> u32 { x + 1 }
        fn unused(x: u32) -> u32 { x * 2 }

        export { used }
    "});
    assert_eq!(case.mir.functions.len(), 1);
    let ExportItem::Function { id, .. } = case.mir.exports[0] else {
        panic!()
    };
    assert_eq!(case.mir.functions[0].id, id);
}

#[test]
fn test_non_inline_callee_survives_dce() {
    // `helper` is called by `entry` but is not marked `#[inline]`, so it must
    // remain in mir.functions as a call target rather than being folded away.
    let case = TestCase::new(indoc! {"
        fn helper(x: u32) -> u32 { x + 1 }
        fn entry(x: u32) -> u32 { helper(x) }

        export { entry }
    "});
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_inline_chain_collapses_to_single_function() {
    // Three `#[inline]` functions chained: the entire chain folds into `main`
    // and the inline helpers are removed by DCE. Only one function survives.
    let case = TestCase::new(indoc! {"
        #[inline]
        fn add_one(x: u32) -> u32 { x + 1 }

        #[inline]
        fn add_two(x: u32) -> u32 { add_one(add_one(x)) }

        fn main(x: u32) -> u32 { add_two(x) }

        export { main }
    "});
    assert_eq!(case.mir.functions.len(), 1);
}

#[test]
fn test_multiple_exports_protect_their_callees() {
    // `f` calls `double`, `g` calls `triple`; `orphan` is called by nobody.
    // Both export roots must protect their own callees; only `orphan` is DCE'd.
    let case = TestCase::new(indoc! {"
        fn double(x: u32) -> u32 { x * 2 }
        fn triple(x: u32) -> u32 { x * 3 }
        fn orphan(x: u32) -> u32 { x * 4 }

        fn f(x: u32) -> u32 { double(x) }
        fn g(x: u32) -> u32 { triple(x) }

        export { f, g }
    "});
    assert_eq!(case.mir.functions.len(), 4); // f, g, double, triple — not orphan
    assert_eq!(case.mir.exports.len(), 2);
}

#[test]
fn test_function_reference_value_survives_dce() {
    // `target` is only referenced as a value passed to `apply` — never called
    // directly from `run`. DCE must keep it because it is a live dependency.
    let case = TestCase::new(indoc! {"
        fn target(x: u32) -> u32 { x + 1 }

        fn apply(f: fn(u32) -> u32, x: u32) -> u32 {
            f(x)
        }

        fn run() -> u32 {
            apply(target, 42)
        }

        export { run }
    "});
    // run, apply, target — all three must survive.
    assert_eq!(case.mir.functions.len(), 3);
}

#[test]
fn test_unused_trait_impl_eliminated_by_dce() {
    // Both Num1 and Num2 implement Scalable, but only Num1 is ever used.
    // The call chain is: run → doubled<Num1> → Num1::value.
    // Num2::value has no incoming edge and must be eliminated by DCE.
    let case = TestCase::new(indoc! {"
        trait Scalable {
            fn value(self) -> i32;
            fn doubled(self) -> i32 { self.value() * 2 }
        }
        struct Num1 { n: i32 }
        struct Num2 { n: i32 }
        impl Scalable for Num1 {
            fn value(self) -> i32 { self.n }
        }
        impl Scalable for Num2 {
            fn value(self) -> i32 { self.n * 10 }
        }
        fn run() -> i32 {
            local n = Num1::{ n: 21 };
            n.doubled()
        }
        export { run }
    "});
    // run, doubled<Num1>, Num1::value — Num2::value must not survive.
    assert_eq!(case.mir.functions.len(), 3);
}

#[test]
fn test_recursive_function_survives_dce() {
    // A self-recursive exported function references itself as a callee.
    // DCE must keep it alive (it is its own root).
    let case = TestCase::new(indoc! {"
        fn count_down(n: u32) -> u32 {
            if n == 0 { 0 } else { count_down(n - 1) }
        }
        export { count_down }
    "});
    assert_eq!(case.mir.functions.len(), 1);
    let ExportItem::Function { id, .. } = case.mir.exports[0] else {
        panic!()
    };
    assert_eq!(case.mir.functions[0].id, id);
}

#[test]
fn test_inline_helper_in_dead_code_is_eliminated() {
    // dead_caller is never exported or transitively reachable.
    // It calls an #[inline] helper. After inlining, dead_caller holds the
    // inlined body, but DCE removes it along with the original helper.
    // Only `live` (the export) must survive.
    let case = TestCase::new(indoc! {"
        #[inline]
        fn helper(x: u32) -> u32 { x + 1 }
        fn dead_caller(x: u32) -> u32 { helper(x) }
        fn live(x: u32) -> u32 { x * 2 }
        export { live }
    "});
    assert_eq!(case.mir.functions.len(), 1);
    let ExportItem::Function { id, .. } = case.mir.exports[0] else {
        panic!()
    };
    assert_eq!(case.mir.functions[0].id, id);
}

#[test]
fn test_deep_call_chain_survives_dce() {
    // entry → step1 → step2 → step3 → leaf: the full depth-4 chain must survive.
    // Verifies that BFS propagation is not cut short at depth 2.
    let case = TestCase::new(indoc! {"
        fn leaf(x: u32) -> u32 { x }
        fn step3(x: u32) -> u32 { leaf(x) }
        fn step2(x: u32) -> u32 { step3(x) }
        fn step1(x: u32) -> u32 { step2(x) }
        fn entry(x: u32) -> u32 { step1(x) }
        export { entry }
    "});
    assert_eq!(case.mir.functions.len(), 5);
}

/// `struct Mixed { a: bool, b: i64, c: u32, d: f64 }` naively laid out takes
/// 28B; after sorting by alignment descending the optimal order is i64, f64,
/// u32, bool — 24B.
#[test]
fn test_struct_layout_is_alignment_sorted() {
    let case = TestCase::new(indoc! {"
        struct Mixed {
            a: bool,
            b: i64,
            c: u32,
            d: f64,
        }

        fn dummy(m: Mixed) -> Mixed { m }
        export { dummy }
    "});
    assert!(case.tir.diagnostics.is_empty());

    // The `dummy` function's first parameter is `Mixed`; its MIR type carries
    // the aggregate index into `mir.aggregates`.
    let sig_index = case.mir.functions[0].signature_index as usize;
    let param_ty = case.mir.signatures[sig_index].params()[0];
    let aggregate_index = match param_ty {
        Type::Aggregate { aggregate_index } => aggregate_index as usize,
        _ => panic!("expected Mixed to lower to an aggregate"),
    };

    let agg = &case.mir.aggregates[aggregate_index];
    // Total size and alignment after alignment-sorted layout.
    assert_eq!(agg.layout.size, 24);
    assert_eq!(agg.layout.align, 8);
    // Physical order: b(i64)@0, d(f64)@8, c(u32)@16, a(bool)@20
    assert_eq!(&*agg.offsets, &[0, 8, 16, 20]);
    assert_eq!(&*agg.values, &[Type::I64, Type::F64, Type::U32, Type::Bool]);
}

// ── Generics / monomorphization
// ───────────────────────────────────────────────

/// Calls `identity` with two *different* type arguments from two separate
/// callers.  Each `(orig_id, type_args)` pair is distinct so
/// `MonoRegistry::get_or_insert` must take the `else` branch for the second
/// type, producing two separate mono functions.
#[test]
fn test_different_type_args_produce_separate_mono_instances() {
    let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T { t }

        fn run_i32()  -> i32  { identity(42) }
        fn run_bool() -> bool { identity(true) }

        export { run_i32, run_bool }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected TIR diagnostics"
    );
    // run_i32 + run_bool + identity<i32> + identity<bool> = 4
    assert_eq!(
        case.mir.functions.len(),
        4,
        "expected 4 MIR functions, got {}",
        case.mir.functions.len()
    );
}

/// A concrete generic `call_wrap<T>` calls another generic `wrap<T>`.
/// The mono pass must run two worklist iterations:
///   pass 1 – lower `call_wrap<i32>` (substitutes TypeParam{0}→i32 in type_args
///             before registering `wrap`, then enqueues `wrap<i32>`)
///   pass 2 – lower `wrap<i32>`
///
/// Root cause of the former stack overflow: type_args were forwarded raw to
/// `MonoRegistry::get_or_insert`, so `wrap<TypeParam{0}>` was registered
/// instead of `wrap<i32>`. When the worklist lowered it with
/// `current_substitutions = [TypeParam{0}]`, `lower_type_index` entered
/// infinite mutual recursion: TypeParam{0} → substitutions[0] → TypeParam{0}.
/// Fix: substitute TypeParam entries in type_args through current_substitutions
/// before calling get_or_insert (MIR GenericCall arm).
#[test]
fn test_generic_calls_generic_multi_iteration_worklist() {
    let case = TestCase::new(indoc! {"
        fn wrap<T>(t: T) -> T { t }
        fn call_wrap<T>(t: T) -> T { wrap(t) }

        fn run() -> i32 { call_wrap(42) }

        export { run }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected TIR diagnostics"
    );
    // run + call_wrap<i32> + wrap<i32> = 3
    assert_eq!(
        case.mir.functions.len(),
        3,
        "expected 3 MIR functions (run + call_wrap<i32> + wrap<i32>), got {}",
        case.mir.functions.len()
    );
}

/// BUG DOCUMENTATION – `#[inline]` on a generic function is currently silently
/// ignored for its mono instances.
///
/// Root cause: the Phase-1 loop that populates `inline_functions` guards on
/// `type_params.is_empty()`, skipping every generic function.  Mono instances
/// created in Phase 2 are never inserted into `inline_functions`, so the Kahn
/// inlining pass never substitutes their call sites.
///
/// Current behaviour : 2 functions survive (`run` + `wrap<i32>`).
/// Correct behaviour : `wrap<i32>` should be inlined into `run`, leaving 1.
///
/// This assertion documents the *current* (limited) behaviour.  If it ever
/// starts failing with `len == 1` the limitation has been fixed.
#[test]
fn test_inline_attribute_on_generic_not_propagated_to_mono_instance() {
    let case = TestCase::new(indoc! {"
        #[inline]
        fn wrap<T>(t: T) -> T { t }

        fn run() -> i32 { wrap(42) }

        export { run }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected TIR diagnostics"
    );
    assert_eq!(
        case.mir.functions.len(),
        2,
        "expected 2 functions (inline attr on generic currently has no effect on mono instances)"
    );
}

// ── Generic struct monomorphization
// ──────────────────────────────────────────────

/// `Point<i32>` and `Point<f32>` must produce two distinct aggregates — one
/// with `[I32, I32]` fields and one with `[F32, F32]` fields. Before the fix,
/// both keyed on `[TypeParam{0}, TypeParam{0}]` in the aggregate cache, so the
/// second instantiation returned the first's aggregate index, silently giving
/// `Point<f32>` the wrong field types.
#[test]
fn test_generic_struct_distinct_aggregates_per_type_arg() {
    let case = TestCase::new(indoc! {"
        struct Point<T> {
            x: T,
            y: T,
        }

        fn get_x_i32(p: Point<i32>) -> i32 { p.x }
        fn get_x_f32(p: Point<f32>) -> f32 { p.x }

        export { get_x_i32, get_x_f32 }
    "});
    assert!(case.tir.diagnostics.is_empty());

    let sig_i32 = case
        .mir
        .functions
        .iter()
        .find(|f| {
            let sig = &case.mir.signatures[f.signature_index as usize];
            sig.result() == Type::I32
        })
        .expect("get_x_i32 not found");
    let sig_f32 = case
        .mir
        .functions
        .iter()
        .find(|f| {
            let sig = &case.mir.signatures[f.signature_index as usize];
            sig.result() == Type::F32
        })
        .expect("get_x_f32 not found");

    let agg_i32 = match case.mir.signatures[sig_i32.signature_index as usize].params()[0] {
        Type::Aggregate { aggregate_index } => aggregate_index as usize,
        _ => panic!("expected Point<i32> to be an aggregate"),
    };
    let agg_f32 = match case.mir.signatures[sig_f32.signature_index as usize].params()[0] {
        Type::Aggregate { aggregate_index } => aggregate_index as usize,
        _ => panic!("expected Point<f32> to be an aggregate"),
    };

    assert_ne!(
        agg_i32, agg_f32,
        "Point<i32> and Point<f32> must map to distinct aggregates"
    );
    assert_eq!(
        &*case.mir.aggregates[agg_i32].values,
        &[Type::I32, Type::I32]
    );
    assert_eq!(
        &*case.mir.aggregates[agg_f32].values,
        &[Type::F32, Type::F32]
    );
}

/// Constructing and accessing a field on a concrete `Point<i32>` inside a
/// non-generic function (no outer `current_substitutions`). Verifies that the
/// struct's own type args are used as substitutions when lowering its fields.
#[test]
fn test_generic_struct_init_and_field_access_concrete() {
    let case = TestCase::new(indoc! {"
        struct Point<T> {
            x: T,
            y: T,
        }

        fn run() -> i32 {
            local p: Point<i32> = Point::<i32>::{ x: 3, y: 7 };
            p.x
        }

        export { run }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir.diagnostics
    );
    assert_eq!(case.mir.functions.len(), 1);

    let agg_idx = case
        .mir
        .aggregates
        .iter()
        .position(|a| a.values.len() == 2 && a.values.iter().all(|&t| t == Type::I32))
        .expect("Point<i32> aggregate not found");
    assert_eq!(case.mir.aggregates[agg_idx].layout.size, 8);
}

/// A generic function operating on a generic struct gets the correct aggregate
/// when monomorphized. `get_x<i64>` takes `Box<i64>` — the aggregate should
/// have a single `I64` field with size 8, not the TypeParam placeholder.
#[test]
fn test_generic_struct_in_generic_function_monomorphizes_correctly() {
    let case = TestCase::new(indoc! {"
        struct Box<T> {
            value: T,
        }

        fn get_value<T>(b: Box<T>) -> T { b.value }

        fn run() -> i64 {
            local b: Box<i64> = Box::<i64>::{ value: 42 };
            get_value(b)
        }

        export { run }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "{:?}",
        case.tir.diagnostics
    );

    let agg = case
        .mir
        .aggregates
        .iter()
        .find(|a| a.values.len() == 1 && a.values[0] == Type::I64)
        .expect("Box<i64> aggregate with I64 field not found");
    assert_eq!(agg.layout.size, 8);
    assert_eq!(agg.layout.align, 8);
}

/// Mutually recursive `#[inline]` functions used to stall Kahn's algorithm:
/// both started with `inline_callee_count == 1` so neither entered the queue.
///
/// The fix adds a cycle-breaking epilog: after the inner Kahn loop drains,
/// one stalled function is evicted as an "anchor" (kept as a normal call),
/// its callers' counts are decremented, and the inner loop resumes.
///
/// For `f ↔ g`:
///   - anchor = f  (stays as a real function)
///   - g's count drops to 0 → g is inlined into f
///   - g disappears from the output
///
/// Result: entry + f = 2 functions.
#[test]
fn test_mutually_recursive_inline_functions_kahn_stall() {
    let case = TestCase::new(indoc! {"
        #[inline]
        fn f(n: u32) -> u32 {
            if n == 0 { 0 } else { g(n - 1) }
        }
        #[inline]
        fn g(n: u32) -> u32 {
            if n == 0 { 0 } else { f(n - 1) }
        }

        fn entry(n: u32) -> u32 { f(n) }

        export { entry }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected TIR diagnostics"
    );
    // entry + f = 2  (g was inlined into f; f is the cycle-break anchor)
    assert_eq!(
        case.mir.functions.len(),
        2,
        "expected 2 functions (entry + f with g inlined); got {}",
        case.mir.functions.len()
    );
}

/// After an `#[inline]` function is substituted into its caller, the inlining
/// pass transfers the inline function's call-graph edges to the caller.  DCE
/// seeds only from exported functions and must reach the mono instance via
/// that propagated edge — if the graph update is skipped, the mono instance
/// is incorrectly eliminated.
#[test]
fn test_inline_function_calling_generic_dce_preserves_mono_instance() {
    let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T { t }

        #[inline]
        fn one_more(x: i32) -> i32 { identity(x + 1) }

        fn run() -> i32 { one_more(41) }

        export { run }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected TIR diagnostics"
    );
    // one_more is inlined into run then removed by DCE.
    // identity<i32> must survive because the graph-edge propagation transferred
    // it to run's callees.  Total: run + identity<i32> = 2.
    assert_eq!(
        case.mir.functions.len(),
        2,
        "expected 2 functions (run + identity<i32>); got {}",
        case.mir.functions.len()
    );
}

#[test]
fn test_multiple_calls_to_generic_produce_single_mono_instance() {
    // Calling `identity<i32>` twice must produce exactly one monomorphized
    // function, not two. The MIR function list should contain:
    //   1. `run` (the concrete caller)
    //   2. one monomorphized copy of `identity` for i32
    let case = TestCase::new(indoc! {"
        fn identity<T>(t: T) -> T {
            t
        }

        fn run() -> i32 {
            identity(1) + identity(2)
        }

        export { run }
    "});
    assert!(
        case.tir.diagnostics.is_empty(),
        "unexpected TIR diagnostics"
    );
    // `run` + one mono instance of `identity<i32>` = 2 functions total.
    assert_eq!(
        case.mir.functions.len(),
        2,
        "expected exactly 2 MIR functions (run + one identity<i32>), got {}",
        case.mir.functions.len()
    );
}

// ── static data
// ─────────────────────────────────────────────────────────────

// String literal tests removed: `string` is no longer a named struct type;
// string literals produce `[]u8` slices. Static-data coverage is provided
// by the array literal tests below.

// ── arrays
// ─────────────────────────────────────────────────────────────────────

#[test]
fn test_array_literal_bytes_are_little_endian() {
    // [1, 2, 3] as heap::[3]i32 → 12 bytes encoding each value as 32-bit LE.
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;
        fn get() -> heap::[3]i32 {
            local arr: heap::[3]i32 = [1, 2, 3];
            arr
        }
        export { get }
    "});
    assert_eq!(case.mir.static_entries.len(), 1);
    assert_eq!(
        &*case.mir.static_entries[0].bytes,
        &[
            1u8, 0, 0, 0, // 1_i32 LE
            2, 0, 0, 0, // 2_i32 LE
            3, 0, 0, 0, // 3_i32 LE
        ],
    );
    assert_eq!(case.mir.static_entries[0].align, 4);
}

#[test]
fn test_array_repeat_bytes_repeated() {
    // [7; 4] as heap::[4]u8 → four bytes each equal to 7.
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;
        fn get() -> heap::[4]u8 {
            local arr: heap::[4]u8 = [7; 4];
            arr
        }
        export { get }
    "});
    assert_eq!(case.mir.static_entries.len(), 1);
    assert_eq!(&*case.mir.static_entries[0].bytes, &[7u8, 7, 7, 7]);
    assert_eq!(case.mir.static_entries[0].align, 1);
}

#[test]
fn test_array_dce_removes_static_data_ownership() {
    // A dead function with an array literal is removed by DCE.
    // No live function should reference its static entry.
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;
        fn live() -> i32 { 42 }
        fn dead() -> heap::[3]i32 {
            local arr: heap::[3]i32 = [1, 2, 3];
            arr
        }
        export { live }
    "});
    assert_eq!(case.mir.functions.len(), 1);
    let live_indices: std::collections::HashSet<u32> = case
        .mir
        .functions
        .iter()
        .flat_map(|f| f.static_data.iter().copied())
        .collect();
    assert!(
        live_indices.is_empty(),
        "live functions must not reference the dead array's entry"
    );
}

#[test]
fn test_static_entry_alignment_matches_element_type() {
    // i32 elements → align 4; f64 elements → align 8; u8 elements → align 1.
    let case = TestCase::new(indoc! {"
        memory heap: Memory<Size = u32>;
        fn ints() -> heap::[2]i32 {
            local a: heap::[2]i32 = [10, 20];
            a
        }
        fn doubles() -> heap::[2]f64 {
            local b: heap::[2]f64 = [1.0; 2];
            b
        }
        fn bytes() -> heap::[2]u8 {
            local c: heap::[2]u8 = [1; 2];
            c
        }
        export { ints, doubles, bytes }
    "});
    assert_eq!(case.mir.static_entries.len(), 3);
    let aligns: std::collections::HashSet<u32> =
        case.mir.static_entries.iter().map(|e| e.align).collect();
    assert!(aligns.contains(&4), "i32 array must have align 4");
    assert!(aligns.contains(&8), "f64 array must have align 8");
    assert!(aligns.contains(&1), "u8 array must have align 1");
}
