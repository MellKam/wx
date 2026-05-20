use indoc::indoc;

use super::*;
use crate::{ast, tir};

#[allow(unused)]
struct TestCase {
    interner: ast::StringInterner,
    files: ast::Files,
    ast: ast::AST,
    tir: tir::TIR,
    mir: MIR,
}

impl TestCase {
    fn new(source: &str) -> Self {
        let mut interner = ast::StringInterner::new();
        let mut files = ast::Files::new();
        let file_id = files
            .add("main.wx".to_string(), source.to_string())
            .unwrap();
        let ast =
            ast::Parser::parse(file_id, &files.get(file_id).unwrap().source, &mut interner);
        let tir = tir::TIR::build(&[&ast], &mut interner);
        let mir = MIR::build(&tir, &interner);
        TestCase { interner, files, ast, tir, mir }
    }
}

// Minimal inline definitions shared across tests that need them.

/// Defines the built-in `string` struct as a flat (ptr, len) pair.
/// String literals lower to this shape in MIR.
const STRING_STRUCT: &str = indoc! {"
    pub struct string {
        ptr: u32,
        len: u32,
    }
"};

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

// ── primitives ────────────────────────────────────────────────────────────────

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

// ── structs ───────────────────────────────────────────────────────────────────

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

// ── associated consts ─────────────────────────────────────────────────────────

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

// ── string literals ───────────────────────────────────────────────────────────

#[test]
fn test_string_literal_lowered_to_tuple() {
    // A string literal lowers to a Packed { StringIndex(ptr), Int(len) } in MIR.
    let case = TestCase::new(&format!("{STRING_STRUCT}\n{}", indoc! {"
        import \"console\" {
            fn log(ptr: u32, len: u32);
        }

        fn greet() {
            local s = \"hello\";
            console::log(s.ptr, s.len);
        }

        export { greet }
    "}));
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_string_field_access_lowered_to_local_tuple_get() {
    // Field access on a string local (ptr/len) should map to LocalTupleGet.
    let case = TestCase::new(&format!("{STRING_STRUCT}\n{}", indoc! {"
        import \"console\" {
            fn log(ptr: u32, len: u32);
        }

        fn main() {
            local str = \"test\";
            console::log(str.ptr, str.len);
        }

        export { main }
    "}));
    insta::assert_yaml_snapshot!(case.mir);
}

/// The Memory32 trait with default method bodies that delegate to wasm intrinsics.
const MEMORY32_TRAIT: &str = indoc! {"
    trait Memory32 {
        const OFFSET: u32;
        const MEMORY_INDEX: u32;
        fn size(self) -> u32 {
            wasm::memory32_size(self)
        }
        fn grow(self, delta: u32) -> u32 {
            wasm::memory32_grow(self, delta)
        }
    }
"};

/// The wasm intrinsic module — must appear after the traits it references.
const WASM_MODULE: &str = indoc! {"
    module wasm {
        #[intrinsic]
        fn memory32_size(mem: impl Memory32) -> u32;
        #[intrinsic]
        fn memory32_grow(mem: impl Memory32, delta: u32) -> u32;
    }
"};

// ── inline methods ────────────────────────────────────────────────────────────

#[test]
fn test_struct_method_call() {
    // Both #[inline] methods get substituted into `to_upper`; the snapshot
    // shows only the arithmetic body with no Call nodes remaining.
    let case = TestCase::new(&format!("{CHAR_ASCII_METHODS}\n{}", indoc! {"
        fn to_upper(c: char) -> char {
            c.to_ascii_uppercase()
        }

        export { to_upper }
    "}));
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_inline_method_is_substituted() {
    // A call to an #[inline] method must be replaced by its body in MIR —
    // the snapshot shows the inlined if/xor logic with no Call node.
    let case = TestCase::new(&format!("{CHAR_ASCII_METHODS}\n{}", indoc! {"
        fn to_upper(c: char) -> char {
            c.to_ascii_uppercase()
        }

        export { to_upper }
    "}));
    insta::assert_yaml_snapshot!(case.mir);
}

// ── memory instructions ───────────────────────────────────────────────────────

#[test]
fn test_memory_grow_lowers_to_memory_grow() {
    // heap.grow(delta) → MemoryGrow { memory_index: 0, delta }
    let case = TestCase::new(&format!("{MEMORY32_TRAIT}\n{WASM_MODULE}\n{}", indoc! {"
        memory heap: Memory32;

        pub fn f(delta: u32) -> u32 {
            heap.grow(delta)
        }

        export { f }
    "}));
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_memory_size_lowers_to_memory_size() {
    // heap.size() → MemorySize { memory_index: 0 }
    let case = TestCase::new(&format!("{MEMORY32_TRAIT}\n{WASM_MODULE}\n{}", indoc! {"
        memory heap: Memory32;

        pub fn f() -> u32 {
            heap.size()
        }

        export { f }
    "}));
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_memory_offset_lowers_to_memory_offset() {
    // heap::OFFSET → MemoryOffset { memory_index: 0 } (placeholder for codegen)
    let case = TestCase::new(&format!("{MEMORY32_TRAIT}\n{WASM_MODULE}\n{}", indoc! {"
        memory heap: Memory32;

        pub fn f() -> u32 {
            heap::OFFSET
        }

        export { f }
    "}));
    insta::assert_yaml_snapshot!(case.mir);
}

#[test]
fn test_memory_index_lowers_to_int() {
    // heap::MEMORY_INDEX → Int { value: 0 } (the wasm linear memory index)
    let case = TestCase::new(&format!("{MEMORY32_TRAIT}\n{WASM_MODULE}\n{}", indoc! {"
        memory heap: Memory32;

        pub fn f() -> u32 {
            heap::MEMORY_INDEX
        }

        export { f }
    "}));
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
    let ExportItem::Function { id, .. } = case.mir.exports[0] else { panic!() };
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

    let mixed_sym = case.interner.get("Mixed").unwrap();
    let mixed_ti = case.tir.type_pool.iter().position(|t| {
        if let tir::Type::Struct { struct_index } = t {
            case.tir.structs[*struct_index as usize].name.inner == mixed_sym
        } else {
            false
        }
    }).unwrap() as u32;

    let layout = compute_layout(
        &case.tir.type_pool,
        &case.tir.structs,
        mixed_ti,
        PointerSize::Memory32,
    );
    assert_eq!(layout.size, 24);
    assert_eq!(layout.align, 8);
}
