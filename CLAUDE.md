# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

WX is a Rust-implemented compiler for a language that targets WebAssembly. Syntax is Rust-inspired. This is a bachelor's thesis project.

## Commands

```bash
# Build
cargo build -p wx-compiler           # core library
cargo build --release -p wx-compiler-cli  # CLI binary → target/release/wx-compiler

# Test (almost all tests live in wx-compiler)
cargo test -p wx-compiler
cargo test -p wx-compiler -- <test_name>  # single test by name

# Update snapshots when output changes legitimately
INSTA_UPDATE=always cargo test -p wx-compiler
# Then strip assertion_line and remove .snap.new files:
find crates/wx-compiler/src -name "*.snap.new" | while read f; do
  grep -v "^assertion_line:" "$f" > "${f%.new}"
  rm "$f"
done

# Format
cargo fmt

# Build the WASM playground package
deno task build:wasm
```

`wx-lsp` is excluded from the workspace and must be built separately:
```bash
cargo build --manifest-path crates/wx-lsp/Cargo.toml
```

## Compilation pipeline

```
source text
    │  ast::Parser::parse()
    ▼
AST  (src/ast/)
    │  vfs::load_single_file_compilation() → CompilationGraph
    │  tir::build(&graph, &mut interner)
    ▼
TIR  (src/tir/) — type-checked, name-resolved IR
    │  MIR::build(&tir, &interner, graph.id_generator)
    ▼
MIR  (src/mir/) — desugared, monomorphized, inlined IR
    │  opt::builder::Builder::build(&mir, func_mir) per function
    ▼
Opt  (src/opt/) — sea-of-nodes SSA per function
    │  opt::scheduler::Scheduler::schedule(&opt, &mir)
    ▼
ScheduledFunction
    │  codegen::Builder::build(&mir, &interner) → Result<WasmModule, ()>
    ▼
WASM bytecode (WasmModule::encode() → Vec<u8>)
```

`std.wx` is embedded via `include_str!` in `lib.rs` as `STDLIB_SOURCE` and is always the first file in the `CompilationGraph`. It defines the `Memory` trait, `wasm` module intrinsics, `impl char` methods, and stdlib constants.

## Key modules (`crates/wx-compiler/src/`)

- **`ast/`** — lexer + parser → AST nodes
- **`tir/builder.rs`** — the largest file; prescan + demand-driven type checker and name resolver
- **`mir/mod.rs`** — desugaring: `+=` → explicit `=`, struct access → `AggregateGet`, `char` → `U32`, inlining, monomorphization, DCE
- **`opt/`** — sea-of-nodes SSA IR for per-function optimization (CSE via `ensure_node`, liveness, scheduling)
- **`codegen/mod.rs`** — WASM bytecode emitter; `Builder::build` is the entry point
- **`fmt/`** — pretty-printer for WX source (used by the LSP formatter)
- **`vfs.rs`** — `CompilationGraph` and file loading; `VirtualFileSource` for in-memory tests
- **`std.wx`** — standard library source embedded at compile time

## TIR resolution design

`tir/builder.rs` uses a prescan + demand-driven approach across four phases:

1. **Phase 1 — `pre_scan_item()`**: walks every item in every file and registers it into `builder.ast_nodes: HashMap<DefId, AstNodeRef>`. No type-checking; just populates the registry.
2. **Phase 2 — `ensure_signature(def_id)`**: called for every registered `DefId` in parse order. Demand-driven — `ensure_signature` is re-entrant safe (guarded by `sig_state: HashMap<DefId, ComputeState>`) so resolving one signature can pull in another on demand.
3. **Phase 3 — `ensure_body(def_id)`**: evaluates function bodies for every registered `DefId`.
4. **Phase 3.5 — `check_trait_conformance()`**: verifies every trait impl provides all required items.
5. **Phase 4 — exports**: processes `export { ... }` blocks after all signatures are resolved.

## Type system

Every type is a `TypeIndex` (u32) into `tir.type_pool`. The first 16 slots are pre-interned in `TypePool::new()` and MUST match the constants in `tir/mod.rs`. Never reorder them; add new pre-interned types at the end only.

| Constant | Index |
|---|---|
| `ERROR_IDX` | 0 |
| `UNIT_IDX` | 1 |
| `NEVER_IDX` | 2 |
| `UNKNOWN_IDX` | 3 |
| `U8_IDX` | 4 |
| `I8_IDX` | 5 |
| `U16_IDX` | 6 |
| `I16_IDX` | 7 |
| `U32_IDX` | 8 |
| `I32_IDX` | 9 |
| `U64_IDX` | 10 |
| `I64_IDX` | 11 |
| `F32_IDX` | 12 |
| `F64_IDX` | 13 |
| `BOOL_IDX` | 14 |
| `CHAR_IDX` | 15 |

`char` is a primitive in TIR but lowers to `U32` in MIR and WASM.

## Language features (current state from tests)

- Primitives: `i32`, `i64`, `u32`, `u64`, `f32`, `f64`, `bool`, `char`, `u8`, `i8`, `u16`, `i16`
- String literals have type `[]u8` (byte slice); there is no separate `string` type
- `local` / `local mut` declarations; `global` / `global mut` for module-level state
- `const` — compile-time evaluated, inlined at every reference site, never emitted as a WASM global
- Functions, `fn(T) -> U` type expressions (first-class function references)
- Structs, `impl` blocks, `pub fn` methods, `#[inline]` attribute
- Traits with default method bodies, associated types (`type Size: PointerSize`), associated consts, `impl Trait for Type`
- Generics / monomorphization — `fn f<T>(t: T) -> T`; `#[inline]` on generic functions is not currently propagated to mono instances (documented in tests)
- `module` declarations for multi-file compilation; `pub` visibility for cross-module access
- `memory` declarations — `memory heap: Memory32;` lowers to WASM linear memory
- `import "module" { fn ... }` — WASM imports; `export { fn, global }` — WASM exports (optionally renamed with `as "name"`)
- `#[intrinsic]` — marks functions in `module wasm { }` as WASM intrinsics (memory ops)
- Untyped integer/float literals coerced by context or via `as T` cast
- `as` casts: integer↔integer; `char`→`u8`/`u16`/`u32`; `u8`/`u16`→`char`; `u32 as char` is blocked
- `loop`, `break <value>`, `continue`, labeled blocks (`outer: { break :outer }`)
- Block expressions (last expression without `;` is the value)

## MIR passes (in order)

1. **Monomorphization** — generic functions instantiated per unique type-arg set via `MonoRegistry`
2. **Inlining** (`run_inlining_pass`) — Kahn topological sort of `#[inline]` call graph; cycle-breaking via anchor selection for mutual recursion
3. **DCE** — BFS from exported functions; unreachable functions removed from `mir.functions`

Struct layout uses alignment-sorted field ordering (fields sorted descending by alignment) to minimize padding.

String literals lower to a `[]u8` slice aggregate `{ StaticPointer, len }`. Static data (string literals, array constants) is currently always placed in `memories[0]` (the first declared memory).

## Testing patterns

Tests live in `#[cfg(test)]` modules at the bottom of each source file. The `TestCase` helper in `tir/tests.rs` and `mir/tests.rs` constructs a `CompilationGraph` (which automatically includes `std.wx`) and runs the pipeline:

```rust
// TIR test
let case = TestCase::new(indoc! { "fn add(a: i32, b: i32) -> i32 { a + b } export { add }" });
assert!(case.tir.diagnostics.is_empty());
insta::assert_yaml_snapshot!(case.tir);

// MIR test
let case = TestCase::new(indoc! { "..." });
assert_eq!(case.mir.functions.len(), 2);
insta::assert_yaml_snapshot!(case.mir);

// Multi-file TIR test
let case = TestCase::new_multi_file("src/main.wx", "module math;", &[("src/math.wx", "pub fn add() -> i32 { 1 }")]);
```

Snapshot files live in `src/tir/snapshots/` and `src/mir/snapshots/`. Never edit `.snap` files by hand. Any change to `std.wx` shifts byte offsets causing all snapshot tests to fail — regenerate with `INSTA_UPDATE=always`.

## Common pitfalls

- **Pre-interned `TypeIndex` ordering:** never insert in the middle of the `intern_type` sequence in `tir::build` — every downstream type check silently gets wrong types.
- **`ensure_signature` re-entrancy:** guarded by `sig_state`; an `InProgress` state means a cycle. Adding resolution code that calls `ensure_signature` recursively is safe only if cycles are handled.
- **`is_numeric_cast` is the gatekeeper for `as` casts** — only integer↔integer and `char`↔`u8`/`u16` are allowed (not `u32 as char`).
- **`pub fn` only:** impl methods without `pub` are not visible to user code via `Type::method()` call syntax.
- **`#[inline]` on generics** is currently not propagated to mono instances (documented bug in `mir/tests.rs`).
