# WX Compiler — AI Onboarding Guide

WX is a Rust-compiled language that targets WebAssembly. Its syntax is inspired by Rust, and the goal is to produce lean `.wasm` modules that closely reflect the WASM spec while staying high-level enough to be ergonomic. This document is written for an AI assistant starting fresh on this codebase.

---

## Repository layout

```
wx/
├── std.wx                        # Standard library (compiled-in at build time)
├── crates/
│   ├── wx-compiler/              # Core compiler library (the only crate you usually touch)
│   │   └── src/
│   │       ├── lib.rs            # Re-exports; embeds STDLIB_SOURCE from std.wx
│   │       ├── ast.rs            # Lexer, parser → AST
│   │       ├── tir.rs            # Type-checked IR (the big file, ~7000 lines)
│   │       ├── mir.rs            # Mid-level IR (desugaring, struct lowering)
│   │       ├── codegen.rs        # WASM bytecode emitter (currently pub-commented out in lib.rs)
│   │       ├── fmt.rs            # Debug/display formatting helpers
│   │       └── snapshots/        # insta snapshot files (*.snap)
│   ├── wx-compiler-cli/          # CLI binary: reads .wx → writes .wasm
│   ├── wx-compiler-wasm/         # WASM-compiled version of the compiler (for the playground)
│   └── wx-lsp/                   # Language server
├── examples/                     # Working .wx programs (fibonacci, pow, globals, …)
└── web/ cli/                     # JS/TS tooling around the compiler
```

---

## Compilation pipeline

```
source text
    │  ast::Parser::parse()
    ▼
AST  (ast.rs)
    │  tir::TIR::build(&[&stdlib_ast, &user_ast], &mut interner)
    ▼
TIR  (tir.rs) — type-checked, name-resolved IR
    │  mir::MIR::build(&tir, &interner)
    ▼
MIR  (mir.rs) — desugared, struct-flattened IR
    │  codegen::Builder::build(&mir, &interner)
    ▼
WASM bytecode (Vec<u8>)
```

**stdlib is always the first AST passed to `TIR::build`.** The order matters: stdlib items are registered before user items. The CLI and every test follow the pattern `TIR::build(&[&stdlib_ast, &user_ast], …)`.

---

## The language (wx syntax)

```wx
// Primitive types: i32 i64 u32 u64 f32 f64 bool char u8 i8 u16 i16
// string is a sealed built-in struct { ptr: []u8, len: u32 }

// Functions
fn add(a: i32, b: i32) -> i32 { a + b }

// Mutability on locals and params is explicit
fn countdown(mut n: i32) -> i32 {
  local mut result: i32 = 0;
  loop {
    if n == 0 { break result }
    result += n;
    n -= 1;
  }
}

// Globals (mutable module-level state)
global mut counter: i32 = 0;

// Constants (compile-time, integer or float, evaluated eagerly)
const MASK: u8 = 0b0010_0000;   // binary literals; _ separators allowed

// Structs
struct Point { x: u32, y: u32 }
fn make(x: u32, y: u32) -> Point { Point::{ x: x, y: y } }
fn get_x(p: Point) -> u32 { p.x }

// impl blocks — methods on primitives or structs
impl char {
  pub fn to_ascii_uppercase(self) -> char {
    if self >= 'a' && self <= 'z' {
      ((self as u8) ^ MASK) as char
    } else {
      self
    }
  }
}

// Imports
import "console" {
  fn log(message: string);
}

// Exports — only items listed here become wasm exports
export { add, countdown, counter }
export { add as "wasm_add" }  // rename on export
```

**Key language rules:**
- `char` is a primitive (lowered to `u32` in MIR/WASM). Comparisons and arithmetic work directly.
- Allowed casts: integer ↔ integer, `char` ↔ `u8`, `char` ↔ `u16`. `u32 as char` is intentionally blocked (unsafe).
- Integer overflow silently wraps (no debug/release distinction yet).
- Integer literals are untyped until coerced to a concrete type. Binary (`0b…`) and hex (`0x…`) literals supported; `_` separators allowed anywhere in the number.
- `local` declares a function-scoped variable. `global` declares a module-level variable.
- `const` is always immutable and evaluated at compile time; currently only integer and float values supported.
- Functions are first-class values: `fn(i32, i32) -> i32` is a valid type.

---

## tir.rs — the core file

This is where almost all compiler logic lives. It is structured as a single `Builder` struct that walks the AST in two passes:

### Two-pass design

```
Pass 1 — define_item()   runs for every item in every file
Pass 2 — build_item()    runs for every item in every file
```

`define_item` registers names into `symbol_lookup` (so later items can reference earlier-declared names). `build_item` evaluates bodies.

**Exception — constants:** consts are fully evaluated eagerly inside `define_item` (no deferred work in `build_item`) because constants always have a value and cannot be imported.

### Type system: TypeIndex and TypePool

Every type is represented as a `TypeIndex` (u32), an index into `type_pool: Vec<Type>`.

**Pre-interned constants** — these indices are hardcoded and MUST match the intern order in `TypePool::new()`:

| Constant       | Index | Type                    |
|----------------|-------|-------------------------|
| `ERROR_IDX`    | 0     | sentinel for errors     |
| `UNIT_IDX`     | 1     | `()` / no return        |
| `NEVER_IDX`    | 2     | diverging (`return`/`break`) |
| `UNKNOWN_IDX`  | 3     | unresolved (inference)  |
| `I32_IDX`      | 4     |                         |
| `I64_IDX`      | 5     |                         |
| `F32_IDX`      | 6     |                         |
| `F64_IDX`      | 7     |                         |
| `U32_IDX`      | 8     |                         |
| `U64_IDX`      | 9     |                         |
| `BOOL_IDX`     | 10    |                         |
| `CHAR_IDX`     | 11    | primitive; lowers to U32 in MIR |
| `STRING_IDX`   | 12    | `Struct { struct_index: 0 }` |
| `U8_IDX`       | 13    |                         |
| `I8_IDX`       | 14    |                         |
| `U16_IDX`      | 15    |                         |
| `I16_IDX`      | 16    |                         |

**If you add a new pre-interned type, add it at the end of `TypePool::new()` and add a matching constant.** Never reorder existing entries — it will silently corrupt all type checks.

### Symbol lookup

Two namespaces exist: `SymbolNamespace::Type` and `SymbolNamespace::Value`.

`symbol_lookup: HashMap<(SymbolNamespace, SymbolU32), GlobalValue>` maps interned symbol names to their meaning:

```rust
pub enum GlobalValue {
    Global { global_index: GlobalIndex },     // runtime module global
    Function { func_index: FunctionIndex },
    Namespace { namespace_index },             // import module alias
    Struct { struct_index: u32 },             // in Type namespace
    Const { const_index: ConstIndex },        // compile-time constant
    True, False,                              // bool literals
    Unreachable, Placeholder,                 // sentinels
}
```

Constants are stored separately from globals:
- `declared_consts: Vec<DeclaredConst>` — name and type
- `const_pool: Vec<ConstValue>` — the evaluated value (`ConstValue::Int(i64)` or `ConstValue::Float(f64)`)

Constants are inlined as `ExprKind::Int`/`ExprKind::Float` at every reference site — they never appear in MIR or WASM as module globals.

### String interning

All identifiers are interned via `string_interner::StringInterner` → `SymbolU32`. This interner is shared across all passes. `interner.resolve(symbol)` gives back the string; `interner.get_or_intern(s)` is idempotent.

### `string` — a special-cased built-in struct

`string` is defined in `std.wx` as struct index 0 (`BUILTIN_STRING_STRUCT = 0`). Its layout is special-cased in `compute_layout()` to always return pointer-pair size regardless of field types, because the WASM ABI passes strings as `(ptr, len)` pairs. If you touch `std.wx` struct definitions, the assertion in `TIR::build` will catch ordering violations.

### impl blocks

`impl_members: HashMap<TypeIndex, HashMap<SymbolU32, ImplEntry>>` stores methods. Both stdlib (`impl char { … }`) and user-defined impl blocks go here. Methods are resolved by target type index + method name symbol.

---

## mir.rs — mid-level IR

MIR's job (from the file's own comment): *desugar syntax like `x += 1` into `x = x + 1`, lower concepts like enums into primitive constants, convert labels from symbols in the interner into numeric indices.*

Key lowering rules:
- `char` (TIR) → `U32` (MIR)
- `u8`, `i8`, `u16`, `i16` → their own MIR types (wrapping on overflow is planned but not yet enforced in codegen)
- Struct field access on a local → `LocalTupleGet { field_index }`
- Struct initialization → `StructCreate { fields: Box<[Expression]> }`
- `string` locals are represented as tuples (ptr, len)

`MIR::build(&tir, &interner)` is the entry point.

---

## Testing

All tests live inside `#[cfg(test)]` modules at the bottom of each source file.

### TestCase helper (tir.rs and mir.rs)

```rust
let case = TestCase::new(indoc! {"
    fn add(a: i32, b: i32) -> i32 { a + b }
    export { add }
"});
// case.tir  — the TIR output
// case.mir  — the MIR output (mir tests)
assert!(case.tir.diagnostics.is_empty(), …);
```

`TestCase::new` always parses `std.wx` first, then the provided source. Tests run on the combined TIR.

### Snapshot tests (insta, yaml format)

```rust
insta::assert_yaml_snapshot!(case.tir);
```

Snapshots live in `src/snapshots/*.snap`. When the output changes legitimately:

```bash
INSTA_UPDATE=always cargo test -p wx-compiler
```

**After running `INSTA_UPDATE=always`**, strip `assertion_line:` from the new `.snap` files and delete any leftover `.snap.new` files to prevent them from causing spurious future failures:

```bash
find crates/wx-compiler/src/snapshots -name "*.snap.new" | while read f; do
  grep -v "^assertion_line:" "$f" > "${f%.new}"
  rm "$f"
done
```

**Never edit snapshot files by hand** — always let insta regenerate them.

### Wasmtime integration tests (mir.rs)

Some MIR tests compile all the way to WASM and execute with `wasmtime` to verify runtime behavior. The `TestCase` in mir.rs includes a `.wasm` field and `.call(fn_name, args)` helper.

---

## std.wx and how it interacts with the compiler

`std.wx` is embedded at compile time via `include_str!` in `lib.rs` as `STDLIB_SOURCE`. It is always compiled first. Currently it defines:

- `string` struct (must be struct index 0)
- `const ASCII_CASE_MASK: u8 = 0b0010_0000`
- `impl char { pub fn to_ascii_uppercase(self) -> char { … } }`

**Any change to std.wx shifts byte offsets**, which causes all snapshot tests to fail (span numbers change). This is expected — run `INSTA_UPDATE=always` and accept the new snapshots.

---

## Common pitfalls

- **Adding a pre-interned type:** add it at the END of the `TypePool::new()` list, add a matching `pub const …_IDX` constant. Never insert in the middle — every existing test snapshot will silently record wrong types.
- **Forgetting `pub` on impl methods:** only `pub fn` methods in impl blocks are visible to user code via `type::method()` call syntax.
- **GlobalValue exhaustiveness:** `GlobalValue` is matched exhaustively in `build_identifier_expression` and `build_const_expression`. Adding a new variant requires handling it in both places.
- **Two-pass order dependency:** `define_item` runs for ALL files before `build_item` starts. A const can only reference other consts defined earlier in file order (no forward references to consts).
- **`is_numeric_cast` is the gatekeeper for `as` casts.** Only integer↔integer and `char`↔`u8`/`u16` are allowed. Do not widen this without considering safety.
