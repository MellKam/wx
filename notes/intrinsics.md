# Intrinsic System Design

## Current state (as of this writing)

### Syntax

Intrinsics are declared with `fn @name` syntax (no body):

```rust
pub fn @memory_grow<M: Memory>(mem: M, delta: M::Size) -> M::Size;
pub fn @memory_size<M: Memory>(mem: M) -> M::Size;
```

Called with `@name()` syntax:

```rust
@memory_grow(heap, delta)
@memory_size(heap)
```

### Pipeline status

| Stage | Status |
|---|---|
| Lexer (`Token::AtIdent`) | Done |
| AST (`Expression::IntrinsicCall`, `Item::IntrinsicFunction`) | Done |
| Formatter | Done |
| TIR Phase 1 — `pre_scan_item` for `IntrinsicFunction` | Done |
| TIR Phase 2 — `ensure_signature` for `IntrinsicFunction` | Done |
| TIR builder — `Expression::IntrinsicCall` handler | **todo!() stub** |
| MIR lowering — `IntrinsicCall` | Not started |
| Codegen | Not started |

## What needs to be implemented next

### 1. TIR `ExprKind::IntrinsicCall`

Add to `tir/mod.rs`:

```rust
IntrinsicCall {
    name: SymbolU32,           // e.g. intern("@memory_grow")
    type_args: Box<[TypeIndex]>,   // resolved M, T, etc.
    arguments: Box<[Expression]>,
},
```

### 2. TIR builder — handle `ast::Expression::IntrinsicCall`

In `tir/builder.rs`, the `todo!("intrinsic call")` stub needs to:
- Look up the `@name` symbol in `symbol_lookup` to find the registered `IntrinsicFunction`
- Use the function's signature to type-check arguments
- Resolve type arguments (e.g. infer `M` from the memory argument)
- Return `ExprKind::IntrinsicCall { name, type_args, arguments }`

### 3. MIR — `Intrinsic` enum

Add in `mir/mod.rs` (or `mir/intrinsics.rs`):

```rust
enum Intrinsic {
    MemoryGrow,
    MemorySize,
}

impl Intrinsic {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "@memory_grow" => Some(Self::MemoryGrow),
            "@memory_size" => Some(Self::MemorySize),
            _ => None,
        }
    }
}
```

### 4. MIR lowering — handle `tir::ExprKind::IntrinsicCall`

Key subtlety: `mem: M` in `@memory_grow(mem, delta)` and `@memory_size(mem)` is NOT
a real WASM stack value. WASM `memory.grow`/`memory.size` encode the memory index in
the instruction itself. So:

- Extract memory `DefId` from `type_args[0]` (the resolved `M` type), not from the
  stack value.
- For `@memory_grow`: lower only `arguments[1]` (delta) as a stack value.
- For `@memory_size`: no stack arguments at all.
- Discard the `mem` argument entirely at the MIR boundary.

The old `memory_index_from_arg` helper (deleted during `FunctionAttribute::Intrinsic`
removal) did this — extracted memory `DefId` from a `TypeIndex` by resolving
`TypeParam` substitutions. Something equivalent is needed here, working off `type_args[0]`.

## Design decisions already made

- `FunctionAttribute::Intrinsic` is **removed**. New intrinsics use `fn @name()` syntax
  exclusively. `#[intrinsic]` on `FunctionDeclaration` no longer works.
- The `Intrinsic` enum lives in **MIR**, not TIR. TIR records the raw symbol name;
  MIR resolves it to the enum. This keeps WASM-specific knowledge out of the type
  checker.
- `@` is kept in the interned symbol (e.g. `@memory_grow`), so declaration and call
  sites share the same symbol without any stripping.
- `std.wx` still has old `module wasm { #[intrinsic] ... }` blocks that need to be
  removed once the new intrinsic call lowering is working end-to-end.
