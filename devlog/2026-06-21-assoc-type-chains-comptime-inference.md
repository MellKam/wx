# 2026-06-21 — Nested associated type chains, cast system cleanup, and cross-branch comptime inference

## Summary

Extended `AssocTypeProjection` from a flat `param_index: u32` to a recursive `base: TypeIndex`, enabling nested associated type access like `A::M::Size`. This unblocked a generic `Vec<T, A: Allocator>` design where the allocator's memory is accessed as `A::M`. Cleaned up the cast system with `are_scalar_compatible` to handle pointer↔pointer casts under abstract memory. Added `null_mut` to std. Fixed the formatter to emit `::` before type argument lists on intrinsic calls. Finished with a compiler fix allowing untyped integer literals to propagate across if-else branches when only one branch provides a concrete type.

## Changes

### Nested `AssocTypeProjection` chains (`tir/mod.rs`, `tir/builder.rs`, `mir/mod.rs`)

`AssocTypeProjection` previously stored `param_index: u32` — a direct index into the enclosing function's type parameters. This prevented chaining: `A::M` was representable but `A::M::Size` was not, because `A::M` is itself an `AssocTypeProjection`, not a `TypeParam`.

Changed the variant to carry `base: TypeIndex` instead:

```rust
AssocTypeProjection {
    trait_index: TraitIndex,
    assoc_name: SymbolU32,
    base: TypeIndex,   // TypeParam or another AssocTypeProjection
}
```

Ripple effects:

- **`write_type`**: recurses through `base` instead of indexing `type_params[param_index]`
- **`substitute_type`**: substitutes `base` first; if the result is a concrete struct/memory type, looks up `impl_members` to resolve the assoc type; if still abstract (TypeParam or AssocTypeProjection), rebuilds the node with the substituted base
- **`infer_type_args`**: match arm for AssocTypeProjection recurses into `base` / `actual_base`
- **`resolve_namespace_member`**: added an `AssocTypeProjection` arm that looks up the trait's assoc type definition, reads its bounds, and returns a new `AssocTypeProjection` with the outer node as `base`
- **`pointer_type_for_memory`**: uses `memory` TypeIndex directly as `base`
- **`mir::resolve_tir_type`**: new helper that recursively resolves through the `base` chain using `current_substitutions` and `impl_members`; `lower_type_index` and `resolve_memory_id` delegate to it

### `are_scalar_compatible` + `type_scalar` rename (`tir/mod.rs`, `tir/builder.rs`)

Renamed `type_primitive` → `type_scalar`, return type `WasmPrimitive` → `WasmScalar`. Concrete-memory pointers still return `Some(I32/I64)` so that `ptr as u32` casts continue to work; abstract-memory pointers return `None`.

Added `are_scalar_compatible(a, b) -> bool` for cast validity:

```rust
pub(crate) fn are_scalar_compatible(&self, a: TypeIndex, b: TypeIndex) -> bool {
    if a == b { return true; }
    match (&self.type_pool[a.as_usize()], &self.type_pool[b.as_usize()]) {
        (Pointer { to: a_to, memory: a_mem, .. }, Pointer { to: b_to, memory: b_mem, .. }) =>
            a_to == b_to && a_mem == b_mem,
        (Array { memory: a_mem, .. }, Array { memory: b_mem, .. }) =>
            a_mem == b_mem,
        _ => matches!((self.type_scalar(a), self.type_scalar(b)), (Some(x), Some(y)) if x == y),
    }
}
```

Pointer↔pointer and array↔array are checked structurally (same `to` + `memory` TypeIndex, any mutability) regardless of whether the memory type is concrete or abstract. Everything else falls back to `type_scalar` equality — which covers primitive↔primitive and concrete-memory pointer↔integer.

`build_cast_expression` now calls `are_scalar_compatible` instead of the old dual-branch match.

The primary motivation was `null_mut`: its body casts `M::*T` to `M::*mut T` — two pointers with abstract memory `M`. `type_scalar` returns `None` for both, so the old path rejected the cast. `are_scalar_compatible` accepts it because `to` and `memory` match.

### `null_mut` in `std.wx`

```wx
#[inline]
pub fn null_mut<M: Memory, T>() -> M::*mut T { @pointer_from::<M, T>(0) as M::*mut T }
```

The turbofish `::<M, T>` is required because `@pointer_from` returns `M::*T` (immutable), and the mutability mismatch blocked the `M` inference through `infer_type_args` (the pointer match arm guards on `pattern_mutable == actual_mutable`). Explicit type args bypass inference entirely.

### Formatter: `::` before intrinsic type args (`fmt/mod.rs`)

Intrinsic calls with type arguments now format as:

```wx
@size_of::<T, M>()
```

instead of `@size_of<T, M>()`. One-character change in the `IntrinsicCall` branch: `"<"` → `"::<"`.

### Cross-branch comptime inference in if-else (`tir/builder.rs`)

Integer literals have type `INTEGER` (a comptime placeholder) until coerced to a concrete type. The coercion normally happens via `scope.expected_type` or `scope.inferred_type` inside the block. When both are `INFER` — which happens when a local has no annotation and neither break values nor the outer context provide a type — `infer_block_type` previously emitted "type annotation required" immediately.

This meant `if cap == 0 { 4 } else { cap * 2 }` failed: the `{ 4 }` then-branch was built before the else-branch, so its scope had no type context even though `cap * 2` would have provided `A::M::Size`.

Four-part fix:

**1. `infer_block_type`**: when `coerce_to = INFER` (both scope fields are INFER), return `Ok(value.ty)` — let the comptime type bubble up instead of erroring immediately.

**2. `build_block_result`**: guard the trailing coerce call with `!inferred_type.is_comptime_number()` to prevent calling `coerce_untyped_expr(INTEGER)`, which would push a spurious "unable to coerce" diagnostic.

**3 & 4. Break and return handlers**: after `infer_block_type`, if the inferred type is still comptime (no context from breaks or outer scope), emit "type annotation required" explicitly. This preserves the error for `break 4` and `return 4` in contexts with no type information, since those are resolved at build time and can't be fixed by cross-branch inference.

**5. `coerce_untyped_expr`**: added `ExprKind::Block { result: Some(ref mut r) }` arm that recurses into the block's result and updates the block's `ty`. Handles `{ 4 }` and `{ { 4 } }` uniformly.

**6. `build_if_else_expression`**: after building both branches independently, if one is comptime and the other is concrete, call `coerce_untyped_expr` on the comptime branch:

```rust
if then_block.ty.is_comptime_number() && !else_block.ty.is_comptime_number() {
    self.coerce_untyped_expr(file_id, &mut then_block, else_block.ty)?;
} else if else_block.ty.is_comptime_number() && !then_block.ty.is_comptime_number() {
    self.coerce_untyped_expr(file_id, &mut else_block, then_block.ty)?;
}
```

Symmetric: neither branch is preferred. Both are built with whatever context the outer scope provides.

**Limitation**: comptime `break` values inside a branch are resolved at build time before the sibling branch's type is known. If a branch contains `break :label 4` and the only source of type information is the sibling, the break still requires a type annotation. A full tree walk would be needed to retroactively coerce break nodes, which was deemed too much overhead.

## Generic Vec example (`examples/vec/main.wx`)

Designed a generic `Vec<T, A: Allocator>` using associated types instead of generic traits (which WX doesn't support):

```wx
trait Allocator {
    type M: Memory;
    fn alloc(mut self, layout: Layout<Self::M>) -> Self::M::*mut u8;
    fn dealloc(mut self, ptr: Self::M::*mut u8, layout: Layout<Self::M>);
}

struct Vec<T, A: Allocator> {
    buf:   A::M::*mut T,
    len:   A::M::Size,
    cap:   A::M::Size,
    alloc: A,
}
```

The memory type is threaded through the allocator trait (`A::M`) rather than being a direct generic parameter. The `Layout` helper struct carries size/align. `BumpAllocator` implements `Allocator` with `type M = heap`. `Vec::new` uses `null_mut::<A::M, T>()` for the initial buffer.

## Key findings

- **`AssocTypeProjection` as a recursive type**: representing chained projections as `base: TypeIndex` (rather than a raw param index) makes the system composable — any depth of chaining works with the same substitution and resolution logic, just recursive calls.
- **Cast validity for abstract types**: `type_scalar` can't return a concrete scalar for abstract-memory pointers (we don't know if it's 32-bit or 64-bit). A structural check (`are_scalar_compatible`) that compares the type *shape* rather than the scalar kind handles these cases without special-casing.
- **Comptime inference deferral**: the existing block type machinery already supports deferred resolution via `scope.expected_type`. The if-else fix exploits this — after one branch is built with a concrete type, we coerce the other branch's already-built comptime result. This avoids re-building the block and avoids tree walks, at the cost of not handling comptime breaks inside the deferred branch.

## Open questions / todos

- **Flatten supertrait bounds**: `TypeParamInfo.bounds` only stores direct bounds; `type_param_satisfies_bound` doesn't traverse supertrait chains. Tests currently pass vacuously via type unification. Fix: during `ensure_signature` for each trait, walk supertraits transitively and append to bounds vec. Documented in `notes/todos.md`.
- **Vec push/get implementation**: the `push` method stub and `get` call in `demo()` are not yet implemented — push needs grow-and-copy logic and get needs pointer arithmetic. The type-level structure (all the `A::M::*mut T` fields and the Allocator trait) now resolves correctly.
