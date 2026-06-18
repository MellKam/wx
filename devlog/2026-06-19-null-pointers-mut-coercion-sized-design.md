# 2026-06-19 — Null pointers, `*mut T → *T` coercion, and `Sized` trait design

## Summary

Cleaned up the bump allocator example to use proper null-terminated linked lists, which drove two real language features: a `null()` intrinsic and implicit coercibility from mutable to immutable pointers. Closed the session with a design discussion on typeset-bounded constants and a compiler-auto-implemented `Sized` trait.

## Changes

### `*mut T → *T` implicit coercion (`tir/builder.rs`)

Added a rule to `coercible_to` so that `*mut T` coerces implicitly to `*T` when the pointee type and memory type match. One rule propagates everywhere: function call arguments, assignments, struct literal fields, block returns — all checked through `coercible_to`.

```rust
if let (
    &Type::Pointer { to: a_to, mutable: true, memory: a_mem },
    &Type::Pointer { to: b_to, mutable: false, memory: b_mem },
) = (&self.tir.type_pool[a.as_usize()], &self.tir.type_pool[b.as_usize()]) {
    if a_to == b_to && a_mem == b_mem { return true; }
}
```

### Mixed-mutability pointer comparison (`tir/builder.rs`)

`build_comparison_binary_expr` previously required `left_type == right_type` for pointer `==`/`!=`. Extended the match arm to accept same-pointee, same-memory pointers regardless of mutability. This lets `cur == null()` work when `cur: *mut Node` and `null()` returns `*Node`.

### `examples/bump_allocator/bump_allocator.wx`

Rewrote the linked list section to use `null()` and a recursive `*Node` pointer field instead of the old `u32` workaround:

```wx
struct Node {
    value: i32,
    next: heap::*Node,   // null() when there is no next node
}
```

`node_new` takes `next: heap::*Node` (immutable); the caller passes `n1` (a `*mut Node`) and the coercion handles it. No explicit `as` casts anywhere. Compiles to 374 bytes.

## Key findings

- **Coercion point centralisation**: `coercible_to` is the single gate for all implicit type conversions. Adding one rule there is enough — no need to patch individual expression builders.
- **Pointer comparison asymmetry**: `null()` always returns an immutable pointer (`*T`), so any comparison against a `*mut T` variable would fail without the mixed-mutability comparison rule. The two fixes (coercion + comparison) are complementary and both needed.
- **LSP hover for trait methods**: trait method implicit `Self` type param is stored with the hardcoded symbol `"Self"` (TIR builder ~line 3470). The `Self` *keyword* resolution at line 1580 is independent of this stored name, so the hover display showing `fn grow<Self: Memory>(...)` is technically correct. Decided to leave as-is.

## Decisions

- **`*mut T → *T` coercion is one-way**: mutable-to-immutable only, matching Rust semantics. Returning a mutable pointer where an immutable is expected is safe; the reverse is not allowed implicitly.
- **LSP hover `Self` left unchanged**: the `Self` name in trait method type params accurately reflects the language semantics. Changing it would require either renaming the stored symbol (cosmetic) or adding a new language feature for named trait type params (`trait<M> Memory { ... }`). Not worth it now.

## Design discussion: `Sized` trait and typeset-bounded constants

Discussed two related features for `std.wx`:

**Typeset bounds on associated consts** — `const SIZE: PointerSize` in a trait means "the impl must supply SIZE as some concrete type in PointerSize." In user-written impls, the type would be inferred from the literal value (same mechanism as untyped integer literals). No new syntax in impls needed.

**Compiler-auto-impl of `Sized`** — `Sized` (with `const SIZE: PointerSize` and `const ALIGN: PointerSize`) should be implemented by the compiler for every concrete type, not by users. Implementation sketch:
1. After TIR Phase 2, walk all registered concrete types
2. Compute size/align from the same field-layout logic MIR already uses (alignment-sorted fields)
3. Synthesize `TraitImplInfo` entries into `tir.trait_impls` — identical to user-written impls
4. Primitive sizes hardcoded; pointer types → `u32` (Memory32) / `u64` (Memory64)
5. For generics: generate the impl per monomorphized instance during MIR

Open question: since WX currently targets WASM32 exclusively, `SIZE`/`ALIGN` could be hardcoded as `u32` to avoid the memory-parameterization problem. Defer the `u64` path until Memory64 support is needed.

**Primary use case for `Sized`**: generic allocation — `fn alloc_one<T: Sized>() -> *mut T { alloc(T::SIZE) as *mut T }` — so users don't need per-type `alloc_foo()` helpers.

## Open questions

- Should `const SIZE: PointerSize` in a trait require an explicit type annotation in user-written impls, or should the type always be inferred? The auto-impl case avoids the question, but user-written impls of other traits with typeset-bounded consts will hit it.
- `@size_of<T>()` as a compiler intrinsic vs. `T::SIZE` via `Sized` — the intrinsic is simpler to implement but the trait approach is more composable. Worth deciding before implementation starts.
