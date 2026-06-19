# 2026-06-19 — `#[inline]` on generics, Vec example, and formatter fixes

## Summary

Fixed `#[inline]` not being propagated to monomorphized generic function instances, which unlocked removing the special-case memory method handling in MIR. Unified the receiver field into `arguments[0]` for `GenericMethodCall`/`MethodCall` in TIR `ExprKind`. Removed auto-generated `SIZE`/`ALIGN` compiler constants. Updated the bump allocator example to use `@size_of`/`@align_of` intrinsics, then wrote a new generic `Vec<M: Memory, T>` example. Fixed the source formatter to emit generic type parameters for structs and impl blocks.

## Changes

### `#[inline]` propagation to mono instances (`mir/mod.rs`)

The monomorphization loop created new `FuncId`s for each instance but never registered them in `inline_functions`. Callers checking `inline_functions.contains(&callee_id)` would always miss mono instances, so they were never inlined.

Fix: after lowering each mono instance, check if the original TIR function had `Inline` attribute and insert the new `mono_id` into `inline_functions`.

```rust
let is_inline = tir_func.attributes.iter().any(|a| *a == tir::ItemAttribute::Inline);
// ... lower function ...
if is_inline { inline_functions.insert(mono_id); }
```

`call_edges` were already correct — `record_call_edge(mono_id)` is called when callers are lowered, so the call graph already knew about mono instances. Only the `inline_functions` set was missing them.

Updated the test from `test_inline_attribute_on_generic_not_propagated_to_mono_instance` (expecting 2 functions) to `test_inline_attribute_on_generic_propagated_to_mono_instance` (expecting 1 — the instance inlined away).

### Removed memory method special case (`mir/mod.rs`)

Previously, `grow` and `size` method calls on memory objects had a 36-line special-case block in MIR that emitted `MemoryGrow`/`MemorySize` instructions directly. With `#[inline]` now propagating to mono instances, the methods inline correctly through the normal path. The special-case block was dead code and was removed.

### Receiver unified into `arguments[0]` (`tir/mod.rs`, `tir/builder.rs`, `mir/mod.rs`)

`GenericMethodCall` and `MethodCall` in `ExprKind` previously had a separate `object: Box<Expression>` field alongside `arguments`. Removed `object` from both variants; the receiver is now always `arguments[0]`.

```rust
GenericMethodCall {
    id: DefId,
    type_args: Box<[TypeIndex]>,
    arguments: Box<[Expression]>,   // [0] = receiver
},
MethodCall {
    arguments: Box<[Expression]>,   // [0] = receiver
    id: ast::DefId,
},
```

Creation sites in `builder.rs` prepend the receiver with `std::iter::once(*object).chain(...)`. MIR lowering simplified from `std::iter::once(lowered_object).chain(arguments...)` to a plain `.map().collect()`. Measured `ExprKind` size at **40 bytes** actual (Rust Analyzer reported 48, missing the `Option<Box<>>` niche optimization).

### Removed auto-generated `SIZE` and `ALIGN` constants (`tir/builder.rs`, `mir/mod.rs`)

`ensure_impl_members` was a 90-line function that synthesized `SIZE` (struct byte size) and `ALIGN` (struct alignment) as compiler-generated associated constants on every struct impl block. Removed the function and its four call sites, replacing each with a direct `impl_members.get(&ty).and_then(|m| m.get(&sym)).cloned()`. The `NamespaceAccess` `_` arm in MIR that computed layout for `SIZE`/`ALIGN` lookups was also removed. Deleted `test_size_associated_const` and `test_size_align_constants` and their snapshots.

### Bump allocator updated to use intrinsics (`examples/bump_allocator/bump_allocator.wx`)

Replaced four separate typed allocators (`alloc_i32`, `alloc_u64`, `alloc_point`, `alloc_node`) with a single generic `alloc<T>()` that uses `@size_of<T, heap>()` and `@align_of<T, heap>()`:

```wx
pub fn alloc<T>() -> heap::*mut T {
    local align = @align_of<T, heap>();
    local ptr = (bump as u32 + align - 1) / align * align;
    local new_end = ptr + @size_of<T, heap>();
    local cur_end = heap.size() * PAGE_SIZE;
    if new_end > cur_end {
        local needed = (new_end - cur_end + PAGE_SIZE - 1) / PAGE_SIZE;
        local _grew = heap.grow(needed);
    }
    bump = new_end as heap::*mut u8;
    ptr as heap::*mut T
}
```

Removed most type annotations from locals (compiler infers from context). Compiles to 303 bytes.

### Generic Vec example (`examples/vec/vec.wx`)

New file demonstrating generics, `@size_of`/`@align_of`, pointer arithmetic, and a custom allocator. `Vec` is generic over both memory and element type:

```wx
struct Vec<M: Memory, T> {
    buf: M::*T,      // null when empty; cast to *mut T at write sites
    len: M::Size,
    cap: M::Size,
}
```

`buf` is stored as immutable `M::*T` (not `M::*mut T`) because initializing a generic pointer type from literal `0` isn't supported; `null()` works for immutable pointers. Cast to `*mut T` at write sites.

Capacity doubling: `cap + 4` when `cap == 0` instead of `if cap == 0 { 4 } else { cap * 2 }` — the arithmetic trick forces the untyped `4` to be inferred as `M::Size` rather than leaving an ambiguous branch. Demo pushes five values and returns their sum (150). Compiles to 566 bytes.

### Formatter: generic type params for structs and impl blocks (`fmt/mod.rs`)

Extracted a `build_type_params(out, interner, type_params)` helper (replacing inlined code in `build_function_signature`) and called it from:

- `build_struct_declaration` — emits `struct Vec<M: Memory, T>`
- `build_impl_definition` — emits `impl<M: Memory, T> Vec<M, T>`

`Item::ImplTrait` has no `type_params` field in the AST and was left unchanged.

## Key findings

- **Mono instances and `inline_functions`**: the call graph (`call_edges`) tracks mono instances correctly because edges are recorded when the *caller* is lowered. Only the separate `inline_functions` set was missing mono IDs — a one-line fix.
- **Receiver-as-args[0]**: the two `GenericMethodCall` creation sites in `builder.rs` needed different treatment. The first site already built a `Vec<Expression>` with the receiver at `[0]`; the second and `MethodCall` had a separate `Box<Expression>` requiring `std::iter::once(*object).chain(...)`. MIR lowering simplified cleanly in both cases.
- **Generic pointer null init**: `0 as M::*mut T` fails for type params because the cast requires a concrete type. Storing as `M::*T` and using `null()` (which works for any pointer type) is the clean workaround.
- **Untyped literal inference in branches**: `if cap == 0 { 4 } else { cap * 2 }` — the `4` literal in the first branch can't be inferred as `M::Size` without seeing the other branch. Using `cap + 4` forces inference from `cap`'s type.

## Decisions

- **Syntax distinction preserved**: `fn::<T>(args)` uses turbofish `::` for regular functions; `@intrinsic<T>(args)` uses bare `<>`. This inconsistency is acknowledged but left for a future syntax pass.
- **`M::*T` not `M::*mut T` for Vec buf**: immutable pointer field avoids the null-init problem cleanly. All mutation goes through a local `let buf_mut = buf as *mut T` cast. Value semantics on `Vec` structs mean the struct is copied on push anyway.
- **`SIZE`/`ALIGN` removed entirely**: the feature was redundant once `@size_of`/`@align_of` intrinsics existed. No migration path needed — no user code relied on it.
