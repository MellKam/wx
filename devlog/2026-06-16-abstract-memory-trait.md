# 2026-06-16 — Abstract memory trait, impl Trait removal, indexing fix

## Summary

Continued from the previous session which had implemented abstract pointer types (`memory: TypeIndex` instead of a concrete `DefId` in TIR). This session cleaned up the fallout from that change, removed the `Memory32`/`Memory64` sugar traits, removed `impl Trait` syntax sugar, and fixed array/slice indexing to work through generic `M: Memory` type parameters.

## Changes

### Removed `Memory32` / `Memory64` sugar traits

`std.wx` previously defined `trait Memory32: Memory<Size = u32> {}` and `trait Memory64: Memory<Size = u64> {}` as convenience aliases. These were removed. Memory declarations now use the full syntax:

```wx
memory heap: Memory<Size = u32>;
memory stack: Memory<Size = u64>;
```

All test files updated accordingly. The supertrait seeding loop in `seed_memory_trait_impl_with` was also removed (it only existed to propagate bindings from `Memory32: Memory<Size = u32>` up the chain — no longer needed). The dead `seed_memory_trait_impl` wrapper function was deleted.

### `TypeParamOwner::Trait` for trait constant `Self`

Trait constants like `const DATA_END: Self::*mut u8` need `Self` to be a valid type parameter so `Self::*mut u8` passes the `MemoryTagged` check. Added `TypeParamOwner::Trait(TraitIndex)` variant. Trait methods keep `TypeParamOwner::Function` (load-bearing for monomorphization — Self is stored at `type_params[0]` and substituted via `type_args[0] = concrete_receiver` at call sites).

### Removed `impl Trait` syntax sugar

`impl Trait` in parameter position (e.g. `fn f(x: impl Foo)`) was removed. Users must use explicit generic parameters: `fn f<T: Foo>(x: T)`. Parser now falls through to `intern_identifier` for the `impl` keyword, which emits E0008 "cannot use keyword as identifier". The `TypeExpression::ImplTrait` AST variant, TIR resolver arm, and formatter arm were all deleted.

### Dead code cleanup

- Removed `ObjectAccess` branch for `"OFFSET"` in MIR (dot-access for memory constants was disallowed; `DATA_END` is namespace-access only)
- Updated stale `supertrait_bindings` doc comment that referenced `Memory32`
- Updated `ast/tests.rs` to use `Memory<Size = u32>` syntax
- Fixed orphaned snapshot file for deleted `test_impl_trait_param` test

### CLAUDE.md snapshot workflow

Updated from the broken `INSTA_UPDATE=always` approach to the correct:
```bash
cargo test -p wx-compiler
cargo insta accept
```

### Array literal coercion tests un-ignored

6 `#[ignore]` tests for array literal type coercion were found to already pass. Annotations removed (382 → 376 passing after impl Trait test removals, then climbing back up).

### Generic `M: Memory` indexing fix

`build_index_expression` calls `pointer_type_for_memory(memory_typeidx)` to determine what type the index expression must have. Previously it returned `TypeIndex::INTEGER` for non-concrete memories (TypeParam), making `arr[some_u32_var]` fail for generic arrays.

**Fix in `pointer_type_for_memory`** (`tir/builder.rs`): when memory is a `TypeParam`, look through the param's bounds for a trait that has `Size` as an associated type, then return `AssocTypeProjection { trait_index, assoc_name: Size, param_index }`. MIR `lower_type_index` already handles `AssocTypeProjection` by substituting via `current_substitutions` → correct concrete type at monomorphization time.

**Open question**: coercing an untyped integer literal (`slice[0]`) to `M::Size` hits the `else` branch in `coerce_untyped_int_expr` and errors. Design decision not yet made: should `-1` be rejected at definition time (it's always invalid for unsigned PointerSize types), and should large values (`> u32::MAX`) be accepted speculatively? Not implemented this session.

### wx-lsp-next fix

`TypeParamOwner::Trait` variant was unhandled in the LSP hover handler (`main.rs:1154`). Added `Trait(_) => return Some("Self".to_string())`.

## Key findings

**`seed_memory_trait_impl_with` bindings propagation**: when a memory declaration is processed, the `Size = u32` binding from `Memory<Size = u32>` must be passed to `seed_memory_trait_impl_with` so that `ImplEntry::AssociatedType { ty }` entries in `impl_members[memory_type]` get substituted to concrete types. Without this, `M::Size` in method signatures fails to resolve to `u32` — tests fail with "unable to coerce to type Memory::Size".

**`AssocTypeProjection` in MIR is fully supported**: `lower_type_index` at `mir/mod.rs:903` resolves projections through `current_substitutions[param_index]` → `impl_members[receiver][assoc_name]`. Integer literals stamped with a projection type would work correctly after monomorphization — they just need TIR coercion to accept them.

**`TypeIndex::INTEGER` is `unreachable!()` in MIR**: `lower_type_index(INTEGER)` panics. Any TIR expression that ends up typed as `INTEGER` at MIR lowering time is a bug. The array index fallback returning `INTEGER` for unrecognized TypeParam bounds was technically broken even before our fix.

## Decisions

- `impl Trait` sugar removed outright (not gated or deprecated). The language design favors explicit generics. Parse-level error is "cannot use keyword as identifier" via the existing `intern_identifier` path — no special diagnostic.
- `TypeParamOwner::Trait` only for trait constants, not trait methods. Trait method Self stays `Function`-owned because monomorphization depends on `type_params[0] = Self` and `type_args[0] = concrete_receiver`.
- Associated constants (`DATA_END`, `MEMORY_INDEX`) are namespace-access only (`heap::DATA_END`), not dot-access (`heap.DATA_END`). The dead dot-access MIR branch was removed.

## Open questions

- **Literal coercion to `M::Size`**: `slice[0]` in a generic `fn f<M: Memory>` doesn't yet work. Decision needed: reject negatives at definition time, defer range check to monomorphization, or be conservative and only allow `[0, u32::MAX]`.
- **`ensure_impl_members` SIZE/ALIGN injection**: still a compiler-internal hack (synthetic `Constant` entries created per-type). A `Sized` lang-item trait defined in `std.wx` would be the clean solution.
- **`MemoryKind::Memory32/Memory64` enum**: still a hardcoded 2-variant enum. Could eventually carry a `pointer_size: u32` derived directly from the `Size` binding, removing the constraint that only `u32` and `u64` are valid.
- **`lang_items` map**: populated via `#[lang = "memory"]` / `#[lang = "pointer_size"]` but never read. Dead infrastructure.
- **Generic struct methods**: one ignored test (`test_generic_struct_method`) still unimplemented.
