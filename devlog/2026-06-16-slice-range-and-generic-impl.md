# 2026-06-16 — Slice range operator and generic impl blocks

## Summary

Two features implemented end-to-end: generic `impl<M, T> M::[]T` blocks (methods on
parameterized types) and the exclusive slice-range operator `arr[i..n]`. Also fixed a
latent lexer bug uncovered by the range syntax, and added a runtime bounds check for
the two-sided range form.

---

## Changes

| File | What changed |
|---|---|
| `src/ast/mod.rs` | `Token::DotDot`, `Expression::SliceRange`, `parse_index_expression` update, lexer float fix |
| `src/tir/mod.rs` | `GenericImplTargetKind`, `generic_impl_dispatch` index on `TIR`, `ExprKind::SliceRange` |
| `src/tir/builder.rs` | `ensure_signature` + `ensure_body` for `AstNodeRef::GenericImplMethod`, `find_generic_impl`, dispatch in `build_object_access_expression` and `build_call_expression`, `build_slice_range_expression` |
| `src/mir/mod.rs` | Full `tir::ExprKind::SliceRange` lowering including pointer arithmetic and bounds trap |
| `src/fmt/mod.rs` | `Expression::SliceRange` arm, `Item::Impl { .. }` pattern fix |
| `std.wx` | Added `impl<M: Memory, T> M::[]T { pub fn len(self) -> M::Size }` to stdlib |
| `src/tir/tests.rs` | TIR tests for generic impl dispatch, bare-type-param rejection, four slice range forms |
| `src/codegen/tests.rs` | Two WAT snapshot tests: `test_slice_range_wat`, `test_slice_range_array_wat` |

---

## Key findings

### Lexer: `1..3` was mis-tokenized as `Float(1.) + Dot + Int(3)`

`consume_number` was greedy on `.`: it consumed the first dot in `..` and returned a float
token. Fixed by peeking one character ahead before consuming:

```rust
'.' if !seen_dot => {
    let mut after = lookahead.clone();
    if after.next() == Some('.') { break; }  // it's `..`, not a decimal
    seen_dot = true;
    _ = self.chars.next();
}
```

### Generic impl dispatch — O(1) via `GenericImplTargetKind`

Problem: how to dispatch `slice.len()` without a linear scan over all generic impl blocks.

Solution: `GenericImplTargetKind` is an enum keyed by the *outer type constructor*
(`Slice`, `Pointer`, `Array`, `Struct(u32)`). A `HashMap<(GenericImplTargetKind, SymbolU32), usize>`
maps `(outer-kind, method-name)` → block index. Lookup is O(1).

When a method call resolves via this path, `infer_type_args_from_types` extracts the
concrete type arguments from the receiver type (e.g., `heap::[]u8` → `M = heap, T = u8`).
The TIR emits `ExprKind::GenericMethodCall { type_args, .. }` which MIR monomorphizes
exactly like a regular generic call.

### `impl<T> T { }` must be rejected

A bare type-param target has no nominal identity and would make dispatch ambiguous.
The check is in `ensure_signature`: after resolving the target type, if it is
`Type::TypeParam { .. }`, emit:

```
error: no nominal type found for inherent implementation
note: either implement a trait on it or create a newtype to wrap it instead
```

### TIR vs MIR responsibility for slice ranges

TIR's `build_slice_range_expression` does only semantic validation:
- object must be `Array` or `Slice`
- bounds, if present, must coerce to the memory's pointer-size type

It stores bounds as `Option<Box<Expression>>` — no default-filling.

MIR lowering fills in defaults:
- absent start → no offset (base_ptr unchanged)
- absent end → `Int { value: array_size }` for arrays, or `AggregateGet { field 1 }` for slices

### Slice range `s[..]` is not a codegen noop

`s[..]` is semantically a structural copy (same ptr, same len). The WAT output is 6
instructions instead of the optimal 2, because the codegen always spills each aggregate
field to a temp before returning. No arithmetic is emitted — just 4 redundant moves.
Not worth a special case unless this shows up hot.

### `i32.gt_s` for unsigned bounds check

`u32` maps to `ScalarType::I32` in the opt layer, and `ExprKind::Greater` always
produces `DataNodeKind::GtS` (signed). This is an existing limitation across all
comparisons in the language — the bounds check inherits it. Wrong for indices > 2^31-1
in theory; acceptable for now.

---

## Decisions

**TIR keeps `Option` bounds, MIR fills defaults.** Keeping the absent-bound signal in
TIR lets LSP features (hover, go-to-def) see the original intent. MIR is the right
place for semantics-altering defaults.

**`from > to` → `unreachable` trap, not panic.** No panic infrastructure exists yet.
WASM `unreachable` is immediate and non-recoverable, which is the right behavior for
now. The trap is only emitted for the two-sided `s[from..to]` form; `s[from..]` and
`s[..to]` are not checked (would need `to <= len`, deferred).

**`impl<M: Memory, T> M::[]T` lives in `std.wx`.** The generic impl for `.len()` was
added to the stdlib rather than requiring users to define it. Codegen tests still carry
their own minimal STD constant (no `@slice_len` needed — MIR lowering extracts the
length via `AggregateGet { value_index: 1 }` directly).

---

## Context for future sessions

**Type param ownership in generic impl methods.** Each method in a generic impl block
gets `TypeParamOwner::Function(its_own_def_id)`. The impl-level type params `M` and `T`
are re-registered per method. `param_index` (0=M, 1=T) is consistent across methods,
so `infer_type_args_from_types` still extracts substitutions correctly when given the
block's target type and the receiver type.

**Adding new generic impl dispatch indices.** `generic_impl_dispatch` is populated in
`ensure_signature` for `AstNodeRef::GenericImplMethod`. The key is
`(GenericImplTargetKind::from_type(target_type), method_name_symbol)`. If two generic
impl blocks define the same method on the same outer constructor, the second one silently
wins — no conflict detection yet.

**Slice internal representation.** In MIR/WASM, a `[]T` is an aggregate of two fields:
`[Pointer { memory }, U32/U64]` (pointer then length). Arrays lower to just
`Pointer { memory }` (the length is static and embedded in the type). The `SliceRange`
lowering in MIR exploits this directly.

**Synthetic scopes in MIR lowering.** The bounds-check `IfElse` needs a `Block` node
for its then-branch, which requires a scope index. Synthetic scopes can be pushed to
`func_ctx.frame` during lowering — they appear in the final `Function.scopes` and the
opt builder sizes its block vector from that count, so the index is valid.

---

## Open questions

- **`from > to` for `s[from..]`**: we currently skip the `from <= len` check. Once
  there's a panic mechanism this should be added. The length is already available at
  the check site (it's `AggregateGet { value_index: 1 }` of the slice local).
- **`i32.gt_s` for unsigned comparisons**: all `Greater`/`Less` nodes produce signed
  WASM instructions regardless of whether the MIR type is signed or unsigned. Fixing
  this requires either a new `ExprKind::GreaterU` / `LessU` in MIR or threading the
  signedness through `build_cmp` in the opt builder.
- **`s[..]` aggregate-spill optimization**: the codegen always materializes aggregate
  fields into temps before returning. For the specific case where all fields are plain
  `AggregateGet` reads of the same local, the temps are redundant. A codegen-level
  peephole could emit `local.get` directly.
- **`impl Trait for generic target`**: e.g. `impl<M, T> SomeTrait for M::[]T` — not
  yet started.
- **Slice single-element indexing on slices in MIR**: `tir::ExprKind::Index` lowering
  has `_ => unreachable!("index lowering only supported on arrays")`. Single-element
  indexing on a slice type (`s[i]` where `s: []T`) is accepted by TIR but panics in
  MIR. Needs a fix similar to the `SliceRange` array/slice split.
