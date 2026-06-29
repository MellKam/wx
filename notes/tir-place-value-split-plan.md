# TIR Place / Value Split — Implementation Plan

> **Status: COMPLETE** — all 6 steps done, 518/518 tests pass.
> `wx-fmt` updated to format `.&` / `.&mut` (see `DotAmp` / `DotAmpMut` text variants).

See [tir-place-value-split.md](./tir-place-value-split.md) for the design spec.

---

## Strategy: additive-first, subtractive-last

Never remove an old variant until all its producers and consumers have been migrated.
The codebase must compile after every step.

```
Step 1  Add new data model          tir/mod.rs                    (additive only)
Step 2  Update TIR producers        tir/builder.rs                (largest step)
Step 3  Update MIR consumers        mir/mod.rs                    (add + simplify)
Step 4  Remove old variants         tir/mod.rs + guided cleanup   (subtractive)
Step 5  Regenerate snapshots        cargo insta
Step 6  Full test run               cargo test -p wx-compiler
```

> **Note on LSPs:** `wx-lsp` is the legacy LSP and does not need to be maintained.
> `wx-lsp-next` works through the `SymbolIndex` and `TIR` public API only — it never
> pattern-matches on `ExprKind` variants — so it requires no changes at all.

---

## Step 1 — Add the new data model (`tir/mod.rs`)

Purely additive. Nothing breaks.

- Add `Place` struct and `PlaceKind` enum as specified in the design doc.
- Add three new `ExprKind` variants: `Load`, `AddressOf`, `Store`.
- Keep `ExprKind::Deref` and `ExprKind::Index` — they are removed in Step 5.
- Add `#[cfg(test)] derive(serde::Serialize)` and `derive(Debug)` to the new types.

---

## Step 2 — Update all TIR producers (`tir/builder.rs`)

Six functions change. After this step `ExprKind::Deref` and `ExprKind::Index` are no
longer emitted anywhere in the builder — they still exist in the enum so the MIR
compiles, but they are dead.

### `build_deref_expression`
Build a `PlaceKind::Deref` node. Extract `memory` and `mutable` from `pointer.ty`
(it resolves to `Type::Pointer { memory, mutable }`). Wrap in `ExprKind::Load`.

### `build_object_access_expression` *(trickiest)*
After building the object expression, inspect its `kind`:
- `ExprKind::Load { place }` — object came from memory. Build `PlaceKind::Field` on
  top of that place (inheriting `memory` and `mutable`), return `ExprKind::Load`.
- anything else — object is a stack value. Keep the existing `ExprKind::ObjectAccess`
  path (local struct AggregateGet). This is the only case `ObjectAccess` covers after
  the refactor.

### `build_index_expression`
Build a `PlaceKind::Index` node. Wrap in `ExprKind::Load`.

### `build_assignment_expr`
The left-hand side discrimination currently matches `ExprKind::Deref` and
`ExprKind::Index` explicitly. Replace both with a single `ExprKind::Load { place }` arm
that emits `ExprKind::Store { target: place, value }`. The inner guard on the
`ExprKind::ObjectAccess` arm (which currently checks `object.kind == Deref | Local`)
simplifies to `Local` only — the Deref sub-case no longer reaches it.

### `build_arithmetic_assignment_expr`
Same changes as `build_assignment_expr` for `+=` / `-=` / `*=` etc.

### `build_address_of_expression` *(new — replaces `todo!()`)*
The operand expression must be `ExprKind::Load { place }`. If it is anything else,
emit a diagnostic: *"cannot take address of a stack value"*. Then:
- If `.&mut` but `!place.mutable` — emit *"cannot take mutable reference through
  immutable pointer"*.
- Otherwise return `ExprKind::AddressOf { place, mutable }` with type
  `Type::Pointer { to: place.ty, memory: place.memory, mutable }`.

---

## Step 3 — Update MIR consumers (`mir/mod.rs`)

Three functions change. Old arms become dead code but stay until Step 5.

### Extract `lower_place_address` helper *(do this first)*
The address arithmetic for `Field` and `Index` places is currently duplicated across
`lower_assignment` and `lower_compound_assignment`. Extract it into a shared:

```rust
fn lower_place_address(
    &mut self,
    place: &tir::Place,
    func_ctx: &mut FunctionContext,
    sink: &mut Vec<Expression>,
) -> (Expression, u32, ast::DefId)  // (base_ptr, static_offset, memory_id)
```

`Load`, `AddressOf`, and `Store` all call this instead of duplicating the logic a
third time.

### `lower_expression`
- Add `tir::ExprKind::Load { place }` arm — call `lower_place_address`, emit
  `PointerLoad`.
- Add `tir::ExprKind::AddressOf { place, .. }` arm — call `lower_place_address`,
  return the pointer value directly (no load).
- Simplify the existing `tir::ExprKind::ObjectAccess` arm: remove its inner branch for
  the `Deref` sub-case (now dead). Only the `AggregateGet` path remains.

### `lower_assignment` and `lower_compound_assignment`
- Add `tir::ExprKind::Store { target, value }` arm — call `lower_place_address`, emit
  `PointerStore`.
- Simplify the existing `ObjectAccess` arm: remove the `Deref` sub-case, keep only
  `Local` → `AggregateSet`.

---

## Step 4 — Remove old variants and clean up

Remove `ExprKind::Deref` and `ExprKind::Index` from `tir/mod.rs`. The compiler will
produce exhaustiveness errors on every remaining match — use those as a precise
checklist to find and delete dead code:

- `tir/builder.rs` — remove stale `ExprKind::Deref { .. }` and `ExprKind::Index { .. }`
  reconstruction inside `build_assignment_expr` and `build_arithmetic_assignment_expr`.
- `mir/mod.rs` — remove the now-dead `tir::ExprKind::Deref` and `tir::ExprKind::Index`
  arms from `lower_expression`, `lower_assignment`, `lower_compound_assignment`.

`wx-lsp-next` needs no changes (it never matches on `ExprKind`). `wx-lsp` is legacy
and is not maintained.

After Step 4 the codebase is clean. `ObjectAccess` means only local struct field access
everywhere.

---

## Step 5 — Regenerate snapshots

The serialized TIR shape changes substantially. Every existing TIR snapshot fails.

```bash
INSTA_UPDATE=always cargo test -p wx-compiler
cargo insta review
```

Review each snapshot — they should all be structurally correct, just using the new node
names (`Load`, `Store`, `AddressOf`, `PlaceKind::Deref` / `Field` / `Index`).

---

## Step 6 — Full test run

```bash
cargo test -p wx-compiler
```

All tests should pass. Any snapshot rejected during review shows up here.

---

## Risk areas

| Area | What to watch |
|---|---|
| `build_object_access_expression` | Must correctly distinguish `ExprKind::Load` (memory place) from all other kinds (stack value). Calls that return a struct by value must stay as `ObjectAccess`, not become `Load`. |
| `build_assignment_expr` `ObjectAccess` inner guard | Currently checks `Deref \| Local \| Global`. After Step 2 only `Local` is valid — tighten the guard and add an error diagnostic if `Global` was never reachable. |
| `lower_place_address` helper | Extract before writing the three consumer arms to avoid triplicating the address arithmetic. |
| Snapshot volume | All TIR snapshots break at once. Budget time for `cargo insta review`. |
