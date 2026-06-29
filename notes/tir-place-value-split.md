# TIR Place / Value Split

## Motivation

Two problems in the current TIR that share the same root cause — `ExprKind` makes no
structural distinction between expressions that produce a stack value and expressions
that refer to a location in linear memory (a *place*):

1. **`AddressOf` cannot be type-safely implemented.** There is no way to enforce at the
   type level that `.&` is only called on a memory place. A runtime recursive walk or a
   stored metadata flag is needed as a workaround.

2. **`ptr.*.field` lowers inefficiently.** In `lower_expression`, `ObjectAccess` whose
   object is a `Deref` falls into the generic branch: it loads the whole struct from
   memory into a temp local, then extracts the field with `AggregateGet`. The correct
   lowering is a single `PointerLoad` at `ptr + field_offset` — the same computation
   that `lower_assignment` already does correctly for stores.

The fix is to introduce a separate `Place` type, mirroring what Rust's THIR and MIR do.

---

## Background — why Rust has HIR, THIR, and MIR

Rust's compiler has three semantic IRs:

- **HIR** — produced after macro expansion and name resolution; kept alive for the whole
  compilation; used for type checking, trait resolution, lints.
- **THIR** (Typed HIR) — built per function body *after* type checking completes; fully
  typed; short-lived; used for pattern exhaustiveness, unsafety checking, and as the
  source of MIR lowering. THIR is where the place / value distinction first becomes
  explicit and structural.
- **MIR** — basic blocks, explicit drops, SSA-like; used for borrow checking,
  optimisation, const evaluation, codegen.

WX TIR corresponds to HIR + THIR combined. WX MIR corresponds to Rust MIR. The place /
value problem that THIR solves for Rust needs to be solved in WX TIR.

---

## Design

### `Place` — a location in linear memory

```rust
pub struct Place {
    pub kind:    PlaceKind,
    pub ty:      TypeIndex,    // type of the value at this location
    pub memory:  TypeIndex,    // which linear memory (set once at build time)
    pub mutable: bool,         // from the root Deref's pointer type (set once at build time)
    pub span:    ast::TextSpan,
}

pub enum PlaceKind {
    /// `ptr.*` — dereferences a pointer; the only way to enter linear memory.
    /// Every place chain starts here.
    Deref { pointer: Box<Expression> },

    /// `place.field` — a named field within a memory place.
    Field { object: Box<Place>, member: ast::Spanned<SymbolU32> },

    /// `place[index]` — an element within a memory place.
    Index { object: Box<Place>, index: Box<Expression> },
}
```

`memory` and `mutable` are computed **once** when a `Place` node is constructed and
stored inline. No recursive walk is ever needed at a use site.

| `PlaceKind`          | `memory`                              | `mutable`                             |
|----------------------|---------------------------------------|---------------------------------------|
| `Deref { pointer }`  | from `pointer.ty` (`Type::Pointer`)   | from `pointer.ty` (`Type::Pointer`)   |
| `Field { object }`   | `object.memory`                       | `object.mutable`                      |
| `Index { object }`   | `object.memory`                       | `object.mutable`                      |

### Changes to `ExprKind`

**Removed variants** (absorbed into `Load` / `Store`):

- `Deref` — always a place read; becomes `Load { place: Deref { .. } }`.
- `Index` — always a place read; becomes `Load { place: Index { .. } }`.
- The deref-rooted path of `ObjectAccess` — becomes `Load { place: Field { .. } }`.

**Added variants**:

```rust
/// Read the value stored at a memory place.
/// Replaces: ExprKind::Deref, ExprKind::Index,
///           and ObjectAccess-via-pointer in value position.
Load { place: Box<Place> },

/// Take the address of a memory place.
/// The Rust type of `place` enforces that .& can only be called on a memory
/// place — impossible to construct on a stack value.
AddressOf { place: Box<Place>, mutable: bool },

/// Write a value to a memory place.
/// Replaces the Deref / Field / Index paths in lower_assignment and
/// lower_compound_assignment.
Store { target: Box<Place>, value: Box<Expression> },
```

**`ObjectAccess` stays**, but now has a narrowed contract: it is only ever a local
struct field read (always lowers to `AggregateGet`). The ambiguity is gone.

### Expression map after the refactor

```
local_struct.field     ->  ObjectAccess { Local, member }              ->  AggregateGet
ptr.*.field            ->  Load { Field { Deref { ptr } } }            ->  ptr + offset, PointerLoad  (one op, not two)
ptr.*[i]               ->  Load { Index { Deref { ptr } } }            ->  ptr + i*size, PointerLoad
ptr.*.field.&          ->  AddressOf { Field { Deref { ptr } } }       ->  ptr + offset  (pure arithmetic)
ptr.*.field.&mut       ->  error if ptr: *Foo; ok if ptr: *mut Foo     ->  enforced by place.mutable
ptr.*[i].field = val   ->  Store { Field { Index { Deref { ptr } } } } ->  ptr + i*size + offset, PointerStore
```

---

## Memory efficiency

`Place` is always `Box<Place>` — heap-allocated only when an actual place expression
exists in the source. Every pure value expression (`Int`, `Local`, `Binary`, `Call`,
etc.) carries zero overhead. `memory` and `mutable` stored inline in `Place` replace all
recursive walks at every use site.

---

## Implementation plan

The detailed step-by-step implementation plan lives in
[tir-place-value-split-plan.md](./tir-place-value-split-plan.md).
The summary below is kept here for quick reference.

### 1. `tir/mod.rs`
- Add `Place` struct and `PlaceKind` enum.
- Remove `ExprKind::Deref`, `ExprKind::Index`.
- Add `ExprKind::Load`, `ExprKind::AddressOf`, `ExprKind::Store`.
- Update `#[cfg(test)] derive(serde::Serialize)` on new types (snapshot tests will need
  regeneration with `cargo insta accept`).

### 2. `tir/builder.rs`
- `build_deref_expression` → builds `PlaceKind::Deref`, wraps in `ExprKind::Load`.
- `build_object_access_expression` → if object's kind is `Load { place }`, build
  `PlaceKind::Field` on top and wrap in a new `Load`; otherwise keep `ObjectAccess`.
- `build_index_expression` → builds `PlaceKind::Index`, wraps in `ExprKind::Load`.
- Assignment lowering → when the left-hand side is `Load { place }`, emit
  `Store { target: place, value }`.
- `build_address_of_expression` (new) → extracts the `Place` from a `Load` node; checks
  `place.mutable` for `.&mut`; returns `AddressOf { place, mutable }`.

### 3. `mir/mod.rs`
- `lower_expression`:
  - `Load { place }` arm → single dispatch on `place.kind`; always emits an optimal
    `PointerLoad` at the correct address. Replaces three scattered arms.
  - `AddressOf { place }` arm → emits address arithmetic (pointer + field/index offset).
  - Remove arms for old `Deref`, `Index`, and the deref-rooted path of `ObjectAccess`.
- `lower_assignment` / `lower_compound_assignment`:
  - `Store { target, value }` arm → dispatch on `target.kind`; replaces the scattered
    `Deref` / `Index` / `ObjectAccess` arms. Both functions become significantly shorter.

### 4. `fmt/` (pretty printer)
- Add `Place` formatting so the LSP formatter and debug output remain correct.

### 5. Snapshots
- All snapshot tests will break because the serialized TIR shape changes.
- Regenerate with `INSTA_UPDATE=always cargo test -p wx-compiler`, then review with
  `cargo insta review`.

---

## What this does NOT change

- The AST (`ast::
Expression::Deref`, `ast::Expression::AddressOf`) — unchanged.
- The MIR `ExprKind` (`PointerLoad`, `PointerStore`) — unchanged interface, just reached
  via cleaner paths.
- Codegen — unaffected; it only sees MIR.
- The `wx-lsp` — it only builds TIR, and the TIR API surface visible to it
  (diagnostics, `DefId`s, type display) is unchanged.
