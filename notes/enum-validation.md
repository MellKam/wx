# Harden enum validation: constant folding, duplicate values, range checks, repr restriction

## Where

- `tir/builder.rs` — enum-processing pass (`builder.rs:4036-4190`)
- `tir/builder.rs` — `build_const_expression` (`builder.rs:7216-7420`)
- `tir/mod.rs` — diagnostic codes, `TypeIndex::is_integer()` (`tir/mod.rs:237-246`)
- `tir/tests.rs` — existing enum tests (`tir/tests.rs:4856-5023`, `6650-6668`)

## What

Enums in wx are "just associated constant numbers" — a repr type plus named integer constants, no runtime tag. The AST/TIR pipeline for them exists and covers the happy path (parsing, auto-increment, duplicate *names*, `Enum::Variant` resolution, comparisons), but validation was never finished. The variant-value pipeline never folds an explicit value expression (`Unary`/`Binary`/const references) down to a concrete integer, which is the root cause of most of the gaps below.

### 1. Constant-fold variant values

`build_const_expression`'s handling of `Unary`, `Binary`, and const/associated-const references for enum variants recurses structurally but never computes an actual `i64` result. Fold these down to a concrete value at enum-build time (integer literals, negation, basic arithmetic on other constants). This is the foundation the rest of the items below build on.

### 2. Duplicate variant *values*

Once folding exists, track seen values per enum (alongside the existing seen-names map at `builder.rs:4069-4104`) and report an error when two variants land on the same value — including an auto-incremented value silently colliding with an explicit one (e.g. `enum E: i32 { A, B = 0 }`).

Currently: only duplicate variant *names* are checked (`test_enum_duplicate_variant_is_error`, `tir/tests.rs:5007-5022`). There is no check anywhere for two variants sharing a numeric value.

### 3. Range-check explicit int literals against the repr type

`build_const_expression`'s `Int` arm (`builder.rs:7223-7227`) assigns `ty: expected_type` with no bounds check at all. Route it through the same `coerce_untyped_int_expr` / `report_integer_literal_out_of_range` machinery (`builder.rs:12429-12599`) already used for ordinary typed literals, so `enum E: i8 { A = 300 }` errors the same way `let x: i8 = 300;` already does.

### 4. Auto-increment overflow

The `next_auto_value: i64` counter (`builder.rs:4073`, incremented via `wrapping_add(1)` at `:4110`, `:4127`, `:4137`, `:4151`) needs a bounds check against the repr type's actual range instead of silently wrapping. Currently `enum E: u8 { A = 255, B }` gives `B` the value `256` cast into 8 bits with no diagnostic.

### 5. Negative values on unsigned repr

`enum E: u32 { A = -1 }` should be rejected. Currently `Unary{Neg, Int}` just tags the result with the repr type — no sign check against an unsigned repr.

### 6. Restrict repr type to integers

Right after repr resolution (`builder.rs:4048-4061`), check that the resolved type satisfies `TypeIndex::is_integer()` (`tir/mod.rs:237-246`) — the same helper already used to validate typeset members (`builder.rs:4798`). Reject `enum E: bool { ... }`, `enum E: f32 { ... }`, struct/pointer reprs, etc. with a dedicated diagnostic.

Considered and rejected: allowing `char` as a repr type too. No real precedent (Rust's `repr(...)` doesn't support `char` either) and no concrete need identified beyond niche ASCII-tile/protocol-tag use cases — not worth the extra surface area for a first pass.

### 7. Type-mismatched explicit values

`build_const_expression`'s bool-literal and associated-const arms (`builder.rs:7255-7264`, `:7293-7343`) ignore `expected_type` when producing the result type, and the enum-builder caller never checks `expr.ty == ty` afterward. Add that check so `enum E: i32 { A = true }` errors instead of silently type-checking.

## Why it matters

Items 2–5 currently compile silently to wrong or colliding values with zero diagnostics — this is the actual bug class motivating this work (duplicate-value and range validation was never finished). Root-causing via constant folding (1) also fixes an auto-increment correctness bug: today `enum E: i32 { A = 1 + 1, B }` gives `B` a stale, wrong value instead of `3`, because `next_auto_value` isn't updated when the explicit value isn't already a plain `Int` literal.

## How

1. Do (6) first — an independent check with no dependencies on the others.
2. Do (1) next as the foundation.
3. Then (2), (3), (4), (5), (7) — all consume the folded value from (1).
4. Add regression tests per case in `tir/tests.rs`.
5. Tighten the two existing weak tests — `test_enum_missing_repr_is_error` and `test_enum_duplicate_variant_is_error` currently only assert "some error occurred"; make them assert the specific `DiagnosticCode`.
6. Add at least one MIR/codegen snapshot test exercising enum lowering to Wasm — none currently exist, unlike structs/tuples/slices which have several.

## Currently implemented (for reference, not part of this task)

- Optional repr syntax at AST level; TIR-level "missing repr → error" (`E1036` / `MissingEnumRepr`)
- Duplicate enum item name detection (`DuplicateDefinition`)
- Duplicate variant name-within-enum detection
- Auto-increment for variants with no explicit value, continuing correctly after a preceding literal-int explicit value
- Float-literal-for-integer-repr rejection
- `Enum::Variant` member resolution, scoped per-enum (no cross-enum variant-name collisions)
- `==`/comparison support between two values of the same enum type
- Compile-time erasure of enum type/values through MIR/codegen to the repr scalar type (no runtime representation) — this part needs no changes
