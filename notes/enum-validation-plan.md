# Enum Validation — Implementation Plan

See [enum-validation.md](./enum-validation.md) for the original task spec (the 7 validation
gaps). This document covers the architecture we converged on to implement it, which is
broader than the original spec: `build_const_expression` is retired in favor of the general
`build_expression`, backed by a small, extensible constant evaluator.

## Strategy: additive-first, subtractive-last

Never remove old code until all producers/consumers have migrated. The codebase must
compile after every step.

```
Step 1  Rename FunctionContext -> BodyContext        tir/builder.rs   (mechanical rename)
Step 2  Add ConstValue + eval_const_expr              tir/mod.rs, tir/builder.rs (additive)
Step 3  Add const_value field to Constant/EnumVariant tir/mod.rs       (additive)
Step 4  Migrate call sites to build_expression        tir/builder.rs   (enum, const, impl-const)
Step 5  Delete build_const_expression                 tir/builder.rs   (subtractive)
Step 6  Update MIR Const-lowering to use const_value   mir/mod.rs
Step 7  Implement the 7 validation checks              tir/builder.rs, tir/mod.rs
Step 8  Regenerate snapshots + full test run            cargo insta / cargo test
```

---

## Why this shape (context for future readers)

The original spec assumed `build_const_expression` would gain ad-hoc constant folding
inside its own `Unary`/`Binary`/`Const` arms. Discarded because:

- MIR's `ExprKind::Const` lowering (`mir/mod.rs:1939-1953`) already only understands
  `Int`/`Float` values and hits `todo!("complex const expression in MIR lowering")` for
  anything else (e.g. `const FOO: i32 = 1 + 1;` panics *today*). Folding needs to produce
  a real cached value MIR can consume directly, not just a prettier `Expression` tree.
- Folding in place (replacing `Unary{Neg, Int}` with `Int{-1}` directly in the stored
  `Expression`) would destroy the source-shaped tree that LSP/semantic-analysis features
  want to keep (hover showing `1 + 1`, not `2`).
- A wholly separate `eval` walking raw AST would have to re-implement name resolution,
  associated-const lookup, and cast-type resolution that `build_const_expression`/
  `build_expression` already do — pure duplication.

So instead: reuse the general expression builder (already handles every shape, already
resolves names/types), evaluate the result into a small cached side-value, and keep the
built `Expression` tree completely untouched.

---

## Step 1 — Rename `FunctionContext` to `BodyContext`

Purely mechanical. `FunctionContext` (builder.rs:6-12) is no longer just for function
bodies — it's now the context for building *any* expression-bearing body: function
bodies, global initializers, const initializers, enum-variant initializers. Rename the
struct, its `impl` block, and all call sites. No behavior change.

---

## Step 2 — Add `ConstValue` and `eval_const_expr` (additive)

In `tir/mod.rs`, near `ExprKind`:

```rust
pub enum ConstValue {
	Int(i64),
	Float(f64),
	Bool(bool),
	Char(char),
	// room to grow: Struct(...), Array(...), etc. — not needed yet.
}
```

In `tir/builder.rs`, a new method:

```rust
fn eval_const_expr(&self, expr: &Expression) -> Result<ConstValue, ()>
```

`Result`, not `Option` — matches this codebase's convention (`build_expression`,
`build_const_expression`, `resolve_type_identifier`, ...) where `Err(())` means "cannot
produce a value here," propagated with `?` through recursive calls, with the *caller*
deciding whether/what diagnostic to report. Internal recursive calls (folding a `Unary`
operand, a `Binary` side, a referenced `Const`) just propagate `Err(())` via `?` without
pushing their own diagnostic; only the top-level call site (Step 4) reports "not a
constant expression" on `Err(())`.

Read-only, operates on an already-built (post `build_expression`) TIR `Expression` — no
`resolve_context`, no interner, no symbol tables needed, since all of that already
happened. Supported subset for this pass:

- `Int{v}` / `Float{v}` / `Bool{v}` / `Char{v}` -> direct passthrough as the matching
  `ConstValue` variant.
- `Unary{InvertSign | BitNot, operand}` where `operand` folds to `ConstValue::Int` ->
  fold to `Int`.
- `Binary{Add|Sub|Mul|Div|Rem, left, right}` where both sides fold to `ConstValue::Int`
  -> fold to `Int`. Comparison/logical operators are not folded (their `Expression::ty`
  is already `BOOL`, which the item-7 type check rejects for enum context — the folder
  doesn't need to understand them).
- `Const{id}` / `NamespaceAccess{member}` -> look up the referenced constant's **own
  cached** `const_value` (see Step 3) and return it directly. Must not re-walk the
  referenced constant's expression tree — this keeps chains like `B = A + 1; C = B + 1;`
  linear instead of blowing up.
- Anything else (calls, blocks, loops, aggregates, ...) -> `Err(())`. This doubles as the
  "is this a valid constant expression" gate: since `build_expression` imposes no shape
  restriction of its own, "does not fold" *is* "not a constant expression" for every
  shape this task cares about.

Explicitly not folded in this pass (documented non-goals, not oversights):
bitwise/shift operators, arithmetic on `Float`/`Bool`/`Char`, aggregates, and any
expression needing a bound-value environment (locals, blocks, `if`/`else`). All of these
are additive extensions later (new `ConstValue` variants + new match arms) that don't
require touching the calling convention.

---

## Step 3 — Add `const_value` to `Constant` and `EnumVariant` (additive)

- `Constant` (tir/mod.rs:318-330): add `pub const_value: Option<ConstValue>`, computed
  once right after the initializer expression is built (`.ok()` of the `eval_const_expr`
  result — the field itself is a cache of "do we have a value," not a fallible
  operation, so `Option` is correct here even though the function that produces it
  returns `Result`).
- `EnumVariant` (tir/mod.rs:852-856): add `pub const_value: Option<ConstValue>`, same
  timing, computed for both explicit and auto-incremented variant values.
- Both need a `stack`/`FunctionBody`-shaped field alongside their `value` expression
  (mirroring `Global.value: Option<FunctionBody>`, `tir/mod.rs:1196-1199`) since
  `build_expression` may legitimately produce scope-bearing nodes even though we reject
  them as non-constant afterward — the `ScopeIndex`es inside the kept `Expression` tree
  must stay dereferenceable for any consumer (LSP, future MIR work) that walks it.
  Exact field naming follows whatever shape `Global` already uses; auto-generated
  (implicit) enum variant values use a trivial/empty stack since they're constructed
  directly as `Int` literals, not through `build_expression`.

---

## Step 4 — Migrate call sites to `build_expression`

Replace the 4 external call sites of `build_const_expression`:
- enum variant explicit value (builder.rs:4117)
- impl-associated const (builder.rs:4265)
- two trait/other const paths (builder.rs:5280, 5660)

Each constructs a trivial `BodyContext` (single root `BlockScope`, empty `lookup`, fresh
`StackFrame`, `GenericScope { owner: TypeParamOwner::Function(item_id), self_type: None }`
— matching the existing `Global` precedent at builder.rs:5963-5983) and calls
`build_expression` instead. Then calls `eval_const_expr` on the result:

```rust
match self.eval_const_expr(&value_expr) {
	Ok(const_value) => { /* store Some(const_value); run range/duplicate checks */ }
	Err(()) => { /* report "not a constant expression"; const_value stays None */ }
}
```

---

## Step 5 — Delete `build_const_expression`

Once nothing calls it (builder.rs:7216-7429), delete it. Its internal recursive calls
(unary/binary operands, grouping, cast) go with it.

---

## Step 6 — Update MIR `Const` lowering

`mir/mod.rs:1939-1953` currently does:

```rust
match &value_expr.kind {
	tir::ExprKind::Int { value } => ...,
	tir::ExprKind::Float { value } => ...,
	_ => todo!("complex const expression in MIR lowering"),
}
```

Widen to match on the cached `const_value: Option<ConstValue>` instead:
`Int(v)`/`Float(v)`/`Bool(v)`/`Char(v)` each map to their existing MIR lowering; `None`
keeps the `todo!()` for genuinely unsupported cases (should be rare/nonexistent post
Step 4, since anything that reached `ensure_body`/`ensure_signature` successfully as a
const initializer must have folded).

---

## Step 7 — The 7 validation checks (from the original spec)

Now layered on top of the new architecture:

1. **Constant folding** — done by Steps 2-6.
2. **Duplicate variant values** — track a `HashMap<i64, span>` alongside the existing
   `seen_variants` name-map in the enum-processing loop (builder.rs:4069-4104), keyed off
   `variant.const_value`. New diagnostic code (next free slot after `E1053`).
3. **Range-check explicit int literals** — bounds-check the folded `Int` value against
   the repr type's range (reuse `IntegerRange::for_integer_type`, already used for
   typesets at builder.rs:4831-4838). Reuse `IntegerLiteralOutOfRange` (E1004).
4. **Auto-increment overflow** — same bounds check applied when assigning
   `next_auto_value` to an implicit variant, instead of blindly `wrapping_add`.
5. **Negative values on unsigned repr** — falls out of item 3's range check for free once
   folding computes the real negated value (`-1` folds to `ConstValue::Int(-1)`, which is
   out of range for `u32` the same way `300` is out of range for `u8`).
6. **Restrict repr type to integers** — independent check right after repr resolution
   (builder.rs:4048), same pattern as the typeset "must be integer" check
   (builder.rs:4798). New diagnostic code.
7. **Type-mismatched explicit values** — a dedicated `expr.ty == ty` check after the
   value expression is built, reusing `TypeMistmatch` (E1001). Runs *before* the
   `eval_const_expr` gate so `A = true` gets a proper type-mismatch message instead of a
   generic "not constant" one.

Regression tests per case in `tir/tests.rs`; tighten `test_enum_missing_repr_is_error`
and `test_enum_duplicate_variant_is_error` to assert specific `DiagnosticCode`s (per
`has_error_code` helper already used elsewhere in that file); add an
MIR/codegen snapshot test for enum lowering to Wasm.

---

## Step 8 — Regenerate snapshots + full test run

`std.wx` and any TIR/MIR shape changes shift snapshot output — regenerate with
`INSTA_UPDATE=always cargo test -p wx-compiler`, review the diff, then `cargo insta accept`.
Full `cargo test -p wx-compiler` must pass before calling this done.
