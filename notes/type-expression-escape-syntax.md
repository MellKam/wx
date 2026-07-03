# `<TypeExpr>::...` — an unambiguous type-position escape for expression grammar

> **Status: REJECTED — not implemented, not planned.** Kept for the record; see
> "Why this was rejected" below. Do not resurrect without re-reading that section first.

## Why this was rejected

The idea was scoped to replace turbofish only where it disambiguates "this identifier
names a type" (struct-init: `Pair::<i32>::{ .. }` → `<Pair<i32>>::{ .. }`), while
deliberately leaving generic **function/method** call witnessing
(`foo::<T>(...)`, `obj.method::<T>()`) on turbofish, since that's a different concept
(see "Scope decision" below, kept as originally written).

The problem: real code immediately reaches for the same bracket syntax at a generic
*call* site too, since visually it looks like it should generalize —
`<size_of<T>>() * count` instead of `size_of::<T>()` (real example from
`doom/m.wx:18`). That reads worse, not better, than turbofish. Generic function/method
calls are far more common in practice than explicit generic struct-init (which is
usually inferred from an expected type and needs no explicit args at all — see the
`build_struct_init_expression` note below). A syntax that's clunkier at the *common*
call site than the thing it's replacing, in order to fix the *rare* site, is a net
ergonomic loss even though it achieves grammar uniformity on paper.

Conclusion: turbofish (`::<T>`) is fine where it already lives — it's the standard,
precedented, already-unambiguous answer for "this call needs explicit type args," and
trying to unify it with type-position generic syntax doesn't hold up once you look at
the actual call sites. The narrower, still-open item from this exploration — unifying
the *struct vs. type-alias* duplication across `resolve_type`'s three existing call
sites (turbofish x2 + `GenericApplication`) — remains worth doing on its own, and does
not require any syntax change. See the conversation this doc came from for that plan;
it's independent of everything below.

---

*(Original design, preserved as written, not current guidance:)*

## Motivation

`fn f(x: Wrapper<i32>)` (type position) and `Wrapper::<i32>::{ ... }` (expression
position) mean the same thing but are spelled differently, parsed by different AST
productions (`TypeExpression::GenericApplication` / `Path` with `type_args` vs.
`PathSegment.type_args` inside expression-path parsing), and resolved by different TIR
code. This was inherited from Rust without re-examining *why* Rust needs it: `<` and `>`
are already binary comparison operators in expression grammar, so `foo<T>(...)` is
genuinely ambiguous with `(foo < T) > (...)` there. Type-expression grammar has no
comparison operators, so `Foo<T>` was never ambiguous in type position — there's nothing
to fix on that side.

The question this doc answers: what should expression position do instead of turbofish,
given the goal is one way to spell a generic type, unambiguous, no parser backtracking.

## Current state (as found — this is not obvious from a first read)

Expression-position "type-shaped" syntax is **not one mechanism**, it's four, and they
answer two genuinely different questions:

**"This identifier names a type, give me a type value"** (what this doc is about):
- `parse_path_expression`'s self-contained `::`-walking loop (ast/mod.rs:3182) owns
  `Pair::<i32>::{ ... }` end to end — it detects `::<` inline and sets
  `type_args` on a `PathSegment`, then hands off to `parse_struct_init_expression` when
  it sees `::{`.
- TIR-side, `build_struct_init_expression` (builder.rs:12743) re-derives the struct's
  `TypeIndex` from the path *independently* of `resolve_type` — it does not reuse
  `resolve_generic_type_application`/`resolve_type_alias_application` from the type-alias
  work. **Confirmed bug, found while researching this doc**: a parametric alias whose
  arity doesn't match its target (`struct Pair<T,U>{..} type Boxed<T> = Pair<T,T>;`)
  produces a wrong `TypeArgCountMismatch` on `Boxed::<i32>::{ ... }`, because the turbofish
  args get applied directly to `Pair`'s 2 params instead of `Boxed`'s 1. Repro is in the
  implementation notes below. This is independent of the syntax question but is strong
  supporting evidence: expression-position type resolution has drifted from type-position
  resolution because it's a separate code path.
- `build_namespace_member_expression` (builder.rs:9104), used for `Type::CONST` /
  `Type::method()`, is **already decoupled from path-walking** — it takes a
  `Spanned<TypeIndex>` plus one final segment, not a path. This matters a lot for the
  plan below.

**"This call needs explicit type-parameter witnessing"** (a different concept, out of scope):
- `Expression::TypeApplication` (a `led` on `Token::ColonColon`, ast/mod.rs:3842) plus
  special-casing in `parse_call_expression` handles `obj.method::<T>()` and bare
  `foo::<T>` (a generic function *reference*, no call) via `build_path_expression`'s
  "single-segment with type args" branch (builder.rs:8547).
- Turbofish here isn't disambiguating "is this a type" — `Foo` in `foo::<T>()` is already
  known to be a value (a function) from context. It's disambiguating "explicit generic
  args follow" from "less-than comparison follows," and `::` already solves that
  unambiguously (`::` can never continue a comparison chain). There's no equivalent
  ambiguity-driven reason to touch this.

## Scope decision

**In scope**: replace the "type value in expression position" spelling —
`Pair::<i32>::{ ... }` → `<Pair<i32>>::{ ... }`, and equally `<Pair<i32>>::method()` /
`<Pair<i32>>::CONST` if a bare `Pair::<i32>::method()` form exists today.

**Out of scope**: `foo::<T>(...)` and `obj.method::<T>()` (function/method generic-arg
witnessing). Different concept, already unambiguous, already the standard cross-language
answer to "this call needs explicit type args." Do not conflate the two.

## Why `<TypeExpr>` works

A bare `<` can never start a value expression under standard binary-operator grammar —
there's no left-hand operand yet for it to be a continuation of. Confirmed directly:
`nud_lookup` (ast/mod.rs:2970), which dispatches on the first token of a fresh operand,
has no case for `Token::LeftArrow` at all — it's a parse error today, meaning the token
sequence is completely unclaimed. This is the same fact Rust's `<Vec<i32>>::new()`
qualified-path syntax relies on (this already exists in Rust, without the `as Trait`
part — not a novel construct).

Once past the opening `<`, everything until the matching `>` is ordinary
`TypeExpression` grammar — already unambiguous, already handles nesting
(`<Pair<i32>>` needs the trailing `>>` split into "close `Pair`'s args" then "close the
escape," and `split_double_right_arrow` (ast/mod.rs:645) already exists and already
does exactly this for `Foo<Bar<T>>` — one more level of reuse, not new machinery).

## Design

### AST
- New `Expression::ExplicitType(Box<Spanned<TypeExpression>>)` variant. Precedent:
  `Expression::Cast { value, ty: Box<Spanned<TypeExpression>> }` already embeds a
  `TypeExpression` inside `Expression` for `as` casts — same shape.
- `Expression::StructInit`'s `path: Box<[PathSegment]>` field generalizes to a small
  enum, e.g.:
  ```rust
  enum StructInitTarget {
      Path(Box<[PathSegment]>),           // Pair::{ .. } / mod::Pair::{ .. }
      Explicit(Box<Spanned<TypeExpression>>), // <Pair<i32>>::{ .. }
  }
  ```

### Parser
- `nud_lookup`: add `Token::LeftArrow => Some((Parser::parse_explicit_type_expression, BindingPower::Primary))`.
- `parse_explicit_type_expression`: consume `<`, call the existing
  `parse_type_expression()`, expect/split the closing `>`, wrap in
  `Expression::ExplicitType`.
- **Continuation is the real open question.** `::{...}` and `::member` currently only
  exist inside `parse_path_expression`'s self-contained loop — they are not general
  `led`s the way `.field`/`.method()` already are. Two ways to give
  `Expression::ExplicitType` the same continuations:
  - **(a) Generalize `::` into a proper `led`**, symmetric with how `Token::Dot` already
    works for any left-hand expression. This is more work up front but removes the
    existing special-casing in `parse_path_expression` too — a real simplification, not
    just a workaround for the new node.
  - **(b) Give `parse_explicit_type_expression` its own small continuation loop**,
    mirroring the relevant parts of `parse_path_expression`'s loop. Less invasive, but
    adds a second, parallel implementation of the same `::` continuation logic instead
    of removing the existing one.
  - Recommend (a) if there's appetite for the larger diff; (b) as a pragmatic first cut
    that can be folded into (a) later without churning call sites twice.

### TIR
- New `build_explicit_type_expression`: literally `self.resolve_type(resolve_context,
  Some(scope), &type_expr)` — reuses the type-position resolver as-is, including the
  alias/generic-application handling already in place. No new resolution logic.
- `build_struct_init_expression` gains an `Explicit(ty_expr)` branch that calls the
  above to get `struct_ty` directly, instead of the current path-walking +
  independent `struct_seg.type_args` resolution. This also **fixes the arity bug**
  found above, since resolution now goes through the same `resolve_type_alias_application`
  path as everywhere else — not a separate reimplementation.
- `build_namespace_member_expression` needs **no changes** — already takes a
  `Spanned<TypeIndex>`, confirmed at builder.rs:9104.

### `wx-fmt`
- Pretty-print `Expression::ExplicitType` as `<{type}>`.
- Update `Expression::StructInit` formatting for the `Explicit` target variant.

## Migration plan (additive-first, subtractive-last — mirrors `tir-place-value-split-plan.md`)

1. **AST**: add `Expression::ExplicitType`, generalize `StructInitTarget`. Purely
   additive — old turbofish struct-init spelling keeps working unchanged.
2. **Parser**: `nud` for `<`, `parse_explicit_type_expression`, continuation (design
   decision (a) or (b) above).
3. **TIR**: `build_explicit_type_expression`, `build_struct_init_expression`'s new
   branch. Verify the arity-bug repro now passes.
4. **`wx-fmt`**: format the new node.
5. **Tests**: new parser + TIR tests for `<Type>::{...}` and `<Type>::method()`,
   including the generic-alias-arity regression case. Snapshot updates as needed.
6. **Subtractive step (separate follow-up, not bundled here)**: once the new syntax is
   proven, decide whether to keep old turbofish-before-`::{`/`::member` as a deprecated
   alias or remove it outright. Removing it is what actually achieves "one spelling" —
   leaving both means the grammar inconsistency this doc set out to fix still exists,
   just with an extra option.

## Open questions for a follow-up decision

- Should `<Foo>::method()` be legal even when `Foo` isn't generic (pure symmetry, no
  parser cost either way), or should style/formatting prefer bare `Foo::method()`
  whenever there's nothing to disambiguate? Recommend: allow both, no special-casing
  needed in the implementation; this is a style-guide question, not a grammar one.
- Confirm final call on step 6 (deprecate vs. remove old turbofish-struct-init spelling)
  before implementing — determines whether this is a net grammar simplification or an
  added third option.
