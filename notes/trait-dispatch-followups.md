# Trait Dispatch & `ImplEntry` Follow-ups

Context: we fixed the core soundness bug where `impl_members` silently let trait-provided
members overwrite each other with no ambiguity diagnostic (first-writer-wins, no error). That
part is **done and merged into the working tree** — see the "Done" section below for what
landed. This doc is the leftover punch list: things we found along the way that are real but
weren't fixed, plus the open design question we didn't resolve. Pick this back up before
considering the trait-dispatch work fully closed out.

## Open design decision: `ImplEntry::AssociatedType` still has no source location

**Where:** `tir/mod.rs` — `ImplEntry::AssociatedType { ty: TypeIndex }` (~line 1073),
`ImplEntry::def_span()`'s `AssociatedType` arm still has the original `todo!()`.

**The bug (confirmed, reproducible):** two traits each declaring the same-named associated
type on one struct crashes the compiler instead of reporting `AmbiguousTraitMember`:

```wx
trait A { type Output; }
trait B { type Output; }
struct S {}
impl A for S { type Output = i32; }
impl B for S { type Output = bool; }
fn use_it() -> S::Output { unreachable }
```

`resolve_impl_member` (`tir/builder.rs`) collects both candidates fine, but building the
ambiguity diagnostic calls `entry.def_span(&self.tir)` on every candidate, and
`AssociatedType`'s arm is `todo!(...)` — verified this panics via a throwaway test (reverted,
not committed).

**Why `ImplEntry::AssociatedConst`'s fix (see Done section) doesn't directly transfer:**
`AssociatedConst` got fixed by pointing at `tir.constants[index]`, because a real `Constant`
record already exists per impl override. There is no equivalent per-impl record for
associated types — `impl A for S { type Output = i32; }`'s own `DefId` (it has one,
`ast::ImplItem::AssociatedType { id, .. }`) is parsed and then thrown away in
`AstNodeRef::ImplTraitAssociatedType` handling (`tir/builder.rs`, look for
`ast::ImplItem::AssociatedType { name, ty, .. }` — note the `..`).

**Key fact that shapes the fix:** associated types in wx *never* have a trait-level default —
`check_trait_conformance` requires them unconditionally (unlike methods, which check
`body.is_none()`), and `entry_has_body` always returns `false` for `AssociatedType`. So every
`ImplEntry::AssociatedType` that can reach the ambiguity path is always a specific impl's own
override, never the trait's bare `type X;` declaration.

**Options considered, not yet chosen:**

1. **Use `Trait::assoc_types[sym]` (`TraitAssocType.id` + `.name_span`, plus `Trait.file_id`
   for the file).** This data already exists, costs zero new storage. Downside: it points at
   the *trait's own declaration* line, not the specific conflicting `impl A for S { .. }` the
   user actually wrote — one hop less precise than the Method/AssociatedConst candidates,
   which do point at the real impl override.
2. **Give associated-type impl-overrides their own per-impl record**, e.g. by reusing
   `TIR::type_aliases: Vec<TypeAlias>` (already has `id`/`file_id`/`name`/`template`/
   `accesses` — almost exactly the right shape) and changing
   `ImplEntry::AssociatedType(TypeAliasIndex)` to mirror `AssociatedConst(ConstIndex)`. More
   precise (points at the actual impl line), more invasive (new construction-site plumbing at
   both the trait's placeholder declaration and the impl override, needs `item_lookup`
   registration like we had to add for the memory-instantiated const case).
3. A dedicated new table just for this — considered and dropped in favor of option 2 (reusing
   `type_aliases`), since inventing a whole new parallel item vector for something this narrow
   didn't seem worth it next to an existing table with the right shape already sitting there.

**Where we left it:** leaning toward starting with option 1 (cheap, fixes the actual crash,
"good enough" diagnostic precision) and treating option 2 as a later precision upgrade if it
turns out to matter in practice — but explicitly did not commit to this, wanted to sit on it.
Whichever is chosen, the mechanical part: `resolve_impl_member`'s `MemberCandidate` struct
(local to the function) would need `trait_index` (or a precomputed `def_span`) added to it,
since the span can no longer be derived from the `ImplEntry` alone — it currently only stores
`{ trait_name: SymbolU32, entry: ImplEntry }`.

## Correctness: cross-block inherent duplicates that are never called go undetected

**Where:** `tir/builder.rs`, `AstNodeRef::ImplBlockMethod` handling, comment starting
"Within-block duplicate check: two methods of the same name in the SAME block. Collisions
against a DIFFERENT block ... are no longer checked eagerly here."

**What:** two separate inherent impl blocks for the same concrete type
(`impl i32 { fn abs(...) }` written twice, e.g. across two files) only get flagged as
`DuplicateDefinition` if something actually calls the conflicting method somewhere in the
program — detection now happens lazily inside `resolve_impl_member`, which only runs on
demand at real use sites. If the method is dead code, the duplicate silently compiles.

**Why it matters:** Rust reports this unconditionally (`E0592`) at the second `impl` block
regardless of usage. This is a narrower gap (only matters for entirely-unused duplicate
definitions) but a real one — this may be an intentional tradeoff (avoiding a separate global
duplicate-scan pass over `impl_block_dispatch`), not confirmed either way.

**Possible fix:** an unconditional pass after all signatures resolve (Phase 3.5-style, next to
`check_trait_conformance`) that walks `impl_block_dispatch` and flags any `(kind, name)`
bucket where two or more blocks both structurally apply to the same concrete type via
`match_impl_block`, independent of whether anything calls that member.

## Correctness: `entry_has_body`'s dropped `ensure_signature` call — plausible but unproven risk

**Where:** `tir/builder.rs` — `fn entry_has_body(&self, entry: ImplEntry) -> bool`.

**What:** the original version of this helper called `self.ensure_signature(def_id)` before
checking `self.sig_state.get(&def_id)`, specifically because `resolve_impl_member` (and hence
`entry_has_body`) is reachable from **Phase 2** (signature-building), not only Phase 3
(body-building) — proven by the associated-type crash repro above, which hit
`resolve_impl_member` from a plain function's return-type annotation (`-> S::Output`), which
resolves during that function's own `ensure_signature`. The current version treats a missing
`sig_state` entry as "no body" (i.e. not a viable trait-default candidate) rather than
resolving it first.

**Why it might matter:** if a trait's default *method* (not assoc type — methods are the ones
`entry_has_body` actually inspects) is looked up as a fallback candidate before its own
signature has been demand-resolved (Phase 2 processes registered `DefId`s in parse order, so
this is order-dependent), a real bodied default could be misclassified as abstract — purely
because of registration order, not anything in the source.

**Status:** flagged, not reproduced. Didn't construct a concrete failing case (would need a
same-named trait *method* — not assoc type — hit from within a signature, before that trait
function's own signature has been demand-resolved). Worth either constructing that repro or
just re-adding the `ensure_signature` call defensively (it's cheap and re-entrant-safe per
`sig_state`'s `ComputeState` guard) rather than relying on phase-ordering guarantees holding.

## Minor: leftover typo

**Where:** `tir/builder.rs`, `AstNodeRef::ImplBlockInit` handling, the
`TypeMistmatch` diagnostic message.

**What:** `"cannot define inherit `impl` for `{}`"` should read `"inherent"`. Flagged twice,
not yet fixed.

## Performance / memory (raised when asked specifically, not yet acted on)

**Hot-path allocation in `resolve_impl_member`:** every single member resolution (every
`.method()` call, every `.field` access, every `Type::CONST` reference across the whole
program, during Phase 3) allocates a `Vec` for candidates — sometimes two
(`inherent_candidates`, then `candidates`) — even though the overwhelmingly common case is
exactly 0 or 1 matches. Suggested fix: track the first match in a plain `Option`, only spill
into a `Vec` the moment a genuine second candidate shows up, so the common case never touches
the allocator. Not implemented — deferred as a "measure if it matters" item, not urgent for a
thesis-scale compiler.

**`HashMap` for small member tables:** `ImplBlock::members`, `Trait::members`,
`TraitImpl::members` are all plain `std::collections::HashMap<SymbolU32, ImplEntry>` (default
SipHash, no fast-hasher override anywhere in the crate). Most impl/trait blocks have a
handful of members; for that size a linear `Vec<(SymbolU32, ImplEntry)>` scan would likely be
both smaller and faster than a `HashMap`'s fixed per-instance overhead. This predates the
trait-dispatch work and applies broadly, not just to what we touched — a structural change,
not a quick fix.

## Not done, explicitly deferred (not bugs, just scope cuts made along the way)

- **`use Trait;`-style lexical scoping** — narrowing which traits are visible at all, rather
  than just erroring on ambiguity. Explicitly deferred early on as a later, additive
  increment; current design doesn't preclude adding it.
- **Disambiguation syntax** — there is currently no way to actually *resolve* an
  `AmbiguousTraitMember`/inherent-duplicate error in source once diagnosed (no
  `<Type as Trait>::method()` equivalent exists yet). Discussed at length (rejected Rust's
  `Trait::method(recv)` and `<Type as Trait>::` syntax specifically; leading candidates were
  `(expr as Trait).method(args)` for receiver methods and `Type::Trait::item` for
  receiver-less items), but nothing was implemented or finally decided.

## Done (for context — not part of the leftover list)

- `impl_members` restricted to inherent impls only; trait-provided members
  (defaults + explicit overrides) resolved on demand via `resolve_impl_member`, which checks
  inherent first (unconditional win, matches Rust), then scans trait impls, erroring via the
  new `AmbiguousTraitMember` (`E1059`) diagnostic on 2+ candidates.
- Fixed two latent MIR panics this surfaced (abstract-method-dispatch-from-default-body and
  associated-type-projection substitution both used to silently read the now-trait-empty
  `impl_members`) by routing them through `trait_impl_lookup` directly instead, since both
  already know the specific trait in hand.
- `ImplEntry::AssociatedConst` changed from `{ id: ast::DefId, ty: TypeIndex }` to a bare
  `AssociatedConst(ConstIndex)`, matching `Method`/`AssociatedFn`'s shape — removes a
  redundant field and an unnecessary `item_lookup` hashmap round-trip. Memory-instantiated
  associated consts (`Memory::DATA_END: Self::*u8`) now fork a fresh `Constant` per
  instantiation (own synthetic `DefId`, registered in `item_lookup`) rather than diverging
  the entry's `ty` from its `Constant`'s `ty`.
- `ResolvedMember::Const` similarly dropped its redundant `ty` field (no divergence case ever
  existed there, unlike `ImplEntry::AssociatedConst`).
