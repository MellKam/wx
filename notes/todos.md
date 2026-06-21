# TODOs

## Flatten supertrait bounds in `TypeParamInfo`

**Where:** `tir/builder.rs` — `resolve_ast_type_params` (or during `ensure_signature` for each trait)

**What:** `TypeParamInfo.bounds: Box<[TraitIndex]>` currently stores only direct bounds as written by the user (e.g. `T: C` stores `[C]`). For a chain `C: B: A`, the compiler never records that `T` also satisfies `B` and `A`.

**Why it matters:** `type_param_satisfies_bound` does a flat `any(|b| b == expected_trait)` lookup, so it would return `false` for a transitive supertrait. Current tests pass vacuously — when `T: C` is passed to `fn requires_a<U: A>`, type inference unifies `U = T` (same TypeIndex), `substitute_expected_type` converts the result to `INFER`, and the bound check is skipped entirely. Soundness is maintained through monomorphization + `check_trait_conformance`, but early call-site errors for violated supertrait bounds are silently missed.

**How:** After resolving each trait bound during `resolve_ast_type_params`, walk the trait's `supertraits` field transitively and append them all to the `bounds` vec before boxing. This makes `type_param_satisfies_bound` correct for deep hierarchies without changing its O(n) lookup structure.
