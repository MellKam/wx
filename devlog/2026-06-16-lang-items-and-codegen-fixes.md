# 2026-06-16 — Lang items, codegen test fixes, MIR slice lowering

## Summary

Fixed a broken codegen test suite (root cause: duplicate stdlib loading), added MIR lowering for `Slice` types, and implemented the `#[lang = "key"]` lang items map in TIR. Ended with architectural discussion on `DefId`, incremental compilation, and the path forward.

## Changes

- `crates/wx-compiler/src/codegen/tests.rs` — removed duplicate STD wrappers from 12 tests; changed `string` → `[]u8` in two import tests
- `crates/wx-compiler/src/mir/mod.rs` — added `Slice` case to `lower_type_index`
- `crates/wx-compiler/src/tir/mod.rs` — renamed `FunctionAttribute` → `ItemAttribute`, added `Lang(SymbolU32)` variant; replaced `Option<LangItems>` with `HashMap<SymbolU32, ast::DefId>`
- `crates/wx-compiler/src/tir/builder.rs` — replaced `resolve_function_attributes` with `resolve_attributes`; added `register_lang_items`; wired registration into Phase 2
- `crates/wx-compiler/src/tir/tests.rs` — added `test_lang_items_registered`
- `crates/wx-compiler/src/opt/snapshots/` — regenerated `snapshot_sched_call_two_args` (Call index shifted due to more stdlib functions)

## Key findings

**Root cause of codegen test failures**: 12 tests used `TestCase::new(&format!("{STD}\n{}", source))`. `TestCase::new` already loads `STDLIB_SOURCE` as a stdlib crate, so these tests loaded the `Memory` trait twice — once as crate index 0 (trait_index 1) and once embedded in the user source (trait_index 5). The `@memory_grow` intrinsic resolved to the user-code copy (trait 5) while `Memory::grow`'s `Self::Size` referenced trait 1, producing a type mismatch "expected `Memory::Size`, found `Memory::Size`" — identical display names, different TypeIndex because `trait_index` differed.

**`std::process::exit(1)` in codegen TestCase**: on TIR errors the codegen test helper calls `exit(1)` rather than panicking, which kills the entire test process. This explains why the test runner reported "test exited abnormally" with a non-1 count of passing tests.

**`Slice` not lowered in MIR**: string literals have type `[]u8` (a `Slice`), but `lower_type_index` had no `Slice` arm and fell through to `unreachable!()`. Added case that creates an aggregate `{Pointer{memory}, U32/U64}` matching the layout already handled by `mir_type_layout`.

**Attribute parsing already stores raw string with quotes**: `#[lang = "memory"]` → `AttributeValue::NameValue(sym)` where the symbol resolves to `"memory"` (with double quotes). `unescape_string` already handles stripping, so reuse it rather than `trim_matches('"')`.

**Symbol index shift after `get_or_intern` in `resolve_attributes`**: calling `get_or_intern("memory")` for lang keys interns new symbols, shifting the `SymbolU32` indices of later-interned strings (export names etc.). 8 snapshot tests failed on the export map keys. Accepted with `cargo insta accept`.

## Decisions

**Register lang items in Phase 2, not Phase 1**

The obvious place is `pre_scan_item` (Phase 1), but `resolve_attributes` (which interns the unquoted key) is already called in Phase 2 (`ensure_signature`) for every function. Calling it again in Phase 1 would double-parse every attribute. Phase 2 is sufficient because nothing consumes `lang_items` until after Phase 4.

**`register_lang_items` takes `&[ItemAttribute]`, not `&[ast::Attribute]`**

The helper receives already-resolved attributes so it never calls `resolve_attributes` itself. Avoids the double-parse the user correctly flagged.

**`ItemAttribute` instead of `FunctionAttribute`**

Unified attribute enum covering all item kinds. Currently `Inline` and `Lang(SymbolU32)`. Inline still only applies to functions; having it in a shared enum is fine — unused variants are simply never produced for non-function items.

**`HashMap<SymbolU32, DefId>` for lang items, not `HashMap<Box<str>, DefId>`**

Keys are `SymbolU32` (interned at registration time via `get_or_intern`). Lookup at call sites uses `interner.get("memory")` which returns `Option<SymbolU32>` without inserting — correct semantics (if the string was never interned, the lang item certainly doesn't exist).

## Context for future sessions

**`DefId` is a sequential counter, not a stable identity.** Every named AST item gets a `DefId` from a global `DefIdGenerator`. This creates N parallel `HashMap<DefId, ItemIndex>` tables in TIR (function, struct, memory, const) and more in codegen (WASM indices). This is a known design debt.

**The N-parallel-HashMaps problem and its correct fix**: rustc and rust-analyzer avoid this by making the typed index *be* the primary ID (`IndexVec<LocalDefId, T>` / `Arena<T>` with `Idx<T>`). The HashMap only appears at the AST→TIR seam. For WX the cleanest fix is: assign typed indices at the TIR boundary, use them everywhere downstream, keep `DefId` only within the TIR builder for demand-driven resolution.

**Incremental compilation design**: both CLI and LSP can be served by a single `Database` struct. CLI creates a fresh one, runs queries cold (100% cache misses = negligible overhead), discards it. LSP keeps it alive and bumps a revision counter on each edit; only stale queries recompute. The current `ensure_signature` with `ComputeState` is already the right shape — the gap is no revision tracking and no input invalidation.

**The prerequisite for any incremental work: stable DefIds.** The sequential counter must be replaced with IDs derived from the syntactic path (e.g. hash of `file_path::module::item_name`). Without this, editing any file shifts all subsequent DefIds, making the cache useless. This is the most important architectural change before attempting incremental.

## Open questions

- Abstract pointer types: `Pointer/Array/Slice { memory: ast::DefId }` can't represent pointers in a generic/abstract memory (e.g. `Self::*mut u8` where `Self: Memory`). The commented-out `// const DATA_END: Self::*mut u8;` in `std.wx` is blocked on this. Fix requires carrying `TypeIndex` (which can be an `AssocTypeProjection`) instead of a concrete `DefId`. Not yet started.
- Should `DefId` → path-based hash happen before or after the abstract pointer fix? They're independent but the DefId change touches everything.
