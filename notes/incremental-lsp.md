# Incremental LSP: Signals-Based Query System

## The Core Idea

Replace the current whole-crate recompile-on-save with a reactive query DAG where each
computed value is cached and only recomputed when its inputs change. The mental model is
Vue's `computed` chain or React's `useMemo` — but applied to compilation.

Dependency chain:

```
file content (String)       ← changed by LSP did_change events
    ↓
AST (per file)              ← reparse only the changed file
    ↓
Crate graph                 ← rebuild graph, reusing unchanged ASTs
    ↓
Compilation unit            ← all crates assembled
    ↓
TIR                         ← typecheck; skip if compilation unit unchanged
```

When a single file is edited, only that file's AST is recomputed. All other ASTs are
cache hits. The crate graph is rebuilt with the one new AST slotted in. TIR reruns over
the new crate graph. Every other file pays zero parse cost.

## Current State (as of 2025-06)

Two-level cache is already in place in `wx-lsp-next`:

- **Parse cache** (`ServerState.parse_cache`): per-crate-root `CompilationGraph` (ASTs
  only). Rebuilt lazily when any file's `lsp_version` advances. Consumed by TIR build.
- **TIR cache** (`CompiledRoot.compiled_versions`): stores the `lsp_version` of every
  file at last TIR build. On `didSave`, if no versions changed, the TIR rebuild is
  skipped entirely.

This eliminates the double-parse-per-save problem (formatting + TIR both used to parse
independently). Result: 1 parse + 1 TIR per save; repeated identical saves cost 0 + 0.

The next step toward the full signals model is per-file AST caching (Phase 2 below).

## Implementation Phases

### Phase 1 — Crate-level parse cache (done)

`ensure_parse_cache` checks all `lsp_version`s; rebuilds the full `CompilationGraph`
only when something changed. TIR skipped when all versions match `compiled_versions`.

### Phase 2 — Per-file AST cache

Goal: within `ensure_parse_cache`, only reparse files whose `lsp_version` changed since
last parse. Unchanged files inject their cached AST into the builder.

Requires a new `CompilationGraphBuilder` API:

```rust
// Accept a pre-parsed module instead of loading+parsing from source.
fn inject_module(&mut self, path: String, ast: Module, source: String) -> FileId
```

Key constraint: `FileId` must be **stable across rebuilds** (currently allocated
sequentially, so a full rebuild assigns new IDs). LSP span lookups (hover, goto-def,
semantic tokens) all depend on `FileId` being consistent with the last TIR. Options:
- Path-keyed `FileId` allocation: same path always gets the same ID.
- Separate "canonical file registry" that persists across graph rebuilds.

The string interner (`CompilationGraph.interner`) is additive (keys are stable, only
grows). Cached ASTs from an old parse still hold valid `StringKey`s in the new interner.
No interner GC is needed for correctness, only for memory growth on long sessions.

### Phase 3 — Incremental TIR (salsa-style)

Goal: when only file A changes, skip re-typechecking items defined in files B and C.

`tir/builder.rs` already has the shape of a query system:
- Phase 1 (`pre_scan_item`) registers all items.
- Phase 2 (`ensure_signature`) is demand-driven and re-entrant safe.
- Phase 3 (`ensure_body`) evaluates function bodies.

What's missing is the **invalidation layer**: tracking which `DefId`s depend on which
source spans, and marking only those as dirty when a file changes.

This is what rust-analyzer achieves via [salsa](https://github.com/salsa-rs/salsa):
- Every "query" (e.g. `function_signature(def_id)`) has a revision it was last computed.
- A global revision counter increments on every file change.
- A query is valid iff its revision == the global revision, OR none of its tracked inputs
  changed their `changed_at` revision since it was last computed.

The **early cutoff** optimization is critical here: if `file_ast(a.wx)` recomputes but
produces a structurally identical result, `changed_at` does NOT advance. Downstream
queries (TIR for functions in other files) see no change and skip recomputation. This
makes comment edits and whitespace changes nearly free end-to-end.

wx AST nodes carry byte-offset spans, so any source edit produces a different AST even
for whitespace. Early cutoff at the AST level would require either:
- Span-insensitive structural AST comparison (complex).
- A separate "semantic fingerprint" per item (e.g. hash of the token stream excluding
  trivia). Doable and useful.

## Hard Problems to Keep in Mind

**TIR is holistic today.** `TIR::build(&mut CompilationGraph)` is one monolithic pass.
It cannot be partially invalidated without restructuring the builder. Phase 3 requires
this restructuring; Phases 1 and 2 do not.

**Dynamic dependency discovery.** `CompilationGraphBuilder` discovers the file set by
parsing `module foo;` declarations recursively. A query system needs stable dependency
edges. Handled by: re-discovering the file set on every crate-graph recompute (cheap,
parsing a single file to find `module` declarations), then using that stable set for
per-file AST queries.

**TIR mutates the graph.** `TIR::build` takes `&mut CompilationGraph`. It reads from
AST nodes but may also annotate them. If it writes into AST nodes, cached ASTs from
before a TIR build may be stale after. Needs verification before Phase 2 is finalized.

**`StringInterner` growth.** On a long LSP session with many file edits, the interner
accumulates strings from deleted identifiers. Not a correctness issue, but worth noting
for very long sessions. A generational interner (GC old generations after a full rebuild)
could address this in the future.

## References

- rust-analyzer architecture: https://rust-analyzer.github.io/book/arch/
- salsa incremental computation: https://github.com/salsa-rs/salsa
- "Responsive compilers" (Nicholas Matsakis): describes the design goals behind salsa
