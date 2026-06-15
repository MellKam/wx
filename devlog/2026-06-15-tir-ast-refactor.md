# 2026-06-15 — TIR builder AST registry refactor

## Summary

Refactored the demand-driven resolution infrastructure in `tir/builder.rs` to improve memory efficiency, eliminate a sorting step, and make the code more readable.

## Changes

- `crates/wx-compiler/src/tir/builder.rs` — all changes below
- `devlog/` — created this devlog system

## Decisions

**Vec + HashMap instead of two HashMaps**

Replaced `ast_nodes: HashMap<DefId, AstNodeRef>` with `ast_nodes: Vec<AstEntry>` and `sig_state: HashMap<DefId, SigEntry>` where `SigEntry` stores `{ node_idx: usize, state: ComputeState }`.

Why: `DefId` is a `u32` assigned sequentially during pre-scan, so insertion order into `ast_nodes` is already sorted order. This eliminates the `O(n log n)` collect+sort that was previously needed before Phase 2. Vec also has much lower per-element overhead than HashMap.

**Populate `sig_state` after Phase 1, not during**

`sig_state` is built in one shot after Phase 1 completes, with `HashMap::with_capacity(ast_nodes.len())`. Phase 2 and 3 only mutate existing entries — they never insert. This avoids incremental rehashing and makes the two-phase boundary explicit in code.

**Named structs instead of tuples**

Introduced `AstEntry<'ast>` and `SigEntry` to replace opaque `.0`/`.1` tuple indexing.

```rust
struct AstEntry<'ast> {
    def_id: ast::DefId,
    file_id: FileId,
    namespace: Option<NamespaceIndex>,
    node: AstNodeRef<'ast>,
}

struct SigEntry {
    node_idx: usize,
    state: ComputeState,
}
```

**`file_id` and `namespace` moved to `AstEntry`**

Previously every `AstNodeRef` variant carried `file_id: FileId` and `namespace: Option<NamespaceIndex>`, requiring a 19-arm dispatch method to extract them. Moving these fields to `AstEntry` removes 38 redundant fields, deletes the `file_id()` and `namespace()` methods, and lets callers destructure cleanly:

```rust
let AstEntry { file_id, namespace, node, .. } = self.ast_nodes[node_idx].clone();
```

**`ComputeState` gained a `Pending` variant**

Previously the initial state was represented by absence from the map. With the pre-allocated approach, all entries exist from the start, so `Pending` is now an explicit state alongside `InProgress` and `Done`.

## Key findings

- The two places that look up `AstNodeRef` by `DefId` without going through `ensure_signature` (around lines 3626 and 3880) now go through `sig_state` first to get `node_idx`, then index into `ast_nodes`.
- Cycle detection checks are pattern-matched against the new struct: `matches!(self.sig_state.get(&def_id), Some(SigEntry { state: ComputeState::InProgress, .. }))`.
