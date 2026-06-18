# 2026-06-18 — Narrow load/store fix, slice intrinsics, purity inference design

## Summary

Fixed a silent codegen bug where all pointer loads/stores emitted full-width `i32.load`/`i32.store` regardless of pointee type. Added `@slice_from_parts` intrinsic. Extended discussion around rich global initializers and a purity inference pass — full implementation plan designed but not yet coded.

---

## Changes

- `crates/wx-compiler/src/opt/mod.rs` — added `MemAccess` enum; changed `PointerLoadResult`, `PointerLoad`, `PointerStore` nodes to carry `MemAccess` instead of `ScalarType`
- `crates/wx-compiler/src/opt/builder.rs` — use `MemAccess::from_mir(expr.ty)` at all pointer load/store sites; aggregate load/store rewritten to index-based loop (avoids borrow-checker conflict and removes `Vec::collect` allocation)
- `crates/wx-compiler/src/opt/scheduler.rs` — added 6 new `Instruction` variants (`I32Load8S/U`, `I32Load16S/U`, `I32Store8`, `I32Store16`); deleted `natural_align()`; rewrote `PointerLoad`/`PointerStore` emission to dispatch on `MemAccess`
- `crates/wx-compiler/src/codegen/mod.rs` — added 6 match arms for new narrow instructions; fixed `TryFrom<mir::Type> for ValueType` to handle `I8/U8/I16/U16` (was `unreachable!()`, would panic when a narrow type appeared in a function signature)
- `crates/wx-compiler/src/opt/tests.rs` — added `test_sched_narrow_loads_emit_correct_opcodes` and `test_sched_narrow_stores_emit_correct_opcodes`
- `crates/wx-compiler/src/codegen/tests.rs` — added `test_narrow_pointer_deref_sign_extension_and_byte_isolation` (runtime wasmtime test)
- `std.wx` — added `@slice_from_parts` declaration
- `crates/wx-compiler/src/mir/mod.rs` — added `"@slice_from_parts"` arm to intrinsic lowering match
- `crates/wx-compiler/src/mir/tests.rs` — added `test_slice_from_parts_lowers_to_aggregate`
- Multiple snapshot files regenerated (byte offsets shifted by new `std.wx` line)

---

## Key findings

**Root cause of narrow load/store bug**: `ScalarType::TryFrom<mir::Type>` maps `U8`, `I8`, `U16`, `I16` all to `ScalarType::I32`, erasing width/sign. `PointerLoadResult` only stored `ty: ScalarType`, so the scheduler had no way to know the original pointee type. The old `natural_align(ScalarType::I32)` always returned 2 (align=4), so `i32.load align=4` was emitted for every pointer load regardless of whether the pointee was a byte.

**`MemAccess` enum** bridges the gap between `mir::Type` (preserves width/sign) and `ScalarType` (WASM stack type). Lives in `opt/mod.rs`. `MemAccess::from_mir(ty)` converts at the MIR→opt boundary; `align_log2()` and `scalar_type()` are derived from it at schedule time. The `PointerLoadResult` data node is `no_cse`, so adding a field to it doesn't affect CSE behavior.

**Bonus bug caught by codegen test**: `TryFrom<mir::Type> for ValueType` had `_ => unreachable!()` at the bottom, which panicked whenever a narrow type (`u8`, `i8`, `u16`, `i16`) appeared as a function parameter or return type. The test `fn read(ptr: heap::*u8) -> u8` hit this immediately. Fix: add all four narrow types mapping to `ValueType::I32`.

**Aggregate load/store borrow conflict**: the original aggregate pointer load code used an iterator that borrowed `self.mir` immutably while also calling `self.push_stmt` (mutable borrow). Rewritten to an index-based loop that reads `Copy` values (`field_offset: u32`, `field_mir_ty: mir::Type`) before the mutable call.

**`@slice_from_parts` MIR lowering is trivial**: a `M::[]T` slice is already `Type::Aggregate { values: [Pointer{memory}, U32/U64] }` in MIR (see `lower_type_index` line ~928). The intrinsic just packs the two arguments into `ExprKind::Aggregate { values: Box::new([data, len]) }`. Field order `[ptr=0, len=1]` matches `@slice_len`'s `value_index: 1` for len.

---

## Decisions

**`MemAccess` in opt rather than carrying pointee type through MIR `Type::Pointer`**

Storing pointee type in `mir::Type::Pointer` was considered but rejected: it would break `Copy` on `Type`, require updating every match arm that handles `Pointer`, and would be redundant since `expr.ty` at every pointer expression site already carries the pointee type. `MemAccess` is computed once at the MIR→opt boundary and flows cleanly through the opt IR.

**Scheduler tests use `format!` interpolation, not `indoc!{}.replace()`**

`indoc!` requires compile-time string literals. For parameterized WX source strings, `format!("{STD}\nmemory heap: ...\nfn read(ptr: heap::{ptr_ty}) ...") ` is cleaner and avoids the `.replace()` anti-pattern.

**Store tests only cover one signedness variant per width**

`i32.store8` truncates the value regardless of sign — sign is irrelevant for stores. Testing both `*mut u8` and `*mut i8` would be redundant. Only `*mut u8 → I32Store8` and `*mut u16 → I32Store16` are tested.

---

## Design discussions (no code yet)

### Rich global initializers + purity

Current `build_const_expression` only handles literals, bool, and single-segment `Const` paths. The goal is to allow block expressions as global initializers, lowered into the WASM start function.

**Purity constraint**: to avoid ordering dependencies between global initializers, initializer blocks must be pure. Purity is defined as: no pointer reads/writes, no `global mut` reads/writes, no `@memory_grow`, no calls to imported functions, no indirect calls through function pointers.

**Key insight**: `local s = [0; 128]; ...; s` is pure under this definition because `s` is a fresh local value — mutations to it don't touch any other global's state. The restriction is on externally-visible state, not on all memory operations. This matches what Rust's `const fn` arrived at (allowing mutable refs to locally-created values since 1.65).

**Decision**: no `pure fn` keyword or `#[pure]` annotation in the language for now. Purity is inferred automatically in a post-inlining MIR pass.

### Purity inference pass design

**Two-phase, no extra tree walk:**

**Phase 1 — local purity during MIR lowering (free)**:
Add `is_pure: bool` to `mir::Function`, initialized `true`. Add `current_function_pure: bool` to `Builder` (alongside existing `current_function_id`). Call `mark_impure()` when emitting: `PointerLoad`, `PointerStore`, `GlobalSet`, `Global { id }` where the global is `global mut`, `MemoryGrow`, indirect calls (callee not `ExprKind::Function { id }`), direct calls to import functions.

`mark_impure()` is a no-op when `current_function_id.is_none()` — same guard used by `record_call_edge`.

To detect import calls cheaply: build a `HashSet<DefId>` of all import function ids once during `MIR::build` init, before function lowering starts. Same init pass builds a `HashSet<DefId>` of `global mut` ids for the global read check.

**Phase 2 — inlining propagation (integrated into Kahn's, O(1) per step)**:
In the Kahn's loop, after `inline_expr` / `static_data.extend_from_slice`:
```rust
if !f_body.is_pure {
    caller_func.is_pure = false;
}
```
Correct because Kahn's processes leaves first — `f_body.is_pure` at clone time reflects the fully-expanded state of `f`'s body.

**Phase 3 — SCC-based propagation after DCE (O(V+E), no tree walk)**:
Run Tarjan's algorithm on the post-inline call graph (`graph.callees` — already updated during Kahn's, NOT `mir.call_edges` which is pre-inlining). Filter to `live` DefId set (computed during DCE). Tarjan's yields SCCs in reverse topological order of the condensation DAG naturally. For each SCC: if any member has `is_pure = false` → mark all false; otherwise check all outgoing edges — if any target is impure → mark all false.

Self-recursive functions are trivial SCCs (size 1, self-edge ignored in outgoing-edge check). Mutual recursion within an SCC: pure iff all members locally pure AND all exit edges go to pure SCCs.

**Where**: `propagate_purity` lives inside `run_inlining_pass`, called after `mir.functions.retain(...)`, using the `graph` and `live` locals already in scope.

**Known problems resolved:**
1. `call_edges` is pre-inlining — use `graph.callees` (post-inlining) instead. This requires `propagate_purity` to be inside `run_inlining_pass`.
2. Import DefIds appear in `graph.callees` but not `mir.functions` — handled by marking callers impure in Phase 1; import nodes are never traversed in Tarjan's.
3. Dead functions remain in `graph.callees` after DCE — filter by `live` set in Tarjan's.
4. `mark_impure()` must not fire during `lower_global` — guarded by `current_function_id.is_none()`.
5. Mutable global detection requires TIR lookup — pre-build `HashSet<DefId>` of `global mut` ids at init.

---

## Open questions / next steps

**Immediate (purity pass)**:
- Implement the three-phase purity pass as described above
- After it works: use `is_pure` in `opt/builder.rs` to make pure call results CSE-eligible (`no_cse = false` for calls to pure functions)

**Near-term (global initializers)**:
- Add start function emission to codegen
- Add block expression support to `build_const_expression` with purity checking
- The purity checker for initializer blocks reuses the same `is_pure` infrastructure

**Intrinsics worth adding (in priority order)**:
1. `@slice_ptr<M: Memory, T>(slice: M::[]T) -> M::*T` — extracts field 0 of slice aggregate; trivial (mirrors `@slice_len`)
2. `@size_of<T>() -> u32` — compile-time size, already computed in `mir_type_layout`; needed for bump allocator alignment math
3. `@pointer_add<M: Memory, T>(ptr: M::*T, offset: M::Size) -> M::*T` — typed pointer arithmetic (offset in units of `sizeof(T)`); depends on `@size_of` internally

**Not doing**:
- `pure fn` keyword or `#[pure]` attribute — purity is inferred, not annotated
- Pointee type in `mir::Type::Pointer` — `expr.ty` already carries it; `MemAccess` at opt boundary is the right approach
