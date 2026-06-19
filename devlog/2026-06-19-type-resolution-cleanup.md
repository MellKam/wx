# 2026-06-19 — Memory management refactor, `typeset`, type resolution error signaling, test audit

## Summary

Two bodies of work committed today. First: removed auto-generated `SIZE`/`ALIGN` constants, updated the bump allocator to use `@size_of`/`@align_of` intrinsics, introduced a generic `Vec<M: Memory, T>` example, implemented `typeset` for compile-time type constraints, fixed `#[inline]` propagation to monomorphized instances, and improved the formatter's handling of generic type parameters. Second: refactored the two type-position resolution helpers (`resolve_type_identifier` and `resolve_namespace_type_member`) to return `Result<TypeIndex, ()>` instead of the `TypeIndex::ERROR` sentinel, added `register_module_access` to type-position namespace traversal, and audited and strengthened weak test assertions across `tir/tests.rs`.

## Changes

### `resolve_type_identifier` and `resolve_namespace_type_member` return type (`tir/builder.rs`)

Both functions previously returned `TypeIndex`, using `TypeIndex::ERROR` as the error sentinel. They now return `Result<TypeIndex, ()>`:

- All error paths return `Err(())`
- All success paths return `Ok(ty)`
- Call sites in functions that already return `Result` use `?`
- Call sites inside `resolve_type` (which must return a `TypeIndex` for the rest of the pipeline) use `let Ok(x) = ... else { return TypeIndex::ERROR; }` or `.unwrap_or(TypeIndex::ERROR)`

This matches how `resolve_namespace_member` already worked for value-space resolution and makes it impossible for callers to silently miss an error (the compiler will warn on unused `Result`).

### `register_module_access` in type-position walks (`tir/builder.rs`)

`resolve_namespace_type_member` gained a new `namespace_span: TextSpan` parameter. In the `Type::Module` branch, before doing the symbol lookup, it now calls `self.register_module_access(namespace_ty, SourceSpan::new(file_id, namespace_span))`. This makes module accesses in type position (e.g. `fn f(v: math::Vec2)`) visible to the LSP for go-to-definition and find-references, the same way value-position module accesses were already registered.

Call sites in `resolve_type`, the const evaluator path, and `build_struct_init_expression` each track `namespace_span` per segment through the multi-segment loop.

A TODO comment marks both the multi-segment loop in `resolve_type` and the path-builder path where full per-intermediate-segment LSP support is deferred until `ExprKind` gains a nested namespace node:

```rust
// TODO: for full LSP per-segment support, ExprKind needs a nested
// namespace node so each intermediate segment carries its own span and
// TypeIndex.  Until then each lookup registers only its own segment span.
```

### New tests (`tir/tests.rs`)

Six new tests covering the type-position paths:

- `test_type_position_inline_module_registers_module_access` — verifies that `math::Vec2` in a parameter type registers a module access on the `math` module declaration
- `test_type_position_three_segment_inline_module_path` — exercises the intermediate loop in `resolve_type` via `outer::inner::Point`
- `test_type_position_undeclared_in_module_path_is_error` — `shapes::NonExistent` must emit E1021
- `test_type_position_non_namespace_as_intermediate_is_error` — using a primitive (`i32`) as a namespace in type position must emit E1021
- `test_struct_init_three_segment_inline_module_path` — struct literal `outer::inner::Point::{ x: 1, y: 2 }` exercises the loop inside `build_struct_init_expression`
- `test_struct_init_undeclared_type_in_module_path_is_error` — `shapes::Ghost::{ }` in return position must emit E1021

### Test audit — weak assertions hardened (`tir/tests.rs`)

Audited all test assertions and upgraded every upgradable weak check:

| Pattern | Upgraded to |
|---|---|
| `assert!(!case.tir.diagnostics.is_empty())` | `assert!(has_error_code(&case.tir, "EXXX"))` |
| `has_error_matching("message substring")` (where emitter has `with_code`) | `has_error_code("EXXX")` |
| `assert_eq!(case.tir.diagnostics.len(), 1)` | `has_error_code("EXXX")` |

Tests updated: `test_duplicate_export`, `test_duplicate_export_with_alias` (E1018), `test_coerce_int_negative_for_u32` (E1005), `test_comptime_integer_vs_float_comparison_errors` (E1014), `test_struct_init_undeclared_name` (E1021), `test_struct_init_errored_field_not_reported_as_missing` (E1001), `test_generic_unknown_bound_is_error` (E1021), `test_assoc_type_bare_name_suggests_self_prefix` (E1021), `test_type_application_wrong_arg_count_is_error` (E1040), `test_type_application_on_non_generic_is_error` (E1040), `test_deref_store_through_immutable_pointer_is_error` (W1000), `test_deref_arithmetic_assignment_through_immutable_pointer_is_error` (W1000), `test_index_wrong_index_type_is_error` (E1001), `test_index_store_through_immutable_array_is_error` (W1000), `test_index_arithmetic_assignment_through_immutable_array_is_error` (W1000), `test_index_memory32_with_u64_variable_is_error` (E1001), `test_generic_impl_bare_type_param_is_error` (E1001), `test_slice_range_on_non_indexable_is_error` (E1042).

`test_generic_call_with_non_satisfying_type_is_error` was **removed**: when strengthened from `!is_empty()` to `has_error_code("E1001")`, it failed because the only diagnostic emitted was an unused-variable warning. TIR does not check trait bounds at generic call sites (that is MIR/monomorphization work). The test was a false positive that only passed because the old assertion was too weak.

Left as `has_error_matching` (emitter has no `with_code`):
- `report_binary_expression_mistmatch` ("cannot assign") — no `DiagnosticCode` attached
- "array element type must be a numeric type" — no `DiagnosticCode` attached

## Key findings

- **`report_binary_expression_mistmatch` has no error code.** The function emits type-mismatch diagnostics for `=`, `+=`, `-=` etc. without calling `with_code()`. The remaining `has_error_matching` calls in tests for "cannot assign" are stuck at message matching until that function gets a code.
- **TIR does not check trait bounds at generic call sites.** Trait conformance is only checked at `impl Trait for Type` (Phase 3.5). A call `add::<bool>(true, false)` where `T: Arithmetic` is not satisfied produces no diagnostic in TIR — it would only fail when MIR tries to monomorphize. Keep this in mind when writing TIR-level tests for generics.

## Decisions

- Return `Result<TypeIndex, ()>` from type-resolution helpers rather than using the sentinel. The sentinel is still the public contract of `resolve_type` itself (called from many places in the pipeline that expect a `TypeIndex`), but the internal helpers now express failure explicitly.
- Defer the per-intermediate-segment LSP registration until `ExprKind` has a nested namespace node. The current `NamespaceAccess.namespace: Spanned<TypeIndex>` can only record one resolved type per access expression; representing a chain like `a::b::C` as a nested tree would require `Box<Expression>` there.

## Open questions

- Should `report_binary_expression_mistmatch` get a `DiagnosticCode`? The obvious candidate would be E1001 (type mismatch), but binary-expression mismatches might warrant a distinct code.
- Should TIR check trait-bound satisfaction at generic call sites, or is leaving it to MIR monomorphization the right layering? Current design means invalid generic instantiations produce no error unless the function is actually reachable from an export.
