# Session Index

## 2026-06-21

- [2026-06-21-assoc-type-chains-comptime-inference.md](2026-06-21-assoc-type-chains-comptime-inference.md) — Nested `AssocTypeProjection` chains (`A::M::Size`), `are_scalar_compatible` for abstract-memory pointer casts, `null_mut` in std, formatter `::` before intrinsic type args, cross-branch comptime inference in if-else

## 2026-06-19

- [2026-06-19-type-resolution-cleanup.md](2026-06-19-type-resolution-cleanup.md) — `Result<TypeIndex, ()>` return for type-resolution helpers, `register_module_access` in type-position paths, test audit: weak assertions hardened to `has_error_code`, false-positive test removed
- [2026-06-19-inline-generics-vec-fmt.md](2026-06-19-inline-generics-vec-fmt.md) — `#[inline]` propagation to mono instances, removed memory method special case, receiver into `arguments[0]`, removed `SIZE`/`ALIGN` auto-constants, generic bump allocator with `@size_of`/`@align_of`, generic `Vec<M, T>` example, formatter generic param support for structs and impl blocks
- [2026-06-19-null-pointers-mut-coercion-sized-design.md](2026-06-19-null-pointers-mut-coercion-sized-design.md) — `null()` intrinsic, `*mut T → *T` implicit coercion, mixed-mutability pointer comparison, bump allocator linked list rewrite, `Sized` trait and typeset-bounded const design discussion

## 2026-06-18

- [2026-06-18-narrow-loads-slice-intrinsics-purity-design.md](2026-06-18-narrow-loads-slice-intrinsics-purity-design.md) — Narrow load/store bug fix (`MemAccess` enum), `@slice_from_parts` intrinsic, purity inference pass design (SCC-based, integrated into inlining pass)

## 2026-06-16

- [2026-06-16-slice-range-and-generic-impl.md](2026-06-16-slice-range-and-generic-impl.md) — Generic `impl<M, T> M::[]T` blocks, exclusive slice range `arr[i..n]`, lexer float/`..` bug fix, `from > to` trap
- [2026-06-16-type-formatter-and-unit-cleanup.md](2026-06-16-type-formatter-and-unit-cleanup.md) — `TypeFormatter` context-aware generic display, intrinsic LSP fix, plan for `unit` → `()` unification and `Grouping` removal
- [2026-06-16-abstract-memory-trait.md](2026-06-16-abstract-memory-trait.md) — Removed Memory32/Memory64 sugar traits, `impl Trait` syntax removal, `TypeParamOwner::Trait` for trait const Self, generic array indexing fix via `AssocTypeProjection`
- [2026-06-16-lang-items-and-codegen-fixes.md](2026-06-16-lang-items-and-codegen-fixes.md) — Lang items map (`#[lang = "key"]`), MIR Slice lowering, codegen test suite fixes, `DefId` architecture discussion

## 2026-06-15

- [2026-06-15-tir-ast-refactor.md](2026-06-15-tir-ast-refactor.md) — Refactored TIR builder AST registry: Vec+HashMap replacing two HashMaps, named structs, `file_id`/`namespace` moved to `AstEntry`
