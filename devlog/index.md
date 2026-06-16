# Session Index

## 2026-06-16

- [2026-06-16-type-formatter-and-unit-cleanup.md](2026-06-16-type-formatter-and-unit-cleanup.md) — `TypeFormatter` context-aware generic display, intrinsic LSP fix, plan for `unit` → `()` unification and `Grouping` removal
- [2026-06-16-abstract-memory-trait.md](2026-06-16-abstract-memory-trait.md) — Removed Memory32/Memory64 sugar traits, `impl Trait` syntax removal, `TypeParamOwner::Trait` for trait const Self, generic array indexing fix via `AssocTypeProjection`
- [2026-06-16-lang-items-and-codegen-fixes.md](2026-06-16-lang-items-and-codegen-fixes.md) — Lang items map (`#[lang = "key"]`), MIR Slice lowering, codegen test suite fixes, `DefId` architecture discussion

## 2026-06-15

- [2026-06-15-tir-ast-refactor.md](2026-06-15-tir-ast-refactor.md) — Refactored TIR builder AST registry: Vec+HashMap replacing two HashMaps, named structs, `file_id`/`namespace` moved to `AstEntry`
