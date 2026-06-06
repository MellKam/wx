# TODO: Import Module Declaration Files

## Problem

Currently, `import "env" { fn malloc(size: u32) -> u32; }` can only be written inline.
There is no way to declare the import block in a separate file (like `env.wx`) and then
reference it by name from other files.

## Desired syntax (sketch)

```wx
// env.wx — declares the import module
import module env from "env";

fn malloc(size: u32) -> u32;
fn free(ptr: u32);
```

```wx
// main.wx — uses it
module env;

pub fn run() {
    let ptr = env::malloc(256);
    env::free(ptr);
}
```

## Current architecture

After the `ModuleNamespace` unification (see `tir/mod.rs`), the TIR has three parallel
structures:

- `tir.namespaces: Vec<ModuleNamespace>` — the symbol table for every named scope
  (both local modules and import modules share the same vec)
- `tir.module_decls: Vec<ModuleDecl>` — declaration-site metadata for `module foo;` / `module foo {}`
- `tir.import_decls: Vec<ImportDecl>` — declaration-site metadata for `import "env" { }`;
  also holds `lookup: HashMap<SymbolU32, ImportValue>` for MIR codegen

`ModuleDecl` already has `own_file_id: Option<FileId>` for multi-file local modules.
The analogous field on `ImportDecl` would point to the `.wx` file that declares the import
module's items.

## What needs to change

1. **Parser / AST** — add a new top-level item syntax for declaring an import module file,
   e.g. `import module <name> from "<external>";`

2. **VFS** — `ensure_module_path` already sets `own_file_id` on the last path segment for
   local modules. A similar mechanism is needed for import modules so the VFS can load
   the file that contains the import declarations.

3. **TIR builder** — when processing an import module file, the builder needs to recognise
   that the items in that file are WASM imports and register them in an `ImportDecl` rather
   than as normal function definitions. The per-item `AstNodeRef::ImportedFunction` path
   already handles this; it just needs to be triggered from the file loader too.

4. **LSP goto-definition** — `ModuleDecl.own_file_id` is already wired up for local modules.
   `ImportDecl` needs an equivalent so hovering over an import module name can navigate to
   its declaration file.

## Open questions

- Should the import declaration file use `import module` at the top, or should the file
  extension / path be enough to identify it as an import module?
- How do we prevent a file from being loaded as both a local module and an import module?
- Should the external WASM module name be inferred from the file name, or always explicit?
