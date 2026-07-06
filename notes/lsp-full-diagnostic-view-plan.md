# LSP: "Click for full compiler diagnostic" view

## Goal

Reproduce rust-analyzer's "click the diagnostic code to see the full
rendered diagnostic" feature for `wx-lsp-next`: clicking a diagnostic's code
badge in VSCode opens a read-only virtual document showing the same
multi-line, source-snippet-annotated rendering `wx-cli` already prints to a
terminal (gutter, line numbers, carets under the offending span, `note:`
lines), instead of just the single-line message VSCode's Problems panel
shows today.

## Non-goals (this pass)

- **No ANSI coloring.** The virtual document is plain text. rust-analyzer's
  version colorizes it via a client-side `anser`-based ANSI parser +
  `TextEditorDecorationType` per style run (see `editors/code/src/diagnostics.ts`
  in `rust-lang/rust-analyzer`) — that's a real, separate follow-up, not part
  of this MVP.
- **No new persistent server state.** The whole design below is built around
  *not* needing one (see "Why lazy, and why no new state" below).

## Why lazy, and why no new state

Two designs were considered:

1. **Eager**: render every diagnostic's full text in `add_compiler_diagnostic`
   and stash it in `Diagnostic.data`, shipped to the client on every
   `publishDiagnostics`. This is what rust-analyzer's `data.rendered` field
   looks like on the wire — but for rust-analyzer that rendering is free
   (rustc already produced it as part of `cargo check --message-format=json`,
   whether or not it's used). For us, `wx-lsp-next` would be doing that
   rendering itself, for every diagnostic, whether or not the user ever
   clicks it, and shipping the (small but nonzero) text over the wire on
   every republish regardless of use.
2. **Lazy (chosen)**: don't render anything up front. Client only carries a
   link (a custom-scheme URI encoding `(file, diagnostic index)`); the
   content provider asks the server for the full text on click, via a new
   custom LSP request, mirroring the existing `wx/virtualFileContent`
   request (used today for virtual stdlib files, see
   `crates/wx-lsp-next/src/main.rs:746` / `vscode/src/extension.ts:112-122`).

Lazy only works cheaply because **the raw, re-renderable diagnostics are
already kept alive** in `ServerState.cached: HashMap<PathBuf, CompiledRoot>`
(`crates/wx-lsp-next/src/main.rs:104`) for exactly as long as we'd need them:
`CompiledRoot.tir.diagnostics` and `CompiledRoot.graph.crates[].diagnostics`
are the original `codespan_reporting::Diagnostic<FileId>` values, full labels
and all — the same values `analysis_from_compiled_root`
(`main.rs:1024`) already reads to build the LSP-shape diagnostics in the
first place. So a lazy request handler doesn't need a new cache: it just
re-derives the same list on demand and re-renders one entry from it. If the
root gets rebuilt (debounced edit) before the user clicks, the lookup
naturally misses — handled as a normal "no longer available" fallback, not a
new failure mode (VSCode's own diagnostic collection would be just as stale
in the eager design).

## Server-side changes (`crates/wx-lsp-next/src/main.rs`)

1. **New params struct**, next to `VirtualFileContentParams` (`main.rs:72`):

   ```rust
   #[derive(serde::Deserialize)]
   struct FullDiagnosticParams {
       uri: String,
       index: usize,
   }
   ```

2. **Shared "does this diagnostic belong to file X" helper** — this is the
   one part that needs care, not just a sketch. `add_compiler_diagnostic`
   silently drops a diagnostic (doesn't push it into `grouped` at all) if
   *any* of four checks fail: no primary-or-first label
   (`main.rs:1105-1112`), the label's file name doesn't resolve
   (`main.rs:1114-1116`), the resolved path isn't absolute (`main.rs:1118-1120`,
   true for stdlib's virtual `wx://std/...` "files"), or `span_to_range`
   fails on the label's span (`main.rs:1122-1130`). The index the client
   sees (`idx` in `diagnosticList.forEach`) is the position within the
   *survivors* of all four checks — so the lazy handler's filter has to
   replicate every one of them, not just "does the file id match", or a
   click could silently render the wrong diagnostic once any earlier one
   for that file had been dropped by one of these guards.

   Fix: extract the shared part into one function both sides call:

   ```rust
   /// Returns the absolute path + LSP range a diagnostic would be filed
   /// under, or `None` for the same reasons `add_compiler_diagnostic`
   /// silently drops a diagnostic (no resolvable primary/first label, the
   /// label's file has no name, the name isn't an absolute path, or the
   /// label's span doesn't map to a valid range). Single source of truth
   /// for "which diagnostics belong to file X, in what order" — used both
   /// when building the published list and when re-deriving it later for
   /// `wx/fullDiagnostic`, so the two can't silently drift apart.
   fn diagnostic_location(
       files: &vfs::Files,
       diagnostic: &CodeDiagnostic<FileId>,
   ) -> Option<(PathBuf, Range)> {
       let label = diagnostic
           .labels
           .iter()
           .find(|label| label.style == LabelStyle::Primary)
           .or_else(|| diagnostic.labels.first())?;
       let path = PathBuf::from(files.name(label.file_id).ok()?);
       if !path.is_absolute() {
           return None;
       }
       let range = span_to_range(
           files,
           SourceSpan::new(
               label.file_id,
               TextSpan::new(label.range.start as u32, label.range.end as u32),
           ),
       )?;
       Some((path, range))
   }
   ```

   `add_compiler_diagnostic` calls this first thing and returns early on
   `None`, using the returned `(path, range)` in place of what it computes
   inline today.

3. **New handler**, next to `virtual_file_content` (`main.rs:746`):

   ```rust
   async fn full_diagnostic(&self, params: FullDiagnosticParams) -> Result<String> {
       let uri = Uri::from_str(&params.uri).map_err(|_| {
           JsonRpcError::invalid_params(format!("bad uri: {}", params.uri))
       })?;
       let state = self.state.lock().await;
       let Some((compiled, _file_id)) = resolve_uri(&state, &uri) else {
           return Ok("Unable to find original wx diagnostic \
                      (file is no longer tracked).".to_string());
       };
       let Some(target_path) = uri_to_path(&uri) else {
           return Ok("Unable to find original wx diagnostic.".to_string());
       };
       // Same order `analysis_from_compiled_root` iterates in — crate
       // diagnostics per crate, then TIR diagnostics — and the same
       // acceptance rule (`diagnostic_location`), so `index` lines up
       // exactly with what the client saw when this was published.
       let diagnostic = compiled
           .graph
           .crates
           .iter()
           .flat_map(|cg| cg.diagnostics.iter())
           .chain(compiled.tir.diagnostics.iter())
           .filter(|d| {
               diagnostic_location(&compiled.graph.files, d)
                   .is_some_and(|(path, _)| path == target_path)
           })
           .nth(params.index);
       let Some(diagnostic) = diagnostic else {
           return Ok("Unable to find original wx diagnostic \
                      (it may have changed since this link was created).".to_string());
       };
       let mut rendered = String::new();
       let _ = term::emit_to_string(
           &mut rendered,
           &term::Config::default(),
           &compiled.graph.files,
           diagnostic,
       );
       Ok(rendered)
   }
   ```

4. **Register the method** in `main()` (`main.rs:772-783`), alongside the
   existing one:

   ```rust
   .custom_method("wx/virtualFileContent", Backend::virtual_file_content)
   .custom_method("wx/fullDiagnostic", Backend::full_diagnostic)
   ```

5. **`add_compiler_diagnostic`** (`main.rs:1100`): switches to calling
   `diagnostic_location` for its own path/range instead of computing them
   inline. `data` stays `None` — no per-diagnostic marker is needed, since
   `add_compiler_diagnostic` only ever pushes a diagnostic *after*
   `diagnostic_location` has already succeeded, so every diagnostic that
   reaches the client is unconditionally re-renderable. The client instead
   gates on `diag.source === "wx"` (set unconditionally here, but not by the
   synthetic `"wx-lsp"` fallback diagnostic in
   `analysis_from_missing_entry_file`, which has no underlying raw diagnostic
   to re-render). No rendering change here — `term::emit_to_string` is *not*
   called from this function, that's the whole point of laziness.

6. Add `use codespan_reporting::term;` to the top-level imports.

## Client-side changes (`vscode/src/extension.ts`)

1. **Diagnostics middleware** — add to `clientOptions`
   (`extension.ts:55-64`, currently has no `middleware` key):

   ```ts
   middleware: {
       handleDiagnostics(uri, diagnosticList, next) {
           diagnosticList.forEach((diag, idx) => {
               if (diag.source !== "wx") return;
               diag.code = {
                   target: vscode.Uri.from({
                       scheme: "wx-diagnostics-view",
                       path: `/diagnostic message [${idx}]`,
                       fragment: uri.toString(),
                       query: idx.toString(),
                   }),
                   value: "Click for full compiler diagnostic",
               };
           });
           next(uri, diagnosticList);
       },
   },
   ```

2. **Content provider**, registered alongside the existing `wx` scheme
   provider (`extension.ts:112-122`):

   ```ts
   context.subscriptions.push(
       workspace.registerTextDocumentContentProvider("wx-diagnostics-view", {
           provideTextDocumentContent: async (uri: Uri) => {
               if (!client) return null;
               return client.sendRequest("wx/fullDiagnostic", {
                   uri: uri.fragment,
                   index: Number(uri.query),
               });
           },
       }),
   );
   ```

   Mirrors the existing `wx/virtualFileContent` provider exactly — same
   shape, different scheme/method.

## Data shapes

- `Diagnostic.data`: stays `None`/absent — no marker needed. The client gates
  on `diagnostic.source === "wx"` instead, which is already unconditionally
  true for every diagnostic `add_compiler_diagnostic` produces (and false for
  the one synthetic `"wx-lsp"` fallback diagnostic, which has nothing to
  re-render). No rendered text ever touches the wire.
- Virtual URI: `wx-diagnostics-view:/diagnostic message [N]?N#<file-uri>` —
  same layout as rust-analyzer's `rust-analyzer-diagnostics-view:` URIs
  (path is a human-readable label only; `query` is the real index; `fragment`
  is the real file URI).
- `wx/fullDiagnostic` request: `{ uri: string, index: number }` -> `string`
  (plain rendered text, or a fallback message).

## Edge cases

- **File no longer tracked** (root evicted/closed): `resolve_uri` returns
  `None` -> fallback string.
- **Root rebuilt since the link was made** (index now out of range, or
  points at a different diagnostic after edits): `.nth(index)` returns
  `None`, or silently returns the *wrong* diagnostic if the list reordered
  without changing length. Accepted as-is for this MVP — same staleness
  window every "click a link to old state" feature has; not solvable
  without content-addressing diagnostics, which is out of scope here.
- **Multiple crates/roots**: `resolve_uri` already scans all of
  `state.cached`, so this isn't a new concern.

## Testing plan

- Rust: a `wx-lsp-next` test constructing a `CompiledRoot` with a known
  diagnostic, calling `full_diagnostic` with the right/wrong index, and
  asserting the rendered text contains the expected source snippet (or the
  fallback string for a bad index/uri).
- Manual: open a `.wx` file with a warning in the real extension, confirm
  the code badge reads "Click for full compiler diagnostic", click it,
  confirm the virtual doc shows the same text `wx-cli` would print for that
  diagnostic.

## Future work

- ANSI coloring in the virtual document, matching rust-analyzer's
  `AnsiDecorationProvider` (render with `termcolor::Ansi` instead of
  plain `emit_to_string`, parse client-side with an ANSI library, apply
  `TextEditorDecorationType`s mapped to `ThemeColor("terminal.ansi*")`).
