use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use tower_lsp_server::ls_types::{
	Diagnostic, DiagnosticSeverity, Position, Range, Uri,
};
use wx_compiler::vfs::FileId;

use crate::completion::{completion_items, find_enclosing_function};
use crate::{
	CompiledRoot, OpenDocument, ServerState, analyze_root, compute_refresh,
	diagnostic_publish_paths, discover_crate_root, find_active_call,
	is_current_version, owning_root,
};

/// Resolves the `FileId` for a given file path from a compiled root.
fn file_id_for(compiled: &CompiledRoot, path: &Path) -> FileId {
	compiled
		.graph
		.crates
		.iter()
		.flat_map(|cg| cg.modules.iter())
		.find(|m| Path::new(&m.file_path) == path)
		.unwrap_or_else(|| {
			panic!("file not found in compiled graph: {}", path.display())
		})
		.file_id
}

fn open_document(text: &str) -> OpenDocument {
	use std::sync::atomic::{AtomicI32, Ordering};
	static COUNTER: AtomicI32 = AtomicI32::new(1);
	OpenDocument {
		text: text.to_string(),
		lsp_version: COUNTER.fetch_add(1, Ordering::Relaxed),
	}
}

#[test]
fn is_current_version_detects_superseded_edits() {
	let root = PathBuf::from("/test/main.wx");
	let mut state = ServerState::default();
	state.open_documents.insert(
		root.clone(),
		OpenDocument {
			text: "fn test() {}".to_string(),
			lsp_version: 1,
		},
	);
	assert!(is_current_version(&state, &root, 1));

	// A newer edit lands (bumping the version) before a debounced rebuild
	// scheduled for the older version gets a chance to run.
	state.open_documents.insert(
		root.clone(),
		OpenDocument {
			text: "fn test() { local x = 1; }".to_string(),
			lsp_version: 2,
		},
	);
	assert!(!is_current_version(&state, &root, 1));
	assert!(is_current_version(&state, &root, 2));
}

#[test]
fn discover_crate_root_walks_up_to_main_wx() {
	let workspace_root = PathBuf::from("/workspace");
	let crate_root = workspace_root.join("app").join("main.wx");
	let child_file = workspace_root.join("app").join("math").join("add.wx");

	let mut open_documents = HashMap::new();
	open_documents.insert(crate_root.clone(), open_document("module math;"));

	let discovered =
		discover_crate_root(&open_documents, &[workspace_root], &child_file);

	assert_eq!(discovered, Some(crate_root));
}

#[test]
fn diagnostic_publish_paths_keeps_previous_files_for_clearing() {
	let main = PathBuf::from("/workspace/app/main.wx");
	let child = PathBuf::from("/workspace/app/math.wx");

	let previous = HashSet::from([main.clone(), child.clone()]);
	let owned_files = HashSet::from([main.clone()]);
	let diagnostics_by_file = HashMap::from([(
		main.clone(),
		vec![Diagnostic {
			range: Range::default(),
			severity: Some(DiagnosticSeverity::ERROR),
			code: None,
			code_description: None,
			source: Some("wx".to_string()),
			message: "error".to_string(),
			related_information: None,
			tags: None,
			data: None,
		}],
	)]);

	let publish_paths =
		diagnostic_publish_paths(&previous, &owned_files, &diagnostics_by_file);

	assert!(publish_paths.contains(&main));
	assert!(publish_paths.contains(&child));
	assert_eq!(publish_paths.len(), 2);
}

#[test]
#[ignore = "fix lsp later"]
fn analyze_root_updates_multi_file_diagnostics_when_overlay_changes() {
	let root = PathBuf::from("/workspace/app/main.wx");
	let child = PathBuf::from("/workspace/app/math.wx");

	let mut state = ServerState::default();
	state.open_documents.insert(
		root.clone(),
		open_document(
			"module math;\n\nfn compute() -> i32 {\n    math::add()\n}\n",
		),
	);
	state.open_documents.insert(
		child.clone(),
		open_document("fn add() -> bool {\n    true\n}\n"),
	);

	let broken = analyze_root(&mut state, &root, &mut Vec::new());
	assert!(broken.owned_files.contains(&root));
	assert!(broken.owned_files.contains(&child));
	assert!(
		broken
			.diagnostics_by_file
			.get(&root)
			.is_some_and(|diagnostics| diagnostics
				.iter()
				.any(|d| d.severity == Some(DiagnosticSeverity::ERROR))),
		"expected a root file error when child module has incompatible type"
	);

	state.open_documents.insert(
		child.clone(),
		open_document("fn add() -> i32 {\n    1\n}\n"),
	);

	let fixed = analyze_root(&mut state, &root, &mut Vec::new());
	assert!(fixed.owned_files.contains(&root));
	assert!(fixed.owned_files.contains(&child));
	assert!(
		fixed
			.diagnostics_by_file
			.get(&root)
			.is_none_or(|diagnostics| diagnostics
				.iter()
				.all(|d| d.severity != Some(DiagnosticSeverity::ERROR))),
		"expected root file errors to clear after fixing the child module overlay"
	);
}

#[test]
#[ignore = "fix lsp later"]
fn refresh_file_from_child_path_discovers_root_and_republishes_root_diagnostics()
 {
	let workspace_root = PathBuf::from("/workspace");
	let root = workspace_root.join("app").join("main.wx");
	let child = workspace_root.join("app").join("math.wx");

	let mut state = ServerState {
		workspace_folders: vec![workspace_root],
		..Default::default()
	};
	state.open_documents.insert(
		root.clone(),
		open_document(
			"module math;\n\nfn compute() -> i32 {\n    math::add()\n}\n",
		),
	);
	state.open_documents.insert(
		child.clone(),
		open_document("fn add() -> bool {\n    true\n}\n"),
	);

	let broken_publish = compute_refresh(&mut state, &child, &mut Vec::new());

	assert_eq!(owning_root(&state, &child), Some(root.as_path()));
	assert_eq!(owning_root(&state, &root), Some(root.as_path()));

	assert!(
		broken_publish.iter().any(|(path, diags)| {
			path == &root
				&& diags
					.iter()
					.any(|d| d.severity == Some(DiagnosticSeverity::ERROR))
		}),
		"expected refresh from child path to publish a root-file error"
	);

	state.open_documents.insert(
		child.clone(),
		open_document("fn add() -> i32 {\n    1\n}\n"),
	);

	let fixed_publish = compute_refresh(&mut state, &child, &mut Vec::new());

	assert!(
		fixed_publish.iter().any(|(path, diags)| {
			path == &root
				&& diags
					.iter()
					.all(|d| d.severity != Some(DiagnosticSeverity::ERROR))
		}),
		"expected refresh from child path to clear root-file errors after fixing the child overlay"
	);
}

// ── Completion integration tests
// ─────────────────────────────────────────────────────

fn compile_source(root: &PathBuf, source: &str) -> (ServerState, CompiledRoot) {
	let mut state = ServerState::default();
	state
		.open_documents
		.insert(root.clone(), open_document(source));
	analyze_root(&mut state, root, &mut Vec::new());
	let compiled = state.cached.remove(root).expect("compilation failed");
	(state, compiled)
}

/// Compiles `root_source` alongside additional `(path, source)` files (e.g.
/// files pulled in via `module foo;` declarations).
fn compile_multi_source(
	root: &PathBuf,
	root_source: &str,
	extra_files: &[(&PathBuf, &str)],
) -> (ServerState, CompiledRoot) {
	let mut state = ServerState::default();
	state
		.open_documents
		.insert(root.clone(), open_document(root_source));
	for (path, source) in extra_files {
		state
			.open_documents
			.insert((*path).clone(), open_document(source));
	}
	analyze_root(&mut state, root, &mut Vec::new());
	let compiled = state.cached.remove(root).expect("compilation failed");
	(state, compiled)
}

#[test]
fn completion_inside_function_includes_params() {
	let root = PathBuf::from("/test/main.wx");
	// Cursor is on the blank line inside the function body (after the newline).
	let source = "fn add(a: i32, b: i32) -> i32 {\n    \n}";
	// offset 37 lands in the "    " whitespace on the second line, inside the body.
	let cursor = 37;

	let (_, compiled) = compile_source(&root, source);
	let file_id = file_id_for(&compiled, &root);

	let items = completion_items(
		&compiled.tir,
		&compiled.graph.interner,
		&compiled.symbol_index,
		file_id,
		source,
		cursor,
	);
	let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

	assert!(
		labels.contains(&"a"),
		"expected param `a` in completions; got: {labels:?}"
	);
	assert!(
		labels.contains(&"b"),
		"expected param `b` in completions; got: {labels:?}"
	);
}

#[test]
fn completion_inside_function_includes_locals_declared_before_cursor() {
	let root = PathBuf::from("/test/main.wx");
	// "x" is declared before cursor; "y" is declared after.
	let source = "fn f() -> i32 {\n    local x: i32 = 1;\n    \n    local y: i32 = 2;\n    x\n}";
	// Find the offset of the blank line (after "    local x: i32 = 1;\n    ").
	let cursor = source.find("\n    local y").unwrap(); // just before "local y"

	let (_, compiled) = compile_source(&root, source);
	let file_id = file_id_for(&compiled, &root);

	let items = completion_items(
		&compiled.tir,
		&compiled.graph.interner,
		&compiled.symbol_index,
		file_id,
		source,
		cursor,
	);
	let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

	assert!(
		labels.contains(&"x"),
		"expected `x` in completions; got: {labels:?}"
	);
	assert!(
		!labels.contains(&"y"),
		"expected `y` NOT in completions yet; got: {labels:?}"
	);
}

#[test]
fn completion_excludes_impl_methods_and_associated_functions() {
	let root = PathBuf::from("/test/main.wx");
	let source = indoc::indoc! { "
		struct Point {
		    x: i32,
		    y: i32,
		}

		impl Point {
		    fn new(x: i32, y: i32) -> Point {
		        Point { x: x, y: y }
		    }

		    pub fn sum(self) -> i32 {
		        self.x + self.y
		    }
		}

		fn test() {

		}
	" };
	// Cursor on the blank line inside `test`'s body.
	let cursor = source.find("fn test() {\n").unwrap() + "fn test() {\n".len();

	let (_, compiled) = compile_source(&root, source);
	let file_id = file_id_for(&compiled, &root);

	let items = completion_items(
		&compiled.tir,
		&compiled.graph.interner,
		&compiled.symbol_index,
		file_id,
		source,
		cursor,
	);
	let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

	assert!(
		labels.contains(&"test"),
		"expected free function `test` in completions; got: {labels:?}"
	);
	assert!(
		labels.contains(&"Point"),
		"expected struct `Point` in completions; got: {labels:?}"
	);
	assert!(
		!labels.contains(&"new"),
		"associated function `Point::new` must not be bare-callable; got: {labels:?}"
	);
	assert!(
		!labels.contains(&"sum"),
		"method `Point::sum` must not be bare-callable; got: {labels:?}"
	);
}

#[test]
fn completion_inside_function_shows_globals_too() {
	let root = PathBuf::from("/test/main.wx");
	let source = "fn helper() -> i32 { 0 }\nfn main() -> i32 {\n    \n}";
	// Cursor in the blank line inside main's body.
	let cursor = source.find("\n    \n}").unwrap() + 5;

	let (_, compiled) = compile_source(&root, source);
	let file_id = file_id_for(&compiled, &root);

	let items = completion_items(
		&compiled.tir,
		&compiled.graph.interner,
		&compiled.symbol_index,
		file_id,
		source,
		cursor,
	);
	let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

	assert!(
		labels.contains(&"helper"),
		"expected global function `helper` in completions; got: {labels:?}"
	);
}

#[test]
fn completion_sorts_locals_before_globals() {
	let root = PathBuf::from("/test/main.wx");
	// "zeta" (local) would sort after "alpha" (global) alphabetically by
	// label — locals should still come first via `sort_text`.
	let source =
		"fn alpha() -> i32 { 0 }\nfn main() -> i32 {\n    local zeta = 1;\n\n}";
	let cursor = source.find("1;\n").unwrap() + "1;\n".len();

	let (_, compiled) = compile_source(&root, source);
	let file_id = file_id_for(&compiled, &root);

	let items = completion_items(
		&compiled.tir,
		&compiled.graph.interner,
		&compiled.symbol_index,
		file_id,
		source,
		cursor,
	);

	let zeta = items
		.iter()
		.find(|i| i.label == "zeta")
		.expect("local `zeta` should be suggested");
	let alpha = items
		.iter()
		.find(|i| i.label == "alpha")
		.expect("global `alpha` should be suggested");

	assert!(
		zeta.sort_text < alpha.sort_text,
		"expected local `zeta` to sort before global `alpha`; zeta sort_text={:?}, alpha sort_text={:?}",
		zeta.sort_text,
		alpha.sort_text
	);
}

#[test]
fn completion_prefix_filters_results() {
	let root = PathBuf::from("/test/main.wx");
	let source = "fn alpha() -> i32 { 0 }\nfn beta() -> i32 {\n    al\n}";
	// Cursor right after "al" inside beta's body.
	let cursor = source.find("al\n}").unwrap() + 2;

	let (_, compiled) = compile_source(&root, source);
	let file_id = file_id_for(&compiled, &root);

	let items = completion_items(
		&compiled.tir,
		&compiled.graph.interner,
		&compiled.symbol_index,
		file_id,
		source,
		cursor,
	);
	let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

	assert!(
		labels.contains(&"alpha"),
		"expected `alpha` in completions; got: {labels:?}"
	);
	assert!(
		!labels.contains(&"beta"),
		"expected `beta` filtered out; got: {labels:?}"
	);
}

#[test]
fn completion_hides_sibling_module_items_without_use() {
	let root = PathBuf::from("/test/main.wx");
	let math = PathBuf::from("/test/math.wx");
	let source = "module math;\nfn main() -> i32 {\n    \n}";
	let cursor = source.find("\n    \n}").unwrap() + 5;

	let (_, compiled) = compile_multi_source(
		&root,
		source,
		&[(&math, "pub fn add() -> i32 { 1 }")],
	);
	let file_id = file_id_for(&compiled, &root);

	let items = completion_items(
		&compiled.tir,
		&compiled.graph.interner,
		&compiled.symbol_index,
		file_id,
		source,
		cursor,
	);
	let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

	assert!(
		labels.contains(&"main"),
		"expected root-level `main` in completions; got: {labels:?}"
	);
	assert!(
		!labels.contains(&"add"),
		"expected `math::add` NOT to be visible unqualified from root; got: {labels:?}"
	);
}

#[test]
fn completion_shows_sibling_module_items_via_wildcard_use() {
	let root = PathBuf::from("/test/main.wx");
	let math = PathBuf::from("/test/math.wx");
	let source = "module math;\nuse math::*;\nfn main() -> i32 {\n    \n}";
	let cursor = source.find("\n    \n}").unwrap() + 5;

	let (_, compiled) = compile_multi_source(
		&root,
		source,
		&[(&math, "pub fn add() -> i32 { 1 }")],
	);
	let file_id = file_id_for(&compiled, &root);

	let items = completion_items(
		&compiled.tir,
		&compiled.graph.interner,
		&compiled.symbol_index,
		file_id,
		source,
		cursor,
	);
	let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

	assert!(
		labels.contains(&"add"),
		"expected `add` visible after `use math::*;`; got: {labels:?}"
	);
}

#[test]
fn resolve_source_and_offset_prefers_live_buffer_over_stale_compiled_source() {
	// Regression test: signature_help used to compute `offset` from
	// `compiled.graph.files` (the source as of the last save) and then slice
	// into that same stale, shorter string. Typing new lines above the
	// cursor without saving (e.g. wrapping existing code in a new `module`
	// block) pushed the live cursor position past the stale source's length
	// and panicked on `source[..offset]`. `resolve_source_and_offset` is the
	// shared fix both `completion` and `signature_help` now go through —
	// this exercises it directly against the exact reported scenario.
	let root = PathBuf::from("/test/main.wx");
	let stale_source = "fn test() {\n\n}";
	let (mut state, compiled) = compile_source(&root, stale_source);
	state.cached.insert(root.clone(), compiled);

	let live_source = "module test {\n    fn foo()\n}\n\nfn test() {\n\n}";
	state
		.open_documents
		.insert(root.clone(), open_document(live_source));

	// Cursor right after "fn foo()" on line 1 — this line doesn't exist at
	// all in `stale_source`, which is only 14 bytes long.
	let position = Position {
		line: 1,
		character: 11,
	};
	let uri = Uri::from_file_path(&root).expect("valid file uri");
	let compiled = state.cached.get(&root).unwrap();
	let file_id = file_id_for(compiled, &root);

	// Must not panic, and must resolve against the live buffer (not the
	// 14-byte stale one) — this is exactly what used to panic.
	let (source, offset) = crate::resolve_source_and_offset(
		&state, compiled, &uri, file_id, position,
	)
	.expect("should resolve source/offset from the live buffer");
	assert_eq!(source, live_source);
	assert!(offset <= source.len());

	let call = find_active_call(source, offset);
	assert!(call.is_some(), "expected an active call for `fn foo(`");
}

#[test]
fn resolve_uri_finds_virtual_stdlib_module() {
	// Regression test: `Uri::to_file_path()` doesn't check the scheme, so it
	// returns a bogus `Some(path)` for a `wx://std/...` virtual URI instead
	// of `None`. `resolve_uri` must not use that to short-circuit past the
	// string-comparison fallback, or hover/goto-definition/semantic-tokens
	// silently stop working inside the standard library file.
	let root = PathBuf::from("/test/main.wx");
	let (_, compiled) = compile_source(&root, "fn main() {}");
	let stdlib_file_id = compiled
		.graph
		.crates
		.iter()
		.flat_map(|cg| cg.modules.iter())
		.find(|m| m.file_path == "lib.wx")
		.map(|m| m.file_id)
		.expect("stdlib module should be present in the compiled graph");
	let uri = crate::file_id_to_uri(&compiled, stdlib_file_id)
		.expect("should construct a wx://std/ URI for the stdlib module");

	let mut state = ServerState::default();
	state.cached.insert(root.clone(), compiled);

	let resolved = crate::resolve_uri(&state, &uri);
	assert_eq!(
		resolved.map(|(_, file_id)| file_id),
		Some(stdlib_file_id),
		"resolve_uri should find the stdlib module via its constructed wx:// URI"
	);
}

#[test]
fn find_enclosing_function_returns_correct_function() {
	let root = PathBuf::from("/test/main.wx");
	let source = "fn first() -> i32 { 1 }\nfn second() -> i32 { 2 }";

	let (_, compiled) = compile_source(&root, source);
	let file_id = file_id_for(&compiled, &root);

	// Cursor inside "first" body — "{ 1 }" roughly at [19, 24)
	let in_first = source.find("{ 1 }").unwrap() as u32 + 2;
	let in_second = source.find("{ 2 }").unwrap() as u32 + 2;

	let first_idx = find_enclosing_function(&compiled.tir, file_id, in_first);
	let second_idx = find_enclosing_function(&compiled.tir, file_id, in_second);

	assert!(
		first_idx.is_some(),
		"expected to find enclosing function for cursor in `first`"
	);
	assert!(
		second_idx.is_some(),
		"expected to find enclosing function for cursor in `second`"
	);
	assert_ne!(
		first_idx, second_idx,
		"cursor positions in different functions should map to different indices"
	);
}
