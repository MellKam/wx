use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use super::*;

fn temp_test_dir(test_name: &str) -> PathBuf {
	let unique = SystemTime::now()
		.duration_since(UNIX_EPOCH)
		.unwrap()
		.as_nanos();
	let path = std::env::temp_dir().join(format!(
		"wx-vfs-{test_name}-{}-{unique}",
		std::process::id()
	));
	fs::create_dir(&path).unwrap();
	path
}

#[test]
fn load_crate_parses_entry_file() {
	let dir = temp_test_dir("root");
	let path = dir.join("main.wx");
	let child_path = dir.join("math.wx");
	fs::write(&path, "module math;").unwrap();
	fs::write(&child_path, "fn add() {} ").unwrap();

	let mut builder = CompilationGraphBuilder::new();
	let crate_id = builder
		.load_binary(path.to_str().unwrap().to_string(), &NativeFileSource)
		.unwrap();
	let graph = &builder.crates[crate_id.as_u32() as usize];

	assert_eq!(graph.modules.len(), 2);

	let root = &graph.modules[graph.root.as_u32() as usize];
	assert_eq!(root.children.len(), 1);

	let child = &graph.modules[root.children[0].as_u32() as usize];
	assert_eq!(child.parent, Some(graph.root));
	assert_eq!(
		child.name.and_then(|name| builder.interner.resolve(name)),
		Some("math")
	);

	fs::remove_file(&path).unwrap();
	fs::remove_file(&child_path).unwrap();
	fs::remove_dir(&dir).unwrap();
}

#[test]
fn load_crate_reports_missing_entry_file() {
	let dir = temp_test_dir("missing");
	let path = dir.join("missing.wx");
	let path_str = path.to_str().unwrap().to_string();

	let mut builder = CompilationGraphBuilder::new();
	assert_eq!(
		builder.load_binary(path_str, &NativeFileSource),
		Err(()),
		"expected missing entry file to be a fatal error, not a diagnostic \
		 — there's no crate to build without it"
	);

	fs::remove_dir(&dir).unwrap();
}

#[test]
fn load_crate_diagnoses_missing_child_module_without_aborting() {
	// Regression test: `module boo;` referencing a file that doesn't exist
	// yet used to abort loading the entire crate. It should instead produce
	// a diagnostic attached to the `module boo;` declaration and let the
	// rest of the crate — including other, unrelated top-level items in the
	// same file — still load normally.
	let dir = temp_test_dir("missing-child-module");
	let path = dir.join("main.wx");
	fs::write(&path, "module boo;\nfn works() -> i32 { 1 }").unwrap();

	let mut builder = CompilationGraphBuilder::new();
	let crate_id = builder
		.load_binary(path.to_str().unwrap().to_string(), &NativeFileSource)
		.expect(
			"a missing child module should be a diagnostic, not a fatal \
			 error — the entry file itself is still readable",
		);
	let graph = &builder.crates[crate_id.as_u32() as usize];

	let root = &graph.modules[graph.root.as_u32() as usize];
	assert_eq!(
		root.children.len(),
		0,
		"the unresolved `boo` module should be omitted, not present as a stub"
	);
	assert!(
		root.ast
			.items
			.iter()
			.any(|item| matches!(&item.inner.inner, ast::Item::Function { .. })),
		"the rest of main.wx should still parse normally"
	);
	assert!(
		graph.diagnostics.iter().any(|d| d.code.as_deref()
			== Some(DiagnosticCode::ModuleFileNotFound.code())),
		"expected a module-not-found diagnostic; got: {:?}",
		graph.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
	);

	fs::remove_file(&path).unwrap();
	fs::remove_dir(&dir).unwrap();
}

#[test]
fn load_crate_resolves_module_directory_file() {
	let dir = temp_test_dir("dir-module-root");
	let path = dir.join("main.wx");
	let child_dir = dir.join("math");
	let child_path = child_dir.join("mod.wx");
	fs::create_dir(&child_dir).unwrap();
	fs::write(&path, "module math;").unwrap();
	fs::write(&child_path, "fn add() {}").unwrap();

	let mut builder = CompilationGraphBuilder::new();
	let crate_id = builder
		.load_binary(path.to_str().unwrap().to_string(), &NativeFileSource)
		.unwrap();
	let graph = &builder.crates[crate_id.as_u32() as usize];

	let root = &graph.modules[graph.root.as_u32() as usize];
	let child = &graph.modules[root.children[0].as_u32() as usize];
	assert_eq!(
		child.file_path,
		child_path.to_str().unwrap().replace('\\', "/")
	);

	fs::remove_file(&path).unwrap();
	fs::remove_file(&child_path).unwrap();
	fs::remove_dir(&child_dir).unwrap();
	fs::remove_dir(&dir).unwrap();
}

#[test]
fn load_crate_rejects_ambiguous_module_paths() {
	let dir = temp_test_dir("ambiguous-module-root");
	let path = dir.join("main.wx");
	let sibling_path = dir.join("math.wx");
	let child_dir = dir.join("math");
	let directory_path = child_dir.join("mod.wx");
	fs::create_dir(&child_dir).unwrap();
	fs::write(&path, "module math;").unwrap();
	fs::write(&sibling_path, "fn from_file() {}").unwrap();
	fs::write(&directory_path, "fn from_dir() {}").unwrap();

	let mut builder = CompilationGraphBuilder::new();
	let crate_id = builder
		.load_binary(path.to_str().unwrap().to_string(), &NativeFileSource)
		.expect(
			"an ambiguous child module should be a diagnostic, not a fatal \
			 error — the crate itself still loads",
		);
	let graph = &builder.crates[crate_id.as_u32() as usize];

	let root = &graph.modules[graph.root.as_u32() as usize];
	assert_eq!(
		root.children.len(),
		0,
		"the ambiguous module should be omitted rather than arbitrarily picked"
	);
	assert!(
		graph
			.diagnostics
			.iter()
			.any(|d| d.code.as_deref() == Some(DiagnosticCode::AmbiguousModuleFile.code())),
		"expected an ambiguous-module diagnostic; got: {:?}",
		graph.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
	);

	fs::remove_file(&path).unwrap();
	fs::remove_file(&sibling_path).unwrap();
	fs::remove_file(&directory_path).unwrap();
	fs::remove_dir(&child_dir).unwrap();
	fs::remove_dir(&dir).unwrap();
}

#[test]
fn load_virtual_compilation_resolves_child_modules_from_workspace_files() {
	let mut builder = CompilationGraphBuilder::new();
	let stdlib_id = builder.load_stdlib();
	let root_id = builder
		.load_binary(
			"src/main.wx".to_string(),
			&VirtualFileSource::new(HashMap::from([
				("src/main.wx".to_string(), "module math;".to_string()),
				("src/math.wx".to_string(), "fn add() {}".to_string()),
			])),
		)
		.expect("failed to load crate");
	let graph = builder.build(root_id, stdlib_id);

	let entry_crate = &graph.crates[1];
	assert_eq!(entry_crate.modules.len(), 2);
	let root = &entry_crate.modules[entry_crate.root.as_u32() as usize];
	assert_eq!(root.file_path, "src/main.wx");
	assert_eq!(root.children.len(), 1);

	let child = &entry_crate.modules[root.children[0].as_u32() as usize];
	assert_eq!(child.file_path, "src/math.wx");
}
