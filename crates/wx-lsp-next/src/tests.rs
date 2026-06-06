use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Range};

use crate::{
    OpenDocument, ServerState, analyze_root, compute_refresh, diagnostic_publish_paths,
    discover_crate_root,
};

fn open_document(text: &str) -> OpenDocument {
    OpenDocument {
        text: text.to_string(),
    }
}

#[test]
fn discover_crate_root_walks_up_to_main_wx() {
    let workspace_root = PathBuf::from("/workspace");
    let crate_root = workspace_root.join("app").join("main.wx");
    let child_file = workspace_root.join("app").join("math").join("add.wx");

    let mut open_documents = HashMap::new();
    open_documents.insert(crate_root.clone(), open_document("module math;"));

    let discovered = discover_crate_root(&open_documents, &[workspace_root], &child_file);

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

    let publish_paths = diagnostic_publish_paths(&previous, &owned_files, &diagnostics_by_file);

    assert!(publish_paths.contains(&main));
    assert!(publish_paths.contains(&child));
    assert_eq!(publish_paths.len(), 2);
}

#[test]
fn analyze_root_updates_multi_file_diagnostics_when_overlay_changes() {
    let root = PathBuf::from("/workspace/app/main.wx");
    let child = PathBuf::from("/workspace/app/math.wx");

    let mut state = ServerState::default();
    state.open_documents.insert(
        root.clone(),
        open_document("module math;\n\nfn compute() -> i32 {\n    math::add()\n}\n"),
    );
    state.open_documents.insert(
        child.clone(),
        open_document("fn add() -> bool {\n    true\n}\n"),
    );

    let broken = analyze_root(&mut state, &root);
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

    let fixed = analyze_root(&mut state, &root);
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
fn refresh_file_from_child_path_discovers_root_and_republishes_root_diagnostics() {
    let workspace_root = PathBuf::from("/workspace");
    let root = workspace_root.join("app").join("main.wx");
    let child = workspace_root.join("app").join("math.wx");

    let mut state = ServerState {
        workspace_folders: vec![workspace_root],
        ..Default::default()
    };
    state.open_documents.insert(
        root.clone(),
        open_document("module math;\n\nfn compute() -> i32 {\n    math::add()\n}\n"),
    );
    state.open_documents.insert(
        child.clone(),
        open_document("fn add() -> bool {\n    true\n}\n"),
    );

    let broken_publish = compute_refresh(&mut state, &child);

    assert_eq!(state.file_to_root.get(&child), Some(&root));
    assert_eq!(state.file_to_root.get(&root), Some(&root));

    assert!(
        broken_publish.iter().any(|(path, diags)| {
            path == &root && diags.iter().any(|d| d.severity == Some(DiagnosticSeverity::ERROR))
        }),
        "expected refresh from child path to publish a root-file error"
    );

    state.open_documents.insert(
        child.clone(),
        open_document("fn add() -> i32 {\n    1\n}\n"),
    );

    let fixed_publish = compute_refresh(&mut state, &child);

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
