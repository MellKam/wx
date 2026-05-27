use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files;
use string_interner::symbol::SymbolU32;

use crate::{STDLIB_FILENAME, STDLIB_SOURCE, ast};

pub trait FileSource {
    fn read_to_string(&self, path: &Path) -> Result<String, LoadError>;

    fn exists(&self, path: &Path) -> bool;
}

pub struct NativeFileSource;

impl FileSource for NativeFileSource {
    fn read_to_string(&self, path: &Path) -> Result<String, LoadError> {
        fs::read_to_string(path).map_err(|_| LoadError::ReadFailed {
            path: path.to_path_buf(),
        })
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }
}

pub struct VirtualFileSource {
    files: HashMap<PathBuf, String>,
}

impl VirtualFileSource {
    pub fn new(files: HashMap<PathBuf, String>) -> Self {
        Self { files }
    }

    pub fn insert(&mut self, path: PathBuf, source: String) -> Option<String> {
        self.files.insert(path, source)
    }
}

impl FileSource for VirtualFileSource {
    fn read_to_string(&self, path: &Path) -> Result<String, LoadError> {
        self.files
            .get(path)
            .cloned()
            .ok_or_else(|| LoadError::ReadFailed {
                path: path.to_path_buf(),
            })
    }

    fn exists(&self, path: &Path) -> bool {
        self.files.contains_key(path)
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct File {
    pub name: String,
    pub source: String,
    line_starts: Vec<usize>,
}

impl File {
    fn line_start(&self, line_index: usize) -> Result<usize, files::Error> {
        match line_index.cmp(&self.line_starts.len()) {
            core::cmp::Ordering::Less => Ok(*self
                .line_starts
                .get(line_index)
                .expect("failed despite previous check")),
            core::cmp::Ordering::Equal => Ok(self.source.len()),
            core::cmp::Ordering::Greater => Err(files::Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Copy, Clone, PartialEq, Eq, serde::Serialize)]
pub struct FileId(u32);

#[cfg_attr(test, derive(serde::Serialize))]
pub struct Files {
    files: Vec<File>,
}

impl Files {
    pub fn new() -> Files {
        Files { files: Vec::new() }
    }

    pub fn add(&mut self, name: String, source: String) -> Option<FileId> {
        let file_id = FileId(u32::try_from(self.files.len()).ok()?);
        let line_starts = files::line_starts(&source).collect();

        self.files.push(File {
            name,
            line_starts,
            source,
        });

        Some(file_id)
    }

    pub fn get(&self, file_id: FileId) -> Result<&File, files::Error> {
        self.files
            .get(file_id.0 as usize)
            .ok_or(files::Error::FileMissing)
    }

    pub fn update(&mut self, file_id: FileId, source: String) {
        if let Some(file) = self.files.get_mut(file_id.0 as usize) {
            file.line_starts = files::line_starts(&source).collect();
            file.source = source;
        }
    }
}

impl<'files> files::Files<'files> for Files {
    type FileId = FileId;
    type Name = &'files str;
    type Source = &'files str;

    fn name(&'files self, file_id: FileId) -> Result<Self::Name, files::Error> {
        Ok(self.get(file_id)?.name.as_ref())
    }

    fn source(&'files self, file_id: FileId) -> Result<Self::Source, files::Error> {
        Ok(&self.get(file_id)?.source)
    }

    fn line_index(&self, file_id: FileId, byte_index: usize) -> Result<usize, files::Error> {
        self.get(file_id)?
            .line_starts
            .binary_search(&byte_index)
            .or_else(|next_line| Ok(next_line - 1))
    }

    fn line_range(
        &self,
        file_id: FileId,
        line_index: usize,
    ) -> Result<core::ops::Range<usize>, files::Error> {
        let file = self.get(file_id)?;
        let line_start = file.line_start(line_index)?;
        let next_line_start = file.line_start(line_index + 1)?;

        Ok(line_start..next_line_start)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct ModuleId(u32);

impl ModuleId {
    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct CrateId(u32);

impl CrateId {
    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0
    }
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct SourceModule {
    pub crate_id: CrateId,
    pub id: ModuleId,
    pub parent: Option<ModuleId>,
    pub children: Vec<ModuleId>,
    pub name: Option<SymbolU32>,
    pub path: Box<[SymbolU32]>,
    pub file_id: FileId,
    pub file_path: PathBuf,
    pub ast: ast::AST,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct CrateGraph {
    pub id: CrateId,
    pub root: ModuleId,
    pub entry_path: PathBuf,
    pub modules: Vec<SourceModule>,
    #[cfg_attr(test, serde(skip))]
    pub id_generator: ast::DefIdGenerator,
    #[cfg_attr(test, serde(skip))]
    pub diagnostics: Vec<Diagnostic<FileId>>,
    #[cfg_attr(test, serde(skip))]
    pub path_to_module: HashMap<PathBuf, ModuleId>,
}

#[cfg_attr(test, derive(serde::Serialize))]
pub struct CompilationGraph {
    pub files: Files,
    pub crates: Vec<CrateGraph>,
    pub entry_crate: CrateId,
    pub stdlib_crate: Option<CrateId>,
    #[cfg_attr(test, serde(skip))]
    pub id_generator: ast::DefIdGenerator,
}

impl CompilationGraph {
    pub fn crate_graph(&self, crate_id: CrateId) -> &CrateGraph {
        &self.crates[crate_id.as_u32() as usize]
    }

    pub fn asts_in_build_order(&self) -> Vec<&ast::AST> {
        let mut asts = Vec::new();
        for crate_graph in &self.crates {
            asts.extend(crate_graph.modules.iter().map(|module| &module.ast));
        }
        asts
    }
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum LoadError {
    ReadFailed {
        path: PathBuf,
    },
    AmbiguousModule {
        file: PathBuf,
        directory_file: PathBuf,
    },
    TooManyFiles,
}

pub fn load_compilation(
    entry_path: &Path,
    interner: &mut ast::StringInterner,
) -> Result<CompilationGraph, LoadError> {
    load_compilation_with_source(entry_path, &NativeFileSource, interner)
}

pub fn load_compilation_with_source(
    entry_path: &Path,
    file_source: &impl FileSource,
    interner: &mut ast::StringInterner,
) -> Result<CompilationGraph, LoadError> {
    let mut files = Files::new();
    let mut id_generator = ast::DefIdGenerator::new();
    let stdlib_crate = load_crate_from_source(
        &NativeFileSource,
        &mut files,
        CrateId(0),
        PathBuf::from(STDLIB_FILENAME),
        STDLIB_SOURCE.to_string(),
        interner,
        &mut id_generator,
    )?;
    let entry_crate = load_crate_from_path(
        file_source,
        &mut files,
        CrateId(1),
        entry_path.to_path_buf(),
        interner,
        &mut id_generator,
    )?;

    Ok(CompilationGraph {
        files,
        crates: vec![stdlib_crate, entry_crate],
        entry_crate: CrateId(1),
        stdlib_crate: Some(CrateId(0)),
        id_generator,
    })
}

pub fn load_inline_compilation(
    file_name: String,
    source: String,
    interner: &mut ast::StringInterner,
) -> Result<CompilationGraph, LoadError> {
    load_inline_compilation_with_source(file_name, source, &NativeFileSource, interner)
}

pub fn load_inline_compilation_with_source(
    file_name: String,
    source: String,
    file_source: &impl FileSource,
    interner: &mut ast::StringInterner,
) -> Result<CompilationGraph, LoadError> {
    let mut files = Files::new();
    let mut id_generator = ast::DefIdGenerator::new();
    let stdlib_crate = load_crate_from_source(
        &NativeFileSource,
        &mut files,
        CrateId(0),
        PathBuf::from(STDLIB_FILENAME),
        STDLIB_SOURCE.to_string(),
        interner,
        &mut id_generator,
    )?;
    let entry_crate = load_crate_from_source(
        file_source,
        &mut files,
        CrateId(1),
        PathBuf::from(file_name),
        source,
        interner,
        &mut id_generator,
    )?;

    Ok(CompilationGraph {
        files,
        crates: vec![stdlib_crate, entry_crate],
        entry_crate: CrateId(1),
        stdlib_crate: Some(CrateId(0)),
        id_generator,
    })
}

pub fn load_single_file_compilation(
    file_name: String,
    source: String,
    interner: &mut ast::StringInterner,
) -> Result<CompilationGraph, LoadError> {
    load_single_file_compilation_with_source(file_name, source, &NativeFileSource, interner)
}

pub fn load_single_file_compilation_with_source(
    file_name: String,
    source: String,
    file_source: &impl FileSource,
    interner: &mut ast::StringInterner,
) -> Result<CompilationGraph, LoadError> {
    let mut files = Files::new();
    let mut id_generator = ast::DefIdGenerator::new();
    let entry_crate = load_crate_from_source(
        file_source,
        &mut files,
        CrateId(0),
        PathBuf::from(file_name),
        source,
        interner,
        &mut id_generator,
    )?;

    Ok(CompilationGraph {
        files,
        crates: vec![entry_crate],
        entry_crate: CrateId(0),
        stdlib_crate: None,
        id_generator,
    })
}

pub fn load_virtual_compilation(
    entry_path: PathBuf,
    workspace_files: HashMap<PathBuf, String>,
    interner: &mut ast::StringInterner,
) -> Result<CompilationGraph, LoadError> {
    let file_source = VirtualFileSource::new(workspace_files);
    load_compilation_with_source(&entry_path, &file_source, interner)
}

fn load_crate_from_path(
    file_source: &impl FileSource,
    files: &mut Files,
    crate_id: CrateId,
    entry_path: PathBuf,
    interner: &mut ast::StringInterner,
    id_generator: &mut ast::DefIdGenerator,
) -> Result<CrateGraph, LoadError> {
    let mut loader = Loader {
        crate_id,
        file_source,
        files,
        modules: Vec::new(),
        diagnostics: Vec::new(),
        path_to_module: HashMap::new(),
        interner,
        id_generator,
    };
    let root = loader.load_module(None, None, Vec::new(), entry_path.clone())?;

    Ok(CrateGraph {
        id: crate_id,
        root,
        entry_path,
        modules: loader.modules,
        id_generator: *loader.id_generator,
        diagnostics: loader.diagnostics,
        path_to_module: loader.path_to_module,
    })
}

fn load_crate_from_source(
    file_source: &impl FileSource,
    files: &mut Files,
    crate_id: CrateId,
    entry_path: PathBuf,
    source: String,
    interner: &mut ast::StringInterner,
    id_generator: &mut ast::DefIdGenerator,
) -> Result<CrateGraph, LoadError> {
    let mut loader = Loader {
        crate_id,
        file_source,
        files,
        modules: Vec::new(),
        diagnostics: Vec::new(),
        path_to_module: HashMap::new(),
        interner,
        id_generator,
    };
    let root =
        loader.load_module_with_source(None, None, Vec::new(), entry_path.clone(), source)?;

    Ok(CrateGraph {
        id: crate_id,
        root,
        entry_path,
        modules: loader.modules,
        id_generator: *loader.id_generator,
        diagnostics: loader.diagnostics,
        path_to_module: loader.path_to_module,
    })
}

struct Loader<'files, 'interner, 'source, S: ?Sized> {
    crate_id: CrateId,
    file_source: &'source S,
    files: &'files mut Files,
    modules: Vec<SourceModule>,
    diagnostics: Vec<Diagnostic<FileId>>,
    path_to_module: HashMap<PathBuf, ModuleId>,
    interner: &'interner mut ast::StringInterner,
    id_generator: &'interner mut ast::DefIdGenerator,
}

impl<'files, 'interner, 'source, S: FileSource + ?Sized> Loader<'files, 'interner, 'source, S> {
    fn load_module(
        &mut self,
        parent: Option<ModuleId>,
        name: Option<SymbolU32>,
        path: Vec<SymbolU32>,
        file_path: PathBuf,
    ) -> Result<ModuleId, LoadError> {
        let source = self.file_source.read_to_string(&file_path)?;

        self.load_module_with_source(parent, name, path, file_path, source)
    }

    fn load_module_with_source(
        &mut self,
        parent: Option<ModuleId>,
        name: Option<SymbolU32>,
        path: Vec<SymbolU32>,
        file_path: PathBuf,
        source: String,
    ) -> Result<ModuleId, LoadError> {
        if let Some(&module_id) = self.path_to_module.get(&file_path) {
            return Ok(module_id);
        }

        let file_id = self
            .files
            .add(file_path.display().to_string(), source)
            .ok_or(LoadError::TooManyFiles)?;
        let ast = ast::Parser::parse(
            file_id,
            &self.files.get(file_id).unwrap().source,
            self.interner,
            self.id_generator,
        );
        let child_names: Box<[SymbolU32]> = ast
            .items
            .iter()
            .filter_map(|item| match &item.inner.inner {
                ast::Item::ModuleDeclaration { name, .. } => Some(name.inner),
                _ => None,
            })
            .collect();

        self.diagnostics.extend(ast.diagnostics.iter().cloned());

        let module_id =
            ModuleId(u32::try_from(self.modules.len()).map_err(|_| LoadError::TooManyFiles)?);
        self.path_to_module.insert(file_path.clone(), module_id);
        self.modules.push(SourceModule {
            crate_id: self.crate_id,
            id: module_id,
            parent,
            children: Vec::new(),
            name,
            path: path.clone().into_boxed_slice(),
            file_id,
            file_path: file_path.clone(),
            ast,
        });

        let mut children = Vec::with_capacity(child_names.len());
        for child_name in child_names {
            let child_path =
                module_file_path(self.file_source, &file_path, child_name, self.interner)?;
            let mut child_module_path = path.clone();
            child_module_path.push(child_name);
            let child_id = self.load_module(
                Some(module_id),
                Some(child_name),
                child_module_path,
                child_path,
            )?;
            children.push(child_id);
        }
        self.modules[module_id.0 as usize].children = children;

        Ok(module_id)
    }
}

fn module_file_path<S: FileSource + ?Sized>(
    file_source: &S,
    parent_file_path: &Path,
    module_name: SymbolU32,
    interner: &ast::StringInterner,
) -> Result<PathBuf, LoadError> {
    let module_name = interner
        .resolve(module_name)
        .expect("module symbol should resolve while loading crate");
    let parent_dir = parent_file_path.parent().unwrap_or_else(|| Path::new(""));
    let sibling_file = parent_dir.join(format!("{module_name}.wx"));
    let directory_file = parent_dir.join(module_name).join("mod.wx");

    if file_source.exists(&sibling_file) && file_source.exists(&directory_file) {
        return Err(LoadError::AmbiguousModule {
            file: sibling_file,
            directory_file,
        });
    }

    if file_source.exists(&sibling_file) {
        return Ok(sibling_file);
    }

    Ok(directory_file)
}

#[cfg(test)]
mod tests {
    use std::fs;
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

        let mut interner = ast::StringInterner::new();
        let mut files = Files::new();
        let mut id_generator = ast::DefIdGenerator::new();
        let graph = load_crate_from_path(
            &NativeFileSource,
            &mut files,
            CrateId(0),
            path.clone(),
            &mut interner,
            &mut id_generator,
        )
        .unwrap();

        assert_eq!(graph.modules.len(), 2);

        let root = &graph.modules[graph.root.as_u32() as usize];
        assert_eq!(root.children.len(), 1);

        let child = &graph.modules[root.children[0].as_u32() as usize];
        assert_eq!(child.parent, Some(graph.root));
        assert_eq!(
            child.name.and_then(|name| interner.resolve(name)),
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
        let mut interner = ast::StringInterner::new();
        let mut files = Files::new();
        let mut id_generator = ast::DefIdGenerator::new();

        match load_crate_from_path(
            &NativeFileSource,
            &mut files,
            CrateId(0),
            path.clone(),
            &mut interner,
            &mut id_generator,
        ) {
            Err(LoadError::ReadFailed { path: failed_path }) => assert_eq!(failed_path, path),
            Err(other) => panic!("unexpected error: {other:?}"),
            Ok(_) => panic!("expected missing file error"),
        }

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

        let mut interner = ast::StringInterner::new();
        let mut files = Files::new();
        let mut id_generator = ast::DefIdGenerator::new();
        let graph = load_crate_from_path(
            &NativeFileSource,
            &mut files,
            CrateId(0),
            path.clone(),
            &mut interner,
            &mut id_generator,
        )
        .unwrap();

        let root = &graph.modules[graph.root.as_u32() as usize];
        let child = &graph.modules[root.children[0].as_u32() as usize];
        assert_eq!(child.file_path, child_path);

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

        let mut interner = ast::StringInterner::new();
        let mut files = Files::new();
        let mut id_generator = ast::DefIdGenerator::new();
        match load_crate_from_path(
            &NativeFileSource,
            &mut files,
            CrateId(0),
            path.clone(),
            &mut interner,
            &mut id_generator,
        ) {
            Err(LoadError::AmbiguousModule {
                file,
                directory_file,
            }) => {
                assert_eq!(file, sibling_path);
                assert_eq!(directory_file, directory_path);
            }
            Err(other) => panic!("unexpected error: {other:?}"),
            Ok(_) => panic!("expected ambiguous module error"),
        }

        fs::remove_file(&path).unwrap();
        fs::remove_file(&sibling_path).unwrap();
        fs::remove_file(&directory_path).unwrap();
        fs::remove_dir(&child_dir).unwrap();
        fs::remove_dir(&dir).unwrap();
    }

    #[test]
    fn load_virtual_compilation_resolves_child_modules_from_workspace_files() {
        let entry_path = PathBuf::from("src/main.wx");
        let child_path = PathBuf::from("src/math.wx");
        let mut workspace_files = HashMap::new();
        workspace_files.insert(entry_path.clone(), "module math;".to_string());
        workspace_files.insert(child_path.clone(), "fn add() {}".to_string());

        let mut interner = ast::StringInterner::new();
        let graph =
            load_virtual_compilation(entry_path.clone(), workspace_files, &mut interner).unwrap();

        let entry_crate = graph.crate_graph(graph.entry_crate);
        assert_eq!(entry_crate.modules.len(), 2);
        let root = &entry_crate.modules[entry_crate.root.as_u32() as usize];
        assert_eq!(root.file_path, entry_path);
        assert_eq!(root.children.len(), 1);

        let child = &entry_crate.modules[root.children[0].as_u32() as usize];
        assert_eq!(child.file_path, child_path);
    }
}
