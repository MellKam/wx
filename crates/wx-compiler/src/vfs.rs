use std::collections::HashMap;
use std::fs;
use std::path::Path;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files;
use string_interner::symbol::SymbolU32;

use crate::ast;

pub trait FileSource {
    fn read_to_string(&self, path: &str) -> Result<String, LoadError>;
    fn exists(&self, path: &str) -> bool;
}

pub struct NativeFileSource;

impl FileSource for NativeFileSource {
    fn read_to_string(&self, path: &str) -> Result<String, LoadError> {
        fs::read_to_string(Path::new(path)).map_err(|_| LoadError::ReadFailed {
            path: path.to_string(),
        })
    }

    fn exists(&self, path: &str) -> bool {
        Path::new(path).exists()
    }
}

pub struct VirtualFileSource {
    files: HashMap<String, String>,
}

impl VirtualFileSource {
    pub fn new(files: HashMap<String, String>) -> Self {
        Self { files }
    }

    pub fn insert(&mut self, path: String, source: String) -> Option<String> {
        self.files.insert(path, source)
    }
}

impl FileSource for VirtualFileSource {
    fn read_to_string(&self, path: &str) -> Result<String, LoadError> {
        self.files
            .get(path)
            .cloned()
            .ok_or_else(|| LoadError::ReadFailed {
                path: path.to_string(),
            })
    }

    fn exists(&self, path: &str) -> bool {
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

    #[inline]
    pub fn as_usize(self) -> usize {
        self.0 as usize
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

    #[inline]
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

pub struct SourceModule {
    pub crate_id: CrateId,
    pub id: ModuleId,
    pub parent: Option<ModuleId>,
    pub children: Vec<ModuleId>,
    pub name: Option<SymbolU32>,
    pub file_id: FileId,
    pub file_path: String,
    pub ast: ast::AST,
}

pub struct CrateGraph {
    pub id: CrateId,
    pub root: ModuleId,
    pub entry_path: String,
    pub modules: Vec<SourceModule>,
    pub diagnostics: Vec<Diagnostic<FileId>>,
    pub path_to_module: HashMap<String, ModuleId>,
}

pub struct CompilationGraph {
    pub files: Files,
    pub crates: Vec<CrateGraph>,
    pub stdlib_crate: CrateId,
    pub root_crate: CrateId,
    pub id_generator: ast::DefIdGenerator,
    pub interner: ast::StringInterner,
}

pub struct CompilationGraphBuilder {
    pub files: Files,
    pub id_generator: ast::DefIdGenerator,
    pub interner: ast::StringInterner,
    pub crates: Vec<CrateGraph>,
}

impl CompilationGraphBuilder {
    pub fn new() -> Self {
        Self {
            files: Files::new(),
            id_generator: ast::DefIdGenerator::new(),
            interner: ast::StringInterner::new(),
            crates: Vec::new(),
        }
    }

    pub fn load_crate(
        &mut self,
        entry_path: String,
        file_source: &impl FileSource,
    ) -> Result<CrateId, LoadError> {
        let crate_id = CrateId(self.crates.len() as u32);
        let crage_graph = CrateGraph::load(self, crate_id, entry_path, file_source)?;
        self.crates.push(crage_graph);
        Ok(crate_id)
    }

    pub fn build(self, root_crate: CrateId, stdlib_crate: CrateId) -> CompilationGraph {
        CompilationGraph {
            files: self.files,
            crates: self.crates,
            stdlib_crate,
            root_crate,
            id_generator: self.id_generator,
            interner: self.interner,
        }
    }
}

impl CrateGraph {
    pub fn module_symbol_path(&self, module_id: ModuleId) -> Box<[SymbolU32]> {
        let mut path = Vec::new();
        let mut current = Some(module_id);

        while let Some(id) = current {
            let module = &self.modules[id.as_u32() as usize];
            if let Some(name) = module.name {
                path.push(name);
            }
            current = module.parent;
        }

        path.reverse();
        path.into_boxed_slice()
    }

    pub fn load(
        ctx: &mut CompilationGraphBuilder,
        crate_id: CrateId,
        entry_path: String,
        file_source: &impl FileSource,
    ) -> Result<Self, LoadError> {
        let mut loader = Loader {
            crate_id,
            file_source,
            modules: Vec::new(),
            diagnostics: Vec::new(),
            path_to_module: HashMap::new(),
            ctx,
        };
        let root = loader.load_module(entry_path.clone(), None, None)?;

        Ok(CrateGraph {
            id: crate_id,
            root,
            entry_path,
            modules: loader.modules,
            diagnostics: loader.diagnostics,
            path_to_module: loader.path_to_module,
        })
    }
}

#[derive(Clone, PartialEq)]
#[derive(Debug)]
pub enum LoadError {
    ReadFailed {
        path: String,
    },
    AmbiguousModule {
        file: String,
        directory_file: String,
    },
    TooManyFiles,
}

struct Loader<'ctx, 'src, S: FileSource + ?Sized> {
    crate_id: CrateId,
    file_source: &'src S,
    modules: Vec<SourceModule>,
    diagnostics: Vec<Diagnostic<FileId>>,
    path_to_module: HashMap<String, ModuleId>,
    ctx: &'ctx mut CompilationGraphBuilder,
}

impl<'ctx, 'src, S: FileSource + ?Sized> Loader<'ctx, 'src, S> {
    fn load_module(
        &mut self,
        file_path: String,
        parent: Option<ModuleId>,
        name: Option<SymbolU32>,
    ) -> Result<ModuleId, LoadError> {
        if let Some(&module_id) = self.path_to_module.get(&file_path) {
            return Ok(module_id);
        }

        let source = self.file_source.read_to_string(&file_path)?;
        let file_id = self
            .ctx
            .files
            .add(file_path.clone(), source)
            .ok_or(LoadError::TooManyFiles)?;
        let ast = ast::Parser::parse(
            file_id,
            &self.ctx.files,
            &mut self.ctx.interner,
            &mut self.ctx.id_generator,
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
            file_id,
            file_path,
            ast,
        });

        let mut children = Vec::with_capacity(child_names.len());
        for child_name in child_names {
            let child_path = self.get_child_module_path(module_id, child_name)?;
            let child_id = self.load_module(child_path, Some(module_id), Some(child_name))?;
            children.push(child_id);
        }
        self.modules[module_id.0 as usize].children = children;

        Ok(module_id)
    }

    fn get_child_module_path(
        &mut self,
        parent_module_id: ModuleId,
        child_module_name: SymbolU32,
    ) -> Result<String, LoadError> {
        let module_name = self
            .ctx
            .interner
            .resolve(child_module_name)
            .expect("module symbol should resolve while loading crate");
        let parent_file_path = &self.modules[parent_module_id.0 as usize].file_path;
        let parent_dir = parent_file_path
            .rfind('/')
            .map(|i| &parent_file_path[..=i])
            .unwrap_or("");
        let sibling_file = format!("{parent_dir}{module_name}.wx");
        let directory_file = format!("{parent_dir}{module_name}/mod.wx");

        if self.file_source.exists(&sibling_file) && self.file_source.exists(&directory_file) {
            return Err(LoadError::AmbiguousModule {
                file: sibling_file,
                directory_file,
            });
        }

        if self.file_source.exists(&sibling_file) {
            return Ok(sibling_file);
        }

        Ok(directory_file)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;
    use crate::STDLIB_SOURCE;

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
            .load_crate(path.to_str().unwrap().to_string(), &NativeFileSource)
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
        match builder.load_crate(path_str.clone(), &NativeFileSource) {
            Err(LoadError::ReadFailed { path: failed_path }) => assert_eq!(failed_path, path_str),
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

        let mut builder = CompilationGraphBuilder::new();
        let crate_id = builder
            .load_crate(path.to_str().unwrap().to_string(), &NativeFileSource)
            .unwrap();
        let graph = &builder.crates[crate_id.as_u32() as usize];

        let root = &graph.modules[graph.root.as_u32() as usize];
        let child = &graph.modules[root.children[0].as_u32() as usize];
        assert_eq!(child.file_path, child_path.to_str().unwrap());

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
        match builder.load_crate(path.to_str().unwrap().to_string(), &NativeFileSource) {
            Err(LoadError::AmbiguousModule {
                file,
                directory_file,
            }) => {
                assert_eq!(file, sibling_path.to_str().unwrap());
                assert_eq!(directory_file, directory_path.to_str().unwrap());
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
        let mut builder = CompilationGraphBuilder::new();
        let stdlib_id = builder
            .load_crate(
                "std.wx".to_string(),
                &VirtualFileSource::new(HashMap::from([(
                    "std.wx".to_string(),
                    STDLIB_SOURCE.to_string(),
                )])),
            )
            .expect("failed to load stdlib crate");
        let root_id = builder
            .load_crate(
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
}
