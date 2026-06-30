use std::collections::HashMap;
use std::fs;
use std::path::Path;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files;
use string_interner::symbol::SymbolU32;

use crate::ast;

#[cfg(test)]
mod tests;

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

	fn source(
		&'files self,
		file_id: FileId,
	) -> Result<Self::Source, files::Error> {
		Ok(&self.get(file_id)?.source)
	}

	fn line_index(
		&self,
		file_id: FileId,
		byte_index: usize,
	) -> Result<usize, files::Error> {
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
	/// Intra-crate path segment declared by `module foo;`. `None` for the crate
	/// root — root items need no qualifier (`add`, not `root::add`).
	pub name: Option<SymbolU32>,
	pub file_id: FileId,
	pub file_path: String,
	pub ast: ast::AST,
}

pub struct CrateGraph {
	pub id: CrateId,
	pub root: ModuleId,
	/// Inter-crate namespace identifier (`std` in `std::Memory`). `None` for
	/// binary/root crates that produce a WASM module and cannot be imported.
	pub name: Option<SymbolU32>,
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

pub const STDLIB_SOURCE: &'static str = include_str!("../../std/lib.wx");

impl CompilationGraphBuilder {
	pub fn new() -> Self {
		Self {
			files: Files::new(),
			id_generator: ast::DefIdGenerator::new(),
			interner: ast::StringInterner::new(),
			crates: Vec::new(),
		}
	}

	pub fn load_stdlib(&mut self) -> Result<CrateId, LoadError> {
		self.load_library(
			"std",
			"lib.wx".to_string(),
			&VirtualFileSource::new(HashMap::from([(
				"lib.wx".to_string(),
				STDLIB_SOURCE.to_string(),
			)])),
		)
	}

	#[inline]
	pub fn load_library(
		&mut self,
		name: &str,
		entry_path: String,
		file_source: &impl FileSource,
	) -> Result<CrateId, LoadError> {
		let name = self.interner.get_or_intern(name);
		self.load_crate(Some(name), entry_path, file_source)
	}

	#[inline]
	pub fn load_binary(
		&mut self,
		entry_path: String,
		file_source: &impl FileSource,
	) -> Result<CrateId, LoadError> {
		self.load_crate(None, entry_path, file_source)
	}

	pub fn load_crate(
		&mut self,
		name: Option<SymbolU32>,
		entry_path: String,
		file_source: &impl FileSource,
	) -> Result<CrateId, LoadError> {
		let crate_id = CrateId(self.crates.len() as u32);
		let mut loader = Loader::new(self, crate_id, file_source);
		let root = loader.load_module(entry_path.clone(), None, None)?;
		let crate_graph = CrateGraph {
			id: crate_id,
			name,
			root,
			entry_path,
			modules: loader.modules,
			diagnostics: loader.diagnostics,
			path_to_module: loader.path_to_module,
		};
		self.crates.push(crate_graph);
		Ok(crate_id)
	}

	pub fn build(
		self,
		root_crate: CrateId,
		stdlib_crate: CrateId,
	) -> CompilationGraph {
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

struct Loader<'ctx, 'src, S: FileSource> {
	crate_id: CrateId,
	file_source: &'src S,
	modules: Vec<SourceModule>,
	diagnostics: Vec<Diagnostic<FileId>>,
	path_to_module: HashMap<String, ModuleId>,
	ctx: &'ctx mut CompilationGraphBuilder,
}

impl<'ctx, 'src, Source: FileSource> Loader<'ctx, 'src, Source> {
	#[inline]
	fn new(
		ctx: &'ctx mut CompilationGraphBuilder,
		crate_id: CrateId,
		file_source: &'src Source,
	) -> Self {
		Self {
			crate_id,
			ctx,
			file_source,
			diagnostics: Vec::new(),
			modules: Vec::new(),
			path_to_module: HashMap::new(),
		}
	}

	fn load_module(
		&mut self,
		file_path: String,
		name: Option<SymbolU32>,
		parent: Option<ModuleId>,
	) -> Result<ModuleId, LoadError> {
		let mut file_path = file_path;
		// We normalize all path separators to '/' to ensure consistent VFS lookup
		// across all platforms (matching virtual file schemas and test assertions).
		// SAFETY: Both '\' and '/' are ASCII bytes, occupying exactly one byte and never affecting multibyte sequences.
		// Thus, this in-place substitution preserves UTF-8 validity.
		unsafe {
			for byte in file_path.as_bytes_mut() {
				if *byte == b'\\' {
					*byte = b'/';
				}
			}
		}
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

		let module_id = ModuleId(
			u32::try_from(self.modules.len())
				.map_err(|_| LoadError::TooManyFiles)?,
		);
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
			let child_path =
				self.get_child_module_path(module_id, child_name)?;
			let child_id = self.load_module(
				child_path,
				Some(child_name),
				Some(module_id),
			)?;
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
		let parent_file_path =
			&self.modules[parent_module_id.0 as usize].file_path;
		let parent_dir = parent_file_path
			.rfind('/')
			.map(|i| &parent_file_path[..=i])
			.unwrap_or("");
		let sibling_file = format!("{parent_dir}{module_name}.wx");
		let directory_file = format!("{parent_dir}{module_name}/mod.wx");

		if self.file_source.exists(&sibling_file)
			&& self.file_source.exists(&directory_file)
		{
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
