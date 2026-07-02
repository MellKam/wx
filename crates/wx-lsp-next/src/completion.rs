use std::collections::HashSet;

use tower_lsp_server::ls_types::{
	CompletionItem, CompletionItemKind, InsertTextFormat,
};
use wx_compiler::ast::StringInterner;
use wx_compiler::tir::{NamespaceIndex, TIR, TypeFormatter};
use wx_compiler::vfs::FileId;

use crate::symbol_index::{SymbolIndex, SymbolKind};

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(PartialEq, Eq)]
pub enum CompletionContext {
	/// Cursor is in expression/statement position. Prefix may be empty (blank trigger).
	Identifier,
	/// Cursor is after `.` — show fields and methods of the receiver type.
	/// `receiver_end` is the exclusive-end byte offset of the receiver text in source.
	DotAccess { receiver_end: usize },
	/// Cursor is after `::` — show module members, enum variants, or associated items.
	/// `lhs_end` is the exclusive-end byte offset of the LHS name in source.
	PathAccess { lhs_end: usize },
	/// Cursor is in a type annotation position (after `:` that is not `::`).
	TypeAnnotation,
}

/// Classifies the completion trigger context at `offset` bytes into `source`.
pub fn classify_context(source: &str, offset: usize) -> CompletionContext {
	// Strip the identifier prefix currently being typed.
	let prefix_start = source[..offset]
		.bytes()
		.rposition(|b| !b.is_ascii_alphanumeric() && b != b'_')
		.map_or(0, |i| i + 1);

	// Everything before the prefix, with trailing ASCII whitespace removed.
	let before = source[..prefix_start]
		.trim_end_matches(|c: char| c.is_ascii_whitespace());

	if before.is_empty() {
		return CompletionContext::Identifier;
	}

	let before_bytes = before.as_bytes();

	match before_bytes[before_bytes.len() - 1] {
		b'.' => {
			let before_dot = before[..before.len() - 1]
				.trim_end_matches(|c: char| c.is_ascii_whitespace());
			CompletionContext::DotAccess {
				receiver_end: before_dot.len(),
			}
		}
		b':' if before_bytes.len() >= 2
			&& before_bytes[before_bytes.len() - 2] == b':' =>
		{
			let before_colons = before[..before.len() - 2]
				.trim_end_matches(|c: char| c.is_ascii_whitespace());
			CompletionContext::PathAccess {
				lhs_end: before_colons.len(),
			}
		}
		b':' => CompletionContext::TypeAnnotation,
		_ => CompletionContext::Identifier,
	}
}

pub fn find_enclosing_function(
	tir: &TIR,
	file_id: FileId,
	cursor_offset: u32,
) -> Option<usize> {
	tir.functions.iter().position(|f| {
		f.file_id == file_id
			&& f.body.as_ref().is_some_and(|body| {
				let span = body.stack.scopes[0].span;
				span.start <= cursor_offset && span.end > cursor_offset
			})
	})
}

/// Collects completion items for all locals visible at `cursor_offset` inside the given function.
/// Walks the innermost scope containing the cursor, then follows the parent chain upward.
pub fn local_completion_items(
	tir: &TIR,
	func_index: usize,
	interner: &StringInterner,
	cursor_offset: u32,
	prefix: &str,
) -> Vec<CompletionItem> {
	let formatter = TypeFormatter::new(tir, interner);
	let function = &tir.functions[func_index];
	let body = match &function.body {
		Some(b) => b,
		None => return vec![],
	};
	let num_params = function.params.len();

	let innermost_idx = body
		.stack
		.scopes
		.iter()
		.enumerate()
		.filter(|(_, s)| {
			s.span.start <= cursor_offset && s.span.end > cursor_offset
		})
		.min_by_key(|(_, s)| s.span.end - s.span.start)
		.map(|(i, _)| i as u32);

	let innermost_idx = match innermost_idx {
		Some(i) => i,
		None => return vec![],
	};

	let mut items = vec![];
	let mut current = Some(innermost_idx);

	while let Some(scope_idx) = current {
		let scope = &body.stack.scopes[scope_idx as usize];
		let is_innermost = scope_idx == innermost_idx;
		let is_root = scope_idx == 0;

		for (local_idx, local) in scope.locals.iter().enumerate() {
			let is_param = is_root && local_idx < num_params;
			// In the innermost scope, skip locals not yet declared (except params which
			// are always in scope). Ancestor-scope locals are always visible.
			if !is_param
				&& is_innermost
				&& local.name.span.start >= cursor_offset
			{
				continue;
			}
			let name = match interner.resolve(local.name.inner) {
				Some(n) => n,
				None => continue,
			};
			if !name.starts_with(prefix) {
				continue;
			}
			let detail = formatter.display_type(local.ty).ok();
			// `0_` sorts before `global_completion_items`' `1_` prefix, so
			// locals are listed first regardless of alphabetical label order.
			let sort_text = Some(format!("0_{name}"));
			items.push(CompletionItem {
				label: name.to_string(),
				kind: Some(CompletionItemKind::VARIABLE),
				detail,
				sort_text,
				..Default::default()
			});
		}

		current = scope.parent;
	}

	items
}

/// Namespace of the module containing `file_id`, for completion requests
/// that fall outside any function body (no enclosing-function namespace to
/// fall back on). Only inline `module foo { }` blocks are missed here —
/// those share the file of their enclosing function, which already carries
/// the right namespace.
fn file_namespace(tir: &TIR, file_id: FileId) -> Option<NamespaceIndex> {
	tir.module_decls
		.iter()
		.find(|decl| decl.own_file_id == Some(file_id))
		.map(|decl| decl.namespace_idx)
}

/// The set of namespaces whose direct symbols are visible from `start`:
/// `start` itself, its `use path::*` wildcard imports, then each ancestor
/// namespace (and its wildcard imports) up to the implicit root. Mirrors the
/// walk `lookup_global_symbol` performs in the type checker, but collects
/// every reachable namespace instead of stopping at the first name match.
pub fn visible_namespaces(
	tir: &TIR,
	start: Option<NamespaceIndex>,
) -> HashSet<Option<NamespaceIndex>> {
	let mut visible = HashSet::new();
	let mut current = start;
	loop {
		if !visible.insert(current) {
			break;
		}
		match current {
			Some(idx) => {
				let ns = &tir.namespaces[idx as usize];
				visible.extend(ns.wildcard_imports.iter().map(|&i| Some(i)));
				current = ns.parent;
			}
			None => {
				visible.extend(
					tir.root_wildcard_imports.iter().map(|&i| Some(i)),
				);
				break;
			}
		}
	}
	visible
}

/// Prefix-searches `symbol_index.global_definitions` and maps each match visible
/// from `visible_from` to a `CompletionItem`.
pub fn global_completion_items(
	tir: &TIR,
	interner: &StringInterner,
	symbol_index: &SymbolIndex,
	prefix: &str,
	visible_from: &HashSet<Option<NamespaceIndex>>,
) -> Vec<CompletionItem> {
	let lower = symbol_index.global_definitions.partition_point(|def| {
		interner.resolve(def.name).unwrap_or("") < prefix
	});

	symbol_index.global_definitions[lower..]
		.iter()
		.take_while(|def| {
			interner.resolve(def.name).unwrap_or("").starts_with(prefix)
		})
		.filter(|def| visible_from.contains(&def.namespace))
		.filter_map(|def| {
			let name = interner.resolve(def.name)?.to_string();
			let item = match &def.info.kind {
				SymbolKind::Function(def_id) => {
					let fi = tir.function_index(*def_id)? as usize;
					let func = &tir.functions[fi];
					let (insert_text, insert_text_format) = if func
						.params
						.is_empty()
					{
						(format!("{}()", name), InsertTextFormat::PLAIN_TEXT)
					} else {
						(format!("{}($1)", name), InsertTextFormat::SNIPPET)
					};
					CompletionItem {
						label: name,
						kind: Some(CompletionItemKind::FUNCTION),
						insert_text: Some(insert_text),
						insert_text_format: Some(insert_text_format),
						..Default::default()
					}
				}
				SymbolKind::Global(_) => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::VARIABLE),
					..Default::default()
				},
				SymbolKind::Const(_) => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::CONSTANT),
					..Default::default()
				},
				SymbolKind::Struct(_) => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::STRUCT),
					..Default::default()
				},
				SymbolKind::Enum(_) => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::ENUM),
					..Default::default()
				},
				SymbolKind::EnumVariant { .. } => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::ENUM_MEMBER),
					..Default::default()
				},
				SymbolKind::Namespace(_) => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::MODULE),
					..Default::default()
				},
				SymbolKind::Trait(_) => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::INTERFACE),
					..Default::default()
				},
				SymbolKind::TypeSet(_) => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::TYPE_PARAMETER),
					..Default::default()
				},
				SymbolKind::AssocType { .. } => CompletionItem {
					label: name,
					kind: Some(CompletionItemKind::TYPE_PARAMETER),
					..Default::default()
				},
				_ => return None,
			};
			// Sorts after `local_completion_items`' `0_` prefix, so locals
			// in scope are listed before same-prefix globals.
			Some(CompletionItem {
				sort_text: Some(format!("1_{}", item.label)),
				..item
			})
		})
		.collect()
}

pub fn completion_items(
	tir: &TIR,
	interner: &StringInterner,
	symbol_index: &SymbolIndex,
	file_id: FileId,
	source: &str,
	offset: usize,
) -> Vec<CompletionItem> {
	let prefix_start = source[..offset]
		.bytes()
		.rposition(|b| !b.is_ascii_alphanumeric() && b != b'_')
		.map_or(0, |i| i + 1);
	let prefix = &source[prefix_start..offset];
	let cursor_offset = offset as u32;
	let func_index = find_enclosing_function(tir, file_id, cursor_offset);
	let current_namespace = match func_index {
		Some(fi) => tir.functions[fi].namespace,
		None => file_namespace(tir, file_id),
	};
	let visible = visible_namespaces(tir, current_namespace);

	match classify_context(source, offset) {
		CompletionContext::Identifier => {
			let mut items = match func_index {
				Some(func_index) => local_completion_items(
					tir,
					func_index,
					interner,
					cursor_offset,
					prefix,
				),
				None => Vec::new(),
			};

			items.extend(global_completion_items(
				tir,
				interner,
				symbol_index,
				prefix,
				&visible,
			));
			items
		}
		CompletionContext::DotAccess { .. } => {
			// TODO: resolve receiver type → impl_members + struct fields
			vec![]
		}
		CompletionContext::PathAccess { .. } => {
			// TODO: resolve LHS → module members / enum variants / assoc fns
			vec![]
		}
		CompletionContext::TypeAnnotation => {
			// TODO: restrict to types only; for now show globals (includes structs/enums)
			global_completion_items(tir, interner, symbol_index, prefix, &visible)
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn empty_source_is_identifier() {
		assert_eq!(classify_context("", 0), CompletionContext::Identifier);
	}

	#[test]
	fn bare_identifier_at_end_is_identifier() {
		assert_eq!(classify_context("foo", 3), CompletionContext::Identifier);
	}

	#[test]
	fn blank_inside_block_is_identifier() {
		let src = "fn add(a: i32, b: i32) -> i32 {\n    ";
		assert_eq!(
			classify_context(src, src.len()),
			CompletionContext::Identifier
		);
	}

	#[test]
	fn partial_identifier_after_operator_is_identifier() {
		assert_eq!(
			classify_context("let x = fo", 10),
			CompletionContext::Identifier
		);
	}

	#[test]
	fn after_open_brace_is_identifier() {
		assert_eq!(classify_context("{ ", 2), CompletionContext::Identifier);
	}

	#[test]
	fn dot_no_prefix_is_dot_access() {
		// "a." — receiver is "a" with exclusive end 1
		assert_eq!(
			classify_context("a.", 2),
			CompletionContext::DotAccess { receiver_end: 1 }
		);
	}

	#[test]
	fn dot_with_partial_field_prefix_is_dot_access() {
		// "foo.bar" at offset 7 — prefix "bar", receiver "foo"
		assert_eq!(
			classify_context("foo.bar", 7),
			CompletionContext::DotAccess { receiver_end: 3 }
		);
	}

	#[test]
	fn dot_with_spaces_is_dot_access() {
		// "foo . " — spaces around dot, receiver_end still points past "foo"
		assert_eq!(
			classify_context("foo . ", 6),
			CompletionContext::DotAccess { receiver_end: 3 }
		);
	}

	#[test]
	fn double_colon_no_prefix_is_path_access() {
		// "Foo::" — lhs exclusive end is 3
		assert_eq!(
			classify_context("Foo::", 5),
			CompletionContext::PathAccess { lhs_end: 3 }
		);
	}

	#[test]
	fn double_colon_with_partial_name_is_path_access() {
		// "math::add" — prefix "add", lhs "math"
		assert_eq!(
			classify_context("math::add", 9),
			CompletionContext::PathAccess { lhs_end: 4 }
		);
	}

	#[test]
	fn single_colon_is_type_annotation() {
		assert_eq!(
			classify_context("x:", 2),
			CompletionContext::TypeAnnotation
		);
	}

	#[test]
	fn colon_after_name_with_space_is_type_annotation() {
		assert_eq!(
			classify_context("local x: ", 9),
			CompletionContext::TypeAnnotation
		);
	}

	#[test]
	fn colon_with_partial_type_is_type_annotation() {
		// "fn f(a: i" — prefix "i"
		assert_eq!(
			classify_context("fn f(a: i", 9),
			CompletionContext::TypeAnnotation
		);
	}

	#[test]
	fn single_colon_not_confused_with_double_colon() {
		// Sanity: "::" is PathAccess, single ":" is TypeAnnotation
		assert_eq!(
			classify_context("Foo::", 5),
			CompletionContext::PathAccess { lhs_end: 3 }
		);
		assert_eq!(
			classify_context("a:", 2),
			CompletionContext::TypeAnnotation
		);
	}

	#[test]
	fn partial_identifier_mid_expression_is_identifier() {
		// "a + b" cursor after "b" — no special trigger
		assert_eq!(classify_context("a + b", 5), CompletionContext::Identifier);
	}
}
