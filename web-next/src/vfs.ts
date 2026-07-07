import { reactive, ref } from "vue";
import { WX_LANGUAGE_ID } from "./language";

export interface VfsFile {
	kind: "file";
	id: string;
	name: string;
	content: string;
}

export interface VfsDir {
	kind: "dir";
	id: string;
	name: string;
	children: VfsNode[];
}

export type VfsNode = VfsFile | VfsDir;

const DEFAULT_MAIN_WX = `use std::*;
memory heap: Memory where { Size = u32 };

import "console" {
	fn log(message: heap::[]u8);
}

fn main() {
	console::log("hello from wx!");
}

export {
	main,
	heap as "memory",
}
`;

const DEFAULT_HOST_JS = `async function run(bytecode) {
	const module = await WebAssembly.compile(bytecode);
	
	const importObject = {
		console: {
			log: (ptr, len) => {
				const memory = instance.exports.memory;
				if (!(memory instanceof WebAssembly.Memory)) {
					console.log("[log: module has no exported memory]");
					return;
				}
				const bytes = new Uint8Array(memory.buffer, ptr, len);
				console.log(new TextDecoder().decode(bytes));
			},
		},
	};

	const instance = await WebAssembly.instantiate(module, importObject);
	if (instance.exports.main instanceof Function) {
		instance.exports.main();
	}

	return instance.exports;
}

return run;
`;

function makeFile(name: string, content: string): VfsFile {
	return { kind: "file", id: crypto.randomUUID(), name, content };
}

function makeDir(name: string, children: VfsNode[] = []): VfsDir {
	return { kind: "dir", id: crypto.randomUUID(), name, children };
}

export const vfsRoot = reactive<VfsNode[]>([
	makeFile("main.wx", DEFAULT_MAIN_WX),
	makeFile("host.js", DEFAULT_HOST_JS),
]);

export const activeFileId = ref<string | null>(vfsRoot[0]!.id);

/** Bumped whenever the tree's *structure* changes (file/folder created).
 * Deliberately separate from `vfsRoot`'s own deep reactivity: watching
 * `vfsRoot` directly to react to new files would also re-fire on every
 * keystroke (editing a file mutates its reactive `content`), which is not
 * what "a new .wx file needs registering with the LSP" cares about. */
export const vfsVersion = ref(0);

interface FoundNode {
	node: VfsNode;
	parentId: string | null;
	path: string;
}

function walk(nodes: VfsNode[], prefix: string, parentId: string | null): FoundNode[] {
	const found: FoundNode[] = [];
	for (const node of nodes) {
		const path = prefix ? `${prefix}/${node.name}` : node.name;
		found.push({ node, parentId, path });
		if (node.kind === "dir") found.push(...walk(node.children, path, node.id));
	}
	return found;
}

function allNodes(): FoundNode[] {
	return walk(vfsRoot, "", null);
}

export function findNode(id: string): FoundNode | undefined {
	return allNodes().find((entry) => entry.node.id === id);
}

/** Path of a node as derived from its current position in the tree — never
 * cached, so a future rename/move can't leave a stale path lying around. */
export function nodePath(id: string): string | undefined {
	return findNode(id)?.path;
}

/** The directory a node should be created alongside: the node itself if
 * it's a directory, otherwise its parent (or root, for either). */
export function containingDirId(id: string | null): string | null {
	if (id === null) return null;
	const entry = findNode(id);
	if (!entry) return null;
	return entry.node.kind === "dir" ? entry.node.id : entry.parentId;
}

export function findByPath(path: string): VfsFile | undefined {
	const entry = allNodes().find((e) => e.path === path && e.node.kind === "file");
	return entry?.node as VfsFile | undefined;
}

export function languageForFile(file: VfsFile): string {
	if (file.name.endsWith(".wx")) return WX_LANGUAGE_ID;
	if (file.name.endsWith(".js")) return "javascript";
	return "plaintext";
}

function childrenOf(dirId: string | null): VfsNode[] {
	if (dirId === null) return vfsRoot;
	const entry = findNode(dirId);
	if (!entry || entry.node.kind !== "dir") return vfsRoot;
	return entry.node.children;
}

export function createFile(parentDirId: string | null, name: string): VfsFile {
	const file = makeFile(name, "");
	childrenOf(parentDirId).push(file);
	vfsVersion.value++;
	return file;
}

export function createFolder(parentDirId: string | null, name: string): VfsDir {
	const dir = makeDir(name);
	childrenOf(parentDirId).push(dir);
	vfsVersion.value++;
	return dir;
}

/** Renames are the only mutation to an existing node's `name` — routed
 * through here (rather than assigning `node.name` directly) so
 * `vfsVersion` bumps and anything keyed off file extension (wx-file
 * discovery, language selection) notices, e.g. finishing naming a
 * just-created file to "foo.wx". */
export function renameNode(id: string, name: string): void {
	const entry = findNode(id);
	if (!entry) return;
	entry.node.name = name;
	vfsVersion.value++;
}

/** Every `.wx` file in the tree with its derived path — the shape
 * `compile()` and wx-lsp's `didOpen` both need. */
export function listWxFiles(): { path: string; file: VfsFile }[] {
	return allNodes()
		.filter((e): e is FoundNode & { node: VfsFile } => e.node.kind === "file" && e.node.name.endsWith(".wx"))
		.map((e) => ({ path: e.path, file: e.node }));
}
