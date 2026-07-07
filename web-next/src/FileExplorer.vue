<script setup lang="ts">
import { ref } from "vue";
import { TreeItem, TreeRoot } from "reka-ui";
import {
	activeFileId,
	containingDirId,
	createFile,
	createFolder,
	renameNode,
	vfsRoot,
	type VfsFile,
	type VfsNode,
} from "./vfs";

// `open` always fires on a file click, even if it's already the active
// file — e.g. the user navigated into a read-only stdlib view via
// hover/go-to-definition (which doesn't touch `activeFileId` at all) and
// then re-clicks the same tree row to get back. A plain `watch` on
// `activeFileId` wouldn't see a change in that case; the caller (App.vue)
// needs an event that's unconditional.
const emit = defineEmits<{ open: [file: VfsFile] }>();

// The folder new files/folders are created into: whichever directory was
// last clicked, or the directory containing the last-opened file. Separate
// from `activeFileId` (that's "what's open in the editor", this is "where
// does the next New File/New Folder land").
const creationTargetDirId = ref<string | null>(null);

// Set immediately after creating a node so its row renders a name input
// instead of a label. Committing/cancelling clears it back to null.
const editingNodeId = ref<string | null>(null);
const editingName = ref("");

function getChildren(node: VfsNode): VfsNode[] | undefined {
	return node.kind === "dir" ? node.children : undefined;
}

function selectFile(node: VfsNode) {
	if (node.kind !== "file") return;
	activeFileId.value = node.id;
	creationTargetDirId.value = containingDirId(node.id);
	emit("open", node);
}

function selectDir(node: VfsNode) {
	if (node.kind !== "dir") return;
	creationTargetDirId.value = node.id;
}

function startCreate(kind: "file" | "dir") {
	const node = kind === "file" ? createFile(creationTargetDirId.value, "") : createFolder(creationTargetDirId.value, "");
	editingNodeId.value = node.id;
	editingName.value = "";
}

/** Removes a just-created, not-yet-named node — only ever used for the
 * create-then-cancel path below, not a general delete. */
function discardNode(id: string) {
	function removeFrom(nodes: VfsNode[]): boolean {
		const index = nodes.findIndex((n) => n.id === id);
		if (index !== -1) {
			nodes.splice(index, 1);
			return true;
		}
		return nodes.some((n) => n.kind === "dir" && removeFrom(n.children));
	}
	removeFrom(vfsRoot);
}

function commitEdit(node: VfsNode) {
	const name = editingName.value.trim();
	if (!name) {
		discardNode(node.id);
	} else {
		renameNode(node.id, name);
		if (node.kind === "file") {
			activeFileId.value = node.id;
			emit("open", node);
		}
	}
	editingNodeId.value = null;
}

function cancelEdit(node: VfsNode) {
	discardNode(node.id);
	editingNodeId.value = null;
}
</script>

<template>
	<div class="flex h-full w-56 shrink-0 flex-col border-r border-neutral-700 bg-[#1e1e1e] text-sm text-neutral-200">
		<div class="flex items-center justify-between border-b border-neutral-700 px-2 py-1.5">
			<span class="text-xs tracking-wide text-neutral-400 uppercase">Files</span>
			<div class="flex gap-1">
				<button
					class="cursor-pointer rounded px-1.5 py-0.5 text-xs hover:bg-neutral-700"
					title="New File"
					@click="startCreate('file')"
				>
					+File
				</button>
				<button
					class="cursor-pointer rounded px-1.5 py-0.5 text-xs hover:bg-neutral-700"
					title="New Folder"
					@click="startCreate('dir')"
				>
					+Folder
				</button>
			</div>
		</div>
		<TreeRoot
			v-slot="{ flattenItems }"
			class="min-h-0 flex-1 overflow-auto py-1"
			:items="vfsRoot"
			:get-key="(node: VfsNode) => node.id"
			:get-children="getChildren"
		>
			<TreeItem
				v-for="item in flattenItems"
				:key="item._id"
				v-slot="{ isExpanded, handleToggle, handleSelect }"
				v-bind="item.bind"
				as-child
			>
				<div
					class="flex cursor-pointer items-center gap-1 px-2 py-0.5 hover:bg-neutral-700/60"
					:class="{
						'bg-neutral-700': item.value.kind === 'file' && item.value.id === activeFileId,
					}"
					:style="{ paddingLeft: `${item.level * 12 + 8}px` }"
					@click="
						item.value.kind === 'dir'
							? (handleToggle(), selectDir(item.value))
							: (handleSelect(), selectFile(item.value))
					"
				>
					<span v-if="item.value.kind === 'dir'" class="w-3 shrink-0 text-neutral-400">
						{{ isExpanded ? "▾" : "▸" }}
					</span>
					<span v-else class="w-3 shrink-0"></span>

					<input
						v-if="editingNodeId === item.value.id"
						v-model="editingName"
						autofocus
						class="min-w-0 flex-1 rounded border border-[#0e639c] bg-[#3c3c3c] px-1 text-neutral-100 outline-none"
						@click.stop
						@keydown.enter="commitEdit(item.value)"
						@keydown.escape="cancelEdit(item.value)"
						@blur="commitEdit(item.value)"
					/>
					<span v-else class="truncate">{{ item.value.name }}</span>
				</div>
			</TreeItem>
		</TreeRoot>
	</div>
</template>
