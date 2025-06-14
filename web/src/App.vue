<script setup lang="ts">
import { ref } from "vue";
import { compile } from "wx-compiler-wasm";

const code = ref(`func add(a: i32, b: i32): i32 { a + b }

export func main(): i32 {
   add(2, 2)
}`);
const result = ref<number | null>(null);

const run = async () => {
	const bytecode = compile("main.wx", code.value);
	if (!bytecode) {
		throw new Error("Compilation failed");
	}

	const module = await WebAssembly.compile(bytecode);
	const instance = await WebAssembly.instantiate(module);
	const { main } = instance.exports as {
		main: () => number;
	};

	result.value = main();
};
</script>

<template>
	<div
		style="
			display: flex;
			flex-direction: column;
			margin: 0 auto;
			width: 640px;
			gap: 16px;
			padding: 24px;
		"
	>
		<textarea
			style="width: 100%; height: 160px; resize: vertical"
			v-model="code"
		/>
		<button @click="run()">Run</button>
		<span v-if="result !== null">Result: {{ result }}</span>
	</div>
</template>

<style>
:root {
	color-scheme: dark;
}

* {
	margin: 0;
	padding: 0;
	box-sizing: border-box;
}
</style>
