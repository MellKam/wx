<script setup lang="ts">
import { ref } from "vue";
import { compile } from "wx-compiler-wasm";

interface Diagnostic {
	message: string;
	notes: any[];
	labels: any[];
	severity: "Error";
}

const code = ref(`func add(a: i32, b: i32) -> i32 { a + b }
func sub(a: i32, b: i32) -> i32 { a - b }

func apply(
  binop: func(i32, i32) -> i32,
  a: i32,
  b: i32,
) -> i32 {
  binop(a, b)
}

export func main() -> i32 {
  local a = apply(add, 5, 10);
  local b = apply(sub, 10, 5);

  a + b
}
`);
const result = ref<number | null>(null);
const diagnostics = ref<Diagnostic[]>();

const run = async () => {
	try {
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
		diagnostics.value = [];
	} catch (error) {
		if (Array.isArray(error)) {
			diagnostics.value = error as Diagnostic[];
			result.value = null;
		}
	}
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
			style="width: 100%; height: 240px; resize: vertical"
			v-model="code"
		/>
		<button @click="run()">Run</button>
		<span v-if="result !== null">Result: {{ result }}</span>
		<span v-else-if="diagnostics && diagnostics.length">
			Diagnostics:
			<ul>
				<li v-for="(diagnostic, index) in diagnostics" :key="index">
					{{ diagnostic }}
				</li>
			</ul>
		</span>
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
