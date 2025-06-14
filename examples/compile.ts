import { compile } from "npm:wx-compiler-wasm";

const bytecode = compile(
	"main.wax",
	"export func add(a: i32, b: i32): i32 { a + b }"
);
if (!bytecode) {
	throw new Error("Compilation failed");
}

const module = await WebAssembly.compile(bytecode);
const instance = await WebAssembly.instantiate(module);
const { add } = instance.exports as {
	add: (a: number, b: number) => number;
};

console.log(add(3, 4)); // Should print 7
