// Types for WASM sections and values
enum Section {
	Custom = 0,
	Type = 1,
	Import = 2,
	Function = 3,
	Table = 4,
	Memory = 5,
	Global = 6,
	Export = 7,
	Start = 8,
	Element = 9,
	Code = 10,
	Data = 11,
}

enum ValueType {
	i32 = 0x7f,
	i64 = 0x7e,
	f32 = 0x7d,
	f64 = 0x7c,
	v128 = 0x7b,
	funcref = 0x70,
	externref = 0x6f,
}

enum ExportType {
	Func = 0x00,
	Table = 0x01,
	Memory = 0x02,
	Global = 0x03,
}

// WebAssembly Instructions (Opcodes)
enum Instruction {
	// Control Instructions
	Unreachable = 0x00,
	Nop = 0x01,
	Block = 0x02,
	Loop = 0x03,
	If = 0x04,
	Else = 0x05,
	End = 0x0b,
	Br = 0x0c,
	BrIf = 0x0d,
	BrTable = 0x0e,
	Return = 0x0f,
	Call = 0x10,
	CallIndirect = 0x11,

	// Parametric Instructions
	Drop = 0x1a,
	Select = 0x1b,

	// Variable Instructions
	LocalGet = 0x20,
	LocalSet = 0x21,
	LocalTee = 0x22,
	GlobalGet = 0x23,
	GlobalSet = 0x24,

	// Memory Instructions
	I32Load = 0x28,
	I64Load = 0x29,
	F32Load = 0x2a,
	F64Load = 0x2b,
	I32Load8S = 0x2c,
	I32Load8U = 0x2d,
	I32Load16S = 0x2e,
	I32Load16U = 0x2f,
	I64Load8S = 0x30,
	I64Load8U = 0x31,
	I64Load16S = 0x32,
	I64Load16U = 0x33,
	I64Load32S = 0x34,
	I64Load32U = 0x35,
	I32Store = 0x36,
	I64Store = 0x37,
	F32Store = 0x38,
	F64Store = 0x39,
	I32Store8 = 0x3a,
	I32Store16 = 0x3b,
	I64Store8 = 0x3c,
	I64Store16 = 0x3d,
	I64Store32 = 0x3e,
	MemorySize = 0x3f,
	MemoryGrow = 0x40,

	// Numeric Instructions - Constants
	I32Const = 0x41,
	I64Const = 0x42,
	F32Const = 0x43,
	F64Const = 0x44,

	// Numeric Instructions - Comparison
	I32Eqz = 0x45,
	I32Eq = 0x46,
	I32Ne = 0x47,
	I32LtS = 0x48,
	I32LtU = 0x49,
	I32GtS = 0x4a,
	I32GtU = 0x4b,
	I32LeS = 0x4c,
	I32LeU = 0x4d,
	I32GeS = 0x4e,
	I32GeU = 0x4f,

	I64Eqz = 0x50,
	I64Eq = 0x51,
	I64Ne = 0x52,
	I64LtS = 0x53,
	I64LtU = 0x54,
	I64GtS = 0x55,
	I64GtU = 0x56,
	I64LeS = 0x57,
	I64LeU = 0x58,
	I64GeS = 0x59,
	I64GeU = 0x5a,

	F32Eq = 0x5b,
	F32Ne = 0x5c,
	F32Lt = 0x5d,
	F32Gt = 0x5e,
	F32Le = 0x5f,
	F32Ge = 0x60,

	F64Eq = 0x61,
	F64Ne = 0x62,
	F64Lt = 0x63,
	F64Gt = 0x64,
	F64Le = 0x65,
	F64Ge = 0x66,

	// Numeric Instructions - Arithmetic
	I32Clz = 0x67,
	I32Ctz = 0x68,
	I32Popcnt = 0x69,
	I32Add = 0x6a,
	I32Sub = 0x6b,
	I32Mul = 0x6c,
	I32DivS = 0x6d,
	I32DivU = 0x6e,
	I32RemS = 0x6f,
	I32RemU = 0x70,
	I32And = 0x71,
	I32Or = 0x72,
	I32Xor = 0x73,
	I32Shl = 0x74,
	I32ShrS = 0x75,
	I32ShrU = 0x76,
	I32Rotl = 0x77,
	I32Rotr = 0x78,

	I64Clz = 0x79,
	I64Ctz = 0x7a,
	I64Popcnt = 0x7b,
	I64Add = 0x7c,
	I64Sub = 0x7d,
	I64Mul = 0x7e,
	I64DivS = 0x7f,
	I64DivU = 0x80,
	I64RemS = 0x81,
	I64RemU = 0x82,
	I64And = 0x83,
	I64Or = 0x84,
	I64Xor = 0x85,
	I64Shl = 0x86,
	I64ShrS = 0x87,
	I64ShrU = 0x88,
	I64Rotl = 0x89,
	I64Rotr = 0x8a,

	F32Abs = 0x8b,
	F32Neg = 0x8c,
	F32Ceil = 0x8d,
	F32Floor = 0x8e,
	F32Trunc = 0x8f,
	F32Nearest = 0x90,
	F32Sqrt = 0x91,
	F32Add = 0x92,
	F32Sub = 0x93,
	F32Mul = 0x94,
	F32Div = 0x95,
	F32Min = 0x96,
	F32Max = 0x97,
	F32Copysign = 0x98,

	F64Abs = 0x99,
	F64Neg = 0x9a,
	F64Ceil = 0x9b,
	F64Floor = 0x9c,
	F64Trunc = 0x9d,
	F64Nearest = 0x9e,
	F64Sqrt = 0x9f,
	F64Add = 0xa0,
	F64Sub = 0xa1,
	F64Mul = 0xa2,
	F64Div = 0xa3,
	F64Min = 0xa4,
	F64Max = 0xa5,
	F64Copysign = 0xa6,

	// Conversions
	I32WrapI64 = 0xa7,
	I32TruncF32S = 0xa8,
	I32TruncF32U = 0xa9,
	I32TruncF64S = 0xaa,
	I32TruncF64U = 0xab,
	I64ExtendI32S = 0xac,
	I64ExtendI32U = 0xad,
	I64TruncF32S = 0xae,
	I64TruncF32U = 0xaf,
	I64TruncF64S = 0xb0,
	I64TruncF64U = 0xb1,
	F32ConvertI32S = 0xb2,
	F32ConvertI32U = 0xb3,
	F32ConvertI64S = 0xb4,
	F32ConvertI64U = 0xb5,
	F32DemoteF64 = 0xb6,
	F64ConvertI32S = 0xb7,
	F64ConvertI32U = 0xb8,
	F64ConvertI64S = 0xb9,
	F64ConvertI64U = 0xba,
	F64PromoteF32 = 0xbb,

	// Reinterpretations
	I32ReinterpretF32 = 0xbc,
	I64ReinterpretF64 = 0xbd,
	F32ReinterpretI32 = 0xbe,
	F64ReinterpretI64 = 0xbf,
}

class WasmModuleBuilder {
	private bytes: number[] = [];

	// Add a byte or array of bytes
	private add(...vals: number[]): void {
		this.bytes.push(...vals);
	}

	// Add a string as bytes
	private addString(str: string): void {
		const bytes = new TextEncoder().encode(str);
		this.add(bytes.length, ...bytes);
	}

	// Initialize module with magic number and version
	private addHeader(): void {
		// Magic number: \0asm
		this.add(0x00, 0x61, 0x73, 0x6d);
		// Version: 1
		this.add(0x01, 0x00, 0x00, 0x00);
	}

	// Add type section for function signatures
	private addTypeSection(
		paramTypes: ValueType[],
		returnTypes: ValueType[]
	): void {
		const contents = [
			0x01, // number of types
			0x60, // func
			paramTypes.length, // param count
			...paramTypes, // param types
			returnTypes.length, // return count
			...returnTypes, // return types
		];

		this.add(
			Section.Type, // section id
			contents.length, // section size
			...contents // section contents
		);
	}

	// Add function section
	private addFunctionSection(typeIndices: number[]): void {
		const contents = [
			typeIndices.length, // number of functions
			...typeIndices, // type indices
		];

		this.add(
			Section.Function, // section id
			contents.length, // section size
			...contents // section contents
		);
	}

	// Add export section
	private addExportSection(
		name: string,
		kind: ExportType,
		index: number
	): void {
		const nameBytes = new TextEncoder().encode(name);
		const contents = [
			0x01, // number of exports
			nameBytes.length, // name length
			...nameBytes, // name
			kind, // export kind
			index, // export index
		];

		this.add(
			Section.Export, // section id
			contents.length, // section size
			...contents // section contents
		);
	}

	// Add code section with function body
	private addCodeSection(instructions: number[]): void {
		const functionBody = [
			0x00, // local decl count
			...instructions, // function body
			Instruction.End, // end
		];

		const contents = [
			0x01, // number of functions
			functionBody.length, // function body size
			...functionBody, // function body
		];

		this.add(
			Section.Code, // section id
			contents.length, // section size
			...contents // section contents
		);
	}

	// Create a complete WASM module
	public static createAddModule(): Uint8Array {
		const builder = new WasmModuleBuilder();

		// Add module header
		builder.addHeader();

		// Add type section (i32, i32) -> i32
		builder.addTypeSection([ValueType.i32, ValueType.i32], [ValueType.i32]);

		// Add function section
		builder.addFunctionSection([0]);

		// Add export section
		builder.addExportSection("add", ExportType.Func, 0);

		// Add code section using typed instructions
		builder.addCodeSection([
			32, 0, 32, 1, 106, 65, 8, 65, 2, 65, 1, 115, 107, 106,
		]);

		return new Uint8Array(builder.bytes);
	}
}

// Usage example
async function initWasmModule() {
	const wasmBytes = WasmModuleBuilder.createAddModule();

	try {
		const result = await WebAssembly.instantiate(wasmBytes);

		// Type definition for the WASM module exports
		type WasmExports = {
			add: (a: number, b: number) => number;
		};

		const { add } = result.instance.exports as WasmExports;

		// Test the add function
		console.log(add(5, 3)); // Should output: 8
	} catch (error) {
		console.error("Failed to instantiate WASM module:", error);
	}
}

// Run the example
initWasmModule();
