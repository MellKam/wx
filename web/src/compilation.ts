import type { editor, MarkerSeverity, MarkerTag } from "monaco-editor";
import { compile as compileWX } from "wx-compiler-wasm";

export interface Diagnostic {
	code: undefined;
	message: string;
	notes: any[];
	labels: {
		file_id: number;
		message: string;
		range: { start: number; end: number };
		style: "Primary";
	}[];
	severity: "Error";
}

export const compile = (
	filename: string,
	source: string
):
	| {
			success: false;
			diagnostics: Diagnostic[];
	  }
	| {
			success: true;
			bytecode: Uint8Array;
	  } => {
	try {
		const bytecode = compileWX(filename, source);
		if (!bytecode) {
			throw new Error("compilation failed: internal error");
		}
		return {
			success: true,
			bytecode,
		};
	} catch (error) {
		if (Array.isArray(error)) {
			return {
				success: false,
				diagnostics: error,
			};
		}
		throw error;
	}
};

const getLineOffsets = (code: string): number[] => {
	if (code === "") return [0];

	const offsets = [0];
	for (let i = 0; i < code.length; i++) {
		if (code[i] === "\n") {
			offsets.push(i + 1);
		}
	}
	return offsets;
};

const offsetToPosition = (
	offsets: number[],
	offset: number
): [line: number, column: number] => {
	let left = 0;
	let right = offsets.length - 1;

	while (left < right) {
		const mid = Math.floor((left + right + 1) / 2);
		if (offsets[mid]! <= offset) {
			left = mid;
		} else {
			right = mid - 1;
		}
	}

	const lineNumber = left + 1;
	const lineStart = offsets[left]!;
	const column = offset - lineStart + 1;

	return [lineNumber, column];
};

export const createMarkerData = (
	code: string,
	diagnostics: Diagnostic[]
): editor.IMarkerData[] => {
	if (diagnostics.length === 0) return [];
	const offsets = getLineOffsets(code);

	return diagnostics
		.filter((diagnostics) => diagnostics.labels.length > 0)
		.map((diagnostic) => {
			const label = diagnostic.labels[0]!;
			const [startLineNumber, startColumn] = offsetToPosition(
				offsets,
				label.range.start
			);
			const [endLineNumber, endColumn] = offsetToPosition(
				offsets,
				label.range.end
			);
			return {
				message: label.message
					? `${diagnostic.message}: ${label.message}`
					: diagnostic.message,
				tags:
					diagnostic.message === "unreachable code"
						? [1 satisfies MarkerTag.Unnecessary]
						: undefined,
				severity: 8 satisfies MarkerSeverity.Error,
				startLineNumber,
				endLineNumber,
				startColumn,
				endColumn,
			};
		});
};
