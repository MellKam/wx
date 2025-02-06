import {
	Expression,
	ExpressionStatement,
	ExpressionKind,
	FunctionDeclarationStatement,
	Parser,
	StatementKind,
	IntExpression,
} from "./parser.ts";
import { Lexer, TokenKind, TokenNameByKind } from "./lexer.ts";
import { IdentifierExpression } from "./parser.ts";
import { UnaryExpression } from "./parser.ts";
import { BinaryExpression } from "./parser.ts";
import { AssignmentExpression } from "./parser.ts";
import { FloatExpression } from "./parser.ts";
import { FunctionReturnStatement } from "./parser.ts";
import { VariableDeclarationStatement } from "./parser.ts";
import { Statement } from "./parser.ts";

const Section = {
	Custom: 0,
	Type: 1,
	Import: 2,
	Function: 3,
	Table: 4,
	Memory: 5,
	Global: 6,
	Export: 7,
	Start: 8,
	Element: 9,
	Code: 10,
	Data: 11,
} as const;
type Section = (typeof Section)[keyof typeof Section];

const ValueType = {
	i32: 0x7f,
	i64: 0x7e,
	f32: 0x7d,
	f64: 0x7c,
} as const;
type ValueType = (typeof ValueType)[keyof typeof ValueType];

const ExportType = {
	Func: 0x00,
	Table: 0x01,
	Memory: 0x02,
	Global: 0x03,
} as const;
type ExportType = (typeof ExportType)[keyof typeof ExportType];

const OpCode = {
	LocalGet: 0x20,
	LocalSet: 0x21,
	LocalTee: 0x22,

	I32Const: 0x41,
	I64Const: 0x42,
	F32Const: 0x43,
	F64Const: 0x44,

	I32Clz: 0x67,
	I32Ctz: 0x68,
	I32Popcnt: 0x69,
	I32Add: 0x6a,
	I32Sub: 0x6b,
	I32Mul: 0x6c,
	I32DivS: 0x6d,
	I32DivU: 0x6e,
	I32RemS: 0x6f,
	I32RemU: 0x70,
	I32And: 0x71,
	I32Or: 0x72,
	I32Xor: 0x73,
	I32Shl: 0x74,
	I32ShrS: 0x75,
	I32ShrU: 0x76,
	I32Rotl: 0x77,
	I32Rotr: 0x78,

	I64Add: 0x7c,
	I64Sub: 0x7d,
	I64Mul: 0x7e,
	I64DivS: 0x7f,
	I64DivU: 0x80,

	F32Add: 0x92,
	F32Sub: 0x93,
	F32Mul: 0x94,
	F32Div: 0x95,

	F64Add: 0xa0,
	F64Sub: 0xa1,
	F64Mul: 0xa2,
	F64Div: 0xa3,
} as const;
type OpCode = (typeof OpCode)[keyof typeof OpCode];

type Instruction = [OpCode, ...number[]];

const lexer = new Lexer();
const parser = Parser.create();

const codegen_expression = (
	locals: Map<
		string,
		{
			type: ValueType;
			index: number;
		}
	>,
	expression: Expression
): Instruction[] => {
	if (expression.kind === ExpressionKind.Identifier) {
		const { value } = expression as IdentifierExpression;
		const local = locals.get(value);
		if (!local) {
			throw new Error("Invalid identifier");
		}
		return [[OpCode.LocalGet, local.index]];
	}
	if (expression.kind === ExpressionKind.Unary) {
		const { operator, right } = expression as UnaryExpression;
		const instructions = codegen_expression(locals, right);
		if (operator === TokenKind.Dash) {
			return [...instructions, [OpCode.I32Const, 1], [OpCode.I32Xor]];
		}
		throw new Error(`Invalid unary operator "${TokenNameByKind[operator]}"`);
	}
	if (expression.kind === ExpressionKind.Int) {
		const { value } = expression as IntExpression;
		return [[OpCode.I32Const, value]];
	}
	if (expression.kind === ExpressionKind.Float) {
		const { value } = expression as FloatExpression;
		return [[OpCode.F32Const, value]];
	}
	if (expression.kind === ExpressionKind.Binary) {
		const { operator, left, right } = expression as BinaryExpression;
		const left_instructions = codegen_expression(locals, left);
		const right_instructions = codegen_expression(locals, right);
		if (operator === TokenKind.Plus) {
			return [...left_instructions, ...right_instructions, [OpCode.I32Add]];
		}
		if (operator === TokenKind.Dash) {
			return [...left_instructions, ...right_instructions, [OpCode.I32Sub]];
		}
		throw new Error("Invalid binary operator");
	}
	if (expression.kind === ExpressionKind.Assignment) {
		const { identifier, value } = expression as AssignmentExpression;
		const local = locals.get(identifier);
		if (!local) {
			throw new Error("Invalid identifier");
		}
		const instructions = codegen_expression(locals, value);
		return [...instructions, [OpCode.LocalSet, local.index]];
	}
	throw new Error("Invalid expression");
};

const codegen_statement = (
	locals: Map<
		string,
		{
			type: ValueType;
			index: number;
		}
	>,
	statement: Statement
): Instruction[] => {
	switch (statement.kind) {
		case StatementKind.Expression: {
			const { expression } = statement as ExpressionStatement;
			return codegen_expression(locals, expression);
		}
		case StatementKind.FunctionReturn: {
			const { value } = statement as FunctionReturnStatement;
			return codegen_expression(locals, value);
		}
		case StatementKind.VariableDeclaration: {
			const { identifier, value } = statement as VariableDeclarationStatement;
			const existing_local = locals.get(identifier);
			if (existing_local) {
				throw new Error("Variable already declared");
			}

			const instructions: Instruction[] = [
				...codegen_expression(locals, value),
				[OpCode.LocalSet, locals.size],
			];

			locals.set(identifier, {
				type: ValueType.i32,
				index: locals.size,
			});
			return instructions;
		}
		default:
			throw new Error("Invalid statement");
	}
};

const codegen_function = (func: FunctionDeclarationStatement) => {
	const locals = new Map();

	const get_type = (type: string) => {
		if (type in ValueType) {
			return ValueType[type as keyof typeof ValueType];
		}
		throw new Error("Invalid type");
	};

	for (let i = 0; i < func.args.length; i++) {
		const [name, type] = func.args[i]!;
		locals.set(name, { type: get_type(type), index: i });
	}
	const instructions = func.body.flatMap((statement) =>
		codegen_statement(locals, statement)
	);
	return instructions;
};

const source = `
fn add(a: i32, b: i32): i32 {
  return a + b;
} 
`;

console.time("total");
const tokens = lexer.tokenize(source);
const ast = parser.parse(tokens);
const instructions = codegen_function(
	ast.at(0) as FunctionDeclarationStatement
);
console.timeEnd("total");

console.log(instructions.flat());
