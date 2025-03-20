import {
	BindingPower,
	parse_expression,
	type Expression,
} from "./expressions.ts";
import { TokenIterator, TokenKind, TokenName, type Token } from "./lexer.ts";
import { unescape_string_literal } from "./utils.ts";

export const enum StatementKind {
	/** @example add(1, 2) */
	Expression,
	/** @example { ... } */
	Block,
	/** @example let x: i32 = 1 */
	VariableDefinition,
	/** @example fn add(x: i32, y: i32): i32 ... */
	FunctionDefinition,
	/** @example return ...; */
	ReturnValue,
	/** @example export ... */
	ExportDefinition,
	/** @example let x: i32; */
	VariableDeclaration,
	/** @example fn add(x: i32, y: i32): i32; */
	FunctionDeclaration,
	/** @example import "env" ... */
	ModuleImportDeclaration,
}

export interface Statement {
	kind: StatementKind;
	start: number;
	end: number;
}

export interface ExpressionStatement extends Statement {
	kind: StatementKind.Expression;
	value: Expression;
	start: number;
	end: number;
}

export interface BlockStatement extends Statement {
	kind: StatementKind.Block;
	body: Statement[];
	start: number;
	end: number;
}

export interface VariableDefinitionStatement extends Statement {
	kind: StatementKind.VariableDefinition;
	id: Token;
	type: Token;
	value: Expression;
	start: number;
	end: number;
}

export interface ExportDefinitionStatement extends Statement {
	kind: StatementKind.ExportDefinition;
	definition: FunctionDefinitionStatement;
	start: number;
	end: number;
}

export interface FunctionParameter {
	id: Token;
	type: Token;
}

export interface FunctionDefinitionStatement extends Statement {
	kind: StatementKind.FunctionDefinition;
	id: Token;
	params: FunctionParameter[];
	result: Token | null;
	body: BlockStatement;
	start: number;
	end: number;
}

export interface ReturnValueStatement extends Statement {
	kind: StatementKind.ReturnValue;
	value: Expression;
	start: number;
	end: number;
}

export interface FunctionDeclarationStatement extends Statement {
	kind: StatementKind.FunctionDeclaration;
	id: Token;
	params: FunctionParameter[];
	result: Token | null;
	start: number;
	end: number;
}

export interface VariableDeclarationStatement extends Statement {
	kind: StatementKind.VariableDeclaration;
	id: Token;
	type: Token;
	start: number;
	end: number;
}

export interface ModuleImportDeclaration extends Statement {
	kind: StatementKind.ModuleImportDeclaration;
	name: Token;
	alias: Token;
	body: BlockStatement;
	start: number;
	end: number;
}

export interface StatementByKind extends Record<StatementKind, Statement> {
	[StatementKind.Expression]: ExpressionStatement;
	[StatementKind.VariableDefinition]: VariableDefinitionStatement;
	[StatementKind.FunctionDefinition]: FunctionDefinitionStatement;
	[StatementKind.ReturnValue]: ReturnValueStatement;
	[StatementKind.ExportDefinition]: ExportDefinitionStatement;
	[StatementKind.FunctionDeclaration]: FunctionDeclarationStatement;
	[StatementKind.VariableDeclaration]: VariableDeclarationStatement;
	[StatementKind.ModuleImportDeclaration]: ModuleImportDeclaration;
}

type StatementHandler = (
	tokens: TokenIterator,
	stack: StatementKind[]
) => Statement;

type StatementLookups = Map<TokenKind, StatementHandler>;

export const parse_statement = (
	tokens: TokenIterator,
	stack: StatementKind[]
): Statement => {
	const token = tokens.current();
	if (!token) {
		throw new Error("Expected statement");
	}

	const stmt_handler = lookups.get(token.kind);
	if (!stmt_handler) {
		console.error(stack);
		throw new Error(
			`No statement handler found for token ${TokenName[token.kind]} at ${
				tokens.position
			}`
		);
	}

	return stmt_handler(tokens, stack);
};

export const parser_expression_statement: StatementHandler = (
	tokens,
	_scope
): ExpressionStatement => {
	const expression = parse_expression(tokens, BindingPower.Default);

	const semicolon = tokens.advance();
	if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
		throw new Error("Expected semicolon after expression statement");
	}

	return {
		kind: StatementKind.Expression,
		value: expression,
		start: expression.start,
		end: semicolon.end,
	};
};

const parse_variable_statement = (
	tokens: TokenIterator,
	stack: StatementKind[]
): VariableDefinitionStatement | VariableDeclarationStatement => {
	const let_keyword = tokens.advance();
	if (!let_keyword || let_keyword.kind !== TokenKind.LetKeyword) {
		throw new Error("Expected let keyword");
	}

	const identifier = tokens.advance();
	if (!identifier || identifier.kind !== TokenKind.Identifier) {
		throw new Error("Expected identifier after let keyword");
	}

	const colon = tokens.advance();
	if (!colon || colon.kind !== TokenKind.Colon) {
		throw new Error("Expected colon after identifier");
	}

	const type = tokens.advance();
	if (!type || type.kind !== TokenKind.Identifier) {
		throw new Error("Expected type after colon");
	}

	const equals_or_semicolon = tokens.advance();
	if (!equals_or_semicolon) {
		throw new Error("Variable statement must end");
	}

	if (equals_or_semicolon.kind === TokenKind.Semicolon) {
		if (stack.at(-1) !== StatementKind.Block)
			return {
				kind: StatementKind.VariableDeclaration,
				id: identifier,
				type: type,
				start: let_keyword.start,
				end: equals_or_semicolon.end,
			};
	}

	if (equals_or_semicolon.kind !== TokenKind.Assign) {
		throw new Error("Unexpected token after type in variable declaration");
	}

	const value = parse_expression(tokens, BindingPower.Default);

	const semicolon = tokens.advance();
	if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
		throw new Error("Expected semicolon after variable declaration statement");
	}

	return {
		kind: StatementKind.VariableDefinition,
		id: identifier,
		type: type,
		value,
		start: let_keyword.start,
		end: semicolon.end,
	};
};

const parse_return_value_statement = (
	tokens: TokenIterator,
	stack: StatementKind[]
): ReturnValueStatement => {
	if (
		stack.at(-1) !== StatementKind.Block &&
		stack.at(-2) !== StatementKind.FunctionDefinition
	) {
		throw new Error("Return statements are only allowed in functions");
	}

	const return_keyword = tokens.advance();
	if (!return_keyword || return_keyword.kind !== TokenKind.ReturnKeyword) {
		throw new Error("Expected return keyword");
	}

	const value = parse_expression(tokens, BindingPower.Default);

	const semicolon = tokens.advance();
	if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
		throw new Error("Expected semicolon after return statement");
	}

	return {
		kind: StatementKind.ReturnValue,
		value,
		start: return_keyword.start,
		end: semicolon.end,
	};
};

const parse_block_statement = (
	tokens: TokenIterator,
	stack: StatementKind[]
): BlockStatement => {
	const parent_stmt = stack.at(-1);
	if (!parent_stmt) {
		throw new Error("Block are not allowed at the root level");
	}
	if (
		parent_stmt !== StatementKind.Block &&
		parent_stmt !== StatementKind.FunctionDefinition &&
		parent_stmt !== StatementKind.ModuleImportDeclaration
	) {
		throw new Error(
			"Block are only allowed in functions, blocks or module imports"
		);
	}

	const open_curly = tokens.advance();
	if (!open_curly || open_curly.kind !== TokenKind.OpenCurely) {
		throw new Error("Expected opening curly brace for block statement");
	}

	const body: Statement[] = [];
	const inner_stack = [...stack, StatementKind.Block];
	while (true) {
		const token = tokens.current();
		if (!token) {
			throw new Error("Expected statement");
		}
		if (token.kind === TokenKind.CloseCurely) break;

		const stmt_handler = lookups.get(token.kind);
		if (!stmt_handler) {
			throw new Error(
				`No statement handler found for token ${JSON.stringify(token)}`
			);
		}
		const statement = stmt_handler(tokens, inner_stack);
		body.push(statement);
	}

	const close_curly = tokens.advance();
	if (!close_curly || close_curly.kind !== TokenKind.CloseCurely) {
		throw new Error("Expected closing curly brace after block body");
	}

	return {
		kind: StatementKind.Block,
		body,
		start: open_curly.start,
		end: close_curly.end,
	};
};

const parse_function_statement = (
	tokens: TokenIterator,
	stack: StatementKind[]
): FunctionDeclarationStatement | FunctionDefinitionStatement => {
	const fn_keyword = tokens.advance();
	if (!fn_keyword || fn_keyword.kind !== TokenKind.FnKeyword) {
		throw new Error("Expected fn keyword");
	}

	const identifier = tokens.advance();
	if (!identifier || identifier.kind !== TokenKind.Identifier) {
		throw new Error("Expected identifier after fn keyword");
	}

	const open_paren = tokens.advance();
	if (!open_paren || open_paren.kind !== TokenKind.OpenParen) {
		throw new Error("Expected opening parenthesis after function name");
	}

	const params: FunctionParameter[] = [];

	while (true) {
		const param_identifier = tokens.advance();
		if (!param_identifier || param_identifier.kind !== TokenKind.Identifier) {
			throw new Error("Expected identifier as parameter name");
		}

		const colon = tokens.advance();
		if (!colon || colon.kind !== TokenKind.Colon) {
			throw new Error("Expected colon after argument name");
		}

		const param_type = tokens.advance();
		if (!param_type || param_type.kind !== TokenKind.Identifier) {
			throw new Error("Expected type after colon");
		}

		params.push({ id: param_identifier, type: param_type });

		const comma = tokens.advance();
		if (!comma) {
			throw new Error("Expected comma or closing parenthesis after argument");
		}
		if (comma.kind === TokenKind.CloseParen) {
			break;
		}
		if (comma.kind !== TokenKind.Comma) {
			throw new Error("Expected comma or closing parenthesis after argument");
		}
	}

	const token = tokens.current();
	if (!token) {
		throw new Error("Expected token after params");
	}

	let result: Token | null = null;
	if (token.kind === TokenKind.Colon) {
		tokens.advance();
		result = tokens.advance();
		if (!result || result.kind !== TokenKind.Identifier) {
			throw new Error("Expected return type after colon");
		}
	}

	const semicolon = tokens.current();
	if (!semicolon) {
		throw new Error("Expected semicolon after function declaration");
	}
	if (semicolon.kind === TokenKind.Semicolon) {
		tokens.advance();
		return {
			kind: StatementKind.FunctionDeclaration,
			id: identifier,
			params,
			result,
			start: fn_keyword.start,
			end: semicolon.end,
		};
	}
	if (semicolon.kind === TokenKind.OpenCurely) {
		return {
			kind: StatementKind.FunctionDefinition,
			id: identifier,
			params,
			result,
			body: parse_block_statement(tokens, [
				...stack,
				StatementKind.FunctionDefinition,
			]),
			start: fn_keyword.start,
			end: semicolon.end,
		};
	}
	throw new Error("Unexpected token after function declaration");
};

const parse_export_definition_statement = (
	tokens: TokenIterator,
	stack: StatementKind[]
): ExportDefinitionStatement => {
	if (stack.length !== 0) {
		throw new Error("Export statements are only allowed in global scope");
	}

	const export_keyword = tokens.advance();
	if (!export_keyword || export_keyword.kind !== TokenKind.ExportKeyword) {
		throw new Error("Expected export keyword");
	}

	const definition = parse_function_statement(tokens, [
		StatementKind.ExportDefinition,
	]) as FunctionDefinitionStatement;

	return {
		kind: StatementKind.ExportDefinition,
		definition,
		start: export_keyword.start,
		end: definition.end,
	};
};

const parse_module_import_statement = (
	tokens: TokenIterator,
	stack: StatementKind[]
): ModuleImportDeclaration => {
	if (stack.length !== 0) {
		throw new Error("Import statements are only allowed in global scope");
	}

	const import_keyword = tokens.advance();
	if (!import_keyword || import_keyword.kind !== TokenKind.ImportKeyword) {
		throw new Error("Expected import keyword");
	}

	const name = tokens.advance();
	if (!name || name.kind !== TokenKind.StringLiteral) {
		throw new Error("Expected string literal as module name");
	}

	const alias = tokens.advance();
	if (!alias || alias.kind !== TokenKind.Identifier) {
		throw new Error("Expected identifier as module alias");
	}

	const body = parse_block_statement(tokens, [
		StatementKind.ModuleImportDeclaration,
	]);

	return {
		kind: StatementKind.ModuleImportDeclaration,
		name: { ...name, value: unescape_string_literal(name.value) },
		alias,
		body,
		start: import_keyword.start,
		end: body.end,
	};
};

const create_statement_lookups = (): StatementLookups => {
	const statement_lookup = new Map<TokenKind, StatementHandler>();

	const statements: Array<[token: TokenKind, handler: StatementHandler]> = [
		[TokenKind.LetKeyword, parse_variable_statement],
		[TokenKind.FnKeyword, parse_function_statement],
		[TokenKind.ReturnKeyword, parse_return_value_statement],
		[TokenKind.ExportKeyword, parse_export_definition_statement],
		[TokenKind.OpenCurely, parse_block_statement],
		[TokenKind.ImportKeyword, parse_module_import_statement],
	];
	for (const [token, handler] of statements) {
		statement_lookup.set(token, handler);
	}

	return statement_lookup;
};

const lookups = create_statement_lookups();
