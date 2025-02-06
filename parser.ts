import { Token, TokenKind } from "./lexer.ts";

export const ExpressionKind = {
	Int: 0, // 123
	Float: 1, // 123.45
	Identifier: 2, // x
	Unary: 3, // -1
	Binary: 4, // 1 + 2
	Assignment: 5, // x = 1
} as const;
export type ExpressionKind =
	(typeof ExpressionKind)[keyof typeof ExpressionKind];

export interface Expression {
	kind: ExpressionKind;
	start: number;
	end: number;
}

export const StatementKind = {
	Expression: 0, // 1 + 2;
	VariableDeclaration: 1, // let x: i32 = 1;
	FunctionDeclaration: 2, // fn add(x: i32, y: i32): i32 { return x + y; }
	FunctionReturn: 3, // return 1;
} as const;
export type StatementKind = (typeof StatementKind)[keyof typeof StatementKind];

export interface Statement {
	kind: StatementKind;
	start: number;
	end: number;
}

export const BindingPower = {
	Default: 0,
	Assignment: 1,
	Additive: 2,
	Multiplicative: 3,
	Primary: 4,
} as const;
export type BindingPower = (typeof BindingPower)[keyof typeof BindingPower];

export class TokenIterator {
	readonly tokens: Token[];
	position: number;

	constructor(tokens: Token[]) {
		this.tokens = tokens;
		this.position = 0;
	}

	current(): Token | null {
		return this.tokens[this.position] || null;
	}

	advance(): Token | null {
		const previous_token = this.tokens[this.position] || null;
		if (previous_token) this.position++;
		return previous_token;
	}
}

type NullDenotationHandler = (
	parser: Parser,
	tokens: TokenIterator
) => Expression;
type LeftDenotationHandler = (
	parser: Parser,
	tokens: TokenIterator,
	left: Expression,
	binding_power: BindingPower
) => Expression;
type StatementHandler = (parser: Parser, tokens: TokenIterator) => Statement;

export class Parser {
	readonly binding_power_lookup: Map<TokenKind, BindingPower>;
	readonly nud_lookup: Map<TokenKind, NullDenotationHandler>;
	readonly led_lookup: Map<TokenKind, LeftDenotationHandler>;
	readonly statement_lookup: Map<TokenKind, StatementHandler>;

	constructor(
		binding_power_lookup: Map<TokenKind, BindingPower>,
		nud_lookup: Map<TokenKind, NullDenotationHandler>,
		led_lookup: Map<TokenKind, LeftDenotationHandler>,
		statement_lookup: Map<TokenKind, StatementHandler>
	) {
		this.binding_power_lookup = binding_power_lookup;
		this.nud_lookup = nud_lookup;
		this.led_lookup = led_lookup;
		this.statement_lookup = statement_lookup;
	}

	static create(): Parser {
		const binding_power_lookup = new Map<TokenKind, BindingPower>();
		const nud_lookup = new Map<TokenKind, NullDenotationHandler>();
		const led_lookup = new Map<TokenKind, LeftDenotationHandler>();
		const statement_lookup = new Map<TokenKind, StatementHandler>();

		const leds: Array<
			[token: TokenKind, bp: BindingPower, handler: LeftDenotationHandler]
		> = [
			[TokenKind.Assign, BindingPower.Assignment, parse_assignment_expression],
			[TokenKind.Plus, BindingPower.Additive, parse_binary_expression],
			[TokenKind.Dash, BindingPower.Additive, parse_binary_expression],
			[TokenKind.Star, BindingPower.Multiplicative, parse_binary_expression],
			[TokenKind.Slash, BindingPower.Multiplicative, parse_binary_expression],
			[TokenKind.Percent, BindingPower.Multiplicative, parse_binary_expression],
		];
		for (const [token, bp, handler] of leds) {
			binding_power_lookup.set(token, bp);
			led_lookup.set(token, handler);
		}

		const nuds: Array<[token: TokenKind, handler: NullDenotationHandler]> = [
			[TokenKind.Identifier, parse_identifier_expression],
			[TokenKind.IntLiteral, parse_int_expression],
			[TokenKind.FloatLiteral, parse_float_expression],
			[TokenKind.OpenParen, parse_grouping_expression],
			[TokenKind.Dash, parse_unary_expression],
		];
		for (const [token, handler] of nuds) {
			binding_power_lookup.set(token, BindingPower.Primary);
			nud_lookup.set(token, handler);
		}

		const statements: Array<[token: TokenKind, handler: StatementHandler]> = [
			[TokenKind.LetKeyword, parse_variable_declaration_statement],
			[TokenKind.FnKeyword, parse_function_declaration_statement],
			[TokenKind.ReturnKeyword, parse_function_return_statement],
		];
		for (const [token, handler] of statements) {
			binding_power_lookup.set(token, BindingPower.Default);
			statement_lookup.set(token, handler);
		}

		return new Parser(
			binding_power_lookup,
			nud_lookup,
			led_lookup,
			statement_lookup
		);
	}

	parse(tokens: Token[]): Statement[] {
		const token_iterator = new TokenIterator(tokens);
		const body: Statement[] = [];

		while (true) {
			const token = token_iterator.current();
			if (!token) break;
			const statement_handler =
				this.statement_lookup.get(token.kind) || parse_expression_statement;

			const statement = statement_handler(this, token_iterator);
			body.push(statement);
		}

		return body;
	}

	parse_expression(tokens: Token[]): Expression {
		const token_iterator = new TokenIterator(tokens);
		return parse_expression(this, token_iterator, BindingPower.Default);
	}
}

const parse_expression = (
	parser: Parser,
	tokens: TokenIterator,
	min_binding_power: BindingPower
): Expression => {
	const token = tokens.current();
	if (!token) {
		throw new Error("No token found");
	}
	const nud_handler = parser.nud_lookup.get(token.kind);
	if (!nud_handler) {
		throw new Error(`No NUD handler found for token ${JSON.stringify(token)}`);
	}
	let left = nud_handler(parser, tokens);

	while (true) {
		const token = tokens.current();
		if (!token) break;
		const operator_binding_power = parser.binding_power_lookup.get(token.kind);
		if (
			operator_binding_power === undefined ||
			operator_binding_power <= min_binding_power
		) {
			break;
		}

		const led_handler = parser.led_lookup.get(token.kind);
		if (!led_handler) {
			throw new Error("No LED handler found");
		}

		left = led_handler(parser, tokens, left, operator_binding_power);
	}

	return left;
};

export interface IntExpression extends Expression {
	kind: typeof ExpressionKind.Int;
	value: number;
	start: number;
	end: number;
}

const parse_int_expression: NullDenotationHandler = (
	_parser,
	tokens
): IntExpression => {
	const token = tokens.advance();
	if (!token) throw new Error("No token found");
	const value = parseInt(token.value);
	if (isNaN(value)) throw new Error("Failed to parse integer");

	return {
		kind: ExpressionKind.Int,
		value,
		start: token.start,
		end: token.end,
	};
};

export interface FloatExpression extends Expression {
	kind: typeof ExpressionKind.Int;
	value: number;
	start: number;
	end: number;
}

const parse_float_expression: NullDenotationHandler = (
	_parser,
	tokens
): FloatExpression => {
	const token = tokens.advance();
	if (!token) throw new Error("No token found");
	const value = parseFloat(token.value);
	if (isNaN(value)) throw new Error("Failed to parse float");

	return {
		kind: ExpressionKind.Int,
		value,
		start: token.start,
		end: token.end,
	};
};

export interface IdentifierExpression extends Expression {
	kind: typeof ExpressionKind.Identifier;
	value: string;
	start: number;
	end: number;
}

export const parse_identifier_expression: NullDenotationHandler = (
	_parser,
	tokens
): IdentifierExpression => {
	const token = tokens.advance();
	if (!token) throw new Error("Expected identifier");

	return {
		kind: ExpressionKind.Identifier,
		value: token.value,
		start: token.start,
		end: token.end,
	};
};

export interface UnaryExpression extends Expression {
	kind: typeof ExpressionKind.Unary;
	operator: TokenKind;
	right: Expression;
	start: number;
	end: number;
}

const parse_unary_expression: NullDenotationHandler = (
	parser,
	tokens
): UnaryExpression => {
	const operator = tokens.advance();
	if (!operator) throw new Error("No token found");
	const right = parse_expression(parser, tokens, BindingPower.Primary);

	return {
		kind: ExpressionKind.Unary,
		operator: operator.kind,
		right,
		start: operator.start,
		end: right.end,
	};
};

const parse_grouping_expression: NullDenotationHandler = (parser, tokens) => {
	const _ = tokens.advance();
	const expression = parse_expression(parser, tokens, BindingPower.Default);
	const token = tokens.advance();
	if (!token) throw new Error("No token found");
	if (token.kind === TokenKind.CloseParen) {
		return expression;
	} else {
		throw new Error("Expected closing parenthesis");
	}
};

export interface BinaryExpression extends Expression {
	kind: typeof ExpressionKind.Binary;
	left: Expression;
	operator: TokenKind;
	right: Expression;
	start: number;
	end: number;
}

const parse_binary_expression: LeftDenotationHandler = (
	parser,
	tokens,
	left,
	binding_power
): BinaryExpression => {
	const operator = tokens.advance();
	if (!operator) throw new Error("No token found");
	const right = parse_expression(parser, tokens, binding_power);

	return {
		kind: ExpressionKind.Binary,
		left,
		operator: operator.kind,
		right,
		start: left.start,
		end: right.end,
	};
};

export interface VariableDeclarationStatement extends Statement {
	kind: typeof StatementKind.VariableDeclaration;
	identifier: string;
	type: string;
	value: Expression;
	start: number;
	end: number;
}

const parse_variable_declaration_statement: StatementHandler = (
	parser,
	tokens
): VariableDeclarationStatement => {
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

	const equals = tokens.advance();
	if (!equals || equals.kind !== TokenKind.Assign)
		throw new Error("Expected equal sign after identifier");

	const value = parse_expression(parser, tokens, BindingPower.Default);

	const semicolon = tokens.advance();
	if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
		throw new Error("Expected semicolon after variable declaration statement");
	}

	return {
		kind: StatementKind.VariableDeclaration,
		identifier: identifier.value,
		type: type.value,
		value,
		start: let_keyword.start,
		end: semicolon.end,
	};
};

export interface ExpressionStatement extends Statement {
	kind: typeof StatementKind.Expression;
	expression: Expression;
	start: number;
	end: number;
}

const parse_expression_statement: StatementHandler = (
	parser,
	tokens
): ExpressionStatement => {
	const expression = parse_expression(parser, tokens, BindingPower.Default);
	const semicolon = tokens.advance();
	if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
		throw new Error("Expected semicolon after expression statement");
	}

	return {
		kind: StatementKind.Expression,
		expression,
		start: expression.start,
		end: semicolon.end,
	};
};

export interface AssignmentExpression extends Expression {
	kind: typeof ExpressionKind.Assignment;
	identifier: string;
	value: Expression;
	start: number;
	end: number;
}

const parse_assignment_expression: LeftDenotationHandler = (
	parser,
	tokens,
	left,
	binding_power
): AssignmentExpression => {
	if (left.kind !== ExpressionKind.Identifier) {
		throw new Error("Expected identifier on the left side of assignment");
	}

	const equals = tokens.advance();
	if (!equals || equals.kind !== TokenKind.Assign) {
		throw new Error("Expected equal sign after identifier");
	}

	const value = parse_expression(parser, tokens, binding_power);

	return {
		kind: ExpressionKind.Assignment,
		identifier: (left as IdentifierExpression).value,
		value,
		start: left.start,
		end: value.end,
	};
};

export interface FunctionDeclarationStatement extends Statement {
	kind: typeof StatementKind.FunctionDeclaration;
	name: string;
	args: Array<[name: string, type: string]>;
	return_type: string;
	body: Statement[];
	start: number;
	end: number;
}

const parse_function_declaration_statement: StatementHandler = (
	parser,
	tokens
): FunctionDeclarationStatement => {
	const fn_keyword = tokens.advance();
	if (!fn_keyword || fn_keyword.kind !== TokenKind.FnKeyword) {
		throw new Error("Expected fn keyword");
	}

	const name = tokens.advance();
	if (!name || name.kind !== TokenKind.Identifier) {
		throw new Error("Expected identifier after fn keyword");
	}

	const open_paren = tokens.advance();
	if (!open_paren || open_paren.kind !== TokenKind.OpenParen) {
		throw new Error("Expected opening parenthesis after function name");
	}

	const args: [name: string, type: string][] = [];
	while (true) {
		const arg_name = tokens.advance();
		if (!arg_name || arg_name.kind !== TokenKind.Identifier) {
			throw new Error("Expected identifier as argument name");
		}

		const colon = tokens.advance();
		if (!colon || colon.kind !== TokenKind.Colon) {
			throw new Error("Expected colon after argument name");
		}

		const arg_type = tokens.advance();
		if (!arg_type || arg_type.kind !== TokenKind.Identifier) {
			throw new Error("Expected type after colon");
		}

		args.push([arg_name.value, arg_type.value]);

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

	const colon = tokens.advance();
	if (!colon || colon.kind !== TokenKind.Colon) {
		throw new Error("Expected colon after closing parenthesis");
	}

	const return_type = tokens.advance();
	if (!return_type || return_type.kind !== TokenKind.Identifier) {
		throw new Error("Expected return type after colon");
	}

	const open_curly = tokens.advance();
	if (!open_curly || open_curly.kind !== TokenKind.OpenCurely) {
		throw new Error("Expected opening curly brace after return type");
	}

	const body: Statement[] = [];
	while (true) {
		const token = tokens.current();
		if (!token) break;
		if (token.kind === TokenKind.CloseCurely) break;

		const statement_handler =
			parser.statement_lookup.get(token.kind) || parse_expression_statement;
		const statement = statement_handler(parser, tokens);
		body.push(statement);
	}

	const close_curly = tokens.advance();
	if (!close_curly || close_curly.kind !== TokenKind.CloseCurely) {
		throw new Error("Expected closing curly brace after function body");
	}

	return {
		kind: StatementKind.FunctionDeclaration,
		name: name.value,
		args,
		return_type: return_type.value,
		body,
		start: fn_keyword.start,
		end: close_curly.end,
	};
};

export interface FunctionReturnStatement extends Statement {
	kind: typeof StatementKind.FunctionReturn;
	value: Expression;
	start: number;
	end: number;
}

const parse_function_return_statement: StatementHandler = (
	parser,
	tokens
): FunctionReturnStatement => {
	const return_keyword = tokens.advance();
	if (!return_keyword || return_keyword.kind !== TokenKind.ReturnKeyword) {
		throw new Error("Expected return keyword");
	}

	const value = parse_expression(parser, tokens, BindingPower.Default);

	const semicolon = tokens.advance();
	if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
		throw new Error("Expected semicolon after return statement");
	}

	return {
		kind: StatementKind.FunctionReturn,
		value,
		start: return_keyword.start,
		end: semicolon.end,
	};
};
