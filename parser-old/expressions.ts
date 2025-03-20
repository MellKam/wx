import { TokenKind, TokenIterator } from "./lexer.ts";

export const enum ExpressionKind {
	/** @example 123 */
	Int,
	/** @example 123.45 */
	Float,
	/** @exapmle foo */
	Identifier,
	/** @example -1 */
	Unary,
	/** @example 1 + 2 */
	Binary,
	/** @example x = 1 */
	Assignment,
	/** @example add(1, 2) */
	FunctionCall,
}

export interface Expression {
	kind: ExpressionKind;
	start: number;
	end: number;
}

export interface IntExpression extends Expression {
	kind: ExpressionKind.Int;
	value: number;
	start: number;
	end: number;
}

export interface FloatExpression extends Expression {
	kind: ExpressionKind.Int;
	value: number;
	start: number;
	end: number;
}

export interface IdentifierExpression extends Expression {
	kind: ExpressionKind.Identifier;
	value: string;
	start: number;
	end: number;
}

export interface UnaryExpression extends Expression {
	kind: ExpressionKind.Unary;
	operator: TokenKind;
	right: Expression;
	start: number;
	end: number;
}

export interface BinaryExpression extends Expression {
	kind: ExpressionKind.Binary;
	left: Expression;
	operator: TokenKind;
	right: Expression;
	start: number;
	end: number;
}

export interface AssignmentExpression extends Expression {
	kind: ExpressionKind.Assignment;
	id: IdentifierExpression;
	value: Expression;
	start: number;
	end: number;
}

export interface FunctionCallExpression extends Expression {
	kind: ExpressionKind.FunctionCall;
	id: IdentifierExpression;
	args: Expression[];
	start: number;
	end: number;
}

export interface ExpressionByKind extends Record<ExpressionKind, Expression> {
	[ExpressionKind.Int]: IntExpression;
	[ExpressionKind.Float]: FloatExpression;
	[ExpressionKind.Identifier]: IdentifierExpression;
	[ExpressionKind.Unary]: UnaryExpression;
	[ExpressionKind.Binary]: BinaryExpression;
	[ExpressionKind.Assignment]: AssignmentExpression;
	[ExpressionKind.FunctionCall]: FunctionCallExpression;
}

export const enum BindingPower {
	Default,
	/** @example 1 = 2 */
	Assignment,
	/** @example 1 == 2 */
	Logical,
	/** @example 1 < 2 */
	Relational,
	/** @example 1 + 2 */
	Additive,
	/** @example 1 * 2 */
	Multiplicative,
	/** @example print() */
	Call,
	/** @example obj.property */
	Member,
	Primary,
}

type NullDenotationHandler = (tokens: TokenIterator) => Expression;
type LeftDenotationHandler = (
	tokens: TokenIterator,
	left: Expression,
	binding_power: BindingPower
) => Expression;

interface ExpressionLookups {
	nud_lookup: Map<TokenKind, NullDenotationHandler>;
	led_lookup: Map<TokenKind, LeftDenotationHandler>;
	bp_lookup: Map<TokenKind, BindingPower>;
}

export const parse_expression = (
	tokens: TokenIterator,
	min_binding_power: BindingPower
): Expression => {
	const token = tokens.current();
	if (!token) {
		throw new Error("No token found");
	}
	const nud_handler = lookups.nud_lookup.get(token.kind);
	if (!nud_handler) {
		throw new Error(`No NUD handler found for token ${JSON.stringify(token)}`);
	}
	let left = nud_handler(tokens);

	while (true) {
		const token = tokens.current();
		if (!token) break;
		const operator_binding_power = lookups.bp_lookup.get(token.kind);
		if (
			operator_binding_power === undefined ||
			operator_binding_power <= min_binding_power
		) {
			break;
		}

		const led_handler = lookups.led_lookup.get(token.kind);
		if (!led_handler) {
			throw new Error("No LED handler found");
		}

		left = led_handler(tokens, left, operator_binding_power);
	}

	return left;
};

const parse_int_expression: NullDenotationHandler = (tokens): IntExpression => {
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

const parse_float_expression: NullDenotationHandler = (
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

const parse_identifier_expression: NullDenotationHandler = (
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

const parse_unary_expression: NullDenotationHandler = (
	tokens
): UnaryExpression => {
	const operator = tokens.advance();
	if (!operator) throw new Error("No token found");
	const right = parse_expression(tokens, BindingPower.Primary);

	return {
		kind: ExpressionKind.Unary,
		operator: operator.kind,
		right,
		start: operator.start,
		end: right.end,
	};
};

const parse_grouping_expression: NullDenotationHandler = (tokens) => {
	const _ = tokens.advance();
	const expression = parse_expression(tokens, BindingPower.Default);
	const token = tokens.advance();
	if (!token) throw new Error("No token found");
	if (token.kind === TokenKind.CloseParen) {
		return expression;
	} else {
		throw new Error("Expected closing parenthesis");
	}
};

const parse_binary_expression: LeftDenotationHandler = (
	tokens,
	left,
	binding_power
): BinaryExpression => {
	const operator = tokens.advance();
	if (!operator) throw new Error("No token found");
	const right = parse_expression(tokens, binding_power);

	return {
		kind: ExpressionKind.Binary,
		left,
		operator: operator.kind,
		right,
		start: left.start,
		end: right.end,
	};
};

const parse_assignment_expression: LeftDenotationHandler = (
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

	const right = parse_expression(tokens, binding_power);

	return {
		kind: ExpressionKind.Assignment,
		id: left as IdentifierExpression,
		value: right,
		start: left.start,
		end: right.end,
	};
};

const parse_function_call_expression: LeftDenotationHandler = (
	tokens,
	left,
	_binding_power
): FunctionCallExpression => {
	if (left.kind !== ExpressionKind.Identifier) {
		throw new Error("Expected identifier for function call");
	}

	const args: Expression[] = [];
	const open_paren = tokens.advance();
	if (!open_paren || open_paren.kind !== TokenKind.OpenParen) {
		throw new Error("Expected '(' after function name");
	}

	while (true) {
		const token = tokens.current();
		if (!token) throw new Error("Unexpected end of input");

		if (token.kind === TokenKind.CloseParen) {
			break;
		}

		const arg = parse_expression(tokens, BindingPower.Default);
		args.push(arg);

		const next_token = tokens.current();
		if (!next_token) throw new Error("Unexpected end of input");

		if (next_token.kind === TokenKind.Comma) {
			tokens.advance(); // Skip comma
		} else if (next_token.kind !== TokenKind.CloseParen) {
			throw new Error("Expected ',' or ')'");
		}
	}

	const close_paren = tokens.advance();
	if (!close_paren || close_paren.kind !== TokenKind.CloseParen) {
		throw new Error("Expected closing parenthesis");
	}

	return {
		kind: ExpressionKind.FunctionCall,
		id: left as IdentifierExpression,
		args,
		start: left.start,
		end: close_paren.end,
	};
};

const create_expression_lookups = (): ExpressionLookups => {
	const bp_lookup = new Map<TokenKind, BindingPower>();
	const nud_lookup = new Map<TokenKind, NullDenotationHandler>();
	const led_lookup = new Map<TokenKind, LeftDenotationHandler>();

	const leds: Array<
		[token: TokenKind, bp: BindingPower, handler: LeftDenotationHandler]
	> = [
		[TokenKind.Assign, BindingPower.Assignment, parse_assignment_expression],
		[TokenKind.Plus, BindingPower.Additive, parse_binary_expression],
		[TokenKind.Dash, BindingPower.Additive, parse_binary_expression],
		[TokenKind.Star, BindingPower.Multiplicative, parse_binary_expression],
		[TokenKind.Slash, BindingPower.Multiplicative, parse_binary_expression],
		[TokenKind.Percent, BindingPower.Multiplicative, parse_binary_expression],
		[TokenKind.OpenParen, BindingPower.Call, parse_function_call_expression],
	];
	for (const [token, bp, handler] of leds) {
		bp_lookup.set(token, bp);
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
		bp_lookup.set(token, BindingPower.Primary);
		nud_lookup.set(token, handler);
	}

	return {
		nud_lookup,
		led_lookup,
		bp_lookup,
	};
};

const lookups = create_expression_lookups();
