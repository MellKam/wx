import type { Expression } from "./expressions.ts";
import type { Token, TokenIterator, TokenKind } from "./lexer.ts";

const enum PatternKind {
	Token,
	Expression,
	Union,
	Sequence,
	Terminal,
}

interface TokenPattern {
	kind: PatternKind.Token;
	token: TokenKind;
}

interface ExpressionPattern {
	kind: PatternKind.Expression;
}

interface UnionPattern {
	kind: PatternKind.Union;
	patterns: Pattern[][];
}

interface SequencePattern {
	kind: PatternKind.Sequence;
	item: Pattern[];
	separator: TokenKind;
}

interface TerminalPattern<T = unknown> {
	kind: PatternKind.Terminal;
	callback: (matched: PatternResult[]) => T;
}

type Pattern =
	| TokenPattern
	| ExpressionPattern
	| UnionPattern
	| SequencePattern
	| TerminalPattern;

interface TokenPatternResult {
	kind: PatternKind.Token;
	token: Token;
}

interface ExpressionPatternResult {
	kind: PatternKind.Expression;
	expression: Expression;
}

interface SequencePatternResult {
	kind: PatternKind.Sequence;
	items: PatternResult[][];
}

interface TerminalPatternResult<T = unknown> {
	kind: PatternKind.Terminal;
	result: T;
}

type PatternResult =
	| TokenPatternResult
	| ExpressionPatternResult
	| SequencePatternResult
	| TerminalPatternResult;

const wx = {
	token: (kind: TokenKind): TokenPattern => {
		return { kind: PatternKind.Token, token: kind };
	},
	expression: (): ExpressionPattern => {
		return { kind: PatternKind.Expression };
	},
	union: (patterns: Pattern[][]): UnionPattern => {
		return { kind: PatternKind.Union, patterns };
	},
	sequence: (item: Pattern[], separator: TokenKind): SequencePattern => {
		return { kind: PatternKind.Sequence, item, separator };
	},
	terminal: <T>(cb: (matched: PatternResult[]) => T): TerminalPattern<T> => {
		return { kind: PatternKind.Terminal, callback: cb };
	},
};

const parse_token_pattern = (
	pattern: TokenPattern,
	tokens: TokenIterator
): TokenPatternResult => {
	const token = tokens.advance();
	if (!token || token.kind !== pattern.token) {
		throw new Error(`Expected token ${pattern.token}`);
	}
	return { kind: PatternKind.Token, token };
};

const parse_expression_pattern = (
	_pattern: ExpressionPattern,
	tokens: TokenIterator
): ExpressionPatternResult => {
	return {
		kind: PatternKind.Expression,
		expression: parse_expression(tokens, BindingPower.Default),
	};
};

const parse_union_pattern = (
	pattern: UnionPattern,
	tokens: TokenIterator
): PatternResult[] => {
	const patterns = pattern.patterns;
	const current_position = tokens.position;

	for (const pattern of patterns) {
		tokens.position = current_position;

		try {
			const result = pattern.map((pattern) => {
				const results = _parse_pattern(pattern, tokens);
				if (results.kind === PatternKind.Sequence) {
					return results.items;
				} else {
					return [results];
				}
			});

			return [{ kind: PatternKind.Sequence, items: result.flat() }];
		} catch (error) {
			continue;
		}
	}

	throw new Error("No union pattern found");
};

const parse_sequence_pattern = (
	pattern: SequencePattern,
	tokens: TokenIterator,
	matched: PatternResult[]
): SequencePatternResult => {
	const items: PatternResult[][] = [];

	while (true) {
		const item = _parse_pattern(pattern.item, tokens, matched);
		items.push(item);

		const separator = tokens.advance();
		if (!separator || separator.kind !== pattern.separator) {
			break;
		}
	}
};

const _parse_pattern = (
	pattern: Pattern,
	tokens: TokenIterator,
	matched: PatternResult[]
): PatternResult[] => {
	switch (pattern.kind) {
		case PatternKind.Token: {
			return [parse_token_pattern(pattern, tokens)];
		}
		case PatternKind.Expression: {
			return [parse_expression_pattern(pattern, tokens)];
		}
		case PatternKind.Union: {
			return parse_union_pattern(pattern, tokens);
		}
		case PatternKind.Sequence: {
			return;
		}
		case PatternKind.Terminal: {
			return [
				{ kind: PatternKind.Terminal, result: pattern.callback(matched) },
			];
		}
	}
};

const parse_pattern = <T>(patterns: Pattern[], tokens: TokenIterator): T => {
	const matched: PatternResult[] = [];

	for (const pattern of patterns) {
		matched.push(..._parse_pattern(pattern, tokens, matched));
	}

	const terminal = matched.at(-1);
	if (!terminal || terminal.kind !== PatternKind.Terminal) {
		throw new Error("No terminal pattern found");
	}

	return terminal.result as T;
};

const result = parse_pattern([
	wx.token(TokenKind.LetKeyword),
	wx.token(TokenKind.Identifier),
	wx.token(TokenKind.Colon),
	wx.token(TokenKind.Identifier),
	wx.union([
		[
			wx.token(TokenKind.Assign),
			wx.expression(),
			wx.token(TokenKind.Semicolon),
			wx.terminal((matched) => {
				const [let_keyword, id, colon, type, assign, expr, semicolon] = matched;
				return { kind: "definition" };
			}),
		],
		[
			wx.token(TokenKind.Semicolon),
			wx.terminal(() => {
				return { kind: "declaration" };
			}),
		],
	]),
]);
