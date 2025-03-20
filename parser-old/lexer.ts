export const TokenKind = Object.freeze({
	/** @example 123 */
	IntLiteral: 1,
	/** @example 123.456 */
	FloatLiteral: 2,
	/** @example true */
	BooleanLiteral: 3,
	/** @example "abc" */
	StringLiteral: 4,
	/** @example let */
	LetKeyword: 5,
	/** @example fn */
	FnKeyword: 6,
	/** @example return */
	ReturnKeyword: 7,
	/** @example export */
	ExportKeyword: 8,
	/** @example import */
	ImportKeyword: 9,
	/** @example foo */
	Identifier: 10,
	/** @example + */
	Plus: 11,
	/** @example - */
	Dash: 12,
	/** @example / */
	Slash: 13,
	/** @example * */
	Star: 14,
	/** @example % */
	Percent: 15,
	/** @example == */
	Equal: 16,
	/** @example ! */
	Exclamation: 17,
	/** @example != */
	NotEqual: 18,
	/** @example < */
	LessThan: 19,
	/** @example <= */
	LessThanOrEqual: 20,
	/** @example > */
	GreaterThan: 21,
	/** @example >= */
	GreaterThanOrEqual: 22,
	/** @example && */
	And: 23,
	/** @example || */
	Or: 24,
	/** @example ( */
	OpenParen: 25,
	/** @example ) */
	CloseParen: 26,
	/** @example { */
	OpenCurely: 27,
	/** @example } */
	CloseCurely: 28,
	/** @example ; */
	Semicolon: 29,
	/** @example : */
	Colon: 30,
	/** @example , */
	Comma: 31,
	/** @example = */
	Assign: 32,
});
export type TokenKind = (typeof TokenKind)[keyof typeof TokenKind];

export const TokenName = Object.freeze(
	Object.fromEntries(
		Object.entries(TokenKind).map(([key, value]) => [value, key])
	) as unknown as {
		[K in keyof typeof TokenKind as (typeof TokenKind)[K]]: K;
	}
);

export interface Token {
	kind: TokenKind;
	value: string;
	start: number;
	end: number;
}

const TokenPatterns: Record<TokenKind, RegExp> = {
	[TokenKind.IntLiteral]: /^\d+/,
	[TokenKind.FloatLiteral]: /^\d+\.\d+/,
	[TokenKind.BooleanLiteral]: /^true|false/,
	[TokenKind.StringLiteral]: /^"([^\\\"]|\\.)*\"/,
	[TokenKind.Identifier]: /^[a-zA-Z_]\w*/,
	[TokenKind.Plus]: /^\+/,
	[TokenKind.Dash]: /^-/,
	[TokenKind.Slash]: /^\//,
	[TokenKind.Star]: /^\*/,
	[TokenKind.Percent]: /^%/,
	[TokenKind.Equal]: /^==/,
	[TokenKind.Exclamation]: /^!/,
	[TokenKind.NotEqual]: /^!=/,
	[TokenKind.LessThan]: /^</,
	[TokenKind.LessThanOrEqual]: /^<=/,
	[TokenKind.GreaterThan]: /^>/,
	[TokenKind.GreaterThanOrEqual]: /^>=/,
	[TokenKind.And]: /^&&/,
	[TokenKind.Or]: /^\|\|/,
	[TokenKind.OpenParen]: /^\(/,
	[TokenKind.CloseParen]: /^\)/,
	[TokenKind.OpenCurely]: /^{/,
	[TokenKind.CloseCurely]: /^}/,
	[TokenKind.Semicolon]: /^;/,
	[TokenKind.Colon]: /^:/,
	[TokenKind.Comma]: /^,/,
	[TokenKind.Assign]: /^=/,
	[TokenKind.LetKeyword]: /^let/,
	[TokenKind.FnKeyword]: /^fn/,
	[TokenKind.ReturnKeyword]: /^return/,
	[TokenKind.ExportKeyword]: /^export/,
	[TokenKind.ImportKeyword]: /^import/,
};

export class Lexer {
	readonly patterns: RegExp[];

	constructor(patterns: RegExp[]) {
		this.patterns = patterns;
	}

	static create(): Lexer {
		return new Lexer([
			/^\s+/,
			...Object.entries(TokenPatterns)
				.sort(([kindA], [kindB]) => parseInt(kindA) - parseInt(kindB))
				.map(([_, regex]) => regex),
		]);
	}

	tokenize(source: string): Token[] {
		const tokens: Token[] = [];
		let position = 0;

		while (position < source.length) {
			let found_match = false;

			for (let i = 0; i < this.patterns.length; i++) {
				const pattern = this.patterns[i]!;
				const match = source.slice(position).match(pattern);

				if (match) {
					const matched = match[0];
					const token_kind = i === 0 ? null : (i as TokenKind);

					if (token_kind !== null) {
						tokens.push({
							kind: token_kind,
							start: position,
							end: position + matched.length,
							value: matched,
						});
					}

					position += matched.length;
					found_match = true;
					break;
				}
			}

			if (!found_match) {
				const line = source.slice(0, position).match(/\n/g)?.length ?? 0;
				const column = position - source.lastIndexOf("\n", position);
				throw new Error(
					`Unknown token at line ${line + 1}, column ${column}\n${
						source.split("\n")[line]
					}`
				);
			}
		}

		return tokens;
	}
}

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
