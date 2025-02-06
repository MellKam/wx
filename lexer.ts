export const TokenKind = {
	IntLiteral: 1, // 123
	FloatLiteral: 2, // 123.456
	BooleanLiteral: 3, // true or false
	Identifier: 4, // abc
	LetKeyword: 5, // let
	FnKeyword: 6, // fn
	ReturnKeyword: 7, // return
	Semicolon: 8, // ;
	Colon: 9, // :
	Plus: 10, // +
	Dash: 11, // -
	Slash: 12, // /
	Star: 13, // *
	Percent: 14, // %
	OpenParen: 15, // (
	CloseParen: 16, // )
	Assign: 17, // =
	Equal: 18, // ==
	Exclamation: 19, // !
	NotEqual: 20, // !=
	LessThan: 21, // <
	LessThanOrEqual: 22, // <=
	GreaterThan: 23, // >
	GreaterThanOrEqual: 24, // >=
	OpenCurely: 25, // {
	CloseCurely: 26, // }
	And: 27, // &&
	Or: 28, // ||
	Comma: 29, // ,
} as const;
export type TokenKind = (typeof TokenKind)[keyof typeof TokenKind];

export const TokenNameByKind = Object.fromEntries(
	Object.entries(TokenKind).map(([key, value]) => [value, key])
) as Record<TokenKind, keyof typeof TokenKind>;

export interface Token {
	kind: TokenKind;
	value: string;
	start: number;
	end: number;
}

export class Lexer {
	readonly patterns: Map<RegExp, TokenKind | null>;

	constructor() {
		this.patterns = new Map([
			[/^\s+/, null],
			[/^\d+\.\d+/, TokenKind.FloatLiteral],
			[/^\d+/, TokenKind.IntLiteral],
			[/^let/, TokenKind.LetKeyword],
			[/^fn/, TokenKind.FnKeyword],
			[/^return/, TokenKind.ReturnKeyword],
			[/^true|false/, TokenKind.BooleanLiteral],
			[/^[a-zA-Z_]\w*/, TokenKind.Identifier],
			[/^;/, TokenKind.Semicolon],
			[/^:/, TokenKind.Colon],
			[/^let/, TokenKind.LetKeyword],
			[/^==/, TokenKind.Equal],
			[/^!=/, TokenKind.NotEqual],
			[/^!/, TokenKind.Exclamation],
			[/^&&/, TokenKind.And],
			[/^\|\|/, TokenKind.Or],
			[/^<=/, TokenKind.LessThanOrEqual],
			[/^>=/, TokenKind.GreaterThanOrEqual],
			[/^</, TokenKind.LessThan],
			[/^>/, TokenKind.GreaterThan],
			[/^=/, TokenKind.Assign],
			[/^\+/, TokenKind.Plus],
			[/^-/, TokenKind.Dash],
			[/^,/, TokenKind.Comma],
			[/^\//, TokenKind.Slash],
			[/^\*/, TokenKind.Star],
			[/^%/, TokenKind.Percent],
			[/^\(/, TokenKind.OpenParen],
			[/^\)/, TokenKind.CloseParen],
			[/^{/, TokenKind.OpenCurely],
			[/^}/, TokenKind.CloseCurely],
		]);
	}

	tokenize(source: string): Token[] {
		const tokens: Token[] = [];
		let position = 0;

		while (position < source.length) {
			let found_match = false;

			for (const [regex, token_kind] of this.patterns) {
				const match = source.slice(position).match(regex);

				if (match) {
					const matched = match[0];

					if (token_kind) {
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
				throw new Error(`Unexpected token at position ${position}`);
			}
		}

		return tokens;
	}
}
