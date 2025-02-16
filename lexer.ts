export const TokenKind = {
  // Literals
  IntLiteral: 1, // 123
  FloatLiteral: 2, // 123.456
  BooleanLiteral: 3, // true or false
  StringLiteral: 4, // "abc"

  // Identifiers
  Identifier: 5, // abc

  // Operators
  Plus: 6, // +
  Dash: 7, // -
  Slash: 8, // /
  Star: 9, // *
  Percent: 10, // %

  // Comparison Operators
  Equal: 11, // ==
  Exclamation: 12, // !
  NotEqual: 13, // !=
  LessThan: 14, // <
  LessThanOrEqual: 15, // <=
  GreaterThan: 16, // >
  GreaterThanOrEqual: 17, // >=

  // Logical Operators
  And: 18, // &&
  Or: 19, // ||

  // Parentheses & Braces
  OpenParen: 20, // (
  CloseParen: 21, // )
  OpenCurely: 22, // {
  CloseCurely: 23, // }

  // Punctuation
  Semicolon: 24, // ;
  Colon: 25, // :
  Comma: 26, // ,

  // Assignment
  Assign: 27, // =

  // Keywords
  LetKeyword: 28, // let
  FnKeyword: 29, // fn
  ReturnKeyword: 30, // return
  ExportKeyword: 31, // export
  ExternKeyword: 32, // extern
  ModuleKeyword: 33, // module
} as const;
export type TokenKind = (typeof TokenKind)[keyof typeof TokenKind];

export const TokenNameByKind = Object.fromEntries(
  Object.entries(TokenKind).map(([key, value]) => [value, key]),
) as Record<TokenKind, keyof typeof TokenKind>;

export interface Token {
  kind: TokenKind;
  value: string;
  start: number;
  end: number;
}

export class Lexer {
  readonly patterns: Map<RegExp, TokenKind | null>;

  constructor(patterns: Map<RegExp, TokenKind | null>) {
    this.patterns = patterns;
  }

  static create(): Lexer {
    const patterns = new Map<RegExp, TokenKind | null>([
      [/^\s+/, null],
      [/^\d+\.\d+/, TokenKind.FloatLiteral],
      [/^\d+/, TokenKind.IntLiteral],
      [/^[a-zA-Z_]\w*/, TokenKind.Identifier],
      [/^let/, TokenKind.LetKeyword],
      [/^fn/, TokenKind.FnKeyword],
      [/^return/, TokenKind.ReturnKeyword],
      [/^export/, TokenKind.ExportKeyword],
      [/^extern/, TokenKind.ExternKeyword],
      [/^true|false/, TokenKind.BooleanLiteral],
      [/^"(?:[^"\\]|\\.)*"/, TokenKind.StringLiteral],
      [/^module/, TokenKind.ModuleKeyword],
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
    return new Lexer(patterns);
  }

  tokenize(source: string): Token[] {
    const tokens: Token[] = [];
    let position = 0;

    while (position < source.length) {
      let found_match = false;

      for (const [regex, token_kind] of this.patterns.entries()) {
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
