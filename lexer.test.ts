import { Lexer, type Token, TokenKind } from "./lexer.ts";
import { assertEquals } from "jsr:@std/assert";
import { describe, it } from "jsr:@std/testing/bdd";

describe("Lexer", () => {
  const lexer = Lexer.create();

  it("should tokenize simple numeric plus expression", () => {
    assertEquals(lexer.tokenize("1+2"), [
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "1",
        start: 0,
        end: 1,
      },
      <Token> { kind: TokenKind.Plus, value: "+", start: 1, end: 2 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "2",
        start: 2,
        end: 3,
      },
    ]);
  });

  it("should tokenize the expression with spaces", () => {
    assertEquals(lexer.tokenize("1 + 2"), [
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "1",
        start: 0,
        end: 1,
      },
      <Token> { kind: TokenKind.Plus, value: "+", start: 2, end: 3 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "2",
        start: 4,
        end: 5,
      },
    ]);
  });

  it("should tokenize the expression with parentheses", () => {
    assertEquals(lexer.tokenize("(1 + 2) + 3"), [
      <Token> { kind: TokenKind.OpenParen, value: "(", start: 0, end: 1 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "1",
        start: 1,
        end: 2,
      },
      <Token> { kind: TokenKind.Plus, value: "+", start: 3, end: 4 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "2",
        start: 5,
        end: 6,
      },
      <Token> {
        kind: TokenKind.CloseParen,
        value: ")",
        start: 6,
        end: 7,
      },
      <Token> { kind: TokenKind.Plus, value: "+", start: 8, end: 9 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "3",
        start: 10,
        end: 11,
      },
    ]);
  });

  it("should tokenize dash token", () => {
    assertEquals(lexer.tokenize("-1 - 2"), [
      <Token> { kind: TokenKind.Dash, value: "-", start: 0, end: 1 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "1",
        start: 1,
        end: 2,
      },
      <Token> { kind: TokenKind.Dash, value: "-", start: 3, end: 4 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "2",
        start: 5,
        end: 6,
      },
    ]);
  });

  it("should tokenize star token", () => {
    assertEquals(lexer.tokenize("6 * 3"), [
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "6",
        start: 0,
        end: 1,
      },
      <Token> { kind: TokenKind.Star, value: "*", start: 2, end: 3 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "3",
        start: 4,
        end: 5,
      },
    ]);
  });

  it("should tokenize slash token", () => {
    assertEquals(lexer.tokenize("6 / 3"), [
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "6",
        start: 0,
        end: 1,
      },
      <Token> { kind: TokenKind.Slash, value: "/", start: 2, end: 3 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "3",
        start: 4,
        end: 5,
      },
    ]);
  });

  it("should tokenize percent token", () => {
    assertEquals(lexer.tokenize("6 % 3"), [
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "6",
        start: 0,
        end: 1,
      },
      <Token> { kind: TokenKind.Percent, value: "%", start: 2, end: 3 },
      <Token> {
        kind: TokenKind.IntLiteral,
        value: "3",
        start: 4,
        end: 5,
      },
    ]);
  });

  it("should tokenize float literals", () => {
    assertEquals(lexer.tokenize("1.0 + 2.05"), [
      <Token> { kind: TokenKind.FloatLiteral, value: "1.0", start: 0, end: 3 },
      <Token> { kind: TokenKind.Plus, value: "+", start: 4, end: 5 },
      <Token> {
        kind: TokenKind.FloatLiteral,
        value: "2.05",
        start: 6,
        end: 10,
      },
    ]);
  });

  it("should tokenize identifiers", () => {
    assertEquals(lexer.tokenize("abc"), [
      <Token> { kind: TokenKind.Identifier, value: "abc", start: 0, end: 3 },
    ]);
    assertEquals(lexer.tokenize("abc_123"), [
      <Token> {
        kind: TokenKind.Identifier,
        value: "abc_123",
        start: 0,
        end: 7,
      },
    ]);
    assertEquals(lexer.tokenize("UPPERcase_09"), [
      <Token> {
        kind: TokenKind.Identifier,
        value: "UPPERcase_09",
        start: 0,
        end: 12,
      },
    ]);
  });

  it("should tokenize curly braces", () => {
    assertEquals(lexer.tokenize("{"), [
      <Token> { kind: TokenKind.OpenCurely, value: "{", start: 0, end: 1 },
    ]);
    assertEquals(lexer.tokenize("}"), [
      <Token> { kind: TokenKind.CloseCurely, value: "}", start: 0, end: 1 },
    ]);
    assertEquals(lexer.tokenize("{ }"), [
      <Token> { kind: TokenKind.OpenCurely, value: "{", start: 0, end: 1 },
      <Token> { kind: TokenKind.CloseCurely, value: "}", start: 2, end: 3 },
    ]);
  });

  it("should tokenize logical operators", () => {
    assertEquals(lexer.tokenize("&&"), [
      <Token> { kind: TokenKind.And, value: "&&", start: 0, end: 2 },
    ]);
    assertEquals(lexer.tokenize("||"), [
      <Token> { kind: TokenKind.Or, value: "||", start: 0, end: 2 },
    ]);
  });

  it("should tokenize comparison operators", () => {
    assertEquals(lexer.tokenize("=="), [
      <Token> { kind: TokenKind.Equal, value: "==", start: 0, end: 2 },
    ]);
    assertEquals(lexer.tokenize("!="), [
      <Token> { kind: TokenKind.NotEqual, value: "!=", start: 0, end: 2 },
    ]);
    assertEquals(lexer.tokenize("<"), [
      <Token> { kind: TokenKind.LessThan, value: "<", start: 0, end: 1 },
    ]);
    assertEquals(lexer.tokenize("<="), [
      <Token> {
        kind: TokenKind.LessThanOrEqual,
        value: "<=",
        start: 0,
        end: 2,
      },
    ]);
    assertEquals(lexer.tokenize(">"), [
      <Token> { kind: TokenKind.GreaterThan, value: ">", start: 0, end: 1 },
    ]);
    assertEquals(lexer.tokenize(">="), [
      <Token> {
        kind: TokenKind.GreaterThanOrEqual,
        value: ">=",
        start: 0,
        end: 2,
      },
    ]);
  });

  it("should correctly tokenize variable declaration statement", () => {
    assertEquals(lexer.tokenize("let a = 1;"), [
      <Token> { kind: TokenKind.LetKeyword, value: "let", start: 0, end: 3 },
      <Token> { kind: TokenKind.Identifier, value: "a", start: 4, end: 5 },
      <Token> { kind: TokenKind.Assign, value: "=", start: 6, end: 7 },
      <Token> { kind: TokenKind.IntLiteral, value: "1", start: 8, end: 9 },
      <Token> { kind: TokenKind.Semicolon, value: ";", start: 9, end: 10 },
    ]);
  });
});
