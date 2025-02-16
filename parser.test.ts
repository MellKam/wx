import { ExpressionKind, Parser, StatementKind } from "./parser.ts";
import type {
  AssignmentExpression,
  BinaryExpression,
  ExpressionStatement,
  FloatExpression,
  FunctionDefinitionStatement,
  IdentifierExpression,
  ReturnValueDefinitionStatement,
  UnaryExpression,
  VariableDefinitionStatement,
} from "./parser.ts";
import { assertEquals } from "jsr:@std/assert";
import { describe, it } from "jsr:@std/testing/bdd";
import { type Token, TokenKind } from "./lexer.ts";

describe("Parser", () => {
  const parser = Parser.create();

  it("should parse simple expression", () => {
    // "1 + 2"
    const tokens: Token[] = [
      {
        kind: TokenKind.IntLiteral,
        value: "1",
        start: 0,
        end: 1,
      },
      { kind: TokenKind.Plus, value: "+", start: 2, end: 3 },
      {
        kind: TokenKind.IntLiteral,
        value: "2",
        start: 4,
        end: 5,
      },
    ];

    assertEquals(
      parser.parse_expression(tokens),
      <BinaryExpression> {
        kind: ExpressionKind.Binary,
        start: 0,
        end: 5,
        left: <FloatExpression> {
          kind: ExpressionKind.Int,
          start: 0,
          end: 1,
          value: 1,
        },
        operator: TokenKind.Plus,
        right: <FloatExpression> {
          kind: ExpressionKind.Int,
          start: 4,
          end: 5,
          value: 2,
        },
      },
    );
  });

  it("should parse the expression with parentheses", () => {
    // "(1 + 2)"
    const tokens: Token[] = [
      { kind: TokenKind.OpenParen, value: "(", start: 0, end: 1 },
      { kind: TokenKind.IntLiteral, value: "1", start: 1, end: 2 },
      { kind: TokenKind.Plus, value: "+", start: 3, end: 4 },
      { kind: TokenKind.IntLiteral, value: "2", start: 5, end: 6 },
      { kind: TokenKind.CloseParen, value: ")", start: 6, end: 7 },
    ];

    assertEquals(
      parser.parse_expression(tokens),
      <BinaryExpression> {
        kind: ExpressionKind.Binary,
        start: 1,
        end: 6,
        left: <FloatExpression> {
          kind: ExpressionKind.Int,
          start: 1,
          end: 2,
          value: 1,
        },
        operator: TokenKind.Plus,
        right: <FloatExpression> {
          kind: ExpressionKind.Int,
          start: 5,
          end: 6,
          value: 2,
        },
      },
    );
  });

  it("should prioritize multiplicative operators", () => {
    // "1 * 2 + 3"
    const tokens: Token[] = [
      { kind: TokenKind.IntLiteral, value: "1", start: 0, end: 1 },
      { kind: TokenKind.Star, value: "*", start: 2, end: 3 },
      { kind: TokenKind.IntLiteral, value: "2", start: 4, end: 5 },
      { kind: TokenKind.Plus, value: "+", start: 6, end: 7 },
      { kind: TokenKind.IntLiteral, value: "3", start: 8, end: 9 },
    ];

    assertEquals(
      parser.parse_expression(tokens),
      <BinaryExpression> {
        kind: ExpressionKind.Binary,
        start: 0,
        end: 9,
        left: <BinaryExpression> {
          kind: ExpressionKind.Binary,
          start: 0,
          end: 5,
          left: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 0,
            end: 1,
            value: 1,
          },
          operator: TokenKind.Star,
          right: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 4,
            end: 5,
            value: 2,
          },
        },
        operator: TokenKind.Plus,
        right: <FloatExpression> {
          kind: ExpressionKind.Int,
          start: 8,
          end: 9,
          value: 3,
        },
      },
    );
  });

  it("should handle unary minus", () => {
    // "-1"
    const tokens: Token[] = [
      { kind: TokenKind.Dash, value: "-", start: 0, end: 1 },
      { kind: TokenKind.IntLiteral, value: "1", start: 1, end: 2 },
    ];

    assertEquals(
      parser.parse_expression(tokens),
      <UnaryExpression> {
        kind: ExpressionKind.Unary,
        start: 0,
        end: 2,
        operator: TokenKind.Dash,
        right: <FloatExpression> {
          kind: ExpressionKind.Int,
          start: 1,
          end: 2,
          value: 1,
        },
      },
    );
  });

  it("should correctly parse complex expression", () => {
    // "5 * -2 + 8 / 4"
    const tokens: Token[] = [
      { kind: TokenKind.IntLiteral, value: "5", start: 0, end: 1 },
      { kind: TokenKind.Star, value: "*", start: 2, end: 3 },
      { kind: TokenKind.Dash, value: "-", start: 4, end: 5 },
      { kind: TokenKind.IntLiteral, value: "2", start: 5, end: 6 },
      { kind: TokenKind.Plus, value: "+", start: 7, end: 8 },
      { kind: TokenKind.IntLiteral, value: "8", start: 9, end: 10 },
      { kind: TokenKind.Slash, value: "/", start: 11, end: 12 },
      { kind: TokenKind.IntLiteral, value: "4", start: 13, end: 14 },
    ];

    assertEquals(
      parser.parse_expression(tokens),
      <BinaryExpression> {
        kind: ExpressionKind.Binary,
        start: 0,
        end: 14,
        left: <BinaryExpression> {
          kind: ExpressionKind.Binary,
          start: 0,
          end: 6,
          left: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 0,
            end: 1,
            value: 5,
          },
          operator: TokenKind.Star,
          right: <UnaryExpression> {
            kind: ExpressionKind.Unary,
            start: 4,
            end: 6,
            operator: TokenKind.Dash,
            right: <FloatExpression> {
              kind: ExpressionKind.Int,
              start: 5,
              end: 6,
              value: 2,
            },
          },
        },
        operator: TokenKind.Plus,
        right: <BinaryExpression> {
          kind: ExpressionKind.Binary,
          start: 9,
          end: 14,
          left: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 9,
            end: 10,
            value: 8,
          },
          operator: TokenKind.Slash,
          right: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 13,
            end: 14,
            value: 4,
          },
        },
      },
    );
  });

  it("should correctly parse variable declaration statement", () => {
    // "let x: i32 = 5;"
    const tokens: Token[] = [
      { kind: TokenKind.LetKeyword, value: "let", start: 0, end: 3 },
      { kind: TokenKind.Identifier, value: "x", start: 4, end: 5 },
      { kind: TokenKind.Colon, value: ":", start: 6, end: 7 },
      { kind: TokenKind.Identifier, value: "i32", start: 8, end: 11 },
      { kind: TokenKind.Assign, value: "=", start: 12, end: 13 },
      { kind: TokenKind.IntLiteral, value: "5", start: 14, end: 15 },
      { kind: TokenKind.Semicolon, value: ";", start: 15, end: 16 },
    ];

    assertEquals(parser.parse(tokens), [
      <VariableDefinitionStatement> {
        kind: StatementKind.VariableDefinition,
        identifier: "x",
        type: "i32",
        value: <FloatExpression> {
          kind: ExpressionKind.Int,
          start: 14,
          end: 15,
          value: 5,
        },
        start: 0,
        end: 16,
      },
    ]);
  });

  it("should correctly parse expression statement", () => {
    // "5 + 3;"
    const tokens: Token[] = [
      { kind: TokenKind.IntLiteral, value: "5", start: 0, end: 1 },
      { kind: TokenKind.Plus, value: "+", start: 2, end: 3 },
      { kind: TokenKind.IntLiteral, value: "3", start: 4, end: 5 },
      { kind: TokenKind.Semicolon, value: ";", start: 5, end: 6 },
    ];

    assertEquals(parser.parse(tokens), [
      <ExpressionStatement> {
        kind: StatementKind.Expression,
        expression: <BinaryExpression> {
          kind: ExpressionKind.Binary,
          start: 0,
          end: 5,
          left: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 0,
            end: 1,
            value: 5,
          },
          operator: TokenKind.Plus,
          right: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 4,
            end: 5,
            value: 3,
          },
        },
        start: 0,
        end: 6,
      },
    ]);
  });

  it("should handle identifier expression", () => {
    // "x;"
    const tokens: Token[] = [
      { kind: TokenKind.Identifier, value: "x", start: 0, end: 1 },
    ];

    assertEquals(
      parser.parse_expression(tokens),
      <IdentifierExpression> {
        kind: ExpressionKind.Identifier,
        start: 0,
        end: 1,
        value: "x",
      },
    );
  });

  it("should correctly parse assignment statement", () => {
    // "x = 5;"
    const tokens: Token[] = [
      { kind: TokenKind.Identifier, value: "x", start: 0, end: 1 },
      { kind: TokenKind.Assign, value: "=", start: 2, end: 3 },
      { kind: TokenKind.IntLiteral, value: "5", start: 4, end: 5 },
      { kind: TokenKind.Semicolon, value: ";", start: 5, end: 6 },
    ];

    assertEquals(parser.parse(tokens), [
      <ExpressionStatement> {
        kind: StatementKind.Expression,
        expression: <AssignmentExpression> {
          kind: ExpressionKind.Assignment,
          identifier: "x",
          value: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 4,
            end: 5,
            value: 5,
          },
          start: 0,
          end: 5,
        },
        start: 0,
        end: 6,
      },
    ]);
  });

  it("should correctly handle expression with identifiers", () => {
    // "x + 5;"
    const tokens: Token[] = [
      { kind: TokenKind.Identifier, value: "x", start: 0, end: 1 },
      { kind: TokenKind.Plus, value: "+", start: 2, end: 3 },
      { kind: TokenKind.IntLiteral, value: "5", start: 4, end: 5 },
      { kind: TokenKind.Semicolon, value: ";", start: 5, end: 6 },
    ];

    assertEquals(parser.parse(tokens), [
      <ExpressionStatement> {
        kind: StatementKind.Expression,
        expression: <BinaryExpression> {
          kind: ExpressionKind.Binary,
          start: 0,
          end: 5,
          left: <IdentifierExpression> {
            kind: ExpressionKind.Identifier,
            start: 0,
            end: 1,
            value: "x",
          },
          operator: TokenKind.Plus,
          right: <FloatExpression> {
            kind: ExpressionKind.Int,
            start: 4,
            end: 5,
            value: 5,
          },
        },
        start: 0,
        end: 6,
      },
    ]);
  });

  it("should handle function declaration", () => {
    // "fn add(x: i32, y: i32): i32 { return x + y; }"
    const tokens: Token[] = [
      { kind: TokenKind.FnKeyword, value: "fn", start: 0, end: 2 },
      { kind: TokenKind.Identifier, value: "add", start: 3, end: 6 },
      { kind: TokenKind.OpenParen, value: "(", start: 6, end: 7 },
      { kind: TokenKind.Identifier, value: "x", start: 7, end: 8 },
      { kind: TokenKind.Colon, value: ":", start: 9, end: 10 },
      { kind: TokenKind.Identifier, value: "i32", start: 11, end: 14 },
      { kind: TokenKind.Comma, value: ",", start: 14, end: 15 },
      { kind: TokenKind.Identifier, value: "y", start: 16, end: 17 },
      { kind: TokenKind.Colon, value: ":", start: 18, end: 19 },
      { kind: TokenKind.Identifier, value: "i32", start: 20, end: 23 },
      { kind: TokenKind.CloseParen, value: ")", start: 23, end: 24 },
      { kind: TokenKind.Colon, value: ":", start: 25, end: 26 },
      { kind: TokenKind.Identifier, value: "i32", start: 27, end: 30 },
      { kind: TokenKind.OpenCurely, value: "{", start: 31, end: 32 },
      { kind: TokenKind.ReturnKeyword, value: "return", start: 33, end: 39 },
      { kind: TokenKind.Identifier, value: "x", start: 40, end: 41 },
      { kind: TokenKind.Plus, value: "+", start: 42, end: 43 },
      { kind: TokenKind.Identifier, value: "y", start: 44, end: 45 },
      { kind: TokenKind.Semicolon, value: ";", start: 45, end: 46 },
      { kind: TokenKind.CloseCurely, value: "}", start: 47, end: 48 },
    ];

    assertEquals(parser.parse(tokens), [
      <FunctionDefinitionStatement> {
        kind: StatementKind.FunctionDefinition,
        name: "add",
        args: [
          ["x", "i32"],
          ["y", "i32"],
        ],
        result: "i32",
        body: [
          <ReturnValueDefinitionStatement> {
            kind: StatementKind.ReturnValueDefinition,
            value: <BinaryExpression> {
              kind: ExpressionKind.Binary,
              left: <IdentifierExpression> {
                kind: ExpressionKind.Identifier,
                start: 40,
                end: 41,
                value: "x",
              },
              operator: TokenKind.Plus,
              right: <IdentifierExpression> {
                kind: ExpressionKind.Identifier,
                start: 44,
                end: 45,
                value: "y",
              },
              start: 40,
              end: 45,
            },
            start: 33,
            end: 46,
          },
        ],
        start: 0,
        end: 48,
      },
    ]);
  });
});
