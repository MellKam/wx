import type { Token, TokenKind } from "./lexer.ts";

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

export interface IntExpression extends Expression {
  kind: typeof ExpressionKind.Int;
  value: number;
  start: number;
  end: number;
}

export interface FloatExpression extends Expression {
  kind: typeof ExpressionKind.Int;
  value: number;
  start: number;
  end: number;
}
export interface IdentifierExpression extends Expression {
  kind: typeof ExpressionKind.Identifier;
  value: string;
  start: number;
  end: number;
}

export interface UnaryExpression extends Expression {
  kind: typeof ExpressionKind.Unary;
  operator: TokenKind;
  right: Expression;
  start: number;
  end: number;
}

export interface BinaryExpression extends Expression {
  kind: typeof ExpressionKind.Binary;
  left: Expression;
  operator: TokenKind;
  right: Expression;
  start: number;
  end: number;
}

export const StatementKind = {
  Expression: 0, // add(1, 2);
  VariableDefinition: 1, // let x: i32 = 1;
  VariableDeclaration: 2, // let x: i32;
  FunctionDefinition: 3, // fn add(x: i32, y: i32): i32 { ... }
  ReturnValueDefinition: 4, // return 1;
  FunctionDeclaration: 5, // fn add(x: i32, y: i32): i32;
  Extern: 6, // extern fn log(num: i32);
  ExportDefinition: 7, // export fn add(x: i32, y: i32): i32 { ... }
} as const;
export type StatementKind = (typeof StatementKind)[keyof typeof StatementKind];

export interface Statement {
  kind: StatementKind;
  start: number;
  end: number;
}

export interface ExportDefinitionStatement extends Statement {
  kind: typeof StatementKind.ExportDefinition;
  definition: FunctionDefinitionStatement;
  start: number;
  end: number;
}

export interface ExpressionStatement extends Statement {
  kind: typeof StatementKind.Expression;
  expression: Expression;
  start: number;
  end: number;
}

export interface VariableDefinitionStatement extends Statement {
  kind: typeof StatementKind.VariableDefinition;
  id: Token;
  type: string;
  value: Expression;
  start: number;
  end: number;
}

export interface AssignmentExpression extends Expression {
  kind: typeof ExpressionKind.Assignment;
  left: IdentifierExpression;
  right: Expression;
  start: number;
  end: number;
}

export interface FunctionParameter {
  id: Token;
  type: Token;
}

export interface FunctionDefinitionStatement extends Statement {
  kind: typeof StatementKind.FunctionDefinition;
  id: Token;
  params: FunctionParameter[];
  result: string | null;
  body: Statement[];
  start: number;
  end: number;
}

export interface ReturnValueDefinitionStatement extends Statement {
  kind: typeof StatementKind.ReturnValueDefinition;
  value: Expression;
  start: number;
  end: number;
}

export interface FunctionDeclarationStatement extends Statement {
  kind: typeof StatementKind.FunctionDeclaration;
  id: Token;
  params: FunctionParameter[];
  result: Token | null;
  start: number;
  end: number;
}

export interface VariableDeclarationStatement extends Statement {
  kind: typeof StatementKind.VariableDeclaration;
  id: Token;
  type: Token;
  start: number;
  end: number;
}

export type DeclarationStatement =
  | FunctionDeclarationStatement
  | VariableDeclarationStatement;

export interface ExternDeclarationStatement extends Statement {
  kind: typeof StatementKind.ExternDeclaration;
  declaration: DeclarationStatement;
  start: number;
  end: number;
}

export interface Module {
  imports: ExternDeclarationStatement[];
  globals: VariableDefinitionStatement[];
  functions: FunctionDefinitionStatement[];
  exports: ExportDefinitionStatement[];
}
