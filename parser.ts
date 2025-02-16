import { type Token, TokenKind } from "./lexer.ts";
import type {
  AssignmentExpression,
  BinaryExpression,
  DeclarationStatement,
  ExportDefinitionStatement,
  Expression,
  ExpressionStatement,
  ExternDeclarationStatement,
  FloatExpression,
  FunctionDeclarationStatement,
  FunctionDefinitionStatement,
  FunctionParameter,
  IdentifierExpression,
  IntExpression,
  Module,
  ReturnValueDefinitionStatement,
  Statement,
  UnaryExpression,
  VariableDeclarationStatement,
  VariableDefinitionStatement,
} from "./ast.ts";
import { ExpressionKind, StatementKind } from "./ast.ts";

export const ScopeKind = {
  Global: 0,
  Function: 1,
} as const;
export type ScopeKind = (typeof ScopeKind)[keyof typeof ScopeKind];

export type NullDenotationHandler = (
  parser: Parser,
  tokens: TokenIterator,
) => Expression;
export type LeftDenotationHandler = (
  parser: Parser,
  tokens: TokenIterator,
  left: Expression,
  binding_power: BindingPower,
) => Expression;
export type StatementHandler = (
  parser: Parser,
  tokens: TokenIterator,
  scope: ScopeKind,
) => Statement;
export type DeclarationStatementHandler = (
  parser: Parser,
  tokens: TokenIterator,
) => DeclarationStatement;

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

export class Parser {
  readonly binding_power_lookup: Map<TokenKind, BindingPower>;
  readonly nud_lookup: Map<TokenKind, NullDenotationHandler>;
  readonly led_lookup: Map<TokenKind, LeftDenotationHandler>;
  readonly statement_lookup: Map<TokenKind, StatementHandler>;
  readonly declaration_lookup: Map<TokenKind, DeclarationStatementHandler>;

  constructor(
    binding_power_lookup: Map<TokenKind, BindingPower>,
    nud_lookup: Map<TokenKind, NullDenotationHandler>,
    led_lookup: Map<TokenKind, LeftDenotationHandler>,
    statement_lookup: Map<TokenKind, StatementHandler>,
    declaration_lookup: Map<TokenKind, DeclarationStatementHandler>,
  ) {
    this.binding_power_lookup = binding_power_lookup;
    this.nud_lookup = nud_lookup;
    this.led_lookup = led_lookup;
    this.statement_lookup = statement_lookup;
    this.declaration_lookup = declaration_lookup;
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
      [TokenKind.LetKeyword, parse_variable_definition_statement],
      [TokenKind.FnKeyword, parse_function_definition_statement],
      [TokenKind.ReturnKeyword, parse_return_value_definition_statement],
      [TokenKind.ExternKeyword, parse_extern_declaration_statement],
      [TokenKind.ExportKeyword, parse_export_definition_statement],
    ];
    for (const [token, handler] of statements) {
      binding_power_lookup.set(token, BindingPower.Default);
      statement_lookup.set(token, handler);
    }

    const declaration_lookup = new Map<TokenKind, DeclarationStatementHandler>([
      [TokenKind.LetKeyword, parse_variable_declaration_statement],
      [TokenKind.FnKeyword, parse_function_declaration_statement],
    ]);

    return new Parser(
      binding_power_lookup,
      nud_lookup,
      led_lookup,
      statement_lookup,
      declaration_lookup,
    );
  }

  parse(tokens: Token[]): Module {
    const token_iterator = new TokenIterator(tokens);
    const module: Module = {
      imports: [],
      globals: [],
      functions: [],
      exports: [],
    };

    while (true) {
      const token = token_iterator.current();
      if (!token) break;
      const statement_handler = this.statement_lookup.get(token.kind);
      if (!statement_handler) {
        throw new Error(
          `No statement handler found for token ${JSON.stringify(token)}`,
        );
      }

      const statement = statement_handler(
        this,
        token_iterator,
        ScopeKind.Global,
      );
      switch (statement.kind) {
        case StatementKind.ExternDeclaration:
          module.imports.push(statement as ExternDeclarationStatement);
          break;
        case StatementKind.VariableDefinition:
          module.globals.push(statement as VariableDefinitionStatement);
          break;
        case StatementKind.FunctionDefinition:
          module.functions.push(statement as FunctionDefinitionStatement);
          break;
        case StatementKind.ExportDefinition:
          module.exports.push(statement as ExportDefinitionStatement);
          break;
      }
    }

    return module;
  }

  _parse_expression(
    tokens: TokenIterator,
    min_binding_power: BindingPower,
  ): Expression {
    const token = tokens.current();
    if (!token) {
      throw new Error("No token found");
    }
    const nud_handler = this.nud_lookup.get(token.kind);
    if (!nud_handler) {
      throw new Error(
        `No NUD handler found for token ${JSON.stringify(token)}`,
      );
    }
    let left = nud_handler(this, tokens);

    while (true) {
      const token = tokens.current();
      if (!token) break;
      const operator_binding_power = this.binding_power_lookup.get(
        token.kind,
      );
      if (
        operator_binding_power === undefined ||
        operator_binding_power <= min_binding_power
      ) {
        break;
      }

      const led_handler = this.led_lookup.get(token.kind);
      if (!led_handler) {
        throw new Error("No LED handler found");
      }

      left = led_handler(this, tokens, left, operator_binding_power);
    }

    return left;
  }

  _parse_declaration(tokens: TokenIterator): DeclarationStatement {
    const token = tokens.current();
    if (!token) {
      throw new Error("No token found");
    }

    const declaration_handler = this.declaration_lookup.get(token.kind);
    if (!declaration_handler) {
      throw new Error(
        `No declaration handler found for token ${JSON.stringify(token)}`,
      );
    }

    return declaration_handler(this, tokens);
  }
}

const parse_export_definition_statement = (
  parser: Parser,
  tokens: TokenIterator,
  scope: ScopeKind,
): ExportDefinitionStatement => {
  if (scope !== ScopeKind.Global) {
    throw new Error("Export statements are only allowed in global scope");
  }

  const export_keyword = tokens.advance();
  if (!export_keyword || export_keyword.kind !== TokenKind.ExportKeyword) {
    throw new Error("Expected export keyword");
  }

  const definition = parse_function_definition_statement(parser, tokens, scope);

  return {
    kind: StatementKind.ExportDefinition,
    definition,
    start: export_keyword.start,
    end: definition.end,
  };
};

const parse_int_expression: NullDenotationHandler = (
  _parser,
  tokens,
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

const parse_float_expression: NullDenotationHandler = (
  _parser,
  tokens,
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
  _parser,
  tokens,
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
  parser,
  tokens,
): UnaryExpression => {
  const operator = tokens.advance();
  if (!operator) throw new Error("No token found");
  const right = parser._parse_expression(tokens, BindingPower.Primary);

  return {
    kind: ExpressionKind.Unary,
    operator: operator.kind,
    right,
    start: operator.start,
    end: right.end,
  };
};

const parse_grouping_expression: NullDenotationHandler = (
  parser,
  tokens,
) => {
  const _ = tokens.advance();
  const expression = parser._parse_expression(
    tokens,
    BindingPower.Default,
  );
  const token = tokens.advance();
  if (!token) throw new Error("No token found");
  if (token.kind === TokenKind.CloseParen) {
    return expression;
  } else {
    throw new Error("Expected closing parenthesis");
  }
};

const parse_binary_expression: LeftDenotationHandler = (
  parser,
  tokens,
  left,
  binding_power,
): BinaryExpression => {
  const operator = tokens.advance();
  if (!operator) throw new Error("No token found");
  const right = parser._parse_expression(tokens, binding_power);

  return {
    kind: ExpressionKind.Binary,
    left,
    operator: operator.kind,
    right,
    start: left.start,
    end: right.end,
  };
};

export const parser_expression_statement: StatementHandler = (
  parser,
  tokens,
  _scope,
): ExpressionStatement => {
  const expression = parser._parse_expression(tokens, BindingPower.Default);

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

const parse_variable_definition_statement: StatementHandler = (
  parser,
  tokens,
  _scope,
): VariableDefinitionStatement => {
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
  if (!equals || equals.kind !== TokenKind.Assign) {
    throw new Error("Expected equal sign after identifier");
  }

  const value = parser._parse_expression(tokens, BindingPower.Default);

  const semicolon = tokens.advance();
  if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
    throw new Error("Expected semicolon after variable declaration statement");
  }

  return {
    kind: StatementKind.VariableDefinition,
    id: identifier,
    type: type.value,
    value,
    start: let_keyword.start,
    end: semicolon.end,
  };
};

const parse_assignment_expression: LeftDenotationHandler = (
  parser,
  tokens,
  left,
  binding_power,
): AssignmentExpression => {
  if (left.kind !== ExpressionKind.Identifier) {
    throw new Error("Expected identifier on the left side of assignment");
  }

  const equals = tokens.advance();
  if (!equals || equals.kind !== TokenKind.Assign) {
    throw new Error("Expected equal sign after identifier");
  }

  const right = parser._parse_expression(tokens, binding_power);

  return {
    kind: ExpressionKind.Assignment,
    left: left as IdentifierExpression,
    right: right,
    start: left.start,
    end: right.end,
  };
};

function parse_function_definition_statement(
  parser: Parser,
  tokens: TokenIterator,
  scope: ScopeKind,
): FunctionDefinitionStatement {
  if (scope !== ScopeKind.Global) {
    throw new Error("Function definitions are only allowed in global scope");
  }

  const fn_keyword = tokens.advance();
  if (!fn_keyword || fn_keyword.kind !== TokenKind.FnKeyword) {
    throw new Error("Expected fn keyword");
  }

  const func_identifier = tokens.advance();
  if (!func_identifier || func_identifier.kind !== TokenKind.Identifier) {
    throw new Error("Expected identifier after fn keyword");
  }

  const open_paren = tokens.advance();
  if (!open_paren || open_paren.kind !== TokenKind.OpenParen) {
    throw new Error("Expected opening parenthesis after function name");
  }

  const params: FunctionParameter[] = [];
  while (true) {
    const param_name = tokens.advance();
    if (!param_name || param_name.kind !== TokenKind.Identifier) {
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

    params.push({ id: param_name, type: param_type });

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

  let result_type: string | null = null;
  if (token.kind === TokenKind.Colon) {
    tokens.advance();
    const result_token = tokens.advance();
    if (!result_token || result_token.kind !== TokenKind.Identifier) {
      throw new Error("Expected return type after colon");
    }
    result_type = result_token.value;
  }

  const open_curly = tokens.advance();
  if (!open_curly || open_curly.kind !== TokenKind.OpenCurely) {
    throw new Error("Expected opening curly brace after return type");
  }

  const body: Statement[] = [];
  while (true) {
    const token = tokens.current();
    if (!token) {
      throw new Error("Expected statement");
    }
    if (token.kind === TokenKind.CloseCurely) break;

    const statement_handler = parser.statement_lookup.get(token.kind);
    if (!statement_handler) {
      throw new Error(
        `No statement handler found for token ${JSON.stringify(token)}`,
      );
    }
    const statement = statement_handler(parser, tokens, ScopeKind.Function);
    body.push(statement);
  }

  const close_curly = tokens.advance();
  if (!close_curly || close_curly.kind !== TokenKind.CloseCurely) {
    throw new Error("Expected closing curly brace after function body");
  }

  return {
    kind: StatementKind.FunctionDefinition,
    id: func_identifier,
    params,
    result: result_type,
    body,
    start: fn_keyword.start,
    end: close_curly.end,
  };
}

const parse_return_value_definition_statement: StatementHandler = (
  parser,
  tokens,
  scope,
): ReturnValueDefinitionStatement => {
  if (scope !== ScopeKind.Function) {
    throw new Error("Return statements are only allowed in function scope");
  }

  const return_keyword = tokens.advance();
  if (!return_keyword || return_keyword.kind !== TokenKind.ReturnKeyword) {
    throw new Error("Expected return keyword");
  }

  const value = parser._parse_expression(tokens, BindingPower.Default);

  const semicolon = tokens.advance();
  if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
    throw new Error("Expected semicolon after return statement");
  }

  return {
    kind: StatementKind.ReturnValueDefinition,
    value,
    start: return_keyword.start,
    end: semicolon.end,
  };
};

const parse_extern_declaration_statement: StatementHandler = (
  parser,
  tokens,
  _scope,
): ExternDeclarationStatement => {
  const extern_keyword = tokens.advance();
  if (!extern_keyword || extern_keyword.kind !== TokenKind.ExternKeyword) {
    throw new Error("Expected extern keyword");
  }

  const declaration = parser._parse_declaration(tokens);

  return {
    kind: StatementKind.ExternDeclaration,
    declaration,
    start: extern_keyword.start,
    end: declaration.end,
  };
};

const parse_variable_declaration_statement: DeclarationStatementHandler = (
  _parser,
  tokens,
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

  const semicolon = tokens.advance();
  if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
    throw new Error(
      "Expected semicolon after variable declaration statement",
    );
  }

  return {
    kind: StatementKind.VariableDeclaration,
    id: identifier,
    type: type,
    start: let_keyword.start,
    end: semicolon.end,
  };
};

const parse_function_declaration_statement: DeclarationStatementHandler = (
  _parser,
  tokens,
): FunctionDeclarationStatement => {
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

  const semicolon = tokens.advance();
  if (!semicolon || semicolon.kind !== TokenKind.Semicolon) {
    throw new Error("Expected semicolon after function declaration");
  }

  return {
    kind: StatementKind.FunctionDeclaration,
    id: identifier,
    params,
    result,
    start: fn_keyword.start,
    end: semicolon.end,
  };
};
