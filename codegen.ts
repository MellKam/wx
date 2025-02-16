import { TokenKind, TokenNameByKind } from "./lexer.ts";
import {
  type AssignmentExpression,
  type BinaryExpression,
  type Expression,
  ExpressionKind,
  type ExpressionStatement,
  type ExternDeclarationStatement,
  type FloatExpression,
  type FunctionDefinitionStatement,
  type IdentifierExpression,
  type IntExpression,
  type Module,
  type ReturnValueDefinitionStatement,
  type Statement,
  StatementKind,
  type UnaryExpression,
  type VariableDefinitionStatement,
} from "./ast.ts";

const Section = {
  Custom: 0,
  Type: 1,
  Import: 2,
  Function: 3,
  Table: 4,
  Memory: 5,
  Global: 6,
  Export: 7,
  Start: 8,
  Element: 9,
  Code: 10,
  Data: 11,
} as const;
type Section = (typeof Section)[keyof typeof Section];

const ValueType = {
  i32: 0x7f,
  i64: 0x7e,
  f32: 0x7d,
  f64: 0x7c,
} as const;
type ValueType = (typeof ValueType)[keyof typeof ValueType];

const ExportType = {
  Func: 0x00,
  Table: 0x01,
  Memory: 0x02,
  Global: 0x03,
} as const;
type ExportType = (typeof ExportType)[keyof typeof ExportType];

const ImportKind = {
  Function: 0,
  Table: 1,
  Memory: 2,
  Global: 3,
} as const;
type ImportKind = (typeof ImportKind)[keyof typeof ImportKind];

const Mutability = {
  Immutable: 0,
  Mutable: 1,
} as const;
type Mutability = (typeof Mutability)[keyof typeof Mutability];

const OpCode = {
  LocalGet: 0x20,
  LocalSet: 0x21,
  LocalTee: 0x22,

  I32Const: 0x41,
  I64Const: 0x42,
  F32Const: 0x43,
  F64Const: 0x44,

  I32Clz: 0x67,
  I32Ctz: 0x68,
  I32Popcnt: 0x69,
  I32Add: 0x6a,
  I32Sub: 0x6b,
  I32Mul: 0x6c,
  I32DivS: 0x6d,
  I32DivU: 0x6e,
  I32RemS: 0x6f,
  I32RemU: 0x70,
  I32And: 0x71,
  I32Or: 0x72,
  I32Xor: 0x73,
  I32Shl: 0x74,
  I32ShrS: 0x75,
  I32ShrU: 0x76,
  I32Rotl: 0x77,
  I32Rotr: 0x78,

  I64Add: 0x7c,
  I64Sub: 0x7d,
  I64Mul: 0x7e,
  I64DivS: 0x7f,
  I64DivU: 0x80,

  F32Add: 0x92,
  F32Sub: 0x93,
  F32Mul: 0x94,
  F32Div: 0x95,

  F64Add: 0xa0,
  F64Sub: 0xa1,
  F64Mul: 0xa2,
  F64Div: 0xa3,
} as const;
type OpCode = (typeof OpCode)[keyof typeof OpCode];

type Instruction = [OpCode, ...number[]];

const codegen_expression = (
  locals: Map<
    string,
    {
      type: ValueType;
      index: number;
    }
  >,
  expression: Expression,
): Instruction[] => {
  if (expression.kind === ExpressionKind.Identifier) {
    const { value } = expression as IdentifierExpression;
    const local = locals.get(value);
    if (!local) {
      throw new Error(`Invalid identifier ${value}`);
    }
    return [[OpCode.LocalGet, local.index]];
  }
  if (expression.kind === ExpressionKind.Assignment) {
    const { left, right } = expression as AssignmentExpression;
    const local = locals.get(left.value);
    if (!local) {
      throw new Error("Invalid identifier");
    }
    const instructions = codegen_expression(locals, right);
    return [...instructions, [OpCode.LocalSet, local.index]];
  }
  if (expression.kind === ExpressionKind.Unary) {
    const { operator, right } = expression as UnaryExpression;
    const instructions = codegen_expression(locals, right);
    if (operator === TokenKind.Dash) {
      return [...instructions, [OpCode.I32Const, 1], [OpCode.I32Xor]];
    }
    throw new Error(`Invalid unary operator "${TokenNameByKind[operator]}"`);
  }
  if (expression.kind === ExpressionKind.Int) {
    const { value } = expression as IntExpression;
    return [[OpCode.I32Const, value]];
  }
  if (expression.kind === ExpressionKind.Float) {
    const { value } = expression as FloatExpression;
    return [[OpCode.F32Const, value]];
  }
  if (expression.kind === ExpressionKind.Binary) {
    const { operator, left, right } = expression as BinaryExpression;
    const left_instructions = codegen_expression(locals, left);
    const right_instructions = codegen_expression(locals, right);
    if (operator === TokenKind.Plus) {
      return [...left_instructions, ...right_instructions, [OpCode.I32Add]];
    }
    if (operator === TokenKind.Dash) {
      return [...left_instructions, ...right_instructions, [OpCode.I32Sub]];
    }
    throw new Error("Invalid binary operator");
  }
  throw new Error("Invalid expression");
};

const codegen_statement = (
  locals: Map<
    string,
    {
      type: ValueType;
      index: number;
    }
  >,
  statement: Statement,
): Instruction[] => {
  switch (statement.kind) {
    case StatementKind.Expression: {
      const { expression } = statement as ExpressionStatement;
      return codegen_expression(locals, expression);
    }
    case StatementKind.ReturnValueDefinition: {
      const { value } = statement as ReturnValueDefinitionStatement;
      return codegen_expression(locals, value);
    }
    case StatementKind.VariableDefinition: {
      const { id, value } = statement as VariableDefinitionStatement;
      const existing_local = locals.get(id.value);
      if (existing_local) {
        throw new Error("Variable already declared");
      }

      const instructions: Instruction[] = [
        ...codegen_expression(locals, value),
        [OpCode.LocalSet, locals.size],
      ];

      locals.set(id.value, {
        type: ValueType.i32,
        index: locals.size,
      });
      return instructions;
    }
    default:
      throw new Error("Invalid statement");
  }
};

export const codegen_function = (func: FunctionDefinitionStatement) => {
  const locals = new Map<string, { type: ValueType; index: number }>();

  const get_type = (type: string) => {
    if (type in ValueType) {
      return ValueType[type as keyof typeof ValueType];
    }
    throw new Error("Invalid type");
  };

  for (let i = 0; i < func.params.length; i++) {
    const param = func.params[i]!;
    locals.set(param.id.value, { type: get_type(param.type.value), index: i });
  }
  const instructions = func.body.flatMap((statement) =>
    codegen_statement(locals, statement)
  );
  return instructions;
};

const get_buildin_type = (type: string): ValueType => {
  if (type in ValueType) {
    return ValueType[type as keyof typeof ValueType]!;
  }
  throw new Error(`Invalid type ${type}`);
};

interface FunctionSignature {
  params: ValueType[];
  result: ValueType | null;
}

const get_function_definition_signature = (
  func: FunctionDefinitionStatement,
): FunctionSignature => {
  return {
    params: func.params.map((param) => get_buildin_type(param.type.value)),
    result: func.result ? get_buildin_type(func.result) : null,
  };
};

function create_type_section(module: Module): number[] {
  const signatures = [
    ...module.exports.map((stmt) =>
      get_function_definition_signature(stmt.definition)
    ),
    ...module.functions.map((func) => get_function_definition_signature(func)),
  ];

  return [
    Section.Type,
    signatures.length,
    ...signatures.flatMap((signature) => {
      return [
        0x60,
        signature.params.length,
        ...signature.params,
        ...(signature.result ? [1, signature.result] : []),
      ];
    }),
  ];
}

function create_import_section(
  externals: ExternDeclarationStatement[],
): number[] {
  return [
    Section.Import,
    externals.length,
    ...externals.flatMap((external) => {
      if (external.declaration.kind === StatementKind.FunctionDeclaration) {
        const { id, params, result } = external.declaration;
        const text_encoder = new TextEncoder();
        const encoded_id = text_encoder.encode(id.value);

        return [
          encoded_id.length,
          ...encoded_id,
          ExportType.Func,
          0,
          ...params.map((param) => get_buildin_type(param.type.value)),
          result ? 1 : 0,
          ...(result ? [get_buildin_type(result.value)] : []),
        ];
      }
      throw new Error("Invalid import");
    }),
  ];
}

function codegen(module: Module): Uint8Array {
  // Magic number: \0asm and version: 1
  const code: number[] = [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00];
}
