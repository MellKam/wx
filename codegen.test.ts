import type { FunctionDefinitionStatement } from "./ast.ts";
import { codegen_function } from "./codegen.ts";
import { Lexer } from "./lexer.ts";
import { Parser } from "./parser.ts";

const source = `
fn add(a: i32, b: i32): i32 {
  return a + b;
} 

extern fn log(num: i32);
`;

const lexer = Lexer.create();
const parser = Parser.create();

console.time("total");
const tokens = lexer.tokenize(source);
const ast = parser.parse(tokens);
const instructions = codegen_function(
  ast.at(0) as FunctionDefinitionStatement,
);
console.timeEnd("total");

console.log(instructions.flat());
console.log(ast);
