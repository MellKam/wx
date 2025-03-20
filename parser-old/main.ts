import { Lexer, TokenIterator } from "./lexer.ts";
import {
	parse_statement,
	type FunctionDefinitionStatement,
} from "./statements.ts";
import { codegen_function } from "../codegen/codegen.ts";

const parse_statements = (tokens: TokenIterator) => {
	const statements = [];
	while (tokens.current()) {
		statements.push(parse_statement(tokens, []));
	}
	return statements;
};

const lexer = Lexer.create();
const tokens = new TokenIterator(
	lexer.tokenize(`
import "env" env {
  fn log(num: i32);
}

fn add(a: i32, b: i32): i32 {
	let y: i32 = 10 * 3;
  return a + b * y;
}
`)
);
const statements = parse_statements(tokens);

const func = statements[1]!;

console.log((func as FunctionDefinitionStatement).body.body);

const instructions = codegen_function(func as FunctionDefinitionStatement);

console.log(instructions);
