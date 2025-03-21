// use string_interner::symbol::SymbolU32;

// use crate::{Expression, Token};

// /**
//  *
//  * export const enum StatementKind {
//     /** @example add(1, 2) */
//     Expression,
//     /** @example { ... } */
//     Block,
//     /** @example let x: i32 = 1 */
//     VariableDefinition,
//     /** @example fn add(x: i32, y: i32): i32 ... */
//     FunctionDefinition,
//     /** @example return ...; */
//     ReturnValue,
//     /** @example export ... */
//     ExportDefinition,
//     /** @example let x: i32; */
//     VariableDeclaration,
//     /** @example fn add(x: i32, y: i32): i32; */
//     FunctionDeclaration,
//     /** @example import "env" ... */
//     ModuleImportDeclaration,
// }
//  */
// struct FunctionParameter {
//     id: SymbolU32,
//     type_: Token,
// }

// enum StatementKind {
//     Expression {
//         value: Expression,
//     },
//     Block {
//         body: Vec<Statement>,
//     },
//     VariableDefinition {
//         id: Token,
//         type_: Token,
//         value: Expression,
//     },
//     FunctionDefinition {
//         id: Token,
//         params: Vec<FunctionParameter>,
//         return_type: Option<Token>,
//         body: Vec<Statement>,
//     },
//     ReturnValue {
//         value: Expression,
//     },
//     ExportDefinition {
//         definition: Box<Statement>,
//     },
//     VariableDeclaration {
//         id: Token,
//         type_: Token,
//     },
//     FunctionDeclaration {
//         id: Token,
//         params: Vec<FunctionParameter>,
//         return_type: Option<Token>,
//     },
//     ModuleImportDeclaration {
//         name: Token,
//         alias: Token,
//     },
// }

// struct Statement {
//     kind: StatementKind,
//     start: usize,
//     end: usize,
// }
