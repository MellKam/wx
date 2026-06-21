pub mod ast;
pub mod codegen;
pub mod fmt;
pub mod mir;
pub mod opt;
#[cfg(test)]
pub mod testing;
pub mod tir;
pub mod vfs;

/// Source code of the standard library, embedded at compile time.
pub const STDLIB_SOURCE: &str = include_str!("../../../std.wx");
