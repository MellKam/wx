pub mod ast;
pub mod codegen;
pub mod fmt;
pub mod mir;
pub mod vfs;
pub mod opt;
#[cfg(test)]
pub mod testing;
pub mod tir;

/// Source code of the standard library, embedded at compile time.
pub const STDLIB_SOURCE: &str = include_str!("../../../std.wx");
/// Canonical filename used for the embedded standard library.
pub const STDLIB_FILENAME: &str = "std.wx";
