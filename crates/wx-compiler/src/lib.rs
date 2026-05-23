pub mod ast;
pub mod codegen;
pub mod fmt;
pub mod mir;
pub mod opt;
#[cfg(test)]
pub mod testing;
pub mod tir;

/// Source code of the standard library, embedded at compile time.
pub const STDLIB_SOURCE: &str = include_str!("../../../std.wx");
/// Canonical filename used for the embedded standard library.
pub const STDLIB_FILENAME: &str = "std.wx";

// trait Scalable {
//     fn value(self) -> i32;
//     fn doubled(self) -> i32 {
//         self.value() * 2
//     }
// }

// struct Num {
//     n: i32,
// }

// impl Scalable for Num {
//     fn value(self) -> i32 {
//         self.n
//     }
// }

// fn run() -> i32 {
//     let x = Num { n: 21 };
//     x.doubled()
// }
