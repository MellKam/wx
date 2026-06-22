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

// trait PointerSize {};

// trait Memory {
//     type Size: PointerSize;
// }

// struct Layout<M: Memory> {
//     size: M::Size,
//     align: M::Size,
// }

// fn size_of<T, M: Memory>() -> M::Size {}
// fn align_of<T, M: Memory>() -> M::Size {}

// impl<M: Memory> Layout<M> {
//     pub fn of<T>() -> Layout<M> {
//         Layout {
//             size: size_of::<T, _>(),
//             align: align_of::<T, _>(),
//         }
//     }
// }
