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

// trait X {
//     type Y;

//     fn z(&self) -> Self::Y;
// }

// fn main(x: impl X) {
//     let z = x.z();
// }

// struct Foo;
// impl X for Foo {
//     type Y = u32;

//     fn z(&self) -> Self::Y {
//         42
//     }
// }

// fn foo() {
//     type Y = <Foo as X>::Y;
//     main(Foo);
// }

// trait PointerSize {}
// impl PointerSize for u32 {}
// impl PointerSize for u64 {}

// trait Memory {
//     type Size: PointerSize;
//     const MEMORY_INDEX: u32;

//     fn grow(self, delta: Self::Size) -> Self::Size;
//     fn size(self) -> Self::Size;
// }

// trait Memory32: Memory<Size = u32> {
//     fn alloc() {
//         Self::Size
//     }
// }
// trait Memory64: Memory<Size = u64> {}
