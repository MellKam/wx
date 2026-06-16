# `typeset` — Compile-Time Type Sets for wx

## Definition

A `typeset` is a **closed, compile-time-only set of concrete types**. It is not a trait, not an enum, and not a memory union. It carries no runtime representation, no vtable, and no tag. It exists purely as a constraint during type checking and is erased entirely before codegen.

```rust
typeset Integer { u8, i8, u16, i16, u32, i32, u64, i64 }
typeset Float { f32, f64 }
typeset Number { Integer, Float }  // composition — flattened to leaf types internally
typeset PointerSize { u32, u64 }
```

---

## Core Rules

**1. Closed set**
Only the compiler or the defining module can declare typeset members. Users cannot add new members via `impl` or any other mechanism. This is the fundamental difference from traits.

**2. No methods or operations**
A `typeset` defines *which types are possible*, not *what those types can do*. Operations are always expressed separately via traits:

```rust
// WRONG mental model:
typeset Integer { ... }  // does NOT imply Add, Eq, etc.

// CORRECT:
fn add<N: Integer + Add>(a: N, b: N) -> N { a + b }
//         ^^^^^^^ what types   ^^^ what it can do
```

**3. Always monomorphized**
Any generic parameterized over a typeset is instantiated once per concrete member that is actually used. No generic version is ever emitted. No runtime dispatch. The typeset constraint is fully erased after type checking.

**4. Composition is flat set union**
When composing typesets, the result is always normalized to the set of concrete leaf types:

```rust
typeset Number { Integer, Float }
// internally: { u8, i8, u16, i16, u32, i32, u64, i64, f32, f64 }
// no nesting at the type system level
```

**5. Single type variable = single concrete type**
A type variable `N: Integer` refers to exactly one concrete type per monomorphized instantiation. Passing two distinct types where one `N` is expected is always a compile error:

```rust
fn add<N: Integer + Add>(a: N, b: N) -> N
add(1u32, 1i32)  // error: expected N, found two distinct types
```

**6. Literal inference**
Integer and float literals are valid in a typeset-bounded context if they fit all members of the relevant kind simultaneously. If the concrete type cannot be inferred from context, the compiler emits "type annotation required" — no implicit defaulting:

```rust
fn add<N: Integer + Add>(a: N, b: N) -> N { a + b }
add(1, 2)         // error: type annotation required
add::<i32>(1, 2)  // ok
let x: i32 = add(1, 2)  // ok — resolved from context
```

**7. Impl coverage is checkable at definition time**
Because typesets are closed, the compiler can verify at the point of a function definition that all required trait impls exist for every member. No surprises at call sites.

---

## Relationship to Traits

| | `trait` | `typeset` |
|---|---|---|
| Open/closed | Open — anyone can impl | Closed — fixed at declaration |
| Runtime repr | vtable (dyn) or monomorphized | Always monomorphized, no vtable |
| Defines behavior | Yes | No |
| Defines membership | No | Yes |
| Literal inference | No | Yes |
| Can be composed | Via bounds `A + B` | Via set union `{ A, B }` |

They are **orthogonal concepts** that compose via bounds:
```rust
fn foo<N: Integer + Add + Eq>(a: N, b: N) -> bool
//         ^typeset^  ^----traits----^
```

---

## Relationship to Comptime Literals

The compiler-internal `{integer}` and `{float}` inference variables that already exist for untyped literals are **the same mechanism**, now made user-visible and nameable. `typeset Integer` and `typeset Float` are the explicit surface syntax for what the compiler already does internally. This unification is intentional.

---

## Use in `Memory`

The canonical motivating example:

```rust
typeset PointerSize { u32, u64 }

#[lang = "memory"]
pub trait Memory {
    type Size: PointerSize;
    const MEMORY_INDEX: u32;
    const DATA_END: Self::*mut u8;

    fn grow(self, delta: Self::Size) -> Self::Size;
    fn size(self) -> Self::Size;
}
```

`Memory::Size` is constrained to be exactly `u32` or `u64`, resolved at compile time based on the target (wasm32 vs wasm64). No runtime branching, no tag, maps directly to the Wasm `i32`/`i64` index type of the target.

---

## What `typeset` is NOT

- Not a Rust-style `union` — no overlapping memory, no runtime value
- Not an `enum` — no tag, no runtime variant
- Not a trait — cannot be implemented by users, defines no behavior
- Not a type alias — does not introduce a new type, only a constraint

# Numeric Literals and Typesets

## Two Distinct Concepts

When working with numeric literals and generic type constraints in wx, it's important to understand the distinction between two related but separate concepts: **literal kinds** and **typesets**.

---

## Literal Kinds — `comptime_int` and `comptime_float`

When you write a numeric literal, wx assigns it a **literal kind** — a temporary compile-time tag that describes how the literal was written, before any concrete type is known.

There are exactly two literal kinds:

- **`comptime_int`** — a whole number literal with no decimal point: `5`, `300`, `0`, `-12`
- **`comptime_float`** — a decimal literal with a decimal point: `5.0`, `3.14`, `-0.5`

These are **not types**. They are temporary compiler-internal tags that exist only during type resolution. A `comptime_int` or `comptime_float` literal is never present in the final compiled output — it must always be resolved to a concrete type before codegen.

The literal kind determines which typesets and concrete types a literal is compatible with:

- A `comptime_int` literal is compatible with any member of `Integer` or any numeric typeset that contains only integer types
- A `comptime_float` literal is compatible with any member of `Float` or any numeric typeset that contains only float types
- A `comptime_int` literal **never** satisfies a `Float` bound — write `1.0` instead of `1`
- A `comptime_float` literal **never** satisfies an `Integer` bound — write `1` instead of `1.0`

This distinction is **strict and enforced at the point of use**:

```rust
fn f<N: Float + Add>(a: N, b: N) -> N { a + b }

f(1.0, 2.0)  // ok — both comptime_float, compatible with Float
f(1, 2.0)    // error: comptime_int literal in Float context, use 1.0
```

```rust
fn g<N: Integer + Add>(a: N, b: N) -> N { a + b }

g(1, 2)      // ok — both comptime_int, compatible with Integer
g(1.0, 2)    // error: comptime_float literal in Integer context, use 1
```

Literal kind errors are reported **early**, at the point of the offending literal, not deferred to monomorphization. This makes them fast to diagnose and easy to fix.

---

## Typesets — Closed Sets of Concrete Types

A `typeset` is a **named, closed, compile-time set of concrete types** used as a type-level bound on a type variable. Unlike a literal kind, which is a temporary tag on a value, a typeset is a real language declaration that survives through type checking all the way to monomorphization.

```rust
typeset Integer { u8, i8, u16, i16, u32, i32, u64, i64 }
typeset Float   { f32, f64 }
typeset Number  { Integer, Float }
```

When a type variable is bounded by a typeset, it means: **this type variable will be resolved to exactly one concrete member of this set, chosen at the call site**.

```rust
fn add<N: Integer + Add>(a: N, b: N) -> N { a + b }
//         ^^^^^^^
//         N will be exactly one of {u8, i8, ..., i64}
//         determined at the call site, not here
```

This is fundamentally different from a `comptime_int` literal, which says "coerce me to whatever concrete type the context demands". A typeset bound says "this type variable ranges over this set" — it is a constraint on a type, not a property of a value.

---

## How They Interact

When a `comptime_int` or `comptime_float` literal appears in a context bounded by a typeset, two separate checks occur:

**1. Kind compatibility check — early, at the literal**

The literal kind must be compatible with the typeset. A `comptime_int` is compatible with `Integer`, a `comptime_float` is compatible with `Float`. A `comptime_float` in an `Integer` context is always an error regardless of what the type variable eventually resolves to:

```rust
fn f<N: Integer + Add>(a: N) -> N { a + 1.0 }
//                                      ^^^
// error: comptime_float literal cannot satisfy Integer bound
//        hint: use 1 instead of 1.0
```

**2. Value range check — deferred, at monomorphization**

If the kind is compatible, the literal's value must fit in the concrete type that the type variable resolves to. This check is deferred until the call site pins the concrete type:

```rust
fn f<N: Integer + Add>(a: N) -> N { a + 300 }
//                                      ^^^
// comptime_int 300, kind compatible with Integer — deferred

f::<u8>(0)
// error: comptime_int literal 300 does not fit in u8 (range 0..=255)

f::<u16>(0)
// ok — 300 fits in u16
```

---

## Summary

| | `comptime_int` / `comptime_float` | `typeset` |
|---|---|---|
| What it is | A tag on an unresolved literal | A named closed set of concrete types |
| Level | Value | Type |
| Lifetime | Dissolved at type resolution | Survives to monomorphization |
| Written by | Never — compiler internal, appears in diagnostics only | User, as a top-level declaration |
| Purpose | Tracks how a literal was written | Constrains which concrete types a type variable may become |
| Kind error timing | Early — at the literal site | N/A |
| Range error timing | Deferred — at monomorphization | N/A |

The key distinction to remember: **`comptime_int` and `comptime_float` are properties of values; typesets are constraints on types.** They cooperate during type resolution but are never the same thing.