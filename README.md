# Web Assembly Expressive Language (WX)

This project is part of my bachelor’s thesis and aims to explore the possibilities of WASM while diving into building a full-blown compiler. It is still in very early stage, so I don't guarantee anything will work at all.

## Design Philosophy

The main goal of this language is to simplify writing WASM modules while closely reflecting the underlying WASM specification in a more high-level, approachable way. Ideally, the language should teach you how WASM works but keep the convenience of a high level language.

## How to start?

The easiest way is to install the compiler cli from npm. Or checko out the playground here: [wx-lang.deno.dev](https://wx-lang.deno.dev/)

```bash
npm install -g wx-compiler

wx-compiler ./main.wx
```

> [!NOTE]
> You will see this warning after compilation. This is because cli imports wasm modules directly, which is currently not stable in Node.js.
>
> ```
> (node:37243) ExperimentalWarning: Importing WebAssembly modules is an experimental feature and might change at any time
> ```

You can also access compiler functionality directly with `wx-compiler-wasm` package.

```ts
import { compile } from "wx-compiler-wasm";

const bytecode = compile("main.wx", `export func main() -> i32 { 2 + 2 }`);
```

Alternatively, you can build the compiler from source by going to `wx-compiler-cli` directory and running:

```bash
cargo run -- ../examples/fibonacci.wx
```

## Syntax

The syntax is inspired by Rust, Zig and Typescript. But I tried to keep the wasm terminology as cloes as possible to the original specification.

### Literals

```
5 // integer literal
true // boolean literal
false // boolean literal
```

### Binary Operators

| Arithmetic Operator |                |
| ------------------- | -------------- |
| `+`                 | Addition       |
| `-`                 | Subtraction    |
| `*`                 | Multiplication |
| `/`                 | Division       |
| `%`                 | Modulo         |

| Comparison Operator |                       |
| ------------------- | --------------------- |
| `==`                | Equality              |
| `!=`                | Inequality            |
| `<`                 | Less than             |
| `<=`                | Less than or equal    |
| `>`                 | Greater than          |
| `>=`                | Greater than or equal |

> [!NOTE]
> Chaining of comparison operators is not allowed, so you cannot write `1 < 2 < 3`. You have to write it as `1 < 2 && 2 < 3`.

| Logical Operator |             |
| ---------------- | ----------- |
| `&&`             | Logical AND |
| `\|\|`           | Logical OR  |

| Arithmetic Assignment |                           |
| --------------------- | ------------------------- |
| `+=`                  | Addition assignment       |
| `-=`                  | Subtraction assignment    |
| `*=`                  | Multiplication assignment |
| `/=`                  | Division assignment       |
| `%=`                  | Modulo assignment         |

| Bitwise Operators |                     |
| ----------------- | ------------------- |
| `&`               | Bitwise AND         |
| `\|`              | Bitwise OR          |
| `^`               | Bitwise XOR         |
| `<<`              | Bitwise left shift  |
| `>>`              | Bitwise right shift |

### Unary Operators

| Unary Operator | Description |
| -------------- | ----------- |
| `-`            | Invert sign |
| `!`            | Logical NOT |
| `^`            | Bitwise NOT |

```
2 + 2 // binary expression

-5 // unary expression
```

### Types

Currently the language supports the following types:

- `i32` - 32-bit signed integer
- `i64` - 64-bit signed integer
- `bool` - true or false
- `unit` - empty type, used for functions that don't return a value
- `never` - type that never returns, used for functions that always panic or loop indefinitely

### Untyped Literals

When you define an integer literal, it doesn't have a type by itself. Instead, the context of the expression determines the type. So, when you define a variable like this:

```wx
local x = 5; // error: type annotation is required
local x: i32 = 5; // ok
local x = 5 as i32; // ok
```

`as` expression can be used to provide the context for the expected type.

### Variables

Local variables are variables that are defined within a function or a block and are not accessible outside of it. They can be either mutable or immutable.

```
local x: i32 = 5; // local immutable variable
local mut y: i32 = 10; // local mutable variable
```

You can't declare a variable without initializing it, so the following is not allowed:

```
local x: i32; // error
local x: i32 = 0; // ok
```

The type annotation is not required in case when the type can be inferred from the initializer value:

```
local x: i32 = 5;
local y = x; // not required to specify type, it will be inferred as i32
```

### Block expression

Blocks are expressions that group statements together. They are defined using curly braces `{}`. The last expression in a block is the value of the block.

This is basically the same as in Rust. I decided to take this approach because it clearly reflects the nature of stack based executation of WASM, where the last value pushed to the stack is the result of the block.

```wx
local x = {
    local x: i32 = 5;
    x + 2 // this is the result value of the block
};
```

Semicolons are required to separate statements.

Block doesn't necessarily have to return a value. The type of such block will be `unit`.

```wx
local x = {
    local x: i32 = 5;
    x + 2; // this is a statement, not an expression
}; // type of x will be unit
```

There are two types of statements:

- Delimited expression - `foo();` any expression and semicolon
- Definition - `local x: i32 = 5;`

Delimited expressions can't return a non empty value, so this means that every value should be consumed or assigned to a variable. If you don't need a value, you can drop it by assigning to `_`;

```wx
_ = foo();
```

### If expressions

If expressions are used to conditionally execute code based on a boolean expression. They can return a value.

```wx
local x: i32 = if 5 > 2 { 5 } else { 2 };

if true { return 5 };
```

### Loops

Currently thre's only one type of loop available, which is just indefinite loop. It can be used to create a loop that runs forever or until a `break` expression is encountered.

```wx
loop {
    // do something
    if some_condition { break } // break out of the loop
}
```

Loops can return a value. You can do this by speficiying the value after break;

```wx
local i = 0;
local x = loop {
    if i > 10 { break i } // break out of the loop and return the value of i
    i += 1;
};
```

You can also use `continue` expression to skip the current iteration and continue with the next one.

### Labels

Labels are markers for blocks, that can be used to break out of nested loops or blocks. They are defined using the `outer:` syntax, where `outer` is the name of the label.

```wx
outer: {
    inner: {
        break :outer
    }
}
```

Labels can be used with plain blocks, loops and if expressions. Only loops don't require an explicit label for breaking out of them, the first enclosing loop will be used as the target for the `break` expression. You can also compine breaking with value and label `break :label 5`;

### Functions

Functions are defined using the `func` keyword. They can have parameters and a return type. If the return type is not specified, it defaults to `unit`(no return value).

```wx
func add(a: i32, b: i32) -> i32 {
    a + b
}
```

To export a function from the module, add an export keyword before the function definition:

```wx
export func main() -> i32 {
    2 + 2
}
```

## Examples

You can find some examples in the `examples` directory. Here are some of them:

- [fibonacci.wx](examples/fibonacci.wx) - A simple fibonacci function.
- [factorial.wx](examples/factorial.wx) - A simple factorial function.
- [pow.wx](examples/pow.wx) - A simple power function.

The compiler also supports output in WAT format, which is a human-readable format for WASM. You can use the `--wat` flag to output additional WAT file.
(Currently not supported in wasm version of the compiler)

```bash
cargo run -- ./examples/fibonacci.wx --wat
```

## Plans for the future

- [ ] Global variables
- [ ] Add support for more types, like `f32`, `f64`, `u32`, `u64`
- [ ] Pattern matching
- [ ] Add support for imports
- [ ] Memory: Pointers, arrays, slices, structs...
- [ ] Tooling: LSP, Formatter, Syntax highlighting
- [ ] More optimizations, like constant folding etc.

## Credits

Here are some of the resources I used to learn about compilers and wasm while working on this project:

- [Julian Hartl (natrixcc)](https://github.com/julian-hartl/natrixcc) - I learned a lot by digging into the source code of this project.
- [tylerlaceby](https://www.youtube.com/@tylerlaceby) - Great youtube channel where I learned a lot about how to write lexers and parsers.
- [Jon Gjengset](https://www.youtube.com/@jonhoo) - Another great author that has a lot of content about Rust with deep dives into the language internals.
- Blog posts and conference talks by [Andrew Kelley](https://github.com/andrewrk), the creator of the Zig.
