I want to explore the topic of address of operator and how do I provide ability to allocate static memory in my language. Let me explain my motivation. I develop a language that focuses on clear representation of wasm semantics, so things like taking a pointer to the value on the stack is discarded because wasm stack is not addressable. Right now we don't even have a address of operator. For allocating static memory like strings or arrays we have a literal expressions which implicitly mean allocate this in the static memory and give me a pointer. So right now it looks like this:

```rust
"abc" // always evaluates to slice of bytes: []u8
[1, 2, 3] // needs type annotation for numbers to know exactly what type we want
local x: [3]u32 = [1, 2, 3] // allocates a static array in the memory and gives you an array pointer to it
```

Ideally I would like these literals to not carry the one specific type, but be untyped until the context forces the type. So that we could use string literal both as slice and array with known length

```wx
local x: [3]u8 = "abc";
local y: [_]u8 = "abc"; // infer the number of items in array from literal
local z: [_]u8 = [1, 2, 3]; // same thing might work for array literals
```

Yeasterday I also started thinking about how do I fit the struct into this picture. Allocating struct in the static memory would be useful to initialize a bootstrap allocator, which is why I wanted to have this feature. But the problem with that is that right now I have struct init literal but it works only on the stack. 

```wx
struct Point {
  x: u32,
  y: u32,
}

fn test() {
  local x = Point::{ x: 0, y: 0 }; // fully allocated on the stack
  // when translated to wasm this is treated as just two i32 on the stack 
}
```

So with that setup I needed to think how can I make static allocation for structs and what came up to me is that we dont' currently have a address of allocator and maybe we could reuse it to give different meaning to this.

```wx 
global point: *mut Point = &mut Point::{ x: 0, y: 0 };
fn test() {
  local x = &mut Point::{ x: 0, y: 0 }; // same thing as in global
}
```

But it's kind of not consistent, because the struct init can have other expressions inside like, for example a function call. Would this mean that I just allocate a uninitialized memory and then set it inside the function, like this? 

Also, if we use the address of literal for static allocation then maybe we should consider making it consistent with other literals? 

```
fn test() {
  local str = &"abc"; // should this be like that?
  local arr = &mut [1, 2, 3]; // and like that?
}
```

Then what dose it mean to have an array literal without address of? How does this fit into the current picture of the language?

As a side note, I was thinking whether we should do address of as prefix or postfix operator. The idea is that we already have deref operator written as postfix, which is easy to chain. `object.*.field`. So obvious solution would be to keep the address of the same, but I'm not sure this is correct long term solution. It seems like more ergonomic though. `object.*.field.&mut` something like this would give you a mutable address of the struct field. So I want to explore this topic and boil down to pros and cons of postfix address of operator.

One more thing we need to take into the context is how do we fit multiple memories in this picture. In wasm we can define multiple memories and my language clearly represents this with special memory item you define at the root module of your program. One problem with it is that if you have multiple memories you need to somehow know in which memory you want to allocate the static literal, like string or array. 

```
memory heap: Memory where { Size = u32 }; // defining 32bit memory
memory mem2: Memory where { Size = u64 }; // another memory with 64bit address space

struct Point {
  x: u32,
  y: u32,
}

fn test() {
  local point = Point::{ x: 0, y: 0 }.&mut; // in which memory it lives?
  local point = &mut Point::{ x: 0, y: 0 }; // same thing with prefix style
  local str: []u8 = "abc"; // do we need to require the memory annotation here? probably so
  local str: heap::[]u8 = "abc"; // this is fine
  local arr = [1, 2, 3].&heap::mut; // does something like this make sense to add to the language
  // what better syntax migth be there
  local arr = heap::&mut [1, 2, 3]; // somehting like this? a bit more consistent with how pointers are defined
  // But I'm not sure this is a good idea
}
```

So the question is whether the address of should contain the memory tag in it or the type should be retreived from the context? Or the & and &mut would be enough? 

THere's probably too much questions at once. Let's break it donw into the main points we need to think about and main decitions we need to take. I want to ensure that language sytanx is consistent and it's easy to think about. If there's a thing that would make implementing such syntax hard, or would carry a possible footgun - let me know so we can think about this together. 




