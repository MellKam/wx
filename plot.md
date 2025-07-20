# Intro (hook)

Hi. You know, being bored software engineer, I spent last 6 month diving deep into compilers and language design. And, as a result I built a compiler for writing high level web assembly. I want to share my process of building and learning this stuff, so you can maybe learns something as well, or at least have a fun time watching while eating.

In this video we will explore:
What web assembly is?
How to write it by hand? (it’s actually pretty easy)
Why wasm is a better version of jvm bytecode?
How to build lexers, parsers, typechecker?
Type Inference, never type, control flow, unreachable code???

That’s a lot. I will try to explain it as simple as possible, and of course, it’s just my interpretation of information so I may be incorrect in some details. Correct me if I’m wrong.

# Introduction to WASM

You probably know, assembly is this low level language that is basically consists of instructions for specific processor architecture. This is probably the biggest problem with sharing our code, as we have different OS, processors, calling conventions which makes it difficult to distribute software. You need to compile it for every possible instruction set, os etc. But we know web, it’s different, on the web we don’t care if you are on x86, arm, linux, macos, windows. This is mostly because of interpreted nature of javascript. That's all good, but you know, this kinda limits the developers, cause now must write everything in javascript, which is not the best language for everything, because of its dynamic nature, garbage collection, unalble to control memory, etc. Wouldn't it be great if we could write our code in any language and run it everywhere? That's basically what web assembly is. It's this low level language, or more precisely bytecode format that is designed to be a compilation target for other langauges.
