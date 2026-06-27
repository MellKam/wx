use indoc::indoc;
use wx_compiler::ast;
use wx_compiler::vfs;

use super::*;

#[allow(unused)]
struct TestCase {
    interner: ast::StringInterner,
    files: vfs::Files,
    ast: ast::AST,
}

impl<'case> TestCase {
    fn new(source: &str) -> Self {
        let mut interner = ast::StringInterner::new();
        let mut files = vfs::Files::new();
        let file_id = files
            .add("main.wx".to_string(), source.to_string())
            .unwrap();
        let mut id_generator = ast::DefIdGenerator::new();
        let ast = ast::Parser::parse(file_id, &files, &mut interner, &mut id_generator);

        TestCase {
            interner,
            files,
            ast,
        }
    }
}

#[test]
fn test_format_simple_function() {
    let case = TestCase::new(indoc! {"
        fn add(a: i32, b: i32) -> i32 { 
            a + b
        }

        export { add, add as \"plus\", minus }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 40,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }

            export {
                add,
                add as \"plus\",
                minus
            }
        "}
    );
}

#[test]
fn test_format_import_block() {
    let case = TestCase::new(indoc! {"
        import \"math\" {
            fn sqrt(f64) -> f64;
            fn pow(base: f64, exponent: f64) -> f64;
            fn log(x: string);
        }

        fn main() {
            local x = sqrt(2.0);
            local y = pow(x, 2.0);
        }

        export { main }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            import \"math\" {
                fn sqrt(f64) -> f64;
                fn pow(base: f64, exponent: f64) -> f64;
                fn log(x: string);
            }

            fn main() {
                local x = sqrt(2.0);
                local y = pow(x, 2.0);
            }

            export {
                main
            }
        "}
    );
}

#[test]
fn test_format_single_import_function_stays_inline() {
    let case = TestCase::new(indoc! {"
        import \"console\" {
            fn log(message: string);
         }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            import \"console\" {
                fn log(message: string);
            }
        "}
    );
}

#[test]
fn test_format_module_items() {
    let case = TestCase::new(indoc! {"
        pub module wasm {
            pub fn answer() -> i32{
                42
            }

            fn helper(  ) {}
        }

        module math;
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            pub module wasm {
                pub fn answer() -> i32 {
                    42
                }

                fn helper() {}
            }

            module math;
        "}
    );
}

#[test]
fn test_format_impl_items() {
    let case = TestCase::new(indoc! {"
        impl i32 {
            #[inline]
            pub fn double(self) -> i32 {
                self * 2
            }

            const ZERO: i32 = 0;
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            impl i32 {
                #[inline]
                pub fn double(self) -> i32 {
                    self * 2
                }

                const ZERO: i32 = 0;
            }
        "}
    );
}

#[test]
fn test_format_trait_items() {
    let case = TestCase::new(indoc! {"
        pub trait Widget: Drawable + Sized {
            type Output: Show + Clone;

            const SIZE: u32;
            fn render(self);

            #[inline]
            fn grow(self, delta: u32) -> u32 {
                delta
            }
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            pub trait Widget: Drawable + Sized {
                type Output: Show + Clone;

                const SIZE: u32;

                fn render(self);

                #[inline]
                fn grow(self, delta: u32) -> u32 {
                    delta
                }
            }
        "}
    );
}

#[test]
fn test_format_const_items() {
    let case = TestCase::new(indoc! {"
        const MAX: i32 = 100;

        const ANSWER = 42;
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            const MAX: i32 = 100;

            const ANSWER = 42;
        "}
    );
}

#[test]
fn test_format_enum_items() {
    let case = TestCase::new(indoc! {"
        enum Status: i32 {
            Foo,
            Bar = 1,
            Baz,
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            enum Status: i32 {
                Foo,
                Bar = 1,
                Baz,
            }
        "}
    );
}

#[test]
fn test_format_struct_items() {
    let case = TestCase::new(indoc! {"
        pub struct Point { pub x: i32, y: i32 }

        struct Unit { value: f64 }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            pub struct Point {
                pub x: i32,
                y: i32,
            }

            struct Unit {
                value: f64,
            }
        "}
    );
}

#[test]
fn test_format_generic_function() {
    let case = TestCase::new(indoc! {"
        fn identity<T>(value: T) -> T {
            value
        }

        fn zip<A, B: Clone + Debug>(a: A, b: B) -> A {
            a
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            fn identity<T>(value: T) -> T {
                value
            }

            fn zip<A, B: Clone + Debug>(a: A, b: B) -> A {
                a
            }
        "}
    );
}

#[test]
fn test_format_struct_init() {
    let case = TestCase::new(indoc! {"
        fn main() {
            local a = Point::{ x: 1, y: 2 };
            local b = Point::{ x: 1, y: 2, z: 3, w: 4, extra_long_field: 99 }
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 40,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            fn main() {
                local a = Point::{ x: 1, y: 2 };
                local b = Point::{
                    x: 1,
                    y: 2,
                    z: 3,
                    w: 4,
                    extra_long_field: 99,
                }
            }
        "}
    );
}

#[test]
fn test_format_struct_init_block_value() {
    let case = TestCase::new(indoc! {"
        fn main() -> i32 {
            local p = Point::{ x: g: { break :g 5 }, y: 10 }
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            fn main() -> i32 {
                local p = Point::{
                    x: g: { break :g 5 },
                    y: 10,
                }
            }
        "}
    );
}

#[test]
fn test_format_local_patterns() {
    let case = TestCase::new(indoc! {"
        fn f(p: Point, pair: (i32, i32)) {
            local x = 1;
            local mut y = 2;
            local _ = 3;
            local (a,b) = pair;
            local (mut c,_) = pair;
            local Point{x,y:renamed} = p;
            local (a,b): (i32,i32) = pair;
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            fn f(p: Point, pair: (i32, i32)) {
                local x = 1;
                local mut y = 2;
                local _ = 3;
                local (a, b) = pair;
                local (mut c, _) = pair;
                local Point { x, y: renamed } = p;
                local (a, b): (i32, i32) = pair;
            }
        "}
    );
}

#[test]
fn test_format_impl_trait_items() {
    let case = TestCase::new(indoc! {"
        impl Iterator for Range {
            type Item = i32;

            fn next(self) -> Self::Item {
                0
            }
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            impl Iterator for Range {
                type Item = i32;

                fn next(self) -> Self::Item {
                    0
                }
            }
        "}
    );
}

#[test]
fn test_format_block_like_statement_semicolon() {
    // Without explicit `;`: formatter does not add one after block-like statements.
    let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            if true {}
            42
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            fn f() -> i32 {
                if true {}
                42
            }
        "}
    );

    // With explicit `;`: formatter preserves it so the user can visually
    // separate the block statement from the expression that follows.
    let case = TestCase::new(indoc! {"
        fn f() -> i32 {
            if true {};
            42
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            fn f() -> i32 {
                if true {};
                42
            }
        "}
    );
}

#[test]
fn test_format_call_args_wrap() {
    let fmt = |src: &str| -> String {
        let case = TestCase::new(src);
        format(
            &case.ast,
            &case.interner,
            &case.files.get(case.ast.file_id).unwrap().source,
            RendererConfig {
                max_line_width: 80,
                indent_width: 4,
                trailing_comma: true,
            },
        )
    };

    // Short call: stays on one line.
    assert_eq!(
        fmt("fn f() { foo(1, 2, 3); }"),
        indoc! {"
            fn f() {
                foo(1, 2, 3);
            }
        "},
    );

    // Long call: each argument on its own line.
    assert_eq!(
        fmt(
            "fn f() { host::draw_rect(food_x * CELL_SIZE, food_y * CELL_SIZE, CELL_SIZE, CELL_SIZE, 0xFFFFFF00); }"
        ),
        indoc! {"
            fn f() {
                host::draw_rect(
                    food_x * CELL_SIZE,
                    food_y * CELL_SIZE,
                    CELL_SIZE,
                    CELL_SIZE,
                    0xFFFFFF00,
                );
            }
        "},
    );

    // Long method call wraps the same way.
    assert_eq!(
        fmt(
            "fn f() { obj.render(food_x * CELL_SIZE, food_y * CELL_SIZE, CELL_SIZE, CELL_SIZE, 0xFFFFFF00); }"
        ),
        indoc! {"
            fn f() {
                obj.render(
                    food_x * CELL_SIZE,
                    food_y * CELL_SIZE,
                    CELL_SIZE,
                    CELL_SIZE,
                    0xFFFFFF00,
                );
            }
        "},
    );
}

#[test]
fn test_format_local_definition_wraps() {
    let fmt = |src: &str| -> String {
        let case = TestCase::new(src);
        format(
            &case.ast,
            &case.interner,
            &case.files.get(case.ast.file_id).unwrap().source,
            RendererConfig {
                max_line_width: 80,
                indent_width: 4,
                trailing_comma: true,
            },
        )
    };

    // Short assignment stays on one line.
    assert_eq!(
        fmt("fn f() { local x = 42; }"),
        indoc! {"
            fn f() {
                local x = 42;
            }
        "},
    );

    // Long non-block-like value: breaks after =, value indented on next line.
    // Using FB_WIDTH (a named constant) makes the expression exceed 80 cols.
    assert_eq!(
        fmt(
            "memory heap: Memory where { Size = u32 }; fn set_pixel(x: u32, y: u32) { local base: heap::*mut u8 = (fb_ptr() + (y * FB_WIDTH + x) * 3) as heap::*mut u8; }"
        ),
        indoc! {"
            memory heap: Memory where { Size = u32 };

            fn set_pixel(x: u32, y: u32) {
                local base: heap::*mut u8 =
                    (fb_ptr() + (y * FB_WIDTH + x) * 3) as heap::*mut u8;
            }
        "},
    );

    // Block-like value (struct init): = stays on the same line, struct breaks inside.
    assert_eq!(
        fmt(
            "fn f() { local p = Point::{ x: very_long_name_one, y: very_long_name_two, z: very_long_name_three }; }"
        ),
        indoc! {"
            fn f() {
                local p = Point::{
                    x: very_long_name_one,
                    y: very_long_name_two,
                    z: very_long_name_three,
                };
            }
        "},
    );
}

#[test]
fn test_format_inline_blocks() {
    let fmt = |src: &str| -> String {
        let case = TestCase::new(src);
        format(
            &case.ast,
            &case.interner,
            &case.files.get(case.ast.file_id).unwrap().source,
            RendererConfig {
                max_line_width: 80,
                indent_width: 4,
                trailing_comma: true,
            },
        )
    };

    // Single-statement if guard: fits → inline.
    assert_eq!(
        fmt(
            "memory heap: Memory where { Size = u32 }; fn check(data: heap::[]u8) -> bool { if data.len() < 4 { return false }; true }"
        ),
        indoc! {"
            memory heap: Memory where { Size = u32 };

            fn check(data: heap::[]u8) -> bool {
                if data.len() < 4 { return false };
                true
            }
        "},
    );

    // if-else as a value expression: both branches fit → inline.
    assert_eq!(
        fmt("fn pick(cond: bool) -> i32 { local x: i32 = if cond { 5 } else { 6 }; x }"),
        indoc! {"
            fn pick(cond: bool) -> i32 {
                local x: i32 = if cond { 5 } else { 6 };
                x
            }
        "},
    );

    // Block that is too long to fit inline → multi-line.
    // indent=4, cond takes 43 chars → remaining=33; block flat=50 > 33 → Break.
    assert_eq!(
        fmt(
            "fn f() -> i32 { if some_very_long_condition_variable { return some_very_long_return_value_here } 0 }"
        ),
        indoc! {"
            fn f() -> i32 {
                if some_very_long_condition_variable {
                    return some_very_long_return_value_here
                }
                0
            }
        "},
    );

    // Multi-statement block always breaks even when short.
    assert_eq!(
        fmt("fn f() -> i32 { local x = 1; x }"),
        indoc! {"
            fn f() -> i32 {
                local x = 1;
                x
            }
        "},
    );
}

#[test]
fn test_format_memory_config() {
    let fmt = |src: &str| -> String {
        let case = TestCase::new(src);
        format(
            &case.ast,
            &case.interner,
            &case.files.get(case.ast.file_id).unwrap().source,
            RendererConfig {
                max_line_width: 80,
                indent_width: 4,
                trailing_comma: true,
            },
        )
    };

    // No config block
    assert_eq!(
        fmt("memory heap: Memory where { Size = u32 };"),
        "memory heap: Memory where { Size = u32 };\n",
    );

    // min_pages only
    assert_eq!(
        fmt("memory heap: Memory where { Size = u32 } { min_pages: 4 };"),
        "memory heap: Memory where { Size = u32 } { min_pages: 4 };\n",
    );

    // max_pages only
    assert_eq!(
        fmt("memory heap: Memory where { Size = u32 } { max_pages: 10 };"),
        "memory heap: Memory where { Size = u32 } { max_pages: 10 };\n",
    );

    // both fields
    assert_eq!(
        fmt("memory heap: Memory where { Size = u32 } { min_pages: 1, max_pages: 10 };"),
        "memory heap: Memory where { Size = u32 } { min_pages: 1, max_pages: 10 };\n",
    );
}

#[test]
fn test_format_binary_chain_breaks_at_line_limit() {
    let fmt = |src: &str| -> String {
        let case = TestCase::new(src);
        format(
            &case.ast,
            &case.interner,
            &case.files.get(case.ast.file_id).unwrap().source,
            RendererConfig {
                max_line_width: 80,
                indent_width: 4,
                trailing_comma: true,
            },
        )
    };

    // Short chain: fits on one line, stays flat.
    assert_eq!(
        fmt("fn f(a: i32, b: i32, c: i32) -> i32 { a | b | c }"),
        indoc! {"
            fn f(a: i32, b: i32, c: i32) -> i32 {
                a | b | c
            }
        "},
    );

    // Long chain: exceeds 80 columns, each operand on its own line.
    assert_eq!(
        fmt(
            "memory heap: Memory where { Size = u32 }; fn read(data: heap::[]u8, off: u32) -> i32 { (data[off] as i32) | ((data[off + 1] as i32) << 8) | ((data[off + 2] as i32) << 16) | ((data[off + 3] as i32) << 24) }"
        ),
        indoc! {"
            memory heap: Memory where { Size = u32 };

            fn read(data: heap::[]u8, off: u32) -> i32 {
                (data[off] as i32)
                    | ((data[off + 1] as i32) << 8)
                    | ((data[off + 2] as i32) << 16)
                    | ((data[off + 3] as i32) << 24)
            }
        "},
    );
}

#[test]
fn test_format_comments_preserved() {
    let fmt = |src: &str| -> String {
        let case = TestCase::new(src);
        format(
            &case.ast,
            &case.interner,
            &case.files.get(case.ast.file_id).unwrap().source,
            RendererConfig::default(),
        )
    };

    // File-header comment before first item.
    assert_eq!(
        fmt(indoc! {"
            // Framebuffer helpers
            const FB_WIDTH: u32 = 320;
            const FB_HEIGHT: u32 = 200;
        "}),
        indoc! {"
            // Framebuffer helpers
            const FB_WIDTH: u32 = 320;
            const FB_HEIGHT: u32 = 200;
        "},
    );

    // Comment between compact items.
    assert_eq!(
        fmt(indoc! {"
            const A: u32 = 1;
            // separator
            const B: u32 = 2;
        "}),
        indoc! {"
            const A: u32 = 1;
            // separator
            const B: u32 = 2;
        "},
    );

    // Comment with blank line before it between compact items.
    assert_eq!(
        fmt(indoc! {"
            const A: u32 = 1;

            // group B
            const B: u32 = 2;
        "}),
        indoc! {"
            const A: u32 = 1;

            // group B
            const B: u32 = 2;
        "},
    );

    // Comment between statements inside a function body.
    assert_eq!(
        fmt(indoc! {"
            fn f() {
                local x: i32 = 1;
                // compute y
                local y: i32 = 2;
            }
        "}),
        indoc! {"
            fn f() {
                local x: i32 = 1;
                // compute y
                local y: i32 = 2;
            }
        "},
    );

    // Doc comment preserved like a regular comment.
    assert_eq!(
        fmt(indoc! {"
            /// Returns the sum.
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }
        "}),
        indoc! {"
            /// Returns the sum.
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }
        "},
    );
}

#[test]
fn test_format_long_type_params_wrap() {
    let case = TestCase::new(indoc! {"
        pub fn memory_copy<Size: PointerSize, SrcMem: Memory where { Size = Size }, DstMem: Memory where { Size = Size }>(dst: DstMem::*mut u8, src: SrcMem::*u8, len: Size) {}
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            pub fn memory_copy<
                Size: PointerSize,
                SrcMem: Memory where { Size = Size },
                DstMem: Memory where { Size = Size },
            >(
                dst: DstMem::*mut u8,
                src: SrcMem::*u8,
                len: Size,
            ) {}
        "},
    );
}

#[test]
fn test_format_impl_trait_multi_segment() {
    // Multi-segment trait name in `impl a::b::Trait for Type` must be
    // rendered with `::` separators (exercises build_path_segments for
    // the ImplTrait trait_name field).
    let case = TestCase::new(indoc! {"
        impl module::Drawable for Point {
            fn draw(self) {}
        }
    "});
    let output = format(
        &case.ast,
        &case.interner,
        &case.files.get(case.ast.file_id).unwrap().source,
        RendererConfig {
            max_line_width: 80,
            indent_width: 4,
            trailing_comma: true,
        },
    );
    assert_eq!(
        output,
        indoc! {"
            impl module::Drawable for Point {
                fn draw(self) {}
            }
        "}
    );
}
