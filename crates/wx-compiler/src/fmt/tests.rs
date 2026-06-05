use indoc::indoc;

use super::*;
use crate::vfs::Files;

#[allow(unused)]
struct TestCase {
    interner: StringInterner,
    files: Files,
    ast: AST,
}

impl<'case> TestCase {
    fn new(source: &str) -> Self {
        let mut interner = StringInterner::new();
        let mut files = Files::new();
        let file_id = files
            .add("main.wx".to_string(), source.to_string())
            .unwrap();
        let mut id_generator = DefIdGenerator::new();
        let ast = Parser::parse(file_id, &files, &mut interner, &mut id_generator);

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
                    x: g: {
                        break :g 5
                    },
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
