mod expression;
mod lexer;
mod statement;
mod unescape;

pub use expression::*;
pub use lexer::*;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use string_interner::{StringInterner, backend::StringBackend};

    use super::*;

    #[test]
    fn it_works() {
        let mut interner = StringInterner::<StringBackend>::new();
        let lexer = Lexer::new();
        let expr_parser = ExpressionParser::new();

        let input = "console.log(\"hel\\\\lo world\", log)".to_string();

        let tokens = lexer.tokenize(&mut interner, input).unwrap();
        let expr = expr_parser
            .parse(&mut tokens.iter().peekable(), BindingPower::Default)
            .unwrap();

        println!("{:?}", expr);

        interner.iter().for_each(|(id, value)| {
            println!("{:?} => {}", id, value);
        });
    }
}
