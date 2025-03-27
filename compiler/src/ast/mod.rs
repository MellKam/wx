mod diagnostics;
mod lexer;
mod parser;

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolU32};

    use super::{
        diagnostics::DiagnosticStore,
        lexer::Lexer,
        parser::{BindingPower, Parser},
    };

    #[test]
    fn basic() {
        let source = "console.log(\"Hello, world!\");";
        let string_interner = StringInterner::<StringBackend>::new();
        let diagnostics = Rc::new(RefCell::new(DiagnosticStore::new()));
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(string_interner, diagnostics, source, lexer);

        let result = parser.parse_expression(BindingPower::Default);

        println!("{:?}", result);
        println!("{:#?}", parser.ast);
        println!("{:#?}", parser.diagnostics.borrow().diagnostics);
        println!(
            "{:#?}",
            parser.interner.iter().collect::<Vec<(SymbolU32, &str)>>()
        );
    }
}
