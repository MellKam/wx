use std::cell::RefCell;

use bumpalo;

use super::lexer;

#[derive(Clone, Debug)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: String,
    pub span: lexer::Span,
    pub kind: DiagnosticKind,
}

#[derive(Debug)]
pub struct DiagnosticsBag {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticsBag {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    // pub fn reporn_unexpected_token(&mut self, expected: lexer::TokenKind, token:
    // lexer::Token) {     self.diagnostics.push(Diagnostic {
    //         message: format!(
    //             "Unexpected token `{}`, expected `{}`.",
    //             token.kind, expected
    //         ),
    //         span: token.span,
    //         kind: DiagnosticKind::Error,
    //     });
    // }

    // pub fn report_invalid_unary_operator(&mut self, token: lexer::Token) {
    //     self.diagnostics.push(Diagnostic {
    //         message: format!("Invalid unary operator `{}`.", token.kind),
    //         span: token.span,
    //         kind: DiagnosticKind::Error,
    //     });
    // }
}

fn get_line_index(text: &str, position: usize) -> usize {
    text[..=position].lines().count() - 1
}

fn get_line_start(text: &str, line_index: usize) -> usize {
    text.lines()
        .take(line_index)
        .map(|line| line.len() + 1)
        .sum()
}
