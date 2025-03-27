use std::{cell::RefCell, ops::Range, rc::Rc};

use super::lexer::{Token, TokenKind};

#[derive(Clone, Debug)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: String,
    pub span: Range<usize>,
    pub kind: DiagnosticKind,
}

#[derive(Debug)]
pub struct DiagnosticStore {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticStore {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn reporn_unexpected_token(&mut self, expected: TokenKind, token: Token) {
        self.diagnostics.push(Diagnostic {
            message: format!(
                "Unexpected token `{}`, expected `{}`.",
                token.kind, expected
            ),
            span: token.span,
            kind: DiagnosticKind::Error,
        });
    }

    pub fn report_invalid_unary_operator(&mut self, token: Token) {
        self.diagnostics.push(Diagnostic {
            message: format!("Invalid unary operator `{}`.", token.kind),
            span: token.span,
            kind: DiagnosticKind::Error,
        });
    }
}

pub type DiagnosticStoreCell = Rc<RefCell<DiagnosticStore>>;

fn get_line_index(text: &str, position: usize) -> usize {
    text[..=position].lines().count() - 1
}

fn get_line_start(text: &str, line_index: usize) -> usize {
    text.lines()
        .take(line_index)
        .map(|line| line.len() + 1)
        .sum()
}

pub struct DiagnosticsPrinter<'a> {
    text: &'a str,
    diagnostics: &'a [Diagnostic],
}

impl<'a> DiagnosticsPrinter<'a> {
    // pub fn new(text: &'a SourceText, diagnostics: &'a [Diagnostic]) -> Self {
    //     Self { text, diagnostics }
    // }

    // /// Stringifies the diagnostic.
    // /// It uses the following format:
    // ///
    // /// 3 |  let x = 1 + "2";
    // ///                  ^^^ Cannot add `&str` to `i32`
    // pub fn stringify_diagnostic(&self, diagnostic: &Diagnostic) -> String {
    //     let line_index = self.text.line_index(diagnostic.span.start);
    //     let line = self.text.get_line(line_index);
    //     let line_start = self.text.line_start(line_index);

    //     let column = diagnostic.span.start - line_start;

    //     let (prefix, span, suffix) = self.get_text_spans(diagnostic, &line, column);

    //     let indent = cmp::min(PREFIX_LENGTH, column);
    //     let (arrow_pointers, arrow_line) = Self::format_arrow(diagnostic, indent);
    //     let error_message = Self::format_error_message(diagnostic, indent, column, line_index);
    //     format!(
    //         "{}{}{}{}{}\n{}\n{}\n{}",
    //         prefix,
    //         Fg(Red),
    //         span,
    //         Fg(Reset),
    //         suffix,
    //         arrow_pointers,
    //         arrow_line,
    //         error_message
    //     )
    // }

    // fn format_error_message(
    //     diagnostic: &Diagnostic,
    //     indent: usize,
    //     column: usize,
    //     line_index: usize,
    // ) -> String {
    //     format!(
    //         "{:indent$}+-- {} ({}:{})",
    //         "",
    //         diagnostic.message,
    //         column + 1,
    //         line_index + 1,
    //         indent = indent
    //     )
    // }

    // fn format_arrow(diagnostic: &Diagnostic, indent: usize) -> (String, String) {
    //     let arrow_pointers = format!(
    //         "{:indent$}{}",
    //         "",
    //         std::iter::repeat('^')
    //             .take(diagnostic.span.length())
    //             .collect::<String>(),
    //         indent = indent
    //     );
    //     let arrow_line = format!("{:indent$}|", "", indent = indent);
    //     (arrow_pointers, arrow_line)
    // }

    // fn get_text_spans(
    //     &'a self,
    //     diagnostic: &Diagnostic,
    //     line: &'a str,
    //     column: usize,
    // ) -> (&'a str, &'a str, &'a str) {
    //     let prefix_start = cmp::max(0, column as isize - PREFIX_LENGTH as isize) as usize;
    //     let prefix_end = column;
    //     let suffix_start = cmp::min(column + diagnostic.span.length(), line.len());
    //     let suffix_end = cmp::min(suffix_start + PREFIX_LENGTH, line.len());

    //     let prefix = &line[prefix_start..prefix_end];
    //     let span = &line[prefix_end..suffix_start];
    //     let suffix = &line[suffix_start..suffix_end];
    //     (prefix, span, suffix)
    // }

    // pub fn print(&self) {
    //     for diagnostic in self.diagnostics {
    //         println!("{}", self.stringify_diagnostic(diagnostic));
    //     }
    // }
}
