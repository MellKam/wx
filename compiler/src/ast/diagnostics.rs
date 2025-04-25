use owo_colors::OwoColorize;

use super::span::TextSpan;

#[derive(Clone, Debug)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: String,
    pub span: TextSpan,
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
}
