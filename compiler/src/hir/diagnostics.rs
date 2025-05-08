use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::EnumIndex;
use crate::files::FileId;
use crate::span::Span;

#[derive(Debug, Clone)]
pub enum DiagnosticContext {
    UnknownEnumVariant {
        file_id: FileId,
        enum_index: EnumIndex,
        span: Span,
    },
}

impl DiagnosticContext {
    pub fn to_diagnostic(self) -> Diagnostic<FileId> {
        use DiagnosticContext::*;
        match self {
            UnknownEnumVariant {
                file_id,
                enum_index,
                span,
            } => Diagnostic::error()
                .with_message("unknown enum variant")
                .with_label(Label::primary(file_id, span)),
        }
    }
}
