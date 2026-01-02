//! Error types for the language module.

use std::fmt;

/// Source location for error reporting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    /// Byte offset of start.
    pub start: usize,
    /// Byte offset of end (exclusive).
    pub end: usize,
    /// 1-based line number.
    pub line: u32,
    /// 1-based column number.
    pub column: u32,
}

impl Span {
    /// Create a new span.
    pub fn new(start: usize, end: usize, line: u32, column: u32) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Merge two spans into one covering both.
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line.min(other.line),
            column: if self.line <= other.line {
                self.column
            } else {
                other.column
            },
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Lexer error.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum LexError {
    #[error("unexpected character '{0}' at {1}")]
    UnexpectedChar(char, Span),

    #[error("unterminated string starting at {0}")]
    UnterminatedString(Span),

    #[error("invalid number '{0}' at {1}")]
    InvalidNumber(String, Span),

    #[error("invalid escape sequence '\\{0}' at {1}")]
    InvalidEscape(char, Span),
}

impl LexError {
    /// Get the span where the error occurred.
    pub fn span(&self) -> Span {
        match self {
            LexError::UnexpectedChar(_, span) => *span,
            LexError::UnterminatedString(span) => *span,
            LexError::InvalidNumber(_, span) => *span,
            LexError::InvalidEscape(_, span) => *span,
        }
    }
}

/// Parser error.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ParseError {
    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("unexpected token '{found}' at {span}, expected {expected}")]
    UnexpectedToken {
        expected: &'static str,
        found: String,
        span: Span,
    },

    #[error("unclosed list starting at {0}")]
    UnclosedList(Span),

    #[error("unexpected ')' at {0}")]
    UnexpectedCloseParen(Span),

    #[error("lex error: {0}")]
    Lex(#[from] LexError),
}

impl ParseError {
    /// Get the span where the error occurred, if available.
    pub fn span(&self) -> Option<Span> {
        match self {
            ParseError::UnexpectedEof => None,
            ParseError::UnexpectedToken { span, .. } => Some(*span),
            ParseError::UnclosedList(span) => Some(*span),
            ParseError::UnexpectedCloseParen(span) => Some(*span),
            ParseError::Lex(e) => Some(e.span()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_display() {
        let span = Span::new(0, 5, 1, 1);
        assert_eq!(span.to_string(), "1:1");
    }

    #[test]
    fn test_span_merge() {
        let a = Span::new(0, 5, 1, 1);
        let b = Span::new(10, 15, 2, 3);
        let merged = a.merge(b);
        assert_eq!(merged.start, 0);
        assert_eq!(merged.end, 15);
        assert_eq!(merged.line, 1);
    }

    #[test]
    fn test_lex_error_span() {
        let err = LexError::UnexpectedChar('x', Span::new(5, 6, 1, 6));
        assert_eq!(err.span().start, 5);
    }
}
