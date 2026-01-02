//! Token types for the S-expression lexer.

use super::error::Span;
use std::fmt;

/// Token types for S-expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Delimiters
    /// `(`
    LParen,
    /// `)`
    RParen,

    // Literals
    /// Integer literal, e.g., `42`, `-17`
    Int(i64),
    /// Floating-point literal, e.g., `3.14`, `-0.5`
    Float(f64),
    /// String literal, e.g., `"hello"`
    String(String),

    // Identifiers
    /// Symbol, e.g., `foo`, `bar-baz`, `+`, `*`
    Symbol(String),
    /// Keyword (colon-prefixed), e.g., `:pattern`, `:effect`
    Keyword(String),

    /// End of file
    Eof,
}

impl TokenKind {
    /// Get a human-readable name for this token kind.
    pub fn name(&self) -> &'static str {
        match self {
            TokenKind::LParen => "'('",
            TokenKind::RParen => "')'",
            TokenKind::Int(_) => "integer",
            TokenKind::Float(_) => "float",
            TokenKind::String(_) => "string",
            TokenKind::Symbol(_) => "symbol",
            TokenKind::Keyword(_) => "keyword",
            TokenKind::Eof => "end of input",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::Int(n) => write!(f, "{n}"),
            TokenKind::Float(n) => write!(f, "{n}"),
            TokenKind::String(s) => write!(f, "\"{s}\""),
            TokenKind::Symbol(s) => write!(f, "{s}"),
            TokenKind::Keyword(k) => write!(f, ":{k}"),
            TokenKind::Eof => write!(f, "<eof>"),
        }
    }
}

/// A token with its source location.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The token type and value.
    pub kind: TokenKind,
    /// Source location.
    pub span: Span,
}

impl Token {
    /// Create a new token.
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Check if this is an EOF token.
    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::Eof)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_kind_display() {
        assert_eq!(TokenKind::LParen.to_string(), "(");
        assert_eq!(TokenKind::Int(42).to_string(), "42");
        assert_eq!(TokenKind::Float(3.14).to_string(), "3.14");
        assert_eq!(TokenKind::String("hello".into()).to_string(), "\"hello\"");
        assert_eq!(TokenKind::Symbol("foo".into()).to_string(), "foo");
        assert_eq!(TokenKind::Keyword("effect".into()).to_string(), ":effect");
    }

    #[test]
    fn test_token_kind_name() {
        assert_eq!(TokenKind::LParen.name(), "'('");
        assert_eq!(TokenKind::Int(0).name(), "integer");
        assert_eq!(TokenKind::Symbol("x".into()).name(), "symbol");
    }

    #[test]
    fn test_token_is_eof() {
        let eof = Token::new(TokenKind::Eof, Span::default());
        assert!(eof.is_eof());

        let paren = Token::new(TokenKind::LParen, Span::default());
        assert!(!paren.is_eof());
    }
}
