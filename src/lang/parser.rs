//! S-expression parser.

use super::ast::{Atom, SExpr};
use super::error::ParseError;
use super::lexer::Lexer;
use super::token::{Token, TokenKind};
use crate::symbol::Symbol;

/// Parser for S-expressions.
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Create a new parser from tokens.
    pub fn from_tokens(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    /// Create a parser and tokenize the source.
    pub fn new(source: &str) -> Result<Self, ParseError> {
        let tokens = Lexer::new(source).tokenize()?;
        Ok(Self::from_tokens(tokens))
    }

    /// Parse all expressions from the source.
    pub fn parse_all(&mut self) -> Result<Vec<SExpr>, ParseError> {
        let mut exprs = Vec::new();
        while !self.is_at_end() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    /// Parse a single expression.
    pub fn parse_expr(&mut self) -> Result<SExpr, ParseError> {
        match self.peek_kind() {
            TokenKind::Eof => Err(ParseError::UnexpectedEof),
            TokenKind::RParen => {
                let span = self.peek().span;
                Err(ParseError::UnexpectedCloseParen(span))
            }
            TokenKind::LParen => self.parse_list(),
            _ => self.parse_atom(),
        }
    }

    /// Parse an atomic expression.
    fn parse_atom(&mut self) -> Result<SExpr, ParseError> {
        let token = self.advance();
        let span = token.span;

        let atom = match &token.kind {
            TokenKind::Int(n) => Atom::Int(*n),
            TokenKind::Float(n) => Atom::Float(*n),
            TokenKind::String(s) => Atom::String(s.clone()),
            TokenKind::Symbol(s) => {
                // Check for boolean literals
                match s.as_str() {
                    "true" => Atom::Bool(true),
                    "false" => Atom::Bool(false),
                    _ => Atom::Symbol(Symbol::new(s)),
                }
            }
            TokenKind::Keyword(k) => Atom::Keyword(Symbol::new(k)),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "atom",
                    found: token.kind.to_string(),
                    span,
                });
            }
        };

        Ok(SExpr::Atom(atom, span))
    }

    /// Parse a list expression.
    fn parse_list(&mut self) -> Result<SExpr, ParseError> {
        let open_paren = self.advance();
        assert!(matches!(open_paren.kind, TokenKind::LParen));
        let start_span = open_paren.span;

        let mut items = Vec::new();

        loop {
            match self.peek_kind() {
                TokenKind::Eof => {
                    return Err(ParseError::UnclosedList(start_span));
                }
                TokenKind::RParen => {
                    let close_paren = self.advance();
                    let span = start_span.merge(close_paren.span);
                    return Ok(SExpr::List(items, span));
                }
                _ => {
                    items.push(self.parse_expr()?);
                }
            }
        }
    }

    // --- Token navigation ---

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or_else(|| {
            self.tokens
                .last()
                .expect("tokens should always have at least EOF")
        })
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens
            .get(self.current - 1)
            .cloned()
            .unwrap_or_else(|| self.tokens.last().unwrap().clone())
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Eof)
    }
}

/// Parse a single S-expression from a string.
pub fn parse(source: &str) -> Result<SExpr, ParseError> {
    let mut parser = Parser::new(source)?;
    parser.parse_expr()
}

/// Parse all S-expressions from a string.
pub fn parse_all(source: &str) -> Result<Vec<SExpr>, ParseError> {
    let mut parser = Parser::new(source)?;
    parser.parse_all()
}

/// Check if input has balanced parentheses (for multi-line input).
pub fn is_complete(input: &str) -> bool {
    let mut depth = 0i32;
    let mut in_string = false;
    let mut escape = false;

    for c in input.chars() {
        if escape {
            escape = false;
            continue;
        }

        if in_string {
            match c {
                '\\' => escape = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }

        match c {
            '"' => in_string = true,
            '(' => depth += 1,
            ')' => depth -= 1,
            ';' => {
                // Skip to end of line (comment)
                // Note: this is a simplified check; full parsing would be better
            }
            _ => {}
        }

        // Too many closing parens
        if depth < 0 {
            return true; // Let the parser report the error
        }
    }

    depth == 0 && !in_string
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_int() {
        let expr = parse("42").unwrap();
        assert_eq!(expr.as_int(), Some(42));
    }

    #[test]
    fn test_parse_negative_int() {
        let expr = parse("-17").unwrap();
        assert_eq!(expr.as_int(), Some(-17));
    }

    #[test]
    fn test_parse_float() {
        let expr = parse("3.14").unwrap();
        assert_eq!(expr.as_float(), Some(3.14));
    }

    #[test]
    fn test_parse_string() {
        let expr = parse(r#""hello world""#).unwrap();
        assert_eq!(expr.as_string(), Some("hello world"));
    }

    #[test]
    fn test_parse_symbol() {
        let expr = parse("foo-bar").unwrap();
        assert!(expr.is_symbol("foo-bar"));
    }

    #[test]
    fn test_parse_keyword() {
        let expr = parse(":pattern").unwrap();
        assert!(expr.is_keyword("pattern"));
    }

    #[test]
    fn test_parse_bool_true() {
        let expr = parse("true").unwrap();
        assert_eq!(expr.as_bool(), Some(true));
    }

    #[test]
    fn test_parse_bool_false() {
        let expr = parse("false").unwrap();
        assert_eq!(expr.as_bool(), Some(false));
    }

    #[test]
    fn test_parse_empty_list() {
        let expr = parse("()").unwrap();
        assert!(expr.is_empty_list());
    }

    #[test]
    fn test_parse_simple_list() {
        let expr = parse("(+ 1 2)").unwrap();
        let items = expr.as_list().unwrap();
        assert_eq!(items.len(), 3);
        assert!(items[0].is_symbol("+"));
        assert_eq!(items[1].as_int(), Some(1));
        assert_eq!(items[2].as_int(), Some(2));
    }

    #[test]
    fn test_parse_nested_list() {
        let expr = parse("(+ (* 2 3) 4)").unwrap();
        let items = expr.as_list().unwrap();
        assert_eq!(items.len(), 3);
        assert!(items[0].is_symbol("+"));
        assert!(items[1].is_call("*"));
        assert_eq!(items[2].as_int(), Some(4));
    }

    #[test]
    fn test_parse_multiple() {
        let exprs = parse_all("1 2 3").unwrap();
        assert_eq!(exprs.len(), 3);
        assert_eq!(exprs[0].as_int(), Some(1));
        assert_eq!(exprs[1].as_int(), Some(2));
        assert_eq!(exprs[2].as_int(), Some(3));
    }

    #[test]
    fn test_parse_multiple_lists() {
        let exprs = parse_all("(a) (b) (c)").unwrap();
        assert_eq!(exprs.len(), 3);
    }

    #[test]
    fn test_parse_with_comments() {
        let expr = parse("; comment\n42").unwrap();
        assert_eq!(expr.as_int(), Some(42));
    }

    #[test]
    fn test_parse_rule_syntax() {
        let expr = parse(
            r#"(rule goat-baas
                 :pattern (= (get ?e Name) "goat")
                 :every 10
                 :effect (emit-message "Baa!"))"#,
        )
        .unwrap();

        assert!(expr.is_call("rule"));
        let items = expr.as_list().unwrap();
        assert!(items[1].is_symbol("goat-baas"));
        assert!(items[2].is_keyword("pattern"));
    }

    #[test]
    fn test_parse_entity_syntax() {
        let expr = parse(r#"(entity goat (Name "goat") (HP 10))"#).unwrap();
        assert!(expr.is_call("entity"));
    }

    #[test]
    fn test_error_unclosed_list() {
        let err = parse("(foo").unwrap_err();
        assert!(matches!(err, ParseError::UnclosedList(_)));
    }

    #[test]
    fn test_error_unexpected_close() {
        let err = parse(")").unwrap_err();
        assert!(matches!(err, ParseError::UnexpectedCloseParen(_)));
    }

    #[test]
    fn test_error_empty_input() {
        let err = parse("").unwrap_err();
        assert!(matches!(err, ParseError::UnexpectedEof));
    }

    #[test]
    fn test_round_trip() {
        let source = "(+ 1 (* 2 3))";
        let expr = parse(source).unwrap();
        assert_eq!(expr.to_string(), source);
    }

    #[test]
    fn test_span_preserved() {
        let expr = parse("(foo bar)").unwrap();
        assert_eq!(expr.span().line, 1);
        assert_eq!(expr.span().column, 1);
    }

    #[test]
    fn test_is_complete_balanced() {
        assert!(is_complete("()"));
        assert!(is_complete("(foo)"));
        assert!(is_complete("(foo (bar))"));
        assert!(is_complete("(a) (b)"));
    }

    #[test]
    fn test_is_complete_unbalanced() {
        assert!(!is_complete("("));
        assert!(!is_complete("(foo"));
        assert!(!is_complete("(("));
        assert!(!is_complete("(foo (bar)"));
    }

    #[test]
    fn test_is_complete_strings() {
        assert!(is_complete(r#""hello""#));
        assert!(!is_complete(r#""hello"#));
        assert!(is_complete(r#"("hello")"#));
    }

    #[test]
    fn test_is_complete_escaped_quotes() {
        assert!(is_complete(r#""hello\"world""#));
        assert!(!is_complete(r#""hello\""#));
    }

    #[test]
    fn test_variable_pattern() {
        let expr = parse("?entity").unwrap();
        assert!(expr.is_symbol("?entity"));
    }
}
