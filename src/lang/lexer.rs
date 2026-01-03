//! S-expression lexer (tokenizer).

use super::error::{LexError, Span};
use super::token::{Token, TokenKind};
use std::iter::Peekable;
use std::str::CharIndices;

/// Lexer for S-expressions.
pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    line: u32,
    column: u32,
    /// Byte offset where current token started.
    token_start: usize,
    /// Line where current token started.
    token_start_line: u32,
    /// Column where current token started.
    token_start_column: u32,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source.
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            line: 1,
            column: 1,
            token_start: 0,
            token_start_line: 1,
            token_start_column: 1,
        }
    }

    /// Tokenize the entire source into a vector of tokens.
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_eof = token.is_eof();
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace_and_comments();
        self.mark_token_start();

        match self.peek_char() {
            None => Ok(self.make_token(TokenKind::Eof)),
            Some('(') => {
                self.advance();
                Ok(self.make_token(TokenKind::LParen))
            }
            Some(')') => {
                self.advance();
                Ok(self.make_token(TokenKind::RParen))
            }
            Some('"') => self.scan_string(),
            Some(':') => self.scan_keyword(),
            Some(c) if c.is_ascii_digit() => self.scan_number(),
            Some('-') | Some('+') => {
                // Could be a number or a symbol
                if self.is_number_start() {
                    self.scan_number()
                } else {
                    self.scan_symbol()
                }
            }
            Some(c) if is_symbol_start(c) => self.scan_symbol(),
            Some(c) => {
                let span = self.current_span();
                self.advance();
                Err(LexError::UnexpectedChar(c, span))
            }
        }
    }

    // --- Character operations ---

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn peek_next_char(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().map(|(_, c)| c)
    }

    fn advance(&mut self) -> Option<char> {
        if let Some((_, c)) = self.chars.next() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            Some(c)
        } else {
            None
        }
    }

    fn current_offset(&mut self) -> usize {
        self.chars
            .peek()
            .map(|(i, _)| *i)
            .unwrap_or(self.source.len())
    }

    // --- Span management ---

    fn mark_token_start(&mut self) {
        self.token_start = self.current_offset();
        self.token_start_line = self.line;
        self.token_start_column = self.column;
    }

    fn current_span(&mut self) -> Span {
        Span::new(
            self.token_start,
            self.current_offset(),
            self.token_start_line,
            self.token_start_column,
        )
    }

    fn make_token(&mut self, kind: TokenKind) -> Token {
        Token::new(kind, self.current_span())
    }

    // --- Whitespace and comments ---

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(c) if c.is_whitespace() => {
                    self.advance();
                }
                Some(';') => {
                    // Skip to end of line
                    while let Some(c) = self.peek_char() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    // --- Number scanning ---

    fn is_number_start(&mut self) -> bool {
        // Check if +/- followed by a digit
        match self.peek_char() {
            Some('-') | Some('+') => matches!(self.peek_next_char(), Some(c) if c.is_ascii_digit()),
            Some(c) => c.is_ascii_digit(),
            None => false,
        }
    }

    fn scan_number(&mut self) -> Result<Token, LexError> {
        let start = self.current_offset();

        // Consume optional sign
        if matches!(self.peek_char(), Some('-') | Some('+')) {
            self.advance();
        }

        // Consume digits before decimal point
        while matches!(self.peek_char(), Some(c) if c.is_ascii_digit()) {
            self.advance();
        }

        // Check for decimal point
        let is_float = if self.peek_char() == Some('.') {
            // Make sure it's followed by a digit (not just "42.")
            if matches!(self.peek_next_char(), Some(c) if c.is_ascii_digit()) {
                self.advance(); // consume '.'
                while matches!(self.peek_char(), Some(c) if c.is_ascii_digit()) {
                    self.advance();
                }
                true
            } else {
                false
            }
        } else {
            false
        };

        // Check for exponent
        let has_exponent = if matches!(self.peek_char(), Some('e') | Some('E')) {
            self.advance();
            if matches!(self.peek_char(), Some('-') | Some('+')) {
                self.advance();
            }
            while matches!(self.peek_char(), Some(c) if c.is_ascii_digit()) {
                self.advance();
            }
            true
        } else {
            false
        };

        let end = self.current_offset();
        let text = &self.source[start..end];

        if is_float || has_exponent {
            match text.parse::<f64>() {
                Ok(n) => Ok(self.make_token(TokenKind::Float(n))),
                Err(_) => Err(LexError::InvalidNumber(
                    text.to_string(),
                    self.current_span(),
                )),
            }
        } else {
            match text.parse::<i64>() {
                Ok(n) => Ok(self.make_token(TokenKind::Int(n))),
                Err(_) => Err(LexError::InvalidNumber(
                    text.to_string(),
                    self.current_span(),
                )),
            }
        }
    }

    // --- String scanning ---

    fn scan_string(&mut self) -> Result<Token, LexError> {
        let start_span = self.current_span();
        self.advance(); // consume opening '"'

        let mut value = String::new();

        loop {
            match self.peek_char() {
                None => return Err(LexError::UnterminatedString(start_span)),
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.peek_char() {
                        None => return Err(LexError::UnterminatedString(start_span)),
                        Some('n') => {
                            self.advance();
                            value.push('\n');
                        }
                        Some('t') => {
                            self.advance();
                            value.push('\t');
                        }
                        Some('r') => {
                            self.advance();
                            value.push('\r');
                        }
                        Some('\\') => {
                            self.advance();
                            value.push('\\');
                        }
                        Some('"') => {
                            self.advance();
                            value.push('"');
                        }
                        Some(c) => {
                            let span = self.current_span();
                            return Err(LexError::InvalidEscape(c, span));
                        }
                    }
                }
                Some(c) => {
                    self.advance();
                    value.push(c);
                }
            }
        }

        Ok(self.make_token(TokenKind::String(value)))
    }

    // --- Symbol and keyword scanning ---

    fn scan_symbol(&mut self) -> Result<Token, LexError> {
        let start = self.current_offset();

        while let Some(c) = self.peek_char() {
            if is_symbol_char(c) {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.current_offset();
        let text = &self.source[start..end];

        Ok(self.make_token(TokenKind::Symbol(text.to_string())))
    }

    fn scan_keyword(&mut self) -> Result<Token, LexError> {
        self.advance(); // consume ':'
        let start = self.current_offset();

        while let Some(c) = self.peek_char() {
            if is_symbol_char(c) {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.current_offset();
        let text = &self.source[start..end];

        Ok(self.make_token(TokenKind::Keyword(text.to_string())))
    }
}

/// Check if a character can start a symbol.
fn is_symbol_start(c: char) -> bool {
    c.is_alphabetic()
        || matches!(
            c,
            '_' | '?' | '!' | '*' | '+' | '-' | '/' | '<' | '>' | '=' | '&' | '%' | '^' | '@'
        )
}

/// Check if a character can be part of a symbol.
///
/// Note: `:` is allowed in the middle of symbols (e.g., `obj:noun`)
/// but NOT at the start (that's a keyword like `:keyword`).
fn is_symbol_char(c: char) -> bool {
    is_symbol_start(c) || c.is_ascii_digit() || c == '.' || c == ':'
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Result<Vec<Token>, LexError> {
        Lexer::new(source).tokenize()
    }

    fn lex_kinds(source: &str) -> Vec<TokenKind> {
        lex(source).unwrap().into_iter().map(|t| t.kind).collect()
    }

    #[test]
    fn test_empty() {
        assert_eq!(lex_kinds(""), vec![TokenKind::Eof]);
    }

    #[test]
    fn test_parens() {
        assert_eq!(
            lex_kinds("()"),
            vec![TokenKind::LParen, TokenKind::RParen, TokenKind::Eof]
        );
    }

    #[test]
    fn test_nested_parens() {
        assert_eq!(
            lex_kinds("(())"),
            vec![
                TokenKind::LParen,
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::RParen,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_integers() {
        assert_eq!(
            lex_kinds("42 -17 +5 0"),
            vec![
                TokenKind::Int(42),
                TokenKind::Int(-17),
                TokenKind::Int(5),
                TokenKind::Int(0),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_floats() {
        assert_eq!(
            lex_kinds("3.14 -0.5 1e10 2.5e-3"),
            vec![
                TokenKind::Float(3.14),
                TokenKind::Float(-0.5),
                TokenKind::Float(1e10),
                TokenKind::Float(2.5e-3),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            lex_kinds(r#""hello" "world""#),
            vec![
                TokenKind::String("hello".into()),
                TokenKind::String("world".into()),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_string_escapes() {
        assert_eq!(
            lex_kinds(r#""a\nb\tc\\d\"e""#),
            vec![TokenKind::String("a\nb\tc\\d\"e".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn test_symbols() {
        assert_eq!(
            lex_kinds("foo bar-baz foo? set!"),
            vec![
                TokenKind::Symbol("foo".into()),
                TokenKind::Symbol("bar-baz".into()),
                TokenKind::Symbol("foo?".into()),
                TokenKind::Symbol("set!".into()),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_symbols_with_colons() {
        // Colon in the middle of a symbol is part of the symbol
        assert_eq!(
            lex_kinds("obj:noun dir:direction foo:bar:baz"),
            vec![
                TokenKind::Symbol("obj:noun".into()),
                TokenKind::Symbol("dir:direction".into()),
                TokenKind::Symbol("foo:bar:baz".into()),
                TokenKind::Eof
            ]
        );

        // Colon at the start is a keyword
        assert_eq!(
            lex_kinds(":keyword :foo"),
            vec![
                TokenKind::Keyword("keyword".into()),
                TokenKind::Keyword("foo".into()),
                TokenKind::Eof
            ]
        );

        // Mixed: keyword followed by symbol with colon
        assert_eq!(
            lex_kinds(":type obj:noun"),
            vec![
                TokenKind::Keyword("type".into()),
                TokenKind::Symbol("obj:noun".into()),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_operators_as_symbols() {
        assert_eq!(
            lex_kinds("+ - * / = < > <= >="),
            vec![
                TokenKind::Symbol("+".into()),
                TokenKind::Symbol("-".into()),
                TokenKind::Symbol("*".into()),
                TokenKind::Symbol("/".into()),
                TokenKind::Symbol("=".into()),
                TokenKind::Symbol("<".into()),
                TokenKind::Symbol(">".into()),
                TokenKind::Symbol("<=".into()),
                TokenKind::Symbol(">=".into()),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            lex_kinds(":pattern :effect :every"),
            vec![
                TokenKind::Keyword("pattern".into()),
                TokenKind::Keyword("effect".into()),
                TokenKind::Keyword("every".into()),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_mixed_expression() {
        assert_eq!(
            lex_kinds("(+ 1 2)"),
            vec![
                TokenKind::LParen,
                TokenKind::Symbol("+".into()),
                TokenKind::Int(1),
                TokenKind::Int(2),
                TokenKind::RParen,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_comments() {
        assert_eq!(
            lex_kinds("; comment\n42"),
            vec![TokenKind::Int(42), TokenKind::Eof]
        );
    }

    #[test]
    fn test_inline_comment() {
        assert_eq!(
            lex_kinds("(foo ; comment\n bar)"),
            vec![
                TokenKind::LParen,
                TokenKind::Symbol("foo".into()),
                TokenKind::Symbol("bar".into()),
                TokenKind::RParen,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(
            lex_kinds("  (  foo  )  "),
            vec![
                TokenKind::LParen,
                TokenKind::Symbol("foo".into()),
                TokenKind::RParen,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_span_tracking() {
        let tokens = lex("(foo 42)").unwrap();
        assert_eq!(tokens[0].span.line, 1);
        assert_eq!(tokens[0].span.column, 1);
        assert_eq!(tokens[1].span.column, 2); // 'foo' starts at column 2
        assert_eq!(tokens[2].span.column, 6); // '42' starts at column 6
    }

    #[test]
    fn test_multiline_span() {
        let tokens = lex("(\n  foo\n)").unwrap();
        assert_eq!(tokens[0].span.line, 1); // (
        assert_eq!(tokens[1].span.line, 2); // foo
        assert_eq!(tokens[2].span.line, 3); // )
    }

    #[test]
    fn test_error_unterminated_string() {
        let err = lex("\"hello").unwrap_err();
        assert!(matches!(err, LexError::UnterminatedString(_)));
    }

    #[test]
    fn test_error_invalid_escape() {
        let err = lex(r#""\x""#).unwrap_err();
        assert!(matches!(err, LexError::InvalidEscape('x', _)));
    }

    #[test]
    fn test_error_unexpected_char() {
        let err = lex("~").unwrap_err();
        assert!(matches!(err, LexError::UnexpectedChar('~', _)));
    }

    #[test]
    fn test_variable_syntax() {
        // Pattern variables like ?e should be parsed as symbols
        assert_eq!(
            lex_kinds("?e ?entity"),
            vec![
                TokenKind::Symbol("?e".into()),
                TokenKind::Symbol("?entity".into()),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_full_example() {
        let source = r#"
            (rule goat-baas
              :pattern (= (get ?e Name) "goat")
              :every 10
              :effect (emit-message "Baa!"))
        "#;
        let tokens = lex(source).unwrap();
        assert!(tokens.len() > 10);
        assert!(matches!(tokens.last().unwrap().kind, TokenKind::Eof));
    }
}
