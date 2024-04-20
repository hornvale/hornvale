use crate::token::kind::TokenKind;
use crate::token::Token;
use derive_more::Display;
use serde::{Deserialize, Serialize};
use std::str::FromStr;

/// An error type for the scanner.
pub mod error;
use error::Error;

/// The `Scanner` type.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
#[display(fmt = "start: {}, current: {}, line: {}", start, current, line_number)]
pub struct Scanner<'source> {
  /// Where this token started.
  pub start: usize,
  /// Where we are currently.
  pub current: usize,
  /// The current line number.
  pub line_number: usize,
  /// The source code.
  pub source: &'source str,
  /// The source bytes.
  pub source_bytes: Vec<u8>,
}

impl<'source> Scanner<'source> {
  /// Constructor.
  pub fn new(source: &'source str) -> Self {
    let start = 0;
    let current = 0;
    let line_number = 1;
    let source_bytes = source.as_bytes().to_vec();
    Self {
      start,
      current,
      line_number,
      source,
      source_bytes,
    }
  }

  /// Scan a token.
  pub fn scan_token(&mut self) -> Result<Token<'source>, Error> {
    self.skip_whitespace();
    self.start = self.current;
    use TokenKind::*;
    if self.is_at_end() {
      return Ok(self.make_token(Eof));
    }
    let char = self.advance();
    let result = match char {
      '(' => self.make_token(LeftParenthesis),
      ')' => self.make_token(RightParenthesis),
      '{' => self.make_token(LeftBrace),
      '}' => self.make_token(RightBrace),
      ',' => self.make_token(Comma),
      '.' => self.make_token(Dot),
      '-' => self.make_token(Minus),
      '+' => self.make_token(Plus),
      ';' => self.make_token(Semicolon),
      '*' => self.make_token(Star),
      '/' => self.make_token(Slash),
      '!' => match self.match_current('=') {
        true => self.make_token(BangEqual),
        false => self.make_token(Bang),
      },
      '=' => match self.match_current('=') {
        true => self.make_token(EqualEqual),
        false => self.make_token(Equal),
      },
      '>' => match self.match_current('=') {
        true => self.make_token(GreaterThanOrEqual),
        false => self.make_token(GreaterThan),
      },
      '<' => match self.match_current('=') {
        true => self.make_token(LessThanOrEqual),
        false => self.make_token(LessThan),
      },
      '"' => self.match_string()?,
      char if char.is_ascii_digit() => self.match_number()?,
      char if self.is_alpha(char) => self.match_identifier()?,
      _ => return Err(error::Error::UnexpectedCharacter(char)),
    };
    Ok(result)
  }

  /// Advance one character through the source and return it.
  #[inline]
  pub fn advance(&mut self) -> char {
    let position = self.current;
    self.current += 1;
    self.source_bytes[position] as char
  }

  /// Are we at the end of the source?
  pub fn is_at_end(&self) -> bool {
    self.current >= self.source.len()
  }

  /// Create a token based on a token type.
  pub fn make_token(&self, kind: TokenKind) -> Token<'source> {
    let lexeme = self.get_lexeme();
    let line_number = self.line_number;
    Token {
      kind,
      lexeme,
      line_number,
    }
  }

  /// Get the lexeme from the start and current position.
  pub fn get_lexeme(&self) -> &'source str {
    &self.source[self.start..self.current]
  }

  /// Does the current character match the one specified?
  pub fn match_current(&mut self, char: char) -> bool {
    if self.is_at_end() {
      return false;
    }
    if self.source_bytes[self.current] as char != char {
      return false;
    }
    self.current += 1;
    true
  }

  /// Try to match and create a token out of a number.
  pub fn match_number(&mut self) -> Result<Token<'source>, Error> {
    while self.peek().is_ascii_digit() {
      self.advance();
    }
    if self.peek() == '.' && self.peek_next().is_ascii_digit() {
      self.advance();
      while self.peek().is_ascii_digit() {
        self.advance();
      }
    }
    let _value = &self.source[self.start..self.current];
    let result = self.make_token(TokenKind::Number);
    Ok(result)
  }

  /// Try to match and create a token out of a string.
  pub fn match_string(&mut self) -> Result<Token<'source>, Error> {
    while self.peek() != '"' && !self.is_at_end() {
      if self.peek() == '\n' {
        self.line_number += 1;
      }
      self.advance();
    }
    let result = if self.is_at_end() {
      self.get_error_token("Unterminated string.")
    } else {
      self.advance();
      self.make_token(TokenKind::String)
    };
    Ok(result)
  }

  /// Try to match and create a token out of an identifier.
  pub fn match_identifier(&mut self) -> Result<Token<'source>, Error> {
    while self.is_alpha_numeric(self.peek()) {
      self.advance();
    }
    let value = &self.source[self.start..self.current];
    let value_type = match TokenKind::from_str(value) {
      Ok(token_type) => token_type,
      Err(_) => TokenKind::Identifier,
    };
    let result = self.make_token(value_type);
    Ok(result)
  }

  /// Is the character an alpha character?
  pub fn is_alpha(&self, char: char) -> bool {
    char.is_ascii_alphabetic() || char == '_'
  }

  /// Is the character an alpha-numeric character?
  pub fn is_alpha_numeric(&self, char: char) -> bool {
    char.is_ascii_digit() || self.is_alpha(char)
  }

  /// Get an error token.
  pub fn get_error_token(&self, message: &'static str) -> Token<'source> {
    let mut result = Token::synthesize(message);
    result.line_number = self.line_number;
    result
  }

  /// Match a single-line comment.
  pub fn match_line_comment(&mut self) {
    while self.peek() != '\n' && !self.is_at_end() {
      self.advance();
    }
  }

  /// Match a multi-line comment.
  pub fn match_multiline_comment(&mut self) {
    while !(self.is_at_end() || self.peek_next() == '*' && self.peek_at_offset(2) == '/') {
      self.advance();
    }
    // Last trailing non-asterisk, non-forward-slash character.
    self.advance();
    // Trailing asterisk.
    self.advance();
    // Trailing forward slash.
    self.advance();
  }

  /// Peek at the current character, but don't advance.
  pub fn peek(&self) -> char {
    self.peek_at_offset(0)
  }

  /// Peek at the next character.
  pub fn peek_next(&self) -> char {
    self.peek_at_offset(1)
  }

  /// Peek at a character at a specified offset.
  pub fn peek_at_offset(&self, offset: usize) -> char {
    match self.current + offset >= self.source.len() {
      true => '\0',
      false => self.source_bytes[self.current + offset] as char,
    }
  }

  /// Skip all the whitespace!
  pub fn skip_whitespace(&mut self) {
    loop {
      match self.peek() {
        '\n' => {
          self.line_number += 1;
          self.advance();
        },
        ' ' | '\r' | '\t' => {
          self.advance();
        },
        '/' => match self.peek_next() {
          '/' => self.match_line_comment(),
          '*' => self.match_multiline_comment(),
          _ => break,
        },
        _ => break,
      }
    }
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  pub fn test_scanner() {
    init();
    use TokenKind::*;
    test_scanner_tokens!(
      "",
      [Ok(Token {
        kind: Eof,
        lexeme: "",
        line_number: 1,
      })]
    );
    let test_cases = vec![
      ("(", LeftParenthesis),
      (")", RightParenthesis),
      ("{", LeftBrace),
      ("}", RightBrace),
      (",", Comma),
      (".", Dot),
      ("-", Minus),
      ("+", Plus),
      (";", Semicolon),
      ("*", Star),
      ("/", Slash),
      ("/", Slash),
      ("!", Bang),
      ("<", LessThan),
      (">", GreaterThan),
    ];
    for (string, kind) in test_cases.iter() {
      test_scanner_tokens!(
        string,
        [Ok(Token {
          kind: *kind,
          lexeme: string,
          line_number: 1,
        })]
      );
    }
    let test_cases2 = vec![
      ("!=", BangEqual),
      ("==", EqualEqual),
      (">=", GreaterThanOrEqual),
      ("<=", LessThanOrEqual),
    ];
    for (string, kind) in test_cases2.iter() {
      test_scanner_tokens!(
        string,
        [Ok(Token {
          kind: *kind,
          lexeme: string,
          line_number: 1,
        })]
      );
    }
    test_scanner_tokens!(
      "/a",
      [
        Ok(Token {
          kind: Slash,
          lexeme: "/",
          line_number: 1,
        }),
        Ok(Token {
          kind: Identifier,
          lexeme: "a",
          line_number: 1,
        })
      ]
    );
    test_scanner_tokens!(
      "/* TEST */",
      [Ok(Token {
        kind: Eof,
        lexeme: "",
        line_number: 1,
      })]
    );
    test_scanner_tokens!(
      "1312.1231215123",
      [Ok(Token {
        kind: Number,
        lexeme: "1312.1231215123",
        line_number: 1,
      })]
    );
    test_scanner_tokens!(
      "\"goat\"",
      [Ok(Token {
        kind: String,
        lexeme: "\"goat\"",
        line_number: 1,
      })]
    );
    test_scanner_tokens!(
      "// single-line comment",
      [Ok(Token {
        kind: Eof,
        lexeme: "",
        line_number: 1,
      })]
    );
    test_scanner_tokens!(
      "\n",
      [Ok(Token {
        kind: Eof,
        lexeme: "",
        line_number: 2,
      })]
    );
  }
}
