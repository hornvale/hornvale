use crate::prelude::{ParserError, Token, TokenKind};

/// A simple scanner for breaking input into tokens.
#[derive(Clone, Debug)]
pub struct Scanner {
  /// The input to scan.
  pub input: String,
  /// Where the current token started.
  pub start: usize,
  /// The current position in the input.
  pub current: usize,
  /// The tokens that have been scanned so far.
  pub tokens: Vec<Token>,
}

// There are many opportunities for optimization here, but we'll keep it
// simple for now. Once it is implemented and working, I'll benchmark it and
// see if there are any bottlenecks that need to be addressed. Currently,
// there are a lot of string allocations, etc.

impl Scanner {
  /// Create a new scanner.
  pub fn new(input: &str) -> Self {
    let input = input.to_string();
    let start = 0;
    let current = 0;
    let tokens = Vec::new();
    Self {
      input,
      start,
      current,
      tokens,
    }
  }

  /// Scan the input into tokens.
  pub fn scan_tokens(&mut self) -> Result<Vec<Token>, ParserError> {
    while !self.is_at_end() {
      self.start = self.current;
      let token = self.scan_token()?;
      self.tokens.push(token);
    }
    self.tokens.push(Token {
      kind: TokenKind::EndOfInput,
      lexeme: "".to_string(),
    });
    // Optimization: take the vector instead of cloning it.
    Ok(self.tokens.clone())
  }

  /// Scan the next token.
  pub fn scan_token(&mut self) -> Result<Token, ParserError> {
    self.skip_whitespace()?;
    self.start = self.current;
    if self.is_at_end() {
      return Ok(self.make_token(TokenKind::EndOfInput));
    }
    let character = self.advance()?;
    let result = match character {
      ',' => self.make_token(TokenKind::Comma),
      '.' => self.make_token(TokenKind::Period),
      ';' => self.make_token(TokenKind::Semicolon),
      '!' => self.make_token(TokenKind::Bang),
      '?' => self.make_token(TokenKind::Question),
      '@' => self.make_token(TokenKind::At),
      '#' => self.make_token(TokenKind::Hash),
      '$' => self.make_token(TokenKind::Dollar),
      '%' => self.make_token(TokenKind::Percent),
      '^' => self.make_token(TokenKind::Caret),
      '&' => self.make_token(TokenKind::Ampersand),
      '*' => self.make_token(TokenKind::Asterisk),
      '/' => self.make_token(TokenKind::ForwardSlash),
      '\\' => self.make_token(TokenKind::BackSlash),
      '(' => self.make_token(TokenKind::LeftParenthesis),
      ')' => self.make_token(TokenKind::RightParenthesis),
      '[' => self.make_token(TokenKind::LeftSquareBracket),
      ']' => self.make_token(TokenKind::RightSquareBracket),
      '{' => self.make_token(TokenKind::LeftCurlyBrace),
      '}' => self.make_token(TokenKind::RightCurlyBrace),
      '<' => self.make_token(TokenKind::LessThan),
      '>' => self.make_token(TokenKind::GreaterThan),
      '=' => self.make_token(TokenKind::Equals),
      '+' => self.make_token(TokenKind::Plus),
      '-' => self.make_token(TokenKind::Minus),
      '|' => self.make_token(TokenKind::Pipe),
      ':' => self.make_token(TokenKind::Colon),
      '_' => self.make_token(TokenKind::Underscore),
      '~' => self.make_token(TokenKind::Tilde),
      '`' => self.make_token(TokenKind::Backtick),
      '"' | '\'' => self.match_string(character)?,
      char if char.is_ascii_digit() => self.match_number_or_ordinal()?,
      char if self.is_alpha(char) => self.match_word()?,
      _ => return Err(ParserError::UnexpectedCharacter(character)),
    };
    Ok(result)
  }

  /// Advance one character through the source and return it.
  pub fn advance(&mut self) -> Result<char, ParserError> {
    let position = self.current;
    self.current += 1;
    self
      .input
      .chars()
      .nth(position)
      .ok_or(ParserError::CharacterOutOfBounds)
  }

  /// Create a token based on a token type.
  pub fn make_token(&self, kind: TokenKind) -> Token {
    let lexeme = self.get_lexeme();
    Token { kind, lexeme }
  }

  /// Try to match and create a token out of a number or ordinal.
  pub fn match_number_or_ordinal(&mut self) -> Result<Token, ParserError> {
    // Keep going as long as we have digits; this will handle both numbers and
    // ordinals.
    while self.peek().is_ascii_digit() {
      self.advance()?;
    }
    // We can encounter either a letter or whitespace after a number, so we
    // need to check for both.
    if self.is_alpha(self.peek()) {
      // If it's a letter, assume it's an ordinal, and just keep going.
      while self.is_alpha(self.peek()) {
        self.advance()?;
      }
      let result = self.make_token(TokenKind::Ordinal);
      Ok(result)
    } else {
      // If it's not a letter, it's a number.
      let result = self.make_token(TokenKind::NumberLiteral);
      Ok(result)
    }
  }

  /// Try to match and create a token out of a string.
  pub fn match_string(&mut self, start_char: char) -> Result<Token, ParserError> {
    // We can begin a string with either a single or double quote.
    // We'll use the start_char to determine which one we're looking for.
    while self.peek() != start_char && !self.is_at_end() {
      self.advance()?;
    }
    let result = if self.is_at_end() {
      return Err(ParserError::UnterminatedStringLiteral);
    } else {
      // Consume the closing quote.
      self.advance()?;
      self.make_token(TokenKind::StringLiteral)
    };
    Ok(result)
  }

  /// Try to match and create a token out of a word.
  pub fn match_word(&mut self) -> Result<Token, ParserError> {
    while self.is_word_char(self.peek()) {
      self.advance()?;
    }
    let value = self.get_lexeme();
    let value_type = match TokenKind::try_from(value.as_str()) {
      Ok(token_type) => token_type,
      Err(_) => TokenKind::Word,
    };
    let result = self.make_token(value_type);
    Ok(result)
  }

  /// Is the character a word character?
  pub fn is_word_char(&self, char: char) -> bool {
    self.is_alpha(char) || char == '-' || char == '\''
  }

  /// Is the character a word character?
  pub fn is_alpha_numeric(&self, char: char) -> bool {
    char.is_ascii_digit() || self.is_alpha(char)
  }

  /// Is the character a letter?
  pub fn is_alpha(&self, char: char) -> bool {
    char.is_ascii_lowercase() || char.is_ascii_uppercase()
  }

  /// Get the lexeme from the start and current position.
  pub fn get_lexeme(&self) -> String {
    self.input[self.start..self.current].to_string()
  }

  /// Are we at the end of the input?
  pub fn is_at_end(&self) -> bool {
    self.current >= self.input.len()
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
    match self.current + offset >= self.input.len() {
      true => '\0',
      false => self.input.chars().nth(self.current + offset).unwrap(),
    }
  }

  /// Skip all the whitespace!
  pub fn skip_whitespace(&mut self) -> Result<(), ParserError> {
    loop {
      match self.peek() {
        '\n' | ' ' | '\r' | '\t' => {
          self.advance()?;
        },
        _ => break Ok(()),
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_scan_tokens() {
    let input = "take sword and shield";
    let mut scanner = Scanner::new(input);
    let tokens = scanner.scan_tokens();
    let expected = vec![
      Token {
        kind: TokenKind::Word,
        lexeme: "take".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "sword".to_string(),
      },
      Token {
        kind: TokenKind::And,
        lexeme: "and".to_string(),
      },
      Token {
        kind: TokenKind::Word,
        lexeme: "shield".to_string(),
      },
      Token {
        kind: TokenKind::EndOfInput,
        lexeme: "".to_string(),
      },
    ];
    assert_eq!(tokens, Ok(expected));
  }

  #[test]
  fn test_scan_token() {
    let input = "take sword";
    let mut scanner = Scanner::new(input);
    let token = scanner.scan_token().unwrap();
    let expected = Token {
      kind: TokenKind::Word,
      lexeme: "take".to_string(),
    };
    assert_eq!(token, expected);
  }

  #[test]
  fn test_advance() {
    let input = "take sword";
    let mut scanner = Scanner::new(input);
    let character = scanner.advance();
    assert_eq!(character, Ok('t'));
  }

  #[test]
  fn test_is_at_end() {
    let input = "take sword";
    let scanner = Scanner::new(input);
    assert_eq!(scanner.is_at_end(), false);
  }

  #[test]
  fn test_peek() {
    let input = "take sword";
    let scanner = Scanner::new(input);
    assert_eq!(scanner.peek(), 't');
  }

  #[test]
  fn test_peek_next() {
    let input = "take sword";
    let scanner = Scanner::new(input);
    assert_eq!(scanner.peek_next(), 'a');
  }

  #[test]
  fn test_peek_at_offset() {
    let input = "take sword";
    let scanner = Scanner::new(input);
    assert_eq!(scanner.peek_at_offset(1), 'a');
  }

  #[test]
  fn test_skip_whitespace() {
    let input = "  take sword";
    let mut scanner = Scanner::new(input);
    scanner.skip_whitespace().unwrap();
    assert_eq!(scanner.peek(), 't');
  }

  #[test]
  fn test_make_token() {
    let input = "take sword";
    let mut scanner = Scanner::new(input);
    scanner.start = 0;
    scanner.current = 4;
    let token = scanner.make_token(TokenKind::Word);
    let expected = Token {
      kind: TokenKind::Word,
      lexeme: "take".to_string(),
    };
    assert_eq!(token, expected);
  }
}