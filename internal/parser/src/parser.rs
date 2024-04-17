use crate::error::ParserError;
use crate::prelude::{Token, TokenKind};
use derivative::Derivative;
use hecs::World;
use hornvale_command::prelude::*;

/// The parser, a simple top-down recursive descent parser.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Parser<'world> {
  /// A list of tokens to parse.
  pub tokens: Vec<Token>,
  /// The world to parse in.
  #[derivative(Debug = "ignore")]
  pub world: &'world mut World,
  /// The current index.
  pub current: usize,
  /// The detected verb.
  pub verb: Option<String>,
}

impl<'world> Parser<'world> {
  /// Create a new parser.
  pub fn new(tokens: Vec<Token>, world: &'world mut World) -> Self {
    let current = 0;
    let verb = None;
    Self {
      tokens,
      world,
      current,
      verb,
    }
  }

  /// Parse a command and its context from the input.
  ///
  /// This is the main entry point for the parser.
  ///
  /// input → command
  pub fn parse(&mut self) -> Result<(&CommandFunction, CommandContext), ParserError> {
    self.parse_command()
  }

  /// Parse a command from the input.
  ///
  /// command → verb-phrase (object-phrase)?
  pub fn parse_command(&mut self) -> Result<(&CommandFunction, CommandContext), ParserError> {
    if self.check_token(TokenKind::EndOfInput) {
      // No input provided.
      return Err(ParserError::NoInput);
    }
    self.consume_verb().map_err(|_| ParserError::NoVerb)?;
    unimplemented!()
  }

  /// Match current token.
  pub fn match_token(&mut self, kind: TokenKind) -> Result<bool, ParserError> {
    if !self.check_token(kind) {
      return Ok(false);
    }
    self.advance()?;
    Ok(true)
  }

  /// Consume the verb.
  pub fn consume_verb(&mut self) -> Result<(), ParserError> {
    if self.check_token(TokenKind::Word) {
      self.verb = self.peek().map(|t| t.lexeme.clone());
      self.advance()?;
      Ok(())
    } else {
      Err(ParserError::NoVerb)
    }
  }

  /// Consume the current token, throwing an error if it is not the expected kind.
  pub fn consume_token(&mut self, expected: TokenKind, message: &str) -> Result<(), ParserError> {
    let current_kind = self.peek().map(|t| t.kind).unwrap_or(TokenKind::EndOfInput);
    if current_kind == expected {
      self.advance()?;
      Ok(())
    } else {
      Err(ParserError::UnexpectedToken(
        expected,
        current_kind,
        message.to_string(),
      ))
    }
  }

  /// Check kind of current token.
  pub fn check_token(&mut self, kind: TokenKind) -> bool {
    self.peek().is_some() && self.peek().unwrap().kind == kind
  }

  /// Advance to the next token.
  pub fn advance(&mut self) -> Result<(), ParserError> {
    if !self.is_at_end() {
      self.current += 1;
    }
    Ok(())
  }

  /// Check if at end of input.
  pub fn is_at_end(&mut self) -> bool {
    self.check_token(TokenKind::EndOfInput)
  }

  /// Peek at the current token.
  pub fn peek(&self) -> Option<Token> {
    self.tokens.get(self.current).cloned()
  }

  /// Get the previous token.
  pub fn previous(&self) -> Option<Token> {
    self.tokens.get(self.current - 1).cloned()
  }

  /// Peek at the next token.
  pub fn peek_next(&self) -> Option<Token> {
    self.tokens.get(self.current + 1).cloned()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::prelude::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_parse() {
    init();
    let mut world = World::new();
    let mut scanner = Scanner::new("look at the table");
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(tokens, &mut world);
    let (command, context) = parser.parse().unwrap();
  }

  #[test]
  fn test_parse_no_input() {
    init();
    let mut world = World::new();
    let tokens = vec![];
    let mut parser = Parser::new(tokens, &mut world);
    let result = parser.parse();
    assert!(result.is_err());
  }

  #[test]
  fn test_parse_no_verb() {
    init();
    let mut world = World::new();
    let tokens = vec![Token {
      kind: TokenKind::Word,
      lexeme: "at".to_string(),
    }];
    let mut parser = Parser::new(tokens, &mut world);
    let result = parser.parse();
    assert!(result.is_err());
    assert!(result.err().unwrap() == ParserError::NoVerb);
  }

  #[test]
  fn test_parse_unexpected_token() {
    init();
    let mut world = World::new();
    let tokens = vec![Token {
      kind: TokenKind::Word,
      lexeme: "at".to_string(),
    }];
    let mut parser = Parser::new(tokens, &mut world);
    let result = parser.consume_token(TokenKind::Word, "Expected a word");
    assert_eq!(
      result,
      Err(ParserError::UnexpectedToken(
        TokenKind::Word,
        TokenKind::Word,
        "Expected a word".to_string()
      ))
    );
  }
}
