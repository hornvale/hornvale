use crate::error::ParserError;
use crate::prelude::{Token, TokenKind};
use derivative::Derivative;
use hecs::World;
use hornvale_command::prelude::*;

#[cfg(test)]
pub mod tests;

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
  /// The command function, if any.
  pub function: Option<CommandFunction>,
  /// The command contexts.
  pub contexts: Vec<CommandContext>,
}

impl<'world> Parser<'world> {
  /// Create a new parser.
  pub fn new(tokens: &[Token], world: &'world mut World) -> Self {
    let tokens = tokens.to_vec();
    let current = 0;
    let function = None;
    let contexts = Vec::new();
    Self {
      tokens,
      world,
      current,
      function,
      contexts,
    }
  }

  /// Parse a command and its context from the input.
  ///
  /// This is the main entry point for the parser.
  ///
  /// input → command | magic_word
  pub fn parse(&mut self) -> Result<(&CommandFunction, Vec<CommandContext>), ParserError> {
    self.assert_non_empty()?;
    if self.match_magic_word()? {
      self.consume_magic_word()?;
      self.bind_magic_word()?;
    } else {
      self.parse_command()?;
      // self.bind_command()?;
    }
    Ok((&self.function.as_ref().unwrap(), self.contexts.clone()))
  }

  /// Parse a command from the input.
  ///
  /// command → verb-phrase (object-phrase)?
  pub fn parse_command(&mut self) -> Result<(), ParserError> {
    self.consume_verb().map_err(|_| ParserError::NoVerb)?;
    Ok(())
  }

  /// Match current token.
  pub fn match_token(&mut self, kind: TokenKind) -> Result<bool, ParserError> {
    if !self.check_token(kind) {
      return Ok(false);
    }
    self.advance()?;
    Ok(true)
  }

  /// Match a magic word.
  pub fn match_magic_word(&mut self) -> Result<bool, ParserError> {
    self.peek().map(|t| t.kind.is_magic_word()).ok_or(ParserError::NoInput)
  }

  /// Consume the verb.
  pub fn consume_verb(&mut self) -> Result<(), ParserError> {
    Ok(())
    // if self.check_token(TokenKind::Verb) {
    //   self.verb = self.peek().map(|t| t.lexeme.clone());
    //   self.advance()?;
    //   Ok(())
    // } else {
    //   Err(ParserError::NoVerb)
    // }
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

  /// Consume a magic word.
  pub fn consume_magic_word(&mut self) -> Result<(), ParserError> {
    self.consume_token(self.peek().unwrap().kind, "Expected a magic word")
  }

  /// Bind the magic word.
  pub fn bind_magic_word(&mut self) -> Result<(), ParserError> {
    // self.function = Some(CommandFunction::MagicWord);
    Ok(())
  }

  /// Check kind of current token.
  pub fn check_token(&self, kind: TokenKind) -> bool {
    self.peek().is_some() && self.peek().unwrap().kind == kind
  }

  /// Advance to the next token.
  pub fn advance(&mut self) -> Result<(), ParserError> {
    if !self.is_at_end() {
      self.current += 1;
    }
    Ok(())
  }

  /// Assert that there is input to parse.
  pub fn assert_non_empty(&self) -> Result<(), ParserError> {
    if self.tokens.is_empty() {
      return Err(ParserError::NoInput);
    }
    if self.is_at_end() {
      return Err(ParserError::NoInput);
    }
    Ok(())
  }

  /// Check if at end of input.
  pub fn is_at_end(&self) -> bool {
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
