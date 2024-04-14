use super::Parser;
use crate::command::Command;
use crate::error::ParserError;
use derivative::Derivative;

/// The `ParserRegistry` holds the parsers.
#[derive(Debug, Default, Derivative)]
pub struct ParserRegistry {
  /// The parsers.
  parsers: Vec<Box<dyn Parser>>,
}

impl ParserRegistry {
  /// Create a new `ParserRegistry`.
  pub fn new() -> Self {
    Self { parsers: Vec::new() }
  }

  /// Register a parser.
  pub fn register(&mut self, parser: Box<dyn Parser>) {
    self.parsers.push(parser);
  }

  /// Parse the input.
  pub fn parse(&self, input: &str) -> Result<Box<dyn Command>, ParserError> {
    for parser in &self.parsers {
      match parser.parse(input) {
        Ok(command) => return Ok(command),
        Err(_) => continue,
      }
    }
    Err(ParserError::CouldNotParseInput)
  }
}
