use super::registry::ParserRegistry;
use super::Parser;
use crate::prelude::ParserError;
use hecs::World;
use hornvale_command::prelude::*;

/// The `ParserManager` dispatches parsing requests to the parsers.
#[derive(Debug, Default)]
pub struct ParserManager {
  /// The parser registry.
  registry: ParserRegistry,
}

impl ParserManager {
  /// Create a new `ParserManager`.
  pub fn new() -> Self {
    Self {
      registry: ParserRegistry::new(),
    }
  }

  /// Register a parser.
  pub fn register(&mut self, parser: Box<dyn Parser>) {
    self.registry.register(parser);
  }

  /// Parse the input.
  pub fn parse(&self, input: &str, world: &World) -> Result<(Box<dyn Command>, CommandContext), ParserError> {
    self.registry.parse(input, world)
  }
}
