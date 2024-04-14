use crate::error::ParserError;
use hecs::World;
use hornvale_command::prelude::*;
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// The `ParserManager` dispatches parsing requests to the parsers.
pub mod manager;
/// The `ParserRegistry` holds the parsers.
pub mod registry;

/// The `Parser` trait.
pub trait Parser {
  /// Get the name of the parser.
  fn name(&self) -> &str;
  /// Parse the input.
  fn parse(&self, input: &str, world: &World) -> Result<(Box<dyn Command>, CommandContext), ParserError>;
}

impl Debug for dyn Parser {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name())
  }
}
