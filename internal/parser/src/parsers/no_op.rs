use crate::prelude::{Parser, ParserError};
use hornvale_command::prelude::*;

/// A parser that always returns the no-op command.
#[derive(Clone, Copy, Debug)]
pub struct NoOpParser;

impl Parser for NoOpParser {
  fn name(&self) -> &str {
    "no-op"
  }
  fn parse(&self, _input: &str) -> Result<Box<dyn Command>, ParserError> {
    Ok(Box::new(NoOpCommand))
  }
}
