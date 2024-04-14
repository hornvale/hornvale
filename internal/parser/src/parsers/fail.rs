use crate::prelude::{Parser, ParserError};
use hornvale_command::prelude::*;

/// A parser that always fails.
#[derive(Clone, Copy, Debug)]
pub struct FailParser;

impl Parser for FailParser {
  fn name(&self) -> &str {
    "fail"
  }
  fn parse(&self, _input: &str) -> Result<Box<dyn Command>, ParserError> {
    Err(ParserError::UnknownError)
  }
}
