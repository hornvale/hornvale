use crate::command::Command;

/// A parsing strategy is a way to parse a string into a Command.
pub trait Strategy: Send {
  // Try to parse the input and produce a Command.
  fn parse(&self, input: &str) -> Option<Box<dyn Command>>;
}
