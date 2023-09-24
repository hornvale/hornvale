use crate::command::CommandTrait;

/// A parsing strategy is a way to parse a string into a Command.
pub trait ParsingStrategy: Send {
  // Try to parse the input and produce a Command.
  fn parse(&self, input: &str) -> Option<Box<dyn CommandTrait>>;
}
