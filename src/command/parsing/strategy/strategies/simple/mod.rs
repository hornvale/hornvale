use crate::command::CommandFactory;
use crate::command::CommandTrait;
use crate::command::ParsingStrategy;

/// A simple verb-noun parsing strategy, similar to that of a MUD.
///
/// Many MUDs use a simple verb-noun parsing strategy, where the first word is
/// the verb and the second word is the noun. This is a simple implementation of
/// that strategy.
///
/// This strategy is not very flexible, but it is easy to implement and easy to
/// understand, and should be very fast.
#[derive(Debug)]
pub struct Simple {
  /// The command factory.
  pub factory: CommandFactory,
}

impl Simple {
  /// Create a new instance.
  pub fn new() -> Self {
    let factory = CommandFactory::default();
    Self { factory }
  }
}

impl Default for Simple {
  fn default() -> Self {
    Self::new()
  }
}

impl ParsingStrategy for Simple {
  /// Parse input.
  fn parse(&self, input: &str) -> Option<Box<dyn CommandTrait>> {
    let input = input.trim();
    if input.is_empty() {
      return None;
    }
    let input = input.to_lowercase();
    let words = input
      .split_whitespace()
      .map(|s| s.trim())
      .filter(|s| !s.is_empty())
      .collect::<Vec<&str>>();
    let command_name = words[0];
    let command = self.factory.create_command(command_name);
    command.as_ref()?;
    let command = command.unwrap();
    Some(command)
  }
}
