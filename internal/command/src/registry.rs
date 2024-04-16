use crate::prelude::{Command, CommandContext, CommandError, CommandFunction};
use hecs::World;
use std::collections::HashMap;

/// A registry for commands.
///
/// This is a simple collection of commands, keyed by a name-modifier pair.
/// The modifier may be an adverb or a preposition, for example.
///
/// Examples:
/// - `take`
/// - `quit`
/// - `look`
/// - `look-at`
/// - `look-behind`
/// - `look-in`
/// - `look-under`
#[derive(Debug, Default)]
pub struct CommandRegistry {
  /// The commands in the registry.
  pub commands: HashMap<&'static str, CommandFunction>,
}

impl CommandRegistry {
  /// Create a new command registry.
  pub fn new() -> Self {
    Self::default()
  }

  /// Register a command in the registry.
  pub fn register<C: Command>(&mut self) {
    self.commands.insert(C::NAME, C::execute);
    for alias in C::SYNONYMS {
      self.commands.insert(alias, C::execute);
    }
  }

  /// Get a command from the registry.
  pub fn get(&self, name: &str) -> Option<&CommandFunction> {
    self.commands.get(name)
  }

  /// Execute a command from the registry.
  pub fn execute(&self, name: &str, world: &mut World, context: &CommandContext) -> Result<(), CommandError> {
    if let Some(&command) = self.get(name) {
      command(world, context)
    } else {
      Err(CommandError::UnknownCommand(name.to_string()))
    }
  }
}
