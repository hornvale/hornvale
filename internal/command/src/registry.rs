use crate::prelude::CommandError;
use hecs::{Entity, World};
use hornvale_core::prelude::*;
use std::collections::HashMap;

/// A registry for commands.
///
/// This is a simple collection of commands, keyed by the name and form.
/// The form, or modifier, may be an adverb or a preposition, for example.
#[derive(Debug, Default)]
pub struct CommandRegistry {
  /// The commands in the registry.
  pub commands: HashMap<&'static str, HashMap<CommandSyntax, CommandFunction>>,
}

impl CommandRegistry {
  /// Create a new command registry.
  pub fn new() -> Self {
    Self::default()
  }

  /// Register a command in the registry.
  pub fn register<C: Command>(&mut self) {
    let entry = self.commands.entry(C::NAME).or_default();
    entry.insert(C::SYNTAX, C::execute);
    for &synonym in C::SYNONYMS {
      let entry = self.commands.entry(synonym).or_default();
      entry.insert(C::SYNTAX, C::execute);
    }
  }

  /// Do we have a command in the registry?
  pub fn has_command(&self, name: &str) -> bool {
    self.commands.contains_key(name)
  }

  /// Do we have this form of a command in the registry?
  pub fn has_form(&self, name: &str, syntax: &CommandSyntax) -> bool {
    self
      .commands
      .get(name)
      .map_or(false, |entry| entry.contains_key(syntax))
  }

  /// Get a command from the registry.
  pub fn get(&self, name: &str, syntax: &CommandSyntax) -> Option<&CommandFunction> {
    self.commands.get(name).and_then(|entry| entry.get(syntax))
  }

  /// Get the syntaxes of a command in the registry.
  pub fn get_syntaxes(&self, name: &str) -> Option<Vec<CommandSyntax>> {
    self.commands.get(name).map(|entry| entry.keys().cloned().collect())
  }

  /// Execute a command from the registry.
  pub fn execute(
    &self,
    name: &str,
    syntax: &CommandSyntax,
    world: &mut World,
    context: &Entity,
  ) -> Result<(), CommandError> {
    if let Some(&command) = self.get(name, syntax) {
      Ok(command(world, context)?)
    } else {
      Err(CommandError::UnknownCommand(name.to_string()))
    }
  }
}
