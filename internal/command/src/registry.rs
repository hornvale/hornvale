use crate::prelude::{Command, CommandContext, CommandError, CommandForm, CommandFunction};
use hecs::World;
use std::collections::HashMap;

/// A registry for commands.
///
/// This is a simple collection of commands, keyed by the name and form.
/// The form, or modifier, may be an adverb or a preposition, for example.
#[derive(Debug, Default)]
pub struct CommandRegistry {
  /// The commands in the registry.
  pub commands: HashMap<&'static str, HashMap<CommandForm, CommandFunction>>,
}

impl CommandRegistry {
  /// Create a new command registry.
  pub fn new() -> Self {
    Self::default()
  }

  /// Register a command in the registry.
  pub fn register<C: Command>(&mut self) {
    let entry = self.commands.entry(C::NAME).or_default();
    entry.insert(C::FORM, C::execute);
    for &synonym in C::SYNONYMS {
      let entry = self.commands.entry(synonym).or_default();
      entry.insert(C::FORM, C::execute);
    }
  }

  /// Do we have a command in the registry?
  pub fn has_command(&self, name: &str) -> bool {
    self.commands.contains_key(name)
  }

  /// Do we have this form of a command in the registry?
  pub fn has_form(&self, name: &str, form: &CommandForm) -> bool {
    self.commands.get(name).map_or(false, |entry| entry.contains_key(form))
  }

  /// Get a command from the registry.
  pub fn get(&self, name: &str, form: &CommandForm) -> Option<&CommandFunction> {
    self.commands.get(name).and_then(|entry| entry.get(form))
  }

  /// Get the forms of a command in the registry.
  pub fn get_forms(&self, name: &str) -> Option<Vec<&CommandForm>> {
    self.commands.get(name).map(|entry| entry.keys().collect())
  }

  /// Execute a command from the registry.
  pub fn execute(
    &self,
    name: &str,
    form: &CommandForm,
    world: &mut World,
    context: &CommandContext,
  ) -> Result<(), CommandError> {
    if let Some(&command) = self.get(name, form) {
      command(world, context)
    } else {
      Err(CommandError::UnknownCommand(name.to_string()))
    }
  }
}
