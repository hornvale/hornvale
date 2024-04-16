use crate::prelude::{CommandContext, CommandError};
use hecs::World;

/// The context of the command.
pub mod context;
/// The command execute() function type.
pub mod function;

/// A command that can be executed.
pub trait Command {
  /// Get the name of the command.
  const NAME: &'static str;
  /// Get synonyms for the command.
  const SYNONYMS: &'static [&'static str];
  /// A brief description of the command.
  const BRIEF: &'static str;
  /// A longer description of the command.
  const DESCRIPTION: &'static str;

  /// Execute the command.
  fn execute(world: &mut World, context: &CommandContext) -> Result<(), CommandError>;
}
