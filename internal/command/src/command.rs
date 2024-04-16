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
  /// Get the description of the command.
  const DESCRIPTION: &'static str;
  /// Get the aliases of the command.
  const ALIASES: &'static [&'static str];

  /// Execute the command.
  fn execute(world: &mut World, context: &CommandContext) -> Result<(), CommandError>;
}
