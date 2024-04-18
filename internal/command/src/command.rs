use crate::prelude::{CommandArity, CommandContext, CommandError, CommandForm};
use hecs::World;

/// A command that can be executed.
pub trait Command {
  /// Get the base name of the command. This is the name that the player will
  /// use to invoke the command.
  const NAME: &'static str;
  /// Get synonyms for the command. These are other names that the player can
  /// use to invoke the command.
  const SYNONYMS: &'static [&'static str];
  /// A brief description of the command. This is used in the help text.
  const BRIEF: &'static str;
  /// A longer description of the command. This is used in the help text.
  const DESCRIPTION: &'static str;
  /// Get the form of the command. This informs the parser how to match input
  /// to the appropriate command.
  const FORM: CommandForm;
  /// Get the arity of the command. This is how many arguments the command
  /// takes.
  const ARITY: CommandArity;

  /// Execute the command.
  fn execute(world: &mut World, context: &CommandContext) -> Result<(), CommandError>;
}
