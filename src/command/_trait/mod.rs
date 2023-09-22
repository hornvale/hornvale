use std::fmt::Debug;

use crate::command::context::Context as CommandContext;
use crate::command::error::Error as CommandError;
use crate::command::result::Result as CommandResult;

/// The `Command` trait.
///
/// This trait is used to represent a command.
pub trait Command: Debug + Send + Sync {
  /// Get the name of the command.
  fn get_name(&self) -> &str;
  /// Execute the command.
  fn execute(&self, context: &mut CommandContext) -> Result<CommandResult, CommandError>;
}
