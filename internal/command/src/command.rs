use crate::prelude::CommandError;
use hecs::World;
use std::fmt::{Debug, Formatter, Result as FmtResult};

/// A command that can be executed.
pub trait Command {
  /// Get the name of the command.
  fn name(&self) -> &str;
  /// Execute the command.
  fn execute(&self, world: &World) -> Result<(), CommandError>;
}

impl Debug for dyn Command {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(f, "{}", self.name())
  }
}
