use mockall::predicate::*;
use mockall::*;

use crate::command::CommandContextDataTrait;

/// The `CommandContext` struct, which represents the context of a command
/// execution.
#[automock]
pub trait CommandContext {
  fn get_data(&self) -> &dyn CommandContextDataTrait;
  fn get_data_mut(&mut self) -> &mut dyn CommandContextDataTrait;
}
