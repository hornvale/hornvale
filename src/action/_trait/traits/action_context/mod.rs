use mockall::predicate::*;
use mockall::*;

use crate::action::ActionContextDataTrait;

/// The `ActionContext` struct, which represents the context of an action
/// execution.
#[automock]
pub trait ActionContext {
  fn get_data(&self) -> &dyn ActionContextDataTrait;
  fn get_data_mut(&mut self) -> &mut dyn ActionContextDataTrait;
}
