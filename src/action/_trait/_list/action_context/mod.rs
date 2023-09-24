use crate::action::ActionContextDataTrait;

/// The `ActionContext` struct, which represents the context of an action
/// execution.
pub trait ActionContext<'context> {
  type Data: ActionContextDataTrait + 'context;
  fn get_data(&self) -> &Self::Data;
  fn get_data_mut(&mut self) -> &mut Self::Data;
}
