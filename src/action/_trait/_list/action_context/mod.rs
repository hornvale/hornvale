use crate::action::ActionContextDataTrait;

/// The `ActionContext` struct, which represents the context of an action
/// execution.
pub trait ActionContext<'data> {
  type Data: ActionContextDataTrait + 'data;
  fn get_data(&self) -> &Self::Data;
  fn get_data_mut(&mut self) -> &mut Self::Data;
}
