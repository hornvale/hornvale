use crate::command::CommandContextDataTrait;

/// The `CommandContext` struct, which represents the context of a command
/// execution.
pub trait CommandContext<'data> {
  type Data: CommandContextDataTrait + 'data;
  fn get_data(&self) -> &Self::Data;
  fn get_data_mut(&mut self) -> &mut Self::Data;
}
