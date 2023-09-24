use crate::command::CommandContextDataTrait;
use crate::command::CommandContextTrait;

/// The `Context` struct, which represents the context of a command execution.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Context<'context> {
  /// All data.
  #[derivative(Debug = "ignore")]
  data: &'context mut dyn CommandContextDataTrait,
}

impl<'context> Context<'context> {
  pub fn new(data: &'context mut dyn CommandContextDataTrait) -> Self {
    Context { data }
  }
}

impl CommandContextTrait for Context<'_> {
  fn get_data(&self) -> &dyn CommandContextDataTrait {
    self.data
  }

  fn get_data_mut(&mut self) -> &mut dyn CommandContextDataTrait {
    self.data
  }
}
