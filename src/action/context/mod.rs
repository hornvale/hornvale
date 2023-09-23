use crate::action::ActionContextDataTrait;
use crate::action::ActionContextTrait;

/// The `Context` struct, which represents the context of a command execution.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Context<'context> {
  /// All data.
  #[derivative(Debug = "ignore")]
  pub data: &'context mut dyn ActionContextDataTrait,
}

impl<'context> Context<'context> {
  pub fn new(data: &'context mut dyn ActionContextDataTrait) -> Self {
    Context { data }
  }
}

impl ActionContextTrait for Context<'_> {
  fn get_data(&self) -> &dyn ActionContextDataTrait {
    self.data
  }

  fn get_data_mut(&mut self) -> &mut dyn ActionContextDataTrait {
    self.data
  }
}
