use crate::command::CommandContextTrait;
use crate::system_data::AllData;

/// The `Context` struct, which represents the context of a command execution.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Context<'context, 'data> {
  /// All data.
  #[derivative(Debug = "ignore")]
  data: &'context mut AllData<'data>,
}

impl<'context, 'data> Context<'context, 'data> {
  pub fn new(data: &'context mut AllData<'data>) -> Self {
    Context { data }
  }
}

impl<'context, 'data> CommandContextTrait<'context> for Context<'context, 'data> {
  type Data = AllData<'data>;
  fn get_data(&self) -> &Self::Data {
    self.data
  }

  fn get_data_mut(&mut self) -> &mut Self::Data {
    self.data
  }
}
