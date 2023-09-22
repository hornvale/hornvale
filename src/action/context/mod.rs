use crate::ecs::AllData;

/// The `Context` struct, which represents the context of a command execution.
#[derive(Debug)]
pub struct Context<'context, 'data> {
  /// All data.
  pub all_data: &'context mut AllData<'data>,
}

impl<'context, 'data> Context<'context, 'data> {
  pub fn new(all_data: &'context mut AllData<'data>) -> Self {
    Context { all_data }
  }
}
