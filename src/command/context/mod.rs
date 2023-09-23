use crate::system::AllData;

/// The `Context` struct, which represents the context of a command execution.
#[derive(Debug)]
pub struct Context<'context, 'data> {
  /// The data.
  pub data: &'context mut AllData<'data>,
}

impl<'context, 'data> Context<'context, 'data> {
  pub fn new(data: &'context mut AllData<'data>) -> Self {
    Context { data }
  }
}
