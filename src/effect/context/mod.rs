use crate::system_data::AllData;

/// The `Context` struct, which represents the context of a effect application.
#[derive(Debug)]
pub struct Context<'context, 'data> {
  /// All data.
  pub data: &'context mut AllData<'data>,
}

impl<'context, 'data> Context<'context, 'data> {
  pub fn new(data: &'context mut AllData<'data>) -> Self {
    Context { data }
  }
}
