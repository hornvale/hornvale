use crate::system_data::AllData;

/// The `CommandContext` struct, which represents the data in the context of a
/// command execution.
pub trait CommandContextData {}

impl<'data> CommandContextData for AllData<'data> {}
