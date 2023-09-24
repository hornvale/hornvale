use crate::event::ActionEvent;
use crate::event::OutputEvent;
use crate::system_data::AllData;
use crate::system_data::WriteEventTrait;

/// The `CommandContext` struct, which represents the data in the context of a
/// command execution.
pub trait CommandContextData: WriteEventTrait<ActionEvent> + WriteEventTrait<OutputEvent> {}

impl<'data> CommandContextData for AllData<'data> {}
