use crate::event::ActionEvent;
use crate::event::EffectEvent;
use crate::system_data::AllData;
use crate::system_data::WriteEventTrait;

/// The `ActionContext` struct, which represents the data in the context of an
/// action execution.
pub trait ActionContextData: WriteEventTrait<ActionEvent> + WriteEventTrait<EffectEvent> {}

impl<'data> ActionContextData for AllData<'data> {}
