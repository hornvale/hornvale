use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::ecs::event::*;

/// The `AllData` type.
///
/// This represents all data available in the ECS world at any time.
///
/// Obviously, this should be used as infrequently as possible, but it is
/// necessary in some scenarios, particularly processing actions and effects.
#[derive(Derivative, SystemData)]
#[derivative(Debug)]
pub struct AllData<'data> {
  #[derivative(Debug = "ignore")]
  pub entities: Entities<'data>,
  #[derivative(Debug = "ignore")]
  pub action_event_channel: Write<'data, EventChannel<ActionEvent>>,
  #[derivative(Debug = "ignore")]
  pub input_event_channel: Read<'data, EventChannel<InputEvent>>,
  #[derivative(Debug = "ignore")]
  pub command_event_channel: Read<'data, EventChannel<CommandEvent>>,
  #[derivative(Debug = "ignore")]
  pub output_event_channel: Write<'data, EventChannel<OutputEvent>>,
}
