use std::sync::Arc;

use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::ecs::event::*;
use crate::ecs::resource::*;
use crate::ecs::system::*;
use crate::effect::EffectTrait;

/// The `AllData` type.
///
/// This represents all data available in the ECS world at any time.
///
/// Obviously, this should be used as infrequently as possible, but it is
/// necessary in some scenarios, particularly processing actions and effects.
#[derive(Derivative, SystemData)]
#[derivative(Debug)]
pub struct AllData<'data> {
  // Entities
  #[derivative(Debug = "ignore")]
  pub entities: Entities<'data>,
  // Components
  // Event Channels
  #[derivative(Debug = "ignore")]
  pub action_event_channel: Write<'data, EventChannel<ActionEvent>>,
  #[derivative(Debug = "ignore")]
  pub effect_event_channel: Write<'data, EventChannel<EffectEvent>>,
  #[derivative(Debug = "ignore")]
  pub input_event_channel: Read<'data, EventChannel<InputEvent>>,
  #[derivative(Debug = "ignore")]
  pub command_event_channel: Read<'data, EventChannel<CommandEvent>>,
  #[derivative(Debug = "ignore")]
  pub output_event_channel: Write<'data, EventChannel<OutputEvent>>,
  // Resources
  #[derivative(Debug = "ignore")]
  pub quit_flag_resource: Write<'data, QuitFlagResource>,
}

impl<'data> WriteEffectEventTrait for AllData<'data> {
  fn write_effect_event(&mut self, effect: Arc<dyn EffectTrait>) {
    self.effect_event_channel.single_write(EffectEvent { effect });
  }
}
