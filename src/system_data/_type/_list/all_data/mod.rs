use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::event::*;
use crate::resource::*;
use crate::system_data::*;

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
  pub input_event_channel: Write<'data, EventChannel<InputEvent>>,
  #[derivative(Debug = "ignore")]
  pub command_event_channel: Write<'data, EventChannel<CommandEvent>>,
  #[derivative(Debug = "ignore")]
  pub output_event_channel: Write<'data, EventChannel<OutputEvent>>,
  // Resources
  #[derivative(Debug = "ignore")]
  pub quit_flag_resource: Write<'data, QuitFlagResource>,
}

impl GetEventChannelTrait<ActionEvent> for AllData<'_> {
  fn get_event_channel(&self) -> &EventChannel<ActionEvent> {
    &self.action_event_channel
  }
}

impl GetMutEventChannelTrait<ActionEvent> for AllData<'_> {
  fn get_mut_event_channel(&mut self) -> &mut EventChannel<ActionEvent> {
    &mut self.action_event_channel
  }
}

impl GetEventChannelTrait<CommandEvent> for AllData<'_> {
  fn get_event_channel(&self) -> &EventChannel<CommandEvent> {
    &self.command_event_channel
  }
}

impl GetMutEventChannelTrait<CommandEvent> for AllData<'_> {
  fn get_mut_event_channel(&mut self) -> &mut EventChannel<CommandEvent> {
    &mut self.command_event_channel
  }
}

impl GetEventChannelTrait<EffectEvent> for AllData<'_> {
  fn get_event_channel(&self) -> &EventChannel<EffectEvent> {
    &self.effect_event_channel
  }
}

impl GetMutEventChannelTrait<EffectEvent> for AllData<'_> {
  fn get_mut_event_channel(&mut self) -> &mut EventChannel<EffectEvent> {
    &mut self.effect_event_channel
  }
}

impl GetEventChannelTrait<InputEvent> for AllData<'_> {
  fn get_event_channel(&self) -> &EventChannel<InputEvent> {
    &self.input_event_channel
  }
}

impl GetMutEventChannelTrait<InputEvent> for AllData<'_> {
  fn get_mut_event_channel(&mut self) -> &mut EventChannel<InputEvent> {
    &mut self.input_event_channel
  }
}

impl GetEventChannelTrait<OutputEvent> for AllData<'_> {
  fn get_event_channel(&self) -> &EventChannel<OutputEvent> {
    &self.output_event_channel
  }
}

impl GetMutEventChannelTrait<OutputEvent> for AllData<'_> {
  fn get_mut_event_channel(&mut self) -> &mut EventChannel<OutputEvent> {
    &mut self.output_event_channel
  }
}
