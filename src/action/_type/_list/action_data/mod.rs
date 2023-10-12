use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::event::ActionEvent;
use crate::resource::AdvanceFlagResource;
use crate::resource::InputReadyFlagResource;
use crate::resource::QuitFlagResource;

#[derive(SystemData)]
pub struct ActionData<'data> {
  pub entities: Entities<'data>,
  pub action_event_channel: Read<'data, EventChannel<ActionEvent>>,
  pub advance_flag_resource: Write<'data, AdvanceFlagResource>,
  pub input_ready_flag_resource: Write<'data, InputReadyFlagResource>,
  pub quit_flag_resource: Write<'data, QuitFlagResource>,
}
