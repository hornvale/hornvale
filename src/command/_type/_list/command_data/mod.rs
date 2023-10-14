use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::event::ActionEvent;
use crate::event::CommandEvent;
use crate::resource::AdvanceFlagResource;

#[derive(SystemData)]
pub struct CommandData<'data> {
  pub entities: Entities<'data>,
  pub command_event_channel: Read<'data, EventChannel<CommandEvent>>,
  pub action_event_channel: Write<'data, EventChannel<ActionEvent>>,
  pub advance_flag_resource: Write<'data, AdvanceFlagResource>,
}
