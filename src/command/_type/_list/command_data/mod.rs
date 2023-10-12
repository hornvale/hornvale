use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::event::CommandEvent;
use crate::resource::InputReadyFlagResource;
use crate::resource::QuitFlagResource;

#[derive(SystemData)]
pub struct CommandData<'data> {
  pub entities: Entities<'data>,
  pub command_event_channel: Read<'data, EventChannel<CommandEvent>>,
  pub input_ready_flag_resource: Write<'data, InputReadyFlagResource>,
  pub quit_flag_resource: Write<'data, QuitFlagResource>,
}
