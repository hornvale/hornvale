use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::component::IsAnActorComponent;
use crate::component::IsInRoomComponent;
use crate::event::ActionEvent;
use crate::event::OutputEvent;
use crate::resource::AdvanceFlagResource;
use crate::resource::InputReadyFlagResource;
use crate::resource::QuitFlagResource;

#[derive(SystemData)]
pub struct ActionData<'data> {
  pub entities: Entities<'data>,
  pub action_event_channel: Read<'data, EventChannel<ActionEvent>>,
  pub output_event_channel: Write<'data, EventChannel<OutputEvent>>,
  pub is_an_actor_component: WriteStorage<'data, IsAnActorComponent>,
  pub is_in_room_component: WriteStorage<'data, IsInRoomComponent>,
  pub advance_flag_resource: Write<'data, AdvanceFlagResource>,
  pub input_ready_flag_resource: Write<'data, InputReadyFlagResource>,
  pub quit_flag_resource: Write<'data, QuitFlagResource>,
}
