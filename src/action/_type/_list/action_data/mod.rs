use specs::prelude::*;
use specs::saveload::MarkerAllocator;
use specs::saveload::SimpleMarker;
use specs::saveload::SimpleMarkerAllocator;
use specs::shrev::EventChannel;

use crate::component::IsARoomComponent;
use crate::component::IsAnActorComponent;
use crate::component::IsInRoomComponent;
use crate::event::ActionEvent;
use crate::event::CommandEvent;
use crate::event::OutputEvent;
use crate::marker::PersistedEntity;
use crate::resource::AdvanceFlagResource;
use crate::resource::InputReadyFlagResource;
use crate::resource::PlayerResource;
use crate::resource::QuitFlagResource;

#[derive(SystemData)]
pub struct ActionData<'data> {
  pub entities: Entities<'data>,
  pub persisted_entity_allocator: Write<'data, SimpleMarkerAllocator<PersistedEntity>>,
  pub action_event_channel: Write<'data, EventChannel<ActionEvent>>,
  pub command_event_channel: Write<'data, EventChannel<CommandEvent>>,
  pub output_event_channel: Write<'data, EventChannel<OutputEvent>>,
  pub is_an_actor_component: WriteStorage<'data, IsAnActorComponent>,
  pub is_a_room_component: WriteStorage<'data, IsARoomComponent>,
  pub is_in_room_component: WriteStorage<'data, IsInRoomComponent>,
  pub advance_flag_resource: Write<'data, AdvanceFlagResource>,
  pub input_ready_flag_resource: Write<'data, InputReadyFlagResource>,
  pub player_resource: Write<'data, PlayerResource>,
  pub quit_flag_resource: Write<'data, QuitFlagResource>,
  pub persisted_entity_markers: WriteStorage<'data, SimpleMarker<PersistedEntity>>,
}

impl<'data> ActionData<'data> {
  /// Retrieve an entity by its marker.
  pub fn get_persisted_entity_by_marker(&mut self, marker: SimpleMarker<PersistedEntity>) -> Entity {
    self
      .persisted_entity_allocator
      .retrieve_entity(marker, &mut self.persisted_entity_markers, &self.entities)
  }

  /// Retrieve a marker for an entity.
  pub fn get_marker_for_persisted_entity(&self, entity: Entity) -> Option<&SimpleMarker<PersistedEntity>> {
    self.persisted_entity_markers.get(entity)
  }
}
