use specs::prelude::*;

use crate::actor::Actor;
use crate::component::*;
use crate::resource::PlayerResource;

/// The `InsertPlayer` system.
///
/// This system inserts the player into the starting room.
#[derive(Debug, Default)]
pub struct InsertPlayer {}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub player_resource: Write<'data, PlayerResource>,
  pub is_a_room: ReadStorage<'data, IsARoomComponent>,
  pub is_in_room: WriteStorage<'data, IsInRoomComponent>,
  pub is_an_actor: WriteStorage<'data, IsAnActorComponent>,
}

impl<'data> System<'data> for InsertPlayer {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running InsertPlayer system.");
    if data.player_resource.entity.is_none() {
      let entity = data.entities.create();
      let room_entity = (&data.entities, &data.is_a_room)
        .join()
        .find(|(_, is_a_room)| is_a_room.0.is_startable)
        .map(|(entity, _)| entity)
        .unwrap();
      data
        .is_an_actor
        .insert(entity, IsAnActorComponent(Actor::default()))
        .unwrap();
      data.is_in_room.insert(entity, IsInRoomComponent(room_entity)).unwrap();
      data.player_resource.entity = Some(entity);
    }
  }
}
