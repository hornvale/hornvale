use hecs::World;

/// A function to get the region and room containing an entity.
pub fn get_region_and_room_containing_entity(world: &World, actor: hecs::Entity) -> (hecs::Entity, hecs::Entity) {
  let query_result = world.query_one::<(&Region, &Room)>(actor);
  let mut query = query_result.unwrap();
  let (region, room) = query.get().unwrap();
  (*region, *room)
}

/// A function to get the room entity at

/// A function to get the room entity from the actor's region and room.
pub fn get_room_entity_containing_entity(world: &World, actor: hecs::Entity) -> hecs::Entity {
  let actor_info = {
    let query_result = world.query_one::<(&Region, &Room)>(actor);
    let mut query = query_result.unwrap();
    let (region, room) = query.get().unwrap();
    (*region, *room)
  };
  let room_entity = {
    world
      .query::<With<(&Region, &Room), &IsARoom>>()
      .into_iter()
      .find(|(_, (&reg, &rm))| {
        let (region, room) = actor_info;
        region == reg && room == rm
      })
      .unwrap()
      .0
  };

  room_entity
}
