use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// Spawn a corridor.
pub trait SpawnCorridor {
  /// Spawn a corridor.
  fn spawn_corridor(
    &mut self,
    from_region: Region,
    to_region: Region,
    from_room: Room,
    direction: PassageDirection,
  ) -> Result<(Entity, Entity), AnyError>;
}

impl SpawnCorridor for World {
  fn spawn_corridor(
    &mut self,
    from_region: Region,
    to_region: Region,
    from_room: Room,
    passage_direction: PassageDirection,
  ) -> Result<(Entity, Entity), AnyError> {
    let corridor_direction = CorridorDirection::from(passage_direction);
    let corridor_kind = CorridorKind::from(to_region);
    let passage_kind = PassageKind::Corridor(corridor_kind);
    let result = (
      self.spawn((
        from_region,
        from_room,
        passage_direction,
        passage_kind,
        IsAPassage,
        CorridorOrigin,
      )),
      self.spawn((
        from_region,
        from_room,
        -corridor_direction,
        corridor_kind,
        CorridorTerminus,
      )),
    );
    Ok(result)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_spawn_corridor() {
    init();
    let mut world = World::new();
    let from_region = Region::default();
    let to_region = from_region + CorridorDirection(Direction::North);
    let from_room = Room::default();
    let passage_direction = PassageDirection(Direction::North);
    let (passage, corridor) = world
      .spawn_corridor(from_region, to_region, from_room, passage_direction)
      .unwrap();
    assert_eq!(
      *world.get::<&Region>(passage).unwrap(),
      from_region,
      "The passage should be in the from region."
    );
    assert_eq!(
      *world.get::<&Room>(passage).unwrap(),
      from_room,
      "The passage should be in the from room."
    );
    assert_eq!(
      *world.get::<&PassageDirection>(passage).unwrap(),
      passage_direction,
      "The passage should be in the correct direction."
    );
    assert_eq!(
      *world.get::<&PassageKind>(passage).unwrap(),
      PassageKind::Corridor(CorridorKind::from(to_region)),
      "The passage should be a corridor."
    );
    assert!(
      world.get::<&IsAPassage>(passage).is_ok(),
      "The passage should be a passage."
    );
    assert_eq!(
      *world.get::<&CorridorOrigin>(passage).unwrap(),
      CorridorOrigin,
      "The passage should be the origin of the corridor."
    );
    assert_eq!(
      *world.get::<&Region>(corridor).unwrap(),
      from_region,
      "The corridor should be in the from region."
    );
    assert_eq!(
      *world.get::<&Room>(corridor).unwrap(),
      from_room,
      "The corridor should be in the from room."
    );
    assert_eq!(
      *world.get::<&CorridorDirection>(corridor).unwrap(),
      -CorridorDirection::from(passage_direction),
      "The corridor should be in the opposite direction."
    );
    assert_eq!(
      *world.get::<&CorridorKind>(corridor).unwrap(),
      CorridorKind::from(to_region),
      "The corridor should be the correct kind."
    );
    assert!(
      world.get::<&CorridorTerminus>(corridor).is_ok(),
      "The corridor should be the terminus of the corridor."
    );
  }
}
