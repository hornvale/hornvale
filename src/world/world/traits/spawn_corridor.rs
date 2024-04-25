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
