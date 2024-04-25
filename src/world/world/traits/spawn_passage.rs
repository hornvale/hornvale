use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// Spawn a passage.
pub trait SpawnPassage {
  /// Spawn a passage.
  fn spawn_passage(
    &mut self,
    region: Region,
    from_room: Room,
    to_room: Room,
    direction: PassageDirection,
  ) -> Result<(Entity, Entity), AnyError>;
}

impl SpawnPassage for World {
  fn spawn_passage(
    &mut self,
    region: Region,
    from_room: Room,
    to_room: Room,
    direction: PassageDirection,
  ) -> Result<(Entity, Entity), AnyError> {
    let result = (
      self.spawn((region, from_room, direction, PassageKind::from(to_room), IsAPassage)),
      self.spawn((region, to_room, -direction, PassageKind::from(from_room), IsAPassage)),
    );
    Ok(result)
  }
}
