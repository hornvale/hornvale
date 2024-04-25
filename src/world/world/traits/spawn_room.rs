use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// Spawn a room.
pub trait SpawnRoom {
  /// Spawn a room.
  fn spawn_room(&mut self, region: Region, room: Room, name: &str, description: &str) -> Result<Entity, AnyError>;
}

impl SpawnRoom for World {
  fn spawn_room(&mut self, region: Region, room: Room, name: &str, description: &str) -> Result<Entity, AnyError> {
    Ok(self.spawn((
      region,
      room,
      Name(name.to_string()),
      Description(description.to_string()),
      IsARoom,
    )))
  }
}
