use std::collections::HashMap;

use crate::chunk::Chunk;
use crate::chunk::ChunkStatus;
use crate::chunk::ChunkType;
use crate::chunk_seed::ChunkSeed;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::RoomId;
use crate::room::Room;

/// The `ChunkBuilder` type.
#[derive(Clone, Debug, Default)]
pub struct Builder {
  /// The `Chunk`'s ID.
  pub id: Option<ChunkId>,
  /// The `Chunk`'s `ChunkPlane`'s ID.
  pub chunk_plane_id: Option<ChunkPlaneId>,
  /// The `Chunk`'s `ChunkSeed`.
  pub chunk_seed: Option<ChunkSeed>,
  /// The `Chunk`'s type.
  pub r#type: Option<ChunkType>,
  /// The `Chunk`'s status.
  pub status: Option<ChunkStatus>,
  /// The `Chunk`'s name.
  pub name: Option<String>,
  /// The `Chunk`'s description.
  pub description: Option<String>,
  /// The `Chunk`'s difficulty.
  pub difficulty: Option<u8>,
  /// The `Chunk`'s starting room.
  pub starting_room_id: Option<RoomId>,
  /// The `Chunk`'s rooms.
  pub rooms: Option<HashMap<RoomId, Room>>,
  /// Whether this is a viable start location.
  pub is_startable: bool,
}

impl Builder {
  /// Creates a new `ChunkBuilder`.
  pub fn new() -> Self {
    Self::default()
  }

  /// Sets the `Chunk`'s ID.
  pub fn id(mut self, id: &ChunkId) -> Self {
    self.id = Some(id.clone());
    self
  }

  /// Sets the `Chunk`'s `ChunkPlane`'s ID.
  pub fn chunk_plane_id(mut self, chunk_plane_id: &ChunkPlaneId) -> Self {
    self.chunk_plane_id = Some(chunk_plane_id.clone());
    self
  }

  /// Sets the `Chunk`'s `ChunkSeed`.
  pub fn chunk_seed(mut self, chunk_seed: &ChunkSeed) -> Self {
    self.chunk_seed = Some(chunk_seed.clone());
    self
  }

  /// Sets the `Chunk`'s type.
  pub fn r#type(mut self, r#type: &ChunkType) -> Self {
    self.r#type = Some(r#type.clone());
    self
  }

  /// Sets the `Chunk`'s status.
  pub fn status(mut self, status: &ChunkStatus) -> Self {
    self.status = Some(status.clone());
    self
  }

  /// Sets the `Chunk`'s name.
  pub fn name(mut self, name: &str) -> Self {
    self.name = Some(name.to_string());
    self
  }

  /// Sets the `Chunk`'s description.
  pub fn description(mut self, description: &str) -> Self {
    self.description = Some(description.to_string());
    self
  }

  /// Sets the `Chunk`'s difficulty.
  pub fn difficulty(mut self, difficulty: u8) -> Self {
    self.difficulty = Some(difficulty);
    self
  }

  /// Sets the `Chunk`'s starting room.
  pub fn starting_room_id(mut self, starting_room_id: &RoomId) -> Self {
    self.starting_room_id = Some(starting_room_id.clone());
    self
  }

  /// Sets the `Chunk`'s rooms.
  pub fn rooms(mut self, rooms: &HashMap<RoomId, Room>) -> Self {
    self.rooms = Some(rooms.clone());
    self
  }

  /// Sets whether this is a viable start location.
  pub fn is_startable(mut self, is_startable: bool) -> Self {
    self.is_startable = is_startable;
    self
  }

  /// Builds the `Chunk`.
  pub fn build(self) -> Chunk {
    Chunk {
      id: self.id.unwrap_or_default(),
      chunk_plane_id: self.chunk_plane_id.unwrap_or_default(),
      chunk_seed: self.chunk_seed.unwrap_or_default(),
      r#type: self.r#type.unwrap_or_default(),
      status: self.status.unwrap_or_default(),
      name: self.name.unwrap_or_default(),
      description: self.description.unwrap_or_default(),
      difficulty: self.difficulty.unwrap_or_default(),
      starting_room_id: self.starting_room_id,
      rooms: self.rooms.unwrap_or_default(),
      is_startable: self.is_startable,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_chunk_builder() {
    let chunk_id = ChunkId::default();
    let chunk_plane_id = ChunkPlaneId::default();
    let chunk = Builder::new()
      .id(&chunk_id)
      .chunk_plane_id(&chunk_plane_id)
      .name("Test Chunk")
      .description("This is a test chunk.")
      .build();
    assert_eq!(chunk.id, chunk_id);
    assert_eq!(chunk.chunk_plane_id, chunk_plane_id);
    assert_eq!(chunk.name, "Test Chunk");
    assert_eq!(chunk.description, "This is a test chunk.");
  }
}
