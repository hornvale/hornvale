use std::collections::HashMap;

use crate::chunk::Chunk;
use crate::chunk::ChunkType;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::ChunkSeedId;
use crate::entity_id::RoomId;
use crate::room::Room;

/// The `ChunkBuilder` type.
#[derive(Clone, Debug, Default)]
pub struct Builder {
  /// The `Chunk`'s ID.
  pub id: Option<ChunkId>,
  /// The `Chunk`'s `ChunkPlane`'s ID.
  pub chunk_plane_id: Option<ChunkPlaneId>,
  /// The `Chunk`'s `ChunkSeed`'s ID.
  pub chunk_seed_id: Option<ChunkSeedId>,
  /// The `Chunk`'s type.
  pub r#type: Option<ChunkType>,
  /// The `Chunk`'s name.
  pub name: Option<String>,
  /// The `Chunk`'s description.
  pub description: Option<String>,
  /// The `Chunk`'s rooms.
  pub rooms: Option<HashMap<RoomId, Room>>,
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

  /// Sets the `Chunk`'s `ChunkSeed`'s ID.
  pub fn chunk_seed_id(mut self, chunk_seed_id: &ChunkSeedId) -> Self {
    self.chunk_seed_id = Some(chunk_seed_id.clone());
    self
  }

  /// Sets the `Chunk`'s type.
  pub fn r#type(mut self, r#type: &ChunkType) -> Self {
    self.r#type = Some(r#type.clone());
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

  /// Sets the `Chunk`'s rooms.
  pub fn rooms(mut self, rooms: &HashMap<RoomId, Room>) -> Self {
    self.rooms = Some(rooms.clone());
    self
  }

  /// Builds the `Chunk`.
  pub fn build(self) -> Chunk {
    let mut result = Chunk::new(
      self.id.unwrap_or_default(),
      &self.chunk_plane_id.unwrap_or_default(),
      &self.chunk_seed_id.unwrap_or_default(),
      self.r#type.unwrap_or_default(),
      self.name.unwrap_or_default(),
      self.description.unwrap_or_default(),
    );
    if let Some(rooms) = self.rooms {
      result.rooms = rooms;
    }
    result
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
