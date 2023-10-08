use std::collections::HashMap;

use crate::chunk_seed::ChunkSeed;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::RoomId;
use crate::room::Room;

pub mod builder;
pub use builder::Builder as ChunkBuilder;
pub mod map_builder;
pub use map_builder::MapBuilder as ChunkMapBuilder;
pub use map_builder::*;
pub mod file_manager;
pub use file_manager::FileManager as ChunkFileManager;
pub mod status;
pub use status::Status as ChunkStatus;
pub mod r#type;
pub use r#type::Type as ChunkType;

/// The `Chunk` struct.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Chunk {
  /// The `Chunk`'s `ChunkPlane`'s ID.
  pub chunk_plane_id: ChunkPlaneId,
  /// The `Chunk`'s `ChunkSeed`.
  pub chunk_seed: ChunkSeed,
  /// The `Chunk`'s ID.
  pub id: ChunkId,
  /// The `Chunk`'s type.
  pub r#type: ChunkType,
  /// The `Chunk`'s status.
  pub status: ChunkStatus,
  /// The `Chunk`'s name.
  pub name: String,
  /// The `Chunk`'s description.
  pub description: String,
  /// The `Chunk`'s difficulty level.
  pub difficulty: u8,
  /// The `Chunk`'s starting room.
  pub starting_room_id: Option<RoomId>,
  /// The `Chunk`'s rooms.
  pub rooms: HashMap<RoomId, Room>,
  /// Whether this is a viable start location.
  pub is_startable: bool,
}

impl Default for Chunk {
  fn default() -> Self {
    Self {
      id: ChunkId::default(),
      chunk_plane_id: ChunkPlaneId::default(),
      chunk_seed: ChunkSeed::default(),
      r#type: ChunkType::default(),
      status: ChunkStatus::default(),
      name: "Default Chunk".to_string(),
      description: "This is the default chunk.".to_string(),
      difficulty: 0,
      starting_room_id: None,
      rooms: HashMap::new(),
      is_startable: false,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_chunk_default() {
    init();
    let chunk = Chunk::default();
    assert_ne!(chunk.id, ChunkId::default());
    assert_ne!(chunk.chunk_plane_id, ChunkPlaneId::default());
    assert_eq!(chunk.r#type, ChunkType::default());
    assert_eq!(chunk.name, "Default Chunk".to_string());
    assert_eq!(chunk.description, "This is the default chunk.".to_string());
    assert_eq!(chunk.rooms, HashMap::new());
  }

  #[test]
  fn test_chunk_new() {
    init();
    let chunk_id = ChunkId::default();
    let chunk_plane_id = ChunkPlaneId::default();
    let chunk_seed = ChunkSeed::default();
    let chunk = Chunk {
      id: chunk_id.clone(),
      chunk_plane_id: chunk_plane_id.clone(),
      chunk_seed: chunk_seed.clone(),
      r#type: ChunkType::default(),
      name: "Test Chunk".to_string(),
      description: "This is a test chunk.".to_string(),
      ..Default::default()
    };
    assert_eq!(chunk.id, chunk_id);
    assert_eq!(chunk.chunk_plane_id, chunk_plane_id);
    assert_eq!(chunk.chunk_seed, chunk_seed);
    assert_eq!(chunk.r#type, ChunkType::default());
    assert_eq!(chunk.name, "Test Chunk".to_string());
    assert_eq!(chunk.description, "This is a test chunk.".to_string());
    assert_eq!(chunk.rooms, HashMap::new());
  }
}
