use anyhow::Context;
use anyhow::Error as AnyError;
use serde_yaml::from_reader as serde_yaml_from_reader;
use serde_yaml::to_string as serde_yaml_to_string;
use std::collections::HashMap;
use std::fs::create_dir_all;
use std::fs::File;
use std::io::Write;

use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::ChunkSeedId;
use crate::entity_id::RoomId;
use crate::room::Room;

pub mod builder;
pub use builder::Builder as ChunkBuilder;
pub mod status;
pub use status::Status as ChunkStatus;
pub mod r#type;
pub use r#type::Type as ChunkType;

/// The `Chunk` struct.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Chunk {
  /// The `Chunk`'s `ChunkPlane`'s ID.
  pub chunk_plane_id: ChunkPlaneId,
  /// The `Chunk`'s `ChunkSeed`'s ID.
  pub chunk_seed_id: ChunkSeedId,
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
  /// The `Chunk`'s rooms.
  pub rooms: HashMap<RoomId, Room>,
}

impl Chunk {
  /// Creates a new `Chunk`.
  pub fn new(
    id: ChunkId,
    chunk_plane_id: &ChunkPlaneId,
    chunk_seed_id: &ChunkSeedId,
    r#type: ChunkType,
    name: String,
    description: String,
  ) -> Self {
    let chunk_plane_id = chunk_plane_id.clone();
    let chunk_seed_id = chunk_seed_id.clone();
    Self {
      id,
      chunk_plane_id,
      chunk_seed_id,
      r#type,
      status: ChunkStatus::default(),
      name,
      description,
      rooms: HashMap::new(),
    }
  }

  /// Saves the `Chunk` in a serialized form.
  pub fn store(&self, base_dir: &str) -> Result<(), AnyError> {
    let file_path = format!("{}/{}.yaml", base_dir, self.id);
    create_dir_all(base_dir).with_context(|| format!("Unable to create directory at {}", base_dir))?;
    let yaml_string = serde_yaml_to_string(self)?;
    let mut file = File::create(file_path).expect("Unable to create file");
    file
      .write_all(yaml_string.as_bytes())
      .expect("Unable to write chunk data");
    Ok(())
  }

  /// Loads the `Chunk` from a serialized form.
  pub fn load(base_dir: &str, chunk_id: &ChunkId) -> Result<Chunk, AnyError> {
    let file_path = format!("{}/{}.yaml", base_dir, chunk_id);
    let file = File::open(file_path.clone()).with_context(|| format!("Unable to create directory at {}", file_path))?;
    let chunk: Chunk = serde_yaml_from_reader(file)?;
    Ok(chunk)
  }
}

impl Default for Chunk {
  fn default() -> Self {
    Self {
      id: ChunkId::default(),
      chunk_plane_id: ChunkPlaneId::default(),
      chunk_seed_id: ChunkSeedId::default(),
      r#type: ChunkType::default(),
      status: ChunkStatus::default(),
      name: "Default Chunk".to_string(),
      description: "This is the default chunk.".to_string(),
      rooms: HashMap::new(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use std::fs::remove_dir_all;

  use crate::room::Room;
  use crate::room::RoomType;
  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

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
    let chunk_seed_id = ChunkSeedId::default();
    let chunk = Chunk::new(
      chunk_id.clone(),
      &chunk_plane_id,
      &chunk_seed_id,
      ChunkType::default(),
      "Test Chunk".to_string(),
      "This is a test chunk.".to_string(),
    );
    assert_eq!(chunk.id, chunk_id);
    assert_eq!(chunk.chunk_plane_id, chunk_plane_id);
    assert_eq!(chunk.chunk_seed_id, chunk_seed_id);
    assert_eq!(chunk.r#type, ChunkType::default());
    assert_eq!(chunk.name, "Test Chunk".to_string());
    assert_eq!(chunk.description, "This is a test chunk.".to_string());
    assert_eq!(chunk.rooms, HashMap::new());
  }

  #[test]
  fn test_chunk_save_and_load() {
    init();
    let chunk_id = ChunkId::default();
    let chunk_plane_id = ChunkPlaneId::default();
    let chunk_seed_id = ChunkSeedId::default();
    let chunk = Chunk::new(
      chunk_id.clone(),
      &chunk_plane_id,
      &chunk_seed_id,
      ChunkType::default(),
      "Test Chunk".to_string(),
      "This is a test chunk.".to_string(),
    );
    let base_dir = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_save_and_load");
    remove_dir_all(&base_dir).ok();
    chunk.store(&base_dir).unwrap();
    let loaded_chunk = Chunk::load(&base_dir, &chunk_id.clone()).unwrap();
    assert_eq!(chunk, loaded_chunk);
  }

  #[test]
  fn test_chunk_with_rooms_save_and_load() {
    init();
    let chunk_id = ChunkId::default();
    let chunk_plane_id = ChunkPlaneId::default();
    let chunk_seed_id = ChunkSeedId::default();
    let mut chunk = Chunk::new(
      chunk_id.clone(),
      &chunk_plane_id,
      &chunk_seed_id,
      ChunkType::default(),
      "Test Chunk".to_string(),
      "This is a test chunk.".to_string(),
    );
    let room_id = RoomId::default();
    let room = Room::new(
      room_id.clone(),
      RoomType::default(),
      "Test Room".to_string(),
      "This is a test room.".to_string(),
      HashMap::new(),
    );
    chunk.rooms.insert(room_id, room);
    let base_dir = format!(
      "{}/{}",
      TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk_with_rooms_save_and_load"
    );
    remove_dir_all(&base_dir).ok();
    chunk.store(&base_dir).unwrap();
    let loaded_chunk = Chunk::load(&base_dir, &chunk_id).unwrap();
    assert_eq!(chunk, loaded_chunk);
  }
}
