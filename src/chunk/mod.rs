use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use crate::entity_id::ChunkId;
use crate::entity_id::RoomId;
use crate::room::Room;

pub mod builder;
pub use builder::Builder as ChunkBuilder;
pub mod r#type;
pub use r#type::Type as ChunkType;

/// The `Chunk` struct.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Chunk {
  /// The `Chunk`'s ID.
  pub id: ChunkId,
  /// The `Chunk`'s type.
  pub r#type: ChunkType,
  /// The `Chunk`'s name.
  pub name: String,
  /// The `Chunk`'s description.
  pub description: String,
  /// The `Chunk`'s rooms.
  pub rooms: HashMap<RoomId, Room>,
}

impl Chunk {
  /// Creates a new `Chunk`.
  pub fn new(id: ChunkId, r#type: ChunkType, name: String, description: String) -> Self {
    Self {
      id,
      r#type,
      name,
      description,
      rooms: HashMap::new(),
    }
  }

  /// Saves the `Chunk` in a serialized form.
  pub fn save_chunk_to_yaml(&self, file_path: &str) -> Result<(), serde_yaml::Error> {
    let yaml_string = serde_yaml::to_string(self)?;
    let mut file = File::create(file_path).expect("Unable to create file");
    file.write_all(yaml_string.as_bytes()).expect("Unable to write data");
    Ok(())
  }

  /// Loads the `Chunk` from a serialized form.
  pub fn load_chunk_from_yaml(file_path: &str) -> Result<Chunk, serde_yaml::Error> {
    let file = File::open(file_path).expect("Unable to open file");
    let chunk: Chunk = serde_yaml::from_reader(file)?;
    Ok(chunk)
  }
}

impl Default for Chunk {
  fn default() -> Self {
    Self {
      id: ChunkId::default(),
      r#type: ChunkType::default(),
      name: "Default Chunk".to_string(),
      description: "This is the default chunk.".to_string(),
      rooms: HashMap::new(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::room::Room;
  use crate::room::RoomType;
  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_chunk_default() {
    init();
    let chunk = Chunk::default();
    assert_ne!(chunk.id, ChunkId::default());
    assert_eq!(chunk.r#type, ChunkType::default());
    assert_eq!(chunk.name, "Default Chunk".to_string());
    assert_eq!(chunk.description, "This is the default chunk.".to_string());
    assert_eq!(chunk.rooms, HashMap::new());
  }

  #[test]
  fn test_chunk_new() {
    init();
    let chunk_id = ChunkId::default();
    let chunk = Chunk::new(
      chunk_id.clone(),
      ChunkType::default(),
      "Test Chunk".to_string(),
      "This is a test chunk.".to_string(),
    );
    assert_eq!(chunk.id, chunk_id);
    assert_eq!(chunk.r#type, ChunkType::default());
    assert_eq!(chunk.name, "Test Chunk".to_string());
    assert_eq!(chunk.description, "This is a test chunk.".to_string());
    assert_eq!(chunk.rooms, HashMap::new());
  }

  #[test]
  fn test_chunk_save_and_load() {
    init();
    let chunk_id = ChunkId::default();
    let chunk = Chunk::new(
      chunk_id.clone(),
      ChunkType::default(),
      "Test Chunk".to_string(),
      "This is a test chunk.".to_string(),
    );
    let file_path = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk.yaml");
    chunk.save_chunk_to_yaml(&file_path).unwrap();
    let loaded_chunk = Chunk::load_chunk_from_yaml(&file_path).unwrap();
    assert_eq!(chunk, loaded_chunk);
  }

  #[test]
  fn test_chunk_with_rooms_save_and_load() {
    init();
    let chunk_id = ChunkId::default();
    let mut chunk = Chunk::new(
      chunk_id.clone(),
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
    );
    chunk.rooms.insert(room_id, room);
    let file_path = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_chunk2.yaml");
    chunk.save_chunk_to_yaml(&file_path).unwrap();
    let loaded_chunk = Chunk::load_chunk_from_yaml(&file_path).unwrap();
    assert_eq!(chunk, loaded_chunk);
  }
}
