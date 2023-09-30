use std::fs::File;
use std::io::Write;

use crate::entity_id::RoomId;

pub mod builder;
pub use builder::Builder as RoomBuilder;
pub mod r#type;
pub use r#type::Type as RoomType;

/// The `Room` type.
#[derive(Clone, Debug, Deserialize, Hash, PartialEq, Serialize)]
pub struct Room {
  /// The `Room`'s ID.
  pub id: RoomId,
  /// The `Room`'s type.
  pub r#type: RoomType,
  /// The `Room`'s name.
  pub name: String,
  /// The `Room`'s description.
  pub description: String,
}

impl Room {
  /// Creates a new `Room`.
  pub fn new(id: RoomId, r#type: RoomType, name: String, description: String) -> Self {
    Self {
      id,
      r#type,
      name,
      description,
    }
  }

  /// Saves the `Room` in a serialized form.
  pub fn save_room_to_yaml(&self, file_path: &str) -> Result<(), serde_yaml::Error> {
    let yaml_string = serde_yaml::to_string(self)?;
    let mut file = File::create(file_path).expect("Unable to create file");
    file.write_all(yaml_string.as_bytes()).expect("Unable to write data");
    Ok(())
  }

  /// Loads the `Room` from a serialized form.
  pub fn load_room_from_yaml(file_path: &str) -> Result<Room, serde_yaml::Error> {
    let file = File::open(file_path).expect("Unable to open file");
    let room: Room = serde_yaml::from_reader(file)?;
    Ok(room)
  }
}

impl Default for Room {
  fn default() -> Self {
    Self {
      id: RoomId::default(),
      r#type: RoomType::default(),
      name: "Default Room".to_string(),
      description: "This is the default room.".to_string(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_room_default() {
    init();
    let room = Room::default();
    assert_eq!(room.name, "Default Room");
    assert_eq!(room.description, "This is the default room.");
  }

  #[test]
  fn test_room_new() {
    init();
    let room_id = RoomId::default();
    let room_type = RoomType::default();
    let room = Room::new(
      room_id,
      room_type,
      "Test Room".to_string(),
      "This is a test room.".to_string(),
    );
    assert_eq!(room.name, "Test Room");
    assert_eq!(room.description, "This is a test room.");
  }

  #[test]
  fn test_room_save_and_load() {
    init();
    let room_id = RoomId::default();
    let room_type = RoomType::default();
    let room = Room::new(
      room_id,
      room_type,
      "Test Room".to_string(),
      "This is a test room.".to_string(),
    );
    let file_path = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_room.yaml");
    room.save_room_to_yaml(&file_path).unwrap();
    let room2 = Room::load_room_from_yaml(&file_path).unwrap();
    assert_eq!(room, room2);
  }
}
