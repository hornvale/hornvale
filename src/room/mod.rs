use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use crate::entity_id::RoomId;
use crate::passage::Passage;
use crate::passage::PassageDirection;

pub mod builder;
pub use builder::Builder as RoomBuilder;
pub mod r#type;
pub use r#type::Type as RoomType;

/// The `Room` type.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Room {
  /// The `Room`'s ID.
  pub id: RoomId,
  /// The `Room`'s type.
  pub r#type: RoomType,
  /// The `Room`'s name.
  pub name: String,
  /// The `Room`'s description.
  pub description: String,
  /// The `Room`'s passages.
  pub passages: HashMap<PassageDirection, Passage>,
}

impl Room {
  /// Creates a new `Room`.
  pub fn new(
    id: RoomId,
    r#type: RoomType,
    name: String,
    description: String,
    passages: HashMap<PassageDirection, Passage>,
  ) -> Self {
    Self {
      id,
      r#type,
      name,
      description,
      passages,
    }
  }

  /// Saves the `Room` in a serialized form.
  pub fn write_to_file(&self, file_path: &str) -> Result<(), serde_yaml::Error> {
    let yaml_string = serde_yaml::to_string(self)?;
    let mut file = File::create(file_path).expect("Unable to create file");
    file.write_all(yaml_string.as_bytes()).expect("Unable to write data");
    Ok(())
  }

  /// Loads the `Room` from a serialized form.
  pub fn load_from_file(file_path: &str) -> Result<Room, serde_yaml::Error> {
    let file = File::open(file_path).expect("Unable to open file");
    let room: Room = serde_yaml::from_reader(file)?;
    Ok(room)
  }

  /// Adds a passage to the `Room`.
  pub fn add_passage(&mut self, direction: &PassageDirection, passage: &Passage) {
    self.passages.insert(direction.clone(), passage.clone());
  }

  /// Describe the passages in the `Room`.
  pub fn describe_passages(&self) -> String {
    let description = match self.passages.len() {
      0 => "There are no obvious exits.".to_string(),
      1 => {
        let (direction, passage) = self.passages.iter().next().unwrap();
        format!(
          "There is a {} to the {}.",
          passage.name,
          direction.to_lower_case_string()
        )
      },
      2 => {
        let mut iter = self.passages.iter();
        let (direction1, passage1) = iter.next().unwrap();
        let (direction2, passage2) = iter.next().unwrap();
        if passage1.r#type == passage2.r#type {
          format!(
            "There are {} to the {} and {}.",
            passage1.r#type.to_plural_string(),
            direction1.to_lower_case_string(),
            direction2.to_lower_case_string()
          )
        } else {
          format!(
            "There is a {} to the {} and a {} to the {}.",
            passage1.r#type,
            direction1.to_lower_case_string(),
            passage2.r#type,
            direction2.to_lower_case_string()
          )
        }
      },
      _ => {
        let mut description = "There are ".to_string();
        let mut type_names = self
          .passages
          .values()
          .map(|passage| passage.r#type.to_string())
          .collect::<Vec<_>>();
        type_names.dedup();
        if type_names.len() == 1 {
          description.push_str(&format!(
            "{} ",
            self.passages.values().next().unwrap().r#type.to_plural_string()
          ));
          let mut iter = self.passages.iter();
          while let Some(passage) = iter.next() {
            if iter.len() == 0 {
              description.push_str(&format!("and {}", passage.0.to_lower_case_string()));
            } else {
              description.push_str(&format!("{}, ", passage.0.to_lower_case_string()));
            }
          }
        } else {
          let mut iter = self.passages.iter();
          while let Some(passage) = iter.next() {
            if iter.len() == 0 {
              description.push_str(", and ");
            }
            description.push_str(&format!(
              "a {} to the {}",
              passage.1.r#type,
              passage.0.to_lower_case_string()
            ));
            if iter.len() > 1 {
              description.push_str(", ");
            }
          }
        }
        description.push('.');
        description
      },
    };
    description
  }
}

impl Default for Room {
  fn default() -> Self {
    Self {
      id: RoomId::default(),
      r#type: RoomType::default(),
      name: "Default Room".to_string(),
      description: "This is the default room.".to_string(),
      passages: HashMap::new(),
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
      HashMap::new(),
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
      HashMap::new(),
    );
    let file_path = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_room.yaml");
    room.write_to_file(&file_path).unwrap();
    let room2 = Room::load_from_file(&file_path).unwrap();
    assert_eq!(room, room2);
  }
}
