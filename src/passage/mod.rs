use std::fs::File;
use std::io::Write;

use crate::entity_id::PassageId;
use crate::entity_id::RoomId;

pub mod r#type;
pub use r#type::Type as PassageType;

/// The `Passage` struct.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Passage {
  /// The `Passage`'s ID.
  pub id: PassageId,
  /// The `Passage`'s type.
  pub r#type: PassageType,
  /// The `Passage`'s name.
  pub name: String,
  /// The `Passage`'s description.
  pub description: String,
  /// The `Passage`'s origin.
  pub origin: RoomId,
  /// The `Passage`'s destination.
  pub destination: RoomId,
}

impl Passage {
  /// Creates a new `Passage`.
  pub fn new(
    id: PassageId,
    r#type: PassageType,
    name: String,
    description: String,
    origin: RoomId,
    destination: RoomId,
  ) -> Self {
    Self {
      id,
      r#type,
      name,
      description,
      origin,
      destination,
    }
  }

  /// Saves the `Passage` in a serialized form.
  pub fn write_to_file(&self, file_path: &str) -> Result<(), serde_yaml::Error> {
    let yaml_string = serde_yaml::to_string(self)?;
    let mut file = File::create(file_path).expect("Unable to create file");
    file.write_all(yaml_string.as_bytes()).expect("Unable to write data");
    Ok(())
  }

  /// Loads the `Passage` from a serialized form.
  pub fn load_from_file(file_path: &str) -> Result<Passage, serde_yaml::Error> {
    let file = File::open(file_path).expect("Unable to open file");
    let passage: Passage = serde_yaml::from_reader(file)?;
    Ok(passage)
  }
}

impl Default for Passage {
  fn default() -> Self {
    Self {
      id: PassageId::default(),
      r#type: PassageType::default(),
      name: "Default Passage".to_string(),
      description: "This is the default passage.".to_string(),
      origin: RoomId::default(),
      destination: RoomId::default(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;
  use crate::test::TEMPORARY_TEST_DATA_DIRECTORY;

  #[test]
  fn test_passage_default() {
    init();
    let passage = Passage::default();
    assert_ne!(passage.id, PassageId::default());
    assert_eq!(passage.r#type, PassageType::default());
    assert_eq!(passage.name, "Default Passage");
    assert_eq!(passage.description, "This is the default passage.");
    assert_ne!(passage.origin, RoomId::default());
    assert_ne!(passage.destination, RoomId::default());
  }

  #[test]
  fn test_passage_write_to_file() {
    init();
    let passage = Passage::default();
    let file_path = format!("{}/{}", TEMPORARY_TEST_DATA_DIRECTORY, "test_passage.yaml");
    passage.write_to_file(&file_path).unwrap();
    let passage_loaded = Passage::load_from_file(&file_path).unwrap();
    assert_eq!(passage, passage_loaded);
  }
}
