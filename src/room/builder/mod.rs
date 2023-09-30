use std::collections::HashMap;

use crate::entity_id::RoomId;
use crate::passage::Passage;
use crate::passage::PassageDirection;
use crate::room::Room;
use crate::room::RoomType;

/// The `RoomBuilder` type.
#[derive(Clone, Debug, Default)]
pub struct Builder {
  /// The `Room`'s ID.
  pub id: Option<RoomId>,
  /// The `Room`'s type.
  pub r#type: Option<RoomType>,
  /// The `Room`'s name.
  pub name: Option<String>,
  /// The `Room`'s description.
  pub description: Option<String>,
  /// The `Room`'s passages.
  pub passages: Option<HashMap<PassageDirection, Passage>>,
}

impl Builder {
  /// Creates a new `RoomBuilder`.
  pub fn new() -> Self {
    Self::default()
  }

  /// Sets the `Room`'s ID.
  pub fn id(mut self, id: &RoomId) -> Self {
    self.id = Some(id.clone());
    self
  }

  /// Sets the `Room`'s type.
  pub fn r#type(mut self, r#type: &RoomType) -> Self {
    self.r#type = Some(r#type.clone());
    self
  }

  /// Sets the `Room`'s name.
  pub fn name(mut self, name: &str) -> Self {
    self.name = Some(name.to_string());
    self
  }

  /// Sets the `Room`'s description.
  pub fn description(mut self, description: &str) -> Self {
    self.description = Some(description.to_string());
    self
  }

  /// Sets the `Room`'s passages.
  pub fn passages(mut self, passages: &HashMap<PassageDirection, Passage>) -> Self {
    self.passages = Some(passages.clone());
    self
  }

  /// Adds a passage to the `Room`.
  pub fn add_passage(mut self, direction: &PassageDirection, passage: &Passage) -> Self {
    if self.passages.is_none() {
      self.passages = Some(HashMap::new());
    }
    let mut passage = passage.clone();
    passage.origin = self.id.clone().unwrap_or_default();
    self.passages.as_mut().unwrap().insert(direction.clone(), passage);
    self
  }

  /// Builds the `Room`.
  pub fn build(self) -> Room {
    Room::new(
      self.id.unwrap_or_default(),
      self.r#type.unwrap_or_default(),
      self.name.unwrap_or_default(),
      self.description.unwrap_or_default(),
      self.passages.unwrap_or_default(),
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_room_builder() {
    let room_id = RoomId::default();
    let room_type = RoomType::default();
    let room = Builder::new()
      .id(&room_id)
      .r#type(&room_type)
      .name("Room")
      .description("A room.")
      .build();
    assert_eq!(room.id, room_id);
    assert_eq!(room.r#type, room_type);
    assert_eq!(room.name, "Room");
    assert_eq!(room.description, "A room.");
  }
}
