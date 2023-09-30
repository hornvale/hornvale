use crate::entity_id::RoomId;
use crate::room::Room;
use crate::room::RoomType;

/// The `RoomBuilder` type.
#[derive(Clone, Debug, Default)]
pub struct Builder {
  /// The `Room`'s ID.
  pub id: RoomId,
  /// The `Room`'s type.
  pub r#type: RoomType,
  /// The `Room`'s name.
  pub name: String,
  /// The `Room`'s description.
  pub description: String,
}

impl Builder {
  /// Creates a new `RoomBuilder`.
  pub fn new() -> Self {
    Self::default()
  }

  /// Sets the `Room`'s ID.
  pub fn id(mut self, id: RoomId) -> Self {
    self.id = id;
    self
  }

  /// Sets the `Room`'s type.
  pub fn r#type(mut self, r#type: RoomType) -> Self {
    self.r#type = r#type;
    self
  }

  /// Sets the `Room`'s name.
  pub fn name(mut self, name: String) -> Self {
    self.name = name;
    self
  }

  /// Sets the `Room`'s description.
  pub fn description(mut self, description: String) -> Self {
    self.description = description;
    self
  }

  /// Builds the `Room`.
  pub fn build(self) -> Room {
    Room::new(self.id, self.r#type, self.name, self.description)
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
      .id(room_id.clone())
      .r#type(room_type.clone())
      .name("Room".to_string())
      .description("A room.".to_string())
      .build();
    assert_eq!(room.id, room_id);
    assert_eq!(room.r#type, room_type);
    assert_eq!(room.name, "Room".to_string());
    assert_eq!(room.description, "A room.".to_string());
  }
}
