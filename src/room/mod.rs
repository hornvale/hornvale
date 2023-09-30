use crate::entity_id::RoomId;

pub mod builder;
pub use builder::Builder as RoomBuilder;
pub mod r#type;
pub use r#type::Type as RoomType;

/// The `Room` type.
#[derive(Clone, Debug, Deserialize, Serialize)]
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
