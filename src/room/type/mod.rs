/// The `RoomType` enum.
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Type {
  /// The `RoomType` variant for a `Room` that is a `Room`.
  #[default]
  Room,
  /// The `RoomType` variant for a `Room` that is a `Hallway`.
  Hallway,
}
