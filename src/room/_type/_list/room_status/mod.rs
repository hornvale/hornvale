/// The `RoomStatus` enum.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub enum RoomStatus {
  /// This room's status is unknown.
  #[default]
  Unknown,
}
