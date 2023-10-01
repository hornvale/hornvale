/// The `PassageType` enum.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub enum Type {
  /// None -- there is no passage in this direction.
  #[default]
  None,
  /// Path -- there is a path in this direction.
  Path,
  /// Stub -- this is a placeholder for a passage that may or may not exist.
  Stub,
  /// Placeholder -- this is a placeholder for a passage to an unloaded room.
  Placeholder,
}
