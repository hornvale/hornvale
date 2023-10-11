/// The `ChunkStatus` enum.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub enum ChunkStatus {
  /// This chunk's status is unknown.
  #[default]
  Unknown,
}
