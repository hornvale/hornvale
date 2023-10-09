/// The `ChunkSeedType` enum.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub enum ChunkSeedType {
  /// This chunk's status is unknown.
  #[default]
  Unknown,
  /// This chunk does not have a fully-defined area.
  Open,
  /// This chunk has a defined area, but it might be modified by further seeds.
  HalfOpen,
  /// This chunk's area is no longer vulnerable to modification.
  Closed,
}
