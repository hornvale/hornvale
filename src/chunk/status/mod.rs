/// The `ChunkStatus` enum.
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Status {
  /// This chunk is blank and has no data.
  #[default]
  Empty,
  /// This chunk has been mapped, i.e. it has rooms.
  Mapped,
}
