/// The `ChunkType` enum.
#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Type {
  /// The default, empty `Chunk`.
  #[default]
  Default,
}
