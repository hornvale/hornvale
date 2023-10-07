use crate::entity_id::BaseId;
use crate::entity_id::IntoBaseIdTrait;
use crate::entity_id::IntoChunkIdTrait;

/// The `ChunkId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct ChunkId(BaseId);

impl ChunkId {
  /// Create a new `ChunkId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl IntoBaseIdTrait for ChunkId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl From<ChunkId> for BaseId {
  fn from(id: ChunkId) -> Self {
    id.0
  }
}

impl From<BaseId> for ChunkId {
  fn from(id: BaseId) -> Self {
    Self(id)
  }
}

impl<T> From<T> for ChunkId
where
  T: IntoChunkIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_chunk_id_from_base_id() {
    let base_id = BaseId::default();
    let chunk_id = ChunkId::from(base_id.clone());
    assert_eq!(base_id, chunk_id.into_base_id());
  }
}
