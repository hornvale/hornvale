use crate::entity_id::BaseId;
use crate::entity_id::IntoBaseIdTrait;
use crate::entity_id::IntoChunkSeedIdTrait;

/// The `ChunkSeedId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct ChunkSeedId(BaseId);

impl ChunkSeedId {
  /// Create a new `ChunkId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl IntoBaseIdTrait for ChunkSeedId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl From<ChunkSeedId> for BaseId {
  fn from(id: ChunkSeedId) -> Self {
    id.0
  }
}

impl From<BaseId> for ChunkSeedId {
  fn from(id: BaseId) -> Self {
    Self(id)
  }
}

impl<T> From<T> for ChunkSeedId
where
  T: IntoChunkSeedIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_chunk_seed_id_from_base_id() {
    let base_id = BaseId::default();
    let chunk_seed_id = ChunkSeedId::from(base_id.clone());
    assert_eq!(base_id, chunk_seed_id.into_base_id());
  }
}
