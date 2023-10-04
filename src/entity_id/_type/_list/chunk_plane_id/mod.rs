use crate::entity_id::BaseId;
use crate::entity_id::IntoBaseIdTrait;
use crate::entity_id::IntoChunkPlaneIdTrait;

/// The `ChunkPlaneId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct ChunkPlaneId(BaseId);

impl ChunkPlaneId {
  /// Create a new `ChunkPlaneId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl IntoBaseIdTrait for ChunkPlaneId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl From<ChunkPlaneId> for BaseId {
  fn from(id: ChunkPlaneId) -> Self {
    id.0
  }
}

impl From<BaseId> for ChunkPlaneId {
  fn from(id: BaseId) -> Self {
    Self(id)
  }
}

impl<T> From<T> for ChunkPlaneId
where
  T: IntoChunkPlaneIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_chunk_plane_id_from_base_id() {
    let base_id = BaseId::default();
    let chunk_plane_id = ChunkPlaneId::from(base_id.clone());
    assert_eq!(base_id, chunk_plane_id.into_base_id());
  }
}
