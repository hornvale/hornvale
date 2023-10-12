use derive_more::Display;

use crate::entity_uuid::BaseUuid;
use crate::entity_uuid::IntoBaseUuidTrait;
use crate::entity_uuid::IntoChunkPlaneUuidTrait;

/// The `ChunkPlaneUuid` type.
///
/// We do this so that we can perform some compile-time type-checking with UUIDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct ChunkPlaneUuid(BaseUuid);

impl_base_uuid_wrapper!(ChunkPlaneUuid);

impl IntoBaseUuidTrait for ChunkPlaneUuid {
  fn into_base_uuid(self) -> BaseUuid {
    self.0
  }
}

impl<T> From<T> for ChunkPlaneUuid
where
  T: IntoChunkPlaneUuidTrait,
{
  fn from(uuid: T) -> Self {
    Self(uuid.into_base_uuid())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::entity_uuid::BaseUuidWrapperTrait;

  #[test]
  fn test_chunk_plane_id_from_base_id() {
    let base_uuid = BaseUuid::default();
    let chunk_plane_uuid = ChunkPlaneUuid::new(base_uuid.to_string());
    assert_eq!(base_uuid, chunk_plane_uuid.into_base_uuid());
  }
}
