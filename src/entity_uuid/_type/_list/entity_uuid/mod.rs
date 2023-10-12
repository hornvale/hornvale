use derive_more::Display;

use crate::entity_uuid::BaseUuid;
use crate::entity_uuid::IntoBaseUuidTrait;
use crate::entity_uuid::IntoEntityUuidTrait;

/// The `EntityUuid` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct EntityUuid(BaseUuid);

impl EntityUuid {
  /// Create a new `EntityUuid`.
  pub fn new(uuid: String) -> Self {
    Self(BaseUuid::new(uuid))
  }
}

impl IntoBaseUuidTrait for EntityUuid {
  fn into_base_uuid(self) -> BaseUuid {
    self.0
  }
}

impl From<EntityUuid> for BaseUuid {
  fn from(uuid: EntityUuid) -> Self {
    uuid.0
  }
}

impl<T> From<T> for EntityUuid
where
  T: IntoEntityUuidTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_uuid())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_entity_uuid_from_base_uuid() {
    let base_uuid = BaseUuid::default();
    let entity_uuid = EntityUuid(base_uuid.clone());
    assert_eq!(base_uuid, entity_uuid.into_base_uuid());
  }
}
