use derive_more::Display;

use crate::entity_uuid::BaseUuid;
use crate::entity_uuid::IntoActionUuidTrait;
use crate::entity_uuid::IntoBaseUuidTrait;

/// The `ActionUuid` type.
///
/// We do this so that we can perform some compile-time type-checking with UUIDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct ActionUuid(pub BaseUuid);

impl_base_uuid_wrapper!(ActionUuid);

impl IntoBaseUuidTrait for ActionUuid {
  fn into_base_uuid(self) -> BaseUuid {
    self.0
  }
}

impl<T> From<T> for ActionUuid
where
  T: IntoActionUuidTrait,
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
  fn test_action_uuid_from_base_uuid() {
    let base_uuid = BaseUuid::default();
    let action_uuid = ActionUuid::new(base_uuid.to_string());
    assert_eq!(base_uuid, action_uuid.into_base_uuid());
  }
}
