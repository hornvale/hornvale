use derive_more::Display;

use crate::entity_uuid::BaseUuid;
use crate::entity_uuid::IntoBaseUuidTrait;
use crate::entity_uuid::IntoEffectUuidTrait;

/// The `EffectUuid` type.
///
/// We do this so that we can perform some compile-time type-checking with UUIDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct EffectUuid(pub BaseUuid);

impl_base_uuid_wrapper!(EffectUuid);

impl IntoBaseUuidTrait for EffectUuid {
  fn into_base_uuid(self) -> BaseUuid {
    self.0
  }
}

impl<T> From<T> for EffectUuid
where
  T: IntoEffectUuidTrait,
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
  fn test_effect_uuid_from_base_uuid() {
    let base_uuid = BaseUuid::default();
    let effect_uuid = EffectUuid::new(base_uuid.to_string());
    assert_eq!(base_uuid, effect_uuid.into_base_uuid());
  }
}
