use derive_more::Display;

use crate::entity_uuid::BaseUuid;
use crate::entity_uuid::IntoBaseUuidTrait;
use crate::entity_uuid::IntoRoomUuidTrait;

/// The `RoomUuid` type.
///
/// We do this so that we can perform some compile-time type-checking with UUIDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct RoomUuid(pub BaseUuid);

impl_base_uuid_wrapper!(RoomUuid);

impl IntoBaseUuidTrait for RoomUuid {
  fn into_base_uuid(self) -> BaseUuid {
    self.0
  }
}

impl<T> From<T> for RoomUuid
where
  T: IntoRoomUuidTrait,
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
  fn test_room_id_from_base_id() {
    let base_uuid = BaseUuid::default();
    let room_uuid = RoomUuid::new(base_uuid.to_string());
    assert_eq!(base_uuid, room_uuid.into_base_uuid());
  }
}
