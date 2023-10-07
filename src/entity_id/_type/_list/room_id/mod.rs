use crate::entity_id::BaseId;
use crate::entity_id::IntoBaseIdTrait;
use crate::entity_id::IntoEntityIdTrait;
use crate::entity_id::IntoRoomIdTrait;

/// The `RoomId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct RoomId(BaseId);

impl RoomId {
  /// Create a new `RoomId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl IntoBaseIdTrait for RoomId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl IntoEntityIdTrait for RoomId {}

impl From<RoomId> for BaseId {
  fn from(id: RoomId) -> Self {
    id.0
  }
}

impl<T> From<T> for RoomId
where
  T: IntoRoomIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_room_id_from_base_id() {
    let base_id = BaseId::default();
    let room_id = RoomId(base_id.clone());
    assert_eq!(base_id, room_id.into_base_id());
  }
}
