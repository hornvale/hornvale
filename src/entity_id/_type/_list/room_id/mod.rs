use crate::entity_id::BaseId;
use crate::entity_id::IntoRoomIdTrait;

/// The `RoomId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct RoomId(BaseId);

impl From<BaseId> for RoomId {
  fn from(id: BaseId) -> Self {
    Self(id)
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
