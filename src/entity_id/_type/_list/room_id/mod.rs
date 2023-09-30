use crate::entity_id::BaseId;
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
