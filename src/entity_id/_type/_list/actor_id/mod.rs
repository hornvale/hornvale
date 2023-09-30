use crate::entity_id::BaseId;
use crate::entity_id::IntoActorIdTrait;

/// The `ActorId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct ActorId(BaseId);

impl From<BaseId> for ActorId {
  fn from(id: BaseId) -> Self {
    Self(id)
  }
}

impl<T> From<T> for ActorId
where
  T: IntoActorIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}
