use crate::entity_id::BaseId;
use crate::entity_id::IntoActorIdTrait;
use crate::entity_id::IntoBaseIdTrait;

/// The `ActorId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct ActorId(BaseId);

impl ActorId {
  /// Create a new `ActorId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl IntoBaseIdTrait for ActorId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl From<ActorId> for BaseId {
  fn from(id: ActorId) -> Self {
    id.0
  }
}

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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_actor_id_from_base_id() {
    let base_id = BaseId::default();
    let actor_id = ActorId::from(base_id.clone());
    assert_eq!(base_id, actor_id.into_base_id());
  }
}
