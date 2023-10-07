use crate::entity_id::BaseId;
use crate::entity_id::IntoBaseIdTrait;
use crate::entity_id::IntoEntityIdTrait;

/// The `EntityId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct EntityId(BaseId);

impl EntityId {
  /// Create a new `EntityId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl IntoBaseIdTrait for EntityId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl From<EntityId> for BaseId {
  fn from(id: EntityId) -> Self {
    id.0
  }
}

impl<T> From<T> for EntityId
where
  T: IntoEntityIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_entity_id_from_base_id() {
    let base_id = BaseId::default();
    let entity_id = EntityId(base_id.clone());
    assert_eq!(base_id, entity_id.into_base_id());
  }
}
