use crate::entity_id::BaseId;
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
