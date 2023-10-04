use crate::entity_id::BaseId;
use crate::entity_id::IntoBaseIdTrait;
use crate::entity_id::IntoEntityIdTrait;
use crate::entity_id::IntoObjectIdTrait;

/// The `ObjectId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct ObjectId(BaseId);

impl ObjectId {
  /// Create a new `ObjectId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl IntoBaseIdTrait for ObjectId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl IntoEntityIdTrait for ObjectId {}

impl From<ObjectId> for BaseId {
  fn from(id: ObjectId) -> Self {
    id.0
  }
}

impl From<BaseId> for ObjectId {
  fn from(id: BaseId) -> Self {
    Self(id)
  }
}

impl<T> From<T> for ObjectId
where
  T: IntoObjectIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_object_id_from_base_id() {
    let base_id = BaseId::default();
    let object_id = ObjectId::from(base_id.clone());
    assert_eq!(base_id, object_id.into_base_id());
  }

  #[test]
  fn test_object_id_from_object_id() {
    let object_id = ObjectId::default();
    let object_id2 = ObjectId::from(object_id.clone());
    assert_eq!(object_id, object_id2);
  }
}
