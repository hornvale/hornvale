use crate::entity_id::BaseId;
use crate::entity_id::IntoBaseIdTrait;
use crate::entity_id::IntoEntityIdTrait;
use crate::entity_id::IntoPassageIdTrait;

/// The `PassageId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct PassageId(BaseId);

impl IntoBaseIdTrait for PassageId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl IntoEntityIdTrait for PassageId {}

impl PassageId {
  /// Create a new `PassageId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl From<PassageId> for BaseId {
  fn from(id: PassageId) -> Self {
    id.0
  }
}

impl<T> From<T> for PassageId
where
  T: IntoPassageIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}
