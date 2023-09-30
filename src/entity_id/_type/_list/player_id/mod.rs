use crate::entity_id::BaseId;
use crate::entity_id::IntoBaseIdTrait;
use crate::entity_id::IntoEntityIdTrait;
use crate::entity_id::IntoPlayerIdTrait;

/// The `PlayerId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct PlayerId(BaseId);

impl IntoBaseIdTrait for PlayerId {
  fn into_base_id(self) -> BaseId {
    self.0
  }
}

impl IntoEntityIdTrait for PlayerId {}

impl PlayerId {
  /// Create a new `PlayerId`.
  pub fn new() -> Self {
    Self(BaseId::default())
  }
}

impl From<PlayerId> for BaseId {
  fn from(id: PlayerId) -> Self {
    id.0
  }
}

impl<T> From<T> for PlayerId
where
  T: IntoPlayerIdTrait,
{
  fn from(id: T) -> Self {
    Self(id.into_base_id())
  }
}
