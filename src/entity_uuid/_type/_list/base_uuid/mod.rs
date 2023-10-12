use derive_more::Display;
use uuid::Uuid;

use crate::entity_uuid::IntoBaseUuidTrait;

/// The `BaseUuid` type.
///
/// We do this so that we can perform some compile-time type-checking with UUIDs.
#[derive(Clone, Debug, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct BaseUuid(pub String);

impl BaseUuid {
  /// Creates a new `BaseUuid`.
  pub fn new(uuid: String) -> Self {
    Self(uuid)
  }
}

impl<T> From<T> for BaseUuid
where
  T: IntoBaseUuidTrait,
{
  fn from(uuid: T) -> Self {
    uuid.into_base_uuid()
  }
}

impl Default for BaseUuid {
  fn default() -> Self {
    Self(Uuid::new_v4().to_string())
  }
}
