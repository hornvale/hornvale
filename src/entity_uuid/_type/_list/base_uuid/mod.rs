use derive_more::Display;
use uuid::Uuid;

/// The `BaseUuid` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct BaseUuid(pub String);

impl BaseUuid {
  /// Creates a new `BaseUuid`.
  pub fn new(uuid: String) -> Self {
    Self(uuid)
  }
}

impl Default for BaseUuid {
  fn default() -> Self {
    Self(Uuid::new_v4().to_string())
  }
}
