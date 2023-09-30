use uuid::Uuid;

/// The `BaseId` type.
///
/// We do this so that we can perform some compile-time type-checking with IDs.
#[derive(Clone, Debug, Deserialize, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize)]
#[repr(transparent)]
pub struct BaseId(pub String);

impl BaseId {
  /// Creates a new `BaseId`.
  pub fn new(id: String) -> Self {
    Self(id)
  }
}

impl Default for BaseId {
  fn default() -> Self {
    Self(Uuid::new_v4().to_string())
  }
}
