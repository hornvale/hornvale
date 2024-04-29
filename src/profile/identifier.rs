use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Profile identifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
#[repr(transparent)]
pub struct ProfileIdentifier(pub String);

impl ProfileIdentifier {
  /// Create a new profile identifier.
  pub fn new() -> Self {
    Self(Uuid::new_v4().to_string())
  }
}

impl Default for ProfileIdentifier {
  fn default() -> Self {
    Self::new()
  }
}
