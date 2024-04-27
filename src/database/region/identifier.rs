use derive_more::Display;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Region identifier.
#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize)]
#[repr(transparent)]
pub struct RegionIdentifier(pub String);

impl RegionIdentifier {
  /// Create a new region identifier.
  pub fn new() -> Self {
    Self(Uuid::new_v4().to_string())
  }
}

impl Default for RegionIdentifier {
  fn default() -> Self {
    Self::new()
  }
}
