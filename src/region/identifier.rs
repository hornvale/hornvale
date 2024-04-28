use derive_more::Display;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A unique identifier for a region.
#[derive(Clone, Debug, Default, Display, Eq, Hash, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct RegionIdentifier(pub String);

impl RegionIdentifier {
  /// Creates a new region identifier.
  pub fn new() -> Self {
    Self(Uuid::new_v4().to_string())
  }

  /// Creates a new region identifier from a string.
  pub fn from_string<S: Into<String>>(s: S) -> Self {
    Self(s.into())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_new() {
    init();
    let region_identifier = RegionIdentifier::new();
    assert_eq!(region_identifier.to_string().len(), 36);
  }

  #[test]
  fn test_from_string() {
    init();
    let region_identifier = RegionIdentifier::from_string("test");
    assert_eq!(region_identifier.to_string(), "test");
  }

  #[test]
  fn test_to_string() {
    init();
    let region_identifier = RegionIdentifier::from_string("test");
    assert_eq!(region_identifier.to_string(), "test");
  }

  #[test]
  fn test_serde() {
    init();
    let region_identifier = RegionIdentifier::from_string("test");
    let json = serde_json::to_string(&region_identifier).unwrap();
    assert_eq!(json, "\"test\"");
    let deserialized_region_identifier: RegionIdentifier = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized_region_identifier, region_identifier);
  }

  #[test]
  fn test_serde_default() {
    init();
    let region_identifier = RegionIdentifier::default();
    let json = serde_json::to_string(&region_identifier).unwrap();
    assert_eq!(json, "\"\"");
    let deserialized_region_identifier: RegionIdentifier = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized_region_identifier, region_identifier);
  }
}
