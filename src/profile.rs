//! # Profile
//!
//! This module contains the functionality for representing a "profile", which
//! is a save file for the game.
//!
//! This is managed by the `Profile` struct, which is itself fairly simple. It
//! contains some information used to locate the player character within the
//! world, as well as some metadata about the game state.

use crate::region::prelude::*;
use serde::{Deserialize, Serialize};

/// Identifier for a profile.
pub mod identifier;
use identifier::ProfileIdentifier;

/// The `Profile` struct represents a save file.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Profile {
  /// The identifier of the profile.
  pub identifier: ProfileIdentifier,
  /// The name of the player character.
  pub name: String,
  /// The region entity ID of the player character.
  pub region: RegionIdentifier,
}

impl Profile {
  /// Create a new `Profile` instance.
  pub fn new(name: &str, region: &RegionIdentifier) -> Self {
    let identifier = ProfileIdentifier::new();
    let name = name.to_string();
    let region = region.clone();
    Self {
      identifier,
      name,
      region,
    }
  }

  /// Serialize the save to a JSON string.
  pub fn to_json(&self) -> String {
    serde_json::to_string(self).unwrap()
  }

  /// Deserialize a save from a JSON string.
  pub fn from_json(json: &str) -> Self {
    serde_json::from_str(json).unwrap()
  }
}

/// The prelude.
pub mod prelude {
  pub use super::identifier::ProfileIdentifier;
  pub use super::Profile;
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_save() {
    init();
    let region = RegionIdentifier::default();
    let save = Profile::new("test", &region);
    let json = save.to_json();
    let loaded_save = Profile::from_json(&json);
    assert_eq!(save, loaded_save);
  }

  #[test]
  fn test_save_new() {
    init();
    let region = RegionIdentifier::default();
    let save = Profile::new("test", &region);
    assert_eq!(save.name, "test");
    assert_eq!(save.region, region);
  }
}
