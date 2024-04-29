//! # Region
//!
//! A region is a tuple of:
//! - `RegionIdentifier`: a unique identifier for the region
//! - `RegionName`: a name for the region
//! - `RegionDescription`: an (optional) description of the region
//! - `RegionPoint`: a point in the 4D grid
//!
//! They are maintained within a `RegionMap`.

use derive_more::Display;
use serde::{Deserialize, Serialize};

/// The adjacency map.
pub mod adjacency_map;
/// A description for a region.
pub mod description;
use description::RegionDescription;
/// A generator for regions.
pub mod generator;
/// The region generator registry.
pub mod generator_registry;
/// Implemented generators.
pub mod generators;
/// A unique identifier for a region.
pub mod identifier;
use identifier::RegionIdentifier;
/// A map of regions.
pub mod map;
/// A name for a region.
pub mod name;
use name::RegionName;
/// A point in the 4D grid.
pub mod point;
use point::RegionPoint;

/// A region.
#[derive(Clone, Debug, Display, Deserialize, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[display(fmt = "{}: {} ({}) @ {}", name, identifier, description, point)]
pub struct Region {
  /// The identifier.
  pub identifier: RegionIdentifier,
  /// The name.
  pub name: RegionName,
  /// The description.
  pub description: RegionDescription,
  /// The point.
  pub point: RegionPoint,
}

impl Region {
  /// Create a new region.
  pub fn new(
    identifier: RegionIdentifier,
    name: RegionName,
    description: RegionDescription,
    point: RegionPoint,
  ) -> Self {
    Self {
      identifier,
      name,
      description,
      point,
    }
  }

  /// Serialize the region to a JSON string.
  pub fn to_json(&self) -> String {
    serde_json::to_string(self).unwrap()
  }

  /// Deserialize a region from a JSON string.
  pub fn from_json(json: &str) -> Self {
    serde_json::from_str(json).unwrap()
  }
}

/// The prelude.
pub mod prelude {
  pub use super::adjacency_map::RegionAdjacencyMap;
  pub use super::description::RegionDescription;
  pub use super::generator::RegionGenerator;
  pub use super::identifier::RegionIdentifier;
  pub use super::map::RegionMap;
  pub use super::name::RegionName;
  pub use super::point::RegionPoint;
  pub use super::Region;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::prelude::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_new() {
    init();
    let region = Region::new(
      RegionIdentifier::new(),
      RegionName("test".to_string()),
      RegionDescription("test".to_string()),
      RegionPoint(Point4D::from((0, 0, 0, 0))),
    );
    assert_eq!(region.name.to_string(), "test");
    assert_eq!(region.description.to_string(), "test");
  }

  #[test]
  fn test_to_json() {
    init();
    let region = Region::new(
      RegionIdentifier("00000000-0000-0000-0000-000000000000".to_string()),
      RegionName("test".to_string()),
      RegionDescription("test description".to_string()),
      RegionPoint(Point4D::from((0, 0, 0, 0))),
    );
    let json = region.to_json();
    assert_eq_pretty!(json, "{\"identifier\":\"00000000-0000-0000-0000-000000000000\",\"name\":\"test\",\"description\":\"test description\",\"point\":{\"w\":0,\"x\":0,\"y\":0,\"z\":0}}");
  }

  #[test]
  fn test_from_json() {
    init();
    let json = "{\"identifier\":\"00000000-0000-0000-0000-000000000000\",\"name\":\"test\",\"description\":\"test\",\"point\":{\"w\":0,\"x\":0,\"y\":0,\"z\":0}}";
    let region = Region::from_json(json);
    assert_eq!(region.name.to_string(), "test");
    assert_eq!(region.description.to_string(), "test");
    assert_eq!(region.to_json(), json);
  }
}
