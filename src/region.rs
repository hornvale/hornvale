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

/// A description for a region.
pub mod description;
use description::RegionDescription;
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

/// The prelude.
pub mod prelude {
  pub use super::description::RegionDescription;
  pub use super::identifier::RegionIdentifier;
  pub use super::map::RegionMap;
  pub use super::name::RegionName;
  pub use super::point::RegionPoint;
}
