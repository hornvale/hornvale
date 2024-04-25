use derive_more::{Add, Neg, Sub};
use serde::{Deserialize, Serialize};

/// Region generators are responsible for creating regions.
pub mod generator;
/// The collection of region generators.
pub mod generators;
/// The entity is a region.
pub mod is_a_region;
/// Trait implementations.
pub mod traits;

/// Regions are 3-dimensional grids of rooms.
///
/// They exist on a separate 3D grid from rooms and are used to manage the
/// loading and unloading of regions as the player moves around the world.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Neg, Ord, PartialEq, PartialOrd, Serialize, Sub)]
pub struct Region {
  /// The w-coordinate of the region.
  pub w: i64,
  /// The x-coordinate of the region.
  pub x: i64,
  /// The y-coordinate of the region.
  pub y: i64,
  /// The z-coordinate of the region.
  pub z: i64,
}
