use derive_more::{Add, Sub};
use serde::{Deserialize, Serialize};

/// Components.
pub mod components;
/// Trait implementations.
pub mod traits;

/// Regions are 3-dimensional grids of rooms.
///
/// They exist on a separate 3D grid from rooms and are used to manage the
/// loading and unloading of regions as the player moves around the world.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Sub)]
pub struct Region {
  /// The x-coordinate of the region.
  pub x: i64,
  /// The y-coordinate of the region.
  pub y: i64,
  /// The z-coordinate of the region.
  pub z: i64,
}
