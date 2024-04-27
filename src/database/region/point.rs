use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Regions are 4-dimensional grids of rooms.
///
/// They exist on a separate 4D grid from rooms and are used to manage the
/// loading and unloading of regions as the player moves around the world.
#[derive(Clone, Copy, Debug, Default, Display, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
#[display(fmt = "Region({}, {}, {}, {})", w, x, y, z)]
pub struct RegionPoint {
  /// The w-coordinate of the region.
  pub w: i64,
  /// The x-coordinate of the region.
  pub x: i64,
  /// The y-coordinate of the region.
  pub y: i64,
  /// The z-coordinate of the region.
  pub z: i64,
}
