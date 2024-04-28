use derive_more::{Add, Display, Neg, Sub};
use serde::{Deserialize, Serialize};

/// Traits and trait implementations.
pub mod traits;

/// 4-dimensional integer point.
#[derive(
  Add, Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, Neg, Ord, PartialEq, PartialOrd, Serialize, Sub,
)]
#[display(fmt = "({}, {}, {}, {})", w, x, y, z)]
pub struct Point4D {
  /// The `w` coordinate (outside <-> inside/-w <-> +w).
  pub w: i64,
  /// The `x` coordinate (west <-> east/-x <-> +x).
  pub x: i64,
  /// The `y` coordinate (south <-> north/-y <-> +y).
  pub y: i64,
  /// The `z` coordinate (down <-> up/-z <-> +z).
  pub z: i64,
}
