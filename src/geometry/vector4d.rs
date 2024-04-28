use derive_more::{Add, Display, Neg, Sub};
use serde::{Deserialize, Serialize};

/// Trait implementations.
pub mod traits;

/// A 4-dimensional integer vector.
#[derive(
  Add, Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, Neg, Ord, PartialEq, PartialOrd, Serialize, Sub,
)]
#[display(fmt = "({}, {}, {}, {})", w, x, y, z)]
pub struct Vector4D {
  /// The `w` component (outside <-> inside/-w <-> +w).
  pub w: i64,
  /// The `x` component (west <-> east/-x <-> +x).
  pub x: i64,
  /// The `y` component (south <-> north/-y <-> +y).
  pub y: i64,
  /// The `z` component (down <-> up/-z <-> +z).
  pub z: i64,
}
