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
  /// The `w` component.
  pub w: i64,
  /// The `x` component.
  pub x: i64,
  /// The `y` component.
  pub y: i64,
  /// The `z` component.
  pub z: i64,
}
