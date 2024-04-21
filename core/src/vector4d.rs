use derive_more::{Add, Mul, Sub};
use serde::{Deserialize, Serialize};

/// Trait implementations.
pub mod traits;

/// A component encapsulating a 4-dimensional vector.
#[derive(Add, Clone, Copy, Debug, Deserialize, Eq, Mul, PartialEq, Serialize, Sub)]
pub struct Vector4D {
  /// The w-coordinate of the vector.
  pub w: i64,
  /// The x-coordinate of the vector.
  pub x: i64,
  /// The y-coordinate of the vector.
  pub y: i64,
  /// The z-coordinate of the vector.
  pub z: i64,
}
