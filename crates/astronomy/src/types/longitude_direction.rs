use derive_more::{Display, Neg};
use serde::{Deserialize, Serialize};

/// The `LongitudeDirection` enum, representing the direction of a longitude.
#[derive(Clone, Copy, Debug, Deserialize, Display, Neg, PartialEq, Serialize)]
pub enum LongitudeDirection {
  /// East.
  #[display(fmt = "E")]
  East,
  /// West.
  #[display(fmt = "W")]
  West,
}
