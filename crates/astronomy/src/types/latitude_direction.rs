use derive_more::{Display, Neg};
use serde::{Deserialize, Serialize};

/// The `LatitudeDirection` enum, representing the direction of a latitude.
#[derive(Clone, Copy, Debug, Deserialize, Display, Neg, PartialEq, Serialize)]
pub enum LatitudeDirection {
  /// North.
  #[display(fmt = "N")]
  North,
  /// South.
  #[display(fmt = "S")]
  South,
}
