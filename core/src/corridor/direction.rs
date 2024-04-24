use crate::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};

/// Traits and trait implementations.
pub mod traits;

/// A `CorridorDirection` is a direction in which a corridor can be built.
///
/// Note that it is not possible to build a corridor in the `Northeast`,
/// `Southeast`, `Southwest`, `Northwest`, `In`, or `Out` directions.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct CorridorDirection(pub Direction);

impl CorridorDirection {
  /// Get the opposite direction of the given direction.
  pub fn opposite(&self) -> Self {
    Self(-self.0)
  }
}
