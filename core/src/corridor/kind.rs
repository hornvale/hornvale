use crate::prelude::Region;
use serde::{Deserialize, Serialize};
use strum::Display;

/// A `CorridorKind` determines if and how a corridor can be traversed.
///
/// In most cases, this will be a simple operation, but we observe them so that
/// we can perform garbage collection, load and unload regions, and so on.
///
/// In other cases, this is more complicated; we can have special corridors that
/// ascend to special regions, for example. These may require storing additional
/// information.
#[derive(Clone, Copy, Debug, Display, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum CorridorKind {
  /// Normal and essentially transparent to the player.
  Default(Region),
  /// The player is ascending to a special region (z>0).
  Ascend(Region),
}

impl From<Region> for CorridorKind {
  fn from(region: Region) -> Self {
    if region.z > 0 {
      CorridorKind::Ascend(region)
    } else {
      CorridorKind::Default(region)
    }
  }
}
